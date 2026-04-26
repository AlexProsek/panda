unit Unit04;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Samples.Spin, Vcl.ComCtrls,
  Vcl.Menus
  , panda.Intfs
  , panda.Arrays
  , panda.ArrManip
  , panda.Arithmetic
  , panda.Math
  , panda.Conv
  , panda.ImgProc.Types
  , panda.ImgProc.Images
  , panda.ImgProc.VCLImages
  , panda.ImgProc.io
  , System.Diagnostics
  , System.UITypes
  ;

type
  TForm4 = class(TForm)
    Panel1: TPanel;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    Button1: TButton;
    FileOpenDialog1: TFileOpenDialog;
    Label1: TLabel;
    edKernelSize: TSpinEdit;
    btExecute: TButton;
    Label2: TLabel;
    cbFilterType: TComboBox;
    StatusBar1: TStatusBar;
    cbUseSepKer: TCheckBox;
    procedure btExecuteClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    fImg: IImage<TRGB24>;
    function CreateKernel(aDim: Integer): INDArray<Single>;
    procedure ShowImage(const aData: INDArray<Byte>);
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}

function TForm4.CreateKernel(aDim: Integer): INDArray<Single>;
var sz: Integer;
    s, r: Single;
    x: TTensorF32;
begin
  sz := edKernelSize.Value;

  case cbFilterType.ItemIndex of
    0: Result := TNDAUt.Full<Single>([sz], 1/sz); // mean filter
    1: begin // Gaussian filter
      r := sz div 2;
      s := r / 2;
      x := ndaRange(Single(-r), Single(r) + 1);
      Result := ndaExp(x * x / (-2 * s * s));
      Result := TTensorF32(Result) / ndaTotal(Result);
    end;
  else
    MessageDlg('Unknown filter type.', mtError, [mbOk], 0);
    exit;
  end;

  if aDim = 2 then
    Result := ndaOuter(Result, Result);
end;

procedure TForm4.btExecuteClick(Sender: TObject);
var src, dst, k: INDArray<Single>;
    ch: INDArray<Byte>;
    sw: TStopWatch;
begin
  sw.Reset;
  sw.Start;

  ch := TImgUt.AsArray<TRGB24, Byte>(fImg);
  ch := TNDAMan.Transpose<Byte>(ch, [2, 0, 1]);
  src := TNDAUt.AsType<Single>(ch);
  if cbUseSepKer.Checked then begin
    k := CreateKernel(1);
    dst := ndaCorrelate(k, src);
    dst := TNDAMan.Transpose<Single>(dst, [0, 2, 1]);
    dst := ndaCorrelate(k, dst);
    ch := TNDAUt.AsType<Byte>(dst);
    ch := TNDAMan.Transpose<Byte>(ch, [2, 1, 0]);
  end else begin
    k := CreateKernel(2);
    dst := ndaCorrelate(k, src);
    ch := TNDAUt.AsType<Byte>(dst);
    ch := TNDAMan.Transpose<Byte>(ch, [1, 2, 0]);
  end;

  sw.Stop;
  StatusBar1.Panels[0].Text := Format('Elapsed time: %f [ms]',
    [sw.Elapsed.TotalMilliseconds]
  );

  ShowImage(ch);
end;

procedure TForm4.ShowImage(const aData: INDArray<Byte>);
var w, h: Integer;
    i: IImage<TRGB24>;
begin
  w := aData.Shape[1];
  h := aData.Shape[0];
  i := TBmpRGB24.Create(w, h);
  TNDAUt.Fill<Byte>(TImgUt.AsArray<TRGB24, Byte>(i), aData);

  Image1.SetBounds(0, 0, w, h);
  Image1.Picture.Assign((i as IBitmapImage).Bitmap);
end;

procedure TForm4.Button1Click(Sender: TObject);
var bmp: TBitmap;
begin
  if FileOpenDialog1.Execute then begin
    bmp := TBitmap.Create;
    if not LoadBitmapFromFile(FileOpenDialog1.FileName, bmp) then begin
      MessageDlg(Format('Import of file ''%s'' failed.', [FileOpenDialog1.FileName]),
        mtError, [mbOk], 0);
      bmp.Free;
      exit;
    end;
    bmp.PixelFormat := pf24bit;
    Image1.SetBounds(0, 0, bmp.Width, bmp.Height);
    Image1.Picture.Assign(bmp);
    fImg := TBmpRGB24.Create(bmp);
    btExecute.Enabled := True;
  end;
end;

end.
