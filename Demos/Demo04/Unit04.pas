unit Unit04;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Samples.Spin, Vcl.ComCtrls
  , panda.Intfs
  , panda.Arrays
  , panda.ArrManip
  , panda.Arithmetic
  , panda.Conv
  , panda.Demos.Utils
  , System.Diagnostics
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
    cbKernelType: TComboBox;
    StatusBar1: TStatusBar;
    procedure btExecuteClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    fImg: INDArray<TRGB24>;
    function CreateKernel: INDArray<Single>;
    procedure ShowImage(const aData: INDArray<Byte>);
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}

function TForm4.CreateKernel: INDArray<Single>;
var sz: Integer;
begin
  sz := edKernelSize.Value;
  // Mean filter
  Result := TNDAUt.Full<Single>([sz, sz], 1/(sz*sz));
end;

procedure TForm4.btExecuteClick(Sender: TObject);
var src, dst, k, ch0, ch1, ch2: INDArray<Single>;
    ch: INDArray<Byte>;
    sh: TNDAShape;
    sw: TStopWatch;
begin
  sw.Start;

  ch := TNDAMan.Transpose<Byte>(TRGB24Channels.Create(fImg), [2, 0, 1]);
  src := TNDAUt.AsType<Single>(ch);
  k := CreateKernel();
  dst := ndaCorrelate(k, src);
  ch := TNDAUt.AsType<Byte>(dst);
  ch := TNDAMan.Transpose<Byte>(ch, [1, 2, 0]);

  sw.Stop;
  StatusBar1.Panels[0].Text := Format('Elapsed time: %f [ms]',
    [sw.Elapsed.TotalMilliseconds]
  );

  ShowImage(ch);
end;

procedure TForm4.ShowImage(const aData: INDArray<Byte>);
var bmp: TBitmap;
var w, h: Integer;
    i: INDArray<TRGB24>;
    ch: INDArray<Byte>;
begin
  w := aData.Shape[1];
  h := aData.Shape[0];
  bmp := TBitmap.Create(w, h);
  bmp.PixelFormat := pf24bit;
  bmp.SetSize(w, h);
  i := TImageRGB24.Create(bmp);
  ch := TRGB24Channels.Create(i);
  TNDAUt.Fill<Byte>(ch, aData);

  Image1.SetBounds(0, 0, bmp.Width, bmp.Height);
  Image1.Picture.Assign(bmp);
end;

procedure TForm4.Button1Click(Sender: TObject);
var bmp: TBitmap;
begin
  if FileOpenDialog1.Execute then begin
    bmp := TBitmap.Create;
    bmp.LoadFromFile(FileOpenDialog1.FileName);
    bmp.PixelFormat := pf24bit;
    Image1.SetBounds(0, 0, bmp.Width, bmp.Height);
    Image1.Picture.Assign(bmp);
    fImg := TImageRGB24.Create(bmp);
    btExecute.Enabled := True;
  end;
end;

end.
