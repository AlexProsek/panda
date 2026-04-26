unit Unit05;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls

  , panda.ImgProc.Types
  , panda.ImgProc.VCLImages
  , panda.ImgProc.CSCvt
  , panda.ImgProc.ImgResize
  , panda.ImgProc.io
  , System.Diagnostics
  , System.Math
  ;

type
  TForm5 = class(TForm)
    Panel1: TPanel;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    Button1: TButton;
    FileOpenDialog1: TFileOpenDialog;
    cbGrayscale: TCheckBox;
    StatusBar1: TStatusBar;
    TrackBar1: TTrackBar;
    lbScale: TLabel;
    cbInterpMethod: TComboBox;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure cbGrayscaleClick(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    fImg: IImage<TRGB24>;
    fImgi8: IImage<Byte>;
    fSW: TStopWatch;
    procedure SWStart;
    procedure SWStop;
    procedure ShowImage(aImg: IImage);
  public
    { Public declarations }
  end;

var
  Form5: TForm5;

implementation

{$R *.dfm}

procedure TForm5.SWStart;
begin
  fSW.Reset;
  fSW.Start;
end;

procedure TForm5.SWStop;
begin
  fSW.Stop;
  StatusBar1.Panels[0].Text := Format('Elapsed time: %f [ms]',
    [fSW.Elapsed.TotalMilliseconds]
  );
end;

procedure TForm5.TrackBar1Change(Sender: TObject);
var r: Double;
    w, h: Integer;
    imgi8: IImage<Byte>;
    imgrgb: IImage<TRGB24>;
    im: TInterpMethod;
const cScale: array [0..10] of Double = (
  0.1, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.4, 1.6, 1.8, 2
);
begin
  r := cScale[TrackBar1.Position];
  lbScale.Caption := Format('%d%%', [Round(r * 100)]);
  w := Ceil(r * fImg.Width);
  h := Ceil(r * fImg.Height);
  case cbInterpMethod.ItemIndex of
    0: im := imNN;
  else
    im := imBilinear;
  end;

  if cbGrayscale.Checked then begin
    if r <> 1 then begin
      imgi8 := TBmpUI8.Create(w, h);

      SWStart;
      ImageResize(fImgi8, imgi8, im);
      SWStop;

      ShowImage(imgi8);
    end else
      ShowImage(fImgi8);
  end else begin
    if r <> 1 then begin
      imgrgb := TBmpRGB24.Create(w, h);

      SWStart;
      ImageResize(fImg, imgrgb, im);
      SWStop;

      ShowImage(imgrgb);
    end else
      ShowImage(fImg);
  end;
end;

procedure TForm5.ShowImage(aImg: IImage);
var dst: IImage<TRGB24>;
begin
  if TCSUt.MatchQ<Byte>(aImg) then begin
    dst := TBmpRGB24.Create(aImg.Width, aImg.Height);
    ColorConvert(aImg as IImage<Byte>, dst);
    aImg := dst;
  end;

  Image1.SetBounds(0, 0, aImg.Width, aImg.Height);
  Image1.Picture.Assign((aImg as IBitmapImage).Bitmap);
end;

procedure TForm5.Button1Click(Sender: TObject);
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
    cbGrayscale.Checked := False;
  end;
end;

procedure TForm5.cbGrayscaleClick(Sender: TObject);
begin
  if cbGrayscale.Checked then begin
    fImgi8 := TBmpUI8.Create(fImg.Width, fImg.Height);

    SWStart;
    ColorConvert(fImg, fImgi8);
    SWStop;

    ShowImage(fImgi8);
  end else
    ShowImage(fImg);

  TrackBar1.Position := 5;
end;

end.
