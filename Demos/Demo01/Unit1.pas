unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Buttons
  , panda.Intfs
  , panda.Arrays
  , panda.ImgProc.Types
  , panda.ImgProc.Images
  , panda.ImgProc.VCLImages
  , panda.ImgProc.io
  ;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    FileOpenDialog1: TFileOpenDialog;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    btFlipH: TSpeedButton;
    btFlipV: TSpeedButton;
    btSplitRGB: TButton;
    procedure Button1Click(Sender: TObject);
    procedure btFlipChanged(Sender: TObject);
    procedure btSplitRGBClick(Sender: TObject);
  private
    { Private declarations }
    fImg: IImage<TRGB24>;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btFlipChanged(Sender: TObject);
var src, dst: INDArray<TRGB24>;
    i: IImage<TRGB24>;
begin
  src := TImgUt.AsArray<TRGB24>(fImg);
  i := TBmpRGB24.Create(fImg.Width, fImg.Height);
  dst := TImgUt.AsArray<TRGB24>(i);

  if btFlipH.Down and btFlipV.Down then
    dst[[NDIAll, NDIAll]] := src[[NDIAll(-1), NDIAll(-1)]]
  else
  if btFlipH.Down then
    dst[[NDIAll, NDIAll]] := src[[NDIAll(-1), NDIAll]]
  else
  if btFlipV.Down then
    dst[[NDIAll, NDIAll]] := src[[NDIAll, NDIAll(-1)]]
  else
    dst[[NDIAll, NDIAll]] := src[[NDIAll, NDIAll]];

  Image1.Picture.Assign((i as IBitmapImage).Bitmap);
end;

procedure TForm1.btSplitRGBClick(Sender: TObject);
var i: IImage<TRGB24>;
    chSrc, chDst: INDArray<Byte>;
    w, h: NativeInt;
begin
  chSrc := TImgUt.AsArray<TRGB24, Byte>(fImg);
  chSrc := chSrc[[NDIAll(2), NDIAll(2)]];
  w := chSrc.Shape[1];
  h := chSrc.Shape[0];
  i := TBmpRGB24.Create(2*w, 2*h);
  chDst := TImgUt.AsArray<TRGB24, Byte>(i);

  TNDAUt.Fill<Byte>(chDst, 0);
  chDst[[NDISpan(h, -1), NDISpan(0, w - 1)]] := chSrc;
  chDst[[NDISpan(h, -1), NDISpan(w, -1), NDI(2)]] := chSrc[[NDIAll, NDIAll, NDI(2)]];
  chDst[[NDISpan(0, h - 1), NDISpan(0, w - 1), NDI(1)]] := chSrc[[NDIAll, NDIAll, NDI(1)]];
  chDst[[NDISpan(0, h - 1), NDISpan(w, -1), NDI(0)]] := chSrc[[NDIAll, NDIAll, NDI(0)]];

  Image1.Picture.Assign((i as IBitmapImage).Bitmap);
end;

procedure TForm1.Button1Click(Sender: TObject);
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

    btFlipH.Enabled := True;
    btFlipV.Enabled := True;
    btSplitRGB.Enabled := True;
  end;
end;

end.
