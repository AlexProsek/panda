unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Buttons
  , panda.Intfs
  , panda.Arrays
  , panda.Demos.Utils
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
    fImg: INDArray<TRGB24>;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btFlipChanged(Sender: TObject);
var i: INDArray<TRGB24>;
begin
  i := TImageRGB24.Create(fImg.Shape[1], fImg.Shape[0]);

  if btFlipH.Down and btFlipV.Down then
    i[[NDIAll, NDIAll]] := fImg[[NDIAll(-1), NDIAll(-1)]]
  else
  if btFlipH.Down then
    i[[NDIAll, NDIAll]] := fImg[[NDIAll(-1), NDIAll]]
  else
  if btFlipV.Down then
    i[[NDIAll, NDIAll]] := fImg[[NDIAll, NDIAll(-1)]]
  else
    i[[NDIAll, NDIAll]] := fImg[[NDIAll, NDIAll]];

  Image1.Picture.Assign(TImageRGB24(i).Bitmap);
end;

procedure TForm1.btSplitRGBClick(Sender: TObject);
var i: INDArray<TRGB24>;
    chSrc, chDst: INDArray<Byte>;
    w, h, w2, h2: NativeInt;
begin
  w := fImg.Shape[1];
  h := fImg.Shape[0];
  chSrc := TRGB24Channels.Create(fImg);
  chSrc := chSrc[[NDIAll(2), NDIAll(2)]];
  w2 := chSrc.Shape[1];
  h2 := chSrc.Shape[0];
  i := TImageRGB24.Create(2*w2, 2*h2);
  chDst := TRGB24Channels.Create(i);

  TNDAUt.Fill<Byte>(chDst, 0);
  chDst[[NDISpan(h2, -1), NDISpan(0, w2 - 1)]] := chSrc;
  chDst[[NDISpan(h2, -1), NDISpan(w2, -1), NDI(2)]] := chSrc[[NDIAll, NDIAll, NDI(2)]];
  chDst[[NDISpan(0, h2 - 1), NDISpan(0, w2 - 1), NDI(1)]] := chSrc[[NDIAll, NDIAll, NDI(1)]];
  chDst[[NDISpan(0, h2 - 1), NDISpan(w2, -1), NDI(0)]] := chSrc[[NDIAll, NDIAll, NDI(0)]];

  Image1.Picture.Assign(TImageRGB24(i).Bitmap);
end;

procedure TForm1.Button1Click(Sender: TObject);
var bmp: TBitmap;
begin
  if FileOpenDialog1.Execute then begin
    bmp := TBitmap.Create;
    bmp.LoadFromFile(FileOpenDialog1.FileName);
    bmp.PixelFormat := pf24bit;
    Image1.SetBounds(0, 0, bmp.Width, bmp.Height);
    Image1.Picture.Assign(bmp);
    fImg := TImageRGB24.Create(bmp);

    btFlipH.Enabled := True;
    btFlipV.Enabled := True;
    btSplitRGB.Enabled := True;
  end;
end;

end.
