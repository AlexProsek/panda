unit Unit11;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls
  , panda.NN.Importer
  , panda.NN
  , panda.NN.ImageEncoder
  , panda.ImgProc.Types
  , panda.ImgProc.Images
  , panda.ImgProc.VCLImages
  , panda.ImgProc.io
  , panda.Intfs
  , panda.Arrays
  , System.IOUtils
  , System.UITypes
  ;

type
  TForm11 = class(TForm)
    Panel1: TPanel;
    btClassify: TButton;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    btLoadImg: TButton;
    FileOpenDialog1: TFileOpenDialog;
    lbClass: TLabel;
    procedure btClassifyClick(Sender: TObject);
    procedure btLoadImgClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  protected
    fNet: TNNetChain;
    fImg: IImage<TRGB24>;
    function FlipH(aBmp: TBitmap): IImage<TRGB24>;
    procedure ImportImage(const aFileName: String; aSilent: Boolean = False);
  public
    { Public declarations }
  end;

var
  Form11: TForm11;

implementation

{$R *.dfm}

function TForm11.FlipH(aBmp: TBitmap): IImage<TRGB24>;
var bmpImg: IImage<TRGB24>;
    src, dst: INDArray<TRGB24>;
begin
  bmpImg := TBmpRGB24.Create(aBmp, False);
  Result := TNDAImg<TRGB24>.Create(aBmp.Width, aBmp.Height);
  src := TImgUt.AsArray<TRGB24>(bmpImg);
  dst := TImgUt.AsArray<TRGB24>(Result);
  dst[[NDIAll(-1)]] := src[[NDIAll]];
end;

procedure TForm11.btClassifyClick(Sender: TObject);
var res: INetClassResult;
    cls: Integer;
begin
  if not Assigned(fImg) then begin
    MessageDlg('No character image to classify.', mtError, [mbOk], 0);
    exit;
  end;

  if not (
    Supports(fNet.Execute(fImg), INetClassResult, res) and
    (Length(res.Indices) > 0))
  then begin
    MessageDlg('Net execution failed.', mtError, [mbOk], 0);
    exit;
  end;

  lbClass.Caption := Format('Class: %d', [res.Indices[0]]);
end;

procedure TForm11.ImportImage(const aFileName: String; aSilent: Boolean);
var bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  try
    if not LoadBitmapFromFile(aFileName, bmp) then begin
      MessageDlg(Format('Import of file ''%s'' failed.', [aFileName]),
        mtError, [mbOk], 0);
      bmp.Free;
      exit;
    end;
    bmp.PixelFormat := pf24bit;
    Image1.SetBounds(0, 0, bmp.Width, bmp.Height);
    Image1.Picture.Assign(bmp);
    fImg := FlipH(bmp);
  finally
    bmp.Free;
  end;
end;

procedure TForm11.btLoadImgClick(Sender: TObject);
begin
  if FileOpenDialog1.Execute then
    ImportImage(FileOpenDialog1.FileName);
end;

procedure TForm11.FormDestroy(Sender: TObject);
begin
  fNet.Free;
end;

procedure TForm11.FormCreate(Sender: TObject);
var path, fn: String;
    importer: TNNMAT4Importer;
    sh: TArray<NativeInt>;
begin
  Image1.Picture.Bitmap.SetSize(100, 100);
  Image1.Picture.Bitmap.PixelFormat := pf24bit;

  path := TPath.GetFullPath(TPath.Combine(
    TPath.GetDirectoryName(Application.ExeName),
    '..\..\..\..\NN\UnitTests\TestData'
  ));

  fn := TPath.Combine(path, 'nn-mnist.mat');
  if not TFile.Exists(fn) then begin
    MessageDlg('Network data file was not found.', mtError, [mbOk], 0);
    exit;
  end;

  importer := TNNMAT4Importer.Create(fn);
  try
    fNet := importer.ReadNetwork;
  finally
    importer.Free;
  end;

  if Assigned(fNet) then begin
    with fNet.InputEncoder as TImageNetEncoder do begin
      SetLength(sh, 3);
      sh[0] := 1;
      sh[1] := Height;
      sh[2] := Width;
    end;

    if fNet.Initialize(sh) then begin
      fn := TPath.Combine(path, 'mnist_char_3.jpg');
      ImportImage(fn, True);
      if Assigned(fImg) then
        FileOpenDialog1.DefaultFolder := path;
      btClassify.Enabled := True;
    end;
  end;
end;

end.
