unit Unit08;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, System.UITypes,
  Vcl.Samples.Spin, Vcl.ComCtrls

  , panda.Intfs
  , panda.Arrays
  , panda.ArrCmp
  , panda.ImgProc.Types
  , panda.ImgProc.VCLImages
  , panda.ImgProc.Images
  , panda.ImgProc.CSCvt
  , panda.ImgProc.io
  , panda.ImgProc.Segmentation
  , panda.ImgProc.Morph
  , panda.Formatter
  , System.Diagnostics
  ;

type
  TForm8 = class(TForm)
    ScrollBox1: TScrollBox;
    pnTop: TPanel;
    Image1: TImage;
    btLoadImage: TButton;
    FileOpenDialog1: TFileOpenDialog;
    cbMethod: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    edThreshold: TSpinEdit;
    btExecute: TButton;
    StatusBar1: TStatusBar;
    lbParams: TLabel;
    edParams: TEdit;
    Label3: TLabel;
    edRadius: TSpinEdit;
    Label4: TLabel;
    cbMMethod: TComboBox;
    Label5: TLabel;
    StaticText1: TStaticText;
    Label6: TLabel;
    edNIter: TSpinEdit;
    btMExecute: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure btLoadImageClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btExecuteClick(Sender: TObject);
    procedure btMExecuteClick(Sender: TObject);
    procedure cbMethodChange(Sender: TObject);
    procedure Image1MouseLeave(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    { Private declarations }
    fSrcImg: IImage<Byte>;
    fBinImg: IImage<Byte>;
    fViewImg: IImage<Byte>;
    fViewPixels: TXYImagePixels<Byte>;
    fSW: TStopWatch;
    fParser: TNDAParser;
    procedure ShowImage(aImg: IImage<Byte>);
    procedure ApplyThreshBin;
    procedure ApplyThreshBinInv;
    procedure ApplyThreshToZero;
    procedure ApplyThreshToZeroInv;
    procedure ApplyAdaptiveBin;
    procedure ApplyOpening;
    procedure ApplyClosing;
    procedure ApplyDilation;
    procedure ApplyErosion;
    procedure ApplyGradient;
    procedure ApplyTopHat;
    procedure ApplyBottomHat;
    procedure SWStart;
    procedure SWStop;
    procedure ShowAdaptiveBinControl(aShow: Boolean);
    procedure ShowThreasholdControl(aShow: Boolean);
    procedure SetBinImg(const aImg: IImage<Byte>);
  public
    { Public declarations }
  end;

var
  Form8: TForm8;

implementation

{$R *.dfm}

const
  MI_THRESH_BIN         = 0;
  MI_THRESH_BIN_INV     = 1;
  MI_THRESH_TOZERO      = 2;
  MI_THRESH_TOZERO_INV  = 3;
  MI_ADAPTIVE_BIN       = 4;

  MMI_OPENING           = 0;
  MMI_CLOSING           = 1;
  MMI_DILATION          = 2;
  MMI_EROSION           = 3;
  MMI_GRADIENT          = 4;
  MMI_TOPHAT            = 5;
  MMI_BOTTOMHAT         = 6;


procedure TForm8.FormDestroy(Sender: TObject);
begin
  fParser.Free;
end;

procedure TForm8.FormCreate(Sender: TObject);
begin
  with cbMethod.Items do begin
    Add('threshold binary');
    Add('threshold binary inverted');
    Add('threshold to zero');
    Add('threshold to zero inverted');
    Add('local adaptive binarize');
  end;
  cbMethod.ItemIndex := 0;
  cbMethodChange(nil);

  lbParams.Caption := #$03b1', '#$03b2', '#$03b3':';
  lbParams.ShowHint := True;
  lbParams.Hint := 'Threshold value '#$03b1'*mean + '#$03b2'*stdDev + '#$03b3' will be used.';

  fParser := TNDAParser.Create;
  fParser.ElementType := TypeInfo(Single);

  with cbMMethod.Items do begin
    Add('Opening');
    Add('Closing');
    Add('Dilation');
    Add('Erosion');
    Add('Gradient');
    Add('TopHat');
    Add('BottomHat');
  end;
  cbMMethod.ItemIndex := 0;
end;

procedure TForm8.Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(fSrcImg) and TImgUt.PositionWithin(fSrcImg, X, Y) then
    StatusBar1.Panels[1].Text := Format('Pixel value: %d', [fViewPixels[X, Y]])
  else
    StatusBar1.Panels[1].Text := 'Pixel value:';
end;

procedure TForm8.Image1MouseLeave(Sender: TObject);
begin
  StatusBar1.Panels[1].Text := 'Pixel value:';
end;

procedure TForm8.SWStart;
begin
  fSW.Reset;
  fSW.Start;
end;

procedure TForm8.SWStop;
begin
  fSW.Stop;
  StatusBar1.Panels[0].Text := Format('Elapsed time: %f [ms]',
    [fSW.Elapsed.TotalMilliseconds]
  );
end;

procedure TForm8.ShowAdaptiveBinControl(aShow: Boolean);
begin
  edParams.Enabled := aShow;
  edRadius.Enabled := aShow;
end;

procedure TForm8.ShowThreasholdControl(aShow: Boolean);
begin
  edThreshold.Enabled := aShow;
end;

procedure TForm8.SetBinImg(const aImg: IImage<Byte>);
var enable: Boolean;
begin
  fBinImg := aImg;
  enable := Assigned(aImg);

  cbMMethod.Enabled := enable;
  edNIter.Enabled := enable;
  btMExecute.Enabled := enable;
end;

procedure TForm8.ShowImage(aImg: IImage<Byte>);
var dst: IImage<TRGB24>;
begin
  fViewImg := aImg;
  fViewPixels := aImg;
  dst := TBmpRGB24.Create(aImg.Width, aImg.Height);
  ColorConvert(aImg, dst);
  Image1.SetBounds(0, 0, dst.Width, dst.Height);
  Image1.Picture.Assign((dst as IBitmapImage).Bitmap);
end;

procedure TForm8.ApplyThreshBin;
var dstImg: IImage<Byte>;
begin
  dstImg := TNDAImg<Byte>.Create(fSrcImg.Width, fSrcImg.Height);

  SWStart;
  Threshold(fSrcImg, dstImg, edThreshold.Value, tmBinary);
  SWStop;

  ShowImage(dstImg);
end;

procedure TForm8.ApplyThreshBinInv;
var dstImg: IImage<Byte>;
begin
  dstImg := TNDAImg<Byte>.Create(fSrcImg.Width, fSrcImg.Height);

  SWStart;
  Threshold(fSrcImg, dstImg, edThreshold.Value, tmBinaryInv);
  SWStop;

  ShowImage(dstImg);
end;

procedure TForm8.ApplyThreshToZero;
var dstImg: IImage<Byte>;
begin
  dstImg := TNDAImg<Byte>.Create(fSrcImg.Width, fSrcImg.Height);

  SWStart;
  Threshold(fSrcImg, dstImg, edThreshold.Value, tmToZero);
  SWStop;

  ShowImage(dstImg);
end;

procedure TForm8.ApplyThreshToZeroInv;
var dstImg: IImage<Byte>;
begin
  dstImg := TNDAImg<Byte>.Create(fSrcImg.Width, fSrcImg.Height);

  SWStart;
  Threshold(fSrcImg, dstImg, edThreshold.Value, tmToZeroInv);
  SWStop;

  ShowImage(dstImg);
end;

procedure TForm8.ApplyAdaptiveBin;
var dstImg: IImage<Byte>;
    params: TArray<Single>;
    arr: INDArray;
    s: String;
begin
  dstImg := TNDAImg<Byte>.Create(fSrcImg.Width, fSrcImg.Height);

  s := edParams.Text;
  if not s.StartsWith('{') then s := Format('{%s}', [s]);

  if not (
    fParser.Parse(s, arr) and
    TNDAUt.TryAsDynArray<Single>(arr, params) and (Length(params) = 3))
  then begin
    MessageDlg('The adaptive threshold parameters should be a triple of real numbers.',
      mtError, [mbOk], 0);
    exit;
  end;

  SWStart;
  LocalAdaptiveBinarize(fSrcImg, dstImg, edRadius.Value, params);
  SWStop;

  ShowImage(dstImg);
end;

procedure TForm8.ApplyOpening;
var dstImg: IImage<Byte>;
begin
  dstImg := TNDAImg<Byte>.Create(fSrcImg.Width, fSrcImg.Height);

  SWStart;
  Opening(fBinImg, dstImg, edNIter.Value);
  SWStop;

  ShowImage(dstImg);
end;

procedure TForm8.ApplyClosing;
var dstImg: IImage<Byte>;
begin
  dstImg := TNDAImg<Byte>.Create(fSrcImg.Width, fSrcImg.Height);

  SWStart;
  Closing(fBinImg, dstImg, edNIter.Value);
  SWStop;

  ShowImage(dstImg);
end;

procedure TForm8.ApplyDilation;
var dstImg: IImage<Byte>;
begin
  dstImg := TNDAImg<Byte>.Create(fSrcImg.Width, fSrcImg.Height);

  SWStart;
  Dilation(fBinImg, dstImg, edNIter.Value);
  SWStop;

  ShowImage(dstImg);
end;

procedure TForm8.ApplyErosion;
var dstImg: IImage<Byte>;
begin
  dstImg := TNDAImg<Byte>.Create(fSrcImg.Width, fSrcImg.Height);

  SWStart;
  Erosion(fBinImg, dstImg, edNIter.Value);
  SWStop;

  ShowImage(dstImg);
end;

procedure TForm8.ApplyGradient;
var dstImg: IImage<Byte>;
begin
  dstImg := TNDAImg<Byte>.Create(fSrcImg.Width, fSrcImg.Height);

  SWStart;
  MorphGradient(fBinImg, dstImg, edNIter.Value);
  SWStop;

  ShowImage(dstImg);
end;

procedure TForm8.ApplyTopHat;
var dstImg: IImage<Byte>;
begin
  dstImg := TNDAImg<Byte>.Create(fSrcImg.Width, fSrcImg.Height);

  SWStart;
  TopHat(fBinImg, dstImg, edNIter.Value);
  SWStop;

  ShowImage(dstImg);
end;

procedure TForm8.ApplyBottomHat;
var dstImg: IImage<Byte>;
begin
  dstImg := TNDAImg<Byte>.Create(fSrcImg.Width, fSrcImg.Height);

  SWStart;
  BottomHat(fBinImg, dstImg, edNIter.Value);
  SWStop;

  ShowImage(dstImg);
end;

procedure TForm8.btExecuteClick(Sender: TObject);
begin
  case cbMethod.ItemIndex of
    MI_THRESH_BIN:          ApplyThreshBin;
    MI_THRESH_BIN_INV:      ApplyThreshBinInv;
    MI_THRESH_TOZERO:       ApplyThreshToZero;
    MI_THRESH_TOZERO_INV:   ApplyThreshToZeroInv;
    MI_ADAPTIVE_BIN:        ApplyAdaptiveBin;
  end;

  if cbMethod.ItemIndex in [MI_THRESH_BIN, MI_THRESH_BIN_INV, MI_ADAPTIVE_BIN] then
    SetBinImg(fViewImg)
  else
    SetBinImg(nil);
end;

procedure TForm8.btLoadImageClick(Sender: TObject);
var bmp: TBitmap;
    img: IImage<TRGB24>;
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
    img := TBmpRGB24.Create(bmp);
    fSrcImg := TBmpUI8.Create(img.Width, img.Height);
    fSrcImg.SetFlags(NDAF_WRITEABLE);
    ColorConvert(img, fSrcImg);
    ShowImage(fSrcImg);
    btExecute.Enabled := True;
  end;
end;

procedure TForm8.btMExecuteClick(Sender: TObject);
begin
  case cbMMethod.ItemIndex of
    MMI_OPENING:    ApplyOpening;
    MMI_CLOSING:    ApplyClosing;
    mmi_dilation:   ApplyDilation;
    MMI_EROSION:    ApplyErosion;
    MMI_GRADIENT:   ApplyGradient;
    MMI_TOPHAT:     ApplyTopHat;
    MMI_BOTTOMHAT:  ApplyBottomHat;
  end;
end;

procedure TForm8.cbMethodChange(Sender: TObject);
begin
  case cbMethod.ItemIndex of
    MI_THRESH_BIN, MI_THRESH_BIN_INV, MI_THRESH_TOZERO, MI_THRESH_TOZERO_INV: begin
      ShowAdaptiveBinControl(False);
      ShowThreasholdControl(True);
    end;
    MI_ADAPTIVE_BIN: begin
      ShowThreasholdControl(False);
      ShowAdaptiveBinControl(True);
    end;
  end;
end;

end.
