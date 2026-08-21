unit Unit10;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Samples.Spin

  , panda.Intfs
  , panda.Arrays
  , panda.Arithmetic
  , panda.ArrManip
  , panda.Math
  , panda.Conv
  , panda.fft
  , panda.Nums
  , panda.ImgProc.Types
  , panda.ImgProc.VCLImages
  , panda.ImgProc.Images
  , panda.ImgProc.CSCvt
  , panda.ImgProc.Filters
  , panda.ImgProc.io
  , System.Diagnostics
  , System.UITypes
  ;

type
  TForm10 = class(TForm)
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    Panel2: TPanel;
    Image1: TImage;
    btLoadImg: TButton;
    FileOpenDialog1: TFileOpenDialog;
    edRadius: TSpinEdit;
    btApply: TButton;
    cbFFTConv: TCheckBox;
    Label1: TLabel;
    cbUseSepKer: TCheckBox;
    procedure btLoadImgClick(Sender: TObject);
    procedure btApplyClick(Sender: TObject);
    procedure cbFFTConvClick(Sender: TObject);
  private
    { Private declarations }
    fSrcImg: IImage<Byte>;
    procedure ShowImage(aImg: IImage);
    procedure GaussianBlur(aR: Integer);
    procedure GaussianBlurSep(aR: Integer);
    procedure GaussianBlurFFT(aR: Integer);
  public
    { Public declarations }
  end;

var
  Form10: TForm10;

implementation

{$R *.dfm}

function GaussianKernel(aR: Integer; aDim: Integer = 1): INDArray<Single>;
var s, r: Single;
    x: TTensorF32;
begin
  Assert((0 < aDim) and (aDim <= 2));

  r := aR;
  s := r / 2;
  x := ndaRange(Single(-r), Single(r) + 1);
  Result := ndaExp(x * x / (-2 * s * s));
  Result := TTensorF32(Result) / ndaTotal(Result);

  if aDim = 2 then
    Result := ndaOuter(Result, Result);
end;

procedure TForm10.btApplyClick(Sender: TObject);
begin
  if cbFFTConv.Checked then
    GaussianBlurFFT(edRadius.Value)
  else
  if cbUseSepKer.Checked then
    GaussianBlurSep(edRadius.Value)
  else
    GaussianBlur(edRadius.Value);
end;

procedure TForm10.btLoadImgClick(Sender: TObject);
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
    btApply.Enabled := True;
  end;
end;

procedure TForm10.cbFFTConvClick(Sender: TObject);
begin
  cbUseSepKer.Enabled := not cbFFTConv.Checked;
end;

procedure TForm10.GaussianBlur(aR: Integer);
var tmpf, k: INDArray<Single>;
    b: INDArray<Byte>;
    res: IImage<Byte>;
    sw: TStopWatch;
begin
  k := GaussianKernel(aR, 2);

  sw.Reset;
  sw.Start;

  b := TImgUt.AsArray<Byte>(fSrcImg);
  tmpf := TNDAUt.AsType<Single>(b);
  tmpf := ndaCorrelate(k, tmpf);
  b := TNDAUt.AsType<Byte>(tmpf);

  sw.Stop;
  StatusBar1.Panels[0].Text := Format('Elapsed time: %f [ms]',
    [sw.Elapsed.TotalMilliseconds]
  );

  res := TImgUt.AsImage<Byte>(b);
  ShowImage(res);
end;

procedure TForm10.GaussianBlurSep(aR: Integer);
var tmpf, k: INDArray<Single>;
    b: INDArray<Byte>;
    res: IImage<Byte>;
    sw: TStopWatch;
begin
  k := GaussianKernel(aR);

  sw.Reset;
  sw.Start;

  b := TImgUt.AsArray<Byte>(fSrcImg);
  tmpf := TNDAUt.AsType<Single>(b);
  tmpf := ndaCorrelate(k, tmpf);
  tmpf := TNDAMan.Transpose<Single>(tmpf);
  tmpf := ndaCorrelate(k, tmpf);
  b := TNDAUt.AsType<Byte>(tmpf);
  b := TNDAMan.Transpose<Byte>(b);

  sw.Stop;
  StatusBar1.Panels[0].Text := Format('Elapsed time: %f [ms]',
    [sw.Elapsed.TotalMilliseconds]
  );

  res := TImgUt.AsImage<Byte>(b);
  ShowImage(res);
end;

procedure TForm10.GaussianBlurFFT(aR: Integer);
var tmpf, k: INDArray<Single>;
    sk, ssrc: INDArray<TCmplx64>;
    b: INDArray<Byte>;
    fft: TRealFFTEval2DF32;
    ifft: TRealIFFTEval2DF32;
    res: IImage<Byte>;
    h, w, kh, kw: Integer;
    sw: TStopWatch;
begin
  k := GaussianKernel(aR, 2);
  kh := k.Shape[0];
  kw := k.Shape[1];
  h := fSrcImg.Height;
  w := fSrcImg.Width;
  tmpf := TNDAUt.Full<Single>([h, w], 0);
  tmpf[[NDISpan(0, kh-1), NDISpan(0, kw-1)]] := k;
  k := tmpf;

  sw.Reset;
  sw.Start;

  fft := TRealFFTEval2DF32.Create;
  ifft := TRealIFFTEval2DF32.Create;
  try
    fft.SpectrumLayout := slNative;
    ifft.SpectrumLayout := slNative;
    ifft.Normalize := True;
    fft.Init(h, w);
    ifft.Init(h, w);

    fft.Execute(k, sk);

    b := TImgUt.AsArray<Byte>(fSrcImg);
    tmpf := TNDAUt.AsType<Single>(b);

    fft.Execute(tmpf, ssrc);

    ssrc := TTensorC64(sk) * TTensorC64(ssrc);

    ifft.Execute(ssrc, tmpf);
    b := TNDAUt.AsType<Byte>(tmpf);
  finally
    fft.Free;
    ifft.Free;
  end;

  sw.Stop;
  StatusBar1.Panels[0].Text := Format('Elapsed time: %f [ms]',
    [sw.Elapsed.TotalMilliseconds]
  );

  res := TImgUt.AsImage<Byte>(b[[NDISpan(2*aR, -1), NDISpan(2*aR, -1)]]);
  ShowImage(res);
end;

procedure TForm10.ShowImage(aImg: IImage);
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


end.
