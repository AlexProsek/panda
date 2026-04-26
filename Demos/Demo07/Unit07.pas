unit Unit07;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  Vcl.Samples.Spin

  , panda.Intfs
  , panda.Arrays
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
  TForm7 = class(TForm)
    pnTop: TPanel;
    Button1: TButton;
    FileOpenDialog1: TFileOpenDialog;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    cbFilter: TComboBox;
    Label1: TLabel;
    btApply: TButton;
    StatusBar1: TStatusBar;
    lbRadius: TLabel;
    edRadius: TSpinEdit;
    lbKerType: TLabel;
    cbKerType: TComboBox;
    cbParallelize: TCheckBox;
    lbIterCnt: TLabel;
    edIterCnt: TSpinEdit;
    lbLambda: TLabel;
    edLambda: TEdit;
    btAddNoise: TButton;
    procedure btApplyClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbFilterSelect(Sender: TObject);
    procedure btAddNoiseClick(Sender: TObject);
  private
    { Private declarations }
    fSrcImg: IImage<Byte>;
    fSW: TStopWatch;
    procedure SWStart;
    procedure SWStop;
    procedure ShowImage(aImg: IImage);
    function CreateKernel: IImage<Byte>;
    function GetFlags: Cardinal;
    procedure ShowMinMaxFilterControls(aShow: Boolean);
    procedure ShowMedianFilterControls(aShow: Boolean);
    procedure ShowTVFilterControls(aShow: Boolean);
  public
    { Public declarations }
    procedure ApplyMinFilter;
    procedure ApplyMaxFilter;
    procedure ApplyMedianFilter;
    procedure ApplyTVFilter;
  end;

var
  Form7: TForm7;

implementation

{$R *.dfm}

const
  // filter indices
  FIDX_MIN_FILTER     = 0;
  FIDX_MAX_FILTER     = 1;
  FIDX_MEDIAN_FILTER  = 2;
  FIDX_TV_FILTER      = 3;

  // kernel indices
  KIDX_BOX            = 0;
  KIDX_DIAMOND        = 1;
  KIDX_CIRCLE         = 2;
  KIDX_DIAMOND_FULL   = 3;
  KIDX_CIRCLE_FULL    = 4;


function TForm7.CreateKernel: IImage<Byte>;
var ker: INDArray<Byte>;
    r: Integer;
begin
  r := edRadius.Value;

  case cbKerType.ItemIndex of
    KIDX_DIAMOND_FULL:
      ker := TNDAUt.Table2D<Byte>(
        function (y, x: NativeInt): Byte
        begin
          if Abs(x) + Abs(y) <= r then
            Result := 1
          else
            Result := 0;
        end,
        -r, r, -r, r
      );

    KIDX_CIRCLE_FULL:
      ker := TNDAUt.Table2D<Byte>(
        function (y, x: NativeInt): Byte
        begin
          if Sqrt(x*x + y*y) <= r then
            Result := 1
          else
            Result := 0;
        end,
        -r, r, -r, r
      );
  else
    exit(nil);
  end;

  Result := TNDArrAsImg<Byte>.Create(ker);
end;

procedure TForm7.FormCreate(Sender: TObject);
begin
  lbIterCnt.Top := lbRadius.Top;
  edIterCnt.Top := edRadius.Top;
  lbLambda.Top := lbRadius.Top;
  edLambda.Top := edRadius.Top;
  pnTop.Height := 90;

  with cbFilter.Items do begin
    Add('Min filter');
    Add('Max filter');
    Add('Median filter');
    Add('Total variation filter');
  end;
  cbFilter.ItemIndex := FIDX_MIN_FILTER;

  with cbKerType.Items do begin
    Add('Box');
    Add('Diamond');
    Add('Circle');
    Add('Diamond (full kernel)');
    Add('Circle (full kernel)');
  end;
  cbKerType.ItemIndex := KIDX_BOX;

  ShowMinMaxFilterControls(True);
  ShowTVFilterControls(False);
end;

function TForm7.GetFlags: Cardinal;
begin
  Result := 0;
  if cbParallelize.Checked then
    Result := Result or MFF_PARALLELIZE;

  if cbKerType.Visible then begin
    case cbKerType.ItemIndex of
      KIDX_DIAMOND: Result := Result or MFF_DIAMOND_KERNEL;
      KIDX_CIRCLE:  Result := Result or MFF_CIRCLE_KERNEL;
    end;
  end;
end;

procedure TForm7.ApplyMinFilter;
var dst: IImage<Byte>;
    ker: IImage<Byte>;
    flags: Cardinal;
begin
  dst := TBmpUI8.Create(fSrcImg.Width, fSrcImg.Height);

  ker := CreateKernel;
  flags := GetFlags;

  SWStart;
  if Assigned(ker) then
    MinFilter(fSrcImg, dst, ker, flags)
  else
    MinFilter(fSrcImg, dst, edRadius.Value, edRadius.Value, flags);
  SWStop;

  ShowImage(dst);
end;

procedure TForm7.ApplyMaxFilter;
var dst: IImage<Byte>;
    ker: IImage<Byte>;
    flags: Cardinal;
begin
  dst := TBmpUI8.Create(fSrcImg.Width, fSrcImg.Height);

  ker := CreateKernel;
  flags := GetFlags;
  SWStart;
  if Assigned(ker) then
    MaxFilter(fSrcImg, dst, ker, flags)
  else
    MaxFilter(fSrcImg, dst, edRadius.Value, edRadius.Value, flags);
  SWStop;

  ShowImage(dst);
end;

procedure TForm7.ApplyMedianFilter;
var dst: IImage<Byte>;
begin
  dst := TBmpUI8.Create(fSrcImg.Width, fSrcImg.Height);

  SWStart;
  MedianFilter(fSrcImg, dst, edRadius.Value, edRadius.Value);
  SWStop;

  ShowImage(dst);
end;

procedure TForm7.ApplyTVFilter;
var src, dst: IImage<Single>;
    dstui8: IImage<Byte>;
    lambda: Single;
begin
  if not (TryStrToFloat(edLambda.Text, lambda) and (lambda > 0)) then begin
    MessageDlg('Lambda parameter has to be positive real number.', mtError, [mbOk], 0);
    exit;
  end;

  src := TNDAImg<Single>.Create(fSrcImg.Width, fSrcImg.Height);
  ColorConvert(fSrcImg, src);

  SWStart;
  TotalVariationFilter(src, dst, edIterCnt.Value, lambda);
  SWStop;

  ColorConvert(dst, dstui8);
  ShowImage(dstui8);
end;

procedure TForm7.btApplyClick(Sender: TObject);
begin
  case cbFilter.ItemIndex of
    FIDX_MIN_FILTER:    ApplyMinFilter;
    FIDX_MAX_FILTER:    ApplyMaxFilter;
    FIDX_MEDIAN_FILTER: ApplyMedianFilter;
    FIDX_TV_FILTER:     ApplyTVFilter;
  end;
end;

procedure TForm7.btAddNoiseClick(Sender: TObject);
var I, w, h, count: Integer;
    c: TNDAVecItems<TMatIdx>;
    m: TNDAMatItems<Byte>;

  function RandIdxs(aCount: Integer): INDArray<TMatIdx>;
  begin
    Result := TNDAUt.Table1D<TMatIdx>(
      function (aIdx: NativeInt): TMatIdx
      begin
        with Result do begin
          RIdx := Random(h - 1);
          CIdx := Random(w - 1);
        end;
      end,
      0, aCount - 1
    );
  end;

begin
  w := fSrcImg.Width;
  h := fSrcImg.Height;
  count := Round(0.01 * w * h);
  m := TImgUt.AsArray<Byte>(fSrcImg);

  // salt
  c := RandIdxs(count);
  for I := 0 to c.Length - 1 do with c[I] do
    m[RIdx, CIdx] := 255;

  // pepper
  c := RandIdxs(count);
  for I := 0 to c.Length - 1 do with c[I] do
    m[RIdx, CIdx] := 0;

  ShowImage(fSrcImg);
end;

procedure TForm7.SWStart;
begin
  fSW.Reset;
  fSW.Start;
end;

procedure TForm7.SWStop;
begin
  fSW.Stop;
  StatusBar1.Panels[0].Text := Format('Elapsed time: %f [ms]',
    [fSW.Elapsed.TotalMilliseconds]
  );
end;

procedure TForm7.ShowImage(aImg: IImage);
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

procedure TForm7.Button1Click(Sender: TObject);
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
    btAddNoise.Enabled := True;
  end;
end;

procedure TForm7.ShowMinMaxFilterControls(aShow: Boolean);
begin
  lbRadius.Visible := aShow;
  edRadius.Visible := aShow;
  lbKerType.Visible := aShow;
  cbKerType.Visible := aShow;
  cbParallelize.Visible := aShow;
end;

procedure TForm7.ShowMedianFilterControls(aShow: Boolean);
begin
  lbRadius.Visible := aShow;
  edRadius.Visible := aShow;
end;

procedure TForm7.ShowTVFilterControls(aShow: Boolean);
begin
  lbIterCnt.Visible := aShow;
  edIterCnt.Visible := aShow;
  lbLambda.Visible := aShow;
  edLambda.Visible := aShow;
end;

procedure TForm7.cbFilterSelect(Sender: TObject);
begin
  case cbFilter.ItemIndex of
    FIDX_MIN_FILTER, FIDX_MAX_FILTER: begin
      ShowTVFilterControls(False);
      ShowMedianFilterControls(False);
      ShowMinMaxFilterControls(True);
    end;
    FIDX_MEDIAN_FILTER: begin
      ShowMinMaxFilterControls(False);
      ShowTVFilterControls(False);
      ShowMedianFilterControls(True);
    end;
    FIDX_TV_FILTER: begin
      ShowMinMaxFilterControls(False);
      ShowMedianFilterControls(False);
      ShowTVFilterControls(True);
    end;
  end;
end;

end.
