unit panda.NN.ImageEncoder;

interface

uses
    panda.Intfs
  , panda.Arrays
  , panda.ArrManip
  , panda.NN
  , panda.ImgProc.Types
  , panda.ImgProc.Images
  , panda.ImgProc.CSCvt
  , panda.ImgProc.ImgResize
  , System.SysUtils
  ;

type
  TNetImageColorSpace = (nicsRGB, nicsGrayscale);

  TImageNetEncoder = class(TNetEncoder)
  protected
    fWidth, fHeight: NativeInt;
    fColorSpace: TNetImageColorSpace;
    fInterleaving, fDataTransposed: Boolean;
    procedure SetWidth(aValue: NativeInt);
    procedure SetHeight(aValue: NativeInt);
  public
    /// <summary>Creates an image encoder with stretch resizing and normalized RGB output by default.</summary>
    constructor Create(aWidth: NativeInt = 128; aHeight: NativeInt = 128;
      aColorSpace: TNetImageColorSpace = nicsRGB); reintroduce;
    /// <summary>Resizes an 8-bit grayscale or RGB24 image and returns pixel values in [0,1].</summary>
    function Encode(const aInput: IInterface): INDArray<Single>; override;

    /// <summary>Output image width in pixels. Must be positive.</summary>
    property Width: NativeInt read fWidth write SetWidth;
    /// <summary>Output image height in pixels. Must be positive.</summary>
    property Height: NativeInt read fHeight write SetHeight;
    /// <summary>
    /// Number of output channels and their source conversion. nicsRGB produces
    /// three channels; nicsGrayscale produces one channel, converting RGB24
    /// input to grayscale when needed.
    /// </summary>
    property ColorSpace: TNetImageColorSpace read fColorSpace write fColorSpace;
    /// <summary>
    /// Controls where the channel axis appears in the output tensor. True stores
    /// channels next to each pixel: [height,width,channels], or
    /// [width,height,channels] when DataTransposed is also True. False stores
    /// one plane per channel: [channels,height,width], or
    /// [channels,width,height] when DataTransposed is True. A grayscale tensor
    /// has a channel dimension of one.
    /// </summary>
    property Interleaving: Boolean read fInterleaving write fInterleaving;
    /// <summary>
    /// Swaps the output tensor's height and width axes and changes the pixel
    /// order in its contiguous data. False orders pixels by rows (y then x);
    /// True orders them by columns (x then y). This affects tensor layout only;
    /// it does not rotate or transpose the image before resizing.
    /// </summary>
    property DataTransposed: Boolean read fDataTransposed write fDataTransposed;
  end;

implementation


{$region 'TImageNetEncoder'}

function MakeGrayImage(const aInput: IImage; aWidth, aHeight: NativeInt): INDArray<Single>;
var src, dst: IImage<Byte>;
    sDst: IImage<Single>;
begin
  if TCSUt.MatchQ<Byte>(aInput) then
    src := (aInput as IImage<Byte>)
  else begin
    src := TNDAImg<Byte>.Create(aInput.Width, aInput.Height);
    ColorConvert((aInput as IImage<TRGB24>), src);
  end;
  dst := TNDAImg<Byte>.Create(aWidth, aHeight);
  ImageResize(src, dst, imBilinear);
  sDst := TNDAImg<Single>.Create(aWidth, aHeight);
  ColorConvert(dst, sDst);
  Result := TImgUt.AsArray<Single>(sDst);
end;

constructor TImageNetEncoder.Create(aWidth, aHeight: NativeInt;
  aColorSpace: TNetImageColorSpace);
begin
  inherited Create;
  Width := aWidth;
  Height := aHeight;
  fColorSpace := aColorSpace;
end;

procedure TImageNetEncoder.SetWidth(aValue: NativeInt);
begin
  if aValue <= 0 then
    raise EArgumentOutOfRangeException.Create('Image width must be positive.');
  fWidth := aValue;
end;

procedure TImageNetEncoder.SetHeight(aValue: NativeInt);
begin
  if aValue <= 0 then
    raise EArgumentOutOfRangeException.Create('Image height must be positive.');
  fHeight := aValue;
end;

function TImageNetEncoder.Encode(const aInput: IInterface): INDArray<Single>;
var grayImage: IImage<Single>;
    rgbImage: IImage<TRGB24>;
    rgbCh: INDArray<Byte>;
    chf32: INDArray<Single>;
    imageInput: IImage;
begin
  if not Supports(aInput, IImage, imageInput) then
    raise EArgumentException.Create('Image encoder requires an IImage input.');

  if fColorSpace = nicsGrayscale then begin
    Result := MakeGrayImage(imageInput, fWidth, fHeight).Reshape([1, fHeight, fWidth]);
    if fDataTransposed then
      Result := TNDAMan.Transpose<Single>(Result, [0, 2, 1]);
    exit;
  end;

  if TCSUt.MatchQ<TRGB24>(imageInput) then begin
    rgbImage := TNDAImg<TRGB24>.Create(fWidth, fHeight);
    ImageResize(imageInput as IImage<TRGB24>, rgbImage, imBilinear);
    rgbCh := TImgUt.AsArray<TRGB24, Byte>(rgbImage);
    Result := TNDAUt.Empty<Single>(rgbCh.Shape);
    grayImage := TImgUt.AsImage<Single>(Result.Reshape([fHeight, 3*fWidth]));
    ColorConvert(TImgUt.AsImage<Byte>(rgbCh.Reshape([fHeight, 3*fWidth])), grayImage);
    if fInterleaving then begin
      if fDataTransposed then
        Result := TNDAMan.Transpose<Single>(Result, [0, 2, 1])
    end else begin
      if fDataTransposed then
        Result := TNDAMan.Transpose<Single>(Result, [2, 1, 0])
      else
        Result := TNDAMan.Transpose<Single>(Result, [2, 0, 1]);
    end;
  end else begin
    chf32 := MakeGrayImage(imageInput, fWidth, fHeight);
    Result := TNDAUt.Empty<Single>([3, fHeight, fWidth]);
    Result[[NDI([0, 1, 2])]] := chf32;
    if fInterleaving then begin
      if fDataTransposed then
        Result := TNDAMan.Transpose<Single>(Result, [2, 1, 0])
      else
        Result := TNDAMan.Transpose<Single>(Result, [1, 2, 0]);
    end else begin
      if fDataTransposed then
        Result := TNDAMan.Transpose<Single>(Result, [0, 2, 1]);
    end;
  end;
end;

{$endregion}

end.
