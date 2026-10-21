unit panda.ImgProc.io;

interface

uses
    panda.ImgProc.Types
  , panda.ImgProc.Images
  , panda.ImgProc.VCLImages
  , panda.ImgProc.RLE
  , Vcl.Graphics
  , Vcl.Imaging.jpeg
  , Vcl.Imaging.pngimage
  , Vcl.Imaging.GIFimg
  , System.SysUtils
  , System.Classes
  , System.IOUtils
  , System.TypInfo
  , winapi.Windows
  ;

{$I AsmDefs.inc}

const
  // Image import attributes: flags occupy bits 0..15; the page occupies 16..31.
  IIA_PAGE = $00000001; // Use the zero-based WIC frame index in the upper word.
  IIA_PAGE_SHIFT = 16;
  IIA_PAGE_MASK = Cardinal($FFFF0000);

  // image export attributes
  IEA_RLE   = 1; // RLE encoding - supported only for grayscale bitmap images

/// <summary>
///   Loads an owning image; missing files and unsupported extensions return nil.
///   TIFF grayscale uses Byte, Word or Single; color uses TRGB24, TRGB32,
///   TRGB48, TRGBA64, TRGBF32 or TRGBAF32, preserving decoded channel depth.
/// </summary>
/// <remarks>
///   aAttrs defaults to page zero. IIA_PAGE selects the zero-based WIC frame
///   encoded in bits 16..31; bits 0..15 contain flags. Invalid attributes or
///   page selection for non-TIFF files raise EArgumentException; out-of-range
///   TIFF pages raise EArgumentOutOfRangeException. Invalid TIFF data and
///   unsupported sample formats raise EInvalidGraphic; allocation errors propagate.
///   Packed grayscale expands to Byte intensities; palettes expand to RGB/RGBA.
///   Premultiplied TIFF color is converted to straight alpha at the same depth,
///   with rounding for integers and zero color at zero alpha. No ICC transform
///   or metadata-driven rotation is applied. TIFF rows follow WIC decoded order,
///   with positive WidthStep. Pixels are copied; no decoder or file is retained.
/// </remarks>
function ImportImage(const aFileName: String; aAttrs: Cardinal = 0): IImage;
function ExportImage(const aImg: IImage; const aFileName: String; aAttrs: Cardinal = 0): Boolean;

/// <summary>
///   Loads a supported file into the caller-owned aBmp. Unsupported extensions
///   return False; load errors propagate. TIFF loads page zero as RGB24,
///   discarding alpha and reducing higher-precision channels to eight bits.
/// </summary>
function LoadBitmapFromFile(const AFileName: string; aBmp: Vcl.Graphics.TBitmap): Boolean;
function SaveBitmapToFile(const AFileName: string; aBmp: Vcl.Graphics.TBitmap): Boolean;

implementation

uses
    Winapi.ActiveX
  , Winapi.Wincodec
  ;

const
  // Windows SDK wincodec.h; absent from Delphi 12's Winapi.Wincodec.
  WicPixelFormat96bppRGBFloat: TGUID = '{E3FED78F-E8DB-4ACF-84C1-E97F6136B327}';

procedure WicCheck(aResult: HRESULT);
begin
  if Failed(aResult) then
    raise EInvalidGraphic.CreateFmt('TIFF decoding failed (0x%.8x)', [Cardinal(aResult)]);
end;

function InitializeWic: HRESULT;
begin
  Result := CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
  if Result <> RPC_E_CHANGED_MODE then
    WicCheck(Result);
end;

function LoadTiffBitmap(const aFileName: string; aBmp: Vcl.Graphics.TBitmap): Boolean;
var initResult: HRESULT;
    gr: TWICImage;
    source: IWICBitmapSource;
    rect: WICRect;
    rowSize: UInt64;
    I: Integer;
begin
  initResult := InitializeWic;
  try
    gr := TWICImage.Create;
    try
      gr.LoadFromFile(aFileName);
      // Drop alpha in WIC before VCL can premultiply and round the colors.
      WicCheck(WICConvertBitmapSource(GUID_WICPixelFormat24bppBGR, gr.Handle, source));
      if (gr.Width <= 0) or (gr.Height <= 0) then
        raise EInvalidGraphic.Create('Invalid TIFF dimensions');
      rowSize := UInt64(gr.Width) * 3;
      if ((rowSize + 3) and not UInt64(3)) * UInt64(gr.Height) > UInt64(MaxInt) then
        raise EInvalidGraphic.Create('TIFF bitmap is too large');
      aBmp.PixelFormat := pf24bit;
      aBmp.SetSize(gr.Width, gr.Height);
      rect.X := 0;
      rect.Width := gr.Width;
      rect.Height := 1;
      for I := 0 to gr.Height - 1 do begin
        rect.Y := I;
        WicCheck(source.CopyPixels(@rect, UINT(rowSize), UINT(rowSize),
          aBmp.ScanLine[I]));
      end;
      Result := True;
    finally
      source := nil;
      gr.Free;
    end;
  finally
    if Succeeded(initResult) then
      CoUninitialize;
  end;
end;

{$region 'Bitmap import/export'}

function LoadBitmapFromFile(const AFileName: string; aBmp: Vcl.Graphics.TBitmap): Boolean;
var ext: string;
    gr: TGraphic;
begin
  ext := TPath.GetExtension(AFileName).ToLower;

  if ext.Equals('.tif') or ext.Equals('.tiff') then
    exit(LoadTiffBitmap(AFileName, aBmp));

  if ext.Equals('.jpg') or ext.Equals('.jpeg') then
    gr := TJPEGImage.Create
  else
  if ext.Equals('.png') then
    gr := TPngImage.Create
  else
  if ext.Equals('.gif') then
    gr := TGIFImage.Create
  else
  if ext.Equals('.bmp') then
    gr := Vcl.Graphics.TBitmap.Create
  else
    exit(False);

  try
    gr.LoadFromFile(AFileName);
    aBmp.Assign(gr);
    Result := True;
  finally
    gr.Free;
  end;
end;

function SaveBitmapToFile(const AFileName: string; aBmp: Vcl.Graphics.TBitmap): Boolean;
var ext: string;
    gr: TGraphic;
begin
  ext := TPath.GetExtension(AFileName).ToLower;

  if ext.Equals('.jpg') or ext.Equals('.jpeg') then
    gr := TJPEGImage.Create
  else
  if ext.Equals('.png') then
    gr := TPngImage.Create
  else
  if ext.Equals('.gif') then
    gr := TGIFImage.Create
  else
  if ext.Equals('.bmp') then
    gr := Vcl.Graphics.TBitmap.Create
  else begin
    exit(False);
  end;

  try
    gr.Assign(aBmp);
    gr.SaveToFile(AFileName);
    Result := True;
  finally
    gr.Free;
  end;
end;

{$endregion}

type
  TBmpColorSpace = (
    CS_Unknown, CS_Mono8, CS_Mono16, CS_RGB24, CS_RGB32, CS_RGB555
  );

function BmpCS(const aImg: IImage): TBmpColorSpace;
var ti: PTypeInfo;
    td: PTypeData;
begin
  Result := CS_Unknown;
  ti := aImg.GetItemType;
  td := GetTypeData(ti);
  case ti^.Kind of
    tkInteger:
      case td^.OrdType of
        otUByte: Result := CS_Mono8;
        otUWord: Result := CS_Mono16;
      else
        exit;
      end;
  else
    if ti = TypeInfo(TRGB24) then
      exit(CS_RGB24);
    if ti = TypeInfo(TRGB32) then
      exit(CS_RGB32);
  end;
end;

type
  TCSHelper = record helper for TBmpColorSpace
    function BytesPerPixel: Integer;
  end;

function TCSHelper.BytesPerPixel: Integer;
begin
  case Self of
    CS_Mono8:   Result :=  1;
    CS_Mono16:  Result :=  2;
    CS_RGB24:   Result :=  3;
    CS_RGB32:   Result :=  4;
    CS_RGB555:  Result :=  2;
  else
    Result := -1;
  end;
end;

type
  TPalEntryHelper = record helper for tagPALETTEENTRY
    function IsGray: Boolean;
  end;

function TPalEntryHelper.IsGray: Boolean;
begin
  Result := (peRed = peGreen) and (peGreen = peBlue);
end;

{$region 'Image import functions'}

type
  TTiffPixelKind = (tpkGray8, tpkGray16, tpkGrayFloat, tpkRGB24, tpkRGBA32,
    tpkRGB48, tpkRGBA64, tpkRGBFloat, tpkRGBAFloat);

function TiffMetadataNumbers(const aReader: IWICMetadataQueryReader;
  aTag: Word): TArray<Cardinal>;
var value: PROPVARIANT;
    status: HRESULT;
    I: Integer;
begin
  Result := nil;
  FillChar(value, SizeOf(value), 0);
  try
    status := aReader.GetMetadataByName(PChar(Format('/ifd/{ushort=%d}', [aTag])), value);
    if Cardinal(status) = WINCODEC_ERR_PROPERTYNOTFOUND then exit;
    WicCheck(status);
    case value.vt of
      VT_UI2: Result := TArray<Cardinal>.Create(value.uiVal);
      VT_UI4: Result := TArray<Cardinal>.Create(value.ulVal);
      VT_VECTOR or VT_UI2: begin
        if (value.caui.cElems = 0) or (value.caui.cElems > Cardinal(MaxInt)) then
          raise EInvalidGraphic.Create('Invalid TIFF metadata array');
        SetLength(Result, value.caui.cElems);
        for I := 0 to High(Result) do
          Result[I] := PWord(PByte(value.caui.pElems) + I * SizeOf(Word))^;
      end;
      VT_VECTOR or VT_UI4: begin
        if (value.caul.cElems = 0) or (value.caul.cElems > Cardinal(MaxInt)) then
          raise EInvalidGraphic.Create('Invalid TIFF metadata array');
        SetLength(Result, value.caul.cElems);
        for I := 0 to High(Result) do
          Result[I] := PCardinal(PByte(value.caul.pElems) + I * SizeOf(Cardinal))^;
      end;
    else
      raise EInvalidGraphic.Create('Unsupported TIFF metadata type');
    end;
  finally
    PropVariantClear(value);
  end;
end;

procedure ValidateTiffMetadata(const aFrame: IWICBitmapFrameDecode; aFileSize: Int64);
var reader: IWICMetadataQueryReader;
    formats, bits, offsets, counts: TArray<Cardinal>;
    sampleFormat, bitDepth: Cardinal;
    I: Integer;
begin
  WicCheck(aFrame.GetMetadataQueryReader(reader));
  formats := TiffMetadataNumbers(reader, 339); // SampleFormat defaults to UINT.
  sampleFormat := 1;
  if Length(formats) <> 0 then sampleFormat := formats[0];
  if not (sampleFormat in [1, 3]) then
    raise EInvalidGraphic.Create('Unsupported TIFF sample format');
  for I := 0 to High(formats) do
    if formats[I] <> sampleFormat then
      raise EInvalidGraphic.Create('Mixed TIFF sample formats are not supported');
  bits := TiffMetadataNumbers(reader, 258); // BitsPerSample defaults to one.
  bitDepth := 1;
  if Length(bits) <> 0 then bitDepth := bits[0];
  for I := 0 to High(bits) do
    if bits[I] <> bitDepth then
      raise EInvalidGraphic.Create('Mixed TIFF sample depths are not supported');
  if ((sampleFormat = 1) and not (bitDepth in [1, 2, 4, 8, 16])) or
     ((sampleFormat = 3) and (bitDepth <> 32)) then
    raise EInvalidGraphic.Create('Unsupported TIFF sample depth');

  offsets := TiffMetadataNumbers(reader, 273); // StripOffsets / StripByteCounts.
  counts := TiffMetadataNumbers(reader, 279);
  if (Length(offsets) = 0) and (Length(counts) = 0) then begin
    offsets := TiffMetadataNumbers(reader, 324); // Tiled TIFFs use analogous tags.
    counts := TiffMetadataNumbers(reader, 325);
  end;
  if (Length(offsets) = 0) or (Length(offsets) <> Length(counts)) then
    raise EInvalidGraphic.Create('Invalid TIFF pixel storage');
  for I := 0 to High(offsets) do
    if UInt64(offsets[I]) + counts[I] > UInt64(aFileSize) then
      raise EInvalidGraphic.Create('Truncated TIFF pixel data');
end;

function StraightChannel(aColor, aAlpha, aMaximum: Cardinal): Cardinal;
var value: UInt64;
begin
  if aAlpha = 0 then exit(0);
  value := (UInt64(aColor) * aMaximum + aAlpha div 2) div aAlpha;
  if value > aMaximum then exit(aMaximum);
  Result := Cardinal(value);
end;

procedure FixTiffRow(aRow: PByte; aWidth: Integer; aKind: TTiffPixelKind;
  aSwap, aPremultiplied: Boolean);
var I: Integer;
    tmp: Single;
    tmpByte: Byte;
    tmpWord: Word;
    p24: PRGB24;
    p32: PRGB32;
    p48: PRGB48;
    p64: PRGBA64;
    pf: PRGBF32;
    paf: PRGBAF32;
begin
  if not (aSwap or aPremultiplied) then exit;
  case aKind of
    tpkRGB24: begin
      p24 := PRGB24(aRow);
      for I := 0 to aWidth - 1 do begin
        tmpByte := p24.B;
        p24.B := p24.R;
        p24.R := tmpByte;
        Inc(p24);
      end;
    end;
    tpkRGBA32: begin
      p32 := PRGB32(aRow);
      if aSwap then begin
        if aPremultiplied then begin
          for I := 0 to aWidth - 1 do begin
            tmpByte := p32.B;
            p32.B := StraightChannel(p32.R, p32.A, 255);
            p32.G := StraightChannel(p32.G, p32.A, 255);
            p32.R := StraightChannel(tmpByte, p32.A, 255);
            Inc(p32);
          end;
        end else begin
          for I := 0 to aWidth - 1 do begin
            tmpByte := p32.B;
            p32.B := p32.R;
            p32.R := tmpByte;
            Inc(p32);
          end;
        end;
      end else
      if aPremultiplied then begin
        for I := 0 to aWidth - 1 do begin
          p32.B := StraightChannel(p32.B, p32.A, 255);
          p32.G := StraightChannel(p32.G, p32.A, 255);
          p32.R := StraightChannel(p32.R, p32.A, 255);
          Inc(p32);
        end;
      end;
    end;
    tpkRGB48: begin
      p48 := PRGB48(aRow);
      for I := 0 to aWidth - 1 do begin
        tmpWord := p48.B;
        p48.B := p48.R;
        p48.R := tmpWord;
        Inc(p48);
      end;
    end;
    tpkRGBA64: begin
      p64 := PRGBA64(aRow);
      if aSwap then begin
        if aPremultiplied then begin
          for I := 0 to aWidth - 1 do begin
            tmpWord := p64.B;
            p64.B := StraightChannel(p64.R, p64.A, 65535);
            p64.G := StraightChannel(p64.G, p64.A, 65535);
            p64.R := StraightChannel(tmpWord, p64.A, 65535);
            Inc(p64);
          end;
        end else begin
          for I := 0 to aWidth - 1 do begin
            tmpWord := p64.B;
            p64.B := p64.R;
            p64.R := tmpWord;
            Inc(p64);
          end;
        end;
      end else
      if aPremultiplied then begin
        for I := 0 to aWidth - 1 do begin
          p64.B := StraightChannel(p64.B, p64.A, 65535);
          p64.G := StraightChannel(p64.G, p64.A, 65535);
          p64.R := StraightChannel(p64.R, p64.A, 65535);
          Inc(p64);
        end;
      end;
    end;
    tpkRGBFloat: begin
      pf := PRGBF32(aRow);
      for I := 0 to aWidth - 1 do begin
        tmp := pf.B;
        pf.B := pf.R;
        pf.R := tmp;
        Inc(pf);
      end;
    end;
    tpkRGBAFloat: begin
      paf := PRGBAF32(aRow);
      if aSwap then begin
        if aPremultiplied then begin
          for I := 0 to aWidth - 1 do begin
            if paf.A = 0 then begin
              paf.B := 0;
              paf.G := 0;
              paf.R := 0;
            end else begin
              tmp := paf.B;
              paf.B := paf.R / paf.A;
              paf.G := paf.G / paf.A;
              paf.R := tmp / paf.A;
            end;
            Inc(paf);
          end;
        end else begin
          for I := 0 to aWidth - 1 do begin
            tmp := paf.B;
            paf.B := paf.R;
            paf.R := tmp;
            Inc(paf);
          end;
        end;
      end else
      if aPremultiplied then begin
        for I := 0 to aWidth - 1 do begin
          if paf.A = 0 then begin
            paf.B := 0;
            paf.G := 0;
            paf.R := 0;
          end else begin
            paf.B := paf.B / paf.A;
            paf.G := paf.G / paf.A;
            paf.R := paf.R / paf.A;
          end;
          Inc(paf);
        end;
      end;
    end;
  end;
end;

function DecodeTiff(const aFileName: string; aPage: Cardinal): IImage;
var factory: IWICImagingFactory;
    decoder: IWICBitmapDecoder;
    frame: IWICBitmapFrameDecode;
    source: IWICBitmapSource;
    palette: IWICPalette;
    fileStream: TFileStream;
    stream: IStream;
    pixelFormat, container: TGUID;
    initResult: HRESULT;
    w, h, pages: UINT;
    hasAlpha: BOOL;
    kind: TTiffPixelKind;
    swap, premultiplied: Boolean;
    pixelSize, rowSize: NativeInt;
    rect: WICRect;
    row: PByte;
    I: Integer;
begin
  initResult := InitializeWic;
  fileStream := nil;
  try
    fileStream := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
    stream := TStreamAdapter.Create(fileStream, soReference);
    WicCheck(CoCreateInstance(CLSID_WICImagingFactory, nil, CLSCTX_INPROC_SERVER,
      IWICImagingFactory, factory));
    WicCheck(factory.CreateDecoderFromStream(stream, GUID_VendorMicrosoft,
      WICDecodeMetadataCacheOnDemand, decoder));
    WicCheck(decoder.GetContainerFormat(container));
    if not IsEqualGUID(container, GUID_ContainerFormatTiff) then
      raise EInvalidGraphic.Create('Expected a TIFF container');
    WicCheck(decoder.GetFrameCount(pages));
    if aPage >= pages then
      raise EArgumentOutOfRangeException.Create('TIFF page index is out of range');
    WicCheck(decoder.GetFrame(aPage, frame));
    ValidateTiffMetadata(frame, fileStream.Size);
    WicCheck(frame.GetSize(w, h));
    WicCheck(frame.GetPixelFormat(pixelFormat));
    source := frame;
    swap := False;
    premultiplied := False;

    if IsEqualGUID(pixelFormat, GUID_WICPixelFormat1bppIndexed) or
       IsEqualGUID(pixelFormat, GUID_WICPixelFormat2bppIndexed) or
       IsEqualGUID(pixelFormat, GUID_WICPixelFormat4bppIndexed) or
       IsEqualGUID(pixelFormat, GUID_WICPixelFormat8bppIndexed) then begin
      WicCheck(factory.CreatePalette(palette));
      WicCheck(frame.CopyPalette(palette));
      WicCheck(palette.HasAlpha(hasAlpha));
      if hasAlpha then
        pixelFormat := GUID_WICPixelFormat32bppBGRA
      else
        pixelFormat := GUID_WICPixelFormat24bppBGR;
      WicCheck(WICConvertBitmapSource(pixelFormat, frame, source));
    end else
    if IsEqualGUID(pixelFormat, GUID_WICPixelFormatBlackWhite) or
       IsEqualGUID(pixelFormat, GUID_WICPixelFormat2bppGray) or
       IsEqualGUID(pixelFormat, GUID_WICPixelFormat4bppGray) then begin
      pixelFormat := GUID_WICPixelFormat8bppGray;
      WicCheck(WICConvertBitmapSource(pixelFormat, frame, source));
    end;

    if IsEqualGUID(pixelFormat, GUID_WICPixelFormat8bppGray) then
      kind := tpkGray8
    else if IsEqualGUID(pixelFormat, GUID_WICPixelFormat16bppGray) then
      kind := tpkGray16
    else if IsEqualGUID(pixelFormat, GUID_WICPixelFormat32bppGrayFloat) then
      kind := tpkGrayFloat
    else if IsEqualGUID(pixelFormat, GUID_WICPixelFormat24bppBGR) or
            IsEqualGUID(pixelFormat, GUID_WICPixelFormat24bppRGB) then begin
      kind := tpkRGB24;
      swap := IsEqualGUID(pixelFormat, GUID_WICPixelFormat24bppRGB);
    end else if IsEqualGUID(pixelFormat, GUID_WICPixelFormat32bppBGRA) or
                IsEqualGUID(pixelFormat, GUID_WICPixelFormat32bppPBGRA) then begin
      kind := tpkRGBA32;
      premultiplied := IsEqualGUID(pixelFormat, GUID_WICPixelFormat32bppPBGRA);
    end else if IsEqualGUID(pixelFormat, GUID_WICPixelFormat48bppRGB) or
                IsEqualGUID(pixelFormat, GUID_WICPixelFormat48bppBGR) then begin
      kind := tpkRGB48;
      swap := IsEqualGUID(pixelFormat, GUID_WICPixelFormat48bppRGB);
    end else if IsEqualGUID(pixelFormat, GUID_WICPixelFormat64bppRGBA) or
                IsEqualGUID(pixelFormat, GUID_WICPixelFormat64bppPRGBA) then begin
      kind := tpkRGBA64;
      swap := True;
      premultiplied := IsEqualGUID(pixelFormat, GUID_WICPixelFormat64bppPRGBA);
    end else if IsEqualGUID(pixelFormat, WicPixelFormat96bppRGBFloat) then begin
      kind := tpkRGBFloat;
      swap := True;
    end else if IsEqualGUID(pixelFormat, GUID_WICPixelFormat128bppRGBAFloat) or
                IsEqualGUID(pixelFormat, GUID_WICPixelFormat128bppPRGBAFloat) then begin
      kind := tpkRGBAFloat;
      swap := True;
      premultiplied := IsEqualGUID(pixelFormat, GUID_WICPixelFormat128bppPRGBAFloat);
    end else
      raise EInvalidGraphic.CreateFmt('Unsupported TIFF pixel format %s',
        [GUIDToString(pixelFormat)]);

    case kind of
      tpkGray8: pixelSize := SizeOf(Byte);
      tpkGray16: pixelSize := SizeOf(Word);
      tpkGrayFloat: pixelSize := SizeOf(Single);
      tpkRGB24: pixelSize := SizeOf(TRGB24);
      tpkRGBA32: pixelSize := SizeOf(TRGB32);
      tpkRGB48: pixelSize := SizeOf(TRGB48);
      tpkRGBA64: pixelSize := SizeOf(TRGBA64);
      tpkRGBFloat: pixelSize := SizeOf(TRGBF32);
      tpkRGBAFloat: pixelSize := SizeOf(TRGBAF32);
    else
      raise EInvalidGraphic.Create('Unsupported TIFF sample type');
    end;
    if (w = 0) or (h = 0) or (w > Cardinal(MaxInt)) or (h > Cardinal(MaxInt)) then
      raise EInvalidGraphic.Create('Invalid TIFF dimensions');
    if UInt64(w) * UInt64(pixelSize) > UInt64(High(NativeInt)) then
      raise EInvalidGraphic.Create('TIFF row is too large');
    rowSize := NativeInt(w) * pixelSize;
    if (UInt64(rowSize) > High(UINT)) or
       (UInt64(rowSize) * h > UInt64(High(NativeInt))) then
      raise EInvalidGraphic.Create('TIFF image is too large');

    case kind of
      tpkGray8: Result := TNDAImg<Byte>.Create(w, h);
      tpkGray16: Result := TNDAImg<Word>.Create(w, h);
      tpkGrayFloat: Result := TNDAImg<Single>.Create(w, h);
      tpkRGB24: Result := TNDAImg<TRGB24>.Create(w, h);
      tpkRGBA32: Result := TNDAImg<TRGB32>.Create(w, h);
      tpkRGB48: Result := TNDAImg<TRGB48>.Create(w, h);
      tpkRGBA64: Result := TNDAImg<TRGBA64>.Create(w, h);
      tpkRGBFloat: Result := TNDAImg<TRGBF32>.Create(w, h);
      tpkRGBAFloat: Result := TNDAImg<TRGBAF32>.Create(w, h);
    end;
    rect.X := 0;
    rect.Width := w;
    rect.Height := 1;
    row := Result.Data;
    for I := 0 to Integer(h) - 1 do begin
      rect.Y := I;
      WicCheck(source.CopyPixels(@rect, UINT(rowSize), UINT(rowSize), row));
      FixTiffRow(row, w, kind, swap, premultiplied);
      Inc(row, rowSize);
    end;
  finally
    // Release COM interfaces before balancing this thread's initialization.
    palette := nil;
    source := nil;
    frame := nil;
    decoder := nil;
    stream := nil;
    fileStream.Free;
    factory := nil;
    if Succeeded(initResult) then
      CoUninitialize;
  end;
end;

function BitmapToImage(aBmp: Vcl.Graphics.TBitmap; var aBmpRefCnt: Integer): IImage;
var cs: TBmpColorSpace;
    mc: Boolean;
    I, count, h, w, ws, pxSz, clrsCount: NativeInt;
    pIn, pOut, pOutRow, pEnd: PByte;
    clrs: array [0..255] of PALETTEENTRY;

  function GrayscaleCheck: Boolean;
  var I: Integer;
  begin
    for I := 0 to clrsCount - 1 do
      if not clrs[I].isGray then exit(False);
    Result := True;
  end;

var isGray: Boolean;
    ddbCs: TBmpColorSpace;
    DIB: TDIBSection;
begin
  Result := nil;
  cs := CS_Unknown;
  ddbCs := CS_Unknown;
  mc := aBmp.Monochrome; // two colors
  isGray := False;
  case aBmp.PixelFormat of
    pfDevice: begin{The bitmap is stored as a device-dependent bitmap.}
      FillChar(DIB, SizeOf(DIB), 0);
      GetObject(aBmp.Handle, SizeOf(DIB), @DIB);
      case DIB.dsBm.bmBitsPixel of
        8:  ddbCs := CS_Mono8;
        24: ddbCs := CS_RGB24;
        32: ddbCs := CS_RGB32;
      end;
      clrsCount := GetPaletteEntries(aBmp.Palette, 0, 256, clrs[0]);
      isGray := GrayscaleCheck;
      if isGray then
        cs := CS_Mono8
      else
        cs := ddbCS;
    end;

    //pf1bit: {The bitmap is a device-independent bitmap with one bit per pixel (black and white palette)};

    pf4bit: begin {uses 16-color palette}
      clrsCount := GetPaletteEntries(aBmp.Palette, 0, 256, clrs[0]);
      isGray := GrayscaleCheck;
      if isGray then cs := CS_Mono8
      else cs := CS_RGB24;
    end;

    pf8bit: begin {pf8bit uses palette}
      clrsCount := GetPaletteEntries(aBmp.Palette, 0, 256, clrs[0]);
      isGray := GrayscaleCheck;
      if isGray then cs := CS_Mono8
      else cs := CS_RGB24;
    end;

    pf15bit: cs := CS_RGB555;
    pf16bit: cs := CS_Mono16; //??? it is ambiguous
    pf24bit: cs := CS_RGB24;
    pf32bit: cs := CS_RGB32;

    //pfCustom: {The bitmap uses some other format. TBitmap does not support pfCustom.}
  else
    exit;
  end;

  if cs <> CS_Unknown then begin
    h := aBmp.Height;
    w := aBmp.Width;
    case cs of
      CS_Mono8:
        Result := TNDAImg<Byte>.Create(w, h);
      CS_RGB24: begin
        Result := TBmpRGB24.Create(aBmp);
        Inc(aBmpRefCnt);
        exit;
      end;
    else
      exit;
    end;

    pOutRow := Result.Data;
    pOut := pOutRow;
    count := cs.BytesPerPixel * w;
    ws := Result.WidthStep;

    if aBmp.PixelFormat = pf8bit then begin  //palette is used
      if isGray then begin
        for I := 0 to h - 1 do begin
          pIn := aBmp.ScanLine[I];
          pOut := pOutRow;
          pEnd := pIn + w;
          while pIn < pEnd do begin
            pOut^ := clrs[pIn^].peRed;
            Inc(pOut);
            Inc(pIn);
          end;
          Inc(pOutRow, ws);
        end;
      end else begin
        for I := 0 to h - 1 do begin
          pIn := aBmp.ScanLine[I];
          pOut := pOutRow;
          pEnd := pIn + w;
          while pIn < pEnd do begin
            PRGB24(pOut)^ := PRGB24(@clrs[pIn^])^;
            Inc(pOut, SizeOf(TRGB24));
            Inc(pIn);
          end;
          Inc(pOutRow, ws);
        end;
      end;
    end else
    if (ddbCs <> CS_Unknown) and (cs = CS_Mono8) then begin
      case ddbCS of
//        CS_Mono8: ;
        CS_RGB24: aBmp.PixelFormat := pf24bit;
        CS_RGB32: aBmp.PixelFormat := pf32bit;
      else
        exit;
      end;
      pxSz := ddbCS.BytesPerPixel;
      for I := 0 to h - 1 do begin
        pIn := aBmp.ScanLine[I];
        pOut := pOutRow;
        pEnd := pOut + w;
        while pOut < pEnd do begin
          pOut^ := clrs[pIn^].peRed;
          Inc(pIn, pxSz);
          Inc(pOut);
        end;
        Inc(pOutRow, ws);
      end;
    end else begin
      for I := 0 to h - 1 do begin
        pIn := aBmp.ScanLine[I];
        Move(pIn^, pOut^, count);
        Inc(pOut, ws);
      end;
    end;
  end;
end;

function ImportImage(const aFileName: String; aAttrs: Cardinal): IImage;
var bmp: Vcl.Graphics.TBitmap;
    refCnt: Integer;
    ext: string;
    isTiff: Boolean;
begin
  Result := nil;
  ext := TPath.GetExtension(aFileName).ToLower;
  isTiff := ext.Equals('.tif') or ext.Equals('.tiff');
  if (aAttrs and not (IIA_PAGE_MASK or Cardinal(IIA_PAGE))) <> 0 then
    raise EArgumentException.Create('Unknown image import attributes');
  if ((aAttrs and IIA_PAGE_MASK) <> 0) and ((aAttrs and IIA_PAGE) = 0) then
    raise EArgumentException.Create('A TIFF page index requires IIA_PAGE');
  if ((aAttrs and IIA_PAGE) <> 0) and not isTiff then
    raise EArgumentException.Create('Page selection is supported only for TIFF');
  if not FileExists(aFileName) then exit;

  if isTiff then
    exit(DecodeTiff(aFileName, (aAttrs and IIA_PAGE_MASK) shr IIA_PAGE_SHIFT));

  refCnt := 0;
  bmp := Vcl.Graphics.TBitmap.Create;
  try
    bmp.Canvas.Lock;
    try
      if LoadBitmapFromFile(aFileName, bmp) then
        Result := BitmapToImage(bmp, refCnt);
    finally
      bmp.Canvas.Unlock;
    end;
  finally
    if refCnt = 0 then
      bmp.Free;
  end;
end;

{$endregion}

{$region 'Image export functions'}

procedure SaveBitmapUI8(const aImg: IImage; const aFileName: String);
var bmp: Vcl.Graphics.TBitmap;
    I, J, w, h, ws, palCount: Integer;
    pIn, pOut: PByte;
    pal: array [0..255] of PALETTEENTRY;
    ibmp: IBitmapImage;
begin
  if not Assigned(aImg) then exit;

  if Supports(aImg, IBitmapImage, ibmp) then begin
    ibmp.Bitmap.SaveToFile(aFileName);
    exit;
  end;

  w := aImg.Width;
  h := aImg.Height;
  bmp := VCL.Graphics.TBitmap.Create;
  try
    bmp.Canvas.Lock;
    try
      bmp.SetSize(w, h);
      bmp.PixelFormat := pf8bit;
      FillChar(pal, SizeOf(pal), 0);

      J := 0;
      for I := 0 to 255 do begin
        PInteger(@pal[I])^ := J;
        Inc(J, $010101);
      end;
      palCount := SetDIBColorTable(bmp.Canvas.Handle, 0, 256, pal);

      pIn := aImg.Data;
      ws := aImg.WidthStep;
      for I := h - 1 downto 0 do begin
        pOut := PByte(bmp.ScanLine[I]);
        Move(pIn^, pOut^, w);
        Inc(pIn, ws);
      end;

      bmp.SaveToFile(aFileName);
    finally
      bmp.Canvas.Unlock;
    end;
  finally
    bmp.Free;
  end;
end;

procedure SaveBitmapUI8_RLE(const aImg: IImage; const aFileName: String); overload;
var hdr: BITMAPFILEHEADER;
    pbih: BITMAPINFOHEADER;
    iw: TRLE8ImageWriter;
    I, J, w, h, ws: Integer;
    pRow: PByte;
    pal: array [0..255] of PALETTEENTRY;
    fs: TFileStream;
begin

  w := aImg.Width;
  h := aImg.Height;
  ws := aImg.WidthStep;
  pRow := aImg.Data + (h - 1) * ws;
  iw.Init(w, h);
  for I := h - 1 downto 0 do begin
    iw.WriteRow(pRow);
    Dec(pRow, ws);
  end;
  iw.WriteEndOfImage;

  J := 0;
  for I := 0 to 255 do begin
    PInteger(@pal[I])^ := J;
    Inc(J, $010101);
  end;

  FillChar(pbih, SizeOf(pbih), 0);
  pbih.biSize := SizeOf(BITMAPINFOHEADER);
  pbih.biWidth := w;
  pbih.biHeight := h;
  pbih.biPlanes := 1;
  pbih.biBitCount := 8;
  pbih.biCompression := BI_RLE8;
  pbih.biSizeImage := iw.ByteCount;
  pbih.biClrUsed := Length(pal);

  hdr.bfType := $4d42;
  hdr.bfSize := SizeOf(hdr) + pbih.biSize + pbih.biClrUsed * SizeOf(TRGBQUAD) +
    pbih.biSizeImage;
  hdr.bfReserved1 := 0;
  hdr.bfReserved2 := 0;
  hdr.bfOffBits := SizeOf(hdr) + pbih.biSize + pbih.biClrUsed * SizeOf(TRGBQUAD);

  fs := TFileStream.Create(aFileName, fmCreate or fmOpenWrite);
  try
    fs.Write(hdr, SizeOf(hdr));
    fs.Write(pbih, SizeOf(pbih));
    fs.Write(pal, SizeOf(pal));
    fs.WriteData(iw.Data, iw.ByteCount);
  finally
    fs.Free;
  end;
end;

function ImageToBitmapRGB24(const aImg: IImage): Vcl.Graphics.TBitmap;
var I, Width, Height, inWs: Integer;
    pIn, pInRow, pOut, pEnd: PByte;
    tmp: Byte;
begin
  Width := aImg.Width;
  Height := aImg.Height;
  Result := Vcl.Graphics.TBitmap.Create;
  Result.SetSize(Width, Height);
  Result.PixelFormat := pf24bit;

  pInRow := aImg.Data;
  inWs := aImg.WidthStep;
  case BmpCS(aImg) of
    CS_Mono8:
      for I := 0 to Height - 1 do begin
        pIn := pInRow;
        pOut := PByte(Result.ScanLine[I]);
        pEnd := pOut + 3 * Width;
        while pOut < pEnd do begin
          tmp := pIn^;
          pOut[0] := tmp;
          pOut[1] := tmp;
          pOut[2] := tmp;
          Inc(pOut, 3);
          Inc(pIn);
        end;
        Inc(pInRow, inWs);
      end;

    CS_RGB24:
      for I := 0 to Height - 1 do begin
        pOut := PByte(Result.ScanLine[I]);
        Move(pInRow^, pOut^, 3 * Width);
        Inc(pInRow, inWs);
      end;

    CS_RGB32:
      for I := 0 to Height - 1 do begin
        pIn := pInRow;
        pOut := PByte(Result.ScanLine[I]);
        pEnd := pOut + 3 * Width;
        while pOut < pEnd do begin
          PRGB24(pOut)^ := PRGB24(pIn)^;
          Inc(pIn, SizeOf(TRGB32));
          Inc(pOut, SizeOf(TRGB24));
        end;
        Inc(pInRow, inWs);
      end;
  else
    Result.Free;
    Result := nil;
  end;
end;


function ExportImage(const aImg: IImage; const aFileName: String; aAttrs: Cardinal): Boolean;
var ibmp: IBitmapImage;
    bmp: VCL.Graphics.TBitmap;
begin
  if aFileName.ToLower.EndsWith('.bmp') and
    (aImg.GetItemType = TypeInfo(Byte))
  then begin
    // grayscale bitmap is treated differently because
    // the standard export procedure converts an image to RGB
    if (aAttrs and IEA_RLE) <> 0 then
      SaveBitmapUI8_RLE(aImg, aFileName)
    else
      SaveBitmapUI8(aImg, aFileName);

   exit(True);
  end;

  if Supports(aImg, IBitmapImage, ibmp) then begin
    Result := SaveBitmapToFile(aFileName, ibmp.Bitmap);
    exit;
  end;

  bmp := ImageToBitmapRGB24(aImg);
  if not Assigned(bmp) then exit(False);

  try
    Result := SaveBitmapToFile(aFileName, bmp);
  finally
    bmp.Free;
  end;
end;

{$endregion}

end.
