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
  // image export attributes
  IEA_RLE   = 1; // RLE encoding - supported only for grayscale bitmap images

function ImportImage(const aFileName: String): IImage;
function ExportImage(const aImg: IImage; const aFileName: String; aAttrs: Cardinal = 0): Boolean;

function LoadBitmapFromFile(const AFileName: string; aBmp: Vcl.Graphics.TBitmap): Boolean;
function SaveBitmapToFile(const AFileName: string; aBmp: Vcl.Graphics.TBitmap): Boolean;

implementation

{$region 'Bitmap import/export'}

function LoadBitmapFromFile(const AFileName: string; aBmp: Vcl.Graphics.TBitmap): Boolean;
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

function ImportImage(const aFileName: String): IImage;
var bmp: Vcl.Graphics.TBitmap;
    refCnt: Integer;
begin
  if not FileExists(aFileName) then exit;

  bmp := Vcl.Graphics.TBitmap.Create;
  try
    bmp.Canvas.Lock;
    try
      refCnt := 0;
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
