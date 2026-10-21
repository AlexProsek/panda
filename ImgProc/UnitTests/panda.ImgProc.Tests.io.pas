unit panda.ImgProc.Tests.io;

interface

uses
    TestFramework
  , System.SysUtils
  , System.Classes
  , System.IOUtils
  , System.ZLib
  , System.TypInfo
  , Winapi.Windows
  , Winapi.ActiveX
  , Vcl.Graphics
  , panda.ImgProc.Types
  , panda.ImgProc.io
  ;

type
  TTiffPage = record
    Width, Height: Cardinal;
    Bits, Channels, Photo, SampleFormat, Extra, Compression: Word;
    Pixels: TBytes;
    Palette: TArray<Word>;
  end;

  TImageIOTests = class(TTestCase)
  private
    fDir, fFile: string;
    function Page(aBits, aChannels, aPhoto: Word;
      const aPixels: TBytes): TTiffPage;
    procedure WriteTiff(const aPages: array of TTiffPage);
    procedure CheckExclusiveAccess;
    procedure CheckImportError(aException: ExceptClass; aAttrs: Cardinal = 0);
    procedure CheckCompressed(aCompression: Word);
    procedure CheckLegacyFormat(const aExt: string);
    procedure CheckThreadApartment(aInitialize: Boolean);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Gray8Rows;
    procedure DefaultSampleFormat;
    procedure Gray16Precision;
    procedure GrayFloatRange;
    procedure RGB24Channels;
    procedure RGBA32Channels;
    procedure RGB48Precision;
    procedure RGBA64Precision;
    procedure RGBFloatRange;
    procedure RGBAFloatRange;
    procedure Premultiplied32;
    procedure Premultiplied64;
    procedure PremultipliedFloat;
    procedure PackedBlackWhite;
    procedure PackedGray4;
    procedure IndexedPalette;
    procedure DefaultPage;
    procedure SelectedPage;
    procedure PageOutOfRange;
    procedure MaximumPageIndex;
    procedure UnknownFlags;
    procedure IndexWithoutFlag;
    procedure NonTiffPageSelection;
    procedure MissingFile;
    procedure UnsupportedExtension;
    procedure CorruptTiff;
    procedure TruncatedTiff;
    procedure UnsupportedCMYK;
    procedure UnsupportedSignedSamples;
    procedure UnsupportedUnsigned32;
    procedure InvalidDimensions;
    procedure OversizedDimensions;
    procedure LzwTiff;
    procedure ZipTiff;
    procedure MixedCaseExtension;
    procedure BitmapConversion;
    procedure BitmapFloatConversion;
    procedure BitmapRows;
    procedure ExistingApartment;
    procedure UninitializedThread;
    procedure BitmapRegression;
    procedure JpegRegression;
    procedure PngRegression;
    procedure GifRegression;
  end;

implementation

function WordBytes(const aValues: array of Word): TBytes;
begin
  SetLength(Result, Length(aValues) * SizeOf(Word));
  if Length(Result) <> 0 then
    Move(aValues[0], Result[0], Length(Result));
end;

function FloatBytes(const aValues: array of Single): TBytes;
begin
  SetLength(Result, Length(aValues) * SizeOf(Single));
  if Length(Result) <> 0 then
    Move(aValues[0], Result[0], Length(Result));
end;

function LzwBytes(const aPixels: TBytes): TBytes;
var buffer: Cardinal;
    bits, count, I: Integer;

  procedure Code(aValue: Cardinal);
  begin
    buffer := (buffer shl 9) or aValue;
    Inc(bits, 9);
    while bits >= 8 do begin
      Dec(bits, 8);
      Result[count] := Byte((buffer shr bits) and $FF);
      Inc(count);
    end;
    buffer := buffer and ((Cardinal(1) shl bits) - 1);
  end;

begin
  // A clear before every literal keeps every TIFF LZW code nine bits wide.
  SetLength(Result, (Length(aPixels) * 18 + 9 + 7) div 8);
  buffer := 0;
  bits := 0;
  count := 0;
  for I := 0 to High(aPixels) do begin
    Code(256);
    Code(aPixels[I]);
  end;
  Code(257);
  if bits <> 0 then
    Result[count] := Byte(buffer shl (8 - bits));
end;

procedure TImageIOTests.SetUp;
var id: TGUID;
begin
  inherited;
  CreateGUID(id);
  fDir := TPath.Combine(TPath.GetTempPath, 'PandaTiff-' + GUIDToString(id));
  TDirectory.CreateDirectory(fDir);
  fFile := TPath.Combine(fDir, 'sample.tif');
end;

procedure TImageIOTests.TearDown;
var fileName: string;
begin
  // Only remove files in the unique directory owned by this test.
  for fileName in TDirectory.GetFiles(fDir) do
    TFile.Delete(fileName);
  TDirectory.Delete(fDir);
  inherited;
end;

function TImageIOTests.Page(aBits, aChannels, aPhoto: Word;
  const aPixels: TBytes): TTiffPage;
begin
  Result := Default(TTiffPage);
  Result.Width := 1;
  Result.Height := 1;
  Result.Bits := aBits;
  Result.Channels := aChannels;
  Result.Photo := aPhoto;
  Result.SampleFormat := 1;
  Result.Extra := 2; // Unassociated alpha.
  Result.Compression := 1;
  Result.Pixels := aPixels;
end;

procedure TImageIOTests.WriteTiff(const aPages: array of TTiffPage);
var stream: TMemoryStream;
    p: TTiffPage;
    pixels: TBytes;
    count, I, J: Integer;
    link, start, dataOffset, bitsOffset, formatOffset, paletteOffset: Cardinal;
    hasExtra: Boolean;
    padding: Byte;

  procedure W16(aValue: Word);
  begin
    stream.WriteBuffer(aValue, SizeOf(aValue));
  end;

  procedure W32(aValue: Cardinal);
  begin
    stream.WriteBuffer(aValue, SizeOf(aValue));
  end;

  procedure Entry(aTag, aType: Word; aCount, aValue: Cardinal);
  begin
    W16(aTag);
    W16(aType);
    W32(aCount);
    W32(aValue);
  end;

begin
  stream := TMemoryStream.Create;
  try
    padding := 0;
    W16($4949);
    W16(42);
    W32(8);
    link := 4;
    for I := 0 to High(aPages) do begin
      p := aPages[I];
      pixels := p.Pixels;
      if p.Compression = 5 then
        pixels := LzwBytes(p.Pixels)
      else if p.Compression = 8 then
        ZCompress(p.Pixels, pixels);
      if Odd(stream.Position) then
        stream.WriteBuffer(padding, 1);
      start := stream.Position;
      stream.Position := link;
      W32(start);
      stream.Position := start;
      count := 12;
      if p.SampleFormat = 0 then Dec(count);
      hasExtra := (p.Photo = 2) and (p.Channels = 4);
      if hasExtra then Inc(count);
      if Length(p.Palette) <> 0 then Inc(count);
      dataOffset := start + 2 + Cardinal(count) * 12 + 4;
      bitsOffset := p.Bits;
      formatOffset := p.SampleFormat;
      if p.Channels > 1 then begin
        bitsOffset := dataOffset;
        Inc(dataOffset, p.Channels * 2);
        if p.SampleFormat <> 0 then begin
          formatOffset := dataOffset;
          Inc(dataOffset, p.Channels * 2);
        end;
      end;
      paletteOffset := dataOffset;
      Inc(dataOffset, Length(p.Palette) * 2);
      W16(count);
      Entry(256, 4, 1, p.Width);
      Entry(257, 4, 1, p.Height);
      Entry(258, 3, p.Channels, bitsOffset);
      Entry(259, 3, 1, p.Compression);
      Entry(262, 3, 1, p.Photo);
      Entry(273, 4, 1, dataOffset);
      Entry(277, 3, 1, p.Channels);
      Entry(278, 4, 1, p.Height);
      Entry(279, 4, 1, Length(pixels));
      Entry(284, 3, 1, 1);
      // Predictor 1 is valid for uncompressed, LZW and ZIP fixtures.
      Entry(317, 3, 1, 1);
      if Length(p.Palette) <> 0 then
        Entry(320, 3, Length(p.Palette), paletteOffset);
      if hasExtra then
        Entry(338, 3, 1, p.Extra);
      if p.SampleFormat <> 0 then
        Entry(339, 3, p.Channels, formatOffset);
      link := stream.Position;
      W32(0);
      if p.Channels > 1 then begin
        for J := 1 to p.Channels do W16(p.Bits);
        if p.SampleFormat <> 0 then
          for J := 1 to p.Channels do W16(p.SampleFormat);
      end;
      if Length(p.Palette) <> 0 then
        stream.WriteBuffer(p.Palette[0], Length(p.Palette) * 2);
      if Length(pixels) <> 0 then
        stream.WriteBuffer(pixels[0], Length(pixels));
    end;
    stream.SaveToFile(fFile);
  finally
    stream.Free;
  end;
end;

procedure TImageIOTests.CheckExclusiveAccess;
var stream: TFileStream;
begin
  stream := TFileStream.Create(fFile, fmOpenReadWrite or fmShareExclusive);
  try
    CheckTrue(stream.Size > 0);
  finally
    stream.Free;
  end;
end;

procedure TImageIOTests.CheckImportError(aException: ExceptClass; aAttrs: Cardinal);
var raised: Boolean;
begin
  raised := False;

  try
    ImportImage(fFile, aAttrs);
  except
    on E: Exception do begin
      CheckTrue(E.ClassType.InheritsFrom(aException), E.ClassName + ': ' + E.Message);
      raised := True;
    end;
  end;

  CheckTrue(raised, 'Expected ' + aException.ClassName);
end;

procedure TImageIOTests.Gray8Rows;
var p: TTiffPage;
    img: IImage;
begin
  p := Page(8, 1, 1, TBytes.Create(1, 2, 3, 101, 102, 103));
  p.Width := 3;
  p.Height := 2;
  WriteTiff([p]);

  img := ImportImage(fFile);

  CheckTrue(img.GetItemType = TypeInfo(Byte));
  CheckEquals(3, img.Width);
  CheckEquals(2, img.Height);
  CheckEquals(3, img.WidthStep);
  CheckEquals(1, img.Data[0]);
  CheckEquals(3, img.Data[2]);
  CheckEquals(101, img.Data[img.WidthStep]);
  CheckEquals(103, img.Data[img.WidthStep + 2]);
  CheckExclusiveAccess;
  CheckEquals(102, img.Data[img.WidthStep + 1]);
end;

procedure TImageIOTests.Gray16Precision;
var p: TTiffPage;
    img: IImage;
begin
  p := Page(16, 1, 1, WordBytes([1, 12345, 65534]));
  p.Width := 3;
  WriteTiff([p]);

  img := ImportImage(fFile);

  CheckTrue(img.GetItemType = TypeInfo(Word));
  CheckEquals(1, PWord(img.Data)^);
  CheckEquals(12345, PWord(img.Data + 2)^);
  CheckEquals(65534, PWord(img.Data + 4)^);
end;

procedure TImageIOTests.DefaultSampleFormat;
var p: TTiffPage;
    img: IImage;
begin
  p := Page(16, 3, 2, WordBytes([12345, 23456, 34567]));
  p.SampleFormat := 0; // Omit the tag; TIFF defaults to unsigned integers.
  WriteTiff([p]);

  img := ImportImage(fFile);

  CheckTrue(img.GetItemType = TypeInfo(TRGB48));
  CheckEquals(12345, PRGB48(img.Data).R);
  CheckEquals(34567, PRGB48(img.Data).B);
end;

procedure TImageIOTests.GrayFloatRange;
var p: TTiffPage;
    img: IImage;
begin
  p := Page(32, 1, 1, FloatBytes([-2.5, 0.125, 12.75]));
  p.Width := 3;
  p.SampleFormat := 3;
  WriteTiff([p]);

  img := ImportImage(fFile);

  CheckTrue(img.GetItemType = TypeInfo(Single));
  CheckEquals(-2.5, PSingle(img.Data)^, 0);
  CheckEquals(0.125, PSingle(img.Data + 4)^, 0);
  CheckEquals(12.75, PSingle(img.Data + 8)^, 0);
end;

procedure TImageIOTests.RGB24Channels;
var p: TTiffPage;
    img: IImage;
begin
  p := Page(8, 3, 2, TBytes.Create(11, 22, 33, 44, 55, 66,
    77, 88, 99, 101, 102, 103, 104, 105, 106, 107, 108, 109));
  p.Width := 3;
  p.Height := 2;
  WriteTiff([p]);

  img := ImportImage(fFile);

  CheckTrue(img.GetItemType = TypeInfo(TRGB24));
  CheckEquals(9, img.WidthStep);
  CheckEquals(11, PRGB24(img.Data).R);
  CheckEquals(22, PRGB24(img.Data).G);
  CheckEquals(33, PRGB24(img.Data).B);
  CheckEquals(101, PRGB24(img.Data + img.WidthStep).R);
  CheckEquals(109, PRGB24(img.Data + img.WidthStep + 6).B);
end;

procedure TImageIOTests.RGBA32Channels;
var img: IImage;
begin
  WriteTiff([Page(8, 4, 2, TBytes.Create(11, 22, 33, 64))]);

  img := ImportImage(fFile);

  CheckTrue(img.GetItemType = TypeInfo(TRGB32));
  CheckEquals(11, PRGB32(img.Data).R);
  CheckEquals(22, PRGB32(img.Data).G);
  CheckEquals(33, PRGB32(img.Data).B);
  CheckEquals(64, PRGB32(img.Data).A);
end;

procedure TImageIOTests.RGB48Precision;
var img: IImage;
begin
  WriteTiff([Page(16, 3, 2, WordBytes([12345, 23456, 34567]))]);

  img := ImportImage(fFile);

  CheckTrue(img.GetItemType = TypeInfo(TRGB48));
  CheckEquals(12345, PRGB48(img.Data).R);
  CheckEquals(23456, PRGB48(img.Data).G);
  CheckEquals(34567, PRGB48(img.Data).B);
end;

procedure TImageIOTests.RGBA64Precision;
var img: IImage;
begin
  WriteTiff([Page(16, 4, 2, WordBytes([12345, 23456, 34567, 45678]))]);

  img := ImportImage(fFile);

  CheckTrue(img.GetItemType = TypeInfo(TRGBA64));
  CheckEquals(12345, PRGBA64(img.Data).R);
  CheckEquals(23456, PRGBA64(img.Data).G);
  CheckEquals(34567, PRGBA64(img.Data).B);
  CheckEquals(45678, PRGBA64(img.Data).A);
end;

procedure TImageIOTests.RGBFloatRange;
var p: TTiffPage;
    img: IImage;
begin
  p := Page(32, 3, 2, FloatBytes([-2.5, 0.125, 12.75]));
  p.SampleFormat := 3;
  WriteTiff([p]);

  img := ImportImage(fFile);

  CheckTrue(img.GetItemType = TypeInfo(TRGBF32));
  CheckEquals(-2.5, PRGBF32(img.Data).R, 0);
  CheckEquals(0.125, PRGBF32(img.Data).G, 0);
  CheckEquals(12.75, PRGBF32(img.Data).B, 0);
end;

procedure TImageIOTests.RGBAFloatRange;
var p: TTiffPage;
    img: IImage;
begin
  p := Page(32, 4, 2, FloatBytes([-2.5, 0.125, 12.75, 0.5]));
  p.SampleFormat := 3;
  WriteTiff([p]);

  img := ImportImage(fFile);

  CheckTrue(img.GetItemType = TypeInfo(TRGBAF32));
  CheckEquals(-2.5, PRGBAF32(img.Data).R, 0);
  CheckEquals(0.125, PRGBAF32(img.Data).G, 0);
  CheckEquals(12.75, PRGBAF32(img.Data).B, 0);
  CheckEquals(0.5, PRGBAF32(img.Data).A, 0);
end;

procedure TImageIOTests.Premultiplied32;
var p: TTiffPage;
    img: IImage;
begin
  p := Page(8, 4, 2, TBytes.Create(16, 32, 64, 128, 1, 2, 3, 0));
  p.Width := 2;
  p.Extra := 1;
  WriteTiff([p]);

  img := ImportImage(fFile);

  CheckTrue(img.GetItemType = TypeInfo(TRGB32));
  CheckEquals(32, PRGB32(img.Data).R);
  CheckEquals(64, PRGB32(img.Data).G);
  CheckEquals(128, PRGB32(img.Data).B);
  CheckEquals(128, PRGB32(img.Data).A);
  CheckEquals(0, PRGB32(img.Data + 4).R);
  CheckEquals(0, PRGB32(img.Data + 4).G);
  CheckEquals(0, PRGB32(img.Data + 4).B);
end;

procedure TImageIOTests.Premultiplied64;
var p: TTiffPage;
    img: IImage;
begin
  p := Page(16, 4, 2, WordBytes([8192, 16384, 32768, 32768, 1, 2, 3, 0]));
  p.Width := 2;
  p.Extra := 1;
  WriteTiff([p]);

  img := ImportImage(fFile);

  CheckTrue(img.GetItemType = TypeInfo(TRGBA64));
  CheckEquals(16384, PRGBA64(img.Data).R);
  CheckEquals(32768, PRGBA64(img.Data).G);
  CheckEquals(65535, PRGBA64(img.Data).B);
  CheckEquals(32768, PRGBA64(img.Data).A);
  CheckEquals(0, PRGBA64(img.Data + 8).R);
  CheckEquals(0, PRGBA64(img.Data + 8).G);
  CheckEquals(0, PRGBA64(img.Data + 8).B);
end;

procedure TImageIOTests.PremultipliedFloat;
var p: TTiffPage;
    img: IImage;
begin
  p := Page(32, 4, 2, FloatBytes([-1.25, 0.0625, 6.375, 0.5, 1, 2, 3, 0]));
  p.Width := 2;
  p.Extra := 1;
  p.SampleFormat := 3;
  WriteTiff([p]);

  img := ImportImage(fFile);

  CheckTrue(img.GetItemType = TypeInfo(TRGBAF32));
  CheckEquals(-2.5, PRGBAF32(img.Data).R, 0);
  CheckEquals(0.125, PRGBAF32(img.Data).G, 0);
  CheckEquals(12.75, PRGBAF32(img.Data).B, 0);
  CheckEquals(0.5, PRGBAF32(img.Data).A, 0);
  CheckEquals(0, PRGBAF32(img.Data + 16).R, 0);
  CheckEquals(0, PRGBAF32(img.Data + 16).G, 0);
  CheckEquals(0, PRGBAF32(img.Data + 16).B, 0);
end;

procedure TImageIOTests.PackedBlackWhite;
var p: TTiffPage;
    img: IImage;
begin
  p := Page(1, 1, 1, TBytes.Create($A0));
  p.Width := 3;
  WriteTiff([p]);

  img := ImportImage(fFile);

  CheckTrue(img.GetItemType = TypeInfo(Byte));
  CheckEquals(255, img.Data[0]);
  CheckEquals(0, img.Data[1]);
  CheckEquals(255, img.Data[2]);
end;

procedure TImageIOTests.PackedGray4;
var p: TTiffPage;
    img: IImage;
begin
  p := Page(4, 1, 1, TBytes.Create($05, $F0));
  p.Width := 3;
  WriteTiff([p]);

  img := ImportImage(fFile);

  CheckTrue(img.GetItemType = TypeInfo(Byte));
  CheckEquals(0, img.Data[0]);
  CheckEquals(85, img.Data[1]);
  CheckEquals(255, img.Data[2]);
end;

procedure TImageIOTests.IndexedPalette;
var p: TTiffPage;
    img: IImage;
begin
  p := Page(1, 1, 3, TBytes.Create($40));
  p.Width := 2;
  // TIFF ColorMap stores all reds, then greens, then blues.
  p.Palette := TArray<Word>.Create(65535, 0, 0, 65535, 0, 0);
  WriteTiff([p]);

  img := ImportImage(fFile);

  CheckTrue(img.GetItemType = TypeInfo(TRGB24));
  CheckEquals(255, PRGB24(img.Data).R);
  CheckEquals(0, PRGB24(img.Data).G);
  CheckEquals(0, PRGB24(img.Data).B);
  CheckEquals(0, PRGB24(img.Data + 3).R);
  CheckEquals(255, PRGB24(img.Data + 3).G);
end;

procedure TImageIOTests.DefaultPage;
var img: IImage;
begin
  WriteTiff([Page(8, 1, 1, TBytes.Create(17)),
    Page(16, 1, 1, WordBytes([12345]))]);

  img := ImportImage(fFile);

  CheckTrue(img.GetItemType = TypeInfo(Byte));
  CheckEquals(17, img.Data^);
end;

procedure TImageIOTests.SelectedPage;
var p: TTiffPage;
    img: IImage;
begin
  p := Page(16, 1, 1, WordBytes([12345, 54321]));
  p.Width := 2;
  WriteTiff([Page(8, 1, 1, TBytes.Create(17)), p]);

  img := ImportImage(fFile, IIA_PAGE or (Cardinal(1) shl IIA_PAGE_SHIFT));

  CheckTrue(img.GetItemType = TypeInfo(Word));
  CheckEquals(2, img.Width);
  CheckEquals(12345, PWord(img.Data)^);
  CheckEquals(54321, PWord(img.Data + 2)^);
  CheckExclusiveAccess;
end;

procedure TImageIOTests.PageOutOfRange;
begin
  WriteTiff([Page(8, 1, 1, TBytes.Create(17))]);

  CheckImportError(EArgumentOutOfRangeException,
    IIA_PAGE or (Cardinal(1) shl IIA_PAGE_SHIFT));

  CheckExclusiveAccess;
end;

procedure TImageIOTests.MaximumPageIndex;
begin
  WriteTiff([Page(8, 1, 1, TBytes.Create(17))]);

  CheckImportError(EArgumentOutOfRangeException, IIA_PAGE or IIA_PAGE_MASK);
end;

procedure TImageIOTests.UnknownFlags;
begin
  CheckImportError(EArgumentException, $00008000);
end;

procedure TImageIOTests.IndexWithoutFlag;
begin
  CheckImportError(EArgumentException, Cardinal(1) shl IIA_PAGE_SHIFT);
end;

procedure TImageIOTests.NonTiffPageSelection;
begin
  fFile := TPath.Combine(fDir, 'sample.png');

  CheckImportError(EArgumentException, IIA_PAGE);
end;

procedure TImageIOTests.MissingFile;
begin
  CheckTrue(ImportImage(fFile) = nil);
end;

procedure TImageIOTests.UnsupportedExtension;
begin
  fFile := TPath.Combine(fDir, 'sample.unknown');
  TFile.WriteAllBytes(fFile, TBytes.Create(1, 2, 3));

  CheckTrue(ImportImage(fFile) = nil);
end;

procedure TImageIOTests.CorruptTiff;
begin
  TFile.WriteAllBytes(fFile, TBytes.Create(1, 2, 3, 4));

  CheckImportError(EInvalidGraphic);

  CheckExclusiveAccess;
end;

procedure TImageIOTests.TruncatedTiff;
var stream: TFileStream;
begin
  WriteTiff([Page(16, 3, 2, WordBytes([12345, 23456, 34567]))]);
  stream := TFileStream.Create(fFile, fmOpenReadWrite);
  try
    stream.Size := stream.Size - 5;
  finally
    stream.Free;
  end;

  CheckImportError(EInvalidGraphic);

  CheckExclusiveAccess;
end;

procedure TImageIOTests.UnsupportedCMYK;
begin
  WriteTiff([Page(8, 4, 5, TBytes.Create(11, 22, 33, 44))]);

  CheckImportError(EInvalidGraphic);

  CheckExclusiveAccess;
end;

procedure TImageIOTests.UnsupportedSignedSamples;
var p: TTiffPage;
begin
  p := Page(16, 1, 1, WordBytes([12345]));
  p.SampleFormat := 2;
  WriteTiff([p]);

  CheckImportError(EInvalidGraphic);
end;

procedure TImageIOTests.UnsupportedUnsigned32;
begin
  WriteTiff([Page(32, 1, 1, TBytes.Create(1, 2, 3, 4))]);

  CheckImportError(EInvalidGraphic);
end;

procedure TImageIOTests.InvalidDimensions;
var p: TTiffPage;
begin
  p := Page(8, 1, 1, TBytes.Create(17));
  p.Width := 0;
  WriteTiff([p]);

  CheckImportError(EInvalidGraphic);
end;

procedure TImageIOTests.OversizedDimensions;
var p: TTiffPage;
begin
  p := Page(16, 4, 2, WordBytes([1, 2, 3, 4]));
  p.Width := MaxInt;
  p.Height := MaxInt;
  WriteTiff([p]);

  CheckImportError(EInvalidGraphic);
end;

procedure TImageIOTests.CheckCompressed(aCompression: Word);
var p: TTiffPage;
    img: IImage;
begin
  p := Page(8, 1, 1, TBytes.Create(1, 2, 3, 101, 102, 103));
  p.Width := 3;
  p.Height := 2;
  p.Compression := aCompression;
  WriteTiff([p]);

  img := ImportImage(fFile);

  CheckTrue(img.GetItemType = TypeInfo(Byte));
  CheckEquals(1, img.Data[0]);
  CheckEquals(3, img.Data[2]);
  CheckEquals(101, img.Data[img.WidthStep]);
  CheckEquals(103, img.Data[img.WidthStep + 2]);
end;

procedure TImageIOTests.LzwTiff;
begin
  CheckCompressed(5);
end;

procedure TImageIOTests.ZipTiff;
begin
  CheckCompressed(8);
end;

procedure TImageIOTests.MixedCaseExtension;
var img: IImage;
begin
  fFile := TPath.Combine(fDir, 'sample.TiFf');
  WriteTiff([Page(8, 1, 1, TBytes.Create(17))]);

  img := ImportImage(fFile, IIA_PAGE);

  CheckEquals(17, img.Data^);
end;

procedure TImageIOTests.BitmapConversion;
var bmp: TBitmap;
begin
  WriteTiff([Page(8, 4, 2, TBytes.Create(11, 22, 33, 64)),
    Page(8, 1, 1, TBytes.Create(17))]);
  bmp := TBitmap.Create;
  try
    CheckTrue(LoadBitmapFromFile(fFile, bmp));

    CheckTrue(bmp.PixelFormat = pf24bit);
    CheckEquals(11, PRGB24(bmp.ScanLine[0]).R);
    CheckEquals(22, PRGB24(bmp.ScanLine[0]).G);
    CheckEquals(33, PRGB24(bmp.ScanLine[0]).B);
    CheckExclusiveAccess;
  finally
    bmp.Free;
  end;
end;

procedure TImageIOTests.BitmapFloatConversion;
var p: TTiffPage;
    bmp: TBitmap;
begin
  p := Page(32, 1, 1, FloatBytes([0.5]));
  p.SampleFormat := 3;
  WriteTiff([p]);
  bmp := TBitmap.Create;
  try
    CheckTrue(LoadBitmapFromFile(fFile, bmp));

    CheckTrue(bmp.PixelFormat = pf24bit);
    CheckEquals(1, bmp.Width);
    CheckExclusiveAccess;
  finally
    bmp.Free;
  end;
end;

procedure TImageIOTests.BitmapRows;
var p: TTiffPage;
    bmp: TBitmap;
begin
  p := Page(8, 1, 1, TBytes.Create(1, 2, 3, 101, 102, 103));
  p.Width := 3;
  p.Height := 2;
  WriteTiff([p]);
  bmp := TBitmap.Create;
  try
    CheckTrue(LoadBitmapFromFile(fFile, bmp));

    CheckTrue(bmp.PixelFormat = pf24bit);
    CheckEquals(1, GetRValue(ColorToRGB(bmp.Canvas.Pixels[0, 0])));
    CheckEquals(3, GetRValue(ColorToRGB(bmp.Canvas.Pixels[2, 0])));
    CheckEquals(101, GetRValue(ColorToRGB(bmp.Canvas.Pixels[0, 1])));
    CheckEquals(103, GetRValue(ColorToRGB(bmp.Canvas.Pixels[2, 1])));
  finally
    bmp.Free;
  end;
end;

procedure TImageIOTests.CheckThreadApartment(aInitialize: Boolean);
var worker: TThread;
    error: string;
    actual: Integer;
    probe: HRESULT;
begin
  WriteTiff([Page(8, 1, 1, TBytes.Create(17))]);
  actual := -1;
  probe := E_FAIL;
  worker := TThread.CreateAnonymousThread(
    procedure
    var img: IImage;
        bmp: TBitmap;
        initResult, probeResult: HRESULT;
    begin
      initResult := E_FAIL;
      try
        if aInitialize then
          initResult := CoInitializeEx(nil, COINIT_MULTITHREADED);
        try
          img := ImportImage(fFile);
          actual := img.Data^;
          bmp := TBitmap.Create;
          try
            LoadBitmapFromFile(fFile, bmp);
          finally
            bmp.Free;
          end;
          probeResult := CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
          probe := probeResult;
          if Succeeded(probeResult) then
            CoUninitialize;
        finally
          if Succeeded(initResult) then
            CoUninitialize;
        end;
      except
        on E: Exception do error := E.ClassName + ': ' + E.Message;
      end;
    end);
  worker.FreeOnTerminate := False;

  worker.Start;
  worker.WaitFor;
  worker.Free;

  CheckEquals('', error);
  CheckEquals(17, actual);
  if aInitialize then
    CheckEquals(Integer(RPC_E_CHANGED_MODE), Integer(probe))
  else
    CheckEquals(Integer(S_OK), Integer(probe));
end;

procedure TImageIOTests.ExistingApartment;
begin
  CheckThreadApartment(True);
end;

procedure TImageIOTests.UninitializedThread;
begin
  CheckThreadApartment(False);
end;

procedure TImageIOTests.CheckLegacyFormat(const aExt: string);
var bmp, loaded: TBitmap;
begin
  fFile := TPath.Combine(fDir, 'sample' + aExt);
  bmp := TBitmap.Create;
  loaded := TBitmap.Create;
  try
    bmp.PixelFormat := pf24bit;
    bmp.SetSize(3, 2);
    bmp.Canvas.Brush.Color := clRed;
    bmp.Canvas.FillRect(Rect(0, 0, 3, 2));
    CheckTrue(SaveBitmapToFile(fFile, bmp));

    CheckTrue(LoadBitmapFromFile(fFile, loaded));

    CheckEquals(3, loaded.Width);
    CheckEquals(2, loaded.Height);
    CheckTrue(GetRValue(ColorToRGB(loaded.Canvas.Pixels[0, 0])) > 200);
  finally
    loaded.Free;
    bmp.Free;
  end;
end;

procedure TImageIOTests.BitmapRegression;
begin
  CheckLegacyFormat('.bmp');
end;

procedure TImageIOTests.JpegRegression;
begin
  CheckLegacyFormat('.jpg');
end;

procedure TImageIOTests.PngRegression;
begin
  CheckLegacyFormat('.png');
end;

procedure TImageIOTests.GifRegression;
begin
  CheckLegacyFormat('.gif');
end;

initialization

  RegisterTest(TImageIOTests.Suite);

end.
