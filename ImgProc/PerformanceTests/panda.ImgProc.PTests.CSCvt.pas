unit panda.ImgProc.PTests.CSCvt;

interface

uses
    TestFramework
  , panda.ImgProc.Types
  , panda.ImgProc.Images
  , panda.ImgProc.CsCvt
  , panda.Tests.NDATestCase
  , panda.Intfs
  , panda.Arrays
  , panda.ArrManip
  ;

type
  TCsCvtTests = class(TNDAPerformanceTestCase)
  published
    procedure TestRGB24ToUI8;
    procedure TestRGB24ToF32;
    procedure TestUI8ToRGB24;
    procedure TestUI8ToF32;
    procedure TestF32ToUI8;
    procedure TestRGB24ToHue;
    procedure TestRGB24ToHSV;
    procedure TestRGB24ToHue_Red;

    procedure TestInterleaveUI8C3;

    procedure CvtHSVToRGB;
    procedure CombineHSVToRGB24;

    procedure SepRGB24ToBGR;
    procedure SepRGB24ToBGR_F32;
    procedure CombBGRToRGB24;
    procedure CombBGRToRGB24_F32;
  end;

implementation

{$region 'TCsCvtTests'}

procedure TCsCvtTests.TestRGB24ToUI8;
var src: TArray<TRGB24>;
    dst: TArray<Byte>;
const N = 1000000;
begin
  SetLength(src, N);
  SetLength(dst, N);

  SWStart;
  cscvtRGB24ToUI8(PByte(src), PByte(dst), N);
  SWStop;
end;

procedure TCsCvtTests.TestRGB24ToF32;
var src: TArray<TRGB24>;
    dst: TArray<Single>;
const N = 1000000;
begin
  SetLength(src, N);
  SetLength(dst, N);

  SWStart;
  cscvtRGB24ToF32(PByte(src), PByte(dst), N);
  SWStop;
end;

procedure TCsCvtTests.TestUI8ToRGB24;
var src: TArray<Byte>;
    dst: TArray<TRGB24>;
const N = 1000000;
begin
  SetLength(src, N);
  SetLength(dst, N);

  SWStart;
  cscvtUI8ToRGB24(PByte(src), PByte(dst), N);
  SWStop;
end;

procedure TCsCvtTests.TestUI8ToF32;
var src: TArray<Byte>;
    dst: TArray<Single>;
const N = 1000000;
begin
  SetLength(src, N);
  SetLength(dst, N);

  SWStart;
  cscvtUI8ToF32(PByte(src), PByte(dst), N);
  SWStop;
end;

procedure TCsCvtTests.TestF32ToUI8;
var src: TArray<Single>;
    dst: TArray<Byte>;
const N = 1000000;
begin
  SetLength(src, N);
  SetLength(dst, N);

  SWStart;
  cscvtF32ToUI8(PByte(src), PByte(dst), N);
  SWStop;
end;

procedure TCsCvtTests.TestRGB24ToHue;
var src: TArray<TRGB24>;
    dst: TArray<Single>;
    I: Integer;
const N = 1000000;
begin
  SetLength(src, N);
  SetLength(dst, N);
  RandSeed := 1234;
  for I := 0 to High(src) do with src[I] do begin
    R := Random(255);
    G := Random(255);
    B := Random(255);
  end;

  SWStart;
  cscvtRGB24ToHue(PByte(src), PByte(dst), N);
  SWStop;
end;

procedure TCsCvtTests.TestRGB24ToHSV;
var src: TArray<TRGB24>;
    h, s, v: TArray<Single>;
    I: Integer;
const N = 1000000;
begin
  SetLength(src, N);
  SetLength(h, N);
  SetLength(s, N);
  SetLength(v, N);
  RandSeed := 1234;
  for I := 0 to High(src) do with src[I] do begin
    R := Random(255);
    G := Random(255);
    B := Random(255);
  end;

  SWStart;
  cssepRGB24ToHSV(PByte(src), PByte(h), PByte(s), PByte(v), N);
  SWStop;
end;

procedure TCsCvtTests.TestRGB24ToHue_Red;
var src: TArray<TRGB24>;
    h, s, v: TArray<Single>;
    I: Integer;
const N = 1000000;
begin
  SetLength(src, N);
  SetLength(h, N);
  SetLength(s, N);
  SetLength(v, N);
  RandSeed := 1234;
  for I := 0 to High(src) do with src[I] do begin
    R := 255;
    G := 0;
    B := 0;
  end;

  SWStart;
  cscvtRGB24ToHue(PByte(src), PByte(h), N);
  SWStop;
end;

procedure TCsCvtTests.TestInterleaveUI8C3;
var ch0, ch1, ch2, res: TArray<Byte>;
const N = 10000000;
begin
  SetLength(ch0, N);
  SetLength(ch1, N);
  SetLength(ch2, N);
  SetLength(res, 3*N);

  SWStart;
  _interleaveUI8C3(PByte(ch0), PByte(ch1), PByte(ch2), PByte(res), N);
  SWStop;
end;

procedure TCsCvtTests.CvtHSVToRGB;
var h, s, v, r, g, b: TArray<Single>;
    I: Integer;
const N = 1000000;
begin
  SetLength(h, N);
  SetLength(s, N);
  SetLength(v, N);
  SetLength(r, N);
  SetLength(g, N);
  SetLength(b, N);
  RandSeed := 1234;
  for I := 0 to High(h) do begin
    h[I] := Random();
    s[I] := Random();
    v[I] := Random();
  end;

  SWStart;
  cscvtHSVToRGB(PSingle(h), PSingle(s), PSingle(v), PSingle(r), PSingle(g), PSingle(b), N);
  SWStop;
end;

procedure TCsCvtTests.CombineHSVToRGB24;
var h, s, v: TArray<Single>;
    rgb: TArray<TRGB24>;
    I: Integer;
const N = 1000000;
begin
  SetLength(h, N);
  SetLength(s, N);
  SetLength(v, N);
  SetLength(rgb, N);
  RandSeed := 1234;
  for I := 0 to High(h) do begin
    h[I] := Random();
    s[I] := Random();
    v[I] := Random();
  end;

  SWStart;
  _combineHSVToRGB24(PByte(h), PByte(s), PByte(v), PByte(rgb), N);
  SWStop;
end;


procedure TCsCvtTests.SepRGB24ToBGR;
var rgb: IImage<TRGB24>;
    r, g, b: IImage<Byte>;
    rgbArr, chArr: INDArray<Byte>;
const
  w = 1024;
  h = 768;
begin
  rgb := TImgUt.ConstantImage<TRGB24>(w, h, RGB24(0, 0, 0));
  r := TImgUt.ConstantImage<Byte>(w, h, 0);
  g := TImgUt.ConstantImage<Byte>(w, h, 0);
  b := TImgUt.ConstantImage<Byte>(w, h, 0);

  SWStart;
  RGBSeparate(rgb, r, g, b);
  SWStop;

  rgbArr := TImgUt.AsArray<TRGB24, Byte>(rgb);
  chArr := TNDAUt.Full<Byte>([3, h, w], 0);

  SWStart;
  TNDAMan.Transpose<Byte>(rgbArr, chArr, [2, 0, 1]);
  SWStop('transposition');
end;

procedure TCsCvtTests.SepRGB24ToBGR_F32;
var rgb: IImage<TRGB24>;
    r, g, b: IImage<Single>;
const
  w = 1024;
  h = 768;
begin
  rgb := TImgUt.ConstantImage<TRGB24>(w, h, RGB24(0, 0, 0));
  r := TImgUt.ConstantImage<Single>(w, h, 0);
  g := TImgUt.ConstantImage<Single>(w, h, 0);
  b := TImgUt.ConstantImage<Single>(w, h, 0);

  SWStart;
  RGBSeparate(rgb, r, g, b);
  SWStop;
end;

procedure TCsCvtTests.CombBGRToRGB24;
var rgb: IImage<TRGB24>;
    r, g, b: IImage<Byte>;
const
  w = 1024;
  h = 768;
begin
  rgb := TImgUt.ConstantImage<TRGB24>(w, h, RGB24(0, 0, 0));
  r := TImgUt.ConstantImage<Byte>(w, h, 0);
  g := TImgUt.ConstantImage<Byte>(w, h, 0);
  b := TImgUt.ConstantImage<Byte>(w, h, 0);

  SWStart;
  RGBCombine(r, g, b, rgb);
  SWStop;
end;

procedure TCsCvtTests.CombBGRToRGB24_F32;
var rgb: IImage<TRGB24>;
    r, g, b: IImage<Single>;
const
  w = 1024;
  h = 768;
begin
  rgb := TImgUt.ConstantImage<TRGB24>(w, h, RGB24(0, 0, 0));
  r := TImgUt.ConstantImage<Single>(w, h, 0);
  g := TImgUt.ConstantImage<Single>(w, h, 0);
  b := TImgUt.ConstantImage<Single>(w, h, 0);

  SWStart;
  RGBCombine(r, g, b, rgb);
  SWStop;
end;

{$endregion}

initialization

  RegisterTest(TCsCvtTests.Suite);

end.
