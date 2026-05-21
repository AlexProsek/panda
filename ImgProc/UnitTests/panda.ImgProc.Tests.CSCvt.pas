unit panda.ImgProc.Tests.CSCvt;

interface

uses
    TestFramework
  , Math
  , SysUtils
  , panda.Intfs
  , panda.Arrays
  , panda.ArrManip
  , panda.ImgProc.Types
  , panda.ImgProc.CSCvt
  , panda.ImgProc.Images
  , panda.Tests.NDATestCase
  ;

{$I AsmDefs.inc}

type
  TCSCvtTests = class(TNDATestCase)
  protected const
    sTol = 1e-6;
    cHSVTol = 1e-1;//3;
  published
    procedure TestRGB24ToUI8_2;
    procedure TestRGB24ToUI8_4;
    procedure TestRGB24ToF32_2;
    procedure TestRGB24ToF32_4;
    procedure TestUI8ToRGB24_16;
    procedure TestUI8ToRGB24_18;
    procedure TestUI8ToF32_8;
    procedure TestUI8ToF32_10;
    procedure TestF32ToUI8_3;
    procedure TestF32ToUI8_8;
    procedure TestF32ToUI8_10;

    procedure TestRGB24ToHue;
    procedure TestRGB24ToHSV_RndSample;
  {$if defined(ASMx64)}
    procedure RGB24Sep16;
    procedure GetMaxRngUI8;

    procedure TestHueV16_Red;
    procedure TestHueV16_Yellow;
    procedure TestHueV16_Green;
    procedure TestHueV16_Cyan;
    procedure TestHueV16_Blue;
    procedure TestHueV16_Magenta;
    procedure TestHueV16_White;
    procedure TestHueV16_Black;
    procedure TestHueV16_R200G100B0;
    procedure TestHueV16_R0G200B100;
    procedure TestHueV16_R100G0B200;
    procedure TestHueV16;

    procedure TestSV16;
    procedure TestSV16_V0;
    procedure TestSV16_R239G223B46;
  {$endif}
  end;

implementation

procedure HSV(R, G, B: Integer; out H, S, V: Single);
var mi, ma, rng: Integer;
begin
  mi := Min(R, Min(G, B));
  ma := Max(R, Max(G, B));
  rng := ma - mi;

  V := ma/255;

  if ma = 0 then
    S := 0
  else
    S := rng/ma;

  if rng = 0 then
    H := 0
  else begin
    if ma = R then begin
      H := 60 * (G - B) / rng;
      if H < 0 then H := H + 360;
    end else
    if ma = G then
      H := 120 + (60 * (B - R)) / rng
    else
      H := 240 + (60 * (R - G)) / rng;
  end;
end;

function Hue(R, G, B: Integer): Single; inline;
var s, v: Single;
begin
  HSV(R, G, B, Result, s, v);
end;

function RGBChannelArray(R, G, B: Byte; aCount: Integer = 16): TArray<Byte>;
var I: Integer;
begin
  SetLength(Result, 3*aCount);
  for I := 0 to aCount- 1 do begin
    Result[I] := B;
    Result[aCount + I] := G;
    Result[2*aCount + I] := R;
  end;
end;

function AppendMaRng(const aChannels: TArray<Byte>): TArray<Byte>;
var I, count, r, g, b, mi, ma: Integer;
begin
  Assert(Length(aChannels) mod 3 = 0);

  count := Length(aChannels) div 3;
  SetLength(Result, 5 * count);
  Move(aChannels[0], Result[0], Length(aChannels));
  for I := 0 to count - 1 do begin
    b := Result[I];
    g := Result[count + I];
    r := Result[2*count + I];

    mi := Min(r, Min(g, b));
    ma := Max(r, Max(g, b));

    Result[3*count + I] := ma;
    Result[4*count + I] := ma - mi;
  end;
end;

function SeparateRGBChannels(const aClrs: TArray<TRGB24>): TArray<Byte>;
var I, count: Integer;
begin
  count := Length(aClrs);
  SetLength(Result, 3*count);
  for I := 0 to count - 1 do with aClrs[I] do begin
    Result[I] := B;
    Result[count + I] := G;
    Result[2*count + I] := R;
  end;
end;

{$region 'TCSCvtTests'}

procedure TCSCvtTests.TestRGB24ToUI8_2;
var src: TArray<TRGB24>;
    dst: TArray<Byte>;
    I: Integer;
begin
  src := TArray<TRGB24>.Create(RGB24(1, 2, 3), RGB24(4, 5, 6));
  SetLength(dst, Length(src));

  cscvtRGB24ToUI8(PByte(src), PByte(dst), Length(src));

  for I := 0 to High(dst) do
    CheckEquals(Round(0.299 * src[I].R + 0.587 * src[I].G + 0.114 * src[I].B), dst[I]);
end;

procedure TCSCvtTests.TestRGB24ToUI8_4;
var src: TArray<TRGB24>;
    dst: TArray<Byte>;
    I: Integer;
begin
  src := TArray<TRGB24>.Create(RGB24(1, 2, 3), RGB24(4, 5, 6), RGB24(7, 8, 9), RGB24(10, 11, 12));
  SetLength(dst, Length(src));

  cscvtRGB24ToUI8(PByte(src), PByte(dst), Length(src));

  for I := 0 to High(dst) do
    CheckEquals(Round(0.299 * src[I].R + 0.587 * src[I].G + 0.114 * src[I].B), dst[I]);
end;

procedure TCSCvtTests.TestRGB24ToF32_2;
var src: TArray<TRGB24>;
    dst: TArray<Single>;
    I: Integer;
begin
  src := TArray<TRGB24>.Create(RGB24(1, 2, 3), RGB24(4, 5, 6));
  SetLength(dst, Length(src));

  cscvtRGB24ToF32(PByte(src), PByte(dst), Length(src));

  for I := 0 to High(dst) do
    CheckEquals((0.299 * src[I].R + 0.587 * src[I].G + 0.114 * src[I].B) / 255, dst[I], sTol);
end;

procedure TCSCvtTests.TestRGB24ToF32_4;
var src: TArray<TRGB24>;
    dst: TArray<Single>;
    I: Integer;
begin
  src := TArray<TRGB24>.Create(RGB24(1, 2, 3), RGB24(4, 5, 6), RGB24(7, 8, 9), RGB24(10, 11, 12));
  SetLength(dst, Length(src));

  cscvtRGB24ToF32(PByte(src), PByte(dst), Length(src));

  for I := 0 to High(dst) do
    CheckEquals((0.299 * src[I].R + 0.587 * src[I].G + 0.114 * src[I].B) / 255, dst[I], sTol);
end;

procedure TCSCvtTests.TestUI8ToRGB24_16;
var src: TArray<Byte>;
    dst: TArray<TRGB24>;
    I: Integer;
begin
  src := TArray<Byte>.Create(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
  SetLength(dst, Length(src));

  cscvtUI8ToRGB24(PByte(src), PByte(dst), Length(src));

  for I := 0 to High(dst) do begin
    CheckEquals(I, dst[I].R);
    CheckEquals(I, dst[I].G);
    CheckEquals(I, dst[I].B);
  end;
end;

procedure TCSCvtTests.TestUI8ToRGB24_18;
var src: TArray<Byte>;
    dst: TArray<TRGB24>;
    I: Integer;
begin
  src := TArray<Byte>.Create(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17);
  SetLength(dst, Length(src));

  cscvtUI8ToRGB24(PByte(src), PByte(dst), Length(src));

  for I := 0 to High(dst) do begin
    CheckEquals(I, dst[I].R);
    CheckEquals(I, dst[I].G);
    CheckEquals(I, dst[I].B);
  end;
end;

procedure TCSCvtTests.TestUI8ToF32_8;
var src: TArray<Byte>;
    dst: TArray<Single>;
    I: Integer;
begin
  src := TArray<Byte>.Create(0, 1, 2, 3, 4, 5, 6, 7);
  SetLength(dst, Length(src));

  cscvtUI8ToF32(PByte(src), PByte(dst), Length(src));

  for I := 0 to High(dst) do
    CheckEquals(I/255, dst[I], sTol);
end;

procedure TCSCvtTests.TestUI8ToF32_10;
var src: TArray<Byte>;
    dst: TArray<Single>;
    I: Integer;
begin
  src := TArray<Byte>.Create(0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
  SetLength(dst, Length(src));

  cscvtUI8ToF32(PByte(src), PByte(dst), Length(src));

  for I := 0 to High(dst) do
    CheckEquals(I/255, dst[I], sTol);
end;

procedure TCSCvtTests.TestF32ToUI8_3;
var src: TArray<Single>;
    dst: TArray<Byte>;
    I: Integer;
begin
  src := TArray<Single>.Create(-1, 0.5, 2);
  SetLength(dst, Length(src));

  cscvtF32ToUI8(PByte(src), PByte(dst), Length(src));

  for I := 0 to High(dst) do
    CheckEquals(Round(Max(0, Min(255, 255*src[I]))), dst[I]);
end;

procedure TCSCvtTests.TestF32ToUI8_8;
var src: TArray<Single>;
    dst: TArray<Byte>;
    I: Integer;
begin
  src := TArray<Single>.Create(-1, 0, 0.5, 1, 2, 1, 0.5, 0);
  SetLength(dst, Length(src));

  cscvtF32ToUI8(PByte(src), PByte(dst), Length(src));

  for I := 0 to High(dst) do
    CheckEquals(Round(Max(0, Min(255, 255*src[I]))), dst[I]);
end;

procedure TCSCvtTests.TestF32ToUI8_10;
var src: TArray<Single>;
    dst: TArray<Byte>;
    I: Integer;
begin
  src := TArray<Single>.Create(-1, 0, 0.5, 1, 2, 1, 0.5, 0, 0.5, 4);
  SetLength(dst, Length(src));

  cscvtF32ToUI8(PByte(src), PByte(dst), Length(src));

  for I := 0 to High(dst) do
    CheckEquals(Round(Max(0, Min(255, 255*src[I]))), dst[I]);
end;

procedure TCSCvtTests.TestRGB24ToHue;
var src: TArray<TRGB24>;
    h: TArray<Single>;
    I: Integer;
const tol = 1e-4;
begin
  src := TArray<TRGB24>.Create(
    RGB24(255,   0,   0),
    RGB24(255, 255,   0),
    RGB24(  0, 255,   0),
    RGB24(  0, 255, 255),
    RGB24(  0,   0, 255),
    RGB24(255,   0, 255)
  );
  SetLength(h, Length(src));

  cscvtRGB24ToHue(PByte(src), PByte(h), Length(src));

  for I := 0 to High(h) do
    CheckEquals(60*I, h[I], tol);
end;

procedure TCSCvtTests.TestRGB24ToHSV_RndSample;
var src: TArray<TRGB24>;
    vh, vs, vv: TArray<Single>;
    h, s, v: Single;
    I: Integer;
const N = 10000;
begin
  SetLength(src, N);
  SetLength(vh, N);
  SetLength(vs, N);
  SetLength(vv, N);
  RandSeed := 1234;
  for I := 0 to High(src) do with src[I] do begin
    R := Random(255);
    G := Random(255);
    B := Random(255);
  end;

  cssepRGB24ToHSV(PByte(src), PByte(vh), PByte(vs), PByte(vv), N);

  for I := 0 to High(src) do begin
    with src[I] do
      HSV(R, G, B, h, s, v);

    CheckEquals(h, vh[I], cHSVTol, Format('Wrong Hue at position %d', [I]));
    CheckEquals(s, vs[I], cHSVTol, Format('Wrong Sat at position %d', [I]));
    CheckEquals(v, vv[I], cHSVTol, Format('Wrong Val at position %d', [I]));
  end;
end;

{$if defined(ASMx64)}


procedure TCSCvtTests.RGB24Sep16;
var vch: array [0..2] of TVec16UI8;
    src: TArray<TRGB24>;
    I: Integer;
begin
  src := TArray<TRGB24>.Create(
    RGB24(1, 2, 3), RGB24(4, 5, 6), RGB24(7, 8, 9), RGB24(9, 8, 7),
    RGB24(6, 5, 4), RGB24(3, 2, 1), RGB24(1, 3, 2), RGB24(4, 6, 5),
    RGB24(7, 9, 8), RGB24(8, 9, 7), RGB24(5, 6, 4), RGB24(2, 3, 1),
    RGB24(2, 1, 3), RGB24(5, 4, 6), RGB24(8, 7, 9), RGB24(9, 7, 8)
  );

  _RGB24Sep16(PByte(src), @vch);

  for I := 0 to High(src) do with src[I] do begin
    CheckEquals(src[I].B, vch[0, I]);
    CheckEquals(src[I].G, vch[1, I]);
    CheckEquals(src[I].R, vch[2, I]);
  end;
end;

procedure TCSCvtTests.GetMaxRngUI8;
var v: array [0..5] of TVec16UI8;
    mi, ma: Integer;
    src: TArray<TRGB24>;
    I: Integer;
begin
  src := TArray<TRGB24>.Create(
    RGB24(1, 2, 3), RGB24(4, 5, 6), RGB24(7, 8, 9), RGB24(9, 8, 7),
    RGB24(6, 5, 4), RGB24(3, 2, 1), RGB24(1, 3, 2), RGB24(4, 6, 5),
    RGB24(7, 9, 8), RGB24(8, 9, 7), RGB24(5, 6, 4), RGB24(2, 3, 1),
    RGB24(2, 1, 3), RGB24(5, 4, 6), RGB24(8, 7, 9), RGB24(9, 7, 8)
  );

  _RGB24Sep16(PByte(src), @v[0]);
  _GetMaxAndRngFromRGBCh(@v[0], @v[3]);

  for I := 0 to High(src) do with src[I] do begin
    mi := Min(R, Min(G, B));
    ma := Max(R, Max(G, B));
    CheckEquals(ma, v[3, I]);
    CheckEquals(ma - mi, v[4, I]);
  end;
end;

procedure TCSCvtTests.TestHueV16_Red;
var src: TArray<Byte>;
    dst: TArray<Single>;
    h: Single;
    I: Integer;
begin
  src := AppendMaRng(RGBChannelArray(255, 0, 0, 16));
  SetLength(dst, 16);

  h := Hue(255, 0, 0);

  _Hue16(PByte(src), PByte(dst));
  for I := 0 to High(dst) do
    CheckEquals(h, dst[I], cHSVTol);
end;

procedure TCSCvtTests.TestHueV16_Yellow;
var src: TArray<Byte>;
    dst: TArray<Single>;
    h: Single;
    I: Integer;
begin
  src := AppendMaRng(RGBChannelArray(255, 255, 0, 16));
  SetLength(dst, 16);

  h := Hue(255, 255, 0);

  _Hue16(PByte(src), PByte(dst));
  for I := 0 to High(dst) do
    CheckEquals(h, dst[I], cHSVTol);
end;

procedure TCSCvtTests.TestHueV16_Green;
var src: TArray<Byte>;
    dst: TArray<Single>;
    h: Single;
    I: Integer;
begin
  src := AppendMaRng(RGBChannelArray(0, 255, 0, 16));
  SetLength(dst, 16);

  h := Hue(0, 255, 0);

  _Hue16(PByte(src), PByte(dst));
  for I := 0 to High(dst) do
    CheckEquals(h, dst[I], cHSVTol);
end;

procedure TCSCvtTests.TestHueV16_Cyan;
var src: TArray<Byte>;
    dst: TArray<Single>;
    h: Single;
    I: Integer;
begin
  src := AppendMaRng(RGBChannelArray(0, 255, 255, 16));
  SetLength(dst, 16);

  h := Hue(0, 255, 255);

  _Hue16(PByte(src), PByte(dst));
  for I := 0 to High(dst) do
    CheckEquals(h, dst[I], cHSVTol);
end;

procedure TCSCvtTests.TestHueV16_Blue;
var src: TArray<Byte>;
    dst: TArray<Single>;
    h: Single;
    I: Integer;
begin
  src := AppendMaRng(RGBChannelArray(0, 0, 255, 16));
  SetLength(dst, 16);

  h := Hue(0, 0, 255);

  _Hue16(PByte(src), PByte(dst));
  for I := 0 to High(dst) do
    CheckEquals(h, dst[I], cHSVTol);
end;

procedure TCSCvtTests.TestHueV16_Magenta;
var src: TArray<Byte>;
    dst: TArray<Single>;
    h: Single;
    I: Integer;
begin
  src := AppendMaRng(RGBChannelArray(255, 0, 255, 16));
  SetLength(dst, 16);

  h := Hue(255, 0, 255);

  _Hue16(PByte(src), PByte(dst));
  for I := 0 to High(dst) do
    CheckEquals(h, dst[I], cHSVTol);
end;

procedure TCSCvtTests.TestHueV16_White;
var src: TArray<Byte>;
    dst: TArray<Single>;
    h: Single;
    I: Integer;
begin
  src := AppendMaRng(RGBChannelArray(255, 255, 255, 16));
  SetLength(dst, 16);

  h := Hue(255, 255, 255);

  _Hue16(PByte(src), PByte(dst));
  for I := 0 to High(dst) do
    CheckEquals(h, dst[I], cHSVTol);
end;

procedure TCSCvtTests.TestHueV16_Black;
var src: TArray<Byte>;
    dst: TArray<Single>;
    h: Single;
    I: Integer;
begin
  src := AppendMaRng(RGBChannelArray(0, 0, 0, 16));
  SetLength(dst, 16);

  h := Hue(0, 0, 0);

  _Hue16(PByte(src), PByte(dst));
  for I := 0 to High(dst) do
    CheckEquals(h, dst[I], cHSVTol);
end;

procedure TCSCvtTests.TestHueV16_R200G100B0;
var src: TArray<Byte>;
    dst: TArray<Single>;
    h: Single;
    I: Integer;
begin
  src := AppendMaRng(RGBChannelArray(200, 100, 0, 16));
  SetLength(dst, 16);

  h := Hue(200, 100, 0);

  _Hue16(PByte(src), PByte(dst));
  for I := 0 to High(dst) do
    CheckEquals(h, dst[I], cHSVTol);
end;

procedure TCSCvtTests.TestHueV16_R0G200B100;
var src: TArray<Byte>;
    dst: TArray<Single>;
    h: Single;
    I: Integer;
begin
  src := AppendMaRng(RGBChannelArray(0, 200, 100, 16));
  SetLength(dst, 16);

  h := Hue(0, 200, 100);

  _Hue16(PByte(src), PByte(dst));
  for I := 0 to High(dst) do
    CheckEquals(h, dst[I], cHSVTol);
end;

procedure TCSCvtTests.TestHueV16_R100G0B200;
var src: TArray<Byte>;
    dst: TArray<Single>;
    h: Single;
    I: Integer;
begin
  src := AppendMaRng(RGBChannelArray(100, 0, 200, 16));
  SetLength(dst, 16);

  h := Hue(100, 0, 200);

  _Hue16(PByte(src), PByte(dst));
  for I := 0 to High(dst) do
    CheckEquals(h, dst[I], cHSVTol);
end;

procedure TCSCvtTests.TestHueV16;
var clrs: TArray<TRGB24>;
    src: TArray<Byte>;
    dst, h: TArray<Single>;
    I: Integer;
begin
  clrs := TArray<TRGB24>.Create(
    RGB24(255,   0,   0),
    RGB24(255, 255,   0),
    RGB24(  0, 255,   0),
    RGB24(  0, 255, 255),
    RGB24(  0,   0, 255),
    RGB24(255,   0, 255),
    RGB24(255,   0,   0),
    RGB24(255, 255,   0),

    RGB24(100, 200,   0),
    RGB24(100, 200, 120),
    RGB24(120, 100, 200),
    RGB24(  0, 100, 200),
    RGB24(  0, 200, 100),
    RGB24(100,   0, 200),
    RGB24(200,   0, 100),
    RGB24(100, 100, 100)
  );
  src := AppendMaRng(SeparateRGBChannels(clrs));
  SetLength(dst, Length(clrs));

  _Hue16(PByte(src), PByte(dst));

  SetLength(h, Length(dst));
  for I := 0 to High(h) do with clrs[I] do
    h[I] := Hue(R, G, B);

  for I := 0 to High(dst) do
    CheckEquals(h[I], dst[I], cHSVTol);
end;

procedure TCSCvtTests.TestSV16;
var src: TArray<Byte>;
    s, v: TArray<Single>;
    I: Integer;
begin
  src := TArray<Byte>.Create(
    1,   2,  3,  4,  5,  6,  7,  8,  9,  1, 11, 12, 13, 14, 15, 16, // max(r,g,b)
    32, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17  // max(r,g,b) - min(r,g,b)
  );
  SetLength(s, 16);
  SetLength(v, 16);

  _SV16(PByte(src), PByte(s), PByte(v));

  for I := 0 to 15 do begin
    CheckEquals(src[I]/255, v[I], cHSVTol);
    CheckEquals(src[16 + I] / src[I], s[I], cHSVTol);
  end;
end;

procedure TCSCvtTests.TestSV16_V0;
var src: TArray<Byte>;
    s, v: TArray<Single>;
    I: Integer;
begin
  SetLength(src, 32);
  SetLength(s, 16);
  SetLength(v, 16);

  _SV16(PByte(src), PByte(s), PByte(v));

  for I := 0 to 15 do begin
    CheckEquals(0, v[I], cHSVTol);
    CheckEquals(0, s[I], cHSVTol);
  end;
end;

procedure TCSCvtTests.TestSV16_R239G223B46;
var src: TArray<Byte>;
    s, v: TArray<Single>;
    I: Integer;
begin
  src := TArray<Byte>.Create(
    239, 197, 242, 215, 163, 228, 182, 179, 243, 161, 154, 211, 254, 190, 152, 251,
    193, 159, 234, 209,  94, 172, 161, 164, 195, 102, 131, 101, 240,  97, 103, 184
  );
  SetLength(s, 16);
  SetLength(v, 16);

  _SV16(PByte(src), PByte(s), PByte(v));

  for I := 0 to 15 do begin
    CheckEquals(src[I]/255, v[I], cHSVTol);
    CheckEquals(src[16 + I] / src[I], s[I], cHSVTol);
  end;
end;

{$endif}

{$endregion}

initialization

  RegisterTest(TCSCvtTests.Suite);

end.
