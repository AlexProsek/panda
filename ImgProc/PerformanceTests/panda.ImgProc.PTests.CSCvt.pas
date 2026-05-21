unit panda.ImgProc.PTests.CSCvt;

interface

uses
    TestFramework
  , panda.ImgProc.Types
  , panda.ImgProc.Images
  , panda.ImgProc.CsCvt
  , panda.Tests.NDATestCase
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


{$endregion}

initialization

  RegisterTest(TCsCvtTests.Suite);

end.
