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

{$endregion}

initialization

  RegisterTest(TCsCvtTests.Suite);

end.
