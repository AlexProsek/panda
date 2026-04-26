unit panda.ImgProc.PTests.RLE;

interface

uses
    TestFramework
  , System.SysUtils
  , System.Math
  , panda.ImgProc.RLE
  , panda.Tests.NDATestCase
  ;

type
  TRLEPerformanceTests = class(TNDAPerformanceTestCase)
  protected const
    cTestCount = 10;
    N = 10000000;
  published
    procedure Encode_AbsSeq2;
    procedure Encode_AbsSeq3;
    procedure Encode_GeneralTest;
  end;

implementation

{$region 'TRLEPerformanceTests'}

procedure TRLEPerformanceTests.Encode_AbsSeq2;
var src, dst: TBytes;
    p, pEnd: PByte;
begin
  SetLength(src, N);
  SetLength(dst, 2 * N);
  p := PByte(src);
  pEnd := PByte(@src[N - 8]);
  while p < pEnd do begin
    p^ := 5;
    Inc(p);
    p^ := 2;
    Inc(p);
    p^ := 2;
    Inc(p);
  end;

  SWStart;
  DoTestLoop(procedure begin RLE8Encode(PByte(src), PByte(dst), Length(src)) end, cTestCount);
  SWStop;
end;

procedure TRLEPerformanceTests.Encode_AbsSeq3;
var src, dst: TBytes;
    p, pEnd: PByte;
    I, count: Integer;
begin
  SetLength(src, N);
  SetLength(dst, 2 * N);
  p := PByte(src);
  pEnd := PByte(@src[N - 8]);
  while p < pEnd do begin
    p^ := 5;
    Inc(p);
    p^ := 6;
    Inc(p);
    p^ := 2;
    Inc(p);
    p^ := 2;
    Inc(p);
  end;

  SWStart;
  DoTestLoop(procedure begin RLE8Encode(PByte(src), PByte(dst), Length(src)) end, cTestCount);
  SWStop;
end;


procedure TRLEPerformanceTests.Encode_GeneralTest;
var src, dst: TBytes;
    p, pEnd: PByte;
    I, v: Integer;
begin
  SetLength(src, N);
  SetLength(dst, 2 * N);
  p := PByte(src);
  pEnd := PByte(@src[N - 512]);
  v := 0;
  while p < pEnd do begin
    Inc(v);
    v := v mod 256;
    I := Max(1, v div 3);
    while I > 0 do begin
      p^ := v;
      Inc(p);
      Dec(I);
    end;
  end;

  SWStart;
  DoTestLoop(procedure begin RLE8Encode(PByte(src), PByte(dst), Length(src)) end, cTestCount);
  SWStop;
end;

initialization

  RegisterTest(TRLEPerformanceTests.Suite);

end.
