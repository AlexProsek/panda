unit panda.PTests.cvCvt;

interface

uses
    TestFramework
  , panda.Tests.NDATestCase
  , panda.cvCvt
  ;

type
  TCVCvtTests = class(TNDAPerformanceTestCase)
  published
    procedure Cvt_UI8F32;
    procedure Cvt_F32UI8;
    procedure Cvt_I32F32;
    procedure Cvt_I64F64;
    procedure Cvt_I64F64_ShortArr;
    procedure Cvt_F32F64;
    procedure Cvt_F64F32;
  end;

  TNormZeroTests = class(TNDAPerformanceTestCase)
  published
    procedure NormZero_F32;
    procedure NormZero_F64;
  end;

implementation

{$region 'TCVCvtTests'}

procedure TCVCvtTests.Cvt_UI8F32;
var x: TArray<Byte>;
    y: TArray<Single>;
const N = 10000000;
begin
  SetLength(x, N);
  SetLength(y, N);

  SWStart;
  cvt(PByte(x), PSingle(y), Length(x));
  SWStop;
end;

procedure TCVCvtTests.Cvt_F32UI8;
var x: TArray<Single>;
    y: TArray<Byte>;
const N = 10000000;
begin
  SetLength(x, N);
  SetLength(y, N);

  SWStart;
  cvt(PSingle(x), PByte(y), Length(x));
  SWStop;
end;

procedure TCVCvtTests.Cvt_I32F32;
var x: TArray<Integer>;
    y: TArray<Single>;
const N = 10000000;
begin
  SetLength(x, N);
  SetLength(y, N);

  SWStart;
  cvt(PInteger(x), PSingle(y), Length(x));
  SWStop;
end;

procedure TCVCvtTests.Cvt_I64F64;
var x: TArray<Int64>;
    y: TArray<Double>;
const N = 10000000;
begin
  SetLength(x, N);
  SetLength(y, N);

  SWStart;
  cvt(PInt64(x), PDouble(y), Length(x));
  SWStop;
end;

procedure TCVCvtTests.Cvt_I64F64_ShortArr;
var x: TArray<Int64>;
    y: TArray<Double>;
    I: Integer;
const N = 10000;
      NLoop = 1000;
begin
  SetLength(x, N);
  SetLength(y, N);

  cvt(PInt64(x), PDouble(y), Length(x)); // warm-up

  SWStart;
  for I := 0 to NLoop - 1 do
    cvt(PInt64(x), PDouble(y), Length(x));
  SWStop;
end;

procedure TCVCvtTests.Cvt_F32F64;
var x: TArray<Single>;
    y: TArray<Double>;
const N = 10000000;
begin
  SetLength(x, N);
  SetLength(y, N);

  SWStart;
  cvt(PSingle(x), PDouble(y), Length(x));
  SWStop;
end;

procedure TCVCvtTests.Cvt_F64F32;
var x: TArray<Double>;
    y: TArray<Single>;
const N = 10000000;
begin
  SetLength(x, N);
  SetLength(y, N);

  SWStart;
  cvt(PDouble(x), PSingle(y), Length(x));
  SWStop;
end;

{$endregion}

{$region 'TNormZeroTests'}

procedure TNormZeroTests.NormZero_F32;
var x, y: TArray<Single>;
    I: Integer;
const N = 1000000;
begin
  SetLength(x, N);
  SetLength(y, N);
  for I := 0 to High(x) do
    if I mod 4 = 0 then x[I] := -0.0;

  SWStart;
  DoTestLoop(procedure begin NormalizeZero(PSingle(x), PSingle(y), Length(x)) end, 100);
  SWStop;
end;

procedure TNormZeroTests.NormZero_F64;
var x, y: TArray<Double>;
    I: Integer;
const N = 1000000;
begin
  SetLength(x, N);
  SetLength(y, N);
  for I := 0 to High(x) do
    if I mod 4 = 0 then x[I] := -0.0;

  SWStart;
  DoTestLoop(procedure begin NormalizeZero(PDouble(x), PDouble(y), Length(x)) end, 100);
  SWStop;
end;

{$endregion}

initialization

  RegisterTest(TCVCvtTests.Suite);
  RegisterTest(TNormZeroTests.Suite);

end.
