unit panda.PTests.cvCmp;

interface

uses
    TestFramework
  , panda.Tests.NDATestCase
  , panda.cvCmp
  ;

type
  TCVCmpTests = class(TNDAPerformanceTestCase)
  published
    procedure CmpLT_VS_I32;
    procedure CmpLT_VV_I32;
    procedure CmpLT_VV_I32_Rnd;
    procedure VecThreshold_UI8;
  end;


implementation

{$region 'TCVCmpTests'}

procedure TCVCmpTests.CmpLT_VS_I32;
var x: TArray<Integer>;
    res: TArray<Boolean>;
    I: Integer;
const N = 10000000;
begin
  SetLength(x, N);
  for I := 0 to High(x) do
    x[I] := I mod 1000;
  SetLength(res, N);

  SWStart;
  CmpLT(PInteger(x), 500, PBoolean(res), N);
  SWStop;
end;

procedure TCVCmpTests.CmpLT_VV_I32;
var x, y: TArray<Integer>;
    res: TArray<Boolean>;
    I: Integer;
const N = 10000000;
begin
  SetLength(x, N);
  SetLength(y, N);
  for I := 0 to High(x) do begin
    x[I] := I mod 1000;
    y[I] := (I + 500) mod 1000;
  end;
  SetLength(res, N);

  SWStart;
  CmpLT(PInteger(x), PInteger(y), PBoolean(res), N);
  SWStop;
end;

procedure TCVCmpTests.CmpLT_VV_I32_Rnd;
var x, y: TArray<Integer>;
    res: TArray<Boolean>;
    I: Integer;
const N = 10000000;
begin
  SetLength(x, N);
  SetLength(y, N);
  RandSeed := 1234;
  for I := 0 to High(x) do begin
    x[I] := Random(1000);
    y[I] := Random(1000);
  end;
  SetLength(res, N);

  SWStart;
  CmpLT(PInteger(x), PInteger(y), PBoolean(res), N);
  SWStop;
end;

procedure TCVCmpTests.VecThreshold_UI8;
var x: TArray<Single>;
const N = 10000000;
begin
  SetLength(x, N);

  SWStart;
  VecThreshold(PByte(x), PByte(x), 100, N);
  SWStop;
end;

{$endregion}

initialization

  RegisterTest(TCVCmpTests.Suite);

end.
