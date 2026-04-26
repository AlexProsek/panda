unit panda.Filters.PTests.OrdStatFilters;

interface

uses
    TestFramework
  , panda.Intfs
  , panda.Arrays
  , panda.Math
  , panda.Filters.OrderStatFilters
  , panda.Tests.NDATestCase
  ;

type
  TOrdStatFilterTests = class(TNDAPerformanceTestCase)
  published
    procedure MinFilter1DUI8_1M_R20;
    procedure MinFilter2DUI8_1kx1k_R20;
    procedure MedianFilter1DUI8_1M_R20;
    procedure MedianFilter1DF64_1M_R20;
    procedure MedianFilter2DUI8_500x500_R20;
  end;

implementation

procedure TOrdStatFilterTests.MinFilter1DUI8_1M_R20;
var f: TMinFilter1DUI8;
    x, res: TArray<Byte>;
const N = 1000000;
begin
  SetLength(x, N);
  SetLength(res, N);

  f := TMinFilter1DUI8.Create;
  try
    f.Radius := 20;

    SWStart;
    f.Execute(PByte(x), PByte(res), N);
    SWStop;
  finally
    f.Free;
  end;
end;

procedure TOrdStatFilterTests.MinFilter2DUI8_1kx1k_R20;
var f: TBoxMinFilter2DUI8;
    x, res: INDArray<Byte>;
const N = 1000;
begin
  x := TNDAUt.Full<Byte>([N, N], 0);
  res := TNDAUt.Full<Byte>([N, N], 0);

  f := TBoxMinFilter2DUI8.Create;
  try
    f.HRadius := 20;
    f.VRadius := 20;

    SWStart;
    f.Execute(x.Data, res.Data, x.Strides[0], res.Strides[0], N, N);
    SWStop;
  finally
    f.Free;
  end;
end;

procedure TOrdStatFilterTests.MedianFilter1DUI8_1M_R20;
var f: TMedianFilter1DUI8;
    x, res: TArray<Byte>;
    I: Integer;
const N = 1000000;
begin
  SetLength(x, N);
  RandSeed := 1234;
  for I := 0 to High(x) do
    x[I] := Random(255);
  SetLength(res, N);

  f := TMedianFilter1DUI8.Create;
  try
    f.Radius := 20;

    SWStart;
    f.Execute(PByte(x), PByte(res), N);
    SWStop;
  finally
    f.Free;
  end;
end;

procedure TOrdStatFilterTests.MedianFilter1DF64_1M_R20;
var f: TMedianFilter1DF64;
    x, res: TArray<Double>;
    I: Integer;
const N = 1000000;
begin
  SetLength(x, N);
  RandSeed := 1234;
  for I := 0 to High(x) do
    x[I] := Random;
  SetLength(res, N);

  f := TMedianFilter1DF64.Create;
  try
    f.Radius := 20;

    SWStart;
    f.Execute(PByte(x), PByte(res), N);
    SWStop;
  finally
    f.Free;
  end;
end;

procedure TOrdStatFilterTests.MedianFilter2DUI8_500x500_R20;
var f: TMedianFilter2DUI8;
    x, res: INDArray<Byte>;
const N = 500;
begin
  RandSeed := 1234;
  x := TNDAUt.Fill<Byte>([N, N], function: Byte begin Result := Random(255) end);
  res := TNDAUt.Full<Byte>([N, N], 0);

  f := TMedianFilter2DUI8.Create;
  try
    f.HRadius := 20;
    f.VRadius := 20;

    SWStart;
    f.Execute(x.Data, res.Data, x.Strides[0], res.Strides[0], N, N);
    SWStop;
  finally
    f.Free;
  end;
end;

initialization

  RegisterTest(TOrdStatFilterTests.Suite);

end.
