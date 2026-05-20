unit panda.PTests.DynArrUtils;

interface

uses
    TestFramework
  , panda.Tests.NDATestCase
  , panda.DynArrayUtils
  , System.Generics.Collections
  ;

type
  TDynArrUtilTests = class(TNDAPerformanceTestCase)
  published
    procedure SystemQuickSort_RevSeq;
    procedure SystemQuickSort_PeriodicSeq;
    procedure SystemQuickSort_URndSeq;
    procedure MegerSort_RevSeq;
    procedure MegerSort_PeriodicSeq;
    procedure MegerSort_URndSeq;
  end;

implementation

procedure TDynArrUtilTests.SystemQuickSort_RevSeq;
var x: TArray<Double>;
    I: Integer;
const N = 1000000;
begin
  SetLength(x, N);
  for I := 0 to N - 1 do
    x[I] := N - I;

  SWStart;
  TArray.Sort<Double>(x);
  SWStop;
end;

procedure TDynArrUtilTests.SystemQuickSort_PeriodicSeq;
var x: TArray<Double>;
    I: Integer;
const N = 1000000;
begin
  SetLength(x, N);
  for I := 0 to N - 1 do
    x[I] := I mod 1000;

  SWStart;
  TArray.Sort<Double>(x);
  SWStop;
end;

procedure TDynArrUtilTests.SystemQuickSort_URndSeq;
var x: TArray<Double>;
    I: Integer;
const N = 1000000;
begin
  RandSeed := 1234;
  SetLength(x, N);
  for I := 0 to N - 1 do
    x[I] := Random;

  SWStart;
  TArray.Sort<Double>(x);
  SWStop;
end;

procedure TDynArrUtilTests.MegerSort_RevSeq;
var x, y: TArray<Double>;
    me: TMergeSort<Double>;
    I: Integer;
const N = 1000000;
begin
  SetLength(x, N);
  for I := 0 to N - 1 do
    x[I] := N - I;
  me.Init;

  SWStart;
  y := me.Sort(x);
  SWStop;
end;

procedure TDynArrUtilTests.MegerSort_PeriodicSeq;
var x, y: TArray<Double>;
    me: TMergeSort<Double>;
    I: Integer;
const N = 1000000;
begin
  SetLength(x, N);
  for I := 0 to N - 1 do
    x[I] := I mod 1000;
  me.Init;

  SWStart;
  y := me.Sort(x);
  SWStop;
end;

procedure TDynArrUtilTests.MegerSort_URndSeq;
var x, y: TArray<Double>;
    me: TMergeSort<Double>;
    I: Integer;
const N = 1000000;
begin
  RandSeed := 1234;
  SetLength(x, N);
  for I := 0 to N - 1 do
    x[I] := Random;
  me.Init;

  SWStart;
  y := me.Sort(x);
  SWStop;
end;

initialization

  RegisterTest(TDynArrUtilTests.Suite);

end.
