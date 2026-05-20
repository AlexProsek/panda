unit panda.PTests.Sorting;

interface

uses
    TestFramework
  , panda.Tests.NDATestCase
  , panda.Sorting
  , System.Generics.Collections
  ;

type
  TSortingTests = class(TNDAPerformanceTestCase)
  published
    procedure QuickSortF64_SortedSeq;
    procedure QuickSortF64_PeriodicSeq;
    procedure QuickSortF64_URndSeq;
    procedure QuickSortF64_TriangleWave;
    procedure MergeSortF64_SortedSeq;
    procedure MergeSortF64_PeriodicSeq;
    procedure MergeSortF64_URndSeq;
    procedure MergeSortF64_TriangleWave;
  end;

implementation

{$region 'TSortingTests'}

procedure TSortingTests.QuickSortF64_SortedSeq;
var x: TArray<Double>;
    I: Integer;
const N = 1000000;
begin
  SetLength(x, N);
  for I := 0 to N - 1 do
    x[I] := N;

  SWStart;
  TSortUtF64.QuickSort(x);
  SWStop;
end;

procedure TSortingTests.QuickSortF64_PeriodicSeq;
var x: TArray<Double>;
    I: Integer;
const N = 1000000;
begin
  SetLength(x, N);
  for I := 0 to N - 1 do
    x[I] := I mod 1000;

  SWStart;
  TSortUtF64.QuickSort(x);
  SWStop;
end;

procedure TSortingTests.QuickSortF64_URndSeq;
var x: TArray<Double>;
    I: Integer;
const N = 1000000;
begin
  RandSeed := 1234;
  SetLength(x, N);
  for I := 0 to N - 1 do
    x[I] := Random;

  SWStart;
  TSortUtF64.QuickSort(x);
  SWStop;
end;

procedure TSortingTests.QuickSortF64_TriangleWave;
var x: TArray<Double>;
    I, J: Integer;
const N = 1000000;
begin
  SetLength(x, N);
  J := N div 8;
  for I := 0 to N - 1 do
    x[I] := Abs(J - I);

  SWStart;
  TSortUtF64.QuickSort(x);
  SWStop;
end;

procedure TSortingTests.MergeSortF64_SortedSeq;
var x, y: TArray<Double>;
    I: Integer;
const N = 1000000;
begin
  SetLength(x, N);
  for I := 0 to N - 1 do
    x[I] := N;

  SWStart;
  y := TSortUtF64.MergeSort(x);
  SWStop;
end;

procedure TSortingTests.MergeSortF64_PeriodicSeq;
var x, y: TArray<Double>;
    I: Integer;
const N = 1000000;
begin
  SetLength(x, N);
  for I := 0 to N - 1 do
    x[I] := I mod 1000;

  SWStart;
  y := TSortUtF64.MergeSort(x);
  SWStop;
end;

procedure TSortingTests.MergeSortF64_URndSeq;
var x, y: TArray<Double>;
    I: Integer;
const N = 1000000;
begin
  RandSeed := 1234;
  SetLength(x, N);
  for I := 0 to N - 1 do
    x[I] := Random;

  SWStart;
  y := TSortUtF64.MergeSort(x);
  SWStop;
end;

procedure TSortingTests.MergeSortF64_TriangleWave;
var x, y: TArray<Double>;
    I, J: Integer;
const N = 1000000;
begin
  SetLength(x, N);
  J := N div 8;
  for I := 0 to N - 1 do
    x[I] := Abs(J - I);

  SWStart;
  y := TSortUtF64.MergeSort(x);
  SWStop;
end;

{$endregion}

initialization

  RegisterTest(TSortingTests.Suite);

end.
