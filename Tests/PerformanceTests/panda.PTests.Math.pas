unit panda.PTests.Math;

interface

uses
    TestFramework
  , panda.Tests.NDATestCase
  , panda.Intfs
  , pandalib
  ;

type
  TMathTests = class(TNDAPerformanceTestCase)
  published
    procedure Dot_Mat;
    procedure Outer_VxV;
    procedure Outer_MxV;
    procedure Outer_VxM;
  end;

  TMathPascalImplTests = class(TNDAPerformanceTestCase)
  published
    procedure Dot_Mat_Pascal;
    procedure Outer_VxV_Pascal;
    procedure Outer_MxV_Pascal;
  end;

implementation

{$region 'TMathTests'}

procedure TMathTests.Dot_Mat;
var a, b: INDArray<Single>;
const N = 500;
begin
  a := nda.Empty<Single>([N, N]);
  b := nda.Empty<Single>([N, N]);

  SWStart;
  nda.Dot(a, b);
  SWStop;
end;

procedure TMathTests.Outer_VxV;
var a, b: INDArray<Single>;
const N = 1000;
begin
  a := nda.Full<Single>([N], 0);
  b := nda.Full<Single>([N], 0);

  SWStart;
  nda.Outer(a, b);
  SWStop;
end;

procedure TMathTests.Outer_MxV;
var a, b: INDArray<Single>;
const N = 200;
begin
  a := nda.Full<Single>([N, N], 0);
  b := nda.Full<Single>([N], 0);

  SWStart;
  nda.Outer(a, b);
  SWStop;
end;

procedure TMathTests.Outer_VxM;
var a, b: INDArray<Single>;
const N = 200;
begin
  a := nda.Full<Single>([N], 0);
  b := nda.Full<Single>([N, N], 0);

  SWStart;
  nda.Outer(a, b);
  SWStop;
end;

{$endregion}

{$region 'TMathPascalImplTests'}

type
//  TReal = Single; // Single to Double conversion significantly decreases performance
  TReal = Double;

procedure TMathPascalImplTests.Dot_Mat_Pascal;
var a, b, c: TArray<TArray<TReal>>;
    ra, rb: TArray<TReal>;
    I, J, K: Integer;
    s: TReal;
const N = 500;
begin
  SetLength(a, N, N);
  SetLength(b, N, N);

  SWStart;

  SetLength(c, N, N);
  SetLength(rb, N);
  for I := 0 to N - 1 do begin // go through all columms of B
    for J := 0 to N - 1 do // take I-th column of B
      rb[J] := b[J, I];
    for J := 0 to N - 1 do begin
      s := 0;
      ra := a[J];
      for K := 0 to N - 1 do // Dot(ra, rb)
        s := s + ra[K] * rb[K];
      c[J, I] := s;
    end;
  end;

//  for I := 0 to N - 1 do
//    for J := 0 to N - 1 do begin
//      s := 0;
//      for K := 0 to N - 1 do
//        s := s + a[I, K] * b[K, I];
//      c[I, J] := s;
//    end;

  SWStop;
end;

procedure TMathPascalImplTests.Outer_VxV_Pascal;
var a, b: TArray<Single>;
    c: TArray<TArray<Single>>;
    I, J: Integer;
const N = 1000;
begin
  SetLength(a, N);
  SetLength(b, N);

  SWStart;

  SetLength(c, N, N);
  for I := 0 to N - 1 do
    for J := 0 to N - 1 do
      c[I, J] := a[I] * b[J];

  SWStop;
end;

procedure TMathPascalImplTests.Outer_MxV_Pascal;
var a: TArray<TArray<Single>>;
    b: TArray<Single>;
    c: TArray<TArray<TArray<Single>>>;
    I, J, K: Integer;
const N = 200;
begin
  SetLength(a, N, N);
  SetLength(b, N);

  SWStart;

  SetLength(c, N, N, N);
  for I := 0 to N - 1 do
    for J := 0 to N - 1 do
      for K := 0 to N - 1 do
        c[I, J, K] := a[I, J] * b[K];

  SWStop;
end;

{$endregion}

initialization

  RegisterTest(TMathPascalImplTests.Suite);
  RegisterTest(TMathTests.Suite);

end.
