unit panda.PTests.Conv;

interface

uses
    TestFramework
  , panda.Tests.NDATestCase
  , panda.Intfs
  , panda.Arrays
  , panda.Conv
  ;

type
  TCorrTests = class(TNDAPerformanceTestCase)
  published
    procedure Corr1D_F32;
    procedure Corr1D_F64;
    procedure Corr2D_F32;
    procedure Corr2D_F64;
  end;

  TCorrPascalImplTests = class(TNDAPerformanceTestCase)
  published
    procedure Corr1D_Pascal_F32;
    procedure Corr1D_Pascal_F64;
    procedure Corr2D_Pascal_F32;
    procedure Corr2D_Pascal_F64;
  end;

implementation

{$region 'TCorrTests'}

procedure TCorrTests.Corr1D_F32;
var k, v: INDArray<Single>;
    corr: TCorrF32;
const
  N = 5000;
  kSz = 10;
begin
  k := TNDAUt.Full<Single>([kSz], 1/kSz);
  v := TNDAUt.Full<Single>([N], 0);

  corr := TCorrF32.Create(k, v.Shape);
  try
    SWStart;
    corr.Evaluate(v);
    SWStop;
  finally
    corr.Free;
  end;
end;

procedure TCorrTests.Corr1D_F64;
var k, v: INDArray<Double>;
    corr: TCorrF64;
const
  N = 5000;
  kSz = 10;
begin
  k := TNDAUt.Full<Double>([kSz], 1/kSz);
  v := TNDAUt.Full<Double>([N], 0);

  corr := TCorrF64.Create(k, v.Shape);
  try
    SWStart;
    corr.Evaluate(v);
    SWStop;
  finally
    corr.Free;
  end;
end;

procedure TCorrTests.Corr2D_F32;
var k, v: INDArray<Single>;
    corr: TCorrF32;
const
  N = 500;
  kSz = 10;
begin
  k := TNDAUt.Full<Single>([kSz, kSz], 1);
  v := TNDAUt.Full<Single>([N, N], 0);

  corr := TCorrF32.Create(k, v.Shape);
  try
    SWStart;
    corr.Evaluate(v);
    SWStop;
  finally
    corr.Free;
  end;
end;

procedure TCorrTests.Corr2D_F64;
var k, v: INDArray<Double>;
    corr: TCorrF64;
const
  N = 500;
  kSz = 10;
begin
  k := TNDAUt.Full<Double>([kSz, kSz], 1);
  v := TNDAUt.Full<Double>([N, N], 0);

  corr := TCorrF64.Create(k, v.Shape);
  try
    SWStart;
    corr.Evaluate(v);
    SWStop;
  finally
    corr.Free;
  end;
end;

{$endregion}

{$region 'TCorrPascalImplTests'}

// Single to Double implicit conversion significantly decreases performance
procedure TCorrPascalImplTests.Corr1D_Pascal_F32;
var k, v, res: TArray<Single>;
    I, J: Integer;
    x: Single;
const N = 5000;
      kSz = 10;
begin
  SetLength(k, kSz);
  for I := 0 to kSz - 1 do
    k[I] := 1/kSz;
  SetLength(v, N);
  SetLength(res, Length(v) - Length(k) + 1);

  SWStart;

  for I := 0 to High(res) do begin
    x := 0;
    for J := 0 to kSz - 1 do
      x := x + k[J] * v[I + J];
    Res[I] := x;
  end;

  SWStop;
end;

procedure TCorrPascalImplTests.Corr1D_Pascal_F64;
var k, v, res: TArray<Double>;
    I, J: Integer;
    x: Double;
const N = 5000;
      kSz = 10;
begin
  SetLength(k, kSz);
  for I := 0 to kSz - 1 do
    k[I] := 1/kSz;
  SetLength(v, N);
  SetLength(res, Length(v) - Length(k) + 1);

  SWStart;

  for I := 0 to High(res) do begin
    x := 0;
    for J := 0 to kSz - 1 do
      x := x + k[J] * v[I + J];
    Res[I] := x;
  end;

  SWStop;
end;

procedure TCorrPascalImplTests.Corr2D_Pascal_F32;
var k, v, res: TArray<TArray<Single>>;
    I, J, L, M: Integer;
    x: Single;
const N = 500;
      kSz = 10;
begin
  SetLength(v, N, N);
  SetLength(k, kSz, kSz);
  SetLength(res, N - kSz + 1, N - kSz + 1);

  SWStart;

  for I := 0 to N - kSz do
    for J := 0 to N - kSz do begin
      x := 0;
      for L := 0 to kSz - 1 do
        for M := 0 to kSz - 1 do
          x := x + k[L, M] * v[I + L, J + M];
      res[I, J] := x;
    end;

  SWStop;
end;

procedure TCorrPascalImplTests.Corr2D_Pascal_F64;
var k, v, res: TArray<TArray<Double>>;
    I, J, L, M: Integer;
    x: Double;
const N = 500;
      kSz = 10;
begin
  SetLength(v, N, N);
  SetLength(k, kSz, kSz);
  SetLength(res, N - kSz + 1, N - kSz + 1);

  SWStart;

  for I := 0 to N - kSz do
    for J := 0 to N - kSz do begin
      x := 0;
      for L := 0 to kSz - 1 do
        for M := 0 to kSz - 1 do
          x := x + k[L, M] * v[I + L, J + M];
      res[I, J] := x;
    end;

  SWStop;
end;

{$endregion}

initialization

  RegisterTest(TCorrPascalImplTests.Suite);
  RegisterTest(TCorrTests.Suite);

end.
