unit panda.Tests.Conv;

interface

uses
    TestFramework
  , panda.Intfs
  , panda.Arrays
  , panda.Conv
  , panda.Tests.NDATestCase

  , panda.Formatter
  ;

type
  TTestCorr = class(TCorrF32)
  end;

  TCorrTests = class(TNDATestCase)
  protected const
    tol = 1e-5;
  published
    procedure Correlate1D;
    procedure Correlate2D;
    procedure Correlate1D_K3;
    procedure Correlate2D_K3;
    procedure Corr1D_Impulse;
    procedure Convolve1D;
    procedure Convolve2D;
    procedure OutShape_KASameDim;
    procedure OutShape_SmallerKDim;
    procedure OutShape_SmallerADim;
    procedure Corr_K1D_A2D;
    procedure Corr_K2D_A3D;
    procedure Corr_K2D_A1D;
    procedure Corr_K3D_A2D;
  end;

implementation

{$region 'TCorrTests'}

procedure TCorrTests.Correlate1D;
var k, ls, res: INDArray<Single>;
    v: TArray<Single>;
    I: Integer;
const cLs: array [0..6] of Single = (1, 3, 2, 5, 1, 4, 3);
begin
  k := TNDAUt.AsArray<Single>([1, 2]);
  ls := TNDAUt.AsArray<Single>(cLs);

  res := ndaCorrelate(k, ls);
  TNDAUt.TryAsDynArray<Single>(res, v);

  CheckEquals(6, Length(v));
  for I := 0 to High(v) do
    CheckEquals(cLs[I] + 2*cLs[I + 1], v[I], tol);
end;

procedure TCorrTests.Correlate2D;
var k, ls, res: INDArray<Single>;
    m: TArray<TArray<Single>>;
    I, J: Integer;
const
  cLsSz = 6;
  cLs: array [0..cLsSz - 1, 0..cLsSz - 1] of Single = (
    (1, 2, 3, 4, 5, 6),
    (2, 3, 4, 3, 2, 1),
    (1, 3, 3, 2, 4, 5),
    (4, 3, 2, 1, 1, 2),
    (2, 2, 1, 3, 4, 5),
    (6, 5, 4, 5, 6, 3)
  );
begin
  k := TNDAUt.AsArray<Single>([[1, 2], [3, 4]]);
  ls := TNDPackedArray<Single>.Create(@cLs, [cLsSz, cLsSz]);

  res := ndaCorrelate(k, ls);
  TNDAUt.TryAsDynArray2D<Single>(res, m);

  CheckEquals(5, Length(m));
  for I := 0 to High(m) do
    CheckEquals(5, Length(m[I]));
  for I := 0 to High(m) do
    for J := 0 to High(m[I]) do
      CheckEquals(cLs[I,J]+2*cLs[I,J+1]+3*cLs[I+1,J]+4*cLs[I+1,J+1], m[I,J], tol);
end;

procedure TCorrTests.Correlate1D_K3;
var k, ls, res: INDArray<Single>;
    v: TArray<Single>;
    I: Integer;
const cLs: array [0..6] of Single = (1, 3, 2, 5, 1, 4, 3);
begin
  k := TNDAUt.AsArray<Single>([1, 2, 3]);
  ls := TNDAUt.AsArray<Single>(cLs);

  res := ndaCorrelate(k, ls);
  TNDAUt.TryAsDynArray<Single>(res, v);

  CheckEquals(5, Length(v));
  for I := 0 to High(v) do
    CheckEquals(cLs[I] + 2*cLs[I + 1] + 3*cLs[I + 2], v[I], tol);
end;

procedure TCorrTests.Correlate2D_K3;
var k, ls, res: INDArray<Single>;
    m: TArray<TArray<Single>>;
    I, J: Integer;
const
  cLsSz = 6;
  cLs: array [0..cLsSz - 1, 0..cLsSz - 1] of Single = (
    (1, 2, 3, 4, 5, 6),
    (2, 3, 4, 3, 2, 1),
    (1, 3, 3, 2, 4, 5),
    (4, 3, 2, 1, 1, 2),
    (2, 2, 1, 3, 4, 5),
    (6, 5, 4, 5, 6, 3)
  );
begin
  k := TNDAUt.AsArray<Single>([[1, 2, 3], [4, 5, 6], [7, 8, 9]]);
  ls := TNDPackedArray<Single>.Create(@cLs, [cLsSz, cLsSz]);

  res := ndaCorrelate(k, ls);
  TNDAUt.TryAsDynArray2D<Single>(res, m);

  CheckEquals(4, Length(m));
  for I := 0 to High(m) do
    CheckEquals(4, Length(m[I]));
  for I := 0 to High(m) do
    for J := 0 to High(m[I]) do
      CheckEquals(
        cLs[I,J]     + 2*cLs[I,J+1]   + 3*cLs[I,J+2]+
        4*cLs[I+1,J] + 5*cLs[I+1,J+1] + 6*cLs[I+1,J+2] +
        7*cLs[I+2,J] + 8*cLs[I+2,J+1] + 9*cLs[I+2,J+2],
        m[I,J], tol);
end;

procedure TCorrTests.Corr1D_Impulse;
var k, ls, res: INDArray<Single>;
    v: TArray<Single>;
    I: Integer;
begin
  k := TNDAUt.Full<Single>([5], 1/5);
  SetLength(v, 201);
  v[100] := 1;
  ls := TDynArrWrapper<Single>.Create(v);

  res := ndaCorrelate(k, ls);
  TNDAUt.TryAsDynArray<Single>(res, v);

  CheckEquals(ls.Shape[0] - k.Shape[0] + 1, Length(v));
  for I := 0 to 95 do
    CheckEquals(0, v[I], tol);
  for I := 96 to 100 do
    CheckEquals(1/5, v[I], tol);
  for I := 101 to High(v) do
    CheckEquals(0, v[I], tol);
end;

procedure TCorrTests.Convolve1D;
var k, ls, res: INDArray<Single>;
    v: TArray<Single>;
    I: Integer;
const cLs: array [0..6] of Single = (1, 3, 2, 5, 1, 4, 3);
begin
  k := TNDAUt.AsArray<Single>([1, 2]);
  ls := TNDAUt.AsArray<Single>(cLs);

  res := ndaConvolve(k, ls);
  TNDAUt.TryAsDynArray<Single>(res, v);

  CheckEquals(6, Length(v));
  for I := 0 to High(v) do
    CheckEquals(2*cLs[I] + cLs[I + 1], v[I], tol);
end;

procedure TCorrTests.Convolve2D;
var k, ls, res: INDArray<Single>;
    m: TArray<TArray<Single>>;
    I, J: Integer;
const
  cLsSz = 6;
  cLs: array [0..cLsSz - 1, 0..cLsSz - 1] of Single = (
    (1, 2, 3, 4, 5, 6),
    (2, 3, 4, 3, 2, 1),
    (1, 3, 3, 2, 4, 5),
    (4, 3, 2, 1, 1, 2),
    (2, 2, 1, 3, 4, 5),
    (6, 5, 4, 5, 6, 3)
  );
begin
  k := TNDAUt.AsArray<Single>([[1, 2], [3, 4]]);
  ls := TNDPackedArray<Single>.Create(@cLs, [cLsSz, cLsSz]);

  res := ndaConvolve(k, ls);
  TNDAUt.TryAsDynArray2D<Single>(res, m);

  CheckEquals(5, Length(m));
  for I := 0 to High(m) do
    CheckEquals(5, Length(m[I]));
  for I := 0 to High(m) do
    for J := 0 to High(m[I]) do
      CheckEquals(4*cLs[I,J]+3*cLs[I,J+1]+2*cLs[I+1,J]+cLs[I+1,J+1], m[I,J], tol);
end;

procedure TCorrTests.OutShape_KASameDim;
var kSh, aSh, resSh: TNDAShape;
begin
  kSh := TNDAShape.Create(2, 3);
  aSh := TNDAShape.Create(5, 5);

  resSh := TCorr.OutShape(aSh, kSh);

  CheckEquals(2, Length(resSh));
  CheckEquals(4, resSh[0]);
  CheckEquals(3, resSh[1]);
end;

procedure TCorrTests.OutShape_SmallerKDim;
var kSh, aSh, resSh: TNDAShape;
begin
  kSh := TNDAShape.Create(6, 2, 3);
  aSh := TNDAShape.Create(5, 5);

  resSh := TCorr.OutShape(aSh, kSh);

  CheckEquals(3, Length(resSh));
  CheckEquals(6, resSh[0]);
  CheckEquals(4, resSh[1]);
  CheckEquals(3, resSh[2]);
end;

procedure TCorrTests.OutShape_SmallerADim;
var kSh, aSh, resSh: TNDAShape;
begin
  kSh := TNDAShape.Create(2, 3);
  aSh := TNDAShape.Create(6, 5, 5);

  resSh := TCorr.OutShape(aSh, kSh);

  CheckEquals(3, Length(resSh));
  CheckEquals(6, resSh[0]);
  CheckEquals(4, resSh[1]);
  CheckEquals(3, resSh[2]);
end;

procedure TCorrTests.Corr_K1D_A2D;
var k, ls, res: INDArray<Single>;
    m: TArray<TArray<Single>>;
    I: Integer;
const cLs: array [0..1, 0..5] of Single = (
    (2, 3, 4, 3, 2, 1),
    (1, 3, 3, 2, 4, 5)
  );
begin
  k := TNDAUt.AsArray<Single>([1, 2]);
  ls := TNDPackedArray<Single>.Create(@cLs, [2, 6]);

  res := ndaCorrelate(k, ls);
  TNDAUt.TryAsDynArray2D<Single>(res, m);

  CheckEquals(2, Length(m));
  CheckEquals(5, Length(m[0]));
  for I := 0 to High(m[0]) do
    CheckEquals(cLs[0,I]+2*cLs[0,I+1], m[0,I], tol);
  for I := 0 to High(m[1]) do
    CheckEquals(cLs[1,I]+2*cLs[1,I+1], m[1,I], tol);
end;

procedure TCorrTests.Corr_K2D_A3D;
var k, ls: INDArray<Single>;
    res: TNDAItems<Single>;
    sh: TArray<NativeInt>;
    I, J: Integer;
const
  cLsSz = 6;
  cLs: array [0..1, 0..cLsSz - 1, 0..cLsSz - 1] of Single = (
     (
      (1, 2, 3, 4, 5, 6),
      (2, 3, 4, 3, 2, 1),
      (1, 3, 3, 2, 4, 5),
      (4, 3, 2, 1, 1, 2),
      (2, 2, 1, 3, 4, 5),
      (6, 5, 4, 5, 6, 3)
    ),
    (
      (2, 3, 5, 4, 1, 2),
      (1, 2, 4, 2, 4, 2),
      (2, 4, 1, 5, 6, 3),
      (3, 2, 1, 2, 4, 2),
      (4, 5, 2, 3, 2, 4),
      (1, 2, 4, 3, 7, 2)
    )
  );
begin
  k := TNDAUt.AsArray<Single>([[1, 2], [3, 4]]);
  ls := TNDPackedArray<Single>.Create(@cLs, [2, cLsSz, cLsSz]);

  res := ndaCorrelate(k, ls);
  sh := res.Shape;
  CheckEquals(3, Length(sh));
  CheckEquals(2, sh[0]);
  CheckEquals(5, sh[1]);
  CheckEquals(5, sh[2]);

  for I := 0 to sh[1] - 1 do
    for J := 0 to sh[2] - 1 do
      CheckEquals(cLs[0,I,J]+2*cLs[0,I,J+1]+3*cLs[0,I+1,J]+4*cLs[0,I+1,J+1],res[[0,I,J]], tol);

  for I := 0 to sh[1] - 1 do
    for J := 0 to sh[2] - 1 do
      CheckEquals(cLs[1,I,J]+2*cLs[1,I,J+1]+3*cLs[1,I+1,J]+4*cLs[1,I+1,J+1], res[[1,I,J]], tol);
end;

procedure TCorrTests.Corr_K2D_A1D;
var k, ls, res: INDArray<Single>;
    v: TArray<TArray<Single>>;
    I: Integer;
const cLs: array [0..6] of Single = (1, 3, 2, 5, 1, 4, 3);
begin
  k := TNDAUt.AsArray<Single>([[1, 2], [3, 4]]);
  ls := TNDAUt.AsArray<Single>(cLs);

  res := ndaCorrelate(k, ls);
  TNDAUt.TryAsDynArray2D<Single>(res, v);

  CheckEquals(2, Length(v));
  CheckEquals(6, Length(v[0]));
  for I := 0 to High(v[0]) do
    CheckEquals(cLs[I] + 2*cLs[I + 1], v[0, I], tol);
  CheckEquals(6, Length(v[1]));
  for I := 0 to High(v[1]) do
    CheckEquals(3*cLs[I] + 4*cLs[I + 1], v[1, I], tol);
end;

procedure TCorrTests.Corr_K3D_A2D;
var k, ls: INDArray<Single>;
    res: TNDAItems<Single>;
    sh: TNDAShape;
    I, J: Integer;
const
  cLsSz = 6;
  cLs: array [0..cLsSz - 1, 0..cLsSz - 1] of Single = (
    (1, 2, 3, 4, 5, 6),
    (2, 3, 4, 3, 2, 1),
    (1, 3, 3, 2, 4, 5),
    (4, 3, 2, 1, 1, 2),
    (2, 2, 1, 3, 4, 5),
    (6, 5, 4, 5, 6, 3)
  );
begin
  k := TNDAUt.AsType<Single>(iRng2NDA([2, 2, 2]).Reshape([2, 2, 2]));
  ls := TNDPackedArray<Single>.Create(@cLs, [cLsSz, cLsSz]);

  res := ndaCorrelate(k, ls);
  sh := res.Shape;
  CheckEquals(3, Length(sh));
  CheckEquals(2, sh[0]);
  CheckEquals(5, sh[1]);
  CheckEquals(5, sh[2]);

  for I := 0 to sh[1] - 1 do
    for J := 0 to sh[2] - 1 do
      CheckEquals(cLs[I,J]+2*cLs[I,J+1]+3*cLs[I+1,J]+4*cLs[I+1,J+1],res[[0,I,J]], tol);

  for I := 0 to sh[1] - 1 do
    for J := 0 to sh[2] - 1 do
      CheckEquals(5*cLs[I,J]+6*cLs[I,J+1]+7*cLs[I+1,J]+8*cLs[I+1,J+1], res[[1,I,J]], tol);
end;

{$endregion}

initialization

  RegisterTest(TCorrTests.Suite);

end.
