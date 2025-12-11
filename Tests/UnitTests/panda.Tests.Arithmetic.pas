unit panda.Tests.Arithmetic;

interface

uses
    TestFramework
  , panda.Tests.NDATestCase
  , panda.Intfs
  , panda.Arrays
  , panda.Arithmetic
  , pandalib
  ;

type
  TArithmeticTests = class(TNDATestCase)
  protected const
    tol = 1e-5;
  published
    procedure AllClose1D;
    procedure AllClose2D;
    procedure AllClose1Ds;
    procedure AllClose2Ds;
    procedure AllClose2Ds_HGaps;
    procedure AllClose3Ds_HGaps;

    procedure AddScalar_Int;
    procedure Add1D_Int;
    procedure Add2D_Int;

    procedure CvtInt32ToF32;

    procedure DivCCArr1DByCCArr1D_Single;
    procedure DivCCArr1DByScalar_Single;
    procedure DivCCArr2DByScalar_Single;
    procedure DivScalarByCCArr1D_Single;
    procedure DivScalarByCCArr2D_Single;
    procedure DivCCArr2DByCCArr1D_Single;
    procedure DivCCArr1DByCCArr2D_Single;

    procedure SubFrom_1DS_Single;
    procedure SubFrom_1D1D_Single;
    procedure SubFrom_2D1D_Single;
  end;

implementation

{$region 'TArithmeticTests'}

procedure TArithmeticTests.AllClose1D;
var a, b: INDArray<Single>;
begin
  a := nda.AsArray<Single>([1, 2, 3]);
  b := nda.AsArray<Single>([1, 2, 3.1]);

  CheckTrue(ndaAllClose(a, b, 0.15));
  CheckFalse(ndaAllClose(a, b, 0.05));
end;

procedure TArithmeticTests.AllClose2D;
var a, b: INDArray<Single>;
begin
  a := nda.AsArray<Single>([[1, 2, 3], [4, 5, 6]]);
  b := nda.AsArray<Single>([[1, 2, 3.1], [4, 5, 6]]);

  CheckTrue(ndaAllClose(a, b, 0.15));
  CheckFalse(ndaAllClose(a, b, 0.05));
end;

procedure TArithmeticTests.AllClose1Ds;
var a: INDArray<Single>;
begin
  a := nda.AsArray<Single>([1.1, 0.9, 1]);

  CheckTrue(ndaAllClose(a, 1, 0.15));
  CheckFalse(ndaAllClose(a, 1, 0.05));
end;

procedure TArithmeticTests.AllClose2Ds;
var a: INDArray<Single>;
begin
  a := nda.AsArray<Single>([[1.1, 0.9, 1], [0.9, 1, 1.1], [1, 1.1, 0.9]]);

  CheckTrue(ndaAllClose(a, 1, 0.15));
  CheckFalse(ndaAllClose(a, 1, 0.05));
end;

procedure TArithmeticTests.AllClose2Ds_HGaps;
var a, av: INDArray<Single>;
begin
  a := nda.AsArray<Single>([[1.1, 0.9, 1], [0.9, 1, 1.1], [1, 1.1, 0.9]]);
  av := a[[NDIAll, NDIAll(2)]];

  CheckTrue(ndaAllClose(av, 1, 0.15));
  CheckFalse(ndaAllClose(av, 1, 0.05));
end;

procedure TArithmeticTests.AllClose3Ds_HGaps;
var a, av: INDArray<Single>;
begin
  a := nda.Range(1, 1.1, 0.1/(3*3*3)).Reshape([3, 3, 3]);
  av := a[[NDIAll, NDIAll, NDIAll(2)]];

  CheckTrue(ndaAllClose(av, 1, 0.15));
  CheckFalse(ndaAllClose(av, 1, 0.05));
end;

procedure TArithmeticTests.AddScalar_Int;
var a: TNDARecI32;
    res: Integer;
begin
  a := nda.Scalar<Integer>(5);
  a := a + a;

  CheckTrue(nda.TryAsScalar<Integer>(a, res));
  CheckEquals(10, res);
end;

procedure TArithmeticTests.Add1D_Int;
var a: TNDARecI32;
    res: TArray<Integer>;
    I: Integer;
begin
  a := nda.AsArray<Integer>([1, 2, 3, 4, 5]);
  a := a + a;

  CheckTrue(nda.TryAsDynArray<Integer>(a, res));
  CheckEquals(5, Length(res));
  for I := 0 to High(res) do
    CheckEquals(2 * (I + 1), res[I]);
end;

procedure TArithmeticTests.Add2D_Int;
var a: TNDARecI32;
    res: TArray<TArray<Integer>>;
    I, J: Integer;
begin
  a := nda.AsArray<Integer>([1, 2, 3, 4, 5, 6], [2, 3]);
  a := a + a;

  CheckTrue(nda.TryAsDynArray2D<Integer>(a, res));
  CheckEquals(2, Length(res));
  CheckEquals(3, Length(res[0]));
  CheckEquals(3, Length(res[1]));
  for I := 0 to High(res) do
    for J := 0 to High(res[I]) do
      CheckEquals(2*(3*I + J + 1), res[I, J]);
end;

procedure TArithmeticTests.CvtInt32ToF32;
var a: TNDARecI32;
    b: TNDARecF32;
    res: TArray<Single>;
    I: Integer;
begin
  a := nda.AsArray<Integer>([1, 2, 3, 4, 5]);
  b := a;

  CheckTrue(nda.TryAsDynArray<Single>(b, res));
  for I := 0 to High(res) do
    CheckEquals(I + 1, res[I], tol);
end;

procedure TArithmeticTests.DivCCArr1DByCCArr1D_Single;
var a: TNDARecF32;
    res: TArray<Single>;
    I: Integer;
begin
  a := nda.AsArray<Single>([1, 2, 3, 4, 5]);
  a := a / a;

  CheckTrue(nda.TryAsDynArray<Single>(a, res));
  CheckEquals(5, Length(res));
  for I := 0 to High(res) do
    CheckEquals(1, res[I], tol);
end;

procedure TArithmeticTests.DivCCArr1DByScalar_Single;
var a: TNDARecF32;
    res: TArray<Single>;
    I: Integer;
begin
  a := nda.AsArray<Single>([1, 2, 3, 4, 5]);
  a := a / 2;

  CheckTrue(nda.TryAsDynArray<Single>(a, res));
  CheckEquals(5, Length(res));
  for I := 0 to High(res) do
    CheckEquals((I + 1) / 2, res[I], tol);
end;

procedure TArithmeticTests.DivCCArr2DByScalar_Single;
var a: TNDARecF32;
    res: TArray<TArray<Single>>;
    I, J: Integer;
begin
  a := nda.AsArray<Single>([[1, 2, 3], [4, 5, 6]]);
  a := a/2;

  CheckTrue(nda.TryAsDynArray2D<Single>(a, res));
  CheckEquals(2, Length(res));
  CheckEquals(3, Length(res[0]));
  CheckEquals(3, Length(res[1]));
  for I := 0 to High(res) do
    for J := 0 to High(res[I]) do
      CheckEquals((3*I + J + 1)/2, res[I, J], tol);
end;

procedure TArithmeticTests.DivScalarByCCArr1D_Single;
var a: TNDARecF32;
    res: TArray<Single>;
    I: Integer;
begin
  a := nda.AsArray<Single>([1, 2, 3, 4, 5]);
  a := 2/a;

  CheckTrue(nda.TryAsDynArray<Single>(a, res));
  CheckEquals(5, Length(res));
  for I := 0 to High(res) do
    CheckEquals(2 / (I + 1), res[I], tol);
end;

procedure TArithmeticTests.DivScalarByCCArr2D_Single;
var a: TNDARecF32;
    res: TArray<TArray<Single>>;
    I, J: Integer;
begin
  a := nda.AsArray<Single>([[1, 2, 3], [4, 5, 6]]);
  a := 2/a;

  CheckTrue(nda.TryAsDynArray2D<Single>(a, res));
  CheckEquals(2, Length(res));
  CheckEquals(3, Length(res[0]));
  CheckEquals(3, Length(res[1]));
  for I := 0 to High(res) do
    for J := 0 to High(res[I]) do
      CheckEquals(2/(3*I + J + 1), res[I, J], tol);
end;

procedure TArithmeticTests.DivCCArr2DByCCArr1D_Single;
var a, b: TNDARecF32;
    res: TArray<TArray<Single>>;
    I, J: Integer;
begin
  a := nda.AsArray<Single>([[1, 2, 3], [4, 5, 6]]);
  b := nda.AsArray<Single>([1, 2, 3]);
  a := a/b;

  CheckTrue(nda.TryAsDynArray2D<Single>(a, res));
  CheckEquals(2, Length(res));
  CheckEquals(3, Length(res[0]));
  CheckEquals(3, Length(res[1]));
  for I := 0 to High(res) do
    for J := 0 to High(res[I]) do
      CheckEquals((3*I + J + 1) / (J + 1), res[I, J], tol);
end;

procedure TArithmeticTests.DivCCArr1DByCCArr2D_Single;
var a, b: TNDARecF32;
    res: TArray<TArray<Single>>;
    I, J: Integer;
begin
  a := nda.AsArray<Single>([[1, 2, 3], [4, 5, 6]]);
  b := nda.AsArray<Single>([1, 2, 3]);
  a := b/a;

  CheckTrue(nda.TryAsDynArray2D<Single>(a, res));
  CheckEquals(2, Length(res));
  CheckEquals(3, Length(res[0]));
  CheckEquals(3, Length(res[1]));
  for I := 0 to High(res) do
    for J := 0 to High(res[I]) do
      CheckEquals((J + 1) / (3*I + J + 1), res[I, J], tol);
end;

procedure TArithmeticTests.SubFrom_1DS_Single;
var a: TNDARecF32;
    res: TArray<Single>;
    I: Integer;
begin
  a := nda.AsArray<Single>([1, 2, 3]);
  a.SubtractFrom(1);

  CheckTrue(nda.TryAsDynArray<Single>(a, res));
  CheckEquals(3, Length(res));
  for I := 0 to High(res) do
    CheckEquals(I, res[I], tol);
end;

procedure TArithmeticTests.SubFrom_1D1D_Single;
var a: TNDARecF32;
    res: TArray<Single>;
    I: Integer;
begin
  a := nda.AsArray<Single>([1, 2, 3]);
  a.SubtractFrom(a/2);

  CheckTrue(nda.TryAsDynArray<Single>(a, res));
  CheckEquals(3, Length(res));
  for I := 0 to High(res) do
    CheckEquals((I + 1)/2, res[I], tol);
end;

procedure TArithmeticTests.SubFrom_2D1D_Single;
var a, b: TNDARecF32;
    res: TArray<TArray<Single>>;
    I, J: Integer;
begin
  a := nda.AsArray<Single>([[1, 2, 3], [4, 5, 6]]);
  b := nda.AsArray<Single>([0, 1, 2]);
  a.SubtractFrom(b);

  CheckTrue(nda.TryAsDynArray2D<Single>(a, res));
  CheckEquals(2, Length(res));
  CheckEquals(3, Length(res[0]));
  CheckEquals(3, Length(res[1]));
  for I := 0 to High(res) do
    for J := 0 to High(res[I]) do
      CheckEquals(1 + 3*I, res[I, J], tol);
end;

{$endregion}

initialization

  RegisterTest(TArithmeticTests.Suite);

end.
