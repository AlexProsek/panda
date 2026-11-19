unit panda.LinAlg.Tests.LUPC128;
interface

uses
    panda.LinAlg.Base
  , TestFramework
  , panda.Nums
  , panda.Intfs
  , panda.Arrays
  , panda.ArrManip
  , panda.LinAlg.LUPC128
  , panda.LinAlg.LUP
  , panda.Tests.NDATestCase
  ;

type
  TCVector = TArray<TCmplx128>;
  TCMatrix = TArray<TCVector>;
  TLUPDecompositionC128Tests = class(TNDATestCase)
  protected
    solver: TLUPDecompositionC128;
    function Matrix: TCMatrix;
    function BandMatrix: TCMatrix;
    function Product(const A: TCMatrix; const X: TCVector): TCVector;
    procedure CheckSolution(const A: TCMatrix; const B, X: TCVector);
    procedure CheckInverse(const A, IA: TCMatrix);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure OneEquation;
    procedure PivotMagnitude;
    procedure PivotTie;
    procedure LUPReconstruction;
    procedure LUPVectorSolve;
    procedure RepeatedCopiedSolves;
    procedure InverseAndDeterminant;
    procedure SingularMatrix;
    procedure RejectNDLayouts;
    procedure LUPMultipleRHS;
    procedure InverseND;
    procedure SmallScale;
    procedure LUPVectorND;
    procedure LUPStridedND;
    procedure BandLU;

  end;

type
  TBlockedLUPC128Tests = class(TLUPDecompositionC128Tests)
  public
    procedure SetUp; override;
  published
    procedure BlockedPanelSizes;
  end;

implementation

type
  TLUPAccess = class(TLUPDecompositionC128);

procedure TLUPDecompositionC128Tests.SetUp;
begin
  solver := TLUPDecompositionC128.Create;
end;

procedure TLUPDecompositionC128Tests.TearDown;
begin
  solver.Free;
end;

function TLUPDecompositionC128Tests.Matrix: TCMatrix;
begin
  Result := TCMatrix.Create(TCVector.Create(Cmplx(1, 1), Cmplx(2, -1)),
    TCVector.Create(Cmplx(0, 3), Cmplx(-1, 2)));
end;

function TLUPDecompositionC128Tests.Product(const A: TCMatrix; const X: TCVector): TCVector;
var I, J: Integer;
begin
  SetLength(Result, Length(A));
  for I := 0 to High(A) do begin
    Result[I] := TCmplx128.Zero;
    for J := 0 to High(X) do
      Result[I] := Result[I] + A[I, J] * X[J];
  end;
end;

procedure TLUPDecompositionC128Tests.CheckSolution(const A: TCMatrix; const B, X: TCVector);
var Y: TCVector;
    I: Integer;
begin
  CheckEquals(Length(A[0]), Length(X));
  Y := Product(A, X);
  CheckEquals(Length(B), Length(Y));
  for I := 0 to High(B) do
    CheckEquals(B[I], Y[I], 1e-12);
end;

procedure TLUPDecompositionC128Tests.CheckInverse(const A, IA: TCMatrix);
var I, J, K: Integer;
    sum, expected: TCmplx128;
begin
  CheckEquals(Length(A), Length(IA));
  for I := 0 to High(A) do
    for J := 0 to High(A) do begin
      sum := TCmplx128.Zero;
      for K := 0 to High(A) do
        sum := sum + A[I, K] * IA[K, J];
      expected := TCmplx128.Zero;
      if I = J then expected := TCmplx128.ReUnit;
      CheckEquals(expected, sum, 1e-12);
    end;
end;

procedure TLUPDecompositionC128Tests.OneEquation;
var A: TCMatrix;
    B, X: TCVector;
begin
  A := TCMatrix.Create(TCVector.Create(Cmplx(0, 2)));
  B := TCVector.Create(Cmplx(4, 6));

  CheckTrue(solver.InitializeWithoutCopy(A) = lsisInitialized);
  solver.Solve(B, X);

  CheckEquals(Cmplx(3, -2), X[0], 1e-12);
end;

procedure TLUPDecompositionC128Tests.PivotMagnitude;
var A: TCMatrix;
    P: TArray<NativeInt>;
begin
  A := TCMatrix.Create(
    TCVector.Create(Cmplx(2, 0), Cmplx(1, 0)),
    TCVector.Create(Cmplx(0, 3), Cmplx(0, 1))
  );

  CheckTrue(solver.InitializeWithoutCopy(A) = lsisInitialized);
  P := Copy(TLUPAccess(solver).fP);

  CheckEquals(1, Integer(P[0]));
end;

procedure TLUPDecompositionC128Tests.PivotTie;
var A: TCMatrix;
    P: TArray<NativeInt>;
begin
  A := TCMatrix.Create(
    TCVector.Create(Cmplx(3, 4), Cmplx(1, 0)),
    TCVector.Create(Cmplx(0, 5), Cmplx(0, 2))
  );

  CheckTrue(solver.InitializeWithoutCopy(A) = lsisInitialized);
  P := Copy(TLUPAccess(solver).fP);

  CheckEquals(0, Integer(P[0]));
end;

procedure TLUPDecompositionC128Tests.LUPReconstruction;
var A, LU: TCMatrix;
    P: TArray<NativeInt>;
    I, J, K: Integer;
    L, U, sum: TCmplx128;
begin
  A := Matrix;
  LU := Matrix;

  CheckTrue(solver.InitializeWithoutCopy(LU) = lsisInitialized);
  P := Copy(TLUPAccess(solver).fP);

  for I := 0 to High(A) do
    for J := 0 to High(A) do begin
      sum := TCmplx128.Zero;
      for K := 0 to High(A) do begin
        L := TCmplx128.Zero;
        if I = K then L := TCmplx128.ReUnit;
        if I > K then L := LU[I, K];
        U := TCmplx128.Zero;
        if K <= J then U := LU[K, J];
        sum := sum + L * U;
      end;
      CheckEquals(A[P[I], J], sum, 1e-12);
    end;
end;

procedure TLUPDecompositionC128Tests.RepeatedCopiedSolves;
var A: TCMatrix;
    B1, B2, X1, X2: TCVector;
begin
  A := Matrix;
  B1 := TCVector.Create(Cmplx(1, 1), Cmplx(2, -1));
  B2 := TCVector.Create(Cmplx(2, -1), Cmplx(1, 1));

  CheckEquals(Ord(lsisInitialized), Ord(solver.Initialize(A)));

  A := nil;
  CheckEquals(Ord(lsssSolved), Ord(solver.Solve(B1, X1)));
  CheckEquals(Ord(lsssSolved), Ord(solver.Solve(B2, X2)));

  CheckTrue(solver.Initialized);
  CheckSolution(Matrix, B1, X1);
  CheckSolution(Matrix, B2, X2);

  CheckEquals(Ord(lsisInitialized), Ord(solver.Initialize(Matrix)));

  CheckSolution(Matrix, B1, X1);
  CheckSolution(Matrix, B2, X2);
end;

procedure TLUPDecompositionC128Tests.LUPVectorSolve;
var A, LU: TCMatrix;
    B, X: TCVector;
begin
  A := Matrix;
  LU := Matrix;
  B := TCVector.Create(Cmplx(4, 2), Cmplx(-1, 3));

  CheckTrue(solver.InitializeWithoutCopy(LU) = lsisInitialized);
  solver.Solve(B, X);

  CheckSolution(A, B, X);
end;

procedure TLUPDecompositionC128Tests.InverseAndDeterminant;
var A, IA, LU: TCMatrix;
    D: TCmplx128;
begin
  A := Matrix;
  LU := Matrix;
  D := A[0, 0] * A[1, 1] - A[0, 1] * A[1, 0];

  CheckTrue(solver.Initialize(A) = lsisInitialized);
  solver.Invert(IA);

  CheckInverse(A, IA);
  CheckTrue(solver.Initialize(A) = lsisInitialized);
  CheckEquals(D, solver.Determinant, 1e-12);
  CheckEquals(Cmplx(1, 1), A[0, 0], 0);

  CheckTrue(solver.InitializeWithoutCopy(LU) = lsisInitialized);
  solver.Invert(IA);

  CheckInverse(A, IA);
  CheckEquals(D, solver.Determinant, 1e-12);
end;

procedure TLUPDecompositionC128Tests.SingularMatrix;
var A: TCMatrix;
begin
  A := TCMatrix.Create(
    TCVector.Create(Cmplx(0, 1), Cmplx(2, 1)),
    TCVector.Create(Cmplx(0, 2), Cmplx(4, 2))
  );

  CheckEquals(Ord(lsisSingular), Ord(solver.Initialize(A)));
  CheckFalse(solver.Initialized);
end;

procedure TLUPDecompositionC128Tests.RejectNDLayouts;
var M, V: INDArray<TCmplx128>;
begin
  M := TNDAUt.AsArray<TCmplx128>(Matrix);
  V := TNDAMan.TransposedView<TCmplx128>(M);

  CheckFalse(solver.InitializeWithoutCopy(V) = lsisInitialized);
end;

procedure TLUPDecompositionC128Tests.LUPMultipleRHS;
var A, B, X: TCMatrix;
    M, MB, MX: INDArray<TCmplx128>;
    J: Integer;
begin
  A := Matrix;
  B := TCMatrix.Create(
    TCVector.Create(Cmplx(1, 2), Cmplx(3, -1)),
    TCVector.Create(Cmplx(-2, 1), Cmplx(4, 3))
  );
  M := TNDAUt.AsArray<TCmplx128>(A);
  MB := TNDAUt.AsArray<TCmplx128>(B);

  CheckTrue(solver.InitializeWithoutCopy(M) = lsisInitialized);
  solver.Solve(MB, MX);

  CheckTrue(TNDAUt.TryAsDynArray2D<TCmplx128>(MX, X));
  for J := 0 to 1 do
    CheckSolution(A, TCVector.Create(B[0, J], B[1, J]), TCVector.Create(X[0, J], X[1, J]));
end;

procedure TLUPDecompositionC128Tests.InverseND;
var A, IA: TCMatrix;
    M, MI: INDArray<TCmplx128>;
begin
  A := Matrix;
  M := TNDAUt.AsArray<TCmplx128>(A);

  CheckTrue(solver.Initialize(M) = lsisInitialized);
  solver.Invert(MI);

  CheckTrue(TNDAUt.TryAsDynArray2D<TCmplx128>(MI, IA));
  CheckInverse(A, IA);
end;

procedure TLUPDecompositionC128Tests.SmallScale;
var A, IA: TCMatrix;
begin
  A := TCMatrix.Create(TCVector.Create(Cmplx(1e-20, 1e-20)));

  CheckTrue(solver.Initialize(A) = lsisInitialized);
  solver.Invert(IA);

  CheckInverse(A, IA);
  CheckTrue(solver.Initialize(A) = lsisInitialized);
  CheckEquals(Cmplx(1e-20, 1e-20), solver.Determinant, 1e-32);
end;

procedure TLUPDecompositionC128Tests.LUPVectorND;
var A, IA: TCMatrix;
    B, X: TCVector;
    M, MB, MX, MI: INDArray<TCmplx128>;
begin
  A := Matrix;
  B := TCVector.Create(Cmplx(4, 2), Cmplx(-1, 3));
  M := TNDAUt.AsArray<TCmplx128>(A);
  MB := TNDAUt.AsArray<TCmplx128>(B);

  CheckTrue(solver.InitializeWithoutCopy(M) = lsisInitialized);
  solver.Solve(MB, MX);
  solver.Invert(MI);

  CheckTrue(TNDAUt.TryAsDynArray<TCmplx128>(MX, X));
  CheckSolution(A, B, X);
  CheckTrue(TNDAUt.TryAsDynArray2D<TCmplx128>(MI, IA));
  CheckInverse(A, IA);
end;

procedure TLUPDecompositionC128Tests.LUPStridedND;
var A, Wide: TCMatrix;
    B, X: TCVector;
    M, V, MX: INDArray<TCmplx128>;
begin
  A := Matrix;
  Wide := TCMatrix.Create(
    TCVector.Create(A[0, 0], A[0, 1], Cmplx(99, 1)),
    TCVector.Create(A[1, 0], A[1, 1], Cmplx(99, 1))
  );
  B := TCVector.Create(Cmplx(4, 2), Cmplx(-1, 3));
  M := TNDAUt.AsArray<TCmplx128>(Wide);
  V := M[[NDIAll, NDISpan(0, 1)]];

  CheckTrue(solver.InitializeWithoutCopy(V) = lsisInitialized);
  solver.Solve(TNDAUt.AsArray<TCmplx128>(B), MX);

  CheckTrue(TNDAUt.TryAsDynArray<TCmplx128>(MX, X));
  CheckSolution(A, B, X);
  CheckTrue(TNDAUt.TryAsDynArray2D<TCmplx128>(M, Wide));
  CheckEquals(Cmplx(99, 1), Wide[0, 2], 0);
  CheckEquals(Cmplx(99, 1), Wide[1, 2], 0);
end;

function TLUPDecompositionC128Tests.BandMatrix: TCMatrix;
begin
  Result := TCMatrix.Create(
    TCVector.Create(Cmplx(0, 0), Cmplx(2, 1), Cmplx(0, 0), Cmplx(0, 0)),
    TCVector.Create(Cmplx(3, -1), Cmplx(1, 2), Cmplx(4, 1), Cmplx(0, 0)),
    TCVector.Create(Cmplx(0, 0), Cmplx(5, 2), Cmplx(2, -1), Cmplx(3, 1)),
    TCVector.Create(Cmplx(0, 0), Cmplx(0, 0), Cmplx(6, 1), Cmplx(4, -2))
  );
end;

procedure TLUPDecompositionC128Tests.BandLU;
var A, LU, IA: TCMatrix;
    B, X: TCVector;
    dense: TLUPDecompositionC128;
begin
  A := BandMatrix;
  LU := BandMatrix;
  B := Product(A, TCVector.Create(Cmplx(1, 1), Cmplx(2, -1), Cmplx(3, 2), Cmplx(4, -2)));
  solver.LowerBandWidth := 1;
  solver.UpperBandWidth := 1;
  dense := TLUPDecompositionC128.Create;
  try
    CheckTrue(dense.Initialize(A) = lsisInitialized);

    CheckTrue(solver.InitializeWithoutCopy(LU) = lsisInitialized);
    solver.Solve(B, X);
    solver.Invert(IA);

    CheckSolution(A, B, X);
    CheckInverse(A, IA);
    CheckEquals(solver.Determinant, dense.Determinant, 1e-12);
  finally
    dense.Free;
  end;
end;

procedure TBlockedLUPC128Tests.SetUp;
begin
  inherited;
  solver.FactorizationMethod := lupBlocked;
  solver.BlockSize := 2;
end;

procedure TBlockedLUPC128Tests.BlockedPanelSizes;
const rows: array[0..4] of Integer = (4, 2, 0, 3, 1);
var A, LU, IA: TCMatrix;
    B, expectedX, X: TCVector;
    P: TArray<NativeInt>;
    I, J, K, R, size: Integer;
    L, S, expectedDet: TCmplx128;
    ordinary: TLUPDecompositionC128;
begin
  SetLength(A, 5);
  SetLength(B, 5);
  SetLength(expectedX, 5);
  for I := 0 to 4 do begin
    SetLength(A[I], 5);
    expectedX[I] := Cmplx(I + 1, 0);
    R := rows[I];
    for J := 0 to 4 do begin
      A[I, J] := Cmplx((R * 3 + J * 2) mod 7 - 3, (I + J) mod 3 - 1) / Cmplx(10, 0);
      if R = J then A[I, J] := A[I, J] + Cmplx(10, 0);
    end;
  end;
  for I := 0 to 4 do begin
    B[I] := TCmplx128.Zero;
    for J := 0 to 4 do B[I] := B[I] + A[I, J] * expectedX[J];
  end;
  ordinary := TLUPDecompositionC128.Create;
  try
    CheckTrue(ordinary.Initialize(A) = lsisInitialized);
    expectedDet := ordinary.Determinant;
  finally
    ordinary.Free;
  end;
  solver.FactorizationMethod := lupBlocked;
  for size in [1, 2, 3, 5, 8] do begin
    LU := TCMatrix.Create(Copy(A[0]), Copy(A[1]), Copy(A[2]), Copy(A[3]), Copy(A[4]));
    solver.BlockSize := size;

    CheckTrue(solver.InitializeWithoutCopy(LU) = lsisInitialized);
    P := Copy(TLUPAccess(solver).fP);
    solver.Solve(B, X);
    solver.Invert(IA);

    CheckSolution(A, B, X);
    CheckInverse(A, IA);
    CheckEquals(expectedDet, solver.Determinant, 1e-6);
    for I := 0 to 4 do
      for J := 0 to 4 do begin
        S := TCmplx128.Zero;
        for K := 0 to 4 do begin
          L := TCmplx128.Zero;
          if I = K then L := TCmplx128.ReUnit;
          if I > K then L := LU[I, K];
          if K <= J then S := S + L * LU[K, J];
        end;

        CheckEquals(A[P[I], J], S, 1e-10);
      end;
  end;
end;

initialization

  RegisterTest(TLUPDecompositionC128Tests.Suite);
  RegisterTest(TBlockedLUPC128Tests.Suite);

end.
