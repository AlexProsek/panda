unit panda.LinAlg.Tests.LUPF128;

interface

uses
    panda.LinAlg.Base
  , TestFramework
  , panda.NumsQP
  , panda.Intfs
  , panda.Arrays
  , panda.ArrManip
  , panda.LinAlg.LUPF128
  , panda.LinAlg.LUP
  ;

type
  TQPVector = TArray<TReal128>;
  TQPMatrix = TArray<TQPVector>;

  TLUPDecompositionF128Tests = class(TTestCase)
  protected
    solver: TLUPDecompositionF128;
    function Matrix: TQPMatrix;
    function BandMatrix: TQPMatrix;
    procedure CheckClose(const Expected, Actual: TReal128);
    procedure CheckSolution(const A: TQPMatrix; const B, X: TQPVector);
    procedure CheckInverse(const A, IA: TQPMatrix);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure LUPReconstruction;
    procedure BlockedPanelSizes;
    procedure LUPVectorSolve;
    procedure RepeatedCopiedSolves;
    procedure InverseDynamic;
    procedure LUPInverseDynamic;
    procedure InverseSmallScale;
    procedure DeterminantLargeScale;
    procedure SingularMatrix;
    procedure RejectNDLayouts;
    procedure LUPMultipleRHS;
    procedure LUPVectorND;
    procedure LUPStridedND;
    procedure InverseND;
    procedure ToleranceBoundary;
    procedure BandLU;
  end;

type
  TBlockedLUPF128Tests = class(TLUPDecompositionF128Tests)
  public
    procedure SetUp; override;
  end;

implementation

type
  TLUPAccess = class(TLUPDecompositionF128);

function Q(aValue: Double): TReal128;
begin
  Result := aValue;
end;

function Pow2(aExponent: Integer): TReal128;
begin
  Result := cOneF128;
  Result.Exponent := aExponent;
end;

procedure TLUPDecompositionF128Tests.SetUp;
begin
  solver := TLUPDecompositionF128.Create;
end;

procedure TLUPDecompositionF128Tests.TearDown;
begin
  solver.Free;
end;

function TLUPDecompositionF128Tests.Matrix: TQPMatrix;
begin
  Result := TQPMatrix.Create(TQPVector.Create(Q(0), Q(2)),
    TQPVector.Create(Q(3), Q(4)));
end;

procedure TLUPDecompositionF128Tests.CheckClose(const Expected, Actual: TReal128);
var Scale, Error: TReal128;
begin
  Scale := Expected.Abs;
  if Scale < cOneF128 then Scale := cOneF128;
  Error := (Actual - Expected).Abs;

  CheckTrue(Error <= Q(4096) * cEpsF128 * Scale, 'TReal128 residual');
end;

procedure TLUPDecompositionF128Tests.CheckSolution(const A: TQPMatrix;
  const B, X: TQPVector);
var I, J: Integer;
    S: TReal128;
begin
  CheckEquals(Length(A[0]), Length(X));
  CheckEquals(Length(A), Length(B));
  for I := 0 to High(A) do begin
    S := cZeroF128;
    for J := 0 to High(X) do
      S := S + A[I, J] * X[J];

    CheckClose(B[I], S);
  end;
end;

procedure TLUPDecompositionF128Tests.CheckInverse(const A, IA: TQPMatrix);
var I, J, K: Integer;
    S, Expected: TReal128;
begin
  CheckEquals(Length(A), Length(IA));
  for I := 0 to High(A) do
    for J := 0 to High(A) do begin
      S := cZeroF128;
      for K := 0 to High(A) do
        S := S + A[I, K] * IA[K, J];
      Expected := cZeroF128;
      if I = J then Expected := cOneF128;

      CheckClose(Expected, S);
    end;
end;

procedure TLUPDecompositionF128Tests.LUPReconstruction;
var A, LU: TQPMatrix;
    P: TArray<NativeInt>;
    I, J, K: Integer;
    L, U, S: TReal128;
begin
  A := TQPMatrix.Create(
    TQPVector.Create(Q(1), Q(2), Q(3)),
    TQPVector.Create(Q(-9), Q(4), Q(2)),
    TQPVector.Create(Q(2), Q(1), Q(5))
  );
  LU := TQPMatrix.Create(Copy(A[0]), Copy(A[1]), Copy(A[2]));

  CheckTrue(solver.InitializeWithoutCopy(LU) = lsisInitialized);
  P := Copy(TLUPAccess(solver).fP);

  CheckEquals(1, P[0], 'Largest absolute pivot');
  for I := 0 to 2 do
    for J := 0 to 2 do begin
      S := cZeroF128;
      for K := 0 to 2 do begin
        L := cZeroF128;
        if I = K then L := cOneF128;
        if I > K then L := LU[I, K];
        U := cZeroF128;
        if K <= J then U := LU[K, J];
        S := S + L * U;
      end;

      CheckClose(A[P[I], J], S);
    end;
end;

procedure TLUPDecompositionF128Tests.RepeatedCopiedSolves;
var A: TQPMatrix;
    B1, B2, X1, X2: TQPVector;
begin
  A := Matrix;
  B1 := TQPVector.Create(Q(1), Q(2));
  B2 := TQPVector.Create(Q(2), Q(1));

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

procedure TLUPDecompositionF128Tests.LUPVectorSolve;
var A, LU: TQPMatrix;
    B, X: TQPVector;
begin
  A := Matrix;
  LU := Matrix;
  B := TQPVector.Create(Q(1), Q(2));

  CheckTrue(solver.InitializeWithoutCopy(LU) = lsisInitialized);
  solver.Solve(B, X);

  CheckSolution(A, B, X);
  CheckClose(Q(-6), solver.Determinant);
end;

procedure TLUPDecompositionF128Tests.InverseDynamic;
var A, IA: TQPMatrix;
begin
  A := Matrix;

  CheckTrue(solver.Initialize(A) = lsisInitialized);
  solver.Invert(IA);

  CheckInverse(A, IA);
  CheckTrue(A[0, 0].IsExactZero);
  CheckTrue(A[1, 1] = Q(4));
  CheckTrue(solver.Initialize(A) = lsisInitialized);
  CheckClose(Q(-6), solver.Determinant);
end;

procedure TLUPDecompositionF128Tests.LUPInverseDynamic;
var LU, IA: TQPMatrix;
begin
  LU := Matrix;

  CheckTrue(solver.InitializeWithoutCopy(LU) = lsisInitialized);
  solver.Invert(IA);

  CheckInverse(Matrix, IA);
end;

procedure TLUPDecompositionF128Tests.InverseSmallScale;
var A, IA: TQPMatrix;
    S: TReal128;
begin
  S := Pow2(-2000);
  A := TQPMatrix.Create(
    TQPVector.Create(S, Q(0)),
    TQPVector.Create(Q(0), Q(2) * S)
  );

  CheckTrue(solver.Initialize(A) = lsisInitialized);
  solver.Invert(IA);

  CheckInverse(A, IA);
  CheckTrue(IA[0, 0] = Pow2(2000));
  CheckTrue(IA[1, 1] = Pow2(1999));
end;

procedure TLUPDecompositionF128Tests.DeterminantLargeScale;
var A: TQPMatrix;
    S, D: TReal128;
begin
  S := Pow2(2000);
  A := TQPMatrix.Create(
    TQPVector.Create(Q(0), S),
    TQPVector.Create(Q(2) * S, Q(0))
  );

  CheckTrue(solver.Initialize(A) = lsisInitialized);
  D := solver.Determinant;

  CheckTrue(D = -Pow2(4001));
end;

procedure TLUPDecompositionF128Tests.SingularMatrix;
var A: TQPMatrix;
begin
  A := TQPMatrix.Create(
    TQPVector.Create(Q(1), Q(2)),
    TQPVector.Create(Q(2), Q(4))
  );

  CheckEquals(Ord(lsisSingular), Ord(solver.Initialize(A)));
  CheckFalse(solver.Initialized);
end;

procedure TLUPDecompositionF128Tests.RejectNDLayouts;
var M, V: INDArray<TReal128>;
begin
  M := TNDAUt.AsArray<TReal128>(Matrix);
  V := TNDAMan.TransposedView<TReal128>(M);

  CheckFalse(solver.InitializeWithoutCopy(V) = lsisInitialized);

  V := TDynArrWrapper<TReal128>.Create(
    TQPVector.Create(Q(0), Q(2), Q(3), Q(4)), [2, 2]);
  V.UnsetFlags(NDAF_WRITEABLE);

  CheckFalse(solver.InitializeWithoutCopy(V) = lsisInitialized);
end;

procedure TLUPDecompositionF128Tests.LUPMultipleRHS;
var A, B, X: TQPMatrix;
    M, MB, MX: INDArray<TReal128>;
    J: Integer;
begin
  A := Matrix;
  B := TQPMatrix.Create(
    TQPVector.Create(Q(1), Q(2)),
    TQPVector.Create(Q(3), Q(4))
  );
  M := TNDAUt.AsArray<TReal128>(A);
  MB := TNDAUt.AsArray<TReal128>(B);

  CheckTrue(solver.InitializeWithoutCopy(M) = lsisInitialized);
  solver.Solve(MB, MX);

  CheckTrue(TNDAUt.TryAsDynArray2D<TReal128>(MX, X));
  for J := 0 to 1 do
    CheckSolution(A, TQPVector.Create(B[0, J], B[1, J]),
      TQPVector.Create(X[0, J], X[1, J]));
end;

procedure TLUPDecompositionF128Tests.LUPVectorND;
var X: TQPVector;
    M, B, MX: INDArray<TReal128>;
begin
  M := TNDAUt.AsArray<TReal128>(Matrix);
  B := TNDAUt.AsArray<TReal128>(TQPVector.Create(Q(1), Q(99), Q(2), Q(99)));

  CheckTrue(solver.InitializeWithoutCopy(M) = lsisInitialized);
  solver.Solve(B[[NDISpan(0, 3, 2)]], MX);

  CheckTrue(TNDAUt.TryAsDynArray<TReal128>(MX, X));
  CheckSolution(Matrix, TQPVector.Create(Q(1), Q(2)), X);
end;

procedure TLUPDecompositionF128Tests.LUPStridedND;
var A, IA: TQPMatrix;
    M, V, MI: INDArray<TReal128>;
begin
  M := TNDAUt.AsArray<TReal128>(TQPMatrix.Create(
    TQPVector.Create(Q(0), Q(2), Q(99)),
    TQPVector.Create(Q(3), Q(4), Q(99))
  ));
  V := M[[NDIAll, NDISpan(0, 1)]];

  CheckTrue(solver.InitializeWithoutCopy(V) = lsisInitialized);
  solver.Invert(MI);

  CheckTrue(TNDAUt.TryAsDynArray2D<TReal128>(MI, IA));
  CheckInverse(Matrix, IA);
  CheckClose(Q(-6), solver.Determinant);
  CheckTrue(TNDAUt.TryAsDynArray2D<TReal128>(M, A));
  CheckTrue(A[0, 2] = Q(99));
  CheckTrue(A[1, 2] = Q(99));
end;

procedure TLUPDecompositionF128Tests.InverseND;
var A, IA: TQPMatrix;
    M, V, MI: INDArray<TReal128>;
begin
  M := TNDAUt.AsArray<TReal128>(Matrix);
  V := TNDAMan.TransposedView<TReal128>(M);
  CheckTrue(TNDAUt.TryAsDynArray2D<TReal128>(V, A));

  CheckTrue(solver.Initialize(V) = lsisInitialized);
  solver.Invert(MI);

  CheckTrue(TNDAUt.TryAsDynArray2D<TReal128>(MI, IA));
  CheckInverse(A, IA);
  CheckTrue(solver.Initialize(V) = lsisInitialized);
  CheckClose(Q(-6), solver.Determinant);
end;

procedure TLUPDecompositionF128Tests.ToleranceBoundary;
var A: TQPMatrix;
    T: TReal128;
begin
  T := Pow2(-2000);
  solver.PivotTolerance := T;
  A := TQPMatrix.Create(TQPVector.Create(T));

  CheckFalse(solver.InitializeWithoutCopy(A) = lsisInitialized, 'Pivot equal to tolerance');

end;

function TLUPDecompositionF128Tests.BandMatrix: TQPMatrix;
begin
  Result := TQPMatrix.Create(
    TQPVector.Create(Q(0), Q(2), Q(0), Q(0)),
    TQPVector.Create(Q(3), Q(1), Q(4), Q(0)),
    TQPVector.Create(Q(0), Q(5), Q(2), Q(3)),
    TQPVector.Create(Q(0), Q(0), Q(6), Q(4))
  );
end;

procedure TLUPDecompositionF128Tests.BandLU;
var A, LU, IA: TQPMatrix;
    B, X: TQPVector;
    dense: TLUPDecompositionF128;
begin
  A := BandMatrix;
  LU := BandMatrix;
  B := TQPVector.Create(Q(4), Q(17), Q(28), Q(34));
  solver.LowerBandWidth := 1;
  solver.UpperBandWidth := 1;
  dense := TLUPDecompositionF128.Create;
  try
    CheckTrue(dense.Initialize(A) = lsisInitialized);

    CheckTrue(solver.InitializeWithoutCopy(LU) = lsisInitialized);
    solver.Solve(B, X);
    solver.Invert(IA);

    CheckSolution(A, B, X);
    CheckInverse(A, IA);
    CheckClose(Q(60), solver.Determinant);
    CheckClose(dense.Determinant, solver.Determinant);
  finally
    dense.Free;
  end;
end;

procedure TBlockedLUPF128Tests.SetUp;
begin
  inherited;
  solver.FactorizationMethod := lupBlocked;
  solver.BlockSize := 2;
end;

procedure TLUPDecompositionF128Tests.BlockedPanelSizes;
const rows: array[0..4] of Integer = (4, 2, 0, 3, 1);
var A, LU, IA: TQPMatrix;
    B, expectedX, X: TQPVector;
    P: TArray<NativeInt>;
    I, J, K, R, size: Integer;
    L, S, expectedDet: TReal128;
    ordinary: TLUPDecompositionF128;
begin
  SetLength(A, 5);
  SetLength(B, 5);
  SetLength(expectedX, 5);
  for I := 0 to 4 do begin
    SetLength(A[I], 5);
    expectedX[I] := Q(I + 1);
    R := rows[I];
    for J := 0 to 4 do begin
      A[I, J] := Q((R * 3 + J * 2) mod 7 - 3) / Q(10);
      if R = J then A[I, J] := A[I, J] + Q(10);
    end;
  end;
  for I := 0 to 4 do begin
    B[I] := cZeroF128;
    for J := 0 to 4 do B[I] := B[I] + A[I, J] * expectedX[J];
  end;
  ordinary := TLUPDecompositionF128.Create;
  try
    CheckTrue(ordinary.Initialize(A) = lsisInitialized);
    expectedDet := ordinary.Determinant;
  finally
    ordinary.Free;
  end;
  solver.FactorizationMethod := lupBlocked;
  for size in [1, 2, 3, 5, 8] do begin
    LU := TQPMatrix.Create(Copy(A[0]), Copy(A[1]), Copy(A[2]), Copy(A[3]), Copy(A[4]));
    solver.BlockSize := size;

    CheckTrue(solver.InitializeWithoutCopy(LU) = lsisInitialized);
    P := Copy(TLUPAccess(solver).fP);
    solver.Solve(B, X);
    solver.Invert(IA);

    CheckSolution(A, B, X);
    CheckInverse(A, IA);
    CheckClose(expectedDet, solver.Determinant);
    for I := 0 to 4 do
      for J := 0 to 4 do begin
        S := cZeroF128;
        for K := 0 to 4 do begin
          L := cZeroF128;
          if I = K then L := cOneF128;
          if I > K then L := LU[I, K];
          if K <= J then S := S + L * LU[K, J];
        end;

        CheckClose(A[P[I], J], S);
      end;
  end;
end;

initialization

  RegisterTest(TLUPDecompositionF128Tests.Suite);
  RegisterTest(TBlockedLUPF128Tests.Suite);

end.
