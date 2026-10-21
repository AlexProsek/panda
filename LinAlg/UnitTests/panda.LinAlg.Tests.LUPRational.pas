unit panda.LinAlg.Tests.LUPRational;

interface

uses
    panda.LinAlg.Base
  , TestFramework
  , panda.NumsAP
  , panda.Intfs
  , panda.Arrays
  , panda.ArrManip
  , panda.LinAlg.LUPRational
  , panda.LinAlg.LUP
  ;

type
  TRVector = TArray<TRational>;
  TRMatrix = TArray<TRVector>;

  TLUPDecompositionRationalTests = class(TTestCase)
  protected
    solver: TLUPDecompositionRational;
    function Matrix: TRMatrix;
    function BandMatrix: TRMatrix;
    function Product(const A: TRMatrix; const X: TRVector): TRVector;
    procedure CheckRational(const Expected, Actual: TRational);
    procedure CheckSolution(const A: TRMatrix; const B, X: TRVector);
    procedure CheckInverse(const A, IA: TRMatrix);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure FirstNonzeroPivot;
    procedure FirstNonzeroAfterZero;
    procedure TinyPivot;
    procedure LUPReconstruction;
    procedure BlockedPanelSizes;
    procedure ManagedFactorAndResultLifetime;
    procedure LUPVectorSolve;
    procedure RepeatedCopiedSolves;
    procedure InverseDynamic;
    procedure DeterminantSign;
    procedure LUPInverseDynamic;
    procedure SingularMatrix;
    procedure RejectNDLayouts;
    procedure LUPMultipleRHS;
    procedure LUPVectorND;
    procedure LUPSingleColumnND;
    procedure LUPStridedND;
    procedure LUPReversedRowsND;
    procedure InverseND;
    procedure LUPInverseND;
    procedure BandLU;
  end;

type
  TBlockedLUPRationalTests = class(TLUPDecompositionRationalTests)
  public
    procedure SetUp; override;
  end;

implementation

type
  TLUPAccess = class(TLUPDecompositionRational);

function Q(aNum: Int64; aDenom: Int64 = 1): TRational;
begin
  Result := TRational.Fraction(TInteger(aNum), TInteger(aDenom));
end;

procedure TLUPDecompositionRationalTests.SetUp;
begin
  solver := TLUPDecompositionRational.Create;
end;

procedure TLUPDecompositionRationalTests.TearDown;
begin
  solver.Free;
end;

function TLUPDecompositionRationalTests.Matrix: TRMatrix;
begin
  Result := TRMatrix.Create(TRVector.Create(Q(0), Q(2)),
    TRVector.Create(Q(3), Q(4)));
end;

function TLUPDecompositionRationalTests.Product(const A: TRMatrix;
  const X: TRVector): TRVector;
var I, J: Integer;
begin
  SetLength(Result, Length(A));
  for I := 0 to High(A) do begin
    Result[I] := Q(0);
    for J := 0 to High(X) do
      Result[I] := Result[I] + A[I, J] * X[J];
  end;
end;

procedure TLUPDecompositionRationalTests.CheckRational(const Expected, Actual: TRational);
begin
  CheckTrue(Actual.Numerator.Count > 0, 'Numerator is initialized');
  CheckTrue(Actual.Denominator.Count > 0, 'Denominator is initialized');
  CheckFalse(Actual.Denominator.IsZero, 'Denominator is nonzero');
  CheckTrue((Expected - Actual).IsZero,
    'Expected ' + Expected.ToString + ', actual ' + Actual.ToString);
end;

procedure TLUPDecompositionRationalTests.CheckSolution(const A: TRMatrix;
  const B, X: TRVector);
var Y: TRVector;
    I: Integer;
begin
  CheckEquals(Length(A[0]), Length(X));
  Y := Product(A, X);
  CheckEquals(Length(B), Length(Y));
  for I := 0 to High(B) do
    CheckRational(B[I], Y[I]);
end;

procedure TLUPDecompositionRationalTests.CheckInverse(const A, IA: TRMatrix);
var I, J, K: Integer;
    S, Expected: TRational;
begin
  CheckEquals(Length(A), Length(IA));
  for I := 0 to High(A) do
    for J := 0 to High(A) do begin
      S := Q(0);
      for K := 0 to High(A) do
        S := S + A[I, K] * IA[K, J];
      Expected := Q(0);
      if I = J then Expected := Q(1);
      CheckRational(Expected, S);
    end;
end;

procedure TLUPDecompositionRationalTests.FirstNonzeroPivot;
var A: TRMatrix;
    P: TArray<NativeInt>;
begin
  A := TRMatrix.Create(
    TRVector.Create(Q(1, 100), Q(1)),
    TRVector.Create(Q(100), Q(2))
  );

  CheckTrue(solver.InitializeWithoutCopy(A) = lsisInitialized);
  P := Copy(TLUPAccess(solver).fP);

  CheckEquals(0, Integer(P[0]));
  CheckRational(Q(1, 100), A[0, 0]);
end;

procedure TLUPDecompositionRationalTests.FirstNonzeroAfterZero;
var A: TRMatrix;
    P: TArray<NativeInt>;
begin
  A := TRMatrix.Create(
    TRVector.Create(Q(0), Q(1), Q(0)),
    TRVector.Create(Q(1, 100), Q(0), Q(1)),
    TRVector.Create(Q(100), Q(0), Q(0))
  );

  CheckTrue(solver.InitializeWithoutCopy(A) = lsisInitialized);
  P := Copy(TLUPAccess(solver).fP);

  CheckEquals(1, Integer(P[0]));
  CheckEquals(0, Integer(P[1]));
end;

procedure TLUPDecompositionRationalTests.TinyPivot;
var A: TRMatrix;
    T: TRational;
begin
  T := TRational.Fraction(TInteger(1), TInteger(1) shl 1100);
  A := TRMatrix.Create(
    TRVector.Create(T, Q(0)),
    TRVector.Create(Q(0), Q(1))
  );

  CheckTrue(solver.InitializeWithoutCopy(A) = lsisInitialized);
  CheckRational(T, solver.Determinant);
end;

procedure TLUPDecompositionRationalTests.LUPReconstruction;
var A, LU: TRMatrix;
    P: TArray<NativeInt>;
    I, J, K: Integer;
    L, U, S: TRational;
begin
  A := TRMatrix.Create(
    TRVector.Create(Q(0), Q(2), Q(1)),
    TRVector.Create(Q(0), Q(0), Q(3)),
    TRVector.Create(Q(4), Q(5), Q(6))
  );
  LU := TRMatrix.Create(Copy(A[0]), Copy(A[1]), Copy(A[2]));

  CheckTrue(solver.InitializeWithoutCopy(LU) = lsisInitialized);
  P := Copy(TLUPAccess(solver).fP);

  CheckEquals(2, Integer(P[0]));
  CheckEquals(0, Integer(P[1]));
  CheckEquals(1, Integer(P[2]));
  for I := 0 to High(A) do
    for J := 0 to High(A) do begin
      S := Q(0);
      for K := 0 to High(A) do begin
        L := Q(0);
        U := Q(0);
        if I = K then L := Q(1);
        if I > K then L := LU[I, K];
        if K <= J then U := LU[K, J];
        S := S + L * U;
      end;
      CheckRational(A[P[I], J], S);
    end;
end;

procedure TLUPDecompositionRationalTests.RepeatedCopiedSolves;
var A: TRMatrix;
    B1, B2, X1, X2: TRVector;
begin
  A := Matrix;
  B1 := TRVector.Create(Q(1, 3), Q(-2, 7));
  B2 := TRVector.Create(Q(-2, 7), Q(1, 3));

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

procedure TLUPDecompositionRationalTests.LUPVectorSolve;
var A, LU: TRMatrix;
    B, X: TRVector;
begin
  A := Matrix;
  LU := Matrix;
  B := TRVector.Create(Q(1, 5), Q(2, 7));

  CheckTrue(solver.InitializeWithoutCopy(LU) = lsisInitialized);
  solver.Solve(B, X);

  CheckSolution(A, B, X);
end;

procedure TLUPDecompositionRationalTests.InverseDynamic;
var A, IA: TRMatrix;
begin
  A := Matrix;

  CheckTrue(solver.Initialize(A) = lsisInitialized);
  solver.Invert(IA);

  CheckInverse(A, IA);
  CheckRational(Q(-2, 3), IA[0, 0]);
  CheckRational(Q(1, 3), IA[0, 1]);
  CheckRational(Q(1, 2), IA[1, 0]);
  CheckRational(Q(0), IA[1, 1]);
  CheckRational(Q(0), A[0, 0]);
  CheckRational(Q(4), A[1, 1]);
end;

procedure TLUPDecompositionRationalTests.DeterminantSign;
var A: TRMatrix;
begin
  A := Matrix;

  CheckTrue(solver.Initialize(A) = lsisInitialized);
  CheckRational(Q(-6), solver.Determinant);
  CheckRational(Q(0), A[0, 0]);

  A := TRMatrix.Create(
    TRVector.Create(Q(0), Q(2), Q(1)),
    TRVector.Create(Q(0), Q(0), Q(3)),
    TRVector.Create(Q(4), Q(5), Q(6))
  );

  CheckTrue(solver.Initialize(A) = lsisInitialized);
  CheckRational(Q(24), solver.Determinant);
end;

procedure TLUPDecompositionRationalTests.LUPInverseDynamic;
var A, LU, IA: TRMatrix;
begin
  A := Matrix;
  LU := Matrix;

  CheckTrue(solver.InitializeWithoutCopy(LU) = lsisInitialized);
  solver.Invert(IA);

  CheckInverse(A, IA);
  CheckRational(Q(-6), solver.Determinant);
end;

procedure TLUPDecompositionRationalTests.SingularMatrix;
var A: TRMatrix;
begin
  A := TRMatrix.Create(TRVector.Create(Q(1, 3), Q(2, 7)),
    TRVector.Create(Q(2, 3), Q(4, 7)));

  CheckEquals(Ord(lsisSingular), Ord(solver.Initialize(A)));
  CheckFalse(solver.Initialized);
end;

procedure TLUPDecompositionRationalTests.RejectNDLayouts;
var M, V: INDArray<TRational>;
begin
  M := TNDAUt.AsArray<TRational>(Matrix);
  V := TNDAMan.TransposedView<TRational>(M);

  CheckFalse(solver.InitializeWithoutCopy(V) = lsisInitialized, 'Reject column-strided LU storage');

  V := TDynArrWrapper<TRational>.Create(
    TRVector.Create(Q(0), Q(2), Q(3), Q(4)), [2, 2]);
  V.UnsetFlags(NDAF_WRITEABLE);

  CheckFalse(solver.InitializeWithoutCopy(V) = lsisInitialized, 'Reject read-only storage');
end;

procedure TLUPDecompositionRationalTests.LUPMultipleRHS;
var A, B, X: TRMatrix;
    M, MB, MX: INDArray<TRational>;
    J: Integer;
begin
  A := Matrix;
  B := TRMatrix.Create(
    TRVector.Create(Q(1, 3), Q(2, 5)),
    TRVector.Create(Q(3, 7), Q(4, 9))
  );
  M := TNDAUt.AsArray<TRational>(A);
  MB := TNDAUt.AsArray<TRational>(B);

  CheckTrue(solver.InitializeWithoutCopy(M) = lsisInitialized);
  solver.Solve(MB, MX);

  CheckTrue(TNDAUt.TryAsDynArray2D<TRational>(MX, X));
  for J := 0 to 1 do
    CheckSolution(A, TRVector.Create(B[0, J], B[1, J]),
      TRVector.Create(X[0, J], X[1, J]));
end;

procedure TLUPDecompositionRationalTests.LUPVectorND;
var X: TRVector;
    M, B, MX: INDArray<TRational>;
begin
  M := TNDAUt.AsArray<TRational>(Matrix);
  B := TNDAUt.AsArray<TRational>(TRVector.Create(Q(1), Q(99), Q(2), Q(99)));

  CheckTrue(solver.InitializeWithoutCopy(M) = lsisInitialized);
  solver.Solve(B[[NDISpan(0, 3, 2)]], MX);
  M := nil;
  B := nil;
  solver.Free;
  solver := nil;

  CheckTrue(TNDAUt.TryAsDynArray<TRational>(MX, X));
  CheckSolution(Matrix, TRVector.Create(Q(1), Q(2)), X);
end;

procedure TLUPDecompositionRationalTests.LUPSingleColumnND;
var X: TRMatrix;
    M, B, MX: INDArray<TRational>;
begin
  M := TNDAUt.AsArray<TRational>(Matrix);
  B := TNDAUt.AsArray<TRational>(TRMatrix.Create(
    TRVector.Create(Q(1)),
    TRVector.Create(Q(2)))
  );

  CheckTrue(solver.InitializeWithoutCopy(M) = lsisInitialized);
  solver.Solve(B, MX);

  CheckEquals(2, MX.NDim);
  CheckTrue(TNDAUt.TryAsDynArray2D<TRational>(MX, X));
  CheckSolution(Matrix, TRVector.Create(Q(1), Q(2)),
    TRVector.Create(X[0, 0], X[1, 0]));
end;

procedure TLUPDecompositionRationalTests.LUPStridedND;
var A, Wide, X: TRMatrix;
    M, V, MX: INDArray<TRational>;
begin
  A := TRMatrix.Create(
    TRVector.Create(Q(0), Q(2), Q(1)),
    TRVector.Create(Q(0), Q(0), Q(3)),
    TRVector.Create(Q(4), Q(5), Q(6))
  );
  Wide := TRMatrix.Create(
    TRVector.Create(Q(0), Q(2), Q(1), Q(99)),
    TRVector.Create(Q(0), Q(0), Q(3), Q(98)),
    TRVector.Create(Q(4), Q(5), Q(6), Q(97))
  );
  M := TNDAUt.AsArray<TRational>(Wide);
  V := M[[NDIAll, NDISpan(0, 2)]];

  CheckTrue(solver.InitializeWithoutCopy(V) = lsisInitialized);
  solver.Solve(TNDAUt.AsArray<TRational>(TRMatrix.Create(
    TRVector.Create(Q(1), Q(2)),
    TRVector.Create(Q(2), Q(4)),
    TRVector.Create(Q(3), Q(6)))), MX);

  CheckTrue(TNDAUt.TryAsDynArray2D<TRational>(MX, X));
  CheckSolution(A, TRVector.Create(Q(1), Q(2), Q(3)),
    TRVector.Create(X[0, 0], X[1, 0], X[2, 0]));
  CheckTrue(TNDAUt.TryAsDynArray2D<TRational>(M, Wide));
  CheckRational(Q(99), Wide[0, 3]);
  CheckRational(Q(98), Wide[1, 3]);
  CheckRational(Q(97), Wide[2, 3]);
  CheckRational(Q(24), solver.Determinant);
end;

procedure TLUPDecompositionRationalTests.LUPReversedRowsND;
var A: TRMatrix;
    X: TRVector;
    M, V, MX: INDArray<TRational>;
begin
  M := TNDAUt.AsArray<TRational>(TRMatrix.Create(
    TRVector.Create(Q(3), Q(4)),
    TRVector.Create(Q(0), Q(2))
  ));
  V := M[[NDISpan(1, 0, -1), NDIAll]];
  CheckTrue(TNDAUt.TryAsDynArray2D<TRational>(V, A));

  CheckTrue(solver.InitializeWithoutCopy(V) = lsisInitialized);
  solver.Solve(TNDAUt.AsArray<TRational>(TRVector.Create(Q(1), Q(2))), MX);

  CheckTrue(TNDAUt.TryAsDynArray<TRational>(MX, X));
  CheckSolution(A, TRVector.Create(Q(1), Q(2)), X);
end;

procedure TLUPDecompositionRationalTests.InverseND;
var A, IA: TRMatrix;
    M, V, MI: INDArray<TRational>;
begin
  M := TNDAUt.AsArray<TRational>(Matrix);
  V := TNDAMan.TransposedView<TRational>(M);
  CheckTrue(TNDAUt.TryAsDynArray2D<TRational>(V, A));

  CheckTrue(solver.Initialize(V) = lsisInitialized);
  solver.Invert(MI);

  CheckTrue(TNDAUt.TryAsDynArray2D<TRational>(MI, IA));
  CheckInverse(A, IA);
  CheckTrue(solver.Initialize(V) = lsisInitialized);
  CheckRational(Q(-6), solver.Determinant);
  CheckTrue(TNDAUt.TryAsDynArray2D<TRational>(M, A));
  CheckRational(Q(0), A[0, 0]);
  CheckRational(Q(2), A[0, 1]);
end;

procedure TLUPDecompositionRationalTests.LUPInverseND;
var IA: TRMatrix;
    M, MI: INDArray<TRational>;
begin
  M := TNDAUt.AsArray<TRational>(Matrix);

  CheckTrue(solver.InitializeWithoutCopy(M) = lsisInitialized);
  solver.Invert(MI);
  M := nil;
  solver.Free;
  solver := nil;

  CheckTrue(TNDAUt.TryAsDynArray2D<TRational>(MI, IA));
  CheckInverse(Matrix, IA);
end;

function TLUPDecompositionRationalTests.BandMatrix: TRMatrix;
begin
  Result := TRMatrix.Create(
    TRVector.Create(Q(0), Q(2), Q(0), Q(0)),
    TRVector.Create(Q(3), Q(1), Q(4), Q(0)),
    TRVector.Create(Q(0), Q(5), Q(2), Q(3)),
    TRVector.Create(Q(0), Q(0), Q(6), Q(4))
  );
end;

procedure TLUPDecompositionRationalTests.BandLU;
var A, LU, IA: TRMatrix;
    B, X: TRVector;
    P: TArray<NativeInt>;
    dense: TLUPDecompositionRational;
begin
  A := BandMatrix;
  LU := BandMatrix;
  B := Product(A, TRVector.Create(Q(1, 2), Q(-2, 3), Q(3, 4), Q(-4, 5)));
  solver.LowerBandWidth := 1;
  solver.UpperBandWidth := 1;
  dense := TLUPDecompositionRational.Create;
  try
    CheckTrue(dense.Initialize(A) = lsisInitialized);

    CheckTrue(solver.InitializeWithoutCopy(LU) = lsisInitialized);
    P := Copy(TLUPAccess(solver).fP);
    solver.Solve(B, X);
    solver.Invert(IA);

    CheckSolution(A, B, X);
    CheckInverse(A, IA);
    CheckRational(Q(60), solver.Determinant);
    CheckRational(dense.Determinant, solver.Determinant);
    CheckEquals(1, Integer(P[0]));
    CheckEquals(0, Integer(P[1]));
  finally
    dense.Free;
  end;
end;

procedure TBlockedLUPRationalTests.SetUp;
begin
  inherited;
  solver.FactorizationMethod := lupBlocked;
  solver.BlockSize := 2;
end;

procedure TLUPDecompositionRationalTests.ManagedFactorAndResultLifetime;
var producer: TLUPDecompositionRational;
    LU, IA: TRMatrix;
    B, X, again: TRVector;
begin
  LU := Matrix;
  B := Product(Matrix, TRVector.Create(Q(1, 3), Q(-2, 7)));
  producer := TLUPDecompositionRational.Create;
  try
    producer.FactorizationMethod := solver.FactorizationMethod;
    producer.BlockSize := 1;

    CheckTrue(producer.InitializeWithoutCopy(LU) = lsisInitialized);
    producer.Solve(B, X);
    producer.Invert(IA);
  finally
    producer.Free;
  end;

  CheckRational(Q(1, 3), X[0]);
  CheckRational(Q(-2, 7), X[1]);
  CheckInverse(Matrix, IA);

  CheckTrue(solver.Initialize(Matrix) = lsisInitialized);
  solver.Solve(B, again);
  LU := nil;
  B := nil;

  CheckRational(Q(1, 3), again[0]);
  CheckRational(Q(-2, 7), again[1]);
  CheckRational(Q(1, 3), X[0]);
  CheckInverse(Matrix, IA);
end;

procedure TLUPDecompositionRationalTests.BlockedPanelSizes;
const rows: array[0..4] of Integer = (4, 2, 0, 3, 1);
var A, LU, IA: TRMatrix;
    B, expectedX, X: TRVector;
    P: TArray<NativeInt>;
    I, J, K, R, size: Integer;
    L, S, expectedDet: TRational;
    ordinary: TLUPDecompositionRational;
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
    B[I] := Q(0);
    for J := 0 to 4 do B[I] := B[I] + A[I, J] * expectedX[J];
  end;
  ordinary := TLUPDecompositionRational.Create;
  try
    CheckTrue(ordinary.Initialize(A) = lsisInitialized);
    expectedDet := ordinary.Determinant;
  finally
    ordinary.Free;
  end;
  solver.FactorizationMethod := lupBlocked;
  for size in [1, 2, 3, 5, 8] do begin
    LU := TRMatrix.Create(Copy(A[0]), Copy(A[1]), Copy(A[2]), Copy(A[3]), Copy(A[4]));
    solver.BlockSize := size;

    CheckTrue(solver.InitializeWithoutCopy(LU) = lsisInitialized);
    P := Copy(TLUPAccess(solver).fP);
    solver.Solve(B, X);
    solver.Invert(IA);

    CheckSolution(A, B, X);
    CheckInverse(A, IA);
    CheckRational(expectedDet, solver.Determinant);
    for I := 0 to 4 do
      for J := 0 to 4 do begin
        S := Q(0);
        for K := 0 to 4 do begin
          L := Q(0);
          if I = K then L := Q(1);
          if I > K then L := LU[I, K];
          if K <= J then S := S + L * LU[K, J];
        end;

        CheckRational(A[P[I], J], S);
      end;
  end;
end;

initialization

  RegisterTest(TLUPDecompositionRationalTests.Suite);
  RegisterTest(TBlockedLUPRationalTests.Suite);

end.
