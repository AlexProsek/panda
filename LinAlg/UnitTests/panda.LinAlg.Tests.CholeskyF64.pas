unit panda.LinAlg.Tests.CholeskyF64;

interface

uses
    panda.LinAlg.Base
  , TestFramework
  , panda.Intfs
  , panda.LinAlg.Cholesky
  , panda.LinAlg.CholeskyF64
  ;

type
  TCholeskyF64Tests = class(TTestCase)
  protected
    fSolver: TCholeskyDecompositionF64;
    function Matrix: TArray<TArray<Double>>;
    procedure CheckSolution(const aX: TArray<Double>);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Defaults;
    procedure ScalarSystem;
    procedure DiagonalSystem;
    procedure FactorReconstruction;
    procedure NormalEquations;
    procedure RepeatedSolves;
    procedure MultipleRHS;
    procedure UpperTriangleIgnored;
    procedure ZeroRejected;
    procedure SemidefiniteRejected;
    procedure IndefiniteRejected;
    procedure ExplicitToleranceBoundary;
    procedure AutomaticTolerance;
    procedure InvalidDimensions;
    procedure UninitializedSolves;
    procedure InvalidRHSPreservesFactors;
    procedure FailedReplacement;
    procedure ExceptionalReplacement;
    procedure CopyIndependent;
    procedure BorrowedDynamicLifetime;
    procedure CopiedStridedReadOnly;
    procedure BorrowedOffset;
    procedure BorrowedReversedRows;
    procedure BorrowedInvalidLayouts;
    procedure StridedRHS;
    procedure ResultsOutliveSolver;
  end;

implementation

uses
    System.Classes
  , System.SysUtils
  , panda.Arrays
  , panda.Nums
  , panda.DynArrayUtils
  ;

type
  TCholeskyAccess = class(TCholeskyDecompositionF64);

  TFailingCholesky = class(TCholeskyDecompositionF64)
  public
    RaisePreparation: Boolean;
  protected
    procedure PrepareFactorization; override;
  end;

procedure TFailingCholesky.PrepareFactorization;
begin
  if RaisePreparation then raise EInvalidOperation.Create('Test preparation failure');
  inherited;
end;

procedure TCholeskyF64Tests.SetUp;
begin
  inherited;
  fSolver := TCholeskyDecompositionF64.Create;
end;

procedure TCholeskyF64Tests.TearDown;
begin
  fSolver.Free;
  inherited;
end;

function TCholeskyF64Tests.Matrix: TArray<TArray<Double>>;
begin
  Result := TArray<TArray<Double>>.Create(
    TArray<Double>.Create(4, 2), TArray<Double>.Create(2, 5));
end;

procedure TCholeskyF64Tests.CheckSolution(const aX: TArray<Double>);
begin
  CheckEquals(2, Length(aX));
  CheckEquals(1, aX[0], 1e-12);
  CheckEquals(2, aX[1], 1e-12);
  CheckEquals(8, 4 * aX[0] + 2 * aX[1], 1e-12);
  CheckEquals(12, 2 * aX[0] + 5 * aX[1], 1e-12);
end;

procedure TCholeskyF64Tests.Defaults;
begin
  CheckFalse(fSolver.Initialized);
  CheckEquals(0, fSolver.PivotTolerance, 0);
  CheckEquals(100, fSolver.ToleranceFactor, 0);
end;

procedure TCholeskyF64Tests.ScalarSystem;
var X: TArray<Double>;
begin
  CheckTrue(fSolver.Initialize(TArray<TArray<Double>>.Create(
    TArray<Double>.Create(4))) = lsisInitialized);

  CheckTrue(fSolver.Solve(TArray<Double>.Create(12), X) = lsssSolved);

  CheckEquals(1, Length(X));
  CheckEquals(3, X[0], 1e-12);
end;

procedure TCholeskyF64Tests.DiagonalSystem;
var X: TArray<Double>;
begin
  CheckTrue(fSolver.Initialize(TArray<TArray<Double>>.Create(
    TArray<Double>.Create(4, 0, 0),
    TArray<Double>.Create(0, 9, 0),
    TArray<Double>.Create(0, 0, 16)
  )) = lsisInitialized);

  CheckTrue(fSolver.Solve(TArray<Double>.Create(4, 18, 48), X) = lsssSolved);

  CheckEquals(1, X[0], 1e-12);
  CheckEquals(2, X[1], 1e-12);
  CheckEquals(3, X[2], 1e-12);
end;

procedure TCholeskyF64Tests.FactorReconstruction;
var A, original: TArray<TArray<Double>>;
    I, J, K: Integer;
    value: Double;
begin
  A := TArray<TArray<Double>>.Create(
    TArray<Double>.Create(4, 2, 4),
    TArray<Double>.Create(2, 10, -1),
    TArray<Double>.Create(4, -1, 21)
  );
  original := TDynAUt.Clone<Double>(A);

  CheckTrue(fSolver.InitializeWithoutCopy(A) = lsisInitialized);

  for I := 0 to 2 do
    for J := 0 to I do begin
      value := 0;
      for K := 0 to J do value := value + A[I, K] * A[J, K];
      CheckEquals(original[I, J], value, 1e-12);
    end;
  CheckEquals(2, A[0, 1], 0);
  CheckEquals(4, A[0, 2], 0);
  CheckEquals(-1, A[1, 2], 0);
end;

procedure TCholeskyF64Tests.NormalEquations;
var A, G: TArray<TArray<Double>>;
    B, C, X: TArray<Double>;
    I, J, K: Integer;
begin
  A := TArray<TArray<Double>>.Create(TArray<Double>.Create(1, 0),
    TArray<Double>.Create(1, 1), TArray<Double>.Create(1, 2));
  B := TArray<Double>.Create(1, 2, 2);
  SetLength(G, 2, 2);
  SetLength(C, 2);
  for I := 0 to 1 do begin
    for K := 0 to 2 do C[I] := C[I] + A[K, I] * B[K];
    for J := 0 to I do
      for K := 0 to 2 do G[I, J] := G[I, J] + A[K, I] * A[K, J];
  end;

  CheckTrue(fSolver.Initialize(G) = lsisInitialized);
  CheckTrue(fSolver.Solve(C, X) = lsssSolved);

  CheckEquals(7 / 6, X[0], 1e-12);
  CheckEquals(0.5, X[1], 1e-12);
  CheckEquals(0, (X[0] - B[0]) + (X[0] + X[1] - B[1]) +
    (X[0] + 2 * X[1] - B[2]), 1e-12);
end;

procedure TCholeskyF64Tests.RepeatedSolves;
var A, factors: TArray<TArray<Double>>;
    B, X, first: TArray<Double>;
    I: Integer;
begin
  A := Matrix;
  B := TArray<Double>.Create(8, 12);
  CheckTrue(fSolver.InitializeWithoutCopy(A) = lsisInitialized);
  factors := TDynAUt.Clone<Double>(A);

  CheckTrue(fSolver.Solve(B, first) = lsssSolved);
  fSolver.PivotTolerance := 1e20;
  fSolver.LowerBandWidth := 0;
  fSolver.UpperBandWidth := 0;
  CheckTrue(fSolver.Solve(B, X) = lsssSolved);

  CheckSolution(first);
  CheckSolution(X);
  X[0] := 99;
  CheckEquals(1, first[0], 1e-12);
  CheckEquals(8, B[0], 0);
  CheckEquals(12, B[1], 0);
  for I := 0 to 1 do
    CheckTrue(CompareMem(@A[I, 0], @factors[I, 0], 2 * SizeOf(Double)));
end;

procedure TCholeskyF64Tests.MultipleRHS;
var A, B, X, again: INDArray<Double>;
begin
  A := TNDAUt.AsArray<Double>(Matrix);
  B := TNDAUt.AsArray<Double>([[8, 4], [12, 2]]);
  CheckTrue(fSolver.Initialize(A) = lsisInitialized);

  CheckTrue(fSolver.Solve(B, X) = lsssSolved);
  CheckTrue(fSolver.Solve(B, again) = lsssSolved);

  CheckEquals(2, X.NDim);
  CheckEquals(2, X.Shape[0]);
  CheckEquals(2, X.Shape[1]);
  CheckEquals(1, X.GetItem([0, 0]), 1e-12);
  CheckEquals(2, X.GetItem([1, 0]), 1e-12);
  CheckEquals(1, X.GetItem([0, 1]), 1e-12);
  CheckEquals(0, X.GetItem([1, 1]), 1e-12);
  CheckEquals(2, again.GetItem([1, 0]), 1e-12);
  CheckEquals(12, B.GetItem([1, 0]), 0);
  CheckEquals(4, A.GetItem([0, 0]), 0);
end;

procedure TCholeskyF64Tests.UpperTriangleIgnored;
var A: TArray<TArray<Double>>;
    X: TArray<Double>;
    upper: Double;
begin
  A := Matrix;
  A[0, 1] := 1e200;
  upper := A[0, 1];

  CheckTrue(fSolver.InitializeWithoutCopy(A) = lsisInitialized);
  CheckTrue(fSolver.Solve(TArray<Double>.Create(8, 12), X) = lsssSolved);

  CheckSolution(X);
  CheckTrue(CompareMem(@upper, @A[0, 1], SizeOf(Double)));

  A := Matrix;
  A[0, 1] := -1e200;

  CheckTrue(fSolver.Initialize(A) = lsisInitialized);
  CheckTrue(fSolver.Solve(TArray<Double>.Create(8, 12), X) = lsssSolved);

  CheckSolution(X);
end;

procedure TCholeskyF64Tests.ZeroRejected;
begin
  CheckTrue(fSolver.Initialize(TArray<TArray<Double>>.Create(
    TArray<Double>.Create(0))) = lsisNotPositiveDefinite);

  CheckFalse(fSolver.Initialized);
end;

procedure TCholeskyF64Tests.SemidefiniteRejected;
begin
  CheckTrue(fSolver.Initialize(TArray<TArray<Double>>.Create(
    TArray<Double>.Create(1, 1), TArray<Double>.Create(1, 1))) = lsisNotPositiveDefinite);

  CheckFalse(fSolver.Initialized);
end;

procedure TCholeskyF64Tests.IndefiniteRejected;
begin
  CheckTrue(fSolver.Initialize(TArray<TArray<Double>>.Create(
    TArray<Double>.Create(1, 2), TArray<Double>.Create(2, 1))) = lsisNotPositiveDefinite);

  CheckFalse(fSolver.Initialized);
end;

procedure TCholeskyF64Tests.ExplicitToleranceBoundary;
var A: TArray<TArray<Double>>;
begin
  A := TArray<TArray<Double>>.Create(TArray<Double>.Create(4, 0),
    TArray<Double>.Create(2, 2));
  fSolver.PivotTolerance := 1;

  CheckTrue(fSolver.Initialize(A) = lsisNotPositiveDefinite);

  fSolver.PivotTolerance := 0.5;

  CheckTrue(fSolver.Initialize(A) = lsisInitialized);
end;

procedure TCholeskyF64Tests.AutomaticTolerance;
var A: TArray<TArray<Double>>;
begin
  A := TArray<TArray<Double>>.Create(TArray<Double>.Create(1, 0),
    TArray<Double>.Create(0, 100 * cEpsF64));

  CheckTrue(fSolver.Initialize(A) = lsisNotPositiveDefinite);

  fSolver.ToleranceFactor := 1;

  CheckTrue(fSolver.Initialize(A) = lsisInitialized);

  fSolver.ToleranceFactor := 0;
  fSolver.PivotTolerance := -1;

  CheckTrue(fSolver.Initialize(A) = lsisNotPositiveDefinite);
end;

procedure TCholeskyF64Tests.InvalidDimensions;
begin
  CheckTrue(fSolver.Initialize(TArray<TArray<Double>>(nil)) = lsisInvalidInput);

  CheckTrue(fSolver.Initialize(TArray<TArray<Double>>.Create(
    TArray<Double>.Create(1, 2))) = lsisInvalidInput);

  CheckTrue(fSolver.Initialize(TArray<TArray<Double>>.Create(
    TArray<Double>.Create(1, 2), TArray<Double>.Create(3))) = lsisInvalidInput);

  CheckTrue(fSolver.Initialize(INDArray<Double>(nil)) = lsisInvalidInput);

  CheckTrue(fSolver.Initialize(TNDAUt.Empty<Double>([2])) = lsisInvalidInput);

  CheckTrue(fSolver.InitializeWithoutCopy(TNDAUt.Empty<Double>([2, 3])) = lsisInvalidInput);
end;

procedure TCholeskyF64Tests.UninitializedSolves;
var X: TArray<Double>;
    Y: INDArray<Double>;
begin
  X := TArray<Double>.Create(99);
  Y := TNDAUt.Empty<Double>([1]);

  CheckTrue(fSolver.Solve(TArray<Double>(nil), X) = lsssNotInitialized);
  CheckTrue(fSolver.Solve(INDArray<Double>(nil), Y) = lsssNotInitialized);

  CheckEquals(0, Length(X));
  CheckTrue(Y = nil);
end;

procedure TCholeskyF64Tests.InvalidRHSPreservesFactors;
var X: TArray<Double>;
    Y: INDArray<Double>;
begin
  CheckTrue(fSolver.Initialize(Matrix) = lsisInitialized);
  X := TArray<Double>.Create(99);

  CheckTrue(fSolver.Solve(TArray<Double>.Create(1), X) = lsssInvalidInput);

  CheckEquals(0, Length(X));
  CheckTrue(fSolver.Initialized);

  Y := TNDAUt.Empty<Double>([1]);

  CheckTrue(fSolver.Solve(TNDAUt.Empty<Double>([3, 2]), Y) = lsssInvalidInput);

  CheckTrue(Y = nil);
  CheckTrue(fSolver.Initialized);

  CheckTrue(fSolver.Solve(TNDAUt.Empty<Double>([1]), Y) = lsssInvalidInput);

  CheckTrue(Y = nil);

  CheckTrue(fSolver.Solve(TNDAUt.Empty<Double>([2, 1, 1]), Y) = lsssInvalidInput);

  CheckTrue(Y = nil);

  CheckTrue(fSolver.Solve(INDArray<Double>(nil), Y) = lsssInvalidInput);

  CheckTrue(Y = nil);
  CheckTrue(fSolver.Solve(TArray<Double>.Create(8, 12), X) = lsssSolved);
  CheckSolution(X);
end;

procedure TCholeskyF64Tests.FailedReplacement;
begin
  CheckTrue(fSolver.Initialize(Matrix) = lsisInitialized);

  CheckTrue(fSolver.Initialize(TArray<TArray<Double>>(nil)) = lsisInvalidInput);

  CheckFalse(fSolver.Initialized);
  CheckEquals(0, Length(TCholeskyAccess(fSolver).fRows));

  CheckTrue(fSolver.Initialize(Matrix) = lsisInitialized);

  CheckTrue(fSolver.Initialize(TArray<TArray<Double>>.Create(
    TArray<Double>.Create(-1))) = lsisNotPositiveDefinite);

  CheckFalse(fSolver.Initialized);
  CheckEquals(0, Length(TCholeskyAccess(fSolver).fRows));
end;

procedure TCholeskyF64Tests.ExceptionalReplacement;
var solver: TFailingCholesky;
    raised: Boolean;
begin
  solver := TFailingCholesky.Create;
  try
    CheckTrue(solver.Initialize(Matrix) = lsisInitialized);
    solver.RaisePreparation := True;
    raised := False;

    try
      solver.Initialize(Matrix);
    except
      on E: EInvalidOperation do raised := True;
    end;

    CheckTrue(raised);
    CheckFalse(solver.Initialized);
    CheckEquals(0, Length(TCholeskyAccess(solver).fRows));
    CheckEquals(0, Length(TCholeskyAccess(solver).fDyn));
    CheckTrue(TCholeskyAccess(solver).fND = nil);
  finally
    solver.Free;
  end;
end;

procedure TCholeskyF64Tests.CopyIndependent;
var A: TArray<TArray<Double>>;
    X: TArray<Double>;
begin
  A := Matrix;

  CheckTrue(fSolver.Initialize(A) = lsisInitialized);

  CheckEquals(4, A[0, 0], 0);
  CheckEquals(2, A[1, 0], 0);
  CheckEquals(5, A[1, 1], 0);
  A[0, 0] := 99;
  A := nil;

  CheckTrue(fSolver.Solve(TArray<Double>.Create(8, 12), X) = lsssSolved);

  CheckSolution(X);
end;

procedure TCholeskyF64Tests.BorrowedDynamicLifetime;
var A: TArray<TArray<Double>>;
    X: TArray<Double>;
begin
  A := Matrix;

  CheckTrue(fSolver.InitializeWithoutCopy(A) = lsisInitialized);

  CheckEquals(2, A[0, 0], 0);
  CheckEquals(1, A[1, 0], 0);
  CheckEquals(2, A[1, 1], 0);
  A := nil;

  CheckTrue(fSolver.Solve(TArray<Double>.Create(8, 12), X) = lsssSolved);

  CheckSolution(X);
end;

procedure TCholeskyF64Tests.CopiedStridedReadOnly;
var M, V, X: INDArray<Double>;
begin
  M := TNDAUt.AsArray<Double>([
    [4, 99, 2, 99],
    [2, 99, 5, 99]
  ]);
  V := M[[NDIAll, NDISpan(0, 3, 2)]];
  V.UnsetFlags(NDAF_WRITEABLE);

  CheckTrue(fSolver.Initialize(V) = lsisInitialized);

  CheckEquals(4, M.GetItem([0, 0]), 0);
  CheckEquals(2, M.GetItem([1, 0]), 0);
  M := nil;
  V := nil;

  CheckTrue(fSolver.Solve(TNDAUt.AsArray<Double>(TArray<Double>.Create(8, 12)), X) = lsssSolved);

  CheckEquals(1, X.NDim);
  CheckEquals(1, X.GetItem([0]), 1e-12);
  CheckEquals(2, X.GetItem([1]), 1e-12);
end;

procedure TCholeskyF64Tests.BorrowedOffset;
var M, V: INDArray<Double>;
    X: TArray<Double>;
begin
  M := TNDAUt.AsArray<Double>([
    [99, 4, 2, 99],
    [99, 2, 5, 99]
  ]);
  V := M[[NDIAll, NDISpan(1, 2)]];

  CheckTrue(fSolver.InitializeWithoutCopy(V) = lsisInitialized);

  CheckEquals(2, M.GetItem([0, 1]), 0);
  CheckEquals(1, M.GetItem([1, 1]), 0);
  CheckEquals(99, M.GetItem([0, 0]), 0);
  CheckEquals(99, M.GetItem([1, 3]), 0);
  M := nil;
  V := nil;

  CheckTrue(fSolver.Solve(TArray<Double>.Create(8, 12), X) = lsssSolved);

  CheckSolution(X);
end;

procedure TCholeskyF64Tests.BorrowedReversedRows;
var M, V: INDArray<Double>;
    X: TArray<Double>;
begin
  M := TNDAUt.AsArray<Double>([[2, 5], [4, 2]]);
  V := M[[NDISpan(1, 0, -1), NDIAll]];

  CheckTrue(fSolver.InitializeWithoutCopy(V) = lsisInitialized);
  CheckTrue(fSolver.Solve(TArray<Double>.Create(8, 12), X) = lsssSolved);

  CheckSolution(X);
  CheckEquals(1, M.GetItem([0, 0]), 0);
  CheckEquals(2, M.GetItem([1, 0]), 0);
end;

procedure TCholeskyF64Tests.BorrowedInvalidLayouts;
var M, V: INDArray<Double>;
begin
  M := TNDAUt.AsArray<Double>([
    [4, 99, 2, 99],
    [2, 99, 5, 99]
  ]);
  V := M[[NDIAll, NDISpan(0, 3, 2)]];

  CheckTrue(fSolver.InitializeWithoutCopy(V) = lsisInvalidInput);

  CheckEquals(4, M.GetItem([0, 0]), 0);

  M := TNDAUt.AsArray<Double>(Matrix);
  M.UnsetFlags(NDAF_WRITEABLE);

  CheckTrue(fSolver.InitializeWithoutCopy(M) = lsisInvalidInput);

  CheckFalse(fSolver.Initialized);
end;

procedure TCholeskyF64Tests.StridedRHS;
var M, B, X: INDArray<Double>;
begin
  CheckTrue(fSolver.Initialize(Matrix) = lsisInitialized);
  M := TNDAUt.AsArray<Double>([12, 99, 8]);
  B := M[[NDISpan(2, 0, -2)]];
  B.UnsetFlags(NDAF_WRITEABLE);

  CheckTrue(fSolver.Solve(B, X) = lsssSolved);

  CheckEquals(1, X.GetItem([0]), 1e-12);
  CheckEquals(2, X.GetItem([1]), 1e-12);
  CheckEquals(12, M.GetItem([0]), 0);

  M := TNDAUt.AsArray<Double>(TArray<TArray<Double>>.Create(
    TArray<Double>.Create(12, 99, 2), TArray<Double>.Create(8, 99, 4)));
  B := M[[NDISpan(1, 0, -1), NDISpan(0, 2, 2)]];

  CheckTrue(fSolver.Solve(B, X) = lsssSolved);

  CheckEquals(1, X.GetItem([0, 0]), 1e-12);
  CheckEquals(2, X.GetItem([1, 0]), 1e-12);
  CheckEquals(1, X.GetItem([0, 1]), 1e-12);
  CheckEquals(0, X.GetItem([1, 1]), 1e-12);
end;

procedure TCholeskyF64Tests.ResultsOutliveSolver;
var X: TArray<Double>;
    Y: INDArray<Double>;
begin
  CheckTrue(fSolver.Initialize(Matrix) = lsisInitialized);

  CheckTrue(fSolver.Solve([8, 12], X) = lsssSolved);
  CheckTrue(fSolver.Solve(TNDAUt.AsArray<Double>([8, 12]), Y) = lsssSolved);
  FreeAndNil(fSolver);

  CheckSolution(X);
  CheckEquals(1, Y.GetItem([0]), 1e-12);
  CheckEquals(2, Y.GetItem([1]), 1e-12);
end;

initialization
  RegisterTest(TCholeskyF64Tests.Suite);

end.
