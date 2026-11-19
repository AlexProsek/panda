unit panda.LinAlg.Tests.LeastSquaresF64;

interface

uses
    TestFramework
  , panda.Intfs
  , panda.LinAlg.LeastSquaresF64
  , panda.LinAlg.Base
  ;

type
  TLeastSquaresF64Tests = class(TTestCase)
  protected
    function Matrix: TArray<TArray<Double>>;
    function RHS: TArray<Double>;
    procedure CheckSolution(const aX: INDArray<Double>);
    procedure CheckInvalidND(const aA, ab: INDArray<Double>);
    procedure CheckInvalidDynamic(const aA: TArray<TArray<Double>>;
      const ab: TArray<Double>);
    procedure CheckInjectedStatus(aInit: TLinearSolverInitStatus;
      aSolve: TLinearSolverSolveStatus; aExpected: TLeastSquaresStatus);
    procedure CheckRefinementPolicy(aUseND, aAccept: Boolean);
  published
    procedure ScalarSystem;
    procedure SquareSystem;
    procedure OverdeterminedSystem;
    procedure InconsistentSystem;
    procedure DynamicMatchesND;
    procedure StridedReadOnlyInputs;
    procedure TransposedInput;
    procedure ReversedInputs;
    procedure DynamicInputsUnchanged;
    procedure NDResultsIndependent;
    procedure DynamicResultsIndependent;
    procedure NilMatrix;
    procedure NilRHS;
    procedure VectorAsMatrix;
    procedure MatrixAsRHS;
    procedure TensorAsMatrix;
    procedure EmptyRows;
    procedure EmptyColumns;
    procedure MismatchedNDRHS;
    procedure UnderdeterminedND;
    procedure EmptyDynamicMatrix;
    procedure EmptyDynamicColumns;
    procedure RaggedDynamicMatrix;
    procedure MismatchedDynamicRHS;
    procedure UnderdeterminedDynamic;
    procedure DependentColumns;
    procedure ZeroColumn;
    procedure NumericallyUnresolved;
    procedure InjectedCholeskyDynamic;
    procedure InjectedCholeskyND;
    procedure InjectedRefinementDynamic;
    procedure InjectedRefinementStridedND;
    procedure NilSolverDynamic;
    procedure NilSolverND;
    procedure InvalidDynamicPreservesSolver;
    procedure InvalidNDPreservesSolver;
    procedure SuppliedSolverOwnershipAndReuse;
    procedure InjectedSingularStatus;
    procedure InjectedNotPositiveDefiniteStatus;
    procedure InjectedInvalidInitialization;
    procedure InjectedNumericalInitialization;
    procedure InjectedInvalidSolve;
    procedure InjectedNumericalSolve;
    procedure InjectedUnexpectedUninitialized;
    procedure RefinementRejectedDynamic;
    procedure RefinementAcceptedDynamic;
    procedure RefinementRejectedND;
    procedure RefinementAcceptedND;
  end;

implementation

uses
    System.SysUtils
  , panda.Arrays
  , panda.ArrManip
  , panda.DynArrayUtils
  , panda.LinAlg.CholeskyF64
  , panda.LinAlg.RefineF64
  ;

type
  TLeastSquaresBackend = class(TCholeskyDecompositionF64)
  public
    class var DestroyCount: Integer;
    var InitCount, SolveCount: Integer;
    InitStatus: TLinearSolverInitStatus;
    SolveStatus: TLinearSolverSolveStatus;
    destructor Destroy; override;
    function InitializeWithoutCopy(const aA: INDArray<Double>): TLinearSolverInitStatus; overload; override;
    function Solve(const aB: INDArray<Double>; out aRes: INDArray<Double>): TLinearSolverSolveStatus; overload; override;
  end;

destructor TLeastSquaresBackend.Destroy;
begin
  Inc(DestroyCount);
  inherited;
end;

function TLeastSquaresBackend.InitializeWithoutCopy(
  const aA: INDArray<Double>): TLinearSolverInitStatus;
begin
  Inc(InitCount);
  if InitStatus = lsisInitialized then
    Result := inherited InitializeWithoutCopy(aA)
  else begin
    Reset;
    Result := InitStatus;
  end;
end;

function TLeastSquaresBackend.Solve(const aB: INDArray<Double>;
  out aRes: INDArray<Double>): TLinearSolverSolveStatus;
begin
  Inc(SolveCount);
  if SolveStatus = lsssSolved then
    Result := inherited Solve(aB, aRes)
  else begin
    aRes := TNDAUt.AsArray<Double>([99]);
    Result := SolveStatus;
  end;
end;

function TLeastSquaresF64Tests.Matrix: TArray<TArray<Double>>;
begin
  Result := TArray<TArray<Double>>.Create(TArray<Double>.Create(1, 0),
    TArray<Double>.Create(1, 1), TArray<Double>.Create(1, 2));
end;

function TLeastSquaresF64Tests.RHS: TArray<Double>;
begin
  Result := TArray<Double>.Create(1, 3, 5);
end;

procedure TLeastSquaresF64Tests.CheckSolution(const aX: INDArray<Double>);
begin
  CheckTrue(aX <> nil);
  CheckEquals(1, aX.NDim);
  CheckEquals(2, aX.Shape[0]);
  CheckEquals(1, aX.GetItem([0]), 1e-12);
  CheckEquals(2, aX.GetItem([1]), 1e-12);
end;

procedure TLeastSquaresF64Tests.CheckInvalidND(const aA, ab: INDArray<Double>);
var X: INDArray<Double>;
    status: TLeastSquaresStatus;
begin
  X := TNDAUt.AsArray<Double>(TArray<Double>.Create(99));

  status := LeastSquares(aA, ab, X);

  CheckTrue(status = lssInvalidInput);
  CheckTrue(X = nil);
end;

procedure TLeastSquaresF64Tests.CheckInvalidDynamic(
  const aA: TArray<TArray<Double>>; const ab: TArray<Double>);
var X: TArray<Double>;
    status: TLeastSquaresStatus;
begin
  X := TArray<Double>.Create(99);

  status := LeastSquares(aA, ab, X);

  CheckTrue(status = lssInvalidInput);
  CheckEquals(0, Length(X));
end;

procedure TLeastSquaresF64Tests.ScalarSystem;
var X: TArray<Double>;
    status: TLeastSquaresStatus;
begin
  status := LeastSquares(TArray<TArray<Double>>.Create(
    TArray<Double>.Create(2)), TArray<Double>.Create(6), X);

  CheckTrue(status = lssSolved);
  CheckEquals(1, Length(X));
  CheckEquals(3, X[0], 1e-12);
end;

procedure TLeastSquaresF64Tests.SquareSystem;
var A: TArray<TArray<Double>>;
    b, X: TArray<Double>;
    status: TLeastSquaresStatus;
begin
  A := TArray<TArray<Double>>.Create(TArray<Double>.Create(2, 1),
    TArray<Double>.Create(1, 3));
  b := TArray<Double>.Create(4, 7);

  status := LeastSquares(A, b, X);

  CheckTrue(status = lssSolved);
  CheckEquals(2, Length(X));
  CheckEquals(1, X[0], 1e-12);
  CheckEquals(2, X[1], 1e-12);
end;

procedure TLeastSquaresF64Tests.OverdeterminedSystem;
var A, b, X: INDArray<Double>;
    status: TLeastSquaresStatus;
begin
  A := TNDAUt.AsArray<Double>(Matrix);
  b := TNDAUt.AsArray<Double>(RHS);

  status := LeastSquares(A, b, X);

  CheckTrue(status = lssSolved);
  CheckSolution(X);
end;

procedure TLeastSquaresF64Tests.InconsistentSystem;
var A, b, X: INDArray<Double>;
    status: TLeastSquaresStatus;
begin
  A := TNDAUt.AsArray<Double>(Matrix);
  b := TNDAUt.AsArray<Double>(TArray<Double>.Create(1, 2, 2));

  status := LeastSquares(A, b, X);

  CheckTrue(status = lssSolved);
  CheckEquals(7 / 6, X.GetItem([0]), 1e-12);
  CheckEquals(0.5, X.GetItem([1]), 1e-12);
end;

procedure TLeastSquaresF64Tests.DynamicMatchesND;
var A: TArray<TArray<Double>>;
    b, X: TArray<Double>;
    ndA, ndb, ndX: INDArray<Double>;
    dynamicStatus, ndStatus: TLeastSquaresStatus;
begin
  A := Matrix;
  b := TArray<Double>.Create(1, 2, 2);
  ndA := TNDAUt.AsArray<Double>(A);
  ndb := TNDAUt.AsArray<Double>(b);

  dynamicStatus := LeastSquares(A, b, X);
  ndStatus := LeastSquares(ndA, ndb, ndX);

  CheckTrue(dynamicStatus = lssSolved);
  CheckTrue(dynamicStatus = ndStatus);
  CheckEquals(Length(X), ndX.Shape[0]);
  CheckEquals(X[0], ndX.GetItem([0]), 1e-12);
  CheckEquals(X[1], ndX.GetItem([1]), 1e-12);
end;

procedure TLeastSquaresF64Tests.StridedReadOnlyInputs;
var M, V, B, W, X: INDArray<Double>;
    original: TArray<TArray<Double>>;
    I, J: Integer;
    status: TLeastSquaresStatus;
begin
  original := TArray<TArray<Double>>.Create(
    TArray<Double>.Create(99, 1, 99, 0, 99),
    TArray<Double>.Create(99, 1, 99, 1, 99),
    TArray<Double>.Create(99, 1, 99, 2, 99)
  );
  M := TNDAUt.AsArray<Double>(original);
  V := M[[NDIAll, NDISpan(1, 3, 2)]];
  B := TNDAUt.AsArray<Double>(TArray<Double>.Create(99, 1, 99, 3, 99, 5, 99));
  W := B[[NDISpan(1, 5, 2)]];
  V.UnsetFlags(NDAF_WRITEABLE);
  W.UnsetFlags(NDAF_WRITEABLE);

  status := LeastSquares(V, W, X);

  CheckTrue(status = lssSolved);
  CheckSolution(X);
  CheckFalse(WriteableQ(V));
  CheckFalse(WriteableQ(W));
  for I := 0 to 2 do
    for J := 0 to 4 do
      CheckEquals(original[I, J], M.GetItem([I, J]), 0);
  for I := 0 to 6 do
    if Odd(I) then
      CheckEquals(I, B.GetItem([I]), 0)
    else
      CheckEquals(99, B.GetItem([I]), 0);
end;

procedure TLeastSquaresF64Tests.TransposedInput;
var M, A, b, X: INDArray<Double>;
    status: TLeastSquaresStatus;
begin
  M := TNDAUt.AsArray<Double>(TArray<TArray<Double>>.Create(
    TArray<Double>.Create(1, 1, 1),
    TArray<Double>.Create(0, 1, 2)
  ));
  A := TNDAMan.TransposedView<Double>(M);
  b := TNDAUt.AsArray<Double>(RHS);

  status := LeastSquares(A, b, X);

  CheckTrue(status = lssSolved);
  CheckSolution(X);
  CheckEquals(0, M.GetItem([1, 0]), 0);
  CheckEquals(2, M.GetItem([1, 2]), 0);
end;

procedure TLeastSquaresF64Tests.ReversedInputs;
var M, A, B, W, X: INDArray<Double>;
    status: TLeastSquaresStatus;
begin
  M := TNDAUt.AsArray<Double>(Matrix);
  A := M[[NDISpan(2, 0, -1), NDISpan(1, 0, -1)]];
  B := TNDAUt.AsArray<Double>(RHS);
  W := B[[NDISpan(2, 0, -1)]];
  A.UnsetFlags(NDAF_WRITEABLE);
  W.UnsetFlags(NDAF_WRITEABLE);

  status := LeastSquares(A, W, X);

  CheckTrue(status = lssSolved);
  CheckEquals(2, X.GetItem([0]), 1e-12);
  CheckEquals(1, X.GetItem([1]), 1e-12);
  CheckEquals(0, M.GetItem([0, 1]), 0);
  CheckEquals(5, B.GetItem([2]), 0);
end;

procedure TLeastSquaresF64Tests.DynamicInputsUnchanged;
var A, original: TArray<TArray<Double>>;
    b, originalB, X: TArray<Double>;
    I: Integer;
    status: TLeastSquaresStatus;
begin
  A := Matrix;
  b := RHS;
  original := TDynAUt.Clone<Double>(A);
  originalB := Copy(b);

  status := LeastSquares(A, b, X);

  CheckTrue(status = lssSolved);
  for I := 0 to High(A) do
    CheckTrue(CompareMem(@original[I, 0], @A[I, 0], 2 * SizeOf(Double)));
  CheckTrue(CompareMem(@originalB[0], @b[0], 3 * SizeOf(Double)));
end;

procedure TLeastSquaresF64Tests.NDResultsIndependent;
var A, b, X, Y: INDArray<Double>;
begin
  A := TNDAUt.AsArray<Double>(Matrix);
  b := TNDAUt.AsArray<Double>(RHS);

  CheckTrue(LeastSquares(A, b, X) = lssSolved);
  CheckTrue(LeastSquares(A, b, Y) = lssSolved);
  X.SetItem([0], 99);
  A := nil;
  b := nil;
  X := nil;

  CheckSolution(Y);
end;

procedure TLeastSquaresF64Tests.DynamicResultsIndependent;
var A: TArray<TArray<Double>>;
    b, X, Y: TArray<Double>;
begin
  A := Matrix;
  b := RHS;

  CheckTrue(LeastSquares(A, b, X) = lssSolved);
  CheckTrue(LeastSquares(A, b, Y) = lssSolved);
  X[0] := 99;
  A := nil;
  b := nil;
  X := nil;

  CheckEquals(2, Length(Y));
  CheckEquals(1, Y[0], 1e-12);
  CheckEquals(2, Y[1], 1e-12);
end;

procedure TLeastSquaresF64Tests.NilMatrix;
begin
  CheckInvalidND(nil, TNDAUt.AsArray<Double>(RHS));
end;

procedure TLeastSquaresF64Tests.NilRHS;
begin
  CheckInvalidND(TNDAUt.AsArray<Double>(Matrix), nil);
end;

procedure TLeastSquaresF64Tests.VectorAsMatrix;
begin
  CheckInvalidND(TNDAUt.AsArray<Double>(RHS), TNDAUt.AsArray<Double>(RHS));
end;

procedure TLeastSquaresF64Tests.MatrixAsRHS;
begin
  CheckInvalidND(TNDAUt.AsArray<Double>(Matrix), TNDAUt.AsArray<Double>(Matrix));
end;

procedure TLeastSquaresF64Tests.TensorAsMatrix;
begin
  CheckInvalidND(TNDAUt.Empty<Double>([3, 2, 1]), TNDAUt.AsArray<Double>(RHS));
end;

procedure TLeastSquaresF64Tests.EmptyRows;
begin
  CheckInvalidND(TNDArray<Double>.Create(nil, TArray<NativeInt>.Create(0, 2),
    TArray<NativeInt>.Create(16, 8)),
    TNDArray<Double>.Create(nil, TArray<NativeInt>.Create(0),
    TArray<NativeInt>.Create(8)));
end;

procedure TLeastSquaresF64Tests.EmptyColumns;
begin
  CheckInvalidND(TNDArray<Double>.Create(nil, TArray<NativeInt>.Create(3, 0),
    TArray<NativeInt>.Create(0, 8)), TNDAUt.AsArray<Double>(RHS));
end;

procedure TLeastSquaresF64Tests.MismatchedNDRHS;
begin
  CheckInvalidND(TNDAUt.AsArray<Double>(Matrix),
    TNDAUt.AsArray<Double>(TArray<Double>.Create(1, 3)));
end;

procedure TLeastSquaresF64Tests.UnderdeterminedND;
begin
  CheckInvalidND(TNDAUt.Empty<Double>([2, 3]),
    TNDAUt.AsArray<Double>(TArray<Double>.Create(1, 3)));
end;

procedure TLeastSquaresF64Tests.EmptyDynamicMatrix;
begin
  CheckInvalidDynamic(nil, nil);
end;

procedure TLeastSquaresF64Tests.EmptyDynamicColumns;
begin
  CheckInvalidDynamic(TArray<TArray<Double>>.Create(nil),
    TArray<Double>.Create(1));
end;

procedure TLeastSquaresF64Tests.RaggedDynamicMatrix;
begin
  CheckInvalidDynamic(TArray<TArray<Double>>.Create(TArray<Double>.Create(1, 0),
    TArray<Double>.Create(1)), TArray<Double>.Create(1, 3));
end;

procedure TLeastSquaresF64Tests.MismatchedDynamicRHS;
begin
  CheckInvalidDynamic(Matrix, TArray<Double>.Create(1, 3));
end;

procedure TLeastSquaresF64Tests.UnderdeterminedDynamic;
begin
  CheckInvalidDynamic(TArray<TArray<Double>>.Create(TArray<Double>.Create(1, 0)),
    TArray<Double>.Create(1));
end;

procedure TLeastSquaresF64Tests.DependentColumns;
var A: TArray<TArray<Double>>;
    b, X: TArray<Double>;
    ndA, ndb, ndX: INDArray<Double>;
    dynamicStatus, ndStatus: TLeastSquaresStatus;
begin
  A := TArray<TArray<Double>>.Create(TArray<Double>.Create(1, 2),
    TArray<Double>.Create(2, 4), TArray<Double>.Create(3, 6));
  b := RHS;
  ndA := TNDAUt.AsArray<Double>(A);
  ndb := TNDAUt.AsArray<Double>(b);
  X := TArray<Double>.Create(99);
  ndX := TNDAUt.AsArray<Double>(X);

  dynamicStatus := LeastSquares(A, b, X);
  ndStatus := LeastSquares(ndA, ndb, ndX);

  CheckTrue(dynamicStatus = lssRankDeficient);
  CheckTrue(dynamicStatus = ndStatus);
  CheckEquals(0, Length(X));
  CheckTrue(ndX = nil);
  CheckEquals(6, A[2, 1], 0);
  CheckEquals(6, ndA.GetItem([2, 1]), 0);
end;

procedure TLeastSquaresF64Tests.ZeroColumn;
var A, b, X: INDArray<Double>;
    status: TLeastSquaresStatus;
begin
  A := TNDAUt.AsArray<Double>(TArray<TArray<Double>>.Create(
    TArray<Double>.Create(1, 0), TArray<Double>.Create(2, 0)));
  b := TNDAUt.AsArray<Double>(TArray<Double>.Create(1, 2));
  X := b;

  status := LeastSquares(A, b, X);

  CheckTrue(status = lssRankDeficient);
  CheckTrue(X = nil);
  CheckEquals(2, b.GetItem([1]), 0);
end;

procedure TLeastSquaresF64Tests.NumericallyUnresolved;
var A: TArray<TArray<Double>>;
    b, X: TArray<Double>;
    status: TLeastSquaresStatus;
begin
  A := TArray<TArray<Double>>.Create(TArray<Double>.Create(1, 0),
    TArray<Double>.Create(0, 1e-8));
  b := TArray<Double>.Create(1, 2e-8);
  X := TArray<Double>.Create(99);

  status := LeastSquares(A, b, X);

  CheckTrue(status = lssRankDeficient);
  CheckEquals(0, Length(X));
end;

procedure TLeastSquaresF64Tests.InjectedCholeskyDynamic;
var solver: TCholeskyDecompositionF64;
    A: TArray<TArray<Double>>;
    b, X: TArray<Double>;
begin
  solver := TCholeskyDecompositionF64.Create;
  try
    A := Matrix;
    b := RHS;

    CheckTrue(LeastSquares(A, b, X, solver) = lssSolved);

    CheckEquals(1, X[0], 1e-12);
    CheckEquals(2, X[1], 1e-12);
    CheckEquals(1, A[0, 0], 0);
    CheckEquals(2, A[2, 1], 0);
    CheckEquals(5, b[2], 0);
    CheckTrue(solver.Initialized);
  finally
    solver.Free;
  end;
end;

procedure TLeastSquaresF64Tests.InjectedCholeskyND;
var solver: TCholeskyDecompositionF64;
    X: INDArray<Double>;
begin
  solver := TCholeskyDecompositionF64.Create;
  try
    CheckTrue(LeastSquares(TNDAUt.AsArray<Double>(Matrix),
      TNDAUt.AsArray<Double>(RHS), X, solver) = lssSolved);

    CheckSolution(X);
    CheckTrue(solver.Initialized);
  finally
    solver.Free;
  end;
end;

procedure TLeastSquaresF64Tests.InjectedRefinementDynamic;
var solver: TGaussRefineSolverF64;
    A: TArray<TArray<Double>>;
    b, X, Y: TArray<Double>;
begin
  solver := TGaussRefineSolverF64.Create(
    TGaussRefineF64.Create(TCholeskyDecompositionF64.Create));
  try
    A := Matrix;
    b := RHS;

    CheckTrue(LeastSquares(A, b, X, solver) = lssSolved);
    CheckTrue(LeastSquares(A, b, Y, solver) = lssSolved);

    CheckEquals(1, X[0], 1e-12);
    CheckEquals(2, X[1], 1e-12);
    CheckTrue(solver.LastResults[0].Status = grsConverged);
    CheckEquals(1, A[0, 0], 0);
    CheckEquals(2, A[2, 1], 0);
    CheckEquals(5, b[2], 0);
    X[0] := 99;
    CheckEquals(1, Y[0], 1e-12);
  finally
    solver.Free;
  end;

  CheckEquals(2, Y[1], 1e-12);
end;

procedure TLeastSquaresF64Tests.InjectedRefinementStridedND;
var solver: TGaussRefineSolverF64;
    M, A, B, V, X, Y: INDArray<Double>;
begin
  solver := TGaussRefineSolverF64.Create;
  try
    M := TNDAUt.AsArray<Double>(Matrix);
    A := M[[NDISpan(2, 0, -1), NDISpan(1, 0, -1)]];
    B := TNDAUt.AsArray<Double>(RHS);
    V := B[[NDISpan(2, 0, -1)]];
    A.UnsetFlags(NDAF_WRITEABLE);
    V.UnsetFlags(NDAF_WRITEABLE);

    CheckTrue(LeastSquares(A, V, X, solver) = lssSolved);
    CheckTrue(LeastSquares(A, V, Y, solver) = lssSolved);

    CheckEquals(2, X.GetItem([0]), 1e-12);
    CheckEquals(1, X.GetItem([1]), 1e-12);
    CheckEquals(2, M.GetItem([2, 1]), 0);
    CheckEquals(5, B.GetItem([2]), 0);
    X.SetItem([0], 99);
    CheckEquals(2, Y.GetItem([0]), 1e-12);
  finally
    solver.Free;
  end;

  CheckEquals(1, Y.GetItem([1]), 1e-12);
end;

procedure TLeastSquaresF64Tests.NilSolverDynamic;
var X: TArray<Double>;
begin
  X := TArray<Double>.Create(99);

  CheckTrue(LeastSquares(Matrix, RHS, X, nil) = lssInvalidInput);

  CheckEquals(0, Length(X));
end;

procedure TLeastSquaresF64Tests.NilSolverND;
var X: INDArray<Double>;
begin
  X := TNDAUt.AsArray<Double>([99]);

  CheckTrue(LeastSquares(TNDAUt.AsArray<Double>(Matrix),
    TNDAUt.AsArray<Double>(RHS), X, nil) = lssInvalidInput);

  CheckTrue(X = nil);
end;

procedure TLeastSquaresF64Tests.InvalidDynamicPreservesSolver;
var solver: TLeastSquaresBackend;
    X: TArray<Double>;
begin
  solver := TLeastSquaresBackend.Create;
  try
    CheckTrue(solver.Initialize(TArray<TArray<Double>>.Create(
      TArray<Double>.Create(4))) = lsisInitialized);

    CheckTrue(LeastSquares(TArray<TArray<Double>>.Create(
      TArray<Double>.Create(1), TArray<Double>.Create(1, 2)), RHS, X, solver) = lssInvalidInput);

    CheckTrue(solver.Initialized);
    CheckEquals(0, solver.InitCount);
    CheckEquals(0, solver.SolveCount);
    CheckTrue(solver.Solve(TArray<Double>.Create(8), X) = lsssSolved);
    CheckEquals(2, X[0], 0);
  finally
    solver.Free;
  end;
end;

procedure TLeastSquaresF64Tests.InvalidNDPreservesSolver;
var solver: TLeastSquaresBackend;
    X: INDArray<Double>;
begin
  solver := TLeastSquaresBackend.Create;
  try
    CheckTrue(solver.Initialize(TNDAUt.AsArray<Double>([[4]])) = lsisInitialized);

    CheckTrue(LeastSquares(INDArray<Double>(nil), TNDAUt.AsArray<Double>(RHS), X, solver) = lssInvalidInput);

    CheckTrue(X = nil);
    CheckTrue(solver.Initialized);
    CheckEquals(0, solver.InitCount);

    CheckTrue(LeastSquares(TNDAUt.AsArray<Double>(Matrix),
      TNDAUt.AsArray<Double>([1, 2]), X, solver) = lssInvalidInput);

    CheckTrue(solver.Initialized);
    CheckEquals(0, solver.InitCount);
  finally
    solver.Free;
  end;
end;

procedure TLeastSquaresF64Tests.SuppliedSolverOwnershipAndReuse;
var solver: TLeastSquaresBackend;
    X, Y: INDArray<Double>;
begin
  TLeastSquaresBackend.DestroyCount := 0;
  solver := TLeastSquaresBackend.Create;
  try
    CheckTrue(solver.Initialize(TNDAUt.AsArray<Double>([[4]])) = lsisInitialized);

    CheckTrue(LeastSquares(TNDAUt.AsArray<Double>(Matrix),
      TNDAUt.AsArray<Double>(RHS), X, solver) = lssSolved);
    CheckTrue(solver.Solve(TNDAUt.AsArray<Double>([3, 3]), Y) = lsssSolved);

    CheckEquals(0, TLeastSquaresBackend.DestroyCount);
    CheckEquals(1, solver.InitCount);
    CheckEquals(2, solver.SolveCount);
    CheckEquals(1, Y.GetItem([0]), 1e-12);
    CheckEquals(0, Y.GetItem([1]), 1e-12);
    CheckSolution(X);
  finally
    solver.Free;
  end;

  CheckEquals(1, TLeastSquaresBackend.DestroyCount);
  CheckSolution(X);
end;

procedure TLeastSquaresF64Tests.CheckInjectedStatus(aInit: TLinearSolverInitStatus;
  aSolve: TLinearSolverSolveStatus; aExpected: TLeastSquaresStatus);
var solver: TLeastSquaresBackend;
    X: INDArray<Double>;
begin
  solver := TLeastSquaresBackend.Create;
  try
    solver.InitStatus := aInit;
    solver.SolveStatus := aSolve;
    X := TNDAUt.AsArray<Double>([99]);

    CheckTrue(LeastSquares(TNDAUt.AsArray<Double>(Matrix),
      TNDAUt.AsArray<Double>(RHS), X, solver) = aExpected);

    CheckTrue(X = nil);
    CheckEquals(1, solver.InitCount);
    if aInit = lsisInitialized then
      CheckEquals(1, solver.SolveCount)
    else
      CheckEquals(0, solver.SolveCount);
  finally
    solver.Free;
  end;
end;

procedure TLeastSquaresF64Tests.InjectedSingularStatus;
begin
  CheckInjectedStatus(lsisSingular, lsssSolved, lssRankDeficient);
end;

procedure TLeastSquaresF64Tests.InjectedNotPositiveDefiniteStatus;
begin
  CheckInjectedStatus(lsisNotPositiveDefinite, lsssSolved, lssRankDeficient);
end;

procedure TLeastSquaresF64Tests.InjectedInvalidInitialization;
begin
  CheckInjectedStatus(lsisInvalidInput, lsssSolved, lssInvalidInput);
end;

procedure TLeastSquaresF64Tests.InjectedNumericalInitialization;
begin
  CheckInjectedStatus(lsisNumericalFailure, lsssSolved, lssNumericalFailure);
end;

procedure TLeastSquaresF64Tests.InjectedInvalidSolve;
begin
  CheckInjectedStatus(lsisInitialized, lsssInvalidInput, lssInvalidInput);
end;

procedure TLeastSquaresF64Tests.InjectedNumericalSolve;
begin
  CheckInjectedStatus(lsisInitialized, lsssNumericalFailure, lssNumericalFailure);
end;

procedure TLeastSquaresF64Tests.InjectedUnexpectedUninitialized;
begin
  CheckInjectedStatus(lsisInitialized, lsssNotInitialized, lssNumericalFailure);
end;

procedure TLeastSquaresF64Tests.CheckRefinementPolicy(aUseND, aAccept: Boolean);
var solver: TGaussRefineSolverF64;
    A: TArray<TArray<Double>>;
    b, X: TArray<Double>;
    ndX: INDArray<Double>;
    status: TLeastSquaresStatus;
begin
  solver := TGaussRefineSolverF64.Create;
  try
    A := TArray<TArray<Double>>.Create(TArray<Double>.Create(1),
      TArray<Double>.Create(1), TArray<Double>.Create(1));
    b := TArray<Double>.Create(1, 0, 0);
    solver.Refiner.MaxIterations := 0;
    solver.AcceptNonConvergedSolution := aAccept;

    if aUseND then begin
      status := LeastSquares(TNDAUt.AsArray<Double>(A), TNDAUt.AsArray<Double>(b), ndX, solver);
      if status = lssSolved then TNDAUt.TryAsDynArray<Double>(ndX, X);
    end else
      status := LeastSquares(A, b, X, solver);

    CheckTrue(solver.LastResults[0].Status = grsIterationLimit);
    CheckTrue(solver.Initialized);
    if aAccept then begin
      CheckTrue(status = lssSolved);
      CheckEquals(1 / 3, X[0], 1e-16);
    end else begin
      CheckTrue(status = lssNotConverged);
      CheckEquals(0, Length(X));
      CheckTrue(ndX = nil);
    end;
  finally
    solver.Free;
  end;
end;

procedure TLeastSquaresF64Tests.RefinementRejectedDynamic;
begin
  CheckRefinementPolicy(False, False);
end;

procedure TLeastSquaresF64Tests.RefinementAcceptedDynamic;
begin
  CheckRefinementPolicy(False, True);
end;

procedure TLeastSquaresF64Tests.RefinementRejectedND;
begin
  CheckRefinementPolicy(True, False);
end;

procedure TLeastSquaresF64Tests.RefinementAcceptedND;
begin
  CheckRefinementPolicy(True, True);
end;

initialization

  RegisterTest(TLeastSquaresF64Tests.Suite);

end.
