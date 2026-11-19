unit panda.LinAlg.Tests.Solver;

interface

uses
    TestFramework
  , panda.Intfs
  , panda.LinAlg.Base
  , panda.LinAlg.RefineF64
  , panda.Tests.NDATestCase
  ;

type
  TBackendBehavior = (bbStatus, bbNonfinite, bbException);

  TLinearSolverTests = class(TNDATestCase)
  protected
    fSolver: TLinearSolver<Double>;
    function CreateSolver: TLinearSolver<Double>; virtual;
    function Matrix: TArray<TArray<Double>>;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure RepeatedVectors;
    procedure MultipleNDRHS;
    procedure BorrowedDynamicStorage;
    procedure BorrowedNDStorage;
    procedure FailedReplacement;
    procedure InvalidRHSPreservesInitialization;
    procedure SuccessfulReplacement;
    procedure ResetInvalidatesInitialization;
  end;

  TCholeskySolverTests = class(TLinearSolverTests)
  protected
    function CreateSolver: TLinearSolver<Double>; override;
  end;

  TRefineSolverTests = class(TLinearSolverTests)
  protected
    function CreateSolver: TLinearSolver<Double>; override;
    procedure CheckNonconvergence(aStagnate, aAccept: Boolean);
  published
    procedure NilRefiner;
    procedure OwnsRefiner;
    procedure WrapInitialized;
    procedure ResetReleasesStorage;
    procedure FailedReplacementReleasesBackend;
    procedure ExceptionalReplacement;
    procedure CopyIndependent;
    procedure BorrowedDynamicFactors;
    procedure BorrowedPivotedRows;
    procedure BorrowedCholeskyFactors;
    procedure BorrowedOffsetNegativeRows;
    procedure CopiedStridedReadOnly;
    procedure RejectedColumnStride;
    procedure RejectedReadOnly;
    procedure RejectedOverlappingRows;
    procedure StridedVectorRHS;
    procedure StridedMatrixRHS;
    procedure IterationLimitRejected;
    procedure IterationLimitAccepted;
    procedure StagnationRejected;
    procedure StagnationAccepted;
    procedure NumericalFailureWithAcceptance;
    procedure LaterColumnFailure;
    procedure MatrixNonconvergenceRejected;
    procedure MatrixNonconvergenceAccepted;
    procedure MatrixMatchesVectorSolves;
    procedure DiagnosticsIndependentAndReplaced;
    procedure InvalidOptionsNoDiagnostics;
    procedure NonfiniteRHSNoDiagnostics;
    procedure SingularInitialization;
    procedure NotPositiveDefiniteInitialization;
    procedure NumericalInitializationFailure;
  end;

  TRefineBackendTests = class(TNDATestCase)
  protected
    procedure CheckInitialFailure(aBehavior: TBackendBehavior);
    procedure CheckCorrectionFailure(aBehavior: TBackendBehavior);
    procedure CheckInitFailure(aStatus: TLinearSolverInitStatus; aExpected: TGaussRefineInitStatus);
  published
    procedure NilBackend;
    procedure OwnsBackend;
    procedure SuppliedLUPReusesFactors;
    procedure SuppliedCholesky;
    procedure SuppliedCholeskyRefines;
    procedure SuppliedCholeskyND;
    procedure CholeskySettings;
    procedure CholeskyRejection;
    procedure CholeskyOverflow;
    procedure InvalidBackendInput;
    procedure SingularBackend;
    procedure NotPositiveDefiniteBackend;
    procedure NonfiniteFactorsOnSuccess;
    procedure NonfiniteFactorsOnRejection;
    procedure InitialStatusFailure;
    procedure InitialNonfiniteResult;
    procedure InitialArithmeticException;
    procedure CorrectionStatusFailure;
    procedure CorrectionNonfiniteResult;
    procedure CorrectionArithmeticException;
    procedure CorrectionFailurePreservesAcceptedUpdate;
  end;

implementation

uses
    System.SysUtils
  , System.Math
  , panda.Arrays
  , panda.DynArrayUtils
  , panda.LinAlg.LUPF64
  , panda.LinAlg.CholeskyF64
  ;

type
  TTrackingBackend = class(TLUPDecompositionF64)
  public
    class var DestroyCount: Integer;
    var InitializeCount, SolveCount, FailOnSolve: Integer;
    Behavior: TBackendBehavior;
    RejectInitialization, NonfiniteFactors: Boolean;
    InitStatus: TLinearSolverInitStatus;
    InitialOffset: Double;
    ShortCorrection: Boolean;
    RaiseInitialization: Boolean;
    destructor Destroy; override;
    function InitializeWithoutCopy(const aA: TArray<TArray<Double>>): TLinearSolverInitStatus; overload; override;
    function Solve(const ab: TArray<Double>; out aRes: TArray<Double>): TLinearSolverSolveStatus; overload; override;
  end;

  TRefinerAccess = class(TGaussRefineF64);
  TLUPAccess = class(TLUPDecompositionF64);

function ScalarMatrix(aValue: Double): TArray<TArray<Double>>;
begin
  Result := TArray<TArray<Double>>.Create(TArray<Double>.Create(aValue));
end;

destructor TTrackingBackend.Destroy;
begin
  Inc(DestroyCount);
  inherited;
end;

function TTrackingBackend.InitializeWithoutCopy(
  const aA: TArray<TArray<Double>>): TLinearSolverInitStatus;
begin
  Inc(InitializeCount);
  if RaiseInitialization then raise EAbort.Create('Test initialization exception');
  if RejectInitialization then begin
    ClearAnchors;
    Result := InitStatus;
  end else
    Result := inherited InitializeWithoutCopy(aA);
  if NonfiniteFactors then aA[0][0] := Infinity;
end;

function TTrackingBackend.Solve(const ab: TArray<Double>;
  out aRes: TArray<Double>): TLinearSolverSolveStatus;
begin
  Inc(SolveCount);
  if SolveCount <> FailOnSolve then begin
    Result := inherited Solve(ab, aRes);
    if Result = lsssSolved then begin
      if SolveCount = 1 then aRes[0] := aRes[0] + InitialOffset;
      if (SolveCount = 2) and ShortCorrection then aRes[0] := aRes[0] / 2;
    end;
    exit;
  end;
  aRes := nil;
  Result := lsssSolved;
  case Behavior of
    bbStatus: Result := lsssNotInitialized;
    bbNonfinite: aRes := TArray<Double>.Create(Infinity);
    bbException: raise EInvalidOp.Create('Test arithmetic failure');
  end;
end;

function TLinearSolverTests.CreateSolver: TLinearSolver<Double>;
begin
  Result := TLUPDecompositionF64.Create;
end;

function TCholeskySolverTests.CreateSolver: TLinearSolver<Double>;
begin
  Result := TCholeskyDecompositionF64.Create;
end;

function TLinearSolverTests.Matrix: TArray<TArray<Double>>;
begin
  Result := TArray<TArray<Double>>.Create(
    TArray<Double>.Create(4, 2), TArray<Double>.Create(2, 5));
end;

procedure TLinearSolverTests.SetUp;
begin
  inherited;
  fSolver := CreateSolver;
end;

procedure TLinearSolverTests.TearDown;
begin
  fSolver.Free;
  inherited;
end;

procedure TLinearSolverTests.RepeatedVectors;
var first, second: TArray<Double>;
begin
  CheckFalse(fSolver.Initialized);
  CheckTrue(fSolver.Initialize(Matrix) = lsisInitialized);

  CheckTrue(fSolver.Solve(TArray<Double>.Create(8, 12), first) = lsssSolved);
  CheckTrue(fSolver.Solve(TArray<Double>.Create(12, 8), second) = lsssSolved);

  CheckEquals([1, 2], first, 1e-12);
  CheckEquals([2.75, 0.5], second, 1e-12);
  second[0] := 99;
  CheckEquals(1, first[0], 1e-12);
end;

procedure TLinearSolverTests.MultipleNDRHS;
var A, B, X: INDArray<Double>;
    values: TArray<TArray<Double>>;
begin
  A := TNDAUt.AsArray<Double>(Matrix);
  B := TNDAUt.AsArray<Double>(TArray<TArray<Double>>.Create(
    TArray<Double>.Create(8, 12), TArray<Double>.Create(12, 8)));
  CheckTrue(fSolver.Initialize(A) = lsisInitialized);

  CheckTrue(fSolver.Solve(B, X) = lsssSolved);

  CheckTrue(TNDAUt.TryAsDynArray2D<Double>(X, values));
  CheckEquals([1, 2.75], values[0], 1e-12);
  CheckEquals([2, 0.5], values[1], 1e-12);
end;

procedure TLinearSolverTests.BorrowedDynamicStorage;
var A: TArray<TArray<Double>>;
    X: TArray<Double>;
begin
  A := Matrix;

  CheckTrue(fSolver.InitializeWithoutCopy(A) = lsisInitialized);
  A := nil;
  CheckTrue(fSolver.Solve(TArray<Double>.Create(8, 12), X) = lsssSolved);
  FreeAndNil(fSolver);

  CheckEquals([1, 2], X, 1e-12);
end;

procedure TLinearSolverTests.BorrowedNDStorage;
var A, B, X: INDArray<Double>;
    values: TArray<Double>;
begin
  A := TNDAUt.AsArray<Double>(Matrix);
  B := TNDAUt.AsArray<Double>(TArray<Double>.Create(8, 12));

  CheckTrue(fSolver.InitializeWithoutCopy(A) = lsisInitialized);
  A := nil;
  CheckTrue(fSolver.Solve(B, X) = lsssSolved);
  FreeAndNil(fSolver);

  CheckTrue(TNDAUt.TryAsDynArray<Double>(X, values));
  CheckEquals([1, 2], values, 1e-12);
end;

procedure TLinearSolverTests.FailedReplacement;
var X: TArray<Double>;
begin
  CheckTrue(fSolver.Initialize(Matrix) = lsisInitialized);

  CheckTrue(fSolver.Initialize(TArray<TArray<Double>>(nil)) = lsisInvalidInput);

  CheckFalse(fSolver.Initialized);
  CheckTrue(fSolver.Solve(TArray<Double>.Create(8, 12), X) = lsssNotInitialized);
  CheckEquals(0, Length(X));
end;

procedure TLinearSolverTests.InvalidRHSPreservesInitialization;
var X: TArray<Double>;
begin
  CheckTrue(fSolver.Initialize(Matrix) = lsisInitialized);
  X := TArray<Double>.Create(99);

  CheckTrue(fSolver.Solve(TArray<Double>.Create(8), X) = lsssInvalidInput);

  CheckEquals(0, Length(X));
  CheckTrue(fSolver.Initialized);
  CheckTrue(fSolver.Solve(TArray<Double>.Create(8, 12), X) = lsssSolved);
  CheckEquals([1, 2], X, 1e-12);
end;

procedure TLinearSolverTests.SuccessfulReplacement;
var X: TArray<Double>;
begin
  CheckTrue(fSolver.Initialize(Matrix) = lsisInitialized);

  CheckTrue(fSolver.Initialize(ScalarMatrix(4)) = lsisInitialized);
  CheckTrue(fSolver.Solve(TArray<Double>.Create(8), X) = lsssSolved);

  CheckEquals([2], X, 1e-12);
end;

procedure TLinearSolverTests.ResetInvalidatesInitialization;
var X: TArray<Double>;
begin
  fSolver.LowerBandWidth := 1;
  CheckTrue(fSolver.Initialize(Matrix) = lsisInitialized);

  fSolver.Reset;
  fSolver.Reset;

  CheckFalse(fSolver.Initialized);
  CheckEquals(1, fSolver.LowerBandWidth);
  CheckTrue(fSolver.Solve(TArray<Double>.Create(8, 12), X) = lsssNotInitialized);
  CheckEquals(0, Length(X));
  CheckTrue(fSolver.Initialize(Matrix) = lsisInitialized);
end;

procedure TRefineBackendTests.NilBackend;
var refine: TGaussRefineF64;
    raised: Boolean;
begin
  refine := nil;
  raised := False;

  try
    refine := TGaussRefineF64.Create(nil);
  except
    on E: EArgumentNilException do raised := True;
  end;
  refine.Free;

  CheckTrue(raised);
end;

procedure TRefineBackendTests.OwnsBackend;
var backend: TTrackingBackend;
    refine: TGaussRefineF64;
begin
  TTrackingBackend.DestroyCount := 0;
  backend := TTrackingBackend.Create;
  refine := TGaussRefineF64.Create(backend);
  try
    CheckTrue(refine.Solver = backend);
    CheckEquals(0, TTrackingBackend.DestroyCount);
  finally
    refine.Free;
  end;

  CheckEquals(1, TTrackingBackend.DestroyCount);
end;

procedure TRefineBackendTests.SuppliedLUPReusesFactors;
var backend: TTrackingBackend;
    refine: TGaussRefineF64;
    X: TArray<Double>;
    info: TGaussRefinementResult;
begin
  backend := TTrackingBackend.Create;
  refine := TGaussRefineF64.Create(backend);
  try
    CheckTrue(refine.Initialize(ScalarMatrix(3)) = grisInitialized);

    info := refine.Solve(TArray<Double>.Create(1), X);

    CheckTrue(info.Status = grsConverged);
    CheckEquals(1, info.Iterations);
    CheckEquals(2, backend.SolveCount);
    CheckEquals([1 / 3], X, 0);

    info := refine.Solve(TArray<Double>.Create(2), X);

    CheckTrue(info.Status = grsConverged);
    CheckEquals(1, backend.InitializeCount);
    CheckEquals(4, backend.SolveCount);
    CheckEquals([2 / 3], X, 0);
  finally
    refine.Free;
  end;
end;

procedure TRefineBackendTests.SuppliedCholesky;
var refine: TGaussRefineF64;
    first, second: TArray<Double>;
    info: TGaussRefinementResult;
begin
  refine := TGaussRefineF64.Create(TCholeskyDecompositionF64.Create);
  try
    CheckTrue(refine.Initialize(ScalarMatrix(4)) = grisInitialized);

    info := refine.Solve(TArray<Double>.Create(8), first);

    CheckTrue(info.Status = grsConverged);
    CheckEquals([2], first, 0);

    info := refine.Solve(TArray<Double>.Create(12), second);

    CheckTrue(info.Status = grsConverged);
    CheckEquals([3], second, 0);
    CheckEquals([2], first, 0);
  finally
    refine.Free;
  end;
end;

procedure TRefineBackendTests.SuppliedCholeskyRefines;
var refine: TGaussRefineF64;
    X: TArray<Double>;
    info: TGaussRefinementResult;
begin
  refine := TGaussRefineF64.Create(TCholeskyDecompositionF64.Create);
  try
    CheckTrue(refine.Initialize(ScalarMatrix(3)) = grisInitialized);

    info := refine.Solve(TArray<Double>.Create(1), X);

    CheckTrue(info.Status = grsConverged);
    CheckTrue(info.Iterations > 0);
    CheckEquals([1 / 3], X, 0);
  finally
    refine.Free;
  end;
end;

procedure TRefineBackendTests.SuppliedCholeskyND;
var refine: TGaussRefineF64;
    A, B, X: INDArray<Double>;
    values: TArray<Double>;
    info: TGaussRefinementResult;
begin
  refine := TGaussRefineF64.Create(TCholeskyDecompositionF64.Create);
  try
    A := TNDAUt.AsArray<Double>(TArray<TArray<Double>>.Create(
      TArray<Double>.Create(4, 2), TArray<Double>.Create(2, 5)));
    B := TNDAUt.AsArray<Double>(TArray<Double>.Create(8, 12));
    CheckTrue(refine.Initialize(A) = grisInitialized);

    info := refine.Solve(B, X);

    CheckTrue(info.Status = grsConverged);
    CheckTrue(TNDAUt.TryAsDynArray<Double>(X, values));
    CheckEquals([1, 2], values, 1e-12);
  finally
    refine.Free;
  end;
end;

procedure TRefineBackendTests.CholeskySettings;
var backend: TCholeskyDecompositionF64;
    refine: TGaussRefineF64;
    X: TArray<Double>;
begin
  backend := TCholeskyDecompositionF64.Create;
  backend.PivotTolerance := 5;
  refine := TGaussRefineF64.Create(backend);
  try
    CheckTrue(refine.Initialize(ScalarMatrix(4)) = grisNotPositiveDefinite);

    backend.PivotTolerance := 1;
    CheckTrue(refine.Initialize(ScalarMatrix(4)) = grisInitialized);
    backend.PivotTolerance := 5;

    CheckTrue(refine.Solve(TArray<Double>.Create(8), X).Status = grsConverged);
    CheckEquals([2], X, 0);
    CheckTrue(refine.Initialize(ScalarMatrix(4)) = grisNotPositiveDefinite);
  finally
    refine.Free;
  end;
end;

procedure TRefineBackendTests.CholeskyRejection;
var refine: TGaussRefineF64;
    X: TArray<Double>;
begin
  refine := TGaussRefineF64.Create(TCholeskyDecompositionF64.Create);
  try
    CheckTrue(refine.Initialize(ScalarMatrix(4)) = grisInitialized);

    CheckTrue(refine.Initialize(ScalarMatrix(-1)) = grisNotPositiveDefinite);

    CheckFalse(refine.Initialized);
    CheckFalse(refine.Solver.Initialized);
    CheckTrue(refine.Solve(TArray<Double>.Create(1), X).Status = grsNotInitialized);
  finally
    refine.Free;
  end;
end;

procedure TRefineBackendTests.CholeskyOverflow;
var refine: TGaussRefineF64;
    A: TArray<TArray<Double>>;
    mask: TArithmeticExceptionMask;
begin
  refine := TGaussRefineF64.Create(TCholeskyDecompositionF64.Create);
  try
    A := TArray<TArray<Double>>.Create(TArray<Double>.Create(1, 1e308),
      TArray<Double>.Create(1e308, 1));
    mask := GetExceptionMask;
    SetExceptionMask(mask + [exOverflow, exInvalidOp]);
    try
      CheckTrue(refine.Initialize(A) = grisNumericalFailure);
    finally
      SetExceptionMask(mask);
    end;

    CheckFalse(refine.Initialized);
    CheckFalse(refine.Solver.Initialized);
    CheckTrue(refine.Initialize(ScalarMatrix(4)) = grisInitialized);
  finally
    refine.Free;
  end;
end;

procedure TRefineBackendTests.CheckInitFailure(aStatus: TLinearSolverInitStatus;
  aExpected: TGaussRefineInitStatus);
var backend: TTrackingBackend;
    refine: TGaussRefineF64;
begin
  backend := TTrackingBackend.Create;
  backend.RejectInitialization := True;
  backend.InitStatus := aStatus;
  refine := TGaussRefineF64.Create(backend);
  try
    CheckTrue(refine.Initialize(ScalarMatrix(3)) = aExpected);

    CheckFalse(refine.Initialized);
    CheckEquals(1, backend.InitializeCount);
    CheckEquals(0, backend.SolveCount);
  finally
    refine.Free;
  end;
end;

procedure TRefineBackendTests.InvalidBackendInput;
begin
  CheckInitFailure(lsisInvalidInput, grisInvalidInput);
end;

procedure TRefineBackendTests.SingularBackend;
begin
  CheckInitFailure(lsisSingular, grisSingular);
end;

procedure TRefineBackendTests.NotPositiveDefiniteBackend;
begin
  CheckInitFailure(lsisNotPositiveDefinite, grisNotPositiveDefinite);
end;

procedure TRefineBackendTests.NonfiniteFactorsOnSuccess;
var backend: TTrackingBackend;
    refine: TGaussRefineF64;
begin
  backend := TTrackingBackend.Create;
  backend.NonfiniteFactors := True;
  refine := TGaussRefineF64.Create(backend);
  try
    CheckTrue(refine.Initialize(ScalarMatrix(3)) = grisNumericalFailure);

    CheckFalse(refine.Initialized);
  finally
    refine.Free;
  end;
end;

procedure TRefineBackendTests.NonfiniteFactorsOnRejection;
var backend: TTrackingBackend;
    refine: TGaussRefineF64;
begin
  backend := TTrackingBackend.Create;
  backend.NonfiniteFactors := True;
  backend.RejectInitialization := True;
  backend.InitStatus := lsisSingular;
  refine := TGaussRefineF64.Create(backend);
  try
    CheckTrue(refine.Initialize(ScalarMatrix(3)) = grisNumericalFailure);

    CheckFalse(refine.Initialized);
  finally
    refine.Free;
  end;
end;

procedure TRefineBackendTests.CheckInitialFailure(aBehavior: TBackendBehavior);
var backend: TTrackingBackend;
    refine: TGaussRefineF64;
    X: TArray<Double>;
    info: TGaussRefinementResult;
begin
  backend := TTrackingBackend.Create;
  backend.FailOnSolve := 1;
  backend.Behavior := aBehavior;
  refine := TGaussRefineF64.Create(backend);
  try
    CheckTrue(refine.Initialize(ScalarMatrix(3)) = grisInitialized);

    info := refine.Solve(TArray<Double>.Create(1), X);

    CheckTrue(info.Status = grsNumericalFailure);
    CheckEquals(0, Length(X));
    CheckEquals(0, info.Iterations);
    CheckTrue(IsNan(info.BackwardError));
    CheckTrue(refine.Initialized);
    CheckEquals(1, backend.SolveCount);
  finally
    refine.Free;
  end;
end;

procedure TRefineBackendTests.CheckCorrectionFailure(aBehavior: TBackendBehavior);
var backend: TTrackingBackend;
    refine: TGaussRefineF64;
    X, initial: TArray<Double>;
    info, referenceInfo: TGaussRefinementResult;
begin
  backend := TTrackingBackend.Create;
  backend.FailOnSolve := 3;
  backend.Behavior := aBehavior;
  refine := TGaussRefineF64.Create(backend);
  try
    CheckTrue(refine.Initialize(ScalarMatrix(3)) = grisInitialized);
    refine.MaxIterations := 0;
    referenceInfo := refine.Solve(TArray<Double>.Create(1), initial);
    refine.MaxIterations := 10;

    info := refine.Solve(TArray<Double>.Create(1), X);

    CheckTrue(info.Status = grsNumericalFailure);
    CheckEquals(initial, X, 0);
    CheckEquals(referenceInfo.BackwardError, info.BackwardError, 0);
    CheckEquals(1, info.Iterations);
    CheckEquals(3, backend.SolveCount);
    CheckTrue(refine.Initialized);
  finally
    refine.Free;
  end;
end;

procedure TRefineBackendTests.InitialStatusFailure;
begin
  CheckInitialFailure(bbStatus);
end;

procedure TRefineBackendTests.InitialNonfiniteResult;
begin
  CheckInitialFailure(bbNonfinite);
end;

procedure TRefineBackendTests.InitialArithmeticException;
begin
  CheckInitialFailure(bbException);
end;

procedure TRefineBackendTests.CorrectionStatusFailure;
begin
  CheckCorrectionFailure(bbStatus);
end;

procedure TRefineBackendTests.CorrectionNonfiniteResult;
begin
  CheckCorrectionFailure(bbNonfinite);
end;

procedure TRefineBackendTests.CorrectionArithmeticException;
begin
  CheckCorrectionFailure(bbException);
end;

procedure TRefineBackendTests.CorrectionFailurePreservesAcceptedUpdate;
var backend: TTrackingBackend;
    refine: TGaussRefineF64;
    X: TArray<Double>;
    info: TGaussRefinementResult;
begin
  backend := TTrackingBackend.Create;
  backend.InitialOffset := -0.5;
  backend.ShortCorrection := True;
  backend.FailOnSolve := 3;
  refine := TGaussRefineF64.Create(backend);
  try
    CheckTrue(refine.Initialize(ScalarMatrix(1)) = grisInitialized);

    info := refine.Solve(TArray<Double>.Create(1), X);

    CheckTrue(info.Status = grsNumericalFailure);
    CheckEquals([0.75], X, 0);
    CheckEquals(1 / 7, info.BackwardError, 1e-16);
    CheckEquals(2, info.Iterations);
    CheckEquals(1, backend.InitializeCount);
  finally
    refine.Free;
  end;
end;

function TRefineSolverTests.CreateSolver: TLinearSolver<Double>;
begin
  Result := TGaussRefineSolverF64.Create;
end;

procedure TRefineSolverTests.NilRefiner;
var wrapper: TGaussRefineSolverF64;
    raised: Boolean;
begin
  wrapper := nil;
  raised := False;

  try
    wrapper := TGaussRefineSolverF64.Create(nil);
  except
    on E: EArgumentNilException do raised := True;
  end;
  wrapper.Free;

  CheckTrue(raised);
end;

procedure TRefineSolverTests.OwnsRefiner;
var refiner: TGaussRefineF64;
    wrapper: TGaussRefineSolverF64;
begin
  TTrackingBackend.DestroyCount := 0;
  refiner := TGaussRefineF64.Create(TTrackingBackend.Create);
  wrapper := TGaussRefineSolverF64.Create(refiner);
  try
    CheckTrue(wrapper.Refiner = refiner);
    CheckEquals(0, TTrackingBackend.DestroyCount);
  finally
    wrapper.Free;
  end;

  CheckEquals(1, TTrackingBackend.DestroyCount);
end;

procedure TRefineSolverTests.WrapInitialized;
var refiner: TGaussRefineF64;
    X: TArray<Double>;
begin
  refiner := TGaussRefineF64.Create;
  CheckTrue(refiner.Initialize(ScalarMatrix(4)) = grisInitialized);
  refiner.RelativeTolerance := 1e-12;
  FreeAndNil(fSolver);

  fSolver := TGaussRefineSolverF64.Create(refiner);

  CheckTrue(fSolver.Initialized);
  CheckEquals(1e-12, refiner.RelativeTolerance, 1e-27);
  CheckTrue(fSolver.Solve(TArray<Double>.Create(12), X) = lsssSolved);
  CheckEquals([3], X, 0);
end;

procedure TRefineSolverTests.ResetReleasesStorage;
var wrapper: TGaussRefineSolverF64;
    backend: TLUPDecompositionF64;
    X: TArray<Double>;
begin
  wrapper := TGaussRefineSolverF64(fSolver);
  backend := TLUPDecompositionF64(wrapper.Refiner.Solver);
  backend.PivotTolerance := 1e-10;
  wrapper.Refiner.MaxIterations := 7;
  wrapper.AcceptNonConvergedSolution := True;
  CheckTrue(fSolver.InitializeWithoutCopy(Matrix) = lsisInitialized);
  CheckTrue(fSolver.Solve(TArray<Double>.Create(8, 12), X) = lsssSolved);

  fSolver.Reset;

  CheckFalse(wrapper.Refiner.Initialized);
  CheckFalse(backend.Initialized);
  CheckEquals(0, Length(TLUPAccess(backend).fDyn));
  CheckTrue(TLUPAccess(backend).fND = nil);
  CheckEquals(0, Length(TRefinerAccess(wrapper.Refiner).fA128));
  CheckEquals(0, Length(TRefinerAccess(wrapper.Refiner).fResidual));
  CheckEquals(0, Length(wrapper.LastResults));
  CheckEquals(7, wrapper.Refiner.MaxIterations);
  CheckEquals(1e-10, backend.PivotTolerance, 1e-25);
  CheckTrue(wrapper.AcceptNonConvergedSolution);
  CheckEquals([1, 2], X, 1e-12);
end;

procedure TRefineSolverTests.FailedReplacementReleasesBackend;
var wrapper: TGaussRefineSolverF64;
begin
  wrapper := TGaussRefineSolverF64(fSolver);
  CheckTrue(fSolver.InitializeWithoutCopy(TNDAUt.AsArray<Double>(Matrix)) = lsisInitialized);

  CheckTrue(fSolver.Initialize(ScalarMatrix(NaN)) = lsisInvalidInput);

  CheckFalse(fSolver.Initialized);
  CheckFalse(wrapper.Refiner.Solver.Initialized);
  CheckTrue(TLUPAccess(wrapper.Refiner.Solver).fND = nil);
  CheckEquals(0, Length(TRefinerAccess(wrapper.Refiner).fA128));
end;

procedure TRefineSolverTests.ExceptionalReplacement;
var backend: TTrackingBackend;
    wrapper: TGaussRefineSolverF64;
    raised: Boolean;
begin
  backend := TTrackingBackend.Create;
  FreeAndNil(fSolver);
  wrapper := TGaussRefineSolverF64.Create(TGaussRefineF64.Create(backend));
  fSolver := wrapper;
  CheckTrue(fSolver.Initialize(Matrix) = lsisInitialized);
  backend.RaiseInitialization := True;
  raised := False;

  try
    fSolver.Initialize(Matrix);
  except
    on E: EAbort do raised := True;
  end;

  CheckTrue(raised);
  CheckFalse(fSolver.Initialized);
  CheckFalse(backend.Initialized);
  CheckEquals(0, Length(TLUPAccess(backend).fDyn));
  CheckEquals(0, Length(TRefinerAccess(wrapper.Refiner).fA128));
  backend.RaiseInitialization := False;
  CheckTrue(fSolver.Initialize(Matrix) = lsisInitialized);
end;

procedure TRefineSolverTests.CopyIndependent;
var A: TArray<TArray<Double>>;
    X: TArray<Double>;
begin
  A := Matrix;

  CheckTrue(fSolver.Initialize(A) = lsisInitialized);

  CheckEquals(4, A[0, 0], 0);
  CheckEquals(2, A[1, 0], 0);
  A[0, 0] := 99;
  A := nil;
  CheckTrue(fSolver.Solve(TArray<Double>.Create(8, 12), X) = lsssSolved);
  CheckEquals([1, 2], X, 1e-12);
end;

procedure TRefineSolverTests.BorrowedDynamicFactors;
var A: TArray<TArray<Double>>;
    X: TArray<Double>;
begin
  A := Matrix;

  CheckTrue(fSolver.InitializeWithoutCopy(A) = lsisInitialized);

  CheckEquals(0.5, A[1, 0], 0);
  CheckEquals(4, A[1, 1], 0);
  A := nil;
  CheckTrue(fSolver.Solve(TArray<Double>.Create(8, 12), X) = lsssSolved);
  CheckEquals([1, 2], X, 1e-12);
end;

procedure TRefineSolverTests.BorrowedOffsetNegativeRows;
var M, V: INDArray<Double>;
    X: TArray<Double>;
begin
  M := TNDAUt.AsArray<Double>([[99, 2, 5, 99], [99, 4, 2, 99]]);
  V := M[[NDISpan(1, 0, -1), NDISpan(1, 2)]];

  CheckTrue(fSolver.InitializeWithoutCopy(V) = lsisInitialized);

  CheckEquals(0.5, M.GetItem([0, 1]), 0);
  CheckEquals(4, M.GetItem([0, 2]), 0);
  CheckEquals(99, M.GetItem([0, 0]), 0);
  M := nil;
  V := nil;
  CheckTrue(fSolver.Solve(TArray<Double>.Create(8, 12), X) = lsssSolved);
  CheckEquals([1, 2], X, 1e-12);
end;

procedure TRefineSolverTests.BorrowedPivotedRows;
var A: TArray<TArray<Double>>;
    X: TArray<Double>;
begin
  A := TArray<TArray<Double>>.Create(TArray<Double>.Create(0, 2),
    TArray<Double>.Create(4, 3));

  CheckTrue(fSolver.InitializeWithoutCopy(A) = lsisInitialized);

  CheckEquals(4, A[0, 0], 0);
  CheckTrue(fSolver.Solve(TArray<Double>.Create(4, 10), X) = lsssSolved);
  CheckEquals([1, 2], X, 0);
end;

procedure TRefineSolverTests.BorrowedCholeskyFactors;
var A: INDArray<Double>;
    X: TArray<Double>;
begin
  FreeAndNil(fSolver);
  fSolver := TGaussRefineSolverF64.Create(
    TGaussRefineF64.Create(TCholeskyDecompositionF64.Create));
  A := TNDAUt.AsArray<Double>(Matrix);

  CheckTrue(fSolver.InitializeWithoutCopy(A) = lsisInitialized);

  CheckEquals(2, A.GetItem([0, 0]), 0);
  CheckEquals(1, A.GetItem([1, 0]), 0);
  CheckEquals(2, A.GetItem([0, 1]), 0);
  CheckTrue(fSolver.Solve(TArray<Double>.Create(8, 12), X) = lsssSolved);
  CheckEquals([1, 2], X, 0);
end;

procedure TRefineSolverTests.CopiedStridedReadOnly;
var M, V: INDArray<Double>;
    X: TArray<Double>;
begin
  M := TNDAUt.AsArray<Double>([[5, 99, 2], [2, 99, 4]]);
  V := M[[NDISpan(1, 0, -1), NDISpan(2, 0, -2)]];
  V.UnsetFlags(NDAF_WRITEABLE);

  CheckTrue(fSolver.Initialize(V) = lsisInitialized);

  CheckEquals(5, M.GetItem([0, 0]), 0);
  CheckEquals(4, M.GetItem([1, 2]), 0);
  CheckTrue(fSolver.Solve(TArray<Double>.Create(8, 12), X) = lsssSolved);
  CheckEquals([1, 2], X, 1e-12);
end;

procedure TRefineSolverTests.RejectedColumnStride;
var M, V: INDArray<Double>;
begin
  M := TNDAUt.AsArray<Double>([[4, 99, 2], [2, 99, 5]]);
  V := M[[NDIAll, NDISpan(0, 2, 2)]];

  CheckTrue(fSolver.InitializeWithoutCopy(V) = lsisInvalidInput);

  CheckFalse(fSolver.Initialized);
  CheckEquals(4, M.GetItem([0, 0]), 0);
  CheckEquals(5, M.GetItem([1, 2]), 0);
end;

procedure TRefineSolverTests.RejectedReadOnly;
var A: INDArray<Double>;
begin
  A := TNDAUt.AsArray<Double>(Matrix);
  A.UnsetFlags(NDAF_WRITEABLE);

  CheckTrue(fSolver.InitializeWithoutCopy(A) = lsisInvalidInput);

  CheckFalse(fSolver.Initialized);
  CheckEquals(4, A.GetItem([0, 0]), 0);
end;

procedure TRefineSolverTests.StridedVectorRHS;
var M, B, X: INDArray<Double>;
begin
  CheckTrue(fSolver.Initialize(Matrix) = lsisInitialized);
  M := TNDAUt.AsArray<Double>([12, 99, 8]);
  B := M[[NDISpan(2, 0, -2)]];
  B.UnsetFlags(NDAF_WRITEABLE);

  CheckTrue(fSolver.Solve(B, X) = lsssSolved);

  CheckEquals(1, X.NDim);
  CheckEquals(1, X.GetItem([0]), 1e-12);
  CheckEquals(2, X.GetItem([1]), 1e-12);
  CheckEquals(12, M.GetItem([0]), 0);
end;

procedure TRefineSolverTests.RejectedOverlappingRows;
var M, V: INDArray<Double>;
begin
  M := TNDAUt.AsArray<Double>([4, 2, 5]);
  V := TNDArray<Double>.Create(M.Data, TArray<NativeInt>.Create(2, 2),
    TArray<NativeInt>.Create(SizeOf(Double), SizeOf(Double)));
  V.SetFlags(NDAF_WRITEABLE);

  CheckTrue(fSolver.InitializeWithoutCopy(V) = lsisInvalidInput);

  CheckFalse(fSolver.Initialized);
  CheckEquals(4, M.GetItem([0]), 0);
  CheckEquals(2, M.GetItem([1]), 0);
  CheckEquals(5, M.GetItem([2]), 0);
end;

procedure TRefineSolverTests.StridedMatrixRHS;
var M, B, X: INDArray<Double>;
begin
  CheckTrue(fSolver.Initialize(Matrix) = lsisInitialized);
  M := TNDAUt.AsArray<Double>([[12, 99, 8], [8, 99, 12]]);
  B := M[[NDISpan(1, 0, -1), NDISpan(0, 2, 2)]];
  B.UnsetFlags(NDAF_WRITEABLE);

  CheckTrue(fSolver.Solve(B, X) = lsssSolved);

  CheckEquals(1, X.GetItem([0, 0]), 1e-12);
  CheckEquals(2, X.GetItem([1, 0]), 1e-12);
  CheckEquals(2.75, X.GetItem([0, 1]), 1e-12);
  CheckEquals(0.5, X.GetItem([1, 1]), 1e-12);
  CheckEquals(2, Length(TGaussRefineSolverF64(fSolver).LastResults));
end;

procedure TRefineSolverTests.CheckNonconvergence(aStagnate, aAccept: Boolean);
var wrapper: TGaussRefineSolverF64;
    X: TArray<Double>;
    status: TLinearSolverSolveStatus;
begin
  wrapper := TGaussRefineSolverF64(fSolver);
  wrapper.AcceptNonConvergedSolution := aAccept;
  if aStagnate then
    wrapper.Refiner.RelativeTolerance := 1e-30
  else
    wrapper.Refiner.MaxIterations := 0;
  CheckTrue(fSolver.Initialize(ScalarMatrix(3)) = lsisInitialized);
  X := TArray<Double>.Create(99);

  status := fSolver.Solve(TArray<Double>.Create(1), X);

  CheckEquals(1, Length(wrapper.LastResults));
  if aStagnate then
    CheckTrue(wrapper.LastResults[0].Status = grsStagnated)
  else
    CheckTrue(wrapper.LastResults[0].Status = grsIterationLimit);
  if aAccept then begin
    CheckTrue(status = lsssSolved);
    CheckEquals([1 / 3], X, 0);
  end else begin
    CheckTrue(status = lsssNotConverged);
    CheckEquals(0, Length(X));
  end;
  CheckTrue(fSolver.Initialized);

  CheckTrue(fSolver.Solve(TArray<Double>.Create(3), X) = lsssSolved);

  CheckEquals([1], X, 0);
end;

procedure TRefineSolverTests.IterationLimitRejected;
begin
  CheckNonconvergence(False, False);
end;

procedure TRefineSolverTests.IterationLimitAccepted;
begin
  CheckNonconvergence(False, True);
end;

procedure TRefineSolverTests.StagnationRejected;
begin
  CheckNonconvergence(True, False);
end;

procedure TRefineSolverTests.StagnationAccepted;
begin
  CheckNonconvergence(True, True);
end;

procedure TRefineSolverTests.NumericalFailureWithAcceptance;
var backend: TTrackingBackend;
    wrapper: TGaussRefineSolverF64;
    X: TArray<Double>;
begin
  backend := TTrackingBackend.Create;
  backend.FailOnSolve := 2;
  backend.Behavior := bbException;
  FreeAndNil(fSolver);
  wrapper := TGaussRefineSolverF64.Create(TGaussRefineF64.Create(backend));
  fSolver := wrapper;
  wrapper.AcceptNonConvergedSolution := True;
  CheckTrue(fSolver.Initialize(ScalarMatrix(3)) = lsisInitialized);

  CheckTrue(fSolver.Solve(TArray<Double>.Create(1), X) = lsssNumericalFailure);

  CheckEquals(0, Length(X));
  CheckTrue(wrapper.LastResults[0].Status = grsNumericalFailure);
  CheckTrue(fSolver.Initialized);
  CheckTrue(fSolver.Solve(TArray<Double>.Create(3), X) = lsssSolved);
  CheckEquals([1], X, 0);
end;

procedure TRefineSolverTests.LaterColumnFailure;
var backend: TTrackingBackend;
    wrapper: TGaussRefineSolverF64;
    B, X: INDArray<Double>;
begin
  backend := TTrackingBackend.Create;
  backend.FailOnSolve := 2;
  backend.Behavior := bbNonfinite;
  FreeAndNil(fSolver);
  wrapper := TGaussRefineSolverF64.Create(TGaussRefineF64.Create(backend));
  fSolver := wrapper;
  CheckTrue(fSolver.Initialize(ScalarMatrix(4)) = lsisInitialized);
  B := TNDAUt.AsArray<Double>([[4, 8, 12]]);
  X := TNDAUt.AsArray<Double>([99]);

  CheckTrue(fSolver.Solve(B, X) = lsssNumericalFailure);

  CheckTrue(X = nil);
  CheckEquals(2, backend.SolveCount);
  CheckEquals(1, backend.InitializeCount);
  CheckEquals(2, Length(wrapper.LastResults));
  CheckTrue(wrapper.LastResults[0].Status = grsConverged);
  CheckTrue(wrapper.LastResults[1].Status = grsNumericalFailure);
  CheckTrue(fSolver.Initialized);
end;

procedure TRefineSolverTests.MatrixMatchesVectorSolves;
var backend: TTrackingBackend;
    B, X: INDArray<Double>;
    first, second: TArray<Double>;
begin
  backend := TTrackingBackend.Create;
  FreeAndNil(fSolver);
  fSolver := TGaussRefineSolverF64.Create(TGaussRefineF64.Create(backend));
  CheckTrue(fSolver.Initialize(Matrix) = lsisInitialized);
  B := TNDAUt.AsArray<Double>([[8, 12], [12, 8]]);

  CheckTrue(fSolver.Solve(B, X) = lsssSolved);
  CheckTrue(fSolver.Solve(TArray<Double>.Create(8, 12), first) = lsssSolved);
  CheckTrue(fSolver.Solve(TArray<Double>.Create(12, 8), second) = lsssSolved);

  CheckEquals(first[0], X.GetItem([0, 0]), 0);
  CheckEquals(first[1], X.GetItem([1, 0]), 0);
  CheckEquals(second[0], X.GetItem([0, 1]), 0);
  CheckEquals(second[1], X.GetItem([1, 1]), 0);
  CheckEquals(1, backend.InitializeCount);
  CheckEquals(4, backend.SolveCount);
end;

procedure TRefineSolverTests.MatrixNonconvergenceRejected;
var wrapper: TGaussRefineSolverF64;
    X: INDArray<Double>;
begin
  wrapper := TGaussRefineSolverF64(fSolver);
  wrapper.Refiner.MaxIterations := 0;
  CheckTrue(fSolver.Initialize(ScalarMatrix(3)) = lsisInitialized);

  CheckTrue(fSolver.Solve(TNDAUt.AsArray<Double>([[3, 1, 6]]), X) = lsssNotConverged);

  CheckTrue(X = nil);
  CheckEquals(2, Length(wrapper.LastResults));
  CheckTrue(wrapper.LastResults[0].Status = grsConverged);
  CheckTrue(wrapper.LastResults[1].Status = grsIterationLimit);
  CheckTrue(fSolver.Initialized);
end;

procedure TRefineSolverTests.MatrixNonconvergenceAccepted;
var wrapper: TGaussRefineSolverF64;
    X: INDArray<Double>;
begin
  wrapper := TGaussRefineSolverF64(fSolver);
  wrapper.Refiner.MaxIterations := 0;
  wrapper.AcceptNonConvergedSolution := True;
  CheckTrue(fSolver.Initialize(ScalarMatrix(3)) = lsisInitialized);

  CheckTrue(fSolver.Solve(TNDAUt.AsArray<Double>([[3, 1, 6]]), X) = lsssSolved);

  CheckEquals(3, Length(wrapper.LastResults));
  CheckTrue(wrapper.LastResults[1].Status = grsIterationLimit);
  CheckEquals(1, X.GetItem([0, 0]), 0);
  CheckEquals(1 / 3, X.GetItem([0, 1]), 1e-16);
  CheckEquals(2, X.GetItem([0, 2]), 0);
end;

procedure TRefineSolverTests.DiagnosticsIndependentAndReplaced;
var wrapper: TGaussRefineSolverF64;
    results: TArray<TGaussRefinementResult>;
    X: TArray<Double>;
begin
  wrapper := TGaussRefineSolverF64(fSolver);
  CheckTrue(fSolver.Initialize(ScalarMatrix(4)) = lsisInitialized);
  CheckTrue(fSolver.Solve(TArray<Double>.Create(8), X) = lsssSolved);
  results := wrapper.LastResults;

  results[0].Status := grsNumericalFailure;

  CheckTrue(wrapper.LastResults[0].Status = grsConverged);
  CheckTrue(fSolver.Solve(TArray<Double>(nil), X) = lsssInvalidInput);
  CheckEquals(0, Length(wrapper.LastResults));

  CheckTrue(fSolver.Solve(TArray<Double>.Create(8), X) = lsssSolved);
  CheckTrue(fSolver.Initialize(ScalarMatrix(4)) = lsisInitialized);

  CheckEquals(0, Length(wrapper.LastResults));
  CheckEquals(1, Length(results));
end;

procedure TRefineSolverTests.InvalidOptionsNoDiagnostics;
var wrapper: TGaussRefineSolverF64;
    X: TArray<Double>;
begin
  wrapper := TGaussRefineSolverF64(fSolver);
  CheckTrue(fSolver.Initialize(ScalarMatrix(4)) = lsisInitialized);
  wrapper.Refiner.MaxIterations := -1;

  CheckTrue(fSolver.Solve(TArray<Double>.Create(8), X) = lsssInvalidInput);

  CheckEquals(0, Length(wrapper.LastResults));
  CheckEquals(0, Length(X));
  CheckTrue(fSolver.Initialized);
end;

procedure TRefineSolverTests.NonfiniteRHSNoDiagnostics;
var X: INDArray<Double>;
begin
  CheckTrue(fSolver.Initialize(ScalarMatrix(4)) = lsisInitialized);

  CheckTrue(fSolver.Solve(TNDAUt.AsArray<Double>([[4, NaN]]), X) = lsssInvalidInput);

  CheckTrue(X = nil);
  CheckEquals(0, Length(TGaussRefineSolverF64(fSolver).LastResults));
  CheckTrue(fSolver.Initialized);
end;

procedure TRefineSolverTests.SingularInitialization;
begin
  CheckTrue(fSolver.Initialize(ScalarMatrix(0)) = lsisSingular);

  CheckFalse(fSolver.Initialized);
end;

procedure TRefineSolverTests.NotPositiveDefiniteInitialization;
begin
  FreeAndNil(fSolver);
  fSolver := TGaussRefineSolverF64.Create(
    TGaussRefineF64.Create(TCholeskyDecompositionF64.Create));

  CheckTrue(fSolver.Initialize(ScalarMatrix(-1)) = lsisNotPositiveDefinite);

  CheckFalse(fSolver.Initialized);
end;

procedure TRefineSolverTests.NumericalInitializationFailure;
var backend: TTrackingBackend;
begin
  backend := TTrackingBackend.Create;
  backend.NonfiniteFactors := True;
  FreeAndNil(fSolver);
  fSolver := TGaussRefineSolverF64.Create(TGaussRefineF64.Create(backend));

  CheckTrue(fSolver.Initialize(ScalarMatrix(3)) = lsisNumericalFailure);

  CheckFalse(fSolver.Initialized);
  CheckFalse(backend.Initialized);
  CheckEquals(0, Length(TLUPAccess(backend).fDyn));
end;

initialization
  RegisterTest(TLinearSolverTests.Suite);
  RegisterTest(TCholeskySolverTests.Suite);
  RegisterTest(TRefineBackendTests.Suite);
  RegisterTest(TRefineSolverTests.Suite);

end.
