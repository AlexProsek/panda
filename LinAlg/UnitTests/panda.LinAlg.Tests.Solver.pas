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
  TBackendBehavior = (bbStatus, bbEmpty, bbWrongLength, bbNonfinite, bbException);

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
  end;

  TCholeskySolverTests = class(TLinearSolverTests)
  protected
    function CreateSolver: TLinearSolver<Double>; override;
  end;

  TRefineBackendTests = class(TNDATestCase)
  protected
    procedure CheckInitialFailure(aBehavior: TBackendBehavior);
    procedure CheckCorrectionFailure(aBehavior: TBackendBehavior);
    procedure CheckInitFailure(aStatus: TLinearSolverInitStatus; aExpected: TGaussRefineInitStatus);
  published
    procedure DefaultBackend;
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
    procedure InitialEmptyResult;
    procedure InitialWrongLength;
    procedure InitialNonfiniteResult;
    procedure InitialArithmeticException;
    procedure CorrectionStatusFailure;
    procedure CorrectionEmptyResult;
    procedure CorrectionWrongLength;
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
    destructor Destroy; override;
    function InitializeWithoutCopy(const aA: TArray<TArray<Double>>): TLinearSolverInitStatus; overload; override;
    function Solve(const ab: TArray<Double>; out aRes: TArray<Double>): TLinearSolverSolveStatus; overload; override;
  end;

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
    bbEmpty: ;
    bbWrongLength: SetLength(aRes, Length(ab) + 1);
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

procedure TRefineBackendTests.DefaultBackend;
var refine: TGaussRefineF64;
begin
  refine := TGaussRefineF64.Create;
  try
    CheckTrue(refine.Solver is TLUPDecompositionF64);
    CheckEquals(10, refine.MaxIterations);
    CheckFalse(refine.Initialized);
  finally
    refine.Free;
  end;
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

procedure TRefineBackendTests.InitialEmptyResult;
begin
  CheckInitialFailure(bbEmpty);
end;

procedure TRefineBackendTests.InitialWrongLength;
begin
  CheckInitialFailure(bbWrongLength);
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

procedure TRefineBackendTests.CorrectionEmptyResult;
begin
  CheckCorrectionFailure(bbEmpty);
end;

procedure TRefineBackendTests.CorrectionWrongLength;
begin
  CheckCorrectionFailure(bbWrongLength);
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

initialization
  RegisterTest(TLinearSolverTests.Suite);
  RegisterTest(TCholeskySolverTests.Suite);
  RegisterTest(TRefineBackendTests.Suite);

end.
