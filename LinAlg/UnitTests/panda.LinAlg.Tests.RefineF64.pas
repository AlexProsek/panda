unit panda.LinAlg.Tests.RefineF64;

interface

uses
    panda.LinAlg.Base
  , TestFramework
  , panda.Intfs
  , panda.Nums
  , panda.NumsQP
  , panda.Arrays
  , panda.Arithmetic
  , panda.ArrManip
  , panda.DynArrayUtils
  , panda.LinAlg.LUPF64
  , panda.LinAlg.LUP
  , panda.LinAlg.LUPF128
  , panda.LinAlg.RefineF64
  , panda.Tests.NDATestCase
  ;

type
  TGaussRefineF64Tests = class(TNDATestCase)
  protected
    solver: TGaussRefineF64;
    function ScalarMatrix(aValue: Double): TDoubleArray2D;
    procedure CheckStatus(aExpected: TGaussRefinementStatus;
      const aResult: TGaussRefinementResult);
    procedure CheckInitStatus(aExpected, aActual: TGaussRefineInitStatus);
    procedure CheckUnavailable(const aResult: TGaussRefinementResult;
      const aX: TDoubleArray);
    function StridedMatrix(const aA: TDoubleArray2D): INDArray<Double>;
    function StridedVector(const aV: TDoubleArray): INDArray<Double>;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure InvalidBlockedSettings;
    procedure FactorizationSettingsPreserveFactors;
    procedure SingleEquation;
    procedure ExactSolution;
    procedure ZeroRHS;
    procedure ZeroSolutionComponent;
    procedure ImprovesIllConditionedSolution;
    procedure HighPrecisionProduct;
    procedure HighPrecisionAccumulation;
    procedure SmallBackwardErrorStillRefines;
    procedure ZeroIterationLimit;
    procedure CorrectionIterationLimit;
    procedure SettingsAppliedToRepeatedCalls;
    procedure SettingsAppliedND;
    procedure UnchangedSolution;
    procedure ResidualUnderflow;
    procedure InitialSolveOverflow;
    procedure SolveFailurePreservesInitialization;
    procedure FactorizationOverflow;
    procedure PivotAndFactorReuse;
    procedure RepeatedRefinedSolves;
    procedure BandedSolve;
    procedure PivotSettingsAppliedAtInitialize;
    procedure FactorSettingsAppliedAtInitialize;
    procedure BandSettingsAppliedAtInitialize;
    procedure DynamicOwnership;
    procedure SolveNDStrided;
    procedure SolveNDNegativeStrides;
    procedure RepeatedNDSolves;
    procedure RejectNDDimensions;
    procedure RejectDynamicDimensions;
    procedure RejectNonfiniteMatrix;
    procedure RejectNonfiniteRHS;
    procedure RejectOptions;
    procedure SingularSystem;
    procedure SolveBeforeInitialize;
    procedure SolveNDBeforeInitialize;
    procedure Reinitialize;
    procedure FailedReinitialize;
    procedure MatrixMutationAfterInitialize;
    procedure NDMatrixMutationAfterInitialize;
    procedure SolveFailureResetsDiagnostics;
  end;

type
  TBlockedRefineF64Tests = class(TGaussRefineF64Tests)
  public
    procedure SetUp; override;
  published
    procedure BlockedRefinementAccuracy;
  end;

implementation

uses
    System.SysUtils
  , System.Math
  ;

procedure TGaussRefineF64Tests.SetUp;
begin
  solver := TGaussRefineF64.Create;
end;

procedure TGaussRefineF64Tests.TearDown;
begin
  solver.Free;
end;

function TGaussRefineF64Tests.ScalarMatrix(aValue: Double): TDoubleArray2D;
begin
  Result := TDoubleArray2D.Create(TDoubleArray.Create(aValue));
end;

procedure TGaussRefineF64Tests.CheckStatus(aExpected: TGaussRefinementStatus;
  const aResult: TGaussRefinementResult);
begin
  CheckEquals(Ord(aExpected), Ord(aResult.Status));
end;

procedure TGaussRefineF64Tests.CheckUnavailable(const aResult: TGaussRefinementResult;
  const aX: TDoubleArray);
begin
  CheckEquals(0, Length(aX));
  CheckEquals(0, aResult.Iterations);
  CheckTrue(IsNan(aResult.BackwardError));
end;

function TGaussRefineF64Tests.StridedMatrix(const aA: TDoubleArray2D): INDArray<Double>;
var padded: TDoubleArray2D;
    I, J, N: Integer;
begin
  N := Length(aA);
  SetLength(padded, N * 2, N * 2);
  for I := 0 to N - 1 do
    for J := 0 to N - 1 do
      padded[I * 2, J * 2] := aA[I, J];
  Result := TNDAUt.AsArray<Double>(padded);
  Result := Result[[NDISpan(0, -1, 2), NDISpan(0, -1, 2)]];
end;

function TGaussRefineF64Tests.StridedVector(const aV: TDoubleArray): INDArray<Double>;
var padded: TDoubleArray;
    I: Integer;
begin
  SetLength(padded, Length(aV) * 2);
  for I := 0 to High(aV) do padded[I * 2] := aV[I];
  Result := TNDAUt.AsArray<Double>(padded);
  Result := Result[[NDISpan(0, -1, 2)]];
end;

procedure TGaussRefineF64Tests.SingleEquation;
var X: TDoubleArray;
    info: TGaussRefinementResult;
begin
  CheckInitStatus(grisInitialized, solver.Initialize(ScalarMatrix(2)));

  info := solver.Solve(TDoubleArray.Create(6), X);

  CheckStatus(grsConverged, info);
  CheckEquals([3], X, 0);
  CheckEquals(0, info.Iterations);
  CheckEquals(0, info.BackwardError, 0);
end;

procedure TGaussRefineF64Tests.ExactSolution;
var A: TDoubleArray2D;
    X: TDoubleArray;
    info: TGaussRefinementResult;
begin
  A := TDoubleArray2D.Create(TDoubleArray.Create(2, 1), TDoubleArray.Create(0, 4));

  CheckInitStatus(grisInitialized, solver.Initialize(A));


  info := solver.Solve(TDoubleArray.Create(4, 8), X);

  CheckStatus(grsConverged, info);
  CheckEquals([1, 2], X, 0);
  CheckEquals(0, info.Iterations);
end;

procedure TGaussRefineF64Tests.ZeroRHS;
var X: TDoubleArray;
    info: TGaussRefinementResult;
begin
  CheckInitStatus(grisInitialized, solver.Initialize(ScalarMatrix(2)));

  info := solver.Solve(TDoubleArray.Create(0), X);

  CheckStatus(grsConverged, info);
  CheckEquals([0], X, 0);
  CheckEquals(0, info.BackwardError, 0);
end;

procedure TGaussRefineF64Tests.ZeroSolutionComponent;
var A: TDoubleArray2D;
    X: TDoubleArray;
    info: TGaussRefinementResult;
begin
  A := TDoubleArray2D.Create(TDoubleArray.Create(2, 0), TDoubleArray.Create(0, 4));

  CheckInitStatus(grisInitialized, solver.Initialize(A));


  info := solver.Solve(TDoubleArray.Create(0, 8), X);

  CheckStatus(grsConverged, info);
  CheckEquals([0, 2], X, 0);
  CheckEquals(0, info.BackwardError, 0);
end;

procedure TGaussRefineF64Tests.ImprovesIllConditionedSolution;
const N = 5;
var A, LU: TDoubleArray2D;
    B, initialX, X: TDoubleArray;
    QA: TArray<TArray<TReal128>>;
    QB, QX: TArray<TReal128>;
    quad: TLUPDecompositionF128;
    ordinary: TLUPDecompositionF64;
    I, J: Integer;
    initialError, refinedError, e, sum, denom, berr, ratio, refResidual: TReal128;
    info: TGaussRefinementResult;
begin
  SetLength(A, N, N);
  SetLength(B, N);
  SetLength(QA, N, N);
  SetLength(QB, N);
  for I := 0 to N - 1 do begin
    B[I] := 1;
    QB[I] := B[I];
    for J := 0 to N - 1 do
      A[I, J] := 1 / (I + J + 1);
  end;
  // Convert the stored Double coefficients, after construction has completed.
  for I := 0 to N - 1 do
    for J := 0 to N - 1 do
      QA[I, J] := A[I, J];
  LU := TDynAUt.Clone<Double>(A);
  quad := TLUPDecompositionF128.Create;
  ordinary := TLUPDecompositionF64.Create;
  try
    CheckTrue((ordinary.InitializeWithoutCopy(LU) = lsisInitialized), 'Double factorization');
    ordinary.Solve(B, initialX);
    CheckTrue((quad.InitializeWithoutCopy(QA) = lsisInitialized), 'TReal128 factorization');
    quad.Solve(QB, QX);

    refResidual := cZeroF128;
    for I := 0 to N - 1 do begin
      sum := cZeroF128;
      for J := 0 to N - 1 do
        sum := sum + TReal128(A[I, J]) * QX[J];
      e := (QB[I] - sum).Abs;
      if e > refResidual then refResidual := e;
    end;
    CheckTrue(refResidual < TReal128(1e-25),
      Format('Reference residual %.17g', [Double(refResidual)]));

    CheckInitStatus(grisInitialized, solver.Initialize(A));


    info := solver.Solve(B, X);

    CheckStatus(grsConverged, info);
    CheckTrue(info.Iterations > 0, 'At least one correction');
    initialError := cZeroF128;
    refinedError := cZeroF128;
    berr := cZeroF128;
    for I := 0 to N - 1 do begin
      e := (TReal128(initialX[I]) - QX[I]).Abs;
      if e > initialError then initialError := e;
      e := (TReal128(X[I]) - QX[I]).Abs;
      if e > refinedError then refinedError := e;

      // Independent dot-product accumulation followed by RHS subtraction.
      sum := cZeroF128;
      denom := TReal128(B[I]).Abs;
      for J := 0 to N - 1 do begin
        e := TReal128(A[I, J]) * TReal128(X[J]);
        sum := sum + e;
        denom := denom + e.Abs;
      end;
      ratio := (QB[I] - sum).Abs / denom;
      if ratio > berr then berr := ratio;
    end;
    CheckTrue(refinedError < initialError * TReal128(0.01),
      Format('Initial error %.17g, refined error %.17g',
        [Double(initialError), Double(refinedError)]));
    CheckEquals(Double(berr), info.BackwardError, 1e-30);
  finally
    ordinary.Free;
    quad.Free;
  end;
end;

procedure TGaussRefineF64Tests.ZeroIterationLimit;
var X: TDoubleArray;
    info: TGaussRefinementResult;
begin
  solver.MaxIterations := 0;

  CheckInitStatus(grisInitialized, solver.Initialize(ScalarMatrix(3)));


  info := solver.Solve(TDoubleArray.Create(1), X);

  CheckStatus(grsIterationLimit, info);
  CheckEquals(0, info.Iterations);
  CheckEquals([1/3], X, 0);
  CheckTrue(info.BackwardError > 0);
end;

procedure TGaussRefineF64Tests.SolveNDStrided;
var A, B, X, savedA, savedB: INDArray<Double>;
    info: TGaussRefinementResult;
begin
  A := StridedMatrix(TDoubleArray2D.Create(
    TDoubleArray.Create(0, 2),
    TDoubleArray.Create(4, 1))
  );
  B := StridedVector(TDoubleArray.Create(4, 6));
  savedA := TNDAUt.Copy<Double>(A);
  savedB := TNDAUt.Copy<Double>(B);
  A.UnsetFlags(NDAF_WRITEABLE);
  B.UnsetFlags(NDAF_WRITEABLE);

  CheckInitStatus(grisInitialized, solver.Initialize(A));


  info := solver.Solve(B, X);

  CheckStatus(grsConverged, info);
  CheckEquals(1, X.NDim);
  CheckTrue(ndaAllClose(X, TNDAUt.AsArray<Double>(TDoubleArray.Create(1, 2)), 0));
  CheckTrue(ndaAllClose(A, savedA, 0));
  CheckTrue(ndaAllClose(B, savedB, 0));
  CheckTrue(X.Data <> B.Data);
end;

procedure TGaussRefineF64Tests.SolveNDNegativeStrides;
var A, B, X: INDArray<Double>;
    info: TGaussRefinementResult;
begin
  A := StridedMatrix(TDoubleArray2D.Create(
    TDoubleArray.Create(0, 2),
    TDoubleArray.Create(4, 1))
  );
  B := StridedVector(TDoubleArray.Create(4, 6));
  A := A[[NDISpan(-1, 0, -1), NDISpan(-1, 0, -1)]];
  B := B[[NDISpan(-1, 0, -1)]];

  CheckInitStatus(grisInitialized, solver.Initialize(A));


  info := solver.Solve(B, X);

  CheckStatus(grsConverged, info);
  CheckTrue(A.Strides[0] < 0);
  CheckTrue(A.Strides[1] < 0);
  CheckTrue(B.Strides[0] < 0);
  CheckTrue(ndaAllClose(X, TNDAUt.AsArray<Double>(TDoubleArray.Create(2, 1)), 0));
end;

procedure TGaussRefineF64Tests.CheckInitStatus(aExpected, aActual: TGaussRefineInitStatus);
begin
  CheckEquals(Ord(aExpected), Ord(aActual));
end;

procedure TGaussRefineF64Tests.HighPrecisionProduct;
var A: TDoubleArray2D;
    X: TDoubleArray;
    t, expected: TReal128;
    info: TGaussRefinementResult;
begin
  A := ScalarMatrix(1 + Power(2, -27));
  t := Power(2, -54);
  expected := t / (TReal128(2) - t);
  solver.MaxIterations := 0;
  CheckInitStatus(grisInitialized, solver.Initialize(A));

  info := solver.Solve(TDoubleArray.Create(1), X);

  CheckStatus(grsIterationLimit, info);
  CheckEquals([1 - Power(2, -27)], X, 0);
  CheckEquals(Double(expected), info.BackwardError, 1e-32);
  CheckTrue(info.BackwardError > 0);
end;

procedure TGaussRefineF64Tests.HighPrecisionAccumulation;
var A: TDoubleArray2D;
    X: TDoubleArray;
    info: TGaussRefinementResult;
begin
  A := TDoubleArray2D.Create(TDoubleArray.Create(1e16, 1, -1e16),
    TDoubleArray.Create(0, 1, 0), TDoubleArray.Create(0, 0, 1));
  TLUPDecompositionF64(solver.Solver).PivotTolerance := 1e-30;
  solver.MaxIterations := 0;
  CheckInitStatus(grisInitialized, solver.Initialize(A));

  info := solver.Solve(TDoubleArray.Create(0, 1, 1), X);

  CheckStatus(grsIterationLimit, info);
  CheckEquals([1, 1, 1], X, 0);
  CheckEquals(5e-17, info.BackwardError, 1e-32);
end;

procedure TGaussRefineF64Tests.SmallBackwardErrorStillRefines;
var X: TDoubleArray;
    expectedX: Double;
    prod, expected: TReal128;
    info: TGaussRefinementResult;
begin
  expectedX := Double(TReal128(1) / TReal128(3));
  prod := TReal128(3) * TReal128(expectedX);
  expected := (TReal128(1) - prod).Abs / (TReal128(1) + prod.Abs);
  CheckInitStatus(grisInitialized, solver.Initialize(ScalarMatrix(3)));

  info := solver.Solve(TDoubleArray.Create(1), X);

  CheckStatus(grsConverged, info);
  CheckEquals(1, info.Iterations);
  CheckEquals([expectedX], X, 0);
  CheckEquals(Double(expected), info.BackwardError, 0);
  CheckTrue(info.BackwardError > 0);
  CheckTrue(info.BackwardError < solver.RelativeTolerance);
end;

procedure TGaussRefineF64Tests.CorrectionIterationLimit;
var A: TDoubleArray2D;
    X: TDoubleArray;
    I, J: Integer;
    info: TGaussRefinementResult;
begin
  SetLength(A, 5, 5);
  for I := 0 to 4 do
    for J := 0 to 4 do A[I][J] := 1 / (I + J + 1);
  solver.MaxIterations := 1;
  solver.RelativeTolerance := 1e-30;
  CheckInitStatus(grisInitialized, solver.Initialize(A));

  info := solver.Solve(TDoubleArray.Create(1, 1, 1, 1, 1), X);

  CheckStatus(grsIterationLimit, info);
  CheckEquals(1, info.Iterations);
  CheckEquals(5, Length(X));
  CheckTrue(info.BackwardError > 0);
end;

procedure TGaussRefineF64Tests.SettingsAppliedToRepeatedCalls;
var X: TDoubleArray;
    info: TGaussRefinementResult;
begin
  CheckInitStatus(grisInitialized, solver.Initialize(ScalarMatrix(3)));
  solver.MaxIterations := 0;

  info := solver.Solve(TDoubleArray.Create(1), X);

  CheckStatus(grsIterationLimit, info);
  CheckEquals(0, info.Iterations);

  solver.MaxIterations := 10;
  solver.RelativeTolerance := 1e-30;

  info := solver.Solve(TDoubleArray.Create(1), X);

  CheckStatus(grsStagnated, info);
  CheckEquals(1, info.Iterations);

  solver.RelativeTolerance := cEpsF64;

  info := solver.Solve(TDoubleArray.Create(1), X);

  CheckStatus(grsConverged, info);
  CheckEquals(1, info.Iterations);
end;

procedure TGaussRefineF64Tests.SettingsAppliedND;
var A, B, X: INDArray<Double>;
    info: TGaussRefinementResult;
begin
  A := TNDAUt.AsArray<Double>(ScalarMatrix(3));
  B := TNDAUt.AsArray<Double>(TDoubleArray.Create(1));
  CheckInitStatus(grisInitialized, solver.Initialize(A));
  solver.MaxIterations := 0;

  info := solver.Solve(B, X);

  CheckStatus(grsIterationLimit, info);
  CheckEquals(0, info.Iterations);

  solver.MaxIterations := 10;
  solver.RelativeTolerance := 1e-30;

  info := solver.Solve(B, X);

  CheckStatus(grsStagnated, info);
  CheckEquals(1, info.Iterations);

  solver.RelativeTolerance := cEpsF64;

  info := solver.Solve(B, X);

  CheckStatus(grsConverged, info);
end;

procedure TGaussRefineF64Tests.UnchangedSolution;
var X: TDoubleArray;
    expectedX: Double;
    prod, expected: TReal128;
    info: TGaussRefinementResult;
begin
  expectedX := Double(TReal128(1) / TReal128(3));
  prod := TReal128(3) * TReal128(expectedX);
  expected := (TReal128(1) - prod).Abs / (TReal128(1) + prod.Abs);
  solver.RelativeTolerance := 1e-30;
  CheckInitStatus(grisInitialized, solver.Initialize(ScalarMatrix(3)));

  info := solver.Solve(TDoubleArray.Create(1), X);

  CheckStatus(grsStagnated, info);
  CheckEquals(1, info.Iterations);
  CheckEquals([expectedX], X, 0);
  CheckEquals(Double(expected), info.BackwardError, 0);
  CheckTrue(info.BackwardError > 0);
end;

procedure TGaussRefineF64Tests.ResidualUnderflow;
var X: TDoubleArray;
    info: TGaussRefinementResult;
begin
  TLUPDecompositionF64(solver.Solver).PivotTolerance := 1e-310;
  CheckInitStatus(grisInitialized, solver.Initialize(ScalarMatrix(1e-300)));

  info := solver.Solve(TDoubleArray.Create(1e-310), X);

  CheckStatus(grsStagnated, info);
  CheckEquals(0, info.Iterations);
  CheckEquals(1e-10, X[0], 1e-20);
  CheckTrue(info.BackwardError > 0);
end;

procedure TGaussRefineF64Tests.InitialSolveOverflow;
var X: TDoubleArray;
    info: TGaussRefinementResult;
begin
  TLUPDecompositionF64(solver.Solver).PivotTolerance := 1e-310;
  CheckInitStatus(grisInitialized, solver.Initialize(ScalarMatrix(1e-300)));

  info := solver.Solve(TDoubleArray.Create(1e308), X);

  CheckStatus(grsNumericalFailure, info);
  CheckUnavailable(info, X);
  CheckTrue(solver.Initialized);
end;

procedure TGaussRefineF64Tests.SolveFailurePreservesInitialization;
var X: TDoubleArray;
    info: TGaussRefinementResult;
begin
  TLUPDecompositionF64(solver.Solver).PivotTolerance := 1e-310;
  CheckInitStatus(grisInitialized, solver.Initialize(ScalarMatrix(1e-300)));
  info := solver.Solve(TDoubleArray.Create(1e308), X);
  CheckStatus(grsNumericalFailure, info);

  info := solver.Solve(TDoubleArray.Create(1e-300), X);

  CheckStatus(grsConverged, info);
  CheckEquals([1], X, 0);
  CheckEquals(0, info.Iterations);
  CheckEquals(0, info.BackwardError, 0);
end;

procedure TGaussRefineF64Tests.FactorizationOverflow;
var A: TDoubleArray2D;
    X: TDoubleArray;
begin
  CheckInitStatus(grisInitialized, solver.Initialize(ScalarMatrix(3)));
  A := TDoubleArray2D.Create(TDoubleArray.Create(1e308, 1e308),
    TDoubleArray.Create(-1e308, 1e308));

  CheckInitStatus(grisNumericalFailure, solver.Initialize(A));

  CheckFalse(solver.Initialized);
  CheckStatus(grsNotInitialized, solver.Solve(TDoubleArray.Create(1, 1), X));

  CheckInitStatus(grisInitialized, solver.Initialize(ScalarMatrix(3)));
  CheckStatus(grsConverged, solver.Solve(TDoubleArray.Create(1), X));

  CheckEquals([1/3], X, 0);
end;

procedure TGaussRefineF64Tests.PivotAndFactorReuse;
var A: TDoubleArray2D;
    X, referenceX: TDoubleArray;
    B: TDoubleArray;
    fresh: TGaussRefineF64;
    info, referenceInfo: TGaussRefinementResult;
    I: Integer;
begin
  A := TDoubleArray2D.Create(TDoubleArray.Create(0, 2), TDoubleArray.Create(4, 1));
  CheckInitStatus(grisInitialized, solver.Initialize(A));
  fresh := TGaussRefineF64.Create;
  try
    for I := 1 to 3 do begin
      B := TDoubleArray.Create(2 * I, 4 * (4 - I) + I);
      CheckInitStatus(grisInitialized, fresh.Initialize(A));

      info := solver.Solve(B, X);
      referenceInfo := fresh.Solve(B, referenceX);

      CheckStatus(grsConverged, info);
      CheckEquals(4 - I, X[0], 0);
      CheckEquals(I, X[1], 0);
      CheckStatus(referenceInfo.Status, info);
      CheckEquals(referenceX, X, 0);
      CheckEquals(referenceInfo.Iterations, info.Iterations);
      CheckEquals(referenceInfo.BackwardError, info.BackwardError, 0);
      CheckTrue(solver.Initialized);
    end;
  finally
    fresh.Free;
  end;
end;

procedure TGaussRefineF64Tests.RepeatedRefinedSolves;
var A: TDoubleArray2D;
    B, X, referenceX: TDoubleArray;
    fresh: TGaussRefineF64;
    info, referenceInfo: TGaussRefinementResult;
    I: Integer;
begin
  A := TDoubleArray2D.Create(TDoubleArray.Create(3, 1), TDoubleArray.Create(1, 7));
  CheckInitStatus(grisInitialized, solver.Initialize(A));
  fresh := TGaussRefineF64.Create;
  try
    for I := 1 to 3 do begin
      B := TDoubleArray.Create(I, 4 - I);
      CheckInitStatus(grisInitialized, fresh.Initialize(A));

      info := solver.Solve(B, X);
      referenceInfo := fresh.Solve(B, referenceX);

      CheckStatus(grsConverged, info);
      if I < 3 then
        CheckTrue(info.Iterations > 0)
      else
        CheckEquals(0, info.Iterations);
      CheckStatus(referenceInfo.Status, info);
      CheckEquals(referenceX, X, 0);
      CheckEquals(referenceInfo.Iterations, info.Iterations);
      CheckEquals(referenceInfo.BackwardError, info.BackwardError, 0);
    end;
  finally
    fresh.Free;
  end;
end;

procedure TGaussRefineF64Tests.BandedSolve;
var A: TDoubleArray2D;
    X: TDoubleArray;
    info: TGaussRefinementResult;
begin
  A := TDoubleArray2D.Create(TDoubleArray.Create(4, 1, 0),
    TDoubleArray.Create(1, 4, 1), TDoubleArray.Create(0, 1, 4));
  TLUPDecompositionF64(solver.Solver).LowerBandWidth := 1;
  TLUPDecompositionF64(solver.Solver).UpperBandWidth := 1;
  CheckInitStatus(grisInitialized, solver.Initialize(A));

  info := solver.Solve(TDoubleArray.Create(6, 12, 14), X);

  CheckStatus(grsConverged, info);
  CheckEquals([1, 2, 3], X, 0);
end;

procedure TGaussRefineF64Tests.PivotSettingsAppliedAtInitialize;
var X: TDoubleArray;
begin
  CheckInitStatus(grisInitialized, solver.Initialize(ScalarMatrix(2)));
  TLUPDecompositionF64(solver.Solver).PivotTolerance := 3;

  CheckStatus(grsConverged, solver.Solve(TDoubleArray.Create(6), X));

  CheckEquals([3], X, 0);
  CheckTrue(solver.Initialized);

  CheckInitStatus(grisSingular, solver.Initialize(ScalarMatrix(2)));

  CheckFalse(solver.Initialized);
end;

procedure TGaussRefineF64Tests.FactorSettingsAppliedAtInitialize;
var X: TDoubleArray;
begin
  CheckInitStatus(grisInitialized, solver.Initialize(ScalarMatrix(2)));
  TLUPDecompositionF64(solver.Solver).ToleranceFactor := 1e20;

  CheckStatus(grsConverged, solver.Solve(TDoubleArray.Create(6), X));

  CheckEquals([3], X, 0);

  CheckInitStatus(grisSingular, solver.Initialize(ScalarMatrix(2)));

  CheckFalse(solver.Initialized);

  TLUPDecompositionF64(solver.Solver).ToleranceFactor := -1;
  TLUPDecompositionF64(solver.Solver).PivotTolerance := -1;

  CheckInitStatus(grisInitialized, solver.Initialize(ScalarMatrix(2)));
end;

procedure TGaussRefineF64Tests.BandSettingsAppliedAtInitialize;
var A: TDoubleArray2D;
    X: TDoubleArray;
begin
  A := TDoubleArray2D.Create(TDoubleArray.Create(0, 2), TDoubleArray.Create(4, 1));
  CheckInitStatus(grisInitialized, solver.Initialize(A));
  TLUPDecompositionF64(solver.Solver).LowerBandWidth := 0;
  TLUPDecompositionF64(solver.Solver).UpperBandWidth := 0;

  CheckStatus(grsConverged, solver.Solve(TDoubleArray.Create(4, 6), X));

  CheckEquals([1, 2], X, 0);

  // A diagonal matrix satisfies the newly declared band.
  A := TDoubleArray2D.Create(TDoubleArray.Create(2, 0), TDoubleArray.Create(0, 4));

  CheckInitStatus(grisInitialized, solver.Initialize(A));
  CheckStatus(grsConverged, solver.Solve(TDoubleArray.Create(4, 8), X));

  CheckEquals([2, 2], X, 0);
end;

procedure TGaussRefineF64Tests.DynamicOwnership;
var A, savedA: TDoubleArray2D;
    B, X, X2: TDoubleArray;
    I: Integer;
begin
  A := TDoubleArray2D.Create(TDoubleArray.Create(0, 2), TDoubleArray.Create(4, 1));
  savedA := TDynAUt.Clone<Double>(A);
  B := TDoubleArray.Create(4, 6);
  CheckInitStatus(grisInitialized, solver.Initialize(A));

  CheckStatus(grsConverged, solver.Solve(B, X));
  CheckStatus(grsConverged, solver.Solve(TDoubleArray.Create(2, 9), X2));
  CheckInitStatus(grisInitialized, solver.Initialize(ScalarMatrix(3)));
  solver.Free;
  solver := nil;

  CheckEquals([1, 2], X, 0);
  CheckEquals([2, 1], X2, 0);
  X2[0] := 99;
  CheckEquals([1, 2], X, 0);
  CheckEquals([4, 6], B, 0);
  for I := 0 to High(A) do
    CheckEquals(savedA[I], A[I], 0);
end;

procedure TGaussRefineF64Tests.RepeatedNDSolves;
var A, B, X, X2: INDArray<Double>;
begin
  A := StridedMatrix(ScalarMatrix(3));
  B := StridedVector(TDoubleArray.Create(1));
  CheckInitStatus(grisInitialized, solver.Initialize(A));

  CheckStatus(grsConverged, solver.Solve(B, X));
  CheckStatus(grsConverged, solver.Solve(TNDAUt.AsArray<Double>(TDoubleArray.Create(6)), X2));
  CheckInitStatus(grisInitialized, solver.Initialize(ScalarMatrix(1)));
  solver.Free;
  solver := nil;

  CheckTrue(ndaAllClose(X, 1/3, 0));
  CheckTrue(ndaAllClose(X2, 2, 0));
  CheckTrue(X.Data <> X2.Data);
  CheckTrue(ndaAllClose(B, 1, 0));
end;

procedure TGaussRefineF64Tests.RejectNDDimensions;
var A, B, X: INDArray<Double>;
    info: TGaussRefinementResult;
begin
  A := TNDAUt.AsArray<Double>(ScalarMatrix(1));
  B := TNDAUt.AsArray<Double>(TDoubleArray.Create(1));

  CheckInitStatus(grisInvalidInput, solver.Initialize(B));
  CheckInitStatus(grisInitialized, solver.Initialize(A));

  info := solver.Solve(A, X);

  CheckStatus(grsInvalidInput, info);
  CheckTrue(X = nil);
  CheckEquals(0, info.Iterations);
  CheckTrue(IsNan(info.BackwardError));
  CheckTrue(solver.Initialized);

  B := TNDAUt.AsArray<Double>(TDoubleArray.Create(1, 2));

  CheckStatus(grsInvalidInput, solver.Solve(B, X));

  CheckTrue(X = nil);

  B := nil;

  CheckStatus(grsInvalidInput, solver.Solve(B, X));

  A := nil;

  CheckInitStatus(grisInvalidInput, solver.Initialize(A));

  CheckFalse(solver.Initialized);

  A := TNDAUt.AsArray<Double>(TDoubleArray2D.Create(TDoubleArray.Create(1, 2)));

  CheckInitStatus(grisInvalidInput, solver.Initialize(A));
end;

procedure TGaussRefineF64Tests.RejectDynamicDimensions;
var A: TDoubleArray2D;
    X: TDoubleArray;
    info: TGaussRefinementResult;
begin
  A := nil;

  CheckInitStatus(grisInvalidInput, solver.Initialize(A));

  A := TDoubleArray2D.Create(TDoubleArray.Create(1, 0));

  CheckInitStatus(grisInvalidInput, solver.Initialize(A));

  A := TDoubleArray2D.Create(TDoubleArray.Create(1, 0), TDoubleArray.Create(1));

  CheckInitStatus(grisInvalidInput, solver.Initialize(A));
  CheckInitStatus(grisInitialized, solver.Initialize(ScalarMatrix(1)));

  info := solver.Solve(TDoubleArray(nil), X);

  CheckStatus(grsInvalidInput, info);
  CheckUnavailable(info, X);

  info := solver.Solve(TDoubleArray.Create(1, 2), X);

  CheckStatus(grsInvalidInput, info);
  CheckUnavailable(info, X);
end;

procedure TGaussRefineF64Tests.RejectNonfiniteMatrix;
var A: INDArray<Double>;
begin
  CheckInitStatus(grisInvalidInput, solver.Initialize(ScalarMatrix(NaN)));
  CheckInitStatus(grisInvalidInput, solver.Initialize(ScalarMatrix(Infinity)));

  A := StridedMatrix(ScalarMatrix(NaN));

  CheckInitStatus(grisInvalidInput, solver.Initialize(A));

  CheckFalse(solver.Initialized);
end;

procedure TGaussRefineF64Tests.RejectNonfiniteRHS;
var X: TDoubleArray;
    B, NX: INDArray<Double>;
    info: TGaussRefinementResult;
begin
  CheckInitStatus(grisInitialized, solver.Initialize(ScalarMatrix(1)));

  info := solver.Solve(TDoubleArray.Create(Infinity), X);

  CheckStatus(grsInvalidInput, info);
  CheckUnavailable(info, X);

  info := solver.Solve(TDoubleArray.Create(NaN), X);

  CheckStatus(grsInvalidInput, info);
  CheckUnavailable(info, X);

  B := StridedVector(TDoubleArray.Create(NaN));

  info := solver.Solve(B, NX);

  CheckStatus(grsInvalidInput, info);
  CheckTrue(NX = nil);
  CheckTrue(solver.Initialized);

  CheckStatus(grsConverged, solver.Solve(TDoubleArray.Create(1), X));
end;

procedure TGaussRefineF64Tests.RejectOptions;
var X: TDoubleArray;
    B, NX: INDArray<Double>;
    info: TGaussRefinementResult;
begin
  solver.MaxIterations := -1;
  solver.RelativeTolerance := NaN;
  CheckInitStatus(grisInitialized, solver.Initialize(ScalarMatrix(1)));

  info := solver.Solve(TDoubleArray.Create(1), X);

  CheckStatus(grsInvalidInput, info);
  CheckUnavailable(info, X);

  solver.MaxIterations := 10;
  solver.RelativeTolerance := 0;

  info := solver.Solve(TDoubleArray.Create(1), X);

  CheckStatus(grsInvalidInput, info);
  CheckUnavailable(info, X);

  solver.RelativeTolerance := NaN;

  info := solver.Solve(TDoubleArray.Create(1), X);

  CheckStatus(grsInvalidInput, info);
  CheckUnavailable(info, X);

  solver.RelativeTolerance := Infinity;
  B := TNDAUt.AsArray<Double>(TDoubleArray.Create(1));

  info := solver.Solve(B, NX);

  CheckStatus(grsInvalidInput, info);
  CheckTrue(NX = nil);
  CheckTrue(solver.Initialized);

  solver.RelativeTolerance := cEpsF64;

  CheckStatus(grsConverged, solver.Solve(TDoubleArray.Create(1), X));
end;

procedure TGaussRefineF64Tests.SingularSystem;
var A: TDoubleArray2D;
begin
  A := TDoubleArray2D.Create(TDoubleArray.Create(1, 2), TDoubleArray.Create(2, 4));

  CheckInitStatus(grisSingular, solver.Initialize(A));

  CheckFalse(solver.Initialized);
end;

procedure TGaussRefineF64Tests.SolveBeforeInitialize;
var X: TDoubleArray;
    info: TGaussRefinementResult;
begin
  X := TDoubleArray.Create(99);

  info := solver.Solve(TDoubleArray.Create(1), X);

  CheckStatus(grsNotInitialized, info);
  CheckUnavailable(info, X);
end;

procedure TGaussRefineF64Tests.SolveNDBeforeInitialize;
var B, X: INDArray<Double>;
    info: TGaussRefinementResult;
begin
  B := nil;
  solver.MaxIterations := -1;

  info := solver.Solve(B, X);

  CheckStatus(grsNotInitialized, info);
  CheckTrue(X = nil);
  CheckEquals(0, info.Iterations);
  CheckTrue(IsNan(info.BackwardError));
end;

procedure TGaussRefineF64Tests.Reinitialize;
var A: TDoubleArray2D;
    X: TDoubleArray;
begin
  CheckInitStatus(grisInitialized, solver.Initialize(ScalarMatrix(2)));
  CheckStatus(grsConverged, solver.Solve(TDoubleArray.Create(6), X));

  CheckInitStatus(grisInitialized, solver.Initialize(ScalarMatrix(3)));
  CheckStatus(grsConverged, solver.Solve(TDoubleArray.Create(6), X));

  CheckEquals([2], X, 0);

  A := TDoubleArray2D.Create(TDoubleArray.Create(2, 0), TDoubleArray.Create(0, 4));

  CheckInitStatus(grisInitialized, solver.Initialize(A));
  CheckStatus(grsConverged, solver.Solve(TDoubleArray.Create(4, 8), X));

  CheckEquals([2, 2], X, 0);

  CheckInitStatus(grisInitialized, solver.Initialize(ScalarMatrix(1)));
  CheckStatus(grsConverged, solver.Solve(TDoubleArray.Create(7), X));

  CheckEquals([7], X, 0);
end;

procedure TGaussRefineF64Tests.FailedReinitialize;
var X: TDoubleArray;
    info: TGaussRefinementResult;
begin
  CheckInitStatus(grisInitialized, solver.Initialize(ScalarMatrix(2)));

  CheckInitStatus(grisInvalidInput, solver.Initialize(ScalarMatrix(NaN)));
  info := solver.Solve(TDoubleArray.Create(6), X);

  CheckFalse(solver.Initialized);
  CheckStatus(grsNotInitialized, info);
  CheckUnavailable(info, X);

  CheckInitStatus(grisInitialized, solver.Initialize(ScalarMatrix(2)));

  CheckInitStatus(grisSingular, solver.Initialize(ScalarMatrix(0)));
  info := solver.Solve(TDoubleArray.Create(6), X);

  CheckFalse(solver.Initialized);
  CheckStatus(grsNotInitialized, info);
  CheckUnavailable(info, X);

  CheckInitStatus(grisInitialized, solver.Initialize(ScalarMatrix(3)));
  CheckStatus(grsConverged, solver.Solve(TDoubleArray.Create(6), X));

  CheckEquals([2], X, 0);
end;

procedure TGaussRefineF64Tests.MatrixMutationAfterInitialize;
var A: TDoubleArray2D;
    X: TDoubleArray;
begin
  A := ScalarMatrix(3);
  CheckInitStatus(grisInitialized, solver.Initialize(A));
  A[0][0] := 99;
  A := nil;

  CheckStatus(grsConverged, solver.Solve(TDoubleArray.Create(1), X));

  CheckEquals([1/3], X, 0);
end;

procedure TGaussRefineF64Tests.NDMatrixMutationAfterInitialize;
var A, X: INDArray<Double>;
begin
  A := TNDAUt.AsArray<Double>(ScalarMatrix(3));
  CheckInitStatus(grisInitialized, solver.Initialize(A));
  A.Data^ := 99;
  A := nil;

  CheckStatus(grsConverged, solver.Solve(TNDAUt.AsArray<Double>(TDoubleArray.Create(1)), X));

  CheckTrue(ndaAllClose(X, 1/3, 0));
end;

procedure TGaussRefineF64Tests.SolveFailureResetsDiagnostics;
var X: TDoubleArray;
    info: TGaussRefinementResult;
begin
  CheckInitStatus(grisInitialized, solver.Initialize(ScalarMatrix(3)));
  CheckStatus(grsConverged, solver.Solve(TDoubleArray.Create(1), X));

  info := solver.Solve(TDoubleArray.Create(NaN), X);

  CheckStatus(grsInvalidInput, info);
  CheckUnavailable(info, X);

  info := solver.Solve(TDoubleArray.Create(6), X);

  CheckStatus(grsConverged, info);
  CheckEquals([2], X, 0);
  CheckEquals(0, info.Iterations);
  CheckEquals(0, info.BackwardError, 0);
end;

procedure TBlockedRefineF64Tests.SetUp;
begin
  inherited;
  TLUPDecompositionF64(solver.Solver).FactorizationMethod := lupBlocked;
  TLUPDecompositionF64(solver.Solver).BlockSize := 2;
end;

procedure TGaussRefineF64Tests.InvalidBlockedSettings;
var X: TDoubleArray;
begin
  CheckInitStatus(grisInitialized, solver.Initialize(ScalarMatrix(2)));
  TLUPDecompositionF64(solver.Solver).FactorizationMethod := lupBlocked;
  TLUPDecompositionF64(solver.Solver).BlockSize := 0;

  CheckInitStatus(grisInvalidInput, solver.Initialize(ScalarMatrix(2)));

  CheckFalse(solver.Initialized);
  CheckStatus(grsNotInitialized, solver.Solve(TDoubleArray.Create(6), X));

  TLUPDecompositionF64(solver.Solver).BlockSize := -1;

  CheckInitStatus(grisInvalidInput, solver.Initialize(ScalarMatrix(2)));

  TLUPDecompositionF64(solver.Solver).BlockSize := 2;

  CheckInitStatus(grisInitialized, solver.Initialize(ScalarMatrix(2)));
end;

procedure TGaussRefineF64Tests.FactorizationSettingsPreserveFactors;
var X: TDoubleArray;
begin
  CheckInitStatus(grisInitialized, solver.Initialize(ScalarMatrix(2)));
  TLUPDecompositionF64(solver.Solver).FactorizationMethod := lupBlocked;
  TLUPDecompositionF64(solver.Solver).BlockSize := 0;

  CheckStatus(grsConverged, solver.Solve(TDoubleArray.Create(6), X));

  CheckEquals([3], X, 0);
  CheckTrue(solver.Initialized);
end;

procedure TBlockedRefineF64Tests.BlockedRefinementAccuracy;
var A: TDoubleArray2D;
    B, X: TDoubleArray;
    info: TGaussRefinementResult;
begin
  A := TDoubleArray2D.Create(
    TDoubleArray.Create(0, 0, 3, 1, 0),
    TDoubleArray.Create(0, 5, 1, 0, 2),
    TDoubleArray.Create(7, 1, 0, 2, 0),
    TDoubleArray.Create(1, 0, 2, 11, 1),
    TDoubleArray.Create(0, 1, 0, 2, 13));
  B := TDoubleArray.Create(13, 23, 17, 56, 75);
  TLUPDecompositionF64(solver.Solver).FactorizationMethod := lupBlocked;
  TLUPDecompositionF64(solver.Solver).BlockSize := 2;

  CheckInitStatus(grisInitialized, solver.Initialize(A));
  info := solver.Solve(B, X);

  CheckStatus(grsConverged, info);
  CheckEquals([1, 2, 3, 4, 5], X, 1e-12);
  CheckTrue(info.BackwardError <= solver.RelativeTolerance);
end;

initialization

  RegisterTest(TGaussRefineF64Tests.Suite);
  RegisterTest(TBlockedRefineF64Tests.Suite);

end.
