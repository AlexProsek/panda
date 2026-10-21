unit panda.LinAlg.RefineF64;

interface

uses
    panda.LinAlg.Base
  , panda.Intfs
  , panda.NumsQP
  ;

type
  /// <summary>Outcome of preparing a matrix for repeated refined solves.</summary>
  TGaussRefineInitStatus = (
    /// <summary>The factors and high-precision matrix are ready.</summary>
    grisInitialized,
    /// <summary>The matrix or factorization settings are invalid.</summary>
    grisInvalidInput,
    /// <summary>The backend rejected a pivot using its singularity policy.</summary>
    grisSingular,
    /// <summary>Factorization failed arithmetically or produced nonfinite values.</summary>
    grisNumericalFailure,
    /// <summary>The Cholesky backend rejected a diagonal residual.</summary>
    grisNotPositiveDefinite
  );

  /// <summary>Reason why iterative refinement stopped.</summary>
  TGaussRefinementStatus = (
    /// <summary>The residual is exactly zero or both stopping tolerances hold.</summary>
    grsConverged,
    /// <summary>Corrections stopped decreasing, changed no values, or underflowed.</summary>
    grsStagnated,
    /// <summary>The permitted number of correction solves was exhausted.</summary>
    grsIterationLimit,
    /// <summary>Reserved; Initialize reports singularity as grisSingular.</summary>
    grsSingular,
    /// <summary>Invalid RHS dimensions, nonfinite RHS, or refinement options.</summary>
    grsInvalidInput,
    /// <summary>Arithmetic failed or produced a nonfinite Double value.</summary>
    grsNumericalFailure,
    /// <summary>No matrix has been successfully initialized.</summary>
    grsNotInitialized
  );

  /// <summary>Stop reason and diagnostics for the returned solution.</summary>
  TGaussRefinementResult = record
    /// <summary>Convergence is a stopping test, not a certified forward-error bound.</summary>
    Status: TGaussRefinementStatus;
    /// <summary>Correction solves attempted, excluding the initial solution.</summary>
    Iterations: Integer;
    /// <summary>
    ///   Componentwise backward error, max_i |b-A*X|_i / (|b|+|A|*|X|)_i,
    ///   evaluated in TReal128 and returned as Double. A zero denominator and
    ///   zero residual contribute zero. NaN when no solution is available.
    /// </summary>
    BackwardError: Double;
  end;

  /// <summary>
  ///   Repeated Double backend solves with TReal128 residuals and Double updates.
  ///   Initialize prepares a nonempty square matrix; Solve accepts one RHS.
  /// </summary>
  /// <remarks>
  ///   Initialize costs O(N^3) and caches O(N^2) factors and TReal128
  ///   coefficients. Each Solve uses O(N) workspace and O(N^2) work per
  ///   refinement iteration. Inputs are unchanged; outputs own independent
  ///   storage. No condition estimate or RHS tolerance is used. Numerical
  ///   failures return a status; allocation and unrelated exceptions propagate.
  ///   Residual and RHS workspaces are reused; Solve allocates its output
  ///   buffers. Calls on one instance must not overlap. Callers select a backend
  ///   suitable for the matrix; no classification or fallback is performed.
  ///   Cholesky requires a complete symmetric positive definite matrix, since
  ///   residuals use both triangles of the original coefficients.
  /// </remarks>
  TGaussRefineF64 = class
  protected
    fSolver: TLinearSolver<Double>;
    fMaxIterations: Integer;
    fRelativeTolerance: Double;
    fInitialized: Boolean;
    fA128: TArray<TArray<TReal128>>;
    fB128, fX128, fResidual: TArray<TReal128>;
    fRHS, fCorrection, fCandidate: TArray<Double>;
    // Cores trust inputs validated by the public entry points.
    // InitializeCore takes ownership of a private, finite square matrix copy.
    function InitializeCore(const aA: TArray<TArray<Double>>): TGaussRefineInitStatus;
    function SolveCore(const ab: TArray<Double>; out aX: TArray<Double>): TGaussRefinementResult;
  public
    /// <summary>Creates an owned LUP backend; allocation exceptions propagate.</summary>
    constructor Create; overload;
    /// <summary>
    ///   Takes ownership of aSolver, including any existing factorization.
    ///   Raises EArgumentNilException for nil. The backend is released on
    ///   destruction; Initialize replaces its existing factors.
    /// </summary>
    constructor Create(aSolver: TLinearSolver<Double>); overload;
    /// <summary>Sets the iteration limit to 10 and relative tolerance to cEpsF64.</summary>
    procedure AfterConstruction; override;
    /// <summary>Releases the contained solver and cached data.</summary>
    destructor Destroy; override;
    /// <summary>
    ///   Copies a finite, nonempty square matrix, converts its original values
    ///   to TReal128, and factors its Double copy using Solver's current settings.
    ///   Backend input/settings rejection returns grisInvalidInput; pivot rejection
    ///   returns grisSingular or grisNotPositiveDefinite. Arithmetic exceptions
    ///   and nonfinite factors return grisNumericalFailure.
    ///   Any unsuccessful call leaves Initialized False. Refinement settings
    ///   are not read or reset. No original Double matrix copy is retained.
    /// </summary>
    function Initialize(const aA: TArray<TArray<Double>>): TGaussRefineInitStatus; overload;
    /// <summary>
    ///   Same as the dynamic-array overload. Copies arbitrary matrix strides,
    ///   including negative strides and read-only views.
    /// </summary>
    function Initialize(const aA: INDArray<Double>): TGaussRefineInitStatus; overload;
    /// <summary>
    ///   Solves and refines using the cached matrix and factors. Requires a finite
    ///   RHS matching the initialized matrix size and valid refinement options.
    ///   Returns a new vector, or nil with zero iterations and NaN backward error
    ///   when no finite initial solution exists. An uninitialized instance returns
    ///   grsNotInitialized before checking the RHS or options. Solves, including
    ///   unsuccessful ones, preserve matrix initialization.
    /// </summary>
    /// <remarks>
    ///   An exact residual converges immediately. Otherwise a correction is
    ///   attempted even for small backward error. Nondecreasing correction norms
    ///   are rejected as stagnation. After an update, convergence requires an exact
    ///   residual or both ||d||inf &lt;= tolerance*||X||inf and backward error
    ///   &lt;= tolerance. An unchanged X or complete residual underflow stagnates.
    ///   MaxIterations bounds correction solves. Arithmetic failure preserves the
    ///   last accepted finite X and its diagnostics. Residual-to-Double range
    ///   errors and floating-point arithmetic exceptions become NumericalFailure.
    /// </remarks>
    function Solve(const ab: TArray<Double>; out aX: TArray<Double>): TGaussRefinementResult; overload;
    /// <summary>
    ///   Same as the dynamic-array overload. Copies arbitrary vector strides,
    ///   including negative strides and read-only views. Returns independent storage.
    /// </summary>
    function Solve(const ab: INDArray<Double>; out aX: INDArray<Double>): TGaussRefinementResult; overload;

    /// <summary>True after successful Initialize, until another Initialize fails.</summary>
    property Initialized: Boolean read fInitialized;
    /// <summary>
    ///   Owned backend, fixed for this instance's lifetime. Configure factorization
    ///   settings on its concrete type; changes apply at the next Initialize.
    ///   Do not free it or independently call its Initialize or Solve methods.
    /// </summary>
    property Solver: TLinearSolver<Double> read fSolver;
    /// <summary>
    ///   Maximum correction solves per Solve; initially 10. Zero evaluates the
    ///   initial solution without corrections. Negative values return InvalidInput.
    ///   Initialize does not read or reset this property.
    /// </summary>
    property MaxIterations: Integer read fMaxIterations write fMaxIterations;
    /// <summary>
    ///   Relative correction and backward-error tolerance per Solve; initially
    ///   cEpsF64. Nonpositive or nonfinite values return InvalidInput. Independent
    ///   of PivotTolerance. Initialize does not read or reset this property.
    /// </summary>
    property RelativeTolerance: Double read fRelativeTolerance write fRelativeTolerance;
  end;

implementation

uses
    System.SysUtils
  , System.Math
  , panda.Arrays
  , panda.ArrCmp
  , panda.cvMath
  , panda.Nums
  , panda.DynArrayUtils
  , panda.LinAlg.LUPF64
  ;

function RefinementResult(aStatus: TGaussRefinementStatus): TGaussRefinementResult;
begin
  Result.Status := aStatus;
  Result.Iterations := 0;
  Result.BackwardError := NaN;
end;

function FiniteValueQ(const aValue: Double): Boolean; inline;
begin
  Result := not (IsNan(aValue) or IsInfinite(aValue));
end;

function FiniteValuesQ(const aV: TArray<Double>): Boolean; inline;
begin
  Result := _IsFinite_F64(PByte(aV), Length(aV), cF64Sz)
end;

function FiniteVectorQ(const aV: TArray<Double>; aN: NativeInt): Boolean;
begin
  Result := (Length(aV) = aN) and FiniteValuesQ(aV);
end;

function FiniteMatrixQ(const aA: TArray<TArray<Double>>): Boolean;
var I, N: NativeInt;
begin
  N := Length(aA);
  if N <= 0 then exit(False);
  for I := 0 to N - 1 do
    if not FiniteVectorQ(aA[I], N) then exit(False);
  Result := True;
end;

function ValidOptions(aMaxIterations: Integer; aTolerance: Double): Boolean;
begin
  Result := (aMaxIterations >= 0) and FiniteValueQ(aTolerance) and (aTolerance > 0);
end;

// All arrays are prepared and dimensioned by the caller. Keep the residual
// in TReal128 until a correction is requested, including diagnostic-only calls.
function Residual(const aA: TArray<TArray<TReal128>>;
  const ab, aX: TArray<TReal128>; var aR: TArray<TReal128>;
  out aError: Double): Boolean;
var I, J: NativeInt;
    r, denom, prod, ratio, maxError: TReal128;
begin
  Result := True;
  maxError := cZeroF128;
  for I := 0 to High(aA) do begin
    r := ab[I];
    denom := r.Abs;
    for J := 0 to High(aX) do begin
      prod := aA[I][J] * aX[J];
      r := r - prod;
      denom := denom + prod.Abs;
    end;
    aR[I] := r;
    if not r.IsExactZero then begin
      Result := False;
      ratio := r.Abs / denom;
      if ratio > maxError then maxError := ratio;
    end;
  end;
  aError := Double(maxError);
end;

constructor TGaussRefineF64.Create;
begin
  inherited Create;
  fSolver := TLUPDecompositionF64.Create;
end;

constructor TGaussRefineF64.Create(aSolver: TLinearSolver<Double>);
begin
  inherited Create;
  if aSolver = nil then
    raise EArgumentNilException.Create('Linear solver must not be nil');
  fSolver := aSolver;
end;

procedure TGaussRefineF64.AfterConstruction;
begin
  inherited;
  fMaxIterations := 10;
  fRelativeTolerance := cEpsF64;
end;

destructor TGaussRefineF64.Destroy;
begin
  fSolver.Free;
  inherited;
end;

function TGaussRefineF64.Initialize(const aA: TArray<TArray<Double>>): TGaussRefineInitStatus;
var A: TArray<TArray<Double>>;
begin
  fInitialized := False;
  Result := grisInvalidInput;
  if not FiniteMatrixQ(aA) then exit;

  A := TDynAUt.Clone<Double>(aA);
  Result := InitializeCore(A);
end;

function TGaussRefineF64.Initialize(const aA: INDArray<Double>): TGaussRefineInitStatus;
var A: TArray<TArray<Double>>;
begin
  fInitialized := False;
  Result := grisInvalidInput;
  if not (
    Assigned(aA) and ndaAllIsFinite(aA) and SquareMatrixQ(aA) and
    TNDAUt.TryAsDynArray2D<Double>(aA, A))
  then
    exit;

  Result := InitializeCore(A);
end;

function TGaussRefineF64.InitializeCore(
  const aA: TArray<TArray<Double>>): TGaussRefineInitStatus;
var N, I, J: NativeInt;
    status: TLinearSolverInitStatus;
begin
  N := Length(aA);
  SetLength(fA128, N, N);
  for I := 0 to N - 1 do
    for J := 0 to N - 1 do
      fA128[I][J] := aA[I][J];

  Result := grisNumericalFailure;
  try
    status := fSolver.InitializeWithoutCopy(aA);
    if not FiniteMatrixQ(aA) then exit;
    case status of
      lsisInitialized: ;
      lsisInvalidInput: exit(grisInvalidInput);
      lsisSingular: exit(grisSingular);
      lsisNotPositiveDefinite: exit(grisNotPositiveDefinite);
    else
      exit;
    end;
  except
    on E: EMathError do exit;
  end;

  SetLength(fB128, N);
  SetLength(fX128, N);
  SetLength(fResidual, N);
  SetLength(fRHS, N);
  fInitialized := True;
  Result := grisInitialized;
end;

function TGaussRefineF64.Solve(const ab: TArray<Double>;
  out aX: TArray<Double>): TGaussRefinementResult;
begin
  aX := nil;
  if not fInitialized then exit(RefinementResult(grsNotInitialized));

  Result := RefinementResult(grsInvalidInput);
  if not (
    ValidOptions(fMaxIterations, fRelativeTolerance) and
    FiniteVectorQ(ab, Length(fA128)))
  then
    exit;

  Result := SolveCore(ab, aX);
end;

function TGaussRefineF64.Solve(const ab: INDArray<Double>;
  out aX: INDArray<Double>): TGaussRefinementResult;
var B, X: TArray<Double>;
    N: NativeInt;
begin
  aX := nil;
  if not fInitialized then exit(RefinementResult(grsNotInitialized));

  N := Length(fA128);
  Result := RefinementResult(grsInvalidInput);
  if not (
    ValidOptions(fMaxIterations, fRelativeTolerance) and
    Assigned(ab) and VectorQ(ab) and (ab.Size = N) and
    ndaAllIsFinite(aB) and TNDAUt.TryAsDynArray<Double>(ab, B))
  then
    exit;

  Result := SolveCore(B, X);
  if Length(X) = N then
    aX := TDynArrWrapper<Double>.Create(X);
end;

function TGaussRefineF64.SolveCore(const ab: TArray<Double>;
  out aX: TArray<Double>): TGaussRefinementResult;
var N, I: NativeInt;
    dNorm, previousNorm, xNorm: Double;
    tol: TReal128;
    nonzero, changed, exact: Boolean;
    error, absValue: Double;
begin
  aX := nil;
  N := Length(fA128);
  tol := fRelativeTolerance;
  previousNorm := 0;

  Result := RefinementResult(grsNumericalFailure);
  try
    if fSolver.Solve(ab, fCandidate) <> lsssSolved then exit;
    if not FiniteVectorQ(fCandidate, N) then exit;

    aX := Copy(fCandidate);
    for I := 0 to N - 1 do begin
      fB128[I] := ab[I];
      fX128[I] := aX[I];
    end;
    if Residual(fA128, fB128, fX128, fResidual, Result.BackwardError) then begin
      Result.Status := grsConverged;
      exit;
    end;

    while Result.Iterations < fMaxIterations do begin
      nonzero := False;
      // Only this conversion's range error is a numerical failure.
      try
        for I := 0 to N - 1 do begin
          fRHS[I] := Double(fResidual[I]);
          nonzero := nonzero or (fRHS[I] <> 0);
        end;
      except
        on E: ERangeError do exit;
      end;
      if not nonzero then begin
        Result.Status := grsStagnated;
        exit;
      end;

      Inc(Result.Iterations);
      if fSolver.Solve(fRHS, fCorrection) <> lsssSolved then exit;
      if not FiniteVectorQ(fCorrection, N) then exit;

      cvAbsMax(PDouble(fCorrection), N, dNorm);
      if (Result.Iterations > 1) and (dNorm >= previousNorm) then begin
        Result.Status := grsStagnated;
        exit;
      end;

      changed := False;
      xNorm := 0;
      for I := 0 to N - 1 do begin
        fCandidate[I] := aX[I] + fCorrection[I];
        if not FiniteValueQ(fCandidate[I]) then exit;
        changed := changed or (fCandidate[I] <> aX[I]);
        fX128[I] := fCandidate[I];
        absValue := Abs(fCandidate[I]);
        if absValue > xNorm then xNorm := absValue;
      end;
      if changed then begin
        // Evaluate before committing, so a failure preserves X and its diagnostics.
        exact := Residual(fA128, fB128, fX128, fResidual, error);
        Move(fCandidate[0], aX[0], N * cF64Sz);
        Result.BackwardError := error;
      end else begin
        // The unchanged solution still has the previously evaluated nonzero residual.
        exact := False;
        error := Result.BackwardError;
      end;
      if exact or ((TReal128(dNorm) <= tol * TReal128(xNorm)) and
        (error <= fRelativeTolerance)) then begin
        Result.Status := grsConverged;
        exit;
      end;
      if not changed then begin
        Result.Status := grsStagnated;
        exit;
      end;
      previousNorm := dNorm;
    end;
    Result.Status := grsIterationLimit;
  except
    on E: EMathError do Result.Status := grsNumericalFailure;
  end;
end;

end.
