unit panda.LinAlg.LeastSquaresF64;

interface

uses
    panda.Intfs
  , panda.LinAlg.Base
  ;

type
  /// <summary>Outcome of a Double least-squares solve.</summary>
  TLeastSquaresStatus = (
    /// <summary>An independent coefficient vector was returned.</summary>
    lssSolved,
    /// <summary>The inputs, dimensions, supplied solver, or solver settings are invalid.</summary>
    lssInvalidInput,
    /// <summary>The normal matrix was rejected by the numerical pivot policy.</summary>
    lssRankDeficient,
    /// <summary>The supplied solver did not meet its convergence policy.</summary>
    lssNotConverged,
    /// <summary>The supplied solver reported a numerical failure.</summary>
    lssNumericalFailure
  );

/// <summary>
///   Minimizes the sum of squared residuals of aA * aX - ab using normal
///   equations and Cholesky factorization with its default tolerance policy.
///   Requires nonempty aA of shape [m, n], m >= n, and ab of shape [m].
///   Any valid strides, including negative strides and read-only views, are
///   accepted. Inputs are unchanged; successful aX owns storage of shape [n].
/// </summary>
/// <remarks>
///   Nil inputs or unsupported dimensions return lssInvalidInput. Rejected
///   normal-matrix pivots return lssRankDeficient, including numerically
///   unresolved systems; this is not an exact rank test. Unsuccessful statuses
///   clear aX. No intercept, normalization or additional weighting is applied.
///   Finite inputs are required; arithmetic and allocation exceptions propagate.
/// </remarks>
function LeastSquares(const aA, ab: INDArray<Double>;
  out aX: INDArray<Double>): TLeastSquaresStatus; overload;

/// <summary>
///   Converts inputs to INDArray and delegates to the INDArray overload.
///   Requires nonempty rectangular aA with m rows and n columns, m >= n,
///   and m values in ab. Inputs are unchanged; successful aX owns n values.
/// </summary>
/// <remarks>
///   Invalid dimensions, including ragged rows, return lssInvalidInput.
///   Normal-matrix pivot rejection returns lssRankDeficient under the same
///   numerical policy as the INDArray overload, not an exact rank test.
///   Unsuccessful statuses clear aX. No intercept, normalization or additional
///   weighting is applied. Finite inputs are required; arithmetic and
///   allocation exceptions propagate.
/// </remarks>
function LeastSquares(const aA: TArray<TArray<Double>>; const ab: TArray<Double>;
 out aX: TArray<Double>): TLeastSquaresStatus; overload;

/// <summary>
///   Forms Double normal equations and solves them with caller-owned aSolver.
///   Requires the same dimensions as the default overload. Valid calls replace
///   the solver's initialization and borrow the private normal matrix for factors.
///   Inputs are unchanged; successful aX owns independent storage.
/// </summary>
/// <remarks>
///   Nil solver or invalid dimensions return InvalidInput without initializing it.
///   Pivot rejection returns RankDeficient; nonconvergence and numerical failure
///   return their corresponding statuses. Unsuccessful statuses clear aX.
///   Solver configuration controls acceptance; refinement diagnostics remain on
///   its concrete type. Refinement applies to the rounded Double normal equations,
///   not residuals formed from original aA and ab. Exceptions propagate.
/// </remarks>
function LeastSquares(const aA, ab: INDArray<Double>; out aX: INDArray<Double>;
  aSolver: TLinearSolver<Double>): TLeastSquaresStatus; overload;

/// <summary>
///   Dynamic-array equivalent of the injected-solver overload. Rejects ragged
///   matrices. The caller owns aSolver; invalid dimensions do not initialize it.
///   Successful aX owns independent storage; unsuccessful statuses clear aX.
///   Uses the same normal equations, status policy, and exception behavior.
/// </summary>
function LeastSquares(const aA: TArray<TArray<Double>>; const ab: TArray<Double>;
  out aX: TArray<Double>; aSolver: TLinearSolver<Double>): TLeastSquaresStatus; overload;

implementation

uses
    panda.Arrays
  , panda.Math
  , panda.LinAlg.CholeskyF64
  , panda.DynArrayUtils
  ;

function LeastSquares(const aA, ab: INDArray<Double>;
  out aX: INDArray<Double>): TLeastSquaresStatus;
var solver: TCholeskyDecompositionF64;
begin
  solver := TCholeskyDecompositionF64.Create;
  try
    Result := LeastSquares(aA, ab, aX, solver);
  finally
    solver.Free;
  end;
end;

function LeastSquares(const aA, ab: INDArray<Double>; out aX: INDArray<Double>;
  aSolver: TLinearSolver<Double>): TLeastSquaresStatus;
var G, At, c: INDArray<Double>;
    m, n: NativeInt;
begin
  aX := nil;
  if not (Assigned(aSolver) and Assigned(aA) and Assigned(ab)) then exit(lssInvalidInput);
  if not (MatrixQ(aA) and VectorQ(ab)) then exit(lssInvalidInput);

  m := aA.Shape[0];
  n := aA.Shape[1];
  if (n <= 0) or (m < n) or (ab.Shape[0] <> m) then exit(lssInvalidInput);

  ndaGram(aA, G, At);
  c := ndaDot(At, TNDAUt.AsContiguousArray<Double>(ab));

  case aSolver.InitializeWithoutCopy(G) of
    lsisInitialized:
      case aSolver.Solve(c, aX) of
        lsssSolved: Result := lssSolved;
        lsssInvalidInput: Result := lssInvalidInput;
        lsssNotConverged: Result := lssNotConverged;
      else
        Result := lssNumericalFailure;
      end;
    lsisSingular, lsisNotPositiveDefinite:
      Result := lssRankDeficient;
    lsisInvalidInput: Result := lssInvalidInput;
  else
    Result := lssNumericalFailure;
  end;
  if Result <> lssSolved then aX := nil;
end;

function LeastSquares(const aA: TArray<TArray<Double>>; const ab: TArray<Double>;
  out aX: TArray<Double>): TLeastSquaresStatus;
var A, b, X: INDArray<Double>;
begin
  aX := nil;
  if not TDynAUt.MatrixQ<Double>(aA) then exit(lssInvalidInput);

  A := TNDAUt.AsArray<Double>(aA);
  b := TNDAUt.AsArray<Double>(ab);

  Result := LeastSquares(A, b, X);

  if Result = lssSolved then
    TNDAUt.TryAsDynArray<Double>(X, aX);
end;

function LeastSquares(const aA: TArray<TArray<Double>>; const ab: TArray<Double>;
  out aX: TArray<Double>; aSolver: TLinearSolver<Double>): TLeastSquaresStatus;
var A, b, X: INDArray<Double>;
begin
  aX := nil;
  if not TDynAUt.MatrixQ<Double>(aA) then exit(lssInvalidInput);

  A := TNDAUt.AsArray<Double>(aA);
  b := TNDAUt.AsArray<Double>(ab);
  Result := LeastSquares(A, b, X, aSolver);

  if Result = lssSolved then
    TNDAUt.TryAsDynArray<Double>(X, aX);
end;

end.
