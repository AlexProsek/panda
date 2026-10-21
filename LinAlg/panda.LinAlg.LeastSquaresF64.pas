unit panda.LinAlg.LeastSquaresF64;

interface

uses
    panda.Intfs
  ;

type
  /// <summary>Outcome of a Double least-squares solve.</summary>
  TLeastSquaresStatus = (
    /// <summary>An independent coefficient vector was returned.</summary>
    lssSolved,
    /// <summary>The inputs do not have supported, matching dimensions.</summary>
    lssInvalidInput,
    /// <summary>The normal matrix was rejected by the numerical pivot policy.</summary>
    lssRankDeficient
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

implementation

uses
    panda.Arrays
  , panda.Math
  , panda.LinAlg.Base
  , panda.LinAlg.CholeskyF64
  , panda.DynArrayUtils
  ;

function LeastSquares(const aA, ab: INDArray<Double>;
  out aX: INDArray<Double>): TLeastSquaresStatus;
var G, At, c: INDArray<Double>;
    solver: TCholeskyDecompositionF64;
    m, n: NativeInt;
begin
  Assert(Assigned(aA) and Assigned(aB));

  if not (MatrixQ(aA) and VectorQ(ab)) then exit(lssInvalidInput);

  m := aA.Shape[0];
  n := aA.Shape[1];
  if (n <= 0) or (m < n) or (ab.Shape[0] <> m) then exit(lssInvalidInput);

  ndaGram(aA, G, At);
  c := ndaDot(At, TNDAUt.AsContiguousArray<Double>(ab));

  solver := TCholeskyDecompositionF64.Create;
  try
    case solver.InitializeWithoutCopy(G) of
      lsisInitialized:
        case solver.Solve(c, aX) of
          lsssSolved: Result := lssSolved;
        else
          Result := lssInvalidInput;
        end;
      lsisSingular, lsisNotPositiveDefinite:
        Result := lssRankDeficient;
    else
      Result := lssInvalidInput;
    end;
  finally
    solver.Free;
  end;
end;

function LeastSquares(const aA: TArray<TArray<Double>>; const ab: TArray<Double>;
  out aX: TArray<Double>): TLeastSquaresStatus;
var A, b, X: INDArray<Double>;
begin
  if not TDynAUt.MatrixQ<Double>(aA) then exit(lssInvalidInput);

  A := TNDAUt.AsArray<Double>(aA);
  b := TNDAUt.AsArray<Double>(ab);

  Result := LeastSquares(A, b, X);

  if Result = lssSolved then
    TNDAUt.TryAsDynArray<Double>(X, aX);
end;

end.
