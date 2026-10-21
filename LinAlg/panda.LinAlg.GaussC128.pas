unit panda.LinAlg.GaussC128;

interface

uses
    panda.Nums
  , panda.Intfs
  , panda.LinAlg.Gauss
  , panda.LinAlg.Base
  , panda.LinAlg.NumericC128
  ;

type
  /// <summary>
  ///   General Gaussian elimination for TCmplx128, including rectangular and
  ///   rank-deficient systems. Returns a particular solution and null-space basis.
  ///   Init copies inputs; InitWithoutCopy aliases writable row-contiguous storage.
  ///   Solve consumes initialization; results retain independent storage.
  ///   Arithmetic and allocation exceptions propagate.
  /// </summary>
  /// <remarks>
  ///   Pivots select the maximum modulus. Values at or below the prepared
  ///   threshold are zero. Automatic thresholds are factor * Max(rows, columns)
  ///   * cEpsF64 * max magnitude, separately for A and b. Homogeneity is exact.
  ///   Products do not conjugate either operand. Inputs and intermediate
  ///   values must be supported by scalar arithmetic.
  /// </remarks>
  TGaussElimC128 = class(TGaussElim<TCmplx128>)
  public const
    /// <summary>Default safety factor for automatic tolerance estimation.</summary>
    cDefaultTolFactor = TLinAlgNumericC128.cDefaultTolFactor;
  protected
    function GetTolerance: TGaussTolerance;
    procedure SetTolerance(const aValue: TGaussTolerance);
    function GetZeroTolerance: Double;
    procedure SetZeroTolerance(const aValue: Double);
    function GetToleranceFactor: Double;
    procedure SetToleranceFactor(const aValue: Double);
    function CreateNumeric: TLinAlgNumeric<TCmplx128>; override;
  public
    /// <summary>
    ///   Returns the tolerances that <c>Solve</c>
    ///   estimates when <c>Tolerance</c> is not set:
    ///   <c>Pivot = aFactor * Max(rows, columns) * eps * max|A|</c> and
    ///   <c>RHS = aFactor * Max(rows, columns) * eps * max|b|</c>.
    ///   <c>aFactor</c> that is not positive selects <c>cDefaultTolFactor</c>.
    ///   <c>RHS</c> is 0 when <c>ab</c> is nil. Both fields are 0 when
    ///   <c>aA</c> has no rows or its rows differ in length.
    ///   The result can be assigned to <c>Tolerance</c> and reused for systems
    ///   whose <c>max|A|</c> and <c>max|b|</c> are of the same magnitude.
    /// </summary>
    class function EstimateTolerance(const aA: TArray<TArray<TCmplx128>>;
      const ab: TArray<TCmplx128> = nil;
      aFactor: Double = cDefaultTolFactor): TGaussTolerance; overload;
    /// <summary>
    ///   Same as the dynamic array overload. Any 2D layout of <c>aA</c> and any
    ///   stride of <c>ab</c> is accepted. Both fields are 0 when <c>aA</c> is
    ///   not a matrix, and <c>RHS</c> is 0 when <c>ab</c> is not a vector.
    /// </summary>
    class function EstimateTolerance(const aA: INDArray<TCmplx128>;
      const ab: INDArray<TCmplx128> = nil;
      aFactor: Double = cDefaultTolFactor): TGaussTolerance; overload;
    /// <summary>
    ///   Pivot and consistency thresholds read when Solve runs. Nonpositive
    ///   fields are estimated from the original matrix and RHS.
    /// </summary>
    property Tolerance: TGaussTolerance read GetTolerance write SetTolerance;
    /// <summary>Reads Pivot; writing sets both Pivot and RHS thresholds.</summary>
    property ZeroTolerance: Double read GetZeroTolerance write SetZeroTolerance;
    /// <summary>
    ///   Automatic tolerance safety factor, initially 100. Nonpositive values
    ///   select 100. Read at Solve; explicit positive thresholds take precedence.
    /// </summary>
    property ToleranceFactor: Double read GetToleranceFactor write SetToleranceFactor;
  end;

implementation

function TGaussElimC128.CreateNumeric: TLinAlgNumeric<TCmplx128>;
begin
  Result := TLinAlgNumericC128.Create;
end;

function TGaussElimC128.GetTolerance: TGaussTolerance;
begin
  Result := TLinAlgNumericC128(fNumeric).Tolerance;
end;

procedure TGaussElimC128.SetTolerance(const aValue: TGaussTolerance);
begin
  TLinAlgNumericC128(fNumeric).Tolerance := aValue;
end;

function TGaussElimC128.GetZeroTolerance: Double;
begin
  Result := TLinAlgNumericC128(fNumeric).ZeroTolerance;
end;

procedure TGaussElimC128.SetZeroTolerance(const aValue: Double);
begin
  TLinAlgNumericC128(fNumeric).ZeroTolerance := aValue;
end;

function TGaussElimC128.GetToleranceFactor: Double;
begin
  Result := TLinAlgNumericC128(fNumeric).ToleranceFactor;
end;

procedure TGaussElimC128.SetToleranceFactor(const aValue: Double);
begin
  TLinAlgNumericC128(fNumeric).ToleranceFactor := aValue;
end;

class function TGaussElimC128.EstimateTolerance(const aA: TArray<TArray<TCmplx128>>;
  const ab: TArray<TCmplx128>; aFactor: Double): TGaussTolerance;
begin
  Result := TLinAlgNumericC128.EstimateTolerance(aA, ab, aFactor);
end;

class function TGaussElimC128.EstimateTolerance(const aA: INDArray<TCmplx128>;
  const ab: INDArray<TCmplx128>; aFactor: Double): TGaussTolerance;
begin
  Result := TLinAlgNumericC128.EstimateTolerance(aA, ab, aFactor);
end;

end.
