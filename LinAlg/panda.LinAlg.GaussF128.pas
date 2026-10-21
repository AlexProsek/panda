unit panda.LinAlg.GaussF128;

interface

uses
    panda.NumsQP
  , panda.Intfs
  , panda.LinAlg.Gauss
  , panda.LinAlg.Base
  , panda.LinAlg.NumericF128
  ;

type
  TGaussToleranceF128 = panda.LinAlg.NumericF128.TGaussToleranceF128;

  /// <summary>
  ///   General Gaussian elimination for TReal128, including rectangular and
  ///   rank-deficient systems. Returns a particular solution and null-space basis.
  ///   Init copies inputs; InitWithoutCopy aliases writable row-contiguous storage.
  ///   Solve consumes initialization; results retain independent storage.
  ///   Arithmetic and allocation exceptions propagate.
  /// </summary>
  /// <remarks>
  ///   Pivots select the maximum absolute value. Values at or below the prepared
  ///   threshold are zero. Automatic thresholds are factor * Max(rows, columns)
  ///   * cEpsF128 * max magnitude, separately for A and b. Homogeneity is exact.
  ///   Inputs and intermediate values must be supported by scalar arithmetic.
  /// </remarks>
  TGaussElimF128 = class(TGaussElim<TReal128>)
  public const
    /// <summary>Default safety factor for automatic tolerance estimation.</summary>
    cDefaultTolFactor = TLinAlgNumericF128.cDefaultTolFactor;
  protected
    function GetTolerance: TGaussToleranceF128;
    procedure SetTolerance(const aValue: TGaussToleranceF128);
    function GetZeroTolerance: TReal128;
    procedure SetZeroTolerance(const aValue: TReal128);
    function GetToleranceFactor: Double;
    procedure SetToleranceFactor(const aValue: Double);
    function CreateNumeric: TLinAlgNumeric<TReal128>; override;
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
    class function EstimateTolerance(const aA: TArray<TArray<TReal128>>;
      const ab: TArray<TReal128> = nil;
      aFactor: Double = cDefaultTolFactor): TGaussToleranceF128; overload;
    /// <summary>
    ///   Same as the dynamic array overload. Any 2D layout of <c>aA</c> and any
    ///   stride of <c>ab</c> is accepted. Both fields are 0 when <c>aA</c> is
    ///   not a matrix, and <c>RHS</c> is 0 when <c>ab</c> is not a vector.
    /// </summary>
    class function EstimateTolerance(const aA: INDArray<TReal128>;
      const ab: INDArray<TReal128> = nil;
      aFactor: Double = cDefaultTolFactor): TGaussToleranceF128; overload;
    /// <summary>
    ///   Pivot and consistency thresholds read when Solve runs. Nonpositive
    ///   fields are estimated from the original matrix and RHS.
    /// </summary>
    property Tolerance: TGaussToleranceF128 read GetTolerance write SetTolerance;
    /// <summary>Reads Pivot; writing sets both Pivot and RHS thresholds.</summary>
    property ZeroTolerance: TReal128 read GetZeroTolerance write SetZeroTolerance;
    /// <summary>
    ///   Automatic tolerance safety factor, initially 100. Nonpositive values
    ///   select 100. Read at Solve; explicit positive thresholds take precedence.
    /// </summary>
    property ToleranceFactor: Double read GetToleranceFactor write SetToleranceFactor;
  end;

implementation

function TGaussElimF128.CreateNumeric: TLinAlgNumeric<TReal128>;
begin
  Result := TLinAlgNumericF128.Create;
end;

function TGaussElimF128.GetTolerance: TGaussToleranceF128;
begin
  Result := TLinAlgNumericF128(fNumeric).Tolerance;
end;

procedure TGaussElimF128.SetTolerance(const aValue: TGaussToleranceF128);
begin
  TLinAlgNumericF128(fNumeric).Tolerance := aValue;
end;

function TGaussElimF128.GetZeroTolerance: TReal128;
begin
  Result := TLinAlgNumericF128(fNumeric).ZeroTolerance;
end;

procedure TGaussElimF128.SetZeroTolerance(const aValue: TReal128);
begin
  TLinAlgNumericF128(fNumeric).ZeroTolerance := aValue;
end;

function TGaussElimF128.GetToleranceFactor: Double;
begin
  Result := TLinAlgNumericF128(fNumeric).ToleranceFactor;
end;

procedure TGaussElimF128.SetToleranceFactor(const aValue: Double);
begin
  TLinAlgNumericF128(fNumeric).ToleranceFactor := aValue;
end;

class function TGaussElimF128.EstimateTolerance(const aA: TArray<TArray<TReal128>>;
  const ab: TArray<TReal128>; aFactor: Double): TGaussToleranceF128;
begin
  Result := TLinAlgNumericF128.EstimateTolerance(aA, ab, aFactor);
end;

class function TGaussElimF128.EstimateTolerance(const aA: INDArray<TReal128>;
  const ab: INDArray<TReal128>; aFactor: Double): TGaussToleranceF128;
begin
  Result := TLinAlgNumericF128.EstimateTolerance(aA, ab, aFactor);
end;

end.
