unit panda.LinAlg.GaussF64;

interface

uses
    panda.Intfs
  , panda.LinAlg.Gauss
  , panda.LinAlg.Base
  , panda.LinAlg.NumericF64
  ;

type
  /// <summary>
  ///   General Gaussian elimination for Double, including rectangular and
  ///   rank-deficient systems. Returns a particular solution and null-space basis.
  ///   Init copies inputs; InitWithoutCopy aliases writable row-contiguous storage.
  ///   Solve consumes initialization; results retain independent storage.
  ///   Arithmetic and allocation exceptions propagate.
  /// </summary>
  /// <remarks>
  ///   Pivots select the maximum absolute value. Values at or below the prepared
  ///   threshold are zero. Automatic thresholds are factor * Max(rows, columns)
  ///   * cEpsF64 * max magnitude, separately for A and b. Homogeneity is exact.
  ///   Inputs and intermediate values must be supported by scalar arithmetic.
  /// </remarks>
  TGaussElimF64 = class(TGaussElim<Double>)
  public const
    /// <summary>Default safety factor for automatic tolerance estimation.</summary>
    cDefaultTolFactor = TLinAlgNumericF64.cDefaultTolFactor;
  protected
    function GetTolerance: TGaussTolerance;
    procedure SetTolerance(const aValue: TGaussTolerance);
    function GetZeroTolerance: Double;
    procedure SetZeroTolerance(const aValue: Double);
    function GetToleranceFactor: Double;
    procedure SetToleranceFactor(const aValue: Double);
    function CreateNumeric: TLinAlgNumeric<Double>; override;
  public
    /// <summary>
    ///   Returns the tolerances that <c>Solve</c>
    ///   estimates when <c>Tolerance</c> is not set:
    ///   <c>Pivot = aFactor * Max(rows, columns) * eps * max|A|</c> and
    ///   <c>RHS = aFactor * Max(rows, columns) * eps * max|b|</c>.
    ///   <c>aFactor</c> that is not positive selects <c>cDefaultTolFactor</c>.
    ///   <c>RHS</c> is 0 when <c>ab</c> is nil. Both fields are 0 when
    ///   <c>aA</c> is empty or its rows differ in length.
    ///   The result can be assigned to <c>Tolerance</c> and reused for systems
    ///   whose <c>max|A|</c> and <c>max|b|</c> are of the same magnitude.
    /// </summary>
    class function EstimateTolerance(const aA: TArray<TArray<Double>>;
      const ab: TArray<Double> = nil;
      aFactor: Double = cDefaultTolFactor): TGaussTolerance; overload;
    /// <summary>
    ///   Same as the dynamic array overload. Any 2D layout of <c>aA</c> and any
    ///   stride of <c>ab</c> is accepted. Both fields are 0 when <c>aA</c> is
    ///   not a matrix, and <c>RHS</c> is 0 when <c>ab</c> is not a vector.
    /// </summary>
    class function EstimateTolerance(const aA: INDArray<Double>;
      const ab: INDArray<Double> = nil;
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

function TGaussElimF64.CreateNumeric: TLinAlgNumeric<Double>;
begin
  Result := TLinAlgNumericF64.Create;
end;

function TGaussElimF64.GetTolerance: TGaussTolerance;
begin
  Result := TLinAlgNumericF64(fNumeric).Tolerance;
end;

procedure TGaussElimF64.SetTolerance(const aValue: TGaussTolerance);
begin
  TLinAlgNumericF64(fNumeric).Tolerance := aValue;
end;

function TGaussElimF64.GetZeroTolerance: Double;
begin
  Result := TLinAlgNumericF64(fNumeric).ZeroTolerance;
end;

procedure TGaussElimF64.SetZeroTolerance(const aValue: Double);
begin
  TLinAlgNumericF64(fNumeric).ZeroTolerance := aValue;
end;

function TGaussElimF64.GetToleranceFactor: Double;
begin
  Result := TLinAlgNumericF64(fNumeric).ToleranceFactor;
end;

procedure TGaussElimF64.SetToleranceFactor(const aValue: Double);
begin
  TLinAlgNumericF64(fNumeric).ToleranceFactor := aValue;
end;

class function TGaussElimF64.EstimateTolerance(const aA: TArray<TArray<Double>>;
  const ab: TArray<Double>; aFactor: Double): TGaussTolerance;
begin
  Result := TLinAlgNumericF64.EstimateTolerance(aA, ab, aFactor);
end;

class function TGaussElimF64.EstimateTolerance(const aA: INDArray<Double>;
  const ab: INDArray<Double>; aFactor: Double): TGaussTolerance;
begin
  Result := TLinAlgNumericF64.EstimateTolerance(aA, ab, aFactor);
end;

end.
