unit panda.LinAlg.LUPF128;

interface

uses
    panda.NumsQP
  , panda.Intfs
  , panda.LinAlg.LUP
  , panda.LinAlg.Base
  , panda.LinAlg.NumericF128
  ;

type
  /// <summary>
  ///   Ordinary or blocked LUP initialization and repeated cached operations for
  ///   TReal128. Uses the shared TLinAlgNumericF128 numerical policy.
  ///   Inputs and intermediate values must be valid for the scalar arithmetic;
  ///   they are not scanned for validity. Arithmetic exceptions propagate.
  /// </summary>
  TLUPDecompositionF128 = class(TLUPDecomposition<TReal128>)
  public const
    /// <summary>Default automatic pivot tolerance safety factor.</summary>
    cDefaultTolFactor = TLinAlgNumericF128.cDefaultTolFactor;
  protected
    function GetPivotTolerance: TReal128;
    procedure SetPivotTolerance(const aValue: TReal128);
    function GetToleranceFactor: Double;
    procedure SetToleranceFactor(const aValue: Double);
    function CreateNumeric: TLinAlgNumeric<TReal128>; override;
  public
    /// <summary>
    ///   Absolute pivot threshold, initially zero. Nonpositive values select
    ///   factor * N * machine epsilon * max magnitude of the original matrix.
    ///   Values at or below this threshold are rejected. No RHS tolerance is used.
    ///   Changes apply at the next initialization; cached factors remain usable.
    /// </summary>
    property PivotTolerance: TReal128 read GetPivotTolerance write SetPivotTolerance;
    /// <summary>
    ///   Automatic tolerance safety factor, initially 100; nonpositive selects
    ///   100. Settings are read at initialization and do not affect cached factors.
    /// </summary>
    property ToleranceFactor: Double read GetToleranceFactor write SetToleranceFactor;
  end;

implementation

function TLUPDecompositionF128.CreateNumeric: TLinAlgNumeric<TReal128>;
begin
  Result := TLinAlgNumericF128.Create;
end;

function TLUPDecompositionF128.GetPivotTolerance: TReal128;
begin
  Result := TLinAlgNumericF128(fNumeric).PivotTolerance;
end;

procedure TLUPDecompositionF128.SetPivotTolerance(const aValue: TReal128);
begin
  TLinAlgNumericF128(fNumeric).PivotTolerance := aValue;
end;

function TLUPDecompositionF128.GetToleranceFactor: Double;
begin
  Result := TLinAlgNumericF128(fNumeric).ToleranceFactor;
end;

procedure TLUPDecompositionF128.SetToleranceFactor(const aValue: Double);
begin
  TLinAlgNumericF128(fNumeric).ToleranceFactor := aValue;
end;

end.
