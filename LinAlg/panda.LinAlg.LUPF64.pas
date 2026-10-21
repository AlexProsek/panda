unit panda.LinAlg.LUPF64;

interface

uses
    panda.Intfs
  , panda.LinAlg.LUP
  , panda.LinAlg.Base
  , panda.LinAlg.NumericF64
  ;

type
  /// <summary>
  ///   Ordinary or blocked LUP initialization and repeated cached operations for
  ///   Double. Uses the shared TLinAlgNumericF64 numerical policy.
  ///   Inputs and intermediate values must be valid for the scalar arithmetic;
  ///   they are not scanned for validity. Arithmetic exceptions propagate.
  /// </summary>
  TLUPDecompositionF64 = class(TLUPDecomposition<Double>)
  public const
    /// <summary>Default automatic pivot tolerance safety factor.</summary>
    cDefaultTolFactor = TLinAlgNumericF64.cDefaultTolFactor;
  protected
    function GetPivotTolerance: Double;
    procedure SetPivotTolerance(const aValue: Double);
    function GetToleranceFactor: Double;
    procedure SetToleranceFactor(const aValue: Double);
    function CreateNumeric: TLinAlgNumeric<Double>; override;
  public
    /// <summary>
    ///   Absolute pivot threshold, initially zero. Nonpositive values select
    ///   factor * N * machine epsilon * max magnitude of the original matrix.
    ///   Values at or below this threshold are rejected. No RHS tolerance is used.
    ///   Changes apply at the next initialization; cached factors remain usable.
    /// </summary>
    property PivotTolerance: Double read GetPivotTolerance write SetPivotTolerance;
    /// <summary>
    ///   Automatic tolerance safety factor, initially 100; nonpositive selects
    ///   100. Settings are read at initialization and do not affect cached factors.
    /// </summary>
    property ToleranceFactor: Double read GetToleranceFactor write SetToleranceFactor;
  end;

implementation

function TLUPDecompositionF64.CreateNumeric: TLinAlgNumeric<Double>;
begin
  Result := TLinAlgNumericF64.Create;
end;

function TLUPDecompositionF64.GetPivotTolerance: Double;
begin
  Result := TLinAlgNumericF64(fNumeric).PivotTolerance;
end;

procedure TLUPDecompositionF64.SetPivotTolerance(const aValue: Double);
begin
  TLinAlgNumericF64(fNumeric).PivotTolerance := aValue;
end;

function TLUPDecompositionF64.GetToleranceFactor: Double;
begin
  Result := TLinAlgNumericF64(fNumeric).ToleranceFactor;
end;

procedure TLUPDecompositionF64.SetToleranceFactor(const aValue: Double);
begin
  TLinAlgNumericF64(fNumeric).ToleranceFactor := aValue;
end;

end.
