unit panda.LinAlg.LUPC128;

interface

uses
    panda.Nums
  , panda.Intfs
  , panda.LinAlg.LUP
  , panda.LinAlg.Base
  , panda.LinAlg.NumericC128
  ;

type
  /// <summary>
  ///   Ordinary or blocked LUP initialization and repeated cached operations for
  ///   TCmplx128. Uses the shared TLinAlgNumericC128 numerical policy.
  ///   Inputs and intermediate values must be valid for the scalar arithmetic;
  ///   they are not scanned for validity. Arithmetic exceptions propagate.
  /// </summary>
  TLUPDecompositionC128 = class(TLUPDecomposition<TCmplx128>)
  public const
    /// <summary>Default automatic pivot tolerance safety factor.</summary>
    cDefaultTolFactor = TLinAlgNumericC128.cDefaultTolFactor;
  protected
    function GetPivotTolerance: Double;
    procedure SetPivotTolerance(const aValue: Double);
    function GetToleranceFactor: Double;
    procedure SetToleranceFactor(const aValue: Double);
    function CreateNumeric: TLinAlgNumeric<TCmplx128>; override;
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

function TLUPDecompositionC128.CreateNumeric: TLinAlgNumeric<TCmplx128>;
begin
  Result := TLinAlgNumericC128.Create;
end;

function TLUPDecompositionC128.GetPivotTolerance: Double;
begin
  Result := TLinAlgNumericC128(fNumeric).PivotTolerance;
end;

procedure TLUPDecompositionC128.SetPivotTolerance(const aValue: Double);
begin
  TLinAlgNumericC128(fNumeric).PivotTolerance := aValue;
end;

function TLUPDecompositionC128.GetToleranceFactor: Double;
begin
  Result := TLinAlgNumericC128(fNumeric).ToleranceFactor;
end;

procedure TLUPDecompositionC128.SetToleranceFactor(const aValue: Double);
begin
  TLinAlgNumericC128(fNumeric).ToleranceFactor := aValue;
end;

end.
