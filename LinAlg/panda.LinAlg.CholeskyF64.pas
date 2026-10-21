unit panda.LinAlg.CholeskyF64;

interface

uses
    panda.LinAlg.Base
  , panda.LinAlg.Cholesky
  , panda.LinAlg.NumericF64
  ;

type
  /// <summary>
  ///   Dense real Cholesky factorization and repeated solves for Double.
  ///   Finite inputs and intermediates valid for scalar arithmetic are required.
  ///   The upper triangle is ignored, including during tolerance preparation.
  ///   Nonfinite diagonal residuals or square roots raise EInvalidOp.
  /// </summary>
  TCholeskyDecompositionF64 = class(TCholeskyDecomposition<Double>)
  public const
    /// <summary>Default automatic pivot tolerance safety factor.</summary>
    cDefaultTolFactor = TLinAlgNumericF64.cDefaultTolFactor;
  protected
    fPivotTolerance: Double;
    fToleranceFactor: Double;
    fCurrentTolerance: Double;
    function CreateNumeric: TLinAlgNumeric<Double>; override;
    procedure PrepareFactorization; override;
    function TrySqrtPivot(const aValue: Double; out aRoot: Double): Boolean; override;
  public
    /// <summary>Creates the arithmetic provider and selects automatic tolerance.</summary>
    procedure AfterConstruction; override;

    /// <summary>
    ///   Absolute threshold on diagonal residuals before square root, initially
    ///   zero. Nonpositive selects factor * N * machine epsilon * maxAbs(lower
    ///   triangle). Residuals at or below the threshold are rejected.
    ///   Changes apply at the next initialization and preserve cached factors.
    /// </summary>
    property PivotTolerance: Double read fPivotTolerance write fPivotTolerance;
    /// <summary>
    ///   Automatic tolerance factor, initially 100; nonpositive selects 100.
    ///   Changes apply at the next initialization and preserve cached factors.
    /// </summary>
    property ToleranceFactor: Double read fToleranceFactor write fToleranceFactor;
  end;

implementation

uses
    panda.Nums
  , System.Math
  , System.SysUtils
  ;

{$POINTERMATH ON}

procedure TCholeskyDecompositionF64.AfterConstruction;
begin
  inherited;
  fToleranceFactor := cDefaultTolFactor;
end;

function TCholeskyDecompositionF64.CreateNumeric: TLinAlgNumeric<Double>;
begin
  Result := TLinAlgNumericF64.Create;
end;

procedure TCholeskyDecompositionF64.PrepareFactorization;
var I: NativeInt;
    maximum, rowMax, factor: Double;
begin
  fCurrentTolerance := fPivotTolerance;
  if fCurrentTolerance > 0 then exit;

  maximum := 0;
  for I := 0 to High(fRows) do begin
    fNumeric.AbsMax(fRows[I], I + 1, rowMax);
    maximum := Max(maximum, rowMax);
  end;
  factor := fToleranceFactor;
  if factor <= 0 then factor := cDefaultTolFactor;
  fCurrentTolerance := factor * Length(fRows) * cEpsF64 * maximum;
end;

function TCholeskyDecompositionF64.TrySqrtPivot(const aValue: Double; out aRoot: Double): Boolean;
begin
  if not IsFinite(aValue) then
    raise EInvalidOp.Create('Nonfinite Cholesky diagonal residual');

  Result := aValue > fCurrentTolerance;
  if Result then begin
    aRoot := Sqrt(aValue);
    if not IsFinite(aRoot) then
      raise EInvalidOp.Create('Nonfinite Cholesky diagonal root');
  end else
    aRoot := 0;
end;

end.
