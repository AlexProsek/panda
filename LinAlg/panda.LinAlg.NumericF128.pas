unit panda.LinAlg.NumericF128;

interface

uses
    panda.NumsQP
  , panda.Intfs
  , panda.LinAlg.Base
  ;

type
  /// <summary>
  ///   Absolute coefficient and right-hand-side tolerances for TGaussElimF128.
  ///   Nonpositive fields are estimated from the data when needed.
  /// </summary>
  TGaussToleranceF128 = record
    /// <summary>Tolerance of pivot and rank tests on the coefficients.</summary>
    Pivot: TReal128;
    /// <summary>Consistency tolerance of Gaussian Solve; unused by LUP initialization.</summary>
    RHS: TReal128;
  end;

  /// <summary>
  ///   TReal128 arithmetic, pivot selection, and tolerance preparation.
  ///   Shared by general elimination and LUP; retains no matrix pointers.
  ///   Uses the numerical range and exceptions of the underlying scalar type.
  /// </summary>
  TLinAlgNumericF128 = class(TLinAlgNumeric<TReal128>)
  public const
    /// <summary>Default value of <c>ToleranceFactor</c>.</summary>
    cDefaultTolFactor = 100;
  protected type
    PReal128 = TLinAlgNumeric<TReal128>.PT;
  protected
    fTol: TGaussToleranceF128;
    fTolFactor: Double;
    fCurTol: TReal128;
    fCurRHSTol: TReal128;
  public
    function ArgMax(const A: TArray<PReal128>; aRIdx, aCIdx,
      aLastRIdx: NativeInt): NativeInt; override;
    procedure PrepareTolerance(const aRows: TArray<PReal128>; aCols: NativeInt; aRHS: Boolean); override;
    function IsZeroValue(const A: TReal128): Boolean; override;
    function IsZeroTol(const A: TReal128): Boolean; override;
    function IsZeroRHSTol(const A: TReal128): Boolean; override;
  protected
    procedure SetPivotTolerance(const aValue: TReal128);
    procedure SetZeroTolerance(const aValue: TReal128);
    class function ScaledTol(aMax: TReal128; aDim: NativeInt; aFactor: Double): TReal128; static;
    class function StridedAbsMax(p: PByte; aStep, aCount: NativeInt): TReal128; static;
  public
    function One: TReal128; override;
    function Zero: TReal128; override;
    function Subtract(const A, B: TReal128): TReal128; override;
    function Divide(const A, B: TReal128): TReal128; override;
    function Negate(const A: TReal128): TReal128; override;
    function Multiply(const A, B: TReal128): TReal128; override;
    function Reciprocal(const A: TReal128): TReal128; override;
    function SubDiv(const A, B, C: TReal128): TReal128; override;
    function NegDiv(const A, B: TReal128): TReal128; override;
    procedure AbsMax(pData: PReal128; aCount: NativeInt; var aMax: TReal128); override;
    procedure Axpy(const a: TReal128; pX, pY: PReal128; aCount: NativeInt); override;
    procedure Dot(pX, pY: PReal128; aCount: NativeInt; out aRes: TReal128); override;
    procedure VecMul(pX: PReal128; const a: TReal128; pY: PReal128; aCount: NativeInt); override;
  public
    /// <summary>Initializes automatic thresholds and the default safety factor.</summary>
    procedure AfterConstruction; override;
    /// <summary>
    ///   Returns the tolerances that Gaussian <c>Solve</c> and LUP initialization
    ///   estimate when <c>Tolerance</c> is not set:
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
    ///   Requested coefficient and consistency thresholds. PrepareTolerance
    ///   estimates nonpositive fields from the supplied rows, before modification.
    /// </summary>
    property Tolerance: TGaussToleranceF128 read fTol write fTol;
    /// <summary>Requested coefficient threshold; leaves RHS settings unchanged.</summary>
    property PivotTolerance: TReal128 read fTol.Pivot write SetPivotTolerance;
    /// <summary>Reads Pivot; writing sets both Pivot and RHS thresholds.</summary>
    property ZeroTolerance: TReal128 read fTol.Pivot write SetZeroTolerance;
    /// <summary>
    ///   Safety factor k used by PrepareTolerance for nonpositive requested
    ///   thresholds. Explicit positive thresholds take precedence.
    ///   A value that is not positive selects <c>cDefaultTolFactor</c>.
    ///   Initial value is <c>cDefaultTolFactor</c>.
    ///   The Double configuration value is converted to TReal128 before the
    ///   tolerance is calculated; matrix and RHS values are not narrowed.
    /// </summary>
    property ToleranceFactor: Double read fTolFactor write fTolFactor;
  end;

implementation

uses
    panda.Arrays
  , System.Math
  ;

{$POINTERMATH ON}

{$region 'TLinAlgNumericF128'}

procedure TLinAlgNumericF128.AfterConstruction;
begin
  inherited;
  fTol := Default(TGaussToleranceF128);
  fTolFactor := cDefaultTolFactor;
  fCurTol := 0;
  fCurRHSTol := 0;
end;

function TLinAlgNumericF128.ArgMax(const A: TArray<PReal128>; aRIdx, aCIdx,
  aLastRIdx: NativeInt): NativeInt;
var I: NativeInt;
    max, tmp: TReal128;
begin
  Result := aRIdx;
  max := A[aRIdx][aCIdx].Abs;
  for I := aRIdx + 1 to aLastRIdx do begin
    tmp := A[I][aCIdx].Abs;
    if tmp > max then begin
      Result := I;
      max := tmp;
    end;
  end;
end;

procedure TLinAlgNumericF128.PrepareTolerance(const aRows: TArray<PReal128>; aCols: NativeInt; aRHS: Boolean);
var I, dim: NativeInt;
    needA, needB: Boolean;
    maxA, maxB, tmp: TReal128;
begin
  fCurTol := fTol.Pivot;
  fCurRHSTol := fTol.RHS;
  needA := fCurTol <= 0;
  needB := aRHS and (fCurRHSTol <= 0);
  if not (needA or needB) then exit;

  maxA := 0;
  maxB := 0;
  for I := 0 to High(aRows) do begin
    if needA and (aCols > 0) then begin
      AbsMax(aRows[I], aCols, tmp);
      if tmp > maxA then maxA := tmp;
    end;
    if needB then begin
      tmp := aRows[I][aCols].Abs;
      if tmp > maxB then maxB := tmp;
    end;
  end;

  dim := Length(aRows);
  if aCols > dim then dim := aCols;
  if needA then
    fCurTol := ScaledTol(maxA, dim, fTolFactor);
  if needB then
    fCurRHSTol := ScaledTol(maxB, dim, fTolFactor);
end;

function TLinAlgNumericF128.IsZeroValue(const A: TReal128): Boolean;
begin
  Result := A.IsExactZero;
end;

function TLinAlgNumericF128.IsZeroTol(const A: TReal128): Boolean;
begin
  Result := A.Abs <= fCurTol;
end;

function TLinAlgNumericF128.IsZeroRHSTol(const A: TReal128): Boolean;
begin
  Result := A.Abs <= fCurRHSTol;
end;

procedure TLinAlgNumericF128.SetPivotTolerance(const aValue: TReal128);
begin
  fTol.Pivot := aValue;
end;

procedure TLinAlgNumericF128.SetZeroTolerance(const aValue: TReal128);
begin
  fTol.Pivot := aValue;
  fTol.RHS := aValue;
end;

class function TLinAlgNumericF128.ScaledTol(aMax: TReal128; aDim: NativeInt; aFactor: Double): TReal128;
begin
  if aFactor <= 0 then aFactor := cDefaultTolFactor;
  Result := TReal128(aFactor) * TReal128(aDim) * cEpsF128 * aMax;
end;

class function TLinAlgNumericF128.StridedAbsMax(p: PByte; aStep, aCount: NativeInt): TReal128;
var I: NativeInt;
    tmp: TReal128;
begin
  Result := 0;
  if aCount <= 0 then exit;
  for I := 0 to aCount - 1 do begin
    tmp := PReal128(p)^.Abs;
    if tmp > Result then Result := tmp;
    Inc(p, aStep);
  end;
end;

class function TLinAlgNumericF128.EstimateTolerance(const aA: TArray<TArray<TReal128>>;
  const ab: TArray<TReal128>; aFactor: Double): TGaussToleranceF128;
var R, C, I: NativeInt;
    maxA, maxB, tmp: TReal128;
begin
  Result := Default(TGaussToleranceF128);
  R := Length(aA);
  if R = 0 then exit;
  C := Length(aA[0]);
  for I := 1 to R - 1 do
    if Length(aA[I]) <> C then exit;

  maxA := 0;
  if C > 0 then
    for I := 0 to R - 1 do begin
      tmp := StridedAbsMax(PByte(@aA[I][0]), SizeOf(TReal128), C);
      if tmp > maxA then maxA := tmp;
    end;
  Result.Pivot := ScaledTol(maxA, Max(R, C), aFactor);

  if Length(ab) > 0 then begin
    maxB := StridedAbsMax(PByte(@ab[0]), SizeOf(TReal128), Length(ab));
    Result.RHS := ScaledTol(maxB, Max(R, C), aFactor);
  end;
end;

class function TLinAlgNumericF128.EstimateTolerance(const aA: INDArray<TReal128>;
  const ab: INDArray<TReal128>; aFactor: Double): TGaussToleranceF128;
var spec: TMatSpec;
    I: NativeInt;
    maxA, maxB, tmp: TReal128;
    pRow: PByte;
begin
  Result := Default(TGaussToleranceF128);
  if not (Assigned(aA) and MatrixQ(aA)) then exit;
  GetMatSpec(aA, spec);

  maxA := 0;
  pRow := spec.Data;
  for I := 0 to spec.NRows - 1 do begin
    tmp := StridedAbsMax(pRow, spec.CStep, spec.NCols);
    if tmp > maxA then maxA := tmp;
    Inc(pRow, spec.RStep);
  end;
  Result.Pivot := ScaledTol(maxA, Max(spec.NRows, spec.NCols), aFactor);

  if Assigned(ab) and VectorQ(ab) then begin
    maxB := StridedAbsMax(ab.Data, ab.Strides[0], ab.Size);
    Result.RHS := ScaledTol(maxB, Max(spec.NRows, spec.NCols), aFactor);
  end;
end;

function TLinAlgNumericF128.One: TReal128;
begin
  Result := cOneF128;
end;

function TLinAlgNumericF128.Zero: TReal128;
begin
  Result := cZeroF128;
end;

function TLinAlgNumericF128.Subtract(const A, B: TReal128): TReal128;
begin
  Result := A - B;
end;

function TLinAlgNumericF128.Divide(const A, B: TReal128): TReal128;
begin
  Result := A / B;
end;

function TLinAlgNumericF128.Negate(const A: TReal128): TReal128;
begin
  Result := -A;
end;

function TLinAlgNumericF128.Multiply(const A, B: TReal128): TReal128;
begin
  Result := A * B;
end;

function TLinAlgNumericF128.Reciprocal(const A: TReal128): TReal128;
begin
  Result := A.Reciprocal;
end;

function TLinAlgNumericF128.SubDiv(const A, B, C: TReal128): TReal128;
begin
  Result := (A - B) / C;
end;

function TLinAlgNumericF128.NegDiv(const A, B: TReal128): TReal128;
begin
  Result := -(A / B);
end;

procedure TLinAlgNumericF128.AbsMax(pData: PReal128; aCount: NativeInt; var aMax: TReal128);
var I: NativeInt;
    V: TReal128;
begin
  aMax := cZeroF128;
  for I := 0 to aCount - 1 do begin
    V := pData[I].Abs;
    if V > aMax then aMax := V;
  end;
end;

procedure TLinAlgNumericF128.Axpy(const a: TReal128; pX, pY: PReal128; aCount: NativeInt);
var I: NativeInt;
begin
  for I := 0 to aCount - 1 do
    pY[I] := pY[I] + a * pX[I];
end;

procedure TLinAlgNumericF128.Dot(pX, pY: PReal128; aCount: NativeInt; out aRes: TReal128);
var I: NativeInt;
    S: TReal128;
begin
  S := cZeroF128;
  for I := 0 to aCount - 1 do
    S := S + pX[I] * pY[I];
  aRes := S;
end;

procedure TLinAlgNumericF128.VecMul(pX: PReal128; const a: TReal128; pY: PReal128; aCount: NativeInt);
var I: NativeInt;
begin
  for I := 0 to aCount - 1 do
    pY[I] := pX[I] * a;
end;

{$endregion}

end.
