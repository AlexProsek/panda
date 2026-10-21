unit panda.LinAlg.NumericF64;

interface

uses
    panda.Intfs
  , panda.LinAlg.Base
  ;

type
  /// <summary>
  ///   Double arithmetic, pivot selection, and tolerance preparation.
  ///   Shared by general elimination and LUP; retains no matrix pointers.
  ///   Uses the numerical range and exceptions of the underlying scalar type.
  /// </summary>
  TLinAlgNumericF64 = class(TLinAlgNumeric<Double>)
  public const
    /// <summary>Default value of <c>ToleranceFactor</c>.</summary>
    cDefaultTolFactor = 100;
  protected type
    PDouble = TLinAlgNumeric<Double>.PT;
  protected
    fTol: TGaussTolerance;
    fTolFactor: Double;
    fCurTol: Double;
    fCurRHSTol: Double;
  public
    function ArgMax(const A: TArray<PDouble>; aRIdx, aCIdx,
      aLastRIdx: NativeInt): NativeInt; override;
    procedure PrepareTolerance(const aRows: TArray<PDouble>; aCols: NativeInt; aRHS: Boolean); override;
    function IsZeroValue(const A: Double): Boolean; override;
    function IsZeroTol(const A: Double): Boolean; override;
    function IsZeroRHSTol(const A: Double): Boolean; override;
  protected
    procedure SetPivotTolerance(const aValue: Double);
    procedure SetZeroTolerance(const aValue: Double);
    class function ScaledTol(aMax: Double; aDim: NativeInt; aFactor: Double): Double; static;
    class function StridedAbsMax(p: PByte; aStep, aCount: NativeInt): Double; static;
  public
    function One: Double; override;
    function Zero: Double; override;
    function Subtract(const A, B: Double): Double; override;
    function Divide(const A, B: Double): Double; override;
    function Negate(const A: Double): Double; override;
    function Multiply(const A, B: Double): Double; override;
    function Reciprocal(const A: Double): Double; override;
    function SubDiv(const A, B, C: Double): Double; override;
    function NegDiv(const A, B: Double): Double; override;
    procedure AbsMax(pData: PDouble; aCount: NativeInt; var aMax: Double); override;
    procedure Axpy(const a: Double; pX, pY: PDouble; aCount: NativeInt); override;
    procedure Dot(pX, pY: PDouble; aCount: NativeInt; out aRes: Double); override;
    procedure VecMul(pX: PDouble; const a: Double; pY: PDouble; aCount: NativeInt); override;
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
    ///   Requested coefficient and consistency thresholds. PrepareTolerance
    ///   estimates nonpositive fields from the supplied rows, before modification.
    /// </summary>
    property Tolerance: TGaussTolerance read fTol write fTol;
    /// <summary>Requested coefficient threshold; leaves RHS settings unchanged.</summary>
    property PivotTolerance: Double read fTol.Pivot write SetPivotTolerance;
    /// <summary>Reads Pivot; writing sets both Pivot and RHS thresholds.</summary>
    property ZeroTolerance: Double read fTol.Pivot write SetZeroTolerance;
    /// <summary>
    ///   Safety factor k used by PrepareTolerance for nonpositive requested
    ///   thresholds. Explicit positive thresholds take precedence.
    ///   A value that is not positive selects <c>cDefaultTolFactor</c>.
    ///   Initial value is <c>cDefaultTolFactor</c>.
    /// </summary>
    property ToleranceFactor: Double read fTolFactor write fTolFactor;
  end;

function IsFinite(const aValue: Double): Boolean; inline;

implementation

uses
    panda.Nums
  , panda.Arrays
  , panda.cvArithmetic
  , panda.cvMath
  , System.Math
  ;

{$POINTERMATH ON}

function IsFinite(const aValue: Double): Boolean;
begin
  Result := not (IsNan(aValue) or IsInfinite(aValue));
end;

{$region 'TLinAlgNumericF64'}

procedure TLinAlgNumericF64.AfterConstruction;
begin
  inherited;
  fTol := Default(TGaussTolerance);
  fTolFactor := cDefaultTolFactor;
  fCurTol := 0;
  fCurRHSTol := 0;
end;

function TLinAlgNumericF64.ArgMax(const A: TArray<PDouble>; aRIdx, aCIdx,
  aLastRIdx: NativeInt): NativeInt;
var I: NativeInt;
    max, tmp: Double;
begin
  Result := aRIdx;
  max := Abs(A[aRIdx][aCIdx]);
  for I := aRIdx + 1 to aLastRIdx do begin
    tmp := Abs(A[I][aCIdx]);
    if tmp > max then begin
      Result := I;
      max := tmp;
    end;
  end;
end;

procedure TLinAlgNumericF64.PrepareTolerance(const aRows: TArray<PDouble>; aCols: NativeInt; aRHS: Boolean);
var I, dim: NativeInt;
    needA, needB: Boolean;
    maxA, maxB, tmp: Double;
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
      tmp := Abs(aRows[I][aCols]);
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

function TLinAlgNumericF64.IsZeroValue(const A: Double): Boolean;
begin
  Result := (A = 0);
end;

function TLinAlgNumericF64.IsZeroTol(const A: Double): Boolean;
begin
  Result := Abs(A) <= fCurTol;
end;

function TLinAlgNumericF64.IsZeroRHSTol(const A: Double): Boolean;
begin
  Result := Abs(A) <= fCurRHSTol;
end;

procedure TLinAlgNumericF64.SetPivotTolerance(const aValue: Double);
begin
  fTol.Pivot := aValue;
end;

procedure TLinAlgNumericF64.SetZeroTolerance(const aValue: Double);
begin
  fTol.Pivot := aValue;
  fTol.RHS := aValue;
end;

class function TLinAlgNumericF64.ScaledTol(aMax: Double; aDim: NativeInt; aFactor: Double): Double;
begin
  if aFactor <= 0 then aFactor := cDefaultTolFactor;
  Result := aFactor * aDim * cEpsF64 * aMax;
end;

class function TLinAlgNumericF64.StridedAbsMax(p: PByte; aStep, aCount: NativeInt): Double;
var I: NativeInt;
    tmp: Double;
begin
  Result := 0;
  if aCount <= 0 then exit;
  if aStep = SizeOf(Double) then begin
    cvAbsMax(System.PDouble(p), aCount, Result);
    exit;
  end;
  for I := 0 to aCount - 1 do begin
    tmp := Abs(System.PDouble(p)^);
    if tmp > Result then Result := tmp;
    Inc(p, aStep);
  end;
end;

class function TLinAlgNumericF64.EstimateTolerance(const aA: TArray<TArray<Double>>;
  const ab: TArray<Double>; aFactor: Double): TGaussTolerance;
var R, C, I: NativeInt;
    maxA, maxB, tmp: Double;
begin
  Result := Default(TGaussTolerance);
  R := Length(aA);
  if R = 0 then exit;
  C := Length(aA[0]);
  for I := 1 to R - 1 do
    if Length(aA[I]) <> C then exit;

  maxA := 0;
  if C > 0 then
    for I := 0 to R - 1 do begin
      cvAbsMax(System.PDouble(@aA[I][0]), C, tmp);
      if tmp > maxA then maxA := tmp;
    end;
  Result.Pivot := ScaledTol(maxA, Max(R, C), aFactor);

  if Length(ab) > 0 then begin
    maxB := StridedAbsMax(PByte(@ab[0]), SizeOf(Double), Length(ab));
    Result.RHS := ScaledTol(maxB, Max(R, C), aFactor);
  end;
end;

class function TLinAlgNumericF64.EstimateTolerance(const aA: INDArray<Double>;
  const ab: INDArray<Double>; aFactor: Double): TGaussTolerance;
var spec: TMatSpec;
    I: NativeInt;
    maxA, maxB, tmp: Double;
    pRow: PByte;
begin
  Result := Default(TGaussTolerance);
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

function TLinAlgNumericF64.One: Double;
begin
  Result := 1.0;
end;

function TLinAlgNumericF64.Zero: Double;
begin
  Result := 0.0;
end;

function TLinAlgNumericF64.Subtract(const A, B: Double): Double;
begin
  Result := A - B;
end;

function TLinAlgNumericF64.Divide(const A, B: Double): Double;
begin
  Result := A / B;
end;

function TLinAlgNumericF64.Negate(const A: Double): Double;
begin
  Result := -A;
end;

function TLinAlgNumericF64.Multiply(const A, B: Double): Double;
begin
  Result := A * B;
end;

function TLinAlgNumericF64.Reciprocal(const A: Double): Double;
begin
  Result := 1.0 / A;
end;

function TLinAlgNumericF64.SubDiv(const A, B, C: Double): Double;
begin
  Result := (A - B) / C;
end;

function TLinAlgNumericF64.NegDiv(const A, B: Double): Double;
begin
  Result := -(A / B);
end;

procedure TLinAlgNumericF64.AbsMax(pData: PDouble; aCount: NativeInt; var aMax: Double);
begin
  cvAbsMax(System.PDouble(pData), aCount, aMax);
end;

procedure TLinAlgNumericF64.Axpy(const a: Double; pX, pY: PDouble; aCount: NativeInt);
begin
  panda.cvArithmetic.axpy(a, System.PDouble(pX), System.PDouble(pY), aCount);
end;

procedure TLinAlgNumericF64.Dot(pX, pY: PDouble; aCount: NativeInt; out aRes: Double);
begin
  panda.cvArithmetic.dot(System.PDouble(pX), System.PDouble(pY), aCount, aRes);
end;

procedure TLinAlgNumericF64.VecMul(pX: PDouble; const a: Double; pY: PDouble; aCount: NativeInt);
begin
  panda.cvArithmetic.VecMul(System.PDouble(pX), a, System.PDouble(pY), aCount);
end;

{$endregion}

end.
