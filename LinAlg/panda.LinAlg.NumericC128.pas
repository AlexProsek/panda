unit panda.LinAlg.NumericC128;

interface

uses
    panda.Nums
  , panda.Intfs
  , panda.LinAlg.Base
  ;

type
  /// <summary>
  ///   TCmplx128 arithmetic, pivot selection, and tolerance preparation.
  ///   Shared by general elimination and LUP; retains no matrix pointers.
  ///   Uses the numerical range and exceptions of the underlying scalar type.
  /// </summary>
  TLinAlgNumericC128 = class(TLinAlgNumeric<TCmplx128>)
  public const
    /// <summary>Default value of <c>ToleranceFactor</c>.</summary>
    cDefaultTolFactor = 100;
  protected type
    PCmplx128 = TLinAlgNumeric<TCmplx128>.PT;
  protected
    fTol: TGaussTolerance;
    fTolFactor: Double;
    fCurTol: Double;
    fCurRHSTol: Double;
  public
    function ArgMax(const A: TArray<PCmplx128>; aRIdx, aCIdx,
      aLastRIdx: NativeInt): NativeInt; override;
    procedure PrepareTolerance(const aRows: TArray<PCmplx128>; aCols: NativeInt; aRHS: Boolean); override;
    function IsZeroValue(const A: TCmplx128): Boolean; override;
    function IsZeroTol(const A: TCmplx128): Boolean; override;
    function IsZeroRHSTol(const A: TCmplx128): Boolean; override;
  protected
    procedure SetPivotTolerance(const aValue: Double);
    procedure SetZeroTolerance(const aValue: Double);
    class function ScaledTol(aMax: Double; aDim: NativeInt; aFactor: Double): Double; static;
    class function StridedAbsMax(p: PByte; aStep, aCount: NativeInt): Double; static;
  public
    function One: TCmplx128; override;
    function Zero: TCmplx128; override;
    function Subtract(const A, B: TCmplx128): TCmplx128; override;
    function Divide(const A, B: TCmplx128): TCmplx128; override;
    function Negate(const A: TCmplx128): TCmplx128; override;
    function Multiply(const A, B: TCmplx128): TCmplx128; override;
    function Reciprocal(const A: TCmplx128): TCmplx128; override;
    function SubDiv(const A, B, C: TCmplx128): TCmplx128; override;
    function NegDiv(const A, B: TCmplx128): TCmplx128; override;
    procedure AbsMax(pData: PCmplx128; aCount: NativeInt; var aMax: TCmplx128); override;
    procedure Axpy(const a: TCmplx128; pX, pY: PCmplx128; aCount: NativeInt); override;
    procedure Dot(pX, pY: PCmplx128; aCount: NativeInt; out aRes: TCmplx128); override;
    procedure VecMul(pX: PCmplx128; const a: TCmplx128; pY: PCmplx128; aCount: NativeInt); override;
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

implementation

uses
    panda.Arrays
  , panda.cvArithmetic
  , System.Math
  ;

{$POINTERMATH ON}

{$region 'TLinAlgNumericC128'}

procedure TLinAlgNumericC128.AfterConstruction;
begin
  inherited;
  fTol := Default(TGaussTolerance);
  fTolFactor := cDefaultTolFactor;
  fCurTol := 0;
  fCurRHSTol := 0;
end;

function TLinAlgNumericC128.ArgMax(const A: TArray<PCmplx128>; aRIdx, aCIdx,
  aLastRIdx: NativeInt): NativeInt;
var I: NativeInt;
    max, tmp: Double;
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

procedure TLinAlgNumericC128.PrepareTolerance(const aRows: TArray<PCmplx128>; aCols: NativeInt; aRHS: Boolean);
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
      tmp := StridedAbsMax(PByte(aRows[I]), SizeOf(TCmplx128), aCols);
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

function TLinAlgNumericC128.IsZeroValue(const A: TCmplx128): Boolean;
begin
  Result := A.IsExactZero;
end;

function TLinAlgNumericC128.IsZeroTol(const A: TCmplx128): Boolean;
begin
  Result := A.Abs <= fCurTol;
end;

function TLinAlgNumericC128.IsZeroRHSTol(const A: TCmplx128): Boolean;
begin
  Result := A.Abs <= fCurRHSTol;
end;

procedure TLinAlgNumericC128.SetPivotTolerance(const aValue: Double);
begin
  fTol.Pivot := aValue;
end;

procedure TLinAlgNumericC128.SetZeroTolerance(const aValue: Double);
begin
  fTol.Pivot := aValue;
  fTol.RHS := aValue;
end;

class function TLinAlgNumericC128.ScaledTol(aMax: Double; aDim: NativeInt; aFactor: Double): Double;
begin
  if aFactor <= 0 then aFactor := cDefaultTolFactor;
  Result := aFactor * aDim * cEpsF64 * aMax;
end;

class function TLinAlgNumericC128.StridedAbsMax(p: PByte; aStep, aCount: NativeInt): Double;
var I: NativeInt;
    tmp: Double;
begin
  Result := 0;
  if aCount <= 0 then exit;
  for I := 0 to aCount - 1 do begin
    tmp := panda.Nums.PCmplx128(p)^.Abs;
    if tmp > Result then Result := tmp;
    Inc(p, aStep);
  end;
end;

class function TLinAlgNumericC128.EstimateTolerance(const aA: TArray<TArray<TCmplx128>>;
  const ab: TArray<TCmplx128>; aFactor: Double): TGaussTolerance;
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
      tmp := StridedAbsMax(PByte(@aA[I][0]), SizeOf(TCmplx128), C);
      if tmp > maxA then maxA := tmp;
    end;
  Result.Pivot := ScaledTol(maxA, Max(R, C), aFactor);

  if Length(ab) > 0 then begin
    maxB := StridedAbsMax(PByte(@ab[0]), SizeOf(TCmplx128), Length(ab));
    Result.RHS := ScaledTol(maxB, Max(R, C), aFactor);
  end;
end;

class function TLinAlgNumericC128.EstimateTolerance(const aA: INDArray<TCmplx128>;
  const ab: INDArray<TCmplx128>; aFactor: Double): TGaussTolerance;
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

function TLinAlgNumericC128.One: TCmplx128;
begin
  Result := TCmplx128.ReUnit;
end;

function TLinAlgNumericC128.Zero: TCmplx128;
begin
  Result := TCmplx128.Zero;
end;

function TLinAlgNumericC128.Subtract(const A, B: TCmplx128): TCmplx128;
begin
  Result := A - B;
end;

function TLinAlgNumericC128.Divide(const A, B: TCmplx128): TCmplx128;
begin
  Result := A / B;
end;

function TLinAlgNumericC128.Negate(const A: TCmplx128): TCmplx128;
begin
  Result := -A;
end;

function TLinAlgNumericC128.Multiply(const A, B: TCmplx128): TCmplx128;
begin
  Result := A * B;
end;

function TLinAlgNumericC128.Reciprocal(const A: TCmplx128): TCmplx128;
begin
  Result := A.Reciprocal;
end;

function TLinAlgNumericC128.SubDiv(const A, B, C: TCmplx128): TCmplx128;
begin
  Result := (A - B) / C;
end;

function TLinAlgNumericC128.NegDiv(const A, B: TCmplx128): TCmplx128;
begin
  Result := -(A / B);
end;

// The base interface returns T; store the maximum modulus in Re.
procedure TLinAlgNumericC128.AbsMax(pData: PCmplx128; aCount: NativeInt; var aMax: TCmplx128);
begin
  aMax := Cmplx(StridedAbsMax(PByte(pData), SizeOf(TCmplx128), aCount), 0);
end;

procedure TLinAlgNumericC128.Axpy(const a: TCmplx128; pX, pY: PCmplx128; aCount: NativeInt);
begin
  panda.cvArithmetic.axpy(a, panda.Nums.PCmplx128(pX), panda.Nums.PCmplx128(pY), aCount);
end;

procedure TLinAlgNumericC128.Dot(pX, pY: PCmplx128; aCount: NativeInt; out aRes: TCmplx128);
begin
  if aCount <= 0 then begin
    aRes := TCmplx128.Zero;
    exit;
  end;
  panda.cvArithmetic.dot(panda.Nums.PCmplx128(pX), panda.Nums.PCmplx128(pY), aCount, aRes);
end;

procedure TLinAlgNumericC128.VecMul(pX: PCmplx128; const a: TCmplx128; pY: PCmplx128; aCount: NativeInt);
begin
  panda.cvArithmetic.VecMul(panda.Nums.PCmplx128(pX), a, panda.Nums.PCmplx128(pY), aCount);
end;

{$endregion}

end.
