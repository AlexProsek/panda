unit panda.LinAlg.NumericRational;

interface

uses
    panda.NumsAP
  , panda.LinAlg.Base
  ;

type
  /// <summary>
  ///   TRational arithmetic, pivot selection, and tolerance preparation.
  ///   Shared by general elimination and LUP; retains no matrix pointers.
  ///   Inputs, including zero, must be explicitly constructed valid rationals.
  ///   Default(TRational) is not zero and is not validated. Arithmetic errors propagate.
  /// </summary>
  TLinAlgNumericRational = class(TLinAlgNumeric<TRational>)
  protected type
    PRational = TLinAlgNumeric<TRational>.PT;
  public
    /// <summary>
    ///   Despite the inherited name, returns the first row I >= aRIdx whose
    ///   column aCIdx is nonzero, stopping at aLastRIdx. Returns aRIdx if every
    ///   candidate is zero, so the caller's exact zero test handles the column.
    /// </summary>
    function ArgMax(const A: TArray<PRational>; aRIdx, aCIdx,
      aLastRIdx: NativeInt): NativeInt; override;
    /// <summary>No-op: aCols and aRHS do not affect exact zero tests.</summary>
    procedure PrepareTolerance(const aRows: TArray<PRational>; aCols: NativeInt; aRHS: Boolean); override;
    function IsZeroValue(const A: TRational): Boolean; override;
    /// <summary>Exact coefficient zero test, using TRational.IsZero.</summary>
    function IsZeroTol(const A: TRational): Boolean; override;
    /// <summary>Exact right-hand-side zero test, using TRational.IsZero.</summary>
    function IsZeroRHSTol(const A: TRational): Boolean; override;
    function One: TRational; override;
    function Zero: TRational; override;
    function Subtract(const A, B: TRational): TRational; override;
    function Divide(const A, B: TRational): TRational; override;
    function Negate(const A: TRational): TRational; override;
    function Multiply(const A, B: TRational): TRational; override;
    function Reciprocal(const A: TRational): TRational; override;
    function SubDiv(const A, B, C: TRational): TRational; override;
    function NegDiv(const A, B: TRational): TRational; override;
    /// <summary>
    ///   Returns the largest exact magnitude, or rational zero for an empty
    ///   vector. Implements the inherited arithmetic hook; not used for pivots.
    /// </summary>
    procedure AbsMax(pData: PRational; aCount: NativeInt; var aMax: TRational); override;
    procedure Axpy(const a: TRational; pX, pY: PRational; aCount: NativeInt); override;
    procedure Dot(pX, pY: PRational; aCount: NativeInt; out aRes: TRational); override;
    procedure VecMul(pX: PRational; const a: TRational; pY: PRational; aCount: NativeInt); override;
  end;

implementation

{$POINTERMATH ON}

{$region 'TLinAlgNumericRational'}

function TLinAlgNumericRational.ArgMax(const A: TArray<PRational>;
  aRIdx, aCIdx, aLastRIdx: NativeInt): NativeInt;
var I: NativeInt;
begin
  for I := aRIdx to aLastRIdx do
    if not A[I][aCIdx].IsZero then exit(I);
  Result := aRIdx;
end;

procedure TLinAlgNumericRational.PrepareTolerance(const aRows: TArray<PRational>; aCols: NativeInt; aRHS: Boolean);
begin
end;

function TLinAlgNumericRational.IsZeroValue(const A: TRational): Boolean;
begin
  Result := A.IsZero;
end;

function TLinAlgNumericRational.IsZeroTol(const A: TRational): Boolean;
begin
  Result := A.IsZero;
end;

function TLinAlgNumericRational.IsZeroRHSTol(const A: TRational): Boolean;
begin
  Result := A.IsZero;
end;

function TLinAlgNumericRational.One: TRational;
begin
  Result := 1;
end;

function TLinAlgNumericRational.Zero: TRational;
begin
  Result := 0;
end;

function TLinAlgNumericRational.Subtract(const A, B: TRational): TRational;
begin
  Result := A - B;
end;

function TLinAlgNumericRational.Divide(const A, B: TRational): TRational;
begin
  Result := A / B;
end;

function TLinAlgNumericRational.Negate(const A: TRational): TRational;
begin
  Result := -A;
end;

function TLinAlgNumericRational.Multiply(const A, B: TRational): TRational;
begin
  Result := A * B;
end;

function TLinAlgNumericRational.Reciprocal(const A: TRational): TRational;
begin
  Result := TRational.Reciprocal(A);
end;

function TLinAlgNumericRational.SubDiv(const A, B, C: TRational): TRational;
begin
  Result := (A - B) / C;
end;

function TLinAlgNumericRational.NegDiv(const A, B: TRational): TRational;
begin
  Result := -(A / B);
end;

procedure TLinAlgNumericRational.AbsMax(pData: PRational; aCount: NativeInt;
  var aMax: TRational);
var I: NativeInt;
    V: TRational;
begin
  aMax := Zero;
  for I := 0 to aCount - 1 do begin
    V := pData[I].Abs;
    if V > aMax then aMax := V;
  end;
end;

procedure TLinAlgNumericRational.Axpy(const a: TRational; pX, pY: PRational;
  aCount: NativeInt);
var I: NativeInt;
begin
  for I := 0 to aCount - 1 do
    pY[I] := pY[I] + a * pX[I];
end;

procedure TLinAlgNumericRational.Dot(pX, pY: PRational; aCount: NativeInt;
  out aRes: TRational);
var I: NativeInt;
    S: TRational;
begin
  S := Zero;
  for I := 0 to aCount - 1 do
    S := S + pX[I] * pY[I];
  aRes := S;
end;

procedure TLinAlgNumericRational.VecMul(pX: PRational; const a: TRational;
  pY: PRational; aCount: NativeInt);
var I: NativeInt;
begin
  for I := 0 to aCount - 1 do
    pY[I] := pX[I] * a;
end;

{$endregion}

end.
