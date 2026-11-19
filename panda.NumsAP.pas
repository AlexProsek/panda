unit panda.NumsAP;

interface

uses
    Math
  , SysUtils
  , Generics.Collections
  , panda.NumsLowLvl
  , panda.cvArithmetic
  ;

{$I AsmDefs.inc}

type
  TInteger = record
  private type
    TSignPair = (spPP, spNP, spPN, spNN); //P - positive, N - negative
  private
    fData: TLimbArray;
  {$ifdef Test}
    fLimbCount: Integer;
  {$endif}
    fSgn: Int8;
    function GetItem(I: Integer): TLimb; inline;
    function GetCount: Integer; inline;
    class function GetSignPair(const A, B: TInteger): TSignPair; static; inline;
    function Hi: TLimb; inline;
    /// <summary>
    ///   Executes long division where <c>A</c> is numerator and <c>B</c> is
    ///   denominator.
    /// </summary>
    /// <returns>
    ///   Returns a quotient in the <c>A</c> and the remainder
    ///   in the <c>B</c>.
    /// </returns>
    /// <remarks>
    ///   <para>
    ///     &#x2022 - it's supposed that length of the <c>B</c> is greather than one
    ///   </para>
    /// </remarks>
    class procedure LongDivMod(var A, B: TInteger); static;
    class procedure ShortDivMod(var A, B: TInteger); static;
    class function AbsCompare(const A, B: TInteger): Integer; static;
    class function StdMul(const A, B: TInteger): TInteger; static;
    /// <summary>
    ///   Trims the leading zeros
    /// </summary>
    procedure TrimLeadingZeros;
    procedure SetSignum(aValue: Int8); inline;
  public
    procedure Init(const aData: TLimbArray; aSgn: Int8 = 1); overload;
    procedure Init(aData: PLimb; aCount: Integer; aSgn: Int8 = 1); overload;
    procedure Init(const aValue: String; aBase: Integer = 10); overload;
    procedure Init(aValue: Integer); overload;
    class operator Add(const A, B: TInteger): TInteger;
    class operator Subtract(const A, B: TInteger): TInteger;
    class operator Negative(const A: TInteger): TInteger; inline;
    class operator Multiply(const A, B: TInteger): TInteger;
    class operator IntDivide(const A, B: TInteger): TInteger;
    class operator Modulus(const A, B: TInteger): TInteger;
    /// <remarks>
    ///  Function calculates <c>Result := A + 1</c>. It is not possible to increment
    ///  source data due to using dynamic array. It is not possible to change data which
    ///  can be shared with another varible.
    /// </remarks>
    class operator Inc(const A: TInteger): TInteger; inline;
    class operator Dec(const A: TInteger): TInteger; inline;
    class operator Equal(const A, B: TInteger): Boolean;
    class operator LessThan(const A, B: TInteger): Boolean; inline;
    class operator LessThanOrEqual(const A, B: TInteger): Boolean; inline;
    class operator GreaterThan(const A, B: TInteger): Boolean; inline;
    class operator GreaterThanOrEqual(const A, B: TInteger): Boolean; inline;
    class operator RightShift(const A: TInteger; const B: Integer): TInteger;
    class operator LeftShift(const A: TInteger; const B: Integer): TInteger;
    class operator BitwiseAnd(const A: TInteger; const B: TInteger): TInteger;
    class operator BitwiseOr(const A: TInteger; const B: TInteger): TInteger;
    class operator BitwiseXor(const A: TInteger; const B: TInteger): TInteger;
    class operator Implicit(A: Integer): TInteger; inline;
    class operator Implicit(const A: Int64): TInteger;
    class operator Implicit(const A: UInt64): TInteger;
    class operator Implicit(const A: TLimbArray): TInteger; inline;
    class operator Implicit(const A: String): TInteger;
    class operator Explicit(const A: TInteger): Double;
    /// <summary>
    ///   Converts number to <c>Int64</c>. If number is greather than <c>Int64</c>
    ///   then it will be truncated. This method is primary intended for testing.
    /// </summary>
    class operator Explicit(const A: TInteger): Int64;
    class operator Explicit(const A: TInteger): UInt64;
  {$ifdef FPC}
    class operator Explicit(const A: TInteger): Integer;
  {$endif}
    function TryAsNativeInt(out aValue: NativeInt): Boolean;
    function TryAsUInt64(out aValue: UInt64): Boolean;
    function ToBinString: String;
    function ToHexString: String;
    function ToDecimalString: String;
    function ToString: String;
    class function HexParse(const aValue: String; out aInt: TInteger): Boolean; static;
    class function DecimalParse(const aValue: String; out aInt: TInteger): Boolean; static;
    class function TryParse(const aValue: String; out aInt: TInteger; aBase: Integer = 10): Boolean; static;
    function GetData: TLimbArray; inline;
    function GetDataPtr: PLimb; inline;
    class function Zero: TInteger; static;
    function IsZero: Boolean; inline;
    function IsOne: Boolean; inline;
    function IsMinusOne: Boolean; inline;
    function IsAbsOne: Boolean; inline;
    /// <returns>
    ///   If <c>A &lt; B</c> then result is -1 else if
    ///   <c>A = B</c> then result is 0 else if <c>A &gt; B</c> then result is 1.
    /// </returns>
    class function Compare(const A, B: TInteger): Integer; static;
    class function Floor(const A: Double): TInteger; static;
    class function Ceil(const A: Double): TInteger; static;
    class function GCD(const A, B: TInteger): TInteger; static;
    class function Power(const A: TInteger; aExponent: Integer): TInteger; overload; static;
    /// <summary>
    ///   Returns <c>(-1)^aExponent</c>
    /// </summary>
    class function PowerOfMinusOne(const aExponent: TInteger): Integer; inline; static;
    class procedure DivMod(const A, B: TInteger; var Q, R: TInteger); static;
    function IsEven: Boolean; inline;
    function IsOdd: Boolean; inline;
    function HighestBitPos: Integer;
    function CountLeadingZeros: Integer; inline;
    function DigitCount: Integer; inline;
    function HasNativeIntPrecision: Boolean; inline;
    class function NativeMaxValuePlusOne: TInteger; static;
    function Abs: TInteger;

    property Item[I: Integer]: TLimb read GetItem; default;
    property Items: TLimbArray read fData;
    property Count: Integer read {$ifdef Test}fLimbCount;{$else}GetCount;{$endif}
    property Signum: Int8 read fSgn write SetSignum;
  end;

  TKaratsubaMul = record
  private
    fFastMulThreshold: Integer;
    function AbsCmp(pA, pB: PLimb; N: NativeInt): Integer;
    function IsZero(pA: PLimb; N: NativeInt): Boolean;
    procedure ShortMul(pA, pB, pRes: PByte; N: NativeInt);
    procedure Mul(pA, pB, pRes: PByte; N: NativeInt);
  public
    function Eval(const A, B: TInteger): TInteger;
    function Eval2(const A, B: TInteger): TInteger;

  /// <Summary>
  ///   If length of both arguments reaches this limit then the Karatsuba multiplication will be used
  /// </Summary>
    property FastMulThreshold: Integer read fFastMulThreshold write fFastMulThreshold;
  end;

  TRational = record
  private
    fNum: TLimbArray;
    fDenom: TLimbArray;
    fSgn: Integer;
    function GetNumerator: TInteger; inline;
    function GetDenominator: TInteger; inline;
  public
    procedure Create(const aNum, aDenom: TLimbArray; aSgn: Integer = 1); overload;
    procedure Create(const aNum, aDenom: UInt64; aSgn: Integer = 1); overload;
    procedure Create(const aNum, aDenom: TInteger); overload;
    class function Fraction(const aNum, aDenom: TInteger): TRational; inline; static;
    class operator Add(const A, B: TRational): TRational; overload;
    class operator Subtract(const A, B: TRational): TRational;
    class operator Multiply(const A, B: TRational): TRational;
    class operator Divide(const A, B: TRational): TRational;
    class operator Negative(const A: TRational): TRational;
    class operator Implicit(const A: TInteger): TRational; overload;
    class operator Implicit(const A: Int64): TRational; overload;
    class operator Explicit(const A: TRational): Double;
    class operator LessThan(const A, B: TRational): Boolean;
    class operator GreaterThan(const A, B: TRational): Boolean;
    class operator LessThanOrEqual(const A, B: TRational): Boolean;
    class operator GreaterThanOrEqual(const A, B: TRational): Boolean;
    class operator Equal(const A, B: TRational): Boolean;
    class operator NotEqual(const A, B: TRational): Boolean;
    class function Reciprocal(const aValue: TInteger): TRational; overload; static;
    class function Reciprocal(const aValue: TRational): TRational; overload; static;
    class function Compare(const A, B: TRational): Integer; static;
    function IsZero: Boolean; inline;
    function IsOne: Boolean; inline;
    function IsMinusOne: Boolean; inline;
    function IsInteger: Boolean; inline;
    function ToString: String;
    procedure Parse(const aValue: String);
    function Round: TInteger;
    function Floor: TInteger;
    function Abs: TRational;

    property Signum: Integer read fSgn;
    property Numerator: TInteger read GetNumerator;
    property Denominator: TInteger read GetDenominator;
  end;

  TReal = record
  private const
    wSz   = SizeOf(TLimb);
  {$ifdef Limb64}
    cMinLimbCount = 1;
  {$else}
    cMinLimbCount = 2;
  {$endif}
  private
    fSgn: Int8;
    fFraction: TLimbArray;
    fExponent: Integer;
    class function AddPositive(const A, B: TReal): TReal; static;
    class function SubtractPositive(const A, B: TReal): TReal; static;
    class procedure ChangePrecision(aSrc: PByte; aSrcLen: Integer;
      aDst: PByte; aDstLen: Integer); static;
    class function BinRoundTo(aSrc: PByte; aSrcLen: Integer;
      aDst: PByte; aDstLen: Integer; aDigits: UInt64): Integer; static;
    class function BinFloorTo(aSrc: PByte; aSrcLen: Integer;
      aDst: PByte; aDstLen: Integer; aDigits: UInt64): Integer; static;
    class function BinCeilTo(aSrc: PByte; aSrclen: Integer;
      aDst: PByte; aDstLen: Integer; aDigits: UInt64): Integer; static;
    class function TrimTail(const aFraction: TLimbArray; aCount: Integer): TLimbArray; static;
    class procedure ShiftDigits(pFraction: PLimb; aCount: Integer; aDigits: Integer); static;
    class function Normalize(aSrc: PByte; aSrcLen: Integer): Integer; overload; static;
    class function Normalize(aSrc, aDst: PByte; aSrcLen: Integer): Integer; overload; static;
    class function AbsCompare(const A, B: TReal): Integer; static;
  {$region 'Getters/Setters'}
    function GetSignum: Integer; inline;
    procedure SetSignum(aValue: Integer); inline;
  {$endregion}
  public
    procedure Init(const aValue: Double; aDigits: Integer = -1); overload;
    procedure Init(const aFraction: TLimbArray; aExponent: Integer = 0; aSgn: Integer = 1); overload;
    procedure Init(const aValue: TInteger; aDigits: Integer = -1); overload;
    procedure Init(const aValue: TRational; aDigits: Integer = -1); overload;
    procedure InitRational(const aNumerator, aDenominator: TInteger; aDigits: Integer = 0);
    class operator Add(const A, B: TReal): TReal;
    class operator Add(const A: TInteger; const B: TReal): TReal; inline;
    class operator Add(const A: TReal; const B: TInteger): TReal; inline;
    class operator Add(const A: TRational; const B: TReal): TReal; inline;
    class operator Add(const A: TReal; const B: TRational): TReal; inline;
    class operator Subtract(const A, B: TReal): TReal; inline;
    class operator Subtract(const A: TInteger; const B: TReal): TReal; inline;
    class operator Subtract(const A: TReal; const B: TInteger): TReal; inline;
    class operator Subtract(const A: TRational; const B: TReal): TReal; inline;
    class operator Subtract(const A: TReal; const B: TRational): TReal; inline;
    class operator Negative(const A: TReal): TReal; inline;
    class operator Multiply(const A, B: TReal): TReal;
    class operator Multiply(const A: TInteger; const B: TReal): TReal; inline;
    class operator Multiply(const A: TReal; const B: TInteger): TReal; inline;
    class operator Divide(const A, B: TReal): TReal; inline;
    class operator Divide(const A: TInteger; const B: TReal): TReal; inline;
    class operator Divide(const A: TReal; const B: TInteger): TReal; inline;
  {$ifdef FPC}
    class operator Multiply(A: Integer; const B: TReal): TReal; overload;
    class operator Divide(const A: TReal; B: Integer): TReal; overload;
  {$endif}
    class operator Implicit(const A: Double): TReal; overload;
    class operator Explicit(const A: NativeInt): TReal; overload;
    class operator Explicit(const A: TInteger): TReal; overload;
    class operator Explicit(const A: TReal): Double; overload;
    class function Compare(const A, B: TReal): Integer; overload; static;
    class function Compare(const A: TInteger; const B: TReal; aExactly: Boolean = False): Integer; overload; static;
    class function Compare(const A: TReal; const B: TInteger; aExactly: Boolean = False): Integer; overload; static;
    class operator LessThan(const A, B: TReal): Boolean;
    class operator GreaterThan(const A, B: TReal): Boolean;
    class operator LessThanOrEqual(const A, B: TReal): Boolean;
    class operator GreaterThanOrEqual(const A, B: TReal): Boolean;
    class operator Equal(const A, B: TReal): Boolean; overload;
    class operator Equal(const A: TInteger; const B: TReal): Boolean; overload;
    class operator Equal(const A: TReal; const B: TInteger): Boolean; overload;
    class operator Equal(const A: TRational; const B: TReal): Boolean; overload;
    class operator Equal(const A: TReal; const B: TRational): Boolean; overload;
    class operator NotEqual(const A, B: TReal): Boolean;
    function AsDouble: Double; inline;
    function TryAsDouble(out aValue: Double): Boolean;
    function Abs: TReal; inline;
    procedure SetPrecision(const aValue: Double); inline;
    procedure SetBinPrecision(const aDigits: Integer); inline;
    function BinaryRoundTo(aDigits: Int64): TReal;
    function BinaryFloorTo(aDigits: Int64): TReal;
    function BinaryCeilTo(aDigits: Int64): TReal;
    function Reciprocal: TReal; inline;
    function IntPower(aExponent: Integer): TReal; overload;
    function IntPower(aExponent: TInteger): TReal; overload;
    function IntDivide(const B: TInteger): TReal;
    class function SchoolDiv(const A, B: TReal): TReal; static;
    class function NewtonDiv(const A, B: TReal): TReal; static;
    function IntegerPart: TInteger;
    function FractionalPart: TReal;
    class function One(aDigitCount: Integer): TReal; static;
    class function Zero(aDigitCount: Integer): TReal; static;
    function IsZeroQ: Boolean; inline;
    function ToDecimalString: String;
    class function TryParse(const aValue: String; out aRes: TReal;
      aBase: Integer): Boolean; overload; static;
    class function TryParse(const aValue: String; out aRes: TReal;
      out aDigitCount: Integer; aBase: Integer): Boolean; overload; static;
    function BinDigitCount: Integer; inline;

    // arithmetic operators without rounding - they are used in some iterative where rounding is unnecessary
    class procedure _NRAddPos(const A, B: TReal; var aRes: TReal; aLimbCount: Integer); static;
    class procedure _NRSubPos(const A, B: TReal; var aRes: TReal; aLimbCount: Integer); static;
    class procedure _NRAdd(const A, B: TReal; var aRes: TReal; aLimbCount: Integer); static;
    class procedure _NRSub(const A, B: TReal; var aRes: TReal; aLimbCount: Integer); static; inline;
    class procedure _NRMul(const A, B: TReal; var aRes: TReal; aLimbCount: Integer); static;

    property Signum: Integer read GetSignum write SetSignum;
    property Fraction: TLimbArray read fFraction;
    property Exponent: Integer read fExponent write fExponent;
  end;

  TCmplx = record
  public
    Re, Im: TReal;
    procedure Init(const aRe, aIm: TReal); inline;
    class operator Add(const A, B: TCmplx): TCmplx; inline;
    class operator Add(const A: TCmplx; const B: TReal): TCmplx; inline;
    class operator Subtract(const A, B: TCmplx): TCmplx; inline;
    class operator Subtract(const A: TCmplx; const B: TReal): TCmplx; inline;
    class operator Negative(const A: TCmplx): TCmplx; inline;
    class operator Multiply(const A, B: TCmplx): TCmplx; inline;
    class operator Multiply(const A: TReal; const B: TCmplx): TCmplx;
    class operator Divide(const A, B: TCmplx): TCmplx; inline;
    class operator Divide(const A: TCmplx; const B: TReal): TCmplx; inline;
    function Conjugate: TCmplx; inline;
    function Reciprocal: TCmplx; inline;
    function Abs: TReal;
  end;

function RealFloor(const aA: TReal): TReal;
function RealCeil(const aA: TReal): TReal;
function RealSqrt(const aA: TReal): TReal;
function RealPower(const aA: TReal; aB: TInteger): TReal;

{$region 'Helper functions'}

function NewtonIntDiv(const A, B: TInteger): TInteger;

{$endregion}

type
  EInvalidNumString = class(Exception);

  TMethodThresholds = record
    KaratsubaMul: Integer;
    NewtonDiv:    Integer;
  end;

var
  g_MThresholds: TMethodThresholds;

implementation

{$region 'Helper functions'}

function NewtonIntDiv(const A, B: TInteger): TInteger;
begin
  Result := (TReal(A) * TReal(B).Reciprocal).IntegerPart;
end;

{$endregion}

{$region 'Math functions'}

function RealFloor(const aA: TReal): TReal;
begin
  if aA.Exponent >= 0 then
    Result := aA.BinaryFloorTo(aA.Exponent + 1)
  else begin
    if aA.Signum > 0 then
      Result := TReal.Zero(aA.BinDigitCount)
    else begin
      Result := TReal.One(aA.BinDigitCount);
      Result.Signum := -1;
    end;
  end;
end;

function RealCeil(const aA: TReal): TReal;
begin
  if aA.Exponent >= 0 then
    Result := aA.BinaryCeilTo(aA.Exponent + 1)
  else begin
    if aA.Signum > 0 then
      Result := TReal.One(aA.BinDigitCount)
    else
      Result := TReal.Zero(aA.BinDigitCount);
  end;
end;

function RealSqrt(const aA: TReal): TReal;
var p, pw, ea: Integer;
    A, xn, xnn, t, c0, c1: TReal;
begin
  Assert(aA.Signum > 0);
  if aA.IsZeroQ then exit(aA);

  p := aA.BinDigitCount;
  pw := p + W_BIT_COUNT;
  ea := aA.Exponent;
  c0.Init(0.5, pw);
  A := aA.BinaryRoundTo(pw);
  A.Exponent := ea mod 2;
  xn.Init(Sqrt(A.AsDouble), pw);
  c1 := A;
  c1.Exponent := A.Exponent - 1;
  pw := -p - 2;
  repeat
    xnn := c0 * xn + c1 / xn;
    t := (xnn - xn);
    xn := xnn;
  until t.IsZeroQ or (t.Exponent < pw);

  Result := xn.BinaryRoundTo(p);
  Result.Exponent := Result.Exponent + (ea div 2);
end;

function RealPower(const aA: TReal; aB: TInteger): TReal;
begin
  if aB.Signum > 0 then
    Result := aA.IntPower(aB)
  else begin
    if aB.Abs.IsOne then
      Result := aA.Reciprocal
    else
      Result := aA.IntPower(aB.Abs).Reciprocal;
  end;
end;

{$endregion}


{$region 'low-level functions'}

procedure _Sub(pA, pB, pRes: Pointer; n: NativeInt);
{$if defined(ASMx86)}
asm
  //pA -> EAX, pB -> EDX, pRes -> ECX, n -> [ESP]
  push ebx
  push esi
  push edi
  mov esi, edx
  mov edi, ecx
  mov ecx, n
  xor edx, edx
  //pA -> EAX, pB -> ESI, pRes -> EDI, n -> ECX
@L:
  bt edx, 0 //set the carry flag
  mov ebx, [eax]
  sbb ebx, [esi]
  mov edx, 0
  adc edx, 0
  mov [edi], ebx
  add eax, 4
  add esi, 4
  add edi, 4
  dec ecx
  jnz @L

  pop edi
  pop esi
  pop ebx
end;
{$elseif defined(ASMx64)}
asm
  // pA -> RCX, pB -> RDX, pRes -> R8, n -> R9
  mov r10, rcx
  mov r11, rdx
  mov rcx, n
  // pA -> R10, pB -> R11, pRes -> R8, n -> RCX
@L:
  bt rdx, 0 // set the carry flag
  mov rax, [r10]
  sbb rax, [r11]
  mov rdx, 0
  adc rdx, 0
  mov [r8], rax
  add r10, 8
  add r11, 8
  add R8, 8
  dec rcx
  jnz @L
end;
{$else}
var tmp: UInt64;
    pair: array [0..1] of Cardinal absolute tmp;
    carry: Cardinal;
    pAc, pBc, pOut: PCardinal;
    pEnd: PByte;
begin
  carry := 0;
  pAc := pA;
  pBc := pB;
  pOut := pRes;
  pEnd := PByte(pA) + n * SizeOf(Cardinal);
  while PByte(pAc) < pEnd do begin
    tmp := pAc^;
    pair[1] := 1;
    Dec(tmp, pBc^);
    Dec(tmp, carry);
    pOut^ := pair[0];
    carry := 1 xor pair[1];
    Inc(pAc);
    Inc(pBc);
    Inc(pOut);
  end;
end;
{$endif}

{$endregion}

{$region 'TInteger'}

function TInteger.Abs: TInteger;
begin
  Result.fData := Self.fData;
  Result.fSgn := 1;
end;

class operator TInteger.Add(const A, B: TInteger): TInteger;
var pA, pB: Pointer;
    nA, nB, n : Integer;
    data: TLimbArray;
    sp: TSignPair;
begin
  sp := GetSignPair(A, B);
  case sp of
    spPN: Result := A.Abs - B.Abs;
    spNP: Result := B.Abs - A.Abs;
    spPP, spNN: begin
      nA := A.Count;
      nB := B.Count;
      n := Max(nA, nB);
      SetLength(data, n);
      if nA >= nB then begin
        pA := A.fData;
        pB := data;
        Move(B.fData[0], data[0], nB * cLimbSize);
      end else begin
        pA := B.fData;
        pB := data;
        Move(A.fData[0], data[0], nA * cLimbSize);
      end;
      nB := _ipAdd(pA, pB, n);
      if nB > 0 then begin
        SetLength(data, n + 1);
        data[n] := 1;
      end;
      Result.fData := data;
      if sp = spNN then Result.fSgn := -1
      else Result.fSgn := 1;
    end;
  else
    raise EArgumentOutOfRangeException.CreateFmt('TInteger.TSignPair(%d) is ' +
      'out of range.', [Ord(sp)]);
  end;
end;

procedure TInteger.Init(const aData: TLimbArray; aSgn: Int8);
begin
  fData := aData;
  TrimLeadingZeros;
  fSgn := IfThen(aSgn >= 0, 1, -1);
end;

procedure TInteger.Init(aData: PLimb; aCount: Integer; aSgn: Int8);
var data: TLimbArray;
begin
  Assert(aCount > 0);
  SetLength(data, aCount);
  Move(aData^, data[0], aCount * W_BYTE_COUNT);
  Init(data, aSgn);
end;

procedure TInteger.Init(const aValue: String; aBase: Integer);
begin
  if not TryParse(aValue, Self, aBase) then
    raise EInvalidNumString.CreateFmt(
      'TInteger.Init: %s is not valid integer form in base %d',
      [aValue, aBase]
    );
end;

procedure TInteger.Init(aValue: Integer);
begin
  SetLength(fData, 1);
  if aValue and I32_HI_BIT <> 0 then begin //negative number
    fData[0] := -aValue;
    fSgn := -1;
  end else begin
    fData[0] := aValue;
    fSgn := 1;
  end;
{$ifdef Test}
  fLimbCount := 1;
{$endif}
end;

class operator TInteger.IntDivide(const A, B: TInteger): TInteger;
var q, r: TInteger;
begin
  q := A;
  r := B;
  if B.Count = 1 then
    ShortDivMod(q, r)
  else LongDivMod(q, r);
  q.fSgn := A.Signum * B.Signum;
  Result := q;
end;

class operator TInteger.Modulus(const A, B: TInteger): TInteger;
var q, r: TInteger;
begin
  q := A;
  r := B;
  if B.Count = 1 then
    ShortDivMod(q, r)
  else LongDivMod(q, r);
  r.fSgn := 1;
  Result := r;
end;

class procedure TInteger.LongDivMod(var A, B: TInteger);
var nA, nB, m, j, pwr: Integer;
    pA, pB: PByte;
    d: TLimb;
    Adata, Bdata, Q: TLimbArray;
begin
  nA := A.Count;
  nB := B.Count;
  m := nA - nB;
  if m < 0 then begin
    B := A;
    A := 0;
    exit;
  end;
  d := HiLimbBaseQuotient(B.Hi);
  pwr := _TopBitPos(d) - 1;

  //normalize A
  SetLength(AData, nA + 1);
  AData[nA] := _shl(PByte(A.fData), PByte(AData), pwr, nA);

  //normalize B
  SetLength(BData, nB + 1);
  BData[nB] := _shl(PByte(B.fData), PByte(BData), pwr, nB);

  pA := PByte(Adata);
  pB := PByte(Bdata);
  SetLength(Q, Max(m + 1, 1));
  _LongDivMod(pA, pB, PByte(Q), nA, nB);

  //denormalize B
  for J := High(AData) downto 0 do
    if AData[J] <> 0 then break;
  SetLength(AData, Max(1, J + 1));
  B.fData := AData;
  B := B shr pwr;
  for J := High(Q) downto 0 do
    if Q[J] <> 0 then break;
  SetLength(Q, Max(1, J + 1));
  A.fData := Q;
end;

class procedure TInteger.ShortDivMod(var A, B: TInteger);
var v, r: TLimb;
    nA: Integer;
    data: TLimbArray;
begin
  Assert(B.Count = 1);
  v := B.fData[0];
  B.fData := nil;
  SetLength(B.fData, 1);
  nA := A.Count;
  SetLength(data, nA);
  r := _ShortDivMod(PByte(A.fData), @v, @data[0], nA);
  if data[High(data)] = 0 then SetLength(data, Max(1, High(data)));
  A.fData := data;
  B.fData[0] := r;
end;

class operator TInteger.Inc(const A: TInteger): TInteger;
begin
  Result := A + 1;
end;

class operator TInteger.Dec(const A: TInteger): TInteger;
begin
  Result := A - 1;
end;

class operator TInteger.Equal(const A, B: TInteger): Boolean;
var n, I: Integer;
begin
  Result := False;
  if A.Signum <> B.Signum then exit;
  n := A.Count;
  if n <> B.Count then exit;
  for I := n - 1 downto 0 do
    if A.fData[I] <> B.fData[I] then exit;
  Result := True;
end;

class operator TInteger.Explicit(const A: TInteger): UInt64;
begin
  case A.Count of
  0: Result := 0;
  1: Result := A.fData[0];
  else
    Move(A.fData[0], Result, SizeOf(UInt64));
  end;
end;

{$ifdef FPC}

class operator TInteger.Explicit(const A: TInteger): Integer;
begin
  case A.Count of
    0: Result := 0;
  else
    Move(A.fData[0], Result, SizeOf(Integer));
	end;
end;

{$endif}

function TInteger.TryAsNativeInt(out aValue: NativeInt): Boolean;
const niSz = SizeOf(NativeInt);
var sz: Integer;
begin
  sz := Count * cLimbSize;
  if sz <= niSz then begin
    if (Hi and W_SIGN_BIT) <> 0 then exit(False);
{$ifdef Limb64}
    Int64(aValue) := fData[0]; // Limb64 is allowed only under x64 platform
{$else}
    FillChar(aValue, niSz, 0);
    Move(fData[0], aValue, sz);
{$endif}
    if fSgn < 0 then aValue := -aValue;
    Result := True;
  end else
    Result := False;
end;

function TInteger.TryAsUInt64(out aValue: UInt64): Boolean;
var buff: array [0..1] of Cardinal absolute aValue;
begin
  case Count of
    1: begin
      buff[0] := fData[0];
      buff[1] := 0;
    end;
    2: begin
      buff[0] := fData[0];
      buff[1] := fData[1];
    end;
  else
    exit(False);
  end;
  Result := True;
end;

class operator TInteger.Explicit(const A: TInteger): Int64;
begin
  case A.Count of
    0: Result := 0;
    1: Result := A.fData[0];
  else
    Move(A.fdata[0], Result, SizeOf(Int64));
  end;
  if A.fSgn < 0 then Result := -Result;
end;

function TInteger.GetCount: Integer;
begin
  Result := Length(fData);
end;

function TInteger.GetData: TLimbArray;
var len: Integer;
begin
  len := Count;
  SetLength(Result, len);
  Move(fData[0], Result[0], len * cLimbSize);
end;

function TInteger.GetDataPtr: PLimb;
begin
  Result := @fData[0];
end;

function TInteger.GetItem(I: Integer): TLimb;
begin
  Result := fData[I];
end;

class function TInteger.GetSignPair(const A, B: TInteger): TSignPair;
begin
  if A.Signum >= 0 then begin
    if B.Signum >= 0 then Result := spPP
    else Result := spPN;
  end else begin
    if B.Signum >= 0 then Result := spNP
    else Result := spNN;
  end;
end;

function TInteger.Hi: TLimb;
begin
  Result := fData[Count - 1];
end;

function TInteger.HighestBitPos: Integer;
var d: TLimb;
begin
  d := fData[Count - 1];
  Result := _TopBitPos(d);
  Result := Result + (Count - 1) * W_BIT_COUNT;
end;

function TInteger.CountLeadingZeros: Integer;
begin
  Result := _CountLeadingZeros(fData[Count - 1]);
end;

function TInteger.DigitCount: Integer;
begin
  Result := (Count shl W_LOG2_BITS) - _CountLeadingZeros(fData[High(fData)]);
end;

function TInteger.HasNativeIntPrecision: Boolean;
var hi: Integer;
begin
  hi := Count - 1;
  Result := not (
    (hi >= (SizeOf(NativeInt) div 4)) or ((fData[hi] and $80000000) <> 0)
  );
end;

class function TInteger.NativeMaxValuePlusOne: TInteger;
var data: TLimbArray;
{$if defined(CPUX86)} // SizeOf(NativeInt) = 4
begin
  SetLength(data, 1);
  data[1] := $80000000;
  Result.Init(data);
end;
{$elseif defined(CPUX64)} // SizeOf(NativeInt) = 8
{$ifdef Limb64}
begin
  SetLength(data, 1);
  data[1] := $8000000000000000;
  Result.Init(data);
end;
{$else}
begin
  SetLength(data, 2);
  data[0] := $0;
  data[1] := $80000000;
  Result.Init(data);
end;
{$endif}
{$else}
var data: TArray<Cardinal>;
    I, count: Integer;
begin
  count := SizeOf(NativeInt);
  SetLength(data, count);
  for I := 0 to count - 2 do
    data[I] := 0;
  data[count - 1] := $80000000;
  Result.Create(data);
end;
{$endif}

class operator TInteger.Implicit(const A: TLimbArray): TInteger;
begin
  Result.fData := A;
  Result.fSgn := 1;
end;

class operator TInteger.Implicit(A: Integer): TInteger;
begin
  Result.Init(A);
end;

{$ifdef OVERFLOWON}
   {$Q-}
{$endif}
{$ifdef RANGEON}
   {$R-}
{$endif}
class operator TInteger.Implicit(const A: Int64): TInteger;
const hiMask = Int64($7fffffff) shl 32;
      sgnMask = Int64(1) shl 63;
{$ifdef Limb64}
begin
  SetLength(Result.fData, 1);
  if A >= 0 then begin
    Result.fData[0] := A;
    Result.fSgn := 1;
  end else begin
    Result.fData[0] := -A;
    Result.fSgn := -1;
  end;
end;
{$else}
var negA: Int64;
begin
  if A < 0 then begin //negative number
    negA := -A;
    if hiMask and negA <> 0 then begin
      SetLength(Result.fData, 2);
      Move(negA, Result.fData[0], SizeOf(Int64));
    end else begin
      SetLength(Result.fData, 1);
      Move(negA, Result.fData[0], SizeOf(Integer));
    end;
    Result.fSgn := -1;
  end else begin  //positive number
    if hiMask and A <> 0 then begin
      SetLength(Result.fData, 2);
      Move(A, Result.fData[0], SizeOf(Int64));
    end else begin
      SetLength(Result.fData, 1);
      Move(A, Result.fData[0], SizeOf(Integer));
    end;
    Result.fSgn := 1;
  end;
end;
{$endif}
{$ifdef OVERFLOWON}
   {$Q+}
{$endif}
{$ifdef RANGEON}
   {$R+}
{$endif}

class operator TInteger.Implicit(const A: UInt64): TInteger;
const hiMask = UInt64($ffffffff) shl 32;
begin
{$ifdef Limb64}
  SetLength(Result.fData, 1);
  Result.fData[0] := A;
{$else}
  if hiMask and A <> 0 then begin
    SetLength(Result.fData, 2);
    Move(A, Result.fData[0], SizeOf(UInt64));
  end else begin
    SetLength(Result.fData, 1);
    Move(A, Result.fData[0], SizeOf(Integer));
  end;
{$endif}
  Result.fSgn := 1;
end;

class operator TInteger.Implicit(const A: String): TInteger;
begin
  // ToDo: Check string prefix - now is only decimal string supported
  if not DecimalParse(A, Result) then
    raise ENotSupportedException.CreateFmt('TInteger.DecimalParse: Number string format ''%s''' +
      ' is not supported.', [A]);
end;

class operator TInteger.Explicit(const A: TInteger): Double;
var wCnt, d: Integer;
    buff: UInt64 absolute Result;
const FRAC_OFFSET = 52;
begin
  wCnt := A.Count;
{$ifdef Limb64}
  if wCnt = 1 then
    Result := A.fData[0]
  else begin
    d := _CountLeadingZeros(A.fData[wCnt - 1]);
    Result := (A.fData[wCnt - 1] shl d) or ((A.fData[wCnt - 2] shr (W_BIT_COUNT - d)));
    Inc(buff, (((UInt64(wCnt) - 1) shl W_LOG2_BITS) - d) shl FRAC_OFFSET);
  end;
{$else}
  case wCnt of
    1: Result := PCardinal(@A.fData[0])^;
    2: Result := PUInt64(@A.fData[0])^;
  else
    Result := PUInt64(@A.fData[wCnt - 2])^;
//    Result := Result * System.Math.Power(2, 32 * (wCnt - 2));
    Inc(buff, (UInt64(wCnt) - 2) shl 57); // 57 = 52 bits of fraction and 5 bits as 32 multiplication
  end;
{$endif}
  if A.Signum < 0 then
    buff := buff or I64_SIGN_BIT;
end;

class operator TInteger.RightShift(const A: TInteger; const B: Integer): TInteger;
var nA, nB, bOffset: Integer;
    underflow: Boolean;
    pA, pB: PByte;
begin
  if B = 0 then exit(A);

  nA := A.Count;
  nB := nA - (B shr W_LOG2_BITS);
  if nB <= 0 then begin
    SetLength(Result.fData, 1);
    Result.fData[0] := 0;
    Result.fSgn := A.Signum;
    exit;
  end;
  bOffset := B mod W_BIT_COUNT;
  if nB = 1 then begin
    SetLength(Result.fData, 1);
    Result.fData[0] := A.Hi shr bOffset;
    Result.fSgn := A.Signum;
    exit;
  end;
  pA := @(A.fData[nA - nB]);
  if bOffset = 0 then begin //simple case which can be solved by Move function
    SetLength(Result.fData, nB);
    Move(pA^, Result.fData[0], nB * cLimbSize);
    Result.fSgn := A.Signum;
    exit;
  end;
  if _TopBitPos(A.Hi) <= bOffset then begin
     underflow := True;
     Dec(nB);
  end else underflow := False;
  SetLength(Result.fData, nB);
  pB := @Result.fData[0];
  _shr(pA, pB, bOffset, nB);
  if underflow then begin
    pB := @Result.fData[nB - 1];
    PLimb(pB)^ := PLimb(pB)^ or (A.Hi shl (W_BIT_COUNT - bOffset));
  end;
  Result.fSgn := A.Signum;
end;

class operator TInteger.LeftShift(const A: TInteger; const B: Integer): TInteger;
var nA, nB, bOffset: Integer;
    pA, pB: Pointer;
    top: TLimb;
begin
  if B = 0 then exit(A);

  nA := A.Count;
  if A.IsZero then begin
    Result := TInteger.Zero;
    exit;
  end;

  nB := nA + (B shr W_LOG2_BITS);
  bOffset := B mod W_BIT_COUNT;
  if bOffset = 0 then begin // simple case which can be solved by Move function
    SetLength(Result.fData, nB);
    bOffset := B shr W_LOG2_BITS; // B div BitCoutn(TLimb)
    FillChar(Result.fData[0], bOffset * cLimbSize, 0);
    Move(A.fData[0], Result.fData[bOffset], nA * cLimbSize);
    Result.fSgn := A.Signum;
    exit;
  end;
  if _TopBitPos(A.Hi) + bOffset > W_BIT_COUNT then Inc(nB);
  SetLength(Result.fData, nB);
  pA := @(A.fData[0]);
  pB := @Result.fData[B shr W_LOG2_BITS];
  top := _shl(pA, pB, bOffset, nA);
  if top <> 0 then Result.fData[nB - 1] := top;
  Result.fSgn := A.Signum;
end;

class operator TInteger.BitwiseAnd(const A: TInteger; const B: TInteger): TInteger;
var N, Na, Nb: Integer;
begin
  case GetSignPair(A, B) of
    spPP: begin
      N := Min(A.Count, B.Count);
      SetLength(Result.fData, N);
      VecAnd(PByte(A.fData), PByte(B.fData), PByte(Result.fData), N * cLimbSize);
      Result.TrimLeadingZeros;
    end;
    spNP: begin
      Na := A.Count;
      Nb := B.Count;
      N := Max(Na, Nb);
      SetLength(Result.fData, N);
      VecNot(PByte(A.fData), PByte(Result.fData), Na * cLimbSize);
      Inc(Result);
      if Na < N then
        FillChar(Result.fData[Na], (N - Na) * cLimbSize, $FF);
      VecAnd(PByte(Result.fData), PByte(B.fData), PByte(Result.fData), Nb * cLimbSize);
      if Integer(Result.Hi) < 0 then
        Result.fSgn := -1;
      Result.TrimLeadingZeros;
    end;
    spPN: begin
      Na := A.Count;
      Nb := B.Count;
      N := Max(Na, Nb);
      SetLength(Result.fData, N);
      VecNot(PByte(B.fData), PByte(Result.fData), Nb * cLimbSize);
      Inc(Result);
      if NB < N then
        FillChar(Result.fData[Nb], (N - Nb) * cLimbSize, $FF);
      VecAnd(PByte(Result.fData), PByte(A.fData), PByte(Result.fData), Na * cLimbSize);
      if Integer(Result.Hi) < 0 then
        Result.fSgn := -1;
      Result.TrimLeadingZeros;
    end;
    spNN: begin
      // todo
    end;
  end;
end;

class operator TInteger.BitwiseOr(const A: TInteger; const B: TInteger): TInteger;
var N, M: Integer;
begin
  N := A.Count;
  M := B.Count;
  if N > M then begin
    SetLength(Result.fData, N);
    Move(A.fData[M], Result.fData[M], (N - M) * SizeOf(Cardinal));
    N := M;
  end else
  if N < M then begin
    SetLength(Result.fData, M);
    Move(B.fData[N], Result.fData[N], (M - N) * SizeOf(Cardinal));
  end else
    SetLength(Result.fData, N);

  VecOr(PByte(A.fData), PByte(B.fData), PByte(Result.fData), N * cLimbSize);
end;

class operator TInteger.BitwiseXor(const A: TInteger; const B: TInteger): TInteger;
var N, M: Integer;
begin
  N := A.Count;
  M := B.Count;
  if N > M then begin
    SetLength(Result.fData, N);
    Move(A.fData[M], Result.fData[M], (N - M) * SizeOf(Cardinal));
    N := M;
  end else
  if N < M then begin
    SetLength(Result.fData, M);
    Move(B.fData[N], Result.fData[N], (M - N) * SizeOf(Cardinal));
  end else
    SetLength(Result.fData, N);

  VecXor(PByte(A.fData), PByte(B.fData), PByte(Result.fData), N * cLimbSize);
  Result.TrimLeadingZeros;
end;

class operator TInteger.LessThan(const A, B: TInteger): Boolean;
begin
  Result := (Compare(A, B) < 0);
end;

class operator TInteger.LessThanOrEqual(const A, B: TInteger): Boolean;
begin
  Result := (Compare(A, B) <= 0)
end;

class operator TInteger.GreaterThan(const A, B: TInteger): Boolean;
begin
  Result := (Compare(A, B) > 0);
end;

class operator TInteger.GreaterThanOrEqual(const A, B: TInteger): Boolean;
begin
  Result := (Compare(A, B) >= 0);
end;

class function TInteger.AbsCompare(const A, B: TInteger): Integer;
var I, nA, nB: Integer;
    da, db: TLimb;
begin
  nA := A.Count;
  nB := B.Count;
  if nA < nB then Result := -1
  else if nA > nB then Result := 1
  else begin
    Result := 0;
    for I := nA - 1 downto 0 do begin
      da := A.fData[I];
      db := B.fData[I];
      if da > db then begin
        Result := 1;
        break;
      end else
      if da < db then begin
        Result := -1;
        break;
      end;
    end;
  end;
end;

class function TInteger.Compare(const A, B: TInteger): Integer;
begin
  if A.Signum < B.Signum then exit(-1);
  if A.Signum > B.Signum then exit(1);
  Result := A.Signum * AbsCompare(A, B);
end;

class function TInteger.StdMul(const A, B: TInteger): TInteger;
var pA, pB, p: Pointer;
    nA, nB, n: Integer;
    data: TLimbArray;
begin
  nA := A.Count;
  nB := B.Count;
  n := nA + nB;
  SetLength(data, n);
  pA := A.fData;
  pB := B.fData;
  p := data;
  if nA = 1 then
    _ShortMul(pB, pA, p, nB)
  else
  if nB = 1 then
    _ShortMul(pA, pB, p, nA)
  else
    _Mul(pA, pB, p, nA, nB);
  Result.fSgn := A.Signum * B.Signum;
  Result.fData := data;
  Result.TrimLeadingZeros;
end;

class operator TInteger.Multiply(const A, B: TInteger): TInteger;
var km: TKaratsubaMul;
begin
  km.FastMulThreshold := g_MThresholds.KaratsubaMul;
//  Result := km.Eval2(A, B);
  Result := km.Eval(A, B);
end;

class operator TInteger.Negative(const A: TInteger): TInteger;
begin
  Result := A;
  Result.fSgn := -Result.fSgn;
end;

class operator TInteger.Subtract(const A, B: TInteger): TInteger;
var pA, pB: Pointer;
    n, nA, nB, I: Integer;
    data: TLimbArray;
    sp: TSignPair;
begin
  sp := GetSignPair(A, B);
  case sp of
    spNP: Result := -(A.Abs + B);
    spPN: Result := A + B.Abs;
    spPP, spNN: begin
      nA := A.Count;
      nB := B.Count;
      n := Max(nA, nB);
      SetLength(data, n);
      if AbsCompare(A, B) >= 0 then begin
        pA := A.fData;
        pB := data;
        Move(B.fData[0], data[0], nB * cLimbSize);
        Result.fSgn := A.Signum;
      end else begin
        pA := B.fData;
        pB := data;
        Move(A.fData[0], data[0], nA * cLimbSize);
        Result.fSgn := -A.Signum;
      end;
      _ipSub(pA, pB, n);
      for I := High(data) downto 1 do
        if data[I] = 0 then Dec(n)
        else break;
      if n > 0 then SetLength(data, n);
      Result.fData := data;
    end;
  end;
end;

procedure TInteger.TrimLeadingZeros;
var I: Integer;
begin
  I := High(fData);
  while I > 0 do begin
    if fData[I] <> 0 then break;
    Dec(I);
  end;
  Inc(I);
{$ifdef Test}
  fLimbCount := I;
{$else}
  SetLength(fData, I);
{$endif}
end;

function TInteger.ToBinString;
var I, J, K, rest: Integer;
    r: TLimb;
    p: PLimb;
    pRes: PChar;
begin
  I := _TopBitPos(@fData[0], Count);
  K := I div cLimbBits;
  rest := I mod cLimbBits;

  SetLength(Result, I);
  pRes := @Result[I];
  p := @fData[0];
  for I := 0 to K - 1 do begin
    r := p^;
    for J := 0 to cLimbBits - 1 do begin
      if r and 1 <> 0 then pRes^ := '1'
      else pRes^ := '0';
      Dec(pRes);
      r := r shr 1;
    end;
    Inc(p);
  end;
  r := p^;
  for J := 0 to rest - 1 do begin
    if r and 1 <> 0 then pRes^ := '1'
    else pRes^ := '0';
    Dec(pRes);
    r := r shr 1;
  end;
end;

function TInteger.ToHexString: String;
var s: String;
    len, I, J, c, N: Integer;
    d: TLimb;
const
{$ifdef Limb64}
  cNibbleCount = 16;
  cNibbleCntLog = 4;
{$else}
  cNibbleCount = 8;
  cNibbleCntLog = 3;
{$endif}
begin
  len := Count shl cNibbleCntLog;
  d := Hi;
  if (Count = 0) and (d = 0) then begin
    SetLength(Result, 1);
    Result[1] := '0';
    exit;
  end;
  N := _CountLeadingZeros(d);
  N := N shr 2;
  Dec(len, N);
  SetLength(s, len);
  N := cNibbleCount - N;
  I := len; J := 0;
  while I > N do begin
    d := fData[J];
    s[I] := cHexNumSymbols[d and cNibbleMask];
    Dec(I);
    s[I] := cHexNumSymbols[(d shr 4) and cNibbleMask];
    Dec(I);
    s[I] := cHexNumSymbols[(d shr 8) and cNibbleMask];
    Dec(I);
    s[I] := cHexNumSymbols[(d shr 12) and cNibbleMask];
    Dec(I);
    s[I] := cHexNumSymbols[(d shr 16) and cNibbleMask];
    Dec(I);
    s[I] := cHexNumSymbols[(d shr 20) and cNibbleMask];
    Dec(I);
    s[I] := cHexNumSymbols[(d shr 24) and cNibbleMask];
    Dec(I);
  {$ifndef Limb64}
    s[I] := cHexNumSymbols[d shr 28];
    Dec(I);
  {$else}
    s[I] := cHexNumSymbols[(d shr 28) and cNibbleMask];
    Dec(I);
    s[I] := cHexNumSymbols[(d shr 32) and cNibbleMask];
    Dec(I);
    s[I] := cHexNumSymbols[(d shr 36) and cNibbleMask];
    Dec(I);
    s[I] := cHexNumSymbols[(d shr 40) and cNibbleMask];
    Dec(I);
    s[I] := cHexNumSymbols[(d shr 44) and cNibbleMask];
    Dec(I);
    s[I] := cHexNumSymbols[(d shr 48) and cNibbleMask];
    Dec(I);
    s[I] := cHexNumSymbols[(d shr 52) and cNibbleMask];
    Dec(I);
    s[I] := cHexNumSymbols[(d shr 56) and cNibbleMask];
    Dec(I);
    s[I] := cHexNumSymbols[d shr 60];
    Dec(I);
  {$endif}
    Inc(J);
  end;
  d := fData[Count - 1];
  c := 0;
  N := I;
  for I := N downto 1 do begin
    s[I] := cHexNumSymbols[(d shr c) and cNibbleMask];
    Inc(c, 4);
  end;
  Result := s;
end;

class function TInteger.HexParse(const aValue: String; out aInt: TInteger): Boolean;
var I, J, len, lo: Integer;
    data: TLimbArray;
    val, tmp: TLimb;
    offset: Byte;
begin
  Result := False;
  len := Length(aValue);
  len := len shl 2;
  SetLength(data, (len div cLimbBits) + IfThen(len mod cLimbBits <> 0, 1, 0));
  offset := 0;
  val := 0;
  J := 0;
  case aValue[1] of
    '+': begin
      lo := 2;
      aInt.fSgn := 1;
    end;
    '-': begin
      lo := 2;
      aInt.fSgn := -1;
    end;
  else
    lo := 1;
    aInt.fSgn := 1;
  end;
  for I := High(aValue) downto lo do begin
    case aValue[I] of
      '0': tmp := $0;
      '1': tmp := $1;
      '2': tmp := $2;
      '3': tmp := $3;
      '4': tmp := $4;
      '5': tmp := $5;
      '6': tmp := $6;
      '7': tmp := $7;
      '8': tmp := $8;
      '9': tmp := $9;
      'a', 'A': tmp := $A;
      'b', 'B': tmp := $B;
      'c', 'C': tmp := $C;
      'd', 'D': tmp := $D;
      'e', 'E': tmp := $E;
      'f', 'F': tmp := $F;
    else
      aInt := TInteger.Zero;
      exit;
    end;
    val := val or (tmp shl offset);
    Inc(offset, 4);
    if offset = cLimbBits then begin
      data[J] := val;
      val := 0;
      offset := 0;
      Inc(J);
    end;
  end;
  if offset > 0 then data[J] := val;
  aInt.fData := data;
  Result := True;
end;

function TInteger.ToDecimalString: String;
var res, d, b: TInteger;
    sRes: TList<String>;
    J, pos, len: Integer;
    s: String;
const
{$ifdef Limb64}
  cDecBase = 10000000000000000000;
  cBaseDigs = 19;
{$else}
  cDecBase = 1000000000;
  cBaseDigs = 9;
{$endif}
begin
  sRes := TList<String>.Create;
  try
    b := cDecBase;
    res := Self;
    d := 1;
    while res.Count > 1 do begin
      d := b;
      ShortDivMod(res, d);
      sRes.Add(UIntToStr(d[0]));
    end;
    s := UIntToStr(res[0]);
    len := Length(s) + sRes.Count * cBaseDigs;
    if fSgn < 0 then Inc(len);
    Result := StringOfChar('0', len);
    if fSgn < 0 then begin
      Result[1] := '-';
      pos := 2;
    end else pos := 1;
    len := Length(s);
    Move(s[1], Result[pos], len * SizeOf(Char));
    Inc(pos, len);
    for J := sRes.Count - 1 downto 0 do begin
      s := sRes[J];
      len := Length(s);
      Move(s[1], Result[pos + cBaseDigs - len], len * SizeOf(Char));
      Inc(pos, cBaseDigs);
    end;
  finally
    sRes.Free;
  end;
end;

function TInteger.ToString: String;
begin
  Result := ToDecimalString;
end;

class function TInteger.DecimalParse(const aValue: String; out aInt: TInteger): Boolean;
var pos, rest: Integer;
    res: TInteger;
    b, i: TLimb;
    s: String;
const
{$ifdef Limb64}
  cDecBase = 10000000000000000000;
  n = 19;
{$else}
  cDecBase = 1000000000;
  n = 9;
{$endif}

  function TryStrToLimb(const S: String; out aLimb: TLimb): Boolean;
  {$ifdef Limb64}
  begin
     Result := TryStrToUInt64(S, aLimb);
  end;
  {$else}
  {$ifdef FPC}
  var tmp: UInt64;
  begin
    Result := TryStrToUInt64(S, tmp);
    aLimb := TLimb(tmp);
	end;
  {$else}
  {$if RTLVersion > 31.00}
  begin
     Result := TryStrToUInt(S, aLimb);
  end;
  {$else}
   var tmp: UInt64;
   begin
     Result := TryStrToUInt64(S, tmp);
     aLimb := TLimb(tmp);
   end;
  {$endif}
  {$endif}
  {$endif}

begin
  Result := False;
  b := cDecBase;
  SetLength(s, n);
  case aValue[1] of
    '+': begin
      pos := 2;
      aInt.fSgn := 1;
    end;
    '-': begin
      pos := 2;
      aInt.fSgn := -1;
    end;
  else
    pos := 1;
    aInt.fSgn := 1;
  end;
  rest := Length(aValue) - pos + 1;
  res := 0;
  while rest >= n do begin
    Move(aValue[pos], s[1], n * SizeOf(Char));
    if not TryStrToLimb(s, i) then exit;
    res := res * b + i;
    Inc(pos, n);
    Dec(rest, n);
  end;
  if rest > 0 then begin
    SetLength(s, rest);
    Move(aValue[pos], s[1], rest * SizeOf(Char));
    if not TryStrToLimb(s, i) then exit;
    b := IntPwr(10, rest);
    res := res * b + i;
  end;
  aInt.fData := res.fData;
  Result := True;
end;

class function TInteger.TryParse(const aValue: String; out aInt: TInteger; aBase: Integer): Boolean;
begin
  case aBase of
    10: Result := DecimalParse(aValue, aInt);
    16: Result := HexParse(aValue, aInt);
  else
    Result := False;
  end;
end;

class function TInteger.Zero: TInteger;
begin
  SetLength(Result.fData, 1);
  Result.fData[0] := 0;
  Result.fSgn := 1;
end;

function TInteger.IsZero: Boolean;
begin
  Result := (Count = 1) and (fData[0] = 0);
end;

function TInteger.IsOne: Boolean;
begin
  Result := (Count = 1) and (fData[0] = 1) and (fSgn >= 0);
end;

function TInteger.IsMinusOne: Boolean;
begin
  Result := (Count = 1) and (fData[0] = 1) and (fSgn < 0);
end;

function TInteger.IsAbsOne: Boolean;
begin
  Result := (Count = 1) and (fData[0] = 1);
end;

function TInteger.IsEven: Boolean;
begin
  Result := ((fData[0] and 1) = 0)
end;

function TInteger.IsOdd: Boolean;
begin
  Result := ((fData[0] and 1) <> 0);
end;

class function TInteger.Floor(const A: Double): TInteger;
var I: UInt64;
    exp: Integer;
    buff: UInt64 absolute A;
const FRAC_MASK = $fffffffffffff;
      NORM_BIT  = $10000000000000;
begin
  I := (buff and FRAC_MASK) or NORM_BIT;
  exp := 52 - Integer(((buff shr 52) and $7ff) - $3ff);
  Result := I;
  if exp > 0 then
    Result := Result shr exp
  else
    Result := Result shl -exp;

  if A < 0 then begin
    if (exp > 0) and ((I and (I64_MASK shr (64 - exp))) <> 0) then
      Result := Result + 1;
    Result.fSgn := -1;
  end;
end;

class function TInteger.Ceil(const A: Double): TInteger;
var I: UInt64;
    exp: Integer;
    buff: UInt64 absolute A;
const FRAC_MASK = $fffffffffffff;
      NORM_BIT  = $10000000000000;
begin
  I := (buff and FRAC_MASK) or NORM_BIT;
  exp := 52 - (((buff shr 52) and $7ff) - $3ff);
  Result := I;
  if exp > 0 then
    Result := Result shr exp
  else
    Result := Result shl -exp;

  if A > 0 then begin
    if (exp > 0) and ((I and (I64_MASK shr (64 - exp))) <> 0) then
      Result := Result + 1
  end else
    Result.fSgn := -1;
end;

class function TInteger.GCD(const A, B: TInteger): TInteger;
var m, d: TLimb;
    I, J: Integer;

  //Finds power of two which divides number that is represented by aValue
  function FindDiv(const aValue: TInteger): Integer;
  begin
    Result := 0;
    d := aValue.fData[0];
    while (d = 0) do begin
      Inc(Result);
      d := aValue.fData[Result];
    end;
    m := 1;
    Result := Result shl W_LOG2_BITS;
    while (m and d) = 0 do begin
      m := m shl 1;
      Inc(Result);
    end;
  end;

var u, v, t: TInteger;
begin
  if A.IsZero then exit(B);
  if B.IsZero then exit(A);

  I := Min(FindDiv(A), FindDiv(B));
  u := A.Abs shr I;
  v := B.Abs shr I;
  if u > v then begin
    t := u; u := v; v := t;
  end;

  if u.IsOdd then
    t := -v
  else t := u;

  while not t.IsZero do begin

    J := FindDiv(t);
    t := t shr J;

    if t.Signum > 0 then u := t
    else v := -t;

    t := u - v;
  end;
  Result := u shl I;
end;

class function TInteger.Power(const A: TInteger; aExponent: Integer): TInteger;
var Y, Z: TInteger;
    N: Integer absolute aExponent;
    t: Integer;
begin
  Assert(aExponent > 0);
  Y := 1;
  Z := A;
  while True do begin
    t := N and 1;
    N := N shr 1;
    if t = 1 then Y := Z * Y;
    if N = 0 then break;
    Z := Z * Z;
  end;
  Result := Y;
end;

class function TInteger.PowerOfMinusOne(const aExponent: TInteger): Integer;
begin
  if (aExponent.fData[0] and 1) <> 0 then Result := -1
  else Result := 1;
end;

class procedure TInteger.DivMod(const A, B: TInteger; var Q, R: TInteger);
begin
  Q := A;
  R := B;
  if B.Count = 1 then
    ShortDivMod(Q, R)
  else LongDivMod(Q, R);
  Q.fSgn := A.Signum * B.Signum;
end;

procedure TInteger.SetSignum(aValue: Int8);
begin
  if aValue >= 0 then
    fSgn := 1
  else
    fSgn := -1;
end;

{$endregion}

{$region 'Karatsuba multiplication'}

function TKaratsubaMul.AbsCmp(pA, pB: PLimb; N: NativeInt): Integer;
var a, b: TLimb;
begin
  Result := 0;
  Dec(N);
  Inc(pA, N);
  Inc(pB, N);
  while N >= 0 do begin
    a := pA^;
    b := pB^;
    if a > b then begin
      Result := 1;
      exit;
    end else
    if a < b then begin
      Result := -1;
      exit;
    end;
    Dec(N);
    Dec(pA);
    Dec(pB);
  end;
end;

function TKaratsubaMul.IsZero(pA: PLimb; N: NativeInt): Boolean;
var pEnd: PByte;
begin
  pEnd := PByte(pA) + N * cLimbSize;
  while PByte(pA) < pEnd do begin
    if pA^ <> 0 then exit(False);
    Inc(pA);
  end;
  Result := True;
end;

// it is supposed that the pB size is N div 2
procedure TKaratsubaMul.ShortMul(pA, pB, pRes: PByte; N: NativeInt);
var pA0, pA1, pTmp: PByte;
    Ndiv2, Ndiv2Sz, NSz: NativeInt;
begin
  Ndiv2 := N shr 1;
  NSz := N * cLimbSize;
  Ndiv2Sz := Ndiv2 * cLimbSize;
  pA0 := pA;
  pA1 := pA + Ndiv2Sz;
  Mul(pA1, pB, pRes + Ndiv2Sz, Ndiv2);
  pTmp := pRes + 2 * NSz;
  Mul(pA0, pB, pTmp, Ndiv2);
  _ipAdd(pTmp, pRes, 3 * Ndiv2Sz);
end;

procedure TKaratsubaMul.Mul(pA, pB, pRes: PByte; N: NativeInt);
var pA0, pB0, pA1, pB1, pTmp: PByte;
    Ndiv2, sgn, Ndiv2Sz, NSz: Integer;
begin
  NSz := N * cLimbSize;
  FillChar(pRes^, 2 * NSz, 0); // clear result buffer
  //ClearBuffer(pRes, N);
  if N <= fFastMulThreshold then begin// classic algorithm
    _Mul(pA, pB, pRes, N, N);
    exit;
  end;
  Ndiv2 := N shr 1;
  Ndiv2Sz := Ndiv2 * cLimbSize;
  pA0 := pA;
  pA1 := pA + Ndiv2Sz;
  pB0 := pB;
  pB1 := pB + Ndiv2Sz;
  pTmp := pRes + 2 * NSz;
  if IsZero(PLimb(pB1), Ndiv2) then begin
    ShortMul(pA, pB, pRes, N);
    FillChar(pTmp^, 2 * NSz, 0); //clear temp buffer
    //ClearBuffer(pRes, N);
    exit;
  end;
  if IsZero(PLimb(pA1), Ndiv2) then begin
    ShortMul(pB, pA, pRes, N);
    FillChar(pTmp^, 2 * NSz, 0); //clear temp buffer
    //ClearBuffer(pRes, N);
    exit;
  end;
  // (2^N + 2^(Ndiv2)) * A1 * B1 + (2^Ndiv2 + 1) * A0 * B0
  Mul(pA0, pB0, pTmp, Ndiv2);
  Move(pTmp^, pRes^, NSz);
  //MoveBuffer(pTmp, pRes, N);
  _ipAdd(pTmp, pRes + Ndiv2Sz, 3 * Ndiv2);
  Mul(pA1, pB1, pTmp, Ndiv2);
  _ipAdd(pTmp, pRes + Ndiv2Sz, 3 * Ndiv2);
  _ipAdd(pTmp, pRes + NSz, N);

  // determine the signum of the (A1 - A0)(B0 - B1)
  if AbsCmp(PLimb(pA1), PLimb(pA0), Ndiv2) >= 0 then begin
    _Sub(pA1, pA0, pTmp, Ndiv2);
    sgn := 1;
  end else begin
    _Sub(pA0, pA1, pTmp, Ndiv2);
    sgn := -1;
  end;
  if AbsCmp(PLimb(pB0), PLimb(pB1), ndiv2) >= 0 then begin
    _Sub(pB0, pB1, pTmp + Ndiv2Sz, Ndiv2);
  end else begin
    _Sub(pB1, pB0, pTmp + Ndiv2Sz, Ndiv2);
    sgn := -1 * sgn;
  end;
  //pTmp <- 2^N * (A1 - A0)(B0 - B1)
  Mul(pTmp, pTmp + Ndiv2Sz, pTmp + NSz, Ndiv2);
  FillChar(pTmp^, Ndiv2Sz, 0);
  //ClearBuffer(pTmp, Ndiv2);
  Move((pTmp + NSz)^, (pTmp + Ndiv2Sz)^, NSz);
  //MoveBuffer(pTmp + NSz, pTmp + Ndiv2Sz, N);
  FillChar((pTmp + 3 * Ndiv2Sz)^, Ndiv2Sz, 0);
  //ClearBuffer(pTmp + 3 * Ndiv2, Ndiv2);
  if sgn > 0 then
    _ipAdd(pTmp + Ndiv2Sz, pRes + Ndiv2Sz, 3 * Ndiv2Sz)
  else begin
    if AbsCmp(PLimb(pTmp), PLimb(pRes), 2 * N) > 0 then begin
      _Dec(pTmp, pRes, 2 * N);
      Move(pTmp^, pRes^, 2 * NSz);
      //MoveBuffer(pTmp, pRes, 2 * N);
    end else begin
      _Dec(pRes, pTmp, 2 * N);
    end;
  end;
  FillChar(pTmp^, 2 * NSz, 0); //clear temp buffer
  //ClearBuffer(pRes, N);
end;

function TKaratsubaMul.Eval(const A, B: TInteger): TInteger;
var N, i: NativeInt;
    buff, res: TLimbArray;
    pA, pB, pRes, pEnd: PByte;
begin
  if Min(A.Count, B.Count) <= fFastMulThreshold then
    exit(TInteger.StdMul(A, B));

  i := Max(A.Count, B.Count);
  N := _TopBitPos(i);
  N := 1 shl (N - 1);
  if i > N then N := N shl 1;
  SetLength(buff, (*9*)20 * N);
  pA := PByte(@buff[0]);
  pB := PByte(@buff[N]);
  pRes := PByte(@buff[2 * N]);
  Move(A.fData[0], pA^, A.Count * cLimbSize);
  Move(B.fData[0], pB^, B.Count * cLimbSize);
  Mul(pA, pB, pRes, N);
  pEnd := pRes;
  pRes := PByte(@buff[4 * N - 1]);
  i := 2 * N;
  while pRes >= pEnd do begin
    if PLimb(pRes)^ = 0 then Dec(i)
    else break;
    Dec(pRes, cLimbSize);
  end;
  if i > 0 then begin
    SetLength(res, i);
    Move(pEnd^, res[0], i * cLimbSize);
    Result.fData := res;
  end else
    Result := 0;
  Result.fSgn := A.Signum * B.Signum;
end;

procedure Split(const A: TInteger; var A0, A1: TInteger; N: NativeInt);
var nA: NativeInt;
begin
  nA := A.Count;
  if nA > N then begin
    SetLength(A1.fData, nA - N);
    Move(A.fData[N], A1.fData[0], (nA - N) * cLimbSize);
    while (N > 1) and (A.fData[N - 1] = 0) do Dec(N);
    SetLength(A0.fData, N);
    Move(A.fData[0], A0.fData[0], N * cLimbSize);
  end else begin
    SetLength(A0.fData, nA);
    Move(A.fData[0], A0.fData[0], nA * cLimbSize);
    A1 := TInteger.Zero;
  end;
  A0.fSgn := 1;
  A1.fSgn := 1;
end;

function TKaratsubaMul.Eval2(const A, B: TInteger): TInteger;
var A0, A1, B0, B1, C, D: TInteger;
    N, nA, nB: NativeInt;
begin
  nA := A.Count;
  nB := B.Count;
  if (nA <= fFastMulThreshold) or (nB <= fFastMulThreshold) then begin
    Result := TInteger.StdMul(A, B);//A * B;
    exit;
  end;
  N := Max(nA, nB);
  if (N and 1) = 1 then Inc(N);
  N := N div 2;
  Split(A, A0, A1, N);
  Split(B, B0, B1, N);
  N := N shl W_LOG2_BITS;
  C := Eval2(A1, B1);
  Result := (C shl (2 * N)) + (C shl N);
  C := Eval2(A0, B0);
  Result := Result + (C shl N) + C;
  C := A0 - A1;
  D := B1 - B0;
  C := Eval2(C, D);
  Result := Result + (C shl N);
  Result.fSgn := A.Signum * B.Signum;
end;

{$endregion}

{$region 'TRational'}

procedure TRational.Create(const aNum, aDenom: TLimbArray; aSgn: Integer);
var a, b, c: TInteger;
begin
  a := aNum;
  b := aDenom;
  if b.IsZero then
    raise EDivByZero.Create('TRational denominator is equal to zero.');
  c := TInteger.GCD(a, b);
  fNum := (a div c).fData;
  fDenom := (b div c).fData;
  fSgn := aSgn;
end;

procedure TRational.Create(const aNum, aDenom: UInt64; aSgn: Integer);
const hiMask = $ffffffff00000000;

  function GetArray(const aValue: UInt64): TLimbArray;
  var data: array [0..1] of Cardinal absolute aValue;
  begin
    if aValue and hiMask = 0 then begin
      SetLength(Result, 1);
      Result[0] := data[0];
    end else begin
      SetLength(Result, 2);
      Move(aValue, Result[0], SizeOf(UInt64));
    end;
  end;

var d: UInt64;
begin
  d := GCD(aNum, aDenom);
  if d = 1 then begin
    fNum := GetArray(aNum);
    fDenom := GetArray(aDenom);
  end else begin
    fNum := GetArray(aNum div d);
    fDenom := GetArray(aDenom div d);
  end;
  fSgn := aSgn;
end;

procedure TRational.Create(const aNum, aDenom: TInteger);
var gcd: TInteger;
begin
  gcd := TInteger.GCD(aNum, aDenom);
  if gcd.IsOne then begin
    fNum := aNum.fData;
    fDenom := aDenom.fData;
  end else begin
    fNum := (aNum div gcd).fData;
    fDenom := (aDenom div gcd).fData;
  end;
  fSgn := aNum.fSgn * aDenom.fSgn;
end;

class function TRational.Fraction(const aNum, aDenom: TInteger): TRational;
begin
  Result.Create(aNum, aDenom);
end;

class operator TRational.Divide(const A, B: TRational): TRational;
var un, vn, ud, vd, d1, d2: TInteger;
begin
  Result.fSgn := A.fSgn * B.fSgn;
  un := A.fNum;
  vn := B.fDenom;
  ud := A.fDenom;
  vd := B.fNum;
  d1 := TInteger.GCD(un, vd);
  d2 := TInteger.GCD(vn, ud);
  Result.fNum := ((un div d1) * (vn div d2)).fData;
  Result.fDenom := ((ud div d2) * (vd div d1)).fData;
end;

class operator TRational.Negative(const A: TRational): TRational;
begin
  Result := A;
  Result.fSgn := -A.fSgn;
end;

function TRational.GetDenominator: TInteger;
begin
  Result.Init(fDenom);
end;

function TRational.GetNumerator: TInteger;
begin
  Result.Init(fNum, fSgn);
end;

class operator TRational.Add(const A, B: TRational): TRational;
var un, vn, ud, vd, t, d1, d2, tmp: TInteger;
begin
  un := A.Numerator;
  vn := B.Numerator;
  ud := A.Denominator;
  vd := B.Denominator;
  d1 := TInteger.GCD(ud, vd);
  if d1.IsOne then begin
    t := un * vd + vn * ud;
    Result.fSgn := t.Signum;
    Result.fNum := t.fData;
    Result.fDenom := (ud * vd).fData;
  end else begin
    tmp := vd div d1;
    t := un * tmp + vn * (ud div d1);
    d2 := TInteger.GCD(t, d1);
    Result.fSgn := t.Signum;
    Result.fNum := (t div d2).fData;
    Result.fDenom := (tmp * (ud div d2)).fData;
  end;
end;

class operator TRational.Implicit(const A: TInteger): TRational;
begin
  with Result do begin
    fNum := A.fData;
    fDenom := TLimbArray.Create(1);
    fSgn := A.Signum;
  end;
end;

class operator TRational.Implicit(const A: Int64): TRational;
begin
  Result := TInteger(A);
end;

class operator TRational.Explicit(const A: TRational): Double;
var shift: Integer;
    bTreatAsSubnormal: Boolean;
    denominator, numerator, q, r: TInteger;
const round4: array [0..7] of Integer = (0, -1, -2, 1, 0, -1, 2, 1);
begin
  numerator := A.Numerator.Abs;
  denominator := A.Denominator.Abs;

  // Choose shift so that 2**54 < numerator / denominator / 2**shift < 2**56
  shift := Numerator.HighestBitPos - Denominator.HighestBitPos - 55;

  (*
    The bTreadAsSubnormal flag catches all cases of subnormal results,
    along with some cases where the result is not subnormal but "is" still
    smaller than 2**-1021. In all these cases, it's sufficient to find the
    closest integer multiple of 2**-1074. We first round to the nearest
    multiple of 2**-1076 using round-to-odd.
  *)
  bTreatAsSubnormal := (shift < -1076);
  if bTreatAsSubnormal then
    shift := -1076;

  // Divide the fraction by 2**shift
  if shift >= 0 then
    denominator := denominator shl shift
  else
    numerator := numerator shl -shift;

  // Convert to the nearest integer, using round-to-odd
  TInteger.DivMod(numerator, denominator, q, r);
  if (not r.IsZero) and q.IsEven then
    Inc(q);

  if bTreatAsSubnormal then begin
    (*
      Round to nearest multiple of 4, rounding ties to the nearest multiple of 8.
      This avoids double rounding from the ldexp call bellow
    *)
    q := q + round4[q[0] and 7];
  end;

  Result := ldexp(Double(q), shift);
  if A.Signum < 0 then
    Result := -Result;
end;

function TRational.Round: TInteger;
var num, denom, r: TInteger;
begin
  num := Numerator.Abs;
  denom := Denominator.Abs;
  TInteger.DivMod(num, denom, Result, r);
  if (r > (denom shr 1)) then
    Inc(Result);
  if Signum < 0 then
    Result := -Result;
end;

function TRational.Floor: TInteger;
var num, denom, r: TInteger;
begin
  num := Numerator.Abs;
  denom := Denominator.Abs;
  if denom.IsOne then exit(Numerator);
  TInteger.DivMod(num, denom, Result, r);
  if Signum < 0 then
    Result := -(Result + 1);
end;

function TRational.Abs: TRational;
begin
  Result.Create(fNum, fDenom);
end;

function TRational.IsOne: Boolean;
begin
  Result := Numerator.IsOne and Denominator.IsOne and (Signum >= 0);
end;

function TRational.IsMinusOne: Boolean;
begin
  Result := Numerator.IsOne and Denominator.IsOne and (Signum < 0);
end;

function TRational.IsZero: Boolean;
begin
  Result := Numerator.IsZero;
end;

function TRational.IsInteger: Boolean;
begin
  Result := (Length(fDenom) = 1) and (fDenom[0] = 1);
end;

class operator TRational.Multiply(const A, B: TRational): TRational;
var un, vn, ud, vd, d1, d2: TInteger;
begin
  Result.fSgn := A.fSgn * B.fSgn;
  un := A.fNum;
  vn := B.fNum;
  ud := A.fDenom;
  vd := B.fDenom;
  d1 := TInteger.GCD(un, vd);
  d2 := TInteger.GCD(vn, ud);
  Result.fNum := ((un div d1) * (vn div d2)).fData;
  Result.fDenom := ((ud div d2) * (vd div d1)).fData;
end;

class operator TRational.Subtract(const A, B: TRational): TRational;
var un, vn, ud, vd, t, d1, d2, tmp: TInteger;
begin
  un := A.Numerator;
  vn := B.Numerator;
  ud := A.Denominator;
  vd := B.Denominator;
  d1 := TInteger.GCD(ud, vd);
  if d1.IsOne then begin
    t := un * vd - vn * ud;
    Result.fSgn := t.Signum;
    Result.fNum := t.fData;
    Result.fDenom := (ud * vd).fData;
  end else begin
    tmp := vd div d1;
    t := un * tmp - vn * (ud div d1);
    d2 := TInteger.GCD(t, d1);
    Result.fSgn := t.Signum;
    Result.fNum := (t div d2).fData;
    Result.fDenom := (tmp * (ud div d2)).fData;
  end;
end;

class function TRational.Compare(const A, B: TRational): Integer;
begin
  if A.Signum < B.Signum then exit(-1);
  if A.Signum > B.Signum then exit(1);
  Result := TInteger.Compare(
    A.Numerator * B.Denominator, B.Numerator * A.Denominator
  );
end;

class operator TRational.LessThan(const A, B: TRational): Boolean;
begin
  Result := (Compare(A, B) < 0);
end;

class operator TRational.GreaterThan(const A, B: TRational): Boolean;
begin
  Result := (Compare(A, B) > 0);
end;

class operator TRational.LessThanOrEqual(const A, B: TRational): Boolean;
begin
  Result := (Compare(A, B) <= 0);
end;

class operator TRational.GreaterThanOrEqual(const A, B: TRational): Boolean;
begin
  Result := (Compare(A, B) >= 0);
end;

class operator TRational.Equal(const A, B: TRational): Boolean;
begin
  Result := (Compare(A, B) = 0);
end;

class operator TRational.NotEqual(const A, B: TRational): Boolean;
begin
  Result := (Compare(A, B) <> 0);
end;

class function TRational.Reciprocal(const aValue: TInteger): TRational;
begin
  with Result do begin
    fNum := TLimbArray.Create(1);
    fDenom := aValue.fData;
    fSgn := aValue.Signum;
  end;
end;

class function TRational.Reciprocal(const aValue: TRational): TRational;
begin
  with Result do begin
    fNum := aValue.fDenom;
    fDenom := aValue.fNum;
    fSgn := aValue.Signum;
  end;
end;

function TRational.ToString: String;
begin
  if IsInteger then
    Result := Numerator.ToDecimalString
  else
    Result := Numerator.ToDecimalString + '/' + Denominator.ToDecimalString;
end;

procedure TRational.Parse(const aValue: String);
var sarr: TArray<String>;
    num, denom: TInteger;
begin
  sarr := aValue.Split(['/']);
  if Length(sarr) <> 2 then
    raise EInvalidNumString.CreateFmt('Invalid rational string ''%s''.', [aValue]);

  if not TInteger.DecimalParse(sarr[0], num) then
    raise EInvalidNumString.CreateFmt('Invalid numerator string ''%s''.', [sarr[0]]);
  if not TInteger.DecimalParse(sarr[1], denom) then
    raise EInvalidNumString.CreateFmt('Invalid denominator string ''%s''.', [sarr[1]]);

  fNum := num.fData;
  fDenom := denom.fData;
  fSgn := (num.Signum * denom.Signum);
end;

{$endregion}


{$region 'TReal'}

procedure TReal.Init(const aValue: Double; aDigits: Integer);
var iVal: UInt64 absolute aValue;
    frac: TLimbArray;
    i, fracLen: Integer;
begin
  if aDigits > 0 then
    fracLen := Max(cMinLimbCount, (aDigits + W_BIT_COUNT - 1) shr W_LOG2_BITS)
  else
    fracLen := cMinLimbCount;
  SetLength(frac, fracLen);

  i := (iVal shr 52) and $7FF;
  if i > 0 then begin
    //normalized numbers
  {$ifdef Limb64}
    frac[fracLen - 1] := W_HI_BIT or (iVal shl 11);
  {$else}
    frac[fracLen - 2] := Cardinal(iVal shl 11);
    frac[fracLen - 1] := I32_HI_BIT or Cardinal(iVal shr 21);
  {$endif}
    fExponent := (i - 1023);
  end else begin
    //subnormal numbers
    if IsZero(aValue) then
      fExponent := 0
    else begin
      raise ENotImplemented.Create('TReal.Init with subnormal numbers is not implemented yet.');
    end;
  end;

  fFraction := frac;
  if (iVal and I64_HI_BIT) <> 0 then
    fSgn := -1
  else
    fSgn := 1;
end;

procedure TReal.Init(const aFraction: TLimbArray; aExponent: Integer; aSgn: Integer);
var count, shift: Integer;
begin
{$ifdef Limb64}
  fFraction := aFraction;
  count := Length(aFraction);
{$else}
  count := Length(aFraction);
  if count = 1 then begin
    SetLength(fFraction, 2);
    fFraction[1] := aFraction[0];
    count := 2;
  end else
    fFraction := aFraction;
{$endif}

  if (fFraction[count - 1] and W_HI_BIT) = 0 then
    shift := Normalize(@fFraction[0], Length(fFraction))
  else
    shift := 0;

  if IsZeroQ then
    Exponent := 0
  else
    Exponent := aExponent - shift;
  Signum := aSgn;
end;

procedure TReal.Init(const aValue: TInteger; aDigits: Integer);
var offset: Integer;
    fracLen, valLen: Integer;
    buff, fraction: TLimbArray;
    pBuff: PByte;
begin
  valLen := aValue.Count;
  if aDigits > 0 then
    fracLen := (aDigits + W_BIT_COUNT - 1) shr W_LOG2_BITS
  else
    fracLen := valLen;
  fracLen := Max(cMinLimbCount, fracLen);
  SetLength(fraction, fracLen);

  if fracLen >= valLen then begin
    Move(aValue.fData[0], fraction[fracLen - valLen], valLen * wSz);
    Normalize(@fraction[0], fracLen);
  end else begin
    offset := aValue.CountLeadingZeros;
    if offset > 0 then begin
      SetLength(buff, valLen);
      _shl(@aValue.fData[0], @buff[0], offset, valLen);
      pBuff := @buff[0];
    end else
      pBuff := @aValue.fData[0];
    BinRoundTo(pBuff, aValue.Count, @fraction[0], fracLen, fracLen * W_BIT_COUNT);
  end;
  fFraction := fraction;
  Exponent := aValue.DigitCount - 1;
  Signum := aValue.Signum;
end;

procedure TReal.Init(const aValue: TRational; aDigits: Integer);
var n, d: TReal;
begin
  n.Init(aValue.Numerator, aDigits);
  d.Init(aValue.Denominator, aDigits);
  Self := n * d.Reciprocal;
end;

procedure TReal.InitRational(const aNumerator, aDenominator: TInteger; aDigits: Integer);
var f: TRational;
begin
  f.Create(aNumerator, aDenominator);
  Init(f, aDigits);
end;

function TReal.AsDouble: Double;
begin
  if not TryAsDouble(Result) then
    raise ERangeError.Create('TReal exponent overlflows Double exponent range.');
end;

function TReal.TryAsDouble(out aValue: Double): Boolean;
var iVal: UInt64 absolute aValue;
    e, carry: Integer;
    hi, lo: UInt64;
    buff: array [0..1] of Cardinal;
const
   cBuffLen = {$ifdef Limb64}1{$else}2{$endif};
begin
  e := Exponent;
  if e = Integer.MinValue then begin
    aValue := 0;
    exit(True);
  end;

  carry := BinRoundTo(@fFraction[0], Length(fFraction), @buff, cBuffLen, 53);
  Inc(e, carry);
  if not InRange(e, -1022, 1023) then exit(False);

  if (buff[1] and I32_HI_BIT) <> 0 then begin
    hi := buff[1] and $7FFFFFFF;
    lo := buff[0];
    Inc(e, 1023);
    iVal := (hi shl 21) or (lo shr 11);
  end else
    iVal := 0;

  iVal := iVal or (UInt64(e) shl 52);
  if fSgn < 0 then
    iVal := iVal or I64_HI_BIT;

  Result := True;
end;

class function TReal.AbsCompare(const A, B: TReal): Integer;
var I, nA, nB, n: Integer;
    da, db: TLimb;
begin
  if A.IsZeroQ then exit(-B.Signum);
  if B.IsZeroQ then exit(A.Signum);

  if A.Exponent < B.Exponent then
    exit(-1);
  if A.Exponent > B.Exponent then
    exit(1);

  nA := Length(A.Fraction);
  nB := Length(B.Fraction);
  n := Min(nA, nB);

  for I := 1 to n do begin
    da := A.fFraction[nA - I];
    db := B.fFraction[nB - I];
    if da > db then
      exit(1);
    if da < db then begin
      exit(-1);
    end;
  end;

  if nA = nB then exit(0);

  if nA > n then begin
    n := nA - n;
    for I := 0 to n - 1 do
      if A.fFraction[I] <> 0 then
        exit(1);
  end else begin
    n := nB - n;
    for I := 0 to n - 1 do
      if B.fFraction[I] <> 0 then
        exit(-1);
  end;
  Result := 0;
end;

class function TReal.AddPositive(const A, B: TReal): TReal;
var ea, eb, i, de, sz: Integer;
    pa, pb, pc: Integer;
    buff: TLimbArray;
    carry: TLimb;
    ra, rb: TReal;
begin
  ea := A.Exponent;
  eb := B.Exponent;
  if ea < eb then begin
    i := ea; ea := eb; eb := i;
    ra := B;
    rb := A;
  end else begin
    ra := A;
    rb := B;
  end;

  pa := ra.BinDigitCount;
  pb := rb.BinDigitCount;
  pc := Min(pa, pb);
  de := ea - eb;
  if de > pc + 1 then begin
    buff := TrimTail(ra.Fraction, (pa - pc) shr W_LOG2_BITS);
    Result.Init(buff, ea, 1);
    exit;
  end;

  if pa > pc then
    ra := ra.BinaryRoundTo(pc);
  if pb > pc then
    rb := rb.BinaryRoundTo(pc);

  sz := pc shr W_LOG2_BITS;
  SetLength(buff, 4 * sz + 1);
  Move(ra.Fraction[0], buff[sz], sz * wSz);
  Move(rb.Fraction[0], buff[3 * sz], sz *  wSz);
  ShiftDigits(@buff[2 * sz], 2 * sz, -de);
  carry := _ipAdd(@buff[2 * sz], @buff[0], 2 * sz);

  if carry > 0 then begin
    Inc(ea);
    carry := _shl(@buff[0], @buff[0], cTopBitIdx, 2 * sz);
    buff[2 * sz] := carry or W_HI_BIT;
    carry := BinRoundTo(@buff[0], 2 * sz + 1, @buff[2 * sz + 1], sz, pc);
    Move(buff[2 * sz + 1], buff[0], sz * wSz);
    Inc(ea, carry);
  end else begin
    carry := BinRoundTo(@buff[0], 2 * sz, @buff[2 * sz], sz, pc);
    Move(buff[2 * sz], buff[0], sz * wSz);
  end;
  Inc(ea, carry);

  SetLength(buff, sz);
  Result.Init(buff, ea, 1);
end;

class function TReal.SubtractPositive(const A, B: TReal): TReal;
var ea, eb, de, sz, sgn: Integer;
    pa, pb, pc, carry: Integer;
    buff: TLimbArray;
    ra, rb: TReal;
begin
  if AbsCompare(A, B) < 0 then begin
    ra := B;
    rb := A;
    sgn := -1;
  end else begin
    ra := A;
    rb := B;
    sgn := 1;
  end;
  if rb.IsZeroQ then begin
    ra.Signum := sgn;
    exit(ra);
  end;

  ea := ra.Exponent;
  eb := rb.Exponent;
  pa := ra.BinDigitCount;
  pb := rb.BinDigitCount;
  pc := Min(pa, pb);
  de := ea - eb;
  if de > pc + 1 then begin
    buff := TrimTail(ra.Fraction, (pa - pc) shr W_LOG2_BITS);
    Result.Init(buff, ea, 1);
    exit;
  end;

  if pa > pc then
    ra := ra.BinaryRoundTo(pc);
  if pb > pc then
    rb := rb.BinaryRoundTo(pc);

  sz := pc shr W_LOG2_BITS;
  SetLength(buff, 4 * sz);
  Move(ra.Fraction[0], buff[3 * sz], sz * wSz);
  Move(rb.Fraction[0], buff[sz], sz * wSz);
  ShiftDigits(@buff[0], 2 * sz, -de);
  _ipSub(@buff[2 * sz], @buff[0], 2 * sz);
  carry := Normalize(@buff[0], 2 * sz);
  Dec(ea, carry);
  carry := BinRoundTo(@buff[0], 2 * sz, @buff[2 * sz], sz, pc - carry);
  Inc(ea, carry);
  Move(buff[2 * sz], buff[0], sz * wSz);
  SetLength(buff, sz);
  Result.Init(buff, ea, sgn);
end;

class procedure TReal.ChangePrecision(aSrc: PByte; aSrcLen: Integer;
  aDst: PByte; aDstLen: Integer);
var offset: Integer;
begin
  if aDstLen > aSrcLen then begin
    offset := aDstLen - aSrcLen;
    FillChar(aDst^, offset * wSz, 0);
    Inc(aDst, offset * wSz);
  end else
  if aSrcLen > aDstLen then begin
    offset := aSrcLen - aDstLen;
    Inc(aSrc, offset * wSz);
    Dec(aSrcLen, offset);
  end;
  Move(aSrc^, aDst^, aSrcLen * wSz);
end;

class function TReal.BinFloorTo(aSrc: PByte; aSrcLen: Integer;
  aDst: PByte; aDstLen: Integer; aDigits: UInt64): Integer;
var offset, count, lastDigPos: Integer;
    pSrc, pDst, pEnd: PByte;
    mask: TLimb;
begin
  Result := 0;
  offset := aDigits shr W_LOG2_BITS;
  lastDigPos := Integer(aDigits and cLimbRemMask) - 1;
  count := Min(aSrcLen, aDstLen);
  if offset > count then begin
    ChangePrecision(aSrc, aSrcLen, aDst, aDstLen);
    exit;
  end;

  if lastDigPos < 0 then begin
    lastDigPos := cTopBitIdx;
    Dec(offset);
  end;

  pSrc := aSrc + (aSrcLen - 1) * wSz;
  pDst := aDst + (aDstLen - 1) * wSz;
  pEnd := pSrc - offset * wSz;
  while pSrc > pEnd do begin
    PLimb(pDst)^ := PLimb(pSrc)^;
    Dec(pSrc, wSz);
    Dec(pDst, wSz);
  end;

  lastDigPos := cTopBitIdx - lastDigPos;
  mask := W_MASK shl lastDigPos;
  PLimb(pDst)^ := PLimb(pSrc)^ and mask;
  Dec(pDst, wSz);
  while pDst >= aDst do begin
    PLimb(pDst)^ := 0;
    Dec(pDst, wSz);
  end;
end;

class function TReal.BinCeilTo(aSrc: PByte; aSrclen: Integer;
  aDst: PByte; aDstLen: Integer; aDigits: UInt64): Integer;
var offset, count, lastDigPos: Integer;
    pSrc, pDst, pEnd: PByte;
    mask: TLimb;
begin
  Result := 0;
  offset := aDigits shr W_LOG2_BITS;
  lastDigPos := Integer(aDigits and cLimbRemMask) - 1;
  count := Min(aSrcLen, aDstLen);
  if offset > count then begin
    ChangePrecision(aSrc, aSrcLen, aDst, aDstLen);
    exit;
  end;

  if lastDigPos < 0 then begin
    lastDigPos := cTopBitIdx;
    Dec(offset);
  end;

  pSrc := aSrc + (aSrcLen - 1) * wSz;
  pDst := aDst + (aDstLen - 1) * wSz;
  pEnd := pSrc - offset * wSz;
  while pSrc > pEnd do begin
    PLimb(pDst)^ := PLimb(pSrc)^;
    Dec(pSrc, wSz);
    Dec(pDst, wSz);
  end;

  lastDigPos := cTopBitIdx - lastDigPos;
  mask := W_MASK shl lastDigPos;
  PLimb(pDst)^ := PLimb(pSrc)^ and mask;
  count := offset + 1;
  Result := _Inc(pDst, UInt64(1) shl lastDigPos, count);
  if Result > 0 then begin
    _Shr(pDst, pDst, 1, count);
    pSrc := aDst + (aDstLen - 1) * wSz;
    PLimb(pSrc)^  := W_HI_BIT or PLimb(pSrc)^;
  end;
  Dec(pDst, wSz);
  while pDst >= aDst do begin
    PLimb(pDst)^ := 0;
    Dec(pDst, wSz);
  end;
end;

class function TReal.BinRoundTo(aSrc: PByte; aSrcLen: Integer;
  aDst: PByte; aDstLen: Integer; aDigits: UInt64): Integer;
var offset, digPos: Integer;
    v, mask: TLimb;
begin
  Result := 0;
  digPos := aDigits and cLimbRemMask;
  offset := aDigits shr W_LOG2_BITS;
  if offset >= aSrcLen then begin
    ChangePrecision(aSrc, aSrcLen, aDst, aDstLen);
    exit;
  end;

  offset := aSrcLen - offset - 1;
  v := PLimb(aSrc + offset * wSz)^;

  mask := W_HI_BIT shr digPos;
  if (v and mask) = 0 then begin
    // 2^(aDigits - 1) * rest < 1/2 -> FloorTo(aSrc, aDigits)
    Result := BinFloorTo(aSrc, aSrcLen, aDst, aDstLen, aDigits);
    exit;
  end;

  mask := W_MASK shr (digPos + 1);
  if ((v and mask) <> 0) or (not _IsZero(PLimb(aSrc), offset - 1)) then begin
    // 2^(aDigits - 1) * rest > 1/2 -> CeilTo(aSrc, aDigits)
    Result := BinCeilTo(aSrc, aSrcLen, aDst, aDstLen, aDigits);
    exit;
  end;

  // 2^(aDigits - 1) * rest = 1/2
  mask := W_HI_BIT shr (digPos - 1);
  if digPos = 0 then
    v := PLimb(aSrc + (offset + 1) * wSz)^;

  if (v and mask) = 0 then begin
    // Floor(2^(aDigits - 1) * aSrc) is even -> CeilTo(aSrc, aDigits)
    Result := BinFloorTo(aSrc, aSrcLen, aDst, aDstLen, aDigits);
    exit;
  end else begin
    // Floor(2^(aDigits - 1) * aSrc) is odd -> FloorTo(aSrc, aDigits)
    Result := BinCeilTo(aSrc, aSrcLen, aDst, aDstLen, aDigits);
    exit;
  end;
end;

class function TReal.TrimTail(const aFraction: TLimbArray;
  aCount: Integer): TLimbArray;
var len: Integer;
begin
  if aCount = 0 then exit(aFraction);

  len := Length(aFraction);
  Dec(len, aCount);
  SetLength(Result, len);
  Move(aFraction[aCount], Result[0], len * cLimbSize);
end;

class procedure TReal.ShiftDigits(pFraction: PLimb; aCount: Integer; aDigits: Integer);
var offset: Integer;
    p: PLimb;
    bRight: Boolean;
begin
  if aDigits = 0 then exit;

  bRight := (aDigits < 0);
  aDigits := System.Abs(aDigits);
  offset := aDigits shr W_LOG2_BITS;
  if offset >= aCount then begin
    FillChar(pFraction^, aCount * cLimbSize, 0);
    exit;
  end;

  if bRight then begin
    Dec(aCount, offset);
    aDigits := aDigits and cLimbRemMask;
    p := pFraction;
    Inc(p, offset);
    if aDigits > 0 then
      _shr(PByte(p), PByte(pFraction), aDigits, aCount)
    else
      Move(p^, pFraction^, aCount * wSz);
    p := pFraction;
    Inc(p, aCount);
    FillChar(p^, offset * wSz, 0);
  end else begin
    raise ENotImplemented.Create('ShiftDigits to left is not implemented yet.');
  end;
end;

class function TReal.Normalize(aSrc: PByte; aSrcLen: Integer): Integer;
var pHi: PByte;
    v: TLimb;
    offset, zeroCnt: Integer;
    dst: PByte;
begin
  v := 0;
  offset := 0;
  pHi := aSrc + (aSrcLen - 1) * wSz;
  while pHi >= aSrc do begin
    v := PLimb(pHi)^;
    if v <> 0 then
      break;
    Inc(offset);
    Dec(pHi, wSz);
  end;

  if v = 0 then exit(0); // zero

  dst := aSrc + offset * wSz;
  zeroCnt := _CountLeadingZeros(v);
  if zeroCnt = 0 then begin
    if offset > 0 then
      Move(aSrc^, dst^, (aSrcLen - offset) * wSz)
  end else
    _shl(aSrc, dst, zeroCnt, aSrcLen - offset);
  if offset > 0 then
    FillChar(aSrc^, offset * wSz, 0);
  Result := (offset shl W_LOG2_BITS) + zeroCnt;
end;

class function TReal.Normalize(aSrc, aDst: PByte; aSrcLen: Integer): Integer;
var pHi: PByte;
    v: TLimb;
    offset, zeroCnt: Integer;
    dst: PByte;
begin
  v := 0;
  offset := 0;
  pHi := aSrc + (aSrcLen - 1) * wSz;
  while pHi >= aSrc do begin
    v := PLimb(pHi)^;
    if v <> 0 then
      break;
    Inc(offset);
    Dec(pHi, wSz);
  end;

  if v = 0 then exit(0); // zero

  dst := aDst + offset * wSz;
  zeroCnt := _CountLeadingZeros(v);
  if zeroCnt = 0 then
    Move(aSrc^, dst^, (aSrcLen - offset) * wSz)
  else
    _shl(aSrc, dst, zeroCnt, aSrcLen - offset);
 if offset > 0 then
    FillChar(aDst^, offset * wSz, 0);
  Result := (offset shl W_LOG2_BITS) + zeroCnt;
end;

function TReal.BinDigitCount: Integer;
begin
  Result := Length(fFraction) shl W_LOG2_BITS;
end;

class operator TReal.Add(const A, B: TReal): TReal;
var sa, sb: Integer;
begin
  sa := A.Signum;
  sb := B.Signum;
  if sa * sb > 0 then begin
    Result := AddPositive(A.Abs, B.Abs);
    Result.Signum := sa;
  end else begin
    Result := SubtractPositive(A.Abs, B.Abs);
    Result.Signum := Result.Signum * sa;
  end;
end;

class operator TReal.Add(const A: TInteger; const B: TReal): TReal;
var ra: TReal;
begin
  ra.Init(A, B.BinDigitCount);
  Result := ra + B;
end;

class operator TReal.Add(const A: TReal; const B: TInteger): TReal;
var rb: TReal;
begin
  rb.Init(B, A.BinDigitCount);
  Result := A + rb;
end;

class operator TReal.Add(const A: TRational; const B: TReal): TReal;
var ra: TReal;
begin
  ra.Init(A, B.BinDigitCount);
  Result := ra + B;
end;

class operator TReal.Add(const A: TReal; const B: TRational): TReal;
var rb: TReal;
begin
  rb.Init(B, A.BinDigitCount);
  Result := A + rb;
end;

class operator TReal.Subtract(const A, B: TReal): TReal;
begin
  Result := A + (-B);
end;

class operator TReal.Subtract(const A: TInteger; const B: TReal): TReal;
var rA: TReal;
begin
  rA.Init(A, B.BinDigitCount);
  Result := rA + (-B);
end;

class operator TReal.Subtract(const A: TReal; const B: TInteger): TReal;
var rB: TReal;
begin
  rB.Init(B, A.BinDigitCount);
  Result := A + (-rB);
end;

class operator TReal.Subtract(const A: TRational; const B: TReal): TReal;
var rA: TReal;
begin
  rA.Init(A, B.BinDigitCount);
  Result := rA + (-B);
end;

class operator TReal.Subtract(const A: TReal; const B: TRational): TReal;
var rB: TReal;
begin
  rB.Init(B, A.BinDigitCount);
  Result := A + (-rB);
end;

class operator TReal.Negative(const A: TReal): TReal;
begin
  with Result do begin
    fSgn := -A.fSgn;
    fFraction := A.Fraction;
    fExponent := A.fExponent;
  end;
end;

class operator TReal.Multiply(const A, B: TReal): TReal;
var pa, pb, pc, ec: Integer;
    fa, fb, fc: TInteger;
    f: TLimbArray;
begin
  pa := A.BinDigitCount;
  pb := B.BinDigitCount;
  pc := Min(pa, pb);
  if A.IsZeroQ or B.IsZeroQ then begin
    Result := TReal.Zero(pc);
    exit;
  end;
  fa := A.Fraction;
  fb := B.Fraction;

  ec := A.Exponent + B.Exponent;
  fc := fa * fb;

  f := fc.fData;
  Inc(ec, 1 - Normalize(@f[0], Length(f)));
  Result.Init(f, ec, A.Signum * B.Signum);
  Result := Result.BinaryRoundTo(pc);
end;

class operator TReal.Divide(const A, B: TReal): TReal;
begin
  if Min(Length(A.Fraction), Length(B.Fraction)) < g_MThresholds.NewtonDiv then
    Result := SchoolDiv(A, B)
  else
    Result := A * B.Reciprocal;
end;

class operator TReal.Divide(const A: TInteger; const B: TReal): TReal;
var ra: TReal;
begin
  ra.Init(A, B.BinDigitCount);
  if Min(A.Count, Length(B.Fraction)) < g_MThresholds.NewtonDiv then
    Result := SchoolDiv(ra, B)
  else
    Result := ra * B.Reciprocal;
end;

class operator TReal.Divide(const A: TReal; const B: TInteger): TReal;
var rb: TReal;
begin
  if B.Count < 2(*todo: select threshold*) then exit(A.IntDivide(B));

  rb.Init(B, A.BinDigitCount);
  if Min(Length(A.Fraction), B.Count) < g_MThresholds.NewtonDiv then
    Result := SchoolDiv(A, rb)
  else
    Result := A * rb.Reciprocal;
end;

class operator TReal.Multiply(const A: TInteger; const B: TReal): TReal;
var ra: TReal;
begin
  ra.Init(A, B.BinDigitCount);
  Result := ra * B;
end;

class operator TReal.Multiply(const A: TReal; const B: TInteger): TReal;
var rb: TReal;
begin
  rb.Init(B, A.BinDigitCount);
  Result := A * rb;
end;

{$ifdef FPC}

class operator TReal.Multiply(A: Integer; const B: TReal): TReal;
var ra: TReal;
    da: Double;
begin
  da := A;
  ra.Init(da, B.BinDigitCount);
  Result := ra * B;
end;

class operator TReal.Divide(const A: TReal; B: Integer): TReal;
var rb: TReal;
    db: Double;
begin
  db := B;
  rb.Init(db, A.BinDigitCount);
  Result := A / rb;
end;

{$endif}

function TReal.Abs: TReal;
begin
  Result := Self;
  Result.Signum := 1;
end;

class operator TReal.Implicit(const A: Double): TReal;
begin
  Result.Init(A);
end;

class operator TReal.Explicit(const A: NativeInt): TReal;
begin
  Result.Init(TInteger(A))
end;

class operator TReal.Explicit(const A: TInteger): TReal;
begin
  Result.Init(A);
end;

class operator TReal.Explicit(const A: TReal): Double;
begin
  Result := A.AsDouble;
end;

class function TReal.Compare(const A, B: TReal): Integer;
begin
  if A.Signum < B.Signum then exit(-1);
  if A.Signum > B.Signum then exit(1);
  Result := A.Signum * AbsCompare(A, B);
end;

class function TReal.Compare(const A: TInteger; const B: TReal; aExactly: Boolean): Integer;
var rA: TReal;
    I, count, bCnt, wCnt: Integer;
begin
  if A.Signum < B.Signum then exit(-1);
  if A.Signum > B.Signum then exit(1);
  rA.Init(A, B.BinDigitCount);
  Result := AbsCompare(rA, B);
  if aExactly and (Result = 0) then begin
    count := A.DigitCount - B.BinDigitCount;
    wCnt := count shr W_LOG2_BITS;
    for I := 0 to wCnt - 1 do
      if A.fData[I] <> 0 then begin
        Result := 1;
        break;
      end;
    bCnt := count mod W_BIT_COUNT;
    if (Result = 0) and (bCnt > 0) then begin
      I := W_MASK shr (W_BIT_COUNT - bCnt);
      I := A.fData[wCnt] and I;
      if I <> 0 then
        Result := 1;
    end;
  end;
  Result := A.Signum * Result;
end;

class function TReal.Compare(const A: TReal; const B: TInteger; aExactly: Boolean): Integer;
begin
  Result := -Compare(B, A, aExactly);
end;

class operator TReal.LessThan(const A, B: TReal): Boolean;
begin
  Result := (Compare(A, B) < 0);
end;

class operator TReal.GreaterThan(const A, B: TReal): Boolean;
begin
  Result := (Compare(A, B) > 0);
end;

class operator TReal.LessThanOrEqual(const A, B: TReal): Boolean;
begin
  Result := (Compare(A, B) <= 0);
end;

class operator TReal.GreaterThanOrEqual(const A, B: TReal): Boolean;
begin
  Result := (Compare(A, B) >= 0);
end;

class operator TReal.Equal(const A, B: TReal): Boolean;
begin
  Result := (Compare(A, B) = 0);
end;

class operator TReal.Equal(const A: TInteger; const B: TReal): Boolean;
begin
  Result := B.FractionalPart.IsZeroQ and (B.IntegerPart = A);
end;

class operator TReal.Equal(const A: TReal; const B: TInteger): Boolean;
begin
  Result := (B = A);
end;

class operator TReal.Equal(const A: TRational; const B: TReal): Boolean;
var rA: TReal;
begin
  rA.Init(A, B.BinDigitCount);
  Result := (Compare(rA, B) = 0);
end;

class operator TReal.Equal(const A: TReal; const B: TRational): Boolean;
begin
  Result := (B = A);
end;

class operator TReal.NotEqual(const A, B: TReal): Boolean;
begin
  Result := (Compare(A, B) <> 0);
end;

procedure TReal.SetPrecision(const aValue: Double);
begin
  Self := Self.BinaryRoundTo(Ceil(aValue * Log2(10)));
end;

procedure TReal.SetBinPrecision(const aDigits: Integer);
begin
  Self := Self.BinaryRoundTo(aDigits);
end;

function TReal.BinaryRoundTo(aDigits: Int64): TReal;
var count, newCnt, dexp: Integer;
    fraction: TLimbArray;
begin
  Assert(aDigits > 0);
  count := Length(fFraction);
  if aDigits = W_BIT_COUNT * count then exit(Self);

  newCnt := Max(cMinLimbCount, (aDigits + W_BIT_COUNT - 1) shr W_LOG2_BITS);
  SetLength(fraction, newCnt);
  dexp := BinRoundTo(@fFraction[0], count, @fraction[0], newCnt, aDigits);
  Result.fFraction := fraction;
  Result.Exponent := Exponent + dexp;
  Result.Signum := Signum;
end;

function TReal.BinaryFloorTo(aDigits: Int64): TReal;
var count, newCnt, dexp: Integer;
    fraction: TLimbArray;
begin
  Assert(aDigits > 0);
  count := Length(fFraction);
  if aDigits = W_BIT_COUNT * count then exit(Self);

  newCnt := Max(cMinLimbCount, (aDigits + W_BIT_COUNT - 1) shr W_LOG2_BITS);
  SetLength(fraction, newCnt);
  if Signum > 0 then
    dexp := BinFloorTo(@fFraction[0], count, @fraction[0], newCnt, aDigits)
  else
    dexp := BinCeilTo(@fFraction[0], count, @fraction[0], newCnt, aDigits);
  Result.fFraction := fraction;
  Result.Exponent := Exponent + dexp;
  Result.Signum := Signum;
end;

function TReal.BinaryCeilTo(aDigits: Int64): TReal;
var count, newCnt, dexp: Integer;
    fraction: TLimbArray;
begin
  Assert(aDigits > 0);
  count := Length(fFraction);
  if aDigits = W_BIT_COUNT * count then exit(Self);

  newCnt := Max(cMinLimbCount, (aDigits + W_BIT_COUNT - 1) shr W_LOG2_BITS);
  SetLength(fraction, newCnt);
  if Signum > 0 then
    dexp := BinCeilTo(@fFraction[0], count, @fraction[0], newCnt, aDigits)
  else
    dexp := BinFloorTo(@fFraction[0], count, @fraction[0], newCnt, aDigits);
  Result.fFraction := fraction;
  Result.Exponent := Exponent + dexp;
  Result.Signum := Signum;
end;

function TReal.Reciprocal: TReal;
var u: TReal;
begin
  u.Init(1, BinDigitCount);
  Result := u / Self;
end;

function TReal.IntPower(aExponent: Integer): TReal;
var q: TReal;
    N: Integer absolute aExponent;
begin
  Result := One(BinDigitCount);
  if aExponent = 0 then
    exit;

  if (aExponent < 0) then begin
    aExponent := -aExponent;
    q := Self.Reciprocal;
  end else
    q := Self;

  Result := One(BinDigitCount);
  while True do begin
    if (aExponent and 1) <> 0 then
      Result := q * Result;
    aExponent := aExponent shr 1;
    if aExponent = 0 then break;
    q := q * q;
  end;
end;

function TReal.IntPower(aExponent: TInteger): TReal;
var q: TReal;
begin
  Result := One(BinDigitCount);
  if (aExponent = 0) then
    exit;

  if (aExponent < 0) then begin
    aExponent := -aExponent;
    q := Self.Reciprocal;
  end else
    q := Self;

  while True do begin
    if aExponent.IsOdd then
      Result := q * Result;
    aExponent := aExponent shr 1;
    if aExponent = 0 then break;
    q := q * q;
  end;
end;

function TReal.IntDivide(const B: TInteger): TReal;
var f: TLimbArray;
    fi: TInteger;
    cntA, cntB: Integer;
begin
  cntA := Length(fFraction);
  cntB := B.Count;
  SetLength(f, cntA + cntB);
  Move(fFraction[0], f[cntB], cntA * W_BYTE_COUNT);
  fi.Init(f);
  fi := fi div B;
  Result.Init(fi.Items, fExponent - (B.Count - 1) * W_BIT_COUNT, fSgn);
  Result := Result.BinaryRoundTo(cntA * W_BIT_COUNT);
end;

class function TReal.SchoolDiv(const A, B: TReal): TReal;
var f: TLimbArray;
    fa, fb, fc: TInteger;
    ACnt, BCnt: Integer;
    pc, pa, pb, p: Integer;
begin
  pa := A.BinDigitCount;
  pb := B.BinDigitCount;
  pc := Min(pa, pb);

  if B.IsZeroQ then
    raise EDivByZero.Create('Division by zero.');

  if A.IsZeroQ then begin
    Result := TReal.Zero(pc);
    exit;
  end;

  ACnt := Length(A.Fraction);
  BCnt := Length(B.Fraction);
  SetLength(f, ACnt + BCnt + 1);
  Move(A.Fraction[0], f[BCnt + 1], ACnt * cLimbSize);
  fa.Init(f);
  fb.Init(B.Fraction);
  fc := fa div fb;
  f := fc.fData;
  p := (Normalize(@f[0], Length(f)) + 1) mod W_BIT_COUNT;
  Result.fFraction := f;
  Result.fExponent := A.Exponent - B.Exponent - p;
  Result.fSgn := A.Signum * B.Signum;
  Result := Result.BinaryRoundTo(pc);
end;

class procedure TReal._NRAddPos(const A, B: TReal; var aRes: TReal; aLimbCount: Integer);
var ea, eb, i, de, sz, p: Integer;
    ALen, BLen, resLen: Integer;
    carry: TLimb;
    ra, rb: TReal;
begin
  ea := A.Exponent;
  eb := B.Exponent;
  if ea < eb then begin
    i := ea; ea := eb; eb := i;
    ra := B;
    rb := A;
  end else begin
    ra := A;
    rb := B;
  end;

  sz := aLimbCount;
  ALen := Length(ra.Fraction);
  BLen := Length(rb.Fraction);
  resLen := Length(aRes.Fraction);
  p := aLimbCount * W_BIT_COUNT;
  de := ea - eb;
  if de > p + 1 then begin
    Move(ra.Fraction[ALen - sz], aRes.Fraction[resLen - sz], sz * wSz);
    aRes.Exponent := ea;
    exit;
  end;

  Move(rb.Fraction[BLen - sz], aRes.Fraction[resLen - sz], sz * wSz);
  ShiftDigits(@aRes.Fraction[resLen - 2*sz], 2 * sz, -de);
  carry := _ipAdd(@ra.Fraction[ALen - 2*sz], @aRes.Fraction[resLen - 2*sz], 2*sz);
  if carry > 0 then begin
    Inc(ea);
    _shr(@aRes.Fraction[resLen - 2*sz], @aRes.Fraction[resLen - 2*sz], 1, 2*sz);
    aRes.Fraction[resLen - 1] := aRes.Fraction[resLen - 1] or W_HI_BIT;
  end;
  aRes.Exponent := ea;
end;

class procedure TReal._NRSubPos(const A, B: TReal; var aRes: TReal; aLimbCount: Integer);
var ea, eb, de, sz, sgn: Integer;
    ALen, BLen, resLen: Integer;
    carry: Integer;
    ra, rb: TReal;
begin
  if AbsCompare(A, B) < 0 then begin
    ra := B;
    rb := A;
    sgn := -1;
  end else begin
    ra := A;
    rb := B;
    sgn := 1;
  end;

  ALen := Length(ra.Fraction);
  BLen := Length(rb.Fraction);
  resLen := Length(aRes.Fraction);
  sz := aLimbCount;
  if rb.IsZeroQ then begin
    Move(ra.Fraction[ALen - sz], aRes.Fraction[resLen - sz], sz * wSz);
    aRes.Exponent := ra.Exponent;
    aRes.fSgn := sgn;
    exit;
  end;

  ea := ra.Exponent;
  eb := rb.Exponent;
  de := ea - eb;
  if de > sz * W_BIT_COUNT + 1 then begin
    aRes.fSgn := sgn;
    exit;
  end;

  Move(rb.Fraction[BLen - sz], aRes.Fraction[resLen - sz], sz * wSz);
  ShiftDigits(@aRes.Fraction[resLen - 2*sz], 2*sz, -de);
  _ipSub(@ra.Fraction[ALen - 2*sz], @aRes.Fraction[resLen - 2*sz], 2*sz);
  carry := Normalize(@aRes.Fraction[resLen - 2*sz], 2*sz);
  aRes.Exponent := ea - carry;
  aRes.fSgn := sgn;
end;

class procedure TReal._NRAdd(const A, B: TReal; var aRes: TReal; aLimbCount: Integer);
var sa, sb: Integer;
begin
  sa := A.Signum;
  sb := B.Signum;
  if sa * sb > 0 then begin
    _NRAddPos(A.Abs, B.Abs, aRes, aLimbCount);
    aRes.Signum := sa;
  end else begin
    _NRSubPos(A.Abs, B.Abs, aRes, aLimbCount);
    aRes.Signum := aRes.Signum * sa;
  end;
end;

class procedure TReal._NRSub(const A, B: TReal; var aRes: TReal; aLimbCount: Integer);
begin
  _NRAdd(A, -B, aRes, aLimbCount);
end;

class procedure TReal._NRMul(const A, B: TReal; var aRes: TReal; aLimbCount: Integer);
var de: Integer;
    fa, fb, fc: TInteger;
    f: TLimbArray;
begin
  if A.IsZeroQ or B.IsZeroQ then begin
    FillChar(aRes.Fraction[Length(aRes.Fraction) - 2*aLimbCount], 2*aLimbCount*wSz, 0);
    aRes.Exponent := 0;
    aRes.fSgn := 1;
    exit;
  end;
  fa.Init(@A.Fraction[Length(A.Fraction) - aLimbCount], aLimbCount);
  fb.Init(@B.Fraction[Length(B.Fraction) - aLimbCount], aLimbCount);
  fc := fa * fb; // TInteger.Multiply is used because it selects optimal multiplication algorithm
  f := fc.fData;
  de := Normalize(@fc.fData[0], @aRes.Fraction[Length(aRes.Fraction) - 2*aLimbCount], Length(f)) - 1;
  aRes.Exponent := A.Exponent + B.Exponent - de;
  aRes.Signum := A.Signum * B.Signum;
end;

class function TReal.NewtonDiv(const A, B: TReal): TReal;
var d, x, u, tmp1, tmp2: TReal;
    p, pw, pc, lCnt: Integer;
begin
  p := Min(A.BinDigitCount, B.BinDigitCount);
  pw := 4 * p;
  u.Init(1, pw);
  d.Init(B.Fraction, -1);
  d.SetBinPrecision(pw);
  x := 1/d.AsDouble;
  x.SetBinPrecision(pw);
  tmp1.Init(0, pw);
  tmp2.Init(0, pw);
  pc := 52;
  lCnt := cMinLimbCount;
  pw := pw div W_BIT_COUNT;
  while pc < p + 2 do begin
    pc := 2 * pc;
    // x := x + x * (u - d * x);
    _NRMul(d, x, tmp1, lCnt);
    _NRSub(u, tmp1, tmp2, lCnt + 1); // +1 to avoid catastrophic cancellation
    _NRMul(tmp2, x, tmp1, lCnt);
    _NRAdd(tmp1, x, tmp2, lCnt);
    Move(tmp2.Fraction[pw - 2*lCnt], x.Fraction[pw - 2*lCnt], 2 * lCnt * wSz);
    x.Exponent := tmp2.Exponent;
    x.fSgn := tmp2.fSgn;
    lCnt := 2 * lCnt;
  end;
  Result.Init(x.fFraction);
  Result.Exponent := x.Exponent - B.Exponent - 1;
  Result.Signum := B.Signum; // Result <- 1/B
  Result := Result.BinaryRoundTo(p);
  Result := A * Result;
end;

function TReal.IntegerPart: TInteger;
var e, n, fracLen, offset: Integer;
    digits: TLimbArray;
    carry: TLimb;
begin
  e := Exponent;
  fracLen := Length(fFraction);

  if e < 0 then exit(TInteger.Zero);

  Inc(e);
  if e > fracLen * W_BIT_COUNT then begin
    n := (e + W_BIT_COUNT - 1) shr W_LOG2_BITS;
    offset := e - fracLen * W_BIT_COUNT;
    SetLength(digits, n);
    carry := _shl(@fFraction[0], @digits[offset div W_BIT_COUNT], offset mod W_BIT_COUNT, fracLen);
    digits[n - 1] := carry;
  end else begin
    n := e shr W_LOG2_BITS;
    offset := -(e mod W_BIT_COUNT);
    if offset <> 0 then Inc(n);
    SetLength(digits, n);
    _shr(@fFraction[Length(fFraction) - n], @digits[0], Byte(offset), n);
  end;
  Result.Init(digits, Signum);
end;

function TReal.FractionalPart: TReal;
var e, n, fracLen, offset: Integer;
    digits: TLimbArray;
begin
  e := Exponent;
  fracLen := Length(fFraction);

  if (e < 0) or IsZeroQ then
    exit(Self);

  if e >= fracLen * W_BIT_COUNT then
    exit(TReal.Zero(fracLen * W_BIT_COUNT));

  Inc(e);
  n := e div W_BIT_COUNT;
  offset := e mod W_BIT_COUNT;
  SetLength(digits, fracLen);
  if fracLen > n then
    _shl(@fFraction[0], @digits[n], offset, fracLen - n);
  Result.Init(digits, -1, Signum);
end;

class function TReal.One(aDigitCount: Integer): TReal;
var sz: Integer;
begin
  Assert(aDigitCount > 0);
  sz := Max(cMinLimbCount, (aDigitCount + W_BIT_COUNT - 1) div W_BIT_COUNT);
  with Result do begin
    fFraction := nil;
    SetLength(fFraction, sz);
    fFraction[sz - 1] := W_HI_BIT;
    Exponent := 0;
    Signum := 1;
  end;
end;

class function TReal.Zero(aDigitCount: Integer): TReal;
var sz: Integer;
begin
  Assert(aDigitCount > 0);
  sz := Max(cMinLimbCount, (aDigitCount + W_BIT_COUNT - 1) div W_BIT_COUNT);
  with Result do begin
    fFraction := nil;
    SetLength(fFraction, sz);
    Exponent := 0;
    Signum := 1;
  end;
end;

function TReal.IsZeroQ: Boolean;
begin
  Result := ((fFraction[High(fFraction)] and W_HI_BIT) = 0);
end;

function TReal.ToDecimalString: String;
var e, p, carry: Integer;
    cDigs, reqDigs, digsDiff: Integer;
    I, count, dExp: Integer;
    x: TReal;
    ip: TInteger;
    s: String;
    dl: Double;
    digs: TList<UInt64>;
    ii: UInt64;
const
  b: UInt64 = 10000000000000000000;
  bDigCount = 19;
  BIT_STEP  = 64;

  function DecimalShift(const aA: TReal; I: Integer): TReal;
  var x: TReal;
  begin
    if I <> 0 then begin
      x.Init(10, p + W_BIT_COUNT);
      Result := aA.BinaryRoundTo(p + W_BIT_COUNT);
      Result := Result * RealPower(x, I);
    end else
      Result := aA;
  end;

  function DecimalRoundRest(var aA: UInt64; aPos: Integer): Integer;
  var q, r, dpwr: TInteger;
  begin
    if aA = 0 then exit(0);
    dpwr := TInteger.Power(10, aPos);
    TInteger.DivMod(aA, dpwr, q, r);
    if r > dpwr shr 1 then
      Inc(q);
    TInteger.DivMod(q * dpwr, b, q, r);
    aA := UInt64(r);
    Result := Integer(q);
  end;

  function IncUInt64(var aA: UInt64): Integer;
  begin
    if aA = UInt64($FFFFFFFFFFFFFFFF) then begin
      aA := 0;
      Result := 1;
    end else begin
      Inc(aA);
      Result := 0;
    end;
  end;

begin
  if IsZeroQ then exit('0');

  e := Exponent;
  p := BinDigitCount;

  x.Init(fFraction, 0, Signum);
  dl := Log10(x.AsDouble) + e * Log10(2);
  dExp := Floor(dl);
  x := DecimalShift(Self, -Ceil(dl));

  digs := TList<UInt64>.Create;
  try
    reqDigs := Floor(Log10(2) * BinDigitCount);
    cDigs := 0;

    repeat
      x := b * x;
      ip := x.IntegerPart;
      x := x.FractionalPart;
      digs.Add(UInt64(ip));
      Inc(cDigs, bDigCount);
    until cDigs > reqDigs;

    digsDiff := cDigs - reqDigs;
    if digsDiff > 0 then begin
      // Round to the last significant digit
      count := digs.Count;
      ii := digs[count - 1];
      carry := DecimalRoundRest(ii, digsDiff);
      I := count - 1;
      while True do begin
        digs[I] := ii;
        if (carry = 0) or (I = 0) then break;
        Dec(I);
        ii := digs[I];
        carry := IncUInt64(ii);
      end;
    end;

    s := UIntToStr(digs.First);
    Result := {$ifdef FPC}AnsiString(s){$else}s{$endif}.Insert(1, '.');
    count := digs.Count;
    for I := 1 to count - 2 do
      Result := Format('%s%0.19u', [Result, UInt64(digs[I])]);
    if digsDiff > 0 then begin
      s := UIntToStr(digs.Last);
      Result := Result + s.Substring(0, bDigCount - digsDiff);
    end;
  finally
    digs.Free;
  end;

  if x.IsZeroQ then begin
    count := 0;
    for I := Length(Result) downto 1 do begin
      if Result[I] <> '0' then break;
      Inc(count);
    end;
    if count > 0 then begin
      count := Length(Result) - count;
      SetLength(Result, count);
    end;
  end;
  Result := Result.TrimRight(['0']);

  if dExp <> 0 then
    Result := Format('%se%d', [Result, dExp]);
end;

class function TReal.TryParse(const aValue: String; out aRes: TReal; aBase: Integer): Boolean;
var digs: Integer;
begin
  Result := TryParse(aValue, aRes, digs, aBase);
end;

class function TReal.TryParse(const aValue: String; out aRes: TReal;
  out aDigitCount: Integer; aBase: Integer): Boolean;
var sarr: TArray<String>;
    i: TInteger;
    exp: TReal;
    p: Integer;
begin
  sarr := aValue.Split(['.']);
  if Length(sarr) <> 2 then exit(False);

  if not TInteger.TryParse(sarr[0] + sarr[1], i, aBase) then
    exit(False);

  aDigitCount := i.DigitCount;
  aRes.Init(i.fData, i.Count * W_BIT_COUNT - 1, i.Signum);
  p := aRes.BinDigitCount;
  aRes := aRes.BinaryRoundTo(p + W_BIT_COUNT);
  exp.Init(10, aRes.BinDigitCount);
  aRes := aRes * RealPower(exp, -Length(sarr[1]));
  aRes := aRes.BinaryRoundTo(p);
  Result := True;
end;

{$region 'Getters/Setters'}

function TReal.GetSignum: Integer;
begin
  Result := fSgn;
end;

procedure TReal.SetSignum(aValue: Integer);
begin
  if aValue < 0 then
    fSgn := -1
  else
    fSgn := 1;
end;

{$endregion}

{$endregion}

{$region 'TCmplx'}

procedure TCmplx.Init(const aRe, aIm: TReal);
begin
  Re := aRe;
  Im := aIm;
end;

class operator TCmplx.Add(const A, B: TCmplx): TCmplx;
begin
  Result.Re := A.Re + B.Re;
  Result.Im := A.Im + B.Im;
end;

class operator TCmplx.Add(const A: TCmplx; const B: TReal): TCmplx;
begin
  Result.Re := A.Re + B;
  Result.Im := A.Im;
end;

class operator TCmplx.Subtract(const A, B: TCmplx): TCmplx;
begin
  Result.Re := A.Re - B.Re;
  Result.Im := A.Im - B.Im;
end;

class operator TCmplx.Subtract(const A: TCmplx; const B: TReal): TCmplx;
begin
  Result.Re := A.Re - B;
  Result.Im := A.Im;
end;

class operator TCmplx.Negative(const A: TCmplx): TCmplx;
begin
  Result.Re := -A.Re;
  Result.Im := -A.Im;
end;

class operator TCmplx.Multiply(const A, B: TCmplx): TCmplx;
begin
  Result.Re := A.Re * B.Re - A.Im * B.Im;
  Result.Im := A.Im * B.Re + A.Re * B.Im;
end;

class operator TCmplx.Multiply(const A: TReal; const B: TCmplx): TCmplx;
begin
  Result.Re := A * B.Re;
  Result.Im := A * B.Im;
end;

class operator TCmplx.Divide(const A, B: TCmplx): TCmplx;
var d: TReal;
begin
  d := (B.Re * B.Re + B.Im * B.Im).Reciprocal;
  Result.Re := (A.Re * B.Re + A.Im * B.Im) * d;
  Result.Im := (A.Im * B.Re - A.Re * B.Im) * d;
end;

class operator TCmplx.Divide(const A: TCmplx; const B: TReal): TCmplx;
var d: TReal;
begin
  d := B.Reciprocal;
  Result.Re := A.Re * d;
  Result.Im := A.Im * d;
end;

function TCmplx.Conjugate: TCmplx;
begin
  Result.Re := Re;
  Result.Im := -Im;
end;

function TCmplx.Reciprocal: TCmplx;
var d: TReal;
begin
  d := (Re * Re + Im * Im).Reciprocal;
  Result.Re := Re * d;
  Result.Im := -Im * d;
end;

function TCmplx.Abs: TReal;
begin
  Result := RealSqrt(Re * Re + Im * Im);
end;

{$endregion}

initialization

  with g_MThresholds do begin
    KaratsubaMul  := 40;
    NewtonDiv     := 500;
  end;

end.
