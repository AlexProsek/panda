unit panda.MPNums;

interface

uses
    System.Math
  , System.SysUtils
  , panda.Nums
  ;

{$I AsmDefs.inc}

{$if defined(ASMx64)}
  {$define Limb64}
{$else}
  {$define Limb32}
{$endif}

type
{$ifdef Limb64}
  TLimb = UInt64;
{$else}
  TLimb = Cardinal;
{$endif}
  PLimb = ^TLimb;
  TLimbArray = TArray<TLimb>;

const
  I64_HI_BIT    = $8000000000000000;
  I64_SIGN_BIT  = I64_HI_BIT;
  I64_MASK      = $FFFFFFFFFFFFFFFF;
  I64_BIT_COUNT = 64;

  I32_HI_BIT    = $80000000;
  I32_SIGN_BIT  = I32_HI_BIT;
  I32_MASK      = $FFFFFFFF;
  I32_BIT_COUNT = 32;

{$ifdef Limb64}
  W_HI_BIT          = I64_HI_BIT;
  W_BIT_COUNT       = I64_BIT_COUNT;
  W_LOG2_BITS       = 6; // Log2(W_BIT_COUNT)
  W_LOG2_BYTES      = 3;
  W_MASK            = I64_MASK;
  W_HI_MASK         = $FFFFFFFF00000000;
  W_LO_MASK         = $00000000FFFFFFFF;
{$else}
  W_HI_BIT          = I32_HI_BIT;
  W_BIT_COUNT       = I32_BIT_COUNT;
  W_LOG2_BITS       = 5;
  W_LOG2_BYTES      = 2;
  W_MASK            = I32_MASK;
  W_HI_MASK         = $FFFF0000;
  W_LO_MASK         = $0000FFFF;
{$endif}
  W_SIGN_BIT        = W_HI_BIT;
  W_BYTE_COUNT      = W_BIT_COUNT div 8;

  cBit32  = UInt32(1) shl 31;
  cBit64  = UInt64(1) shl 63;

  cLimbSize     = SizeOf(TLimb);
{$ifdef Limb64}
  cLimbBits     = 64;
  cTopBitIdx    = 63;
  cTopBit       = UInt64(1) shl 63;
  cLimbRemMask  = $3f;
{$else}
  cLimbBits     = 32;
  cTopBitIdx    = 31;
  cWordBits     = 32;
  cTopBit       = Cardinal(1) shl 31;
  cLimbRemMask  = $1f;
{$endif}
  cNibbleMask   = $f;
  cHexNumSymbols: array [0..cNibbleMask] of Char =
    ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');

type
  TInt128 = record
  private const
    LCnt = {$ifdef Limb64}2{$else}4{$endif};
  public type
    TLimbs = array [0..1] of UInt64;
  private
    fLimbs: TLimbs;
    function Hi32: Cardinal; inline;
    class procedure LongDivMod(var A, B: TInt128); static;
    class procedure ShortDivMod(var A, B: TInt128); static;
    class function Compare(const A, B: TInt128): Integer; static; inline;
  public
    procedure Init(const aLo, aHi: UInt64); overload; inline;
    class operator Implicit(const aValue: Int64): TInt128; inline;
    class operator Implicit(const aValue: UInt64): TInt128; inline;
    class operator Explicit(const aValue: TInt128): Int64; inline;
    class operator Add(const A, B: TInt128): TInt128; inline;
    class operator Subtract(const A, B: TInt128): TInt128; inline;
    class operator Negative(const A: TInt128): TInt128; inline;
    class operator Multiply(const A, B: TInt128): TInt128;
    class operator IntDivide(const A, B: TInt128): TInt128;
    class operator Modulus(const A, B: TInt128): TInt128;
    class operator RightShift(const A: TInt128; const B: Cardinal): TInt128;
    class operator LeftShift(const A: TInt128; const B: Cardinal): TInt128;
    class operator LessThan(const A, B: TInt128): Boolean; inline;
    class operator GreaterThan(const A, B: TInt128): Boolean; inline;
    class operator LessThanOrEqual(const A, B: TInt128): Boolean; inline;
    class operator GreaterThanOrEqual(const A, B: TInt128): Boolean; inline;
    class operator Equal(const A, B: TInt128): Boolean; inline;
    class operator NotEqual(const A, B: TInt128): Boolean; inline;

    function ZeroQ: Boolean; inline;
    function Lo: UInt64; inline;
    function Hi: UInt64; inline;
  end;
  PInt128 = ^TInt128;

  TUInt128 = record
  private const
    LCnt = {$ifdef Limb64}2{$else}4{$endif};
  public type
    TLimbs = array [0..1] of UInt64;
  private
    fLimbs: TLimbs;
    function Hi32: Cardinal; inline;
    class procedure LongDivMod(var A, B: TUInt128); static;
    class procedure ShortDivMod(var A, B: TUInt128); static;
    class function Compare(const A, B: TUInt128): Integer; static; inline;
  public
    procedure Init(const aLo, aHi: UInt64); overload; inline;
    class operator Implicit(const aValue: UInt64): TUInt128; inline;
    class operator Explicit(const aValue: TUInt128): UInt64; inline;
    class operator Add(const A, B: TUInt128): TUInt128; inline;
    class operator Subtract(const A, B: TUInt128): TUInt128; inline;
    class operator Inc(const A: TUInt128): TUInt128; inline;
    class operator Negative(const A: TUInt128): TUInt128; inline;
    class operator Multiply(const A, B: TUInt128): TUInt128;
    class operator IntDivide(const A, B: TUInt128): TUInt128;
    class operator Modulus(const A, B: TUInt128): TUInt128;
    class operator RightShift(const A: TUInt128; const B: Cardinal): TUInt128;
    class operator LeftShift(const A: TUInt128; const B: Cardinal): TUInt128;
    class operator LessThan(const A, B: TUInt128): Boolean; inline;
    class operator GreaterThan(const A, B: TUInt128): Boolean; inline;
    class operator LessThanOrEqual(const A, B: TUInt128): Boolean; inline;
    class operator GreaterThanOrEqual(const A, B: TUInt128): Boolean; inline;
    class operator Equal(const A, B: TUInt128): Boolean; inline;
    class operator NotEqual(const A, B: TUInt128): Boolean; inline;
    class operator BitwiseAnd(const A, B: TUInt128): TUInt128; inline;
    class operator BitwiseOr(const A, B: TUInt128): TUInt128; inline;
    class operator BitwiseXor(const A, B: TUInt128): TUInt128; inline;
    class operator LogicalNot(const A: TUInt128): TUInt128; inline;

    function ZeroQ: Boolean; inline;
    function Lo: UInt64; inline;
    function Hi: UInt64; inline;
  end;
  PUInt128 = ^TUInt128;

  TReal128 = record
  private const
    MAX_EXPONENT        =  16383;
    MIN_EXPONENT        = -16382;
    EXP_BIAS            =  16383; // 2^14 - 1
    SGN_MASK            = $8000000000000000;
    EXP_MASK            = $7FFF000000000000;
    HI_FRAC_MASK        = $0000FFFFFFFFFFFF;
    LEADING_ONE         = $0001000000000000;
    OF_QUARD            = LEADING_ONE shl 1;
    BIT_CNT             = 128;
    FRAC_BIT_CNT        = 112;
    FRAC_OFFSET         = 16;

    EXP_BIAS_F64        = 1023;
    FRAC_OFFSET_F64     = 12;
    FRAC_MASK_F64       = $000FFFFFFFFFFFFF;

    cLCnt = {$ifdef Limb64}2{$else}4{$endif};
  private
    fLimbs: array [0..1] of UInt64;
    class function AbsCompare(const A, B: TReal128): Integer; static;
    class function AddPositive(const A, B: TReal128): TReal128; static;
    class function SubtractPositive(const A, B: TReal128): TReal128; static;
    /// aBuff is 256-bit buffer
    class function Round(const aBuff): Integer; static;
    class procedure AdjustByReminder(const A, B: TReal128; var Q: TReal128); static;
    function InternalReciprocal: TReal128;
  {$region 'Getters/Setters'}
    function GetSignum: Integer; inline;
    procedure SetSignum(aValue: Integer); inline;
    function GetExponent: Integer; inline;
    procedure SetExponent(aValue: Integer); inline;
  {$endregion}
  public
    procedure Init(const aValue: Double); overload;
    procedure Init(const aLo, aHi: UInt64); overload; inline;
    class operator Implicit(const A: Double): TReal128; overload; inline;
    class operator Explicit(const A: TReal128): Double; overload; inline;
    class operator Add(const A, B: TReal128): TReal128;
    class operator Subtract(const A, B: TReal128): TReal128; inline;
    class operator Negative(const A: TReal128): TReal128; inline;
    class operator Multiply(const A, B: TReal128): TReal128;
    class operator Divide(const A, B: TReal128): TReal128; inline;
    class function Compare(const A, B: TReal128): Integer; static; inline;
    class operator LessThan(const A, B: TReal128): Boolean; inline;
    class operator GreaterThan(const A, B: TReal128): Boolean; inline;
    class operator LessThanOrEqual(const A, B: TReal128): Boolean; inline;
    class operator GreaterThanOrEqual(const A, B: TReal128): Boolean; inline;
    class operator Equal(const A, B: TReal128): Boolean; inline;
    class operator NotEqual(const A, B: TReal128): Boolean; inline;

    function Reciprocal: TReal128; inline;
    function Sqrt: TReal128;
    function AsDouble: Double;
    function Abs: TReal128; inline;
    function IsExactZero: Boolean; inline;
    function IsZero: Boolean;  overload; inline;
    function IsZero(const aEps: TReal128): Boolean; overload; inline;
    function Lo: UInt64; inline;
    function Hi: UInt64; inline;

    property Signum: Integer read GetSignum write SetSignum;
    property Exponent: Integer read GetExponent write SetExponent;
  end;
  PReal128 = ^TReal128;

  TCmplx256 = record
    Re, Im: TReal128;
    procedure Init(const aRe, aIm: TReal128);
    class operator Add(const A, B: TCmplx256): TCmplx256; inline;
    class operator Subtract(const A, B: TCmplx256): TCmplx256; inline;
    class operator Multiply(const A, B: TCmplx256): TCmplx256; inline;
    class operator Multiply(const A: TReal128; const B: TCmplx256): TCmplx256; inline;
    class operator Divide(const A, B: TCmplx256): TCmplx256;
    class operator Divide(const A: TCmplx256; const B: TReal128): TCmplx256;
    class operator Negative(const A: TCmplx256): TCmplx256; inline;
    class operator Implicit(const A: TReal128): TCmplx256; inline;
    class operator Explicit(const A: Double): TCmplx256; inline;
    function Reciprocal: TCmplx256; inline;
    function IsReal: Boolean; overload; inline;
    function IsReal(const aEps: TReal128): Boolean; overload; inline;
    function IsZero: Boolean; overload; inline;
    function IsZero(const aEps: TReal128): Boolean; overload; inline;
    function Abs: TReal128;
  end;
  PCmplx256 = ^TCmplx256;

const
  cEpsF128:   TReal128 = (fLimbs: (0, $3f8f000000000000));  // 2^-112
  cOneF128:   TReal128 = (fLimbs: (0, $3fff000000000000));
  cZeroF128:  TReal128 = (fLimbs: (0, 0));
  cNaNF128:   TReal128 = (fLimbs: ($ffffffffffffffff, $7fffffffffffffff));
  cInfF128:   TReal128 = (fLimbs: (0, $7fff000000000000));
  cNInfF128:  TReal128 = (fLimbs: (0, $ffff000000000000));

  cZeroC256:  TCmplx256 = (Re: (fLimbs: (0, 0)); Im: (fLimbs: (0, 0)));

{$region 'low-level functions'}

/// <summary>
///   Evaluates in-place addition <c>B <- A + B</c>. It's supposed that <c>A</c> and <c>B</c> are
///   positive numbers. It's also supposed that the both numbers have the same size.
/// </summary>
/// <remarks>
///   <para>
///     &#x2022; Function returns 1 when result overflows the output buffer
///   </para>
/// </remarks>
function _ipAdd(pA, pB: PByte; n: NativeInt): TLimb;
/// <summary>
///   Evaluates in-place subtraction <c>B <- A - B</c>. It's supposed that <c>A</c> and <c>B</c> are
///   positive numbers and <c>A</c> is greather than or equal to the <c>B</c>. It's also
///   supposed that the both numbers have the same size.
/// </summary>
procedure _ipSub(pA, pB: PByte; n: NativeInt);
/// <summary>
///   Evaluates <c>A + D</c>, where <c>D</c> is machine precision unsigned integer.
///   Result is stored in <c>A</c> buffer.
/// </summary>
function _Inc(pA: PByte; D: TLimb; n: NativeInt): TLimb;

function _AddI128(pA, pB, pRes: PByte): Integer;
function _SubI128(pA, pB, pRes: PByte): Integer;
procedure _NegI128(pA, pRes: PByte);

function _IncUI128(pA: PByte; D: TLimb): TLimb;
{$if defined(ASMx64)}
procedure _MulUI128(pA, pB, pRes: PByte);
{$endif}

procedure _Mul(pA, pB, pRes: PByte; nA, nB: NativeInt);
function _ShortDivMod(pA, pB, pRes: PByte; nA: NativeInt): TLimb;
procedure _LongDivMod(pA, pB, pQ: PByte; nA: NativeInt; nB: NativeInt);

/// <summary>
///   Executes left bit shift.
/// </summary>
/// <param name="pSrc">Points to the  source data.</param>
/// <param name="pDst">Points to the  of the destination data.</param>
/// <param name="Count">
///   Number of bits. Result will be shifted by this count. It's supposed that
///   <c>Count</c> is less than or eqaul 31 (number of bits of the word).
/// </param>
/// <param name="Nsrc">Number of source's words.</param>
/// <returns>
///   Carry from the heighest word.
/// </returns>
function _shl(pSrc, pDst: PByte; count: Byte; Nsrc: NativeInt): TLimb;
/// <summary>
///   Executes right bit shift.
/// </summary>
/// <param name="pSrc">Points to the source data.</param>
/// <param name="pDst">Points to the the destination data.</param>
/// <param name="Count">
///   Number of bits. Result will be shifted by this count. It's supposed that
///   <c>Count</c> is less than or eqaul 31 (number of bits of the word).
/// </param>
/// <param name="Nsrc">Number of source's limbs.</param>
function _shr(pSrc, pDst: PByte; count: Byte; Nsrc: NativeInt): TLimb;
/// <summary>
///   Executes bit shift in the direction according to <c>aDigits</c> signum
/// </summary>
/// <remarks>
///   <para>
///     &#x2022; For <c>aDigits > 0</c> is executed left shift and for
///     <c>aDigits</c> is executed right shift.
///   </para>
///   <para>
///     &#x2022; Unlinke <c>_shr</c>, <c>_shl</c> the <c>aDigits</c> can be arbitrary number.
///   </para>
/// </remarks>
procedure _ipLongShift(pA: PLimb; aCount: Integer; aDigits: NativeInt);

/// <summary>
///   Finds offset of the most significant bit of the number <c>A</c>.
/// </summary>
/// <remarks>
///   Index 1 corresponds with the least significant digit. If the <c>A</c>
///   is equal to zero then function returns zero.
/// </remarks>
function _TopBitPos(A: TLimb): NativeInt; overload;
function _TopBitPos(pA: PLimb; N: NativeInt): NativeInt; overload;
function _CountLeadingZeros(A: TLimb): NativeInt; {$ifndef ASM}inline;{$endif} overload;
function _CountLeadingZeros(pA: PLimb; N: NativeInt): NativeInt; overload;
function _CountTrailingZeros(A: TLimb): NativeInt; overload;
function _IsZero(pA: PLimb; N: NativeInt): Boolean;
/// <summary>
///   Returns the number of limbs without leading zero limbs.
/// </summary>
/// <param name="pA">points to the buffer</param>
/// <param name="N">length of the buffer (number of the buffer's limbs)</param>
function _Length(pA: PByte; N: NativeInt): NativeInt;

{$endregion}

{$region 'exception helpers'}

procedure RaiseInt128Overflow;

{$endregion}

implementation

procedure RaiseInt128Overflow;
begin
  raise EIntOverflow.Create('TInt128 overflow.');
end;

{$region 'helper functions'}

function HiLimbBaseQuotient(aValue: TLimb): TLimb; {$ifndef CPUx64}inline;{$endif}
{$if defined(ASMx64)}
asm
  // rcx <- aValue
  cmp rcx, $FFFFFFFFFFFFFFFF
  jne @Q
  mov rax, 1
  ret
@Q:
  inc rcx
  mov rdx, 1
  xor rax, rax
  div rcx
end;
{$else}
begin
  Result := $100000000 div (UInt64(aValue) + 1);
end;
{$endif}

{$endregion}

{$region 'low-level functions'}

function _ipAdd(pA, pB: PByte; n: NativeInt): TLimb;
{$if defined(ASMx86)}
asm
  //pA -> EAX, pB -> EDX, n -> ECX
  push ebx
  push edi
  push esi
  mov esi, eax
  mov edi, edx
  xor edx, edx //EDX will be used to store a carry flag
@L:
  bt edx, 0 //set the carry flag
  mov eax, [esi]
  mov ebx, [edi]
  adc eax, ebx
  mov edx, 0
  adc edx, 0
  mov [edi], eax
  add esi, 4
  add edi, 4
  dec ecx
  jnz @L

  mov eax, edx //return the last carry flag
  pop esi
  pop edi
  pop ebx
end;
{$elseif defined(ASMx64)}
asm
  // pA -> RCX, pB -> RDX, n -> R8
  mov r10, rcx
  mov r11, rdx
  xor rdx, rdx // RDX is used for carry flag
  // pA -> R10, pB -> R11
@L:
  bt rdx, 0 // set the carry flag
  mov rax, [r10]
  adc rax, [r11]
  mov rdx, 0
  adc rdx, 0
  mov [r11], rax
  add r10, 8
  add r11, 8
  dec r8
  jnz @L

  mov rax, rdx
end;
{$else}
var tmp: UInt64;
    pair: array [0..1] of Cardinal absolute tmp;
    carry: Cardinal;
    pAc, pBc: PCardinal;
    pEnd: PByte;
begin
  carry := 0;
  pAc := PCardinal(pA);
  pBc := PCardinal(pB);
  pEnd := pA + n * SizeOf(Cardinal);
  while PByte(pAc) < pEnd do begin
    tmp := pAc^;
    Inc(tmp, pBc^);
    Inc(tmp, carry);
    pBc^ := pair[0];
    carry := pair[1];
    Inc(pAc);
    Inc(pBc);
  end;
  Result := carry;
end;
{$endif}

procedure _ipSub(pA, pB: PByte; n: NativeInt);
{$if defined(ASMx86)}
asm
  //pA -> EAX, pB -> EDX, n -> ECX
  push ebx
  push esi
  push edi
  mov esi, eax
  mov edi, edx
  xor edx, edx //EDX will be used to store a carry flag
@L:
  bt edx, 0 //set the carry flag
  mov eax, [esi]
  mov ebx, [edi]
  sbb eax, ebx
  mov edx, 0
  adc edx, 0
  mov [edi], eax
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
  // pA -> RCX, pB -> RDX, n -> R8
  mov r10, rcx
  mov r11, rdx
  xor rdx, rdx
@L:
  bt rdx, 0 // set the carry flag
  mov rax, [r10]
  sbb rax, [r11]
  mov rdx, 0
  adc rdx, 0
  mov [r11], rax
  add r10, 8
  add r11, 8
  dec r8
  jnz @L
end;
{$else}
var tmp: UInt64;
    pair: array [0..1] of Cardinal absolute tmp;
    carry: Cardinal;
    pAc, pBc: PCardinal;
    pEnd: PByte;
begin
  carry := 0;
  pAc := PCardinal(pA);
  pBc := PCardinal(pB);
  pEnd := pA + n * cLimbSize;
  while PByte(pAc) < pEnd do begin
    tmp := pAc^;
    pair[1] := 1;
    Dec(tmp, pBc^);
    Dec(tmp, carry);
    pBc^ := pair[0];
    carry := 1 xor pair[1];
    Inc(pAc);
    Inc(pBc);
  end;
end;
{$endif}

procedure _NegMPI(pA, pRes: Pbyte; N: NativeInt);
{$if defined(ASMx64)}
// RCX <- pA, RDX <- pRes, r8 <- N
asm
  mov rax, 1
@L:
  mov r9, [rcx]
  not r9
  add r9, rax
  mov rax, 0
  adc rax, 0
  mov [rdx], r9
  add rcx, 8
  add rdx, 8
  dec r8
  jnz @L
end;
{$else}
var C: UInt64;
    CArr: array [0..1] of Cardinal absolute C;
    I: NativeInt;
    cr: Cardinal;
begin
  cr := 1;
  for I := 0 to N - 1 do begin
    CArr[1] := 0;
    CArr[0] := not PCardinal(pA)^;
    C := C + cr;
    PCardinal(pRes)^ := CArr[0];
    cr := CArr[1];
    Inc(pRes, cI32Sz);
    Inc(pA, cI32Sz);
  end;
end;
{$endif}

function _Inc(pA: PByte; D: TLimb; n: NativeInt): TLimb;
{$if defined(ASMx86)}
asm
  //EAX <- pA, EDX <- D, ECX <- n
  push esi
  mov esi, eax
@L:
  mov eax, [esi]
  add eax, edx
  mov edx, 0
  adc edx, 0
  mov [esi], eax
  add esi, 4
  dec ecx
  jnz @L

  mov eax, edx
  pop esi
end;
{$elseif defined(ASMx64)}
asm
  // RCX <- pA, RDX <- D, R8 <- n
@L:
  mov rax, [rcx]
  add rax, rdx
  mov rdx, 0
  adc rdx, 0
  mov [rcx], rax
  add r10, 8
  dec r8
  jnz @L

  mov rax, rdx
end;
{$else}
var tmp: UInt64;
    pair: array [0..1] of Cardinal absolute tmp;
    pAc: PCardinal;
    pEnd: PByte;
begin
  pAc := PCardinal(pA);
  pEnd := pA + n * cLimbSize;
  while PByte(pAc) < pEnd do begin
    tmp := pAc^;
    Inc(tmp, D);
    pAc^ := pair[0];
    D := pair[1];
    Inc(pAc);
  end;
  Result := D;
end;
{$endif}

function _AddI128(pA, pB, pRes: PByte): Integer;
{$if defined(ASMx64)}
// RCX <- pA, RDX <- pB, R8 <- pRes
asm
  xor rax, rax
  mov r9, [rcx]
  mov r10, [rcx + 8]
  add r9, [rdx]
  adc r10, [rdx + 8]
  mov [r8], r9
  mov [r8 + 8], r10
  adc rax, 0 // return the last carry
end;
{$else}
var C: UInt64;
    CArr: array [0..1] of Cardinal absolute C;
begin
  C := UInt64(PCardinal(pA)^) + PCardinal(pB)^;
  PCardinal(pRes)^ := CArr[0];
  Inc(pRes, 4);
  Inc(pA, 4);
  Inc(pB, 4);
  C := UInt64(PCardinal(pA)^) + PCardinal(pB)^ + CArr[1];
  PCardinal(pRes)^ := CArr[0];
  Inc(pRes, 4);
  Inc(pA, 4);
  Inc(pB, 4);
  C := UInt64(PCardinal(pA)^) + PCardinal(pB)^ + CArr[1];
  PCardinal(pRes)^ := CArr[0];
  Inc(pRes, 4);
  Inc(pA, 4);
  Inc(pB, 4);
  C := UInt64(PCardinal(pA)^) + PCardinal(pB)^ + CArr[1];
  PCardinal(pRes)^ := CArr[0];
  Result := CArr[1];
end;
{$endif}

function _SubI128(pA, pB, pRes: PByte): Integer;
{$if defined(ASMx64)}
// RCX <- pA, RDX <- pB, R8 <- pRes
asm
  xor rax, rax
  mov r9, [rcx]
  mov r10, [rcx + 8]
  sub r9, [rdx]
  sbb r10, [rdx + 8]
  mov [r8], r9
  mov [r8 + 8], r10
  adc rax, 0
end;
{$else}
var C: UInt64;
    CArr: array [0..1] of Cardinal absolute C;
    cr: Cardinal;
begin
  cr := 0;
  CArr[1] := 1;
  CArr[0] := PCardinal(pA)^;
  C := C - PCardinal(pB)^ - cr;
  PCardinal(pRes)^ := CArr[0];
  cr := CArr[1] xor $1;
  Inc(pRes, 4);
  Inc(pA, 4);
  Inc(pB, 4);
  CArr[1] := 1;
  CArr[0] := PCardinal(pA)^;
  C := C - PCardinal(pB)^ - cr;
  PCardinal(pRes)^ := CArr[0];
  cr := CArr[1] xor $1;
  Inc(pRes, 4);
  Inc(pA, 4);
  Inc(pB, 4);
  CArr[1] := 1;
  CArr[0] := PCardinal(pA)^;
  C := C - PCardinal(pB)^ - cr;
  PCardinal(pRes)^ := CArr[0];
  cr := CArr[1] xor $1;
  Inc(pRes, 4);
  Inc(pA, 4);
  Inc(pB, 4);
  CArr[1] := 1;
  CArr[0] := PCardinal(pA)^;
  C := C - PCardinal(pB)^ - cr;
  PCardinal(pRes)^ := CArr[0];
  Result := CArr[1] xor $1;
end;
{$endif}

procedure _NegI128(pA, pRes: PByte);
{$if defined(ASMx64)}
// RCX <- pA, RDX <- pRes
asm
  mov r8, [rcx]
  mov r9, [rcx + 8]
  not r8
  not r9
  add r8, 1
  adc r9, 0
  mov [rdx], r8
  mov [rdx + 8], r9
end;
{$else}
var C: UInt64;
    CArr: array [0..1] of Cardinal absolute C;
    cr: Cardinal;
begin
  cr := 1;
  CArr[1] := 0;
  CArr[0] := not PCardinal(pA)^;
  C := C + cr;
  PCardinal(pRes)^ := CArr[0];
  cr := CArr[1];
  Inc(pRes, 4);
  Inc(pA, 4);
  CArr[1] := 0;
  CArr[0] := not PCardinal(pA)^;
  C := C + cr;
  PCardinal(pRes)^ := CArr[0];
  cr := CArr[1];
  Inc(pRes, 4);
  Inc(pA, 4);
  CArr[1] := 0;
  CArr[0] := not PCardinal(pA)^;
  C := C + cr;
  PCardinal(pRes)^ := CArr[0];
  cr := CArr[1];
  Inc(pRes, 4);
  Inc(pA, 4);
  CArr[1] := 0;
  CArr[0] := not PCardinal(pA)^;
  C := C + cr;
  PCardinal(pRes)^ := CArr[0];
end;
{$endif}

function _IncUI128(pA: PByte; D: TLimb): TLimb;
{$if defined(ASMx64)}
// RCX <- pA, RDX <- D
asm
  xor rax, rax
  mov r8, [rcx]
  mov r9, [rcx + 8] // (r8, r9) <- (A0, A1)
  add r8, rdx
  adc r9, 0
  mov [rcx], r8
  mov [rcx + 8], r9
  adc rax, 0 // return the last carry
end;
{$else}
var tmp: UInt64;
    pair: array [0..1] of Cardinal absolute tmp;
    pAc: PCardinal;
begin
  pAc := PCardinal(pA);
  tmp := pAc^;
  Inc(tmp, D);
  pAc^ := pair[0];
  D := pair[1];
  Inc(pAc);
  tmp := pAc^;
  Inc(tmp, D);
  pAc^ := pair[0];
  D := pair[1];
  Inc(pAc);
  tmp := pAc^;
  Inc(tmp, D);
  pAc^ := pair[0];
  D := pair[1];
  Inc(pAc);
  tmp := pAc^;
  Inc(tmp, D);
  pAc^ := pair[0];
  Result := pair[1];
end;
{$endif}

{$if defined(ASMx64)}

procedure _MulUI128(pA, pB, pRes: PByte);
// RCX <- pA, RDX <- pB, R8 <- pRes
asm
  push rbx
  push rsi
  push rdi
  mov rsi, rdx // RSI <- pB
  mov rdi, r8  // RDI <- pRes
  xor r8, r8
  xor r9, r9
  xor r10, r10
  xor r11, r11

  mov rax, [rcx]
  mov rbx, [rsi]
  mul rbx
  mov r8, rax
  mov r9, rdx // (R8, R9) -> A0*B0

  mov rax, [rcx]
  mov rbx, [rsi + 8]
  mul rbx
  add r9, rax
  adc r10, rdx
  adc r11, 0  // (R9, R10) += A0*B0

  mov rax, [rcx + 8]
  mov rbx, [rsi]
  mul rbx
  add r9, rax
  adc r10, rdx
  adc r11, 0  // (R9, R10) += A1*B0

  mov rax, [rcx + 8]
  mov rbx, [rsi + 8]
  mul rbx
  add r10, rax
  adc r11, rdx // (R10, R11) += A1*B1

  mov [rdi], r8
  mov [rdi + 8], r9
  mov [rdi + 16], r10
  mov [rdi + 24], r11

  pop rdi
  pop rsi
  pop rbx
end;

{$endif}

function _Length128(pA: PByte): Integer; inline;
{$if SizeOf(TLimb) = 8}
begin
  if PUInt64(pA + 8)^ > 0 then exit(2);
  if PUInt64(pA)^ > 0 then exit(1);
  Result := 0;
end;
{$else}
begin
  if PUInt32(pA + 12)^ > 0 then exit(4);
  if PUInt32(pA + 8)^ > 0 then exit(3);
  if PUInt32(pA + 4)^ > 0 then exit(2);
  if PUInt32(pA)^ > 0 then exit(1);
  Result := 0;
end;
{$endif}

procedure _Mul(pA, pB, pRes: PByte; nA, nB: NativeInt);
{$if defined(ASMx86)}
asm
  push esi
  push edi
  //EAX <- pA, EDX <- pB, ECX <- pRes, [ESP + 12] <- nA, [ESP + 8] <- nB
  mov esi, eax //ESI <- pA
  mov edi, ecx //EDI <- pRes
  push ebx
  push 0 //k in [ESP + $C]
  push 0 //j in [ESP + $8] {j = B's index}
  push 0 //i in [ESP + $4] {i = A's index}
  push edx //[ESP] contains pB

@M3: //outer loop; initialize inner loop (i <- 0, k <- 0)
  mov dword ptr [esp + $4], 0
  mov dword ptr [esp + $C], 0
  mov ecx, [esp + $4]
@M4: //inner loop
  mov eax, [esi + 4 * ecx] //EAX <- u_i
  mov ebx, [esp]
  mov ebx, [ebx]
  mul ebx //(EAX:EDX) <- u_i x v_j
  add ecx, [esp + $8] //ECX <- i + j
  add eax, [edi + 4 * ecx] //EAX += w_{i + j}
  adc edx, 0 //(EAX:EDX) <- u_i x v_j + w_{i + j}
  add eax, [esp + $C]
  adc edx, 0 // t := (EAX:EDX) <- u_i x v_j + w_{i + j} + k
  mov [edi + 4 * ecx], eax // w_{i + j} <- t mod base
  mov [esp + $c], edx //k <- Floor(t / base)
//M5: Inc(i); if (i < nA) then @M4 else w_{j+m} <- k
  mov ecx, [esp + $4]
  inc ecx
  cmp ecx, nA
  je @E1
  mov [esp + $4], ecx //Inc(i)
  jmp @M4
@E1: //end of inner loop
  mov ecx, [esp + $8] //ECX <- j
  add ecx, nA
  mov [edi + 4 * ecx], edx // w_{j + m} <- k
//M6: Inc(j); if (j < nB) then @M3 else @E
  mov ecx, [esp + $8]
  inc ecx
  cmp ecx, nB
  je @E2
  mov [esp + $8], ecx
  add [esp], 4
  jmp @M3
@E2: //end of outer loop
  add esp, $10 //skip local vars
  pop ebx
  pop edi
  pop esi
end;
{$elseif defined(ASMx64)}
asm
  // pA -> RCX, pB -> RDX, pRes -> R8, nA -> R9
  push rdi
  mov r10, rcx // R10 <- pA
  mov r11, rdx // R11 <- pB
  mov rdi, r8 // RDI <- pRes
  push 0 // k in [RSP + 8]
  push 0 // j in [RSP] {j = B's index}
@M3: //outer loop; initialize inner loop (i <- 0, k <- 0)
  mov qword ptr [rsp + 8], 0 // k <- 0
  xor rcx, rcx // RCX <- i = 0 (A's index)
@M4: //inner loop
  mov rax, [r10 + 8 * rcx] // RAX <- u_i
  mul qword ptr [r11] // (RAX:RDX) <- u_i x v_j
  mov r8, rcx
  add r8, [rsp] // R8 <- i + j
  add rax, [rdi + 8 * r8] // RAX += w_{i + j}
  adc rdx, 0 // (RAX:RDX) <- u_i x v_j + w_{i + j}
  add rax, [rsp + 8]
  adc rdx, 0 // t := (RAX:RDX) <- u_i x v_j + w_{i + j} + k
  mov [rdi + 8 * r8], rax // w_{i + j} <- t mod base
  mov [rsp + 8], rdx // k <- Floor(t / base)
//M5: Inc(i); if (i < nA) then @M4 else w_{j+m} <- k
  inc rcx
  cmp rcx, nA
  je @E1
  jmp @M4
@E1: //end of inner loop
  mov rcx, [rsp] // RCX <- j
  mov rax, rcx
  add rax, nA
  mov [rdi + 8 * rax], rdx // w_{j + m} <- k
//M6: Inc(j); if (j < nB) then @M3 else @E
  inc rcx
  cmp rcx, nB
  je @E2
  mov [rsp], rcx
  add r11, 8
  jmp @M3
@E2: //end of outer loop
  add rsp, 16 //skip local vars (j, k)
  pop rdi
end;
{$else}
var I, J: Integer;
    K: Cardinal;
    tmp, V: UInt64;
    pair: array [0..1] of Cardinal absolute tmp;
    pW: PByte;
begin
  for J := 0 to nB - 1 do begin
    V := PCardinal(pB + J * cI32Sz)^;// V <- v_j
    if V = 0 then
      PCardinal(pRes + (J + nA) * cI32Sz)^ := 0
    else begin
      K := 0;
      for I := 0 to nA - 1 do begin
        pW := pRes + (I + J) * cI32Sz;
        tmp := PCardinal(pA + I * cI32Sz)^; // tmp <- u_i
        tmp := tmp * V;
        Inc(tmp, PCardinal(pW{w_i+j})^);
        Inc(tmp, K{carry});
        PCardinal(pW)^ := pair[0];
        K := pair[1];
      end;
      PCardinal(pRes + (J + nA) * cI32Sz)^ := K;
    end;
  end;
end;
{$endif}

function _ShortDivMod(pA, pB, pRes: PByte; nA: NativeInt): TLimb;
{$if defined(ASMx86)}
//EAX <- pA, EDX <- pB, ECX <- pRes, [ESP + $c] <- nA
asm
  push ebx
  push edi
  push esi
  mov ebx, [edx]      //EBX <- pB^
  mov edi, ecx        //EDI <- pRes
  mov ecx, nA         //ECX <- NA
  mov esi, eax        //ESI <- pA

  lea esi, [esi + 4 * ecx - 4] //ESI <- @u_{n - 1}
  lea edi, [edi + 4 * ecx - 4] //EDI <- @w_{n - 1}
  xor edx, edx //EDX <- r := 0
@L:
  mov eax, [esi] //(EAX:EDX) <- r * b + u_j
  div ebx //EAX <- (r*b + u_j) div v; EDX <- (r*b + u_j) mod v
  mov [edi], eax //store quotient
  sub esi, 4
  sub edi, 4
  dec ecx
  jnz @L

  mov eax, edx //returns remainder
  pop esi
  pop edi
  pop ebx
end;
{$elseif defined(ASMx64)}
asm
  // RCX <- pA, RDX <- pB, R8 <- pRes, R9 <- nA
  mov r10, rcx
  mov r11, [rdx] // r11 <- B

  lea r10, [r10 + 8 * r9 - 8] // r10 <- @A[n - 1]
  lea r8, [r8 + 8 * r9 - 8] // r8 <- @Res[n - 1]
  xor rdx, rdx // RDX <- r := 0
@L:
  mov rax, [r10] // (RAX:RDX) <- r * b + A[j]
  div r11 // RAX <- (r*b + A[j]) div B; RDX <- (r*b + A[j]) mod B
  mov [r8], rax // store quotient
  sub r10, 8
  sub r8, 8
  dec r9
  jnz @L

  mov rax, rdx
end;
{$else}
var tmp, q, r: UInt64;
    pEnd: PByte;
    v: Cardinal;
    pair: array [0..1] of Cardinal absolute tmp;
begin
  pEnd := pA;
  pA := pA + nA * cLimbSize - cI32Sz;
  pRes := pRes + nA * cLimbSize - cI32Sz;
  tmp := 0;
  v := PCardinal(pB)^;
  while pA >= pEnd do begin
    pair[0] := PCardinal(pA)^;
    DivMod(tmp, v, q, r);
    PCardinal(pRes)^ := Cardinal(q);
    pair[1] := r;
    Dec(pRes, 4);
    Dec(pA, 4);
  end;
  Result := r;
end;
{$endif}

procedure _LongDivMod(pA, pB, pQ: PByte; nA: NativeInt; nB: NativeInt);
{$if defined(ASMx86)}
asm
  push ebx
  push esi
  push edi
  mov esi, pA
  mov edi, pQ // (quotient result)
  push 1 // [ESP + $18] <- carry
  push 0 // [ESP + $14] <- i + j
  push 0 // [ESP + $10] <- i
  push 0 // [ESP + $c] <- r
  push 0 // [ESP + 8] <- q
  mov eax, nA
  sub eax, nB
  push eax // j in [ESP + 4] <- m = nA - nB
  push pB // [ESP] -> v_0
  shl eax, 2
  add edi, eax // [edi] -> @Q[m]

@D3: //q evaluation
  mov ecx, nB
  add ecx, [esp + 4] //ECX <- n + j
  mov edx, [esi + 4 * ecx] //EDX <- u_{n + j}
  mov eax, [esi + 4 * ecx - 4] //EAX <- u_{n+j-1} => (EDX:EAX)<- u_{n+j}b + u_{n+j-1}
  mov ecx, nB
  mov ebx, [esp]
  mov ebx, [ebx + 4 * ecx - 4] // EBX <- v_{n - 1}
  cmp edx, ebx
  jae @1D3
  div ebx
  mov [esp + $8], eax //store q
  mov [esp + $c], edx //store r
  jmp @2D3
@1D3:
  mov ecx, nB
  add ecx, [esp + 4] //ECX <- n + j
  mov edx, [esi + 4 * ecx - 4] //EDX <- u_{j + n - 1}
  mov eax, $ffffffff //(EDX:EAX) <- u_{j + n - 1} * b + (b - 1)
  jmp @4D3
@3D3:
  mov eax, [esp + $8] //EAX <- q
  dec eax //Dec(q)
  mov edx, [esp + $c] //EDX <- r
@4D3:
  mov [esp + $8], eax //store q
  mov ecx, nB
  mov ebx, [esp]
  add edx, [ebx + 4 * ecx - 4] //EDX <- r + v_{n - 1}
  jc @D4
  mov [esp + $c], edx //store r
  mov eax, [esp + $8] //EAX <- q
@2D3: //Test whether q*v_{n - 2} <= r*b + u _{j + n - 2}
  mov ecx, nB
  mov ebx, [esp]
  mul [ebx + 4 * ecx - 8] //(EDX:EAX) <- q * v_{n - 2}
  cmp edx, [esp + $c] //cmp(EDX, r)
  jb @D4
  ja @3D3
  mov ecx, nB
  cmp eax, [esi + 4 * ecx - 8] //cmp(EAX, u_{j + n - 2}
  ja @3D3
@D4: //Multiplication and subtraction
  mov [esp + $18], 0 //set carry
  mov [esp + $10], 0 //i <- 0
  mov ecx, [esp + $4]
  mov [esp + $14], ecx //(i + j) <- j
@2D4:
  mov ecx, [esp + $10] //ECX <- i
  mov ebx, [esp]
  mov eax, [ebx + 4 * ecx] // EAX <- v_i
  mul [esp + $8] //(EDX: EAX) <- q * v_i
  mov ecx, [esp + $14] //ECX <- i + j
  sub [esi + 4 * ecx], eax //u_{i + j} - q * v_i
  adc edx, 0
  mov ebx, [esp + $18] //load last carry
  sub [esi + 4 * ecx], ebx //subtract the last carry flag
  adc edx, 0
  mov [esp + $18], edx //store carry
  inc [esp + $10] //Inc(i)
  inc [esp + $14] //Inc(i + j)
  mov ecx, [esp + $10] //ECX <- i
  cmp ecx, nB
  jbe @2D4
@D5: //Remainder test
  mov eax, [esp + $8] //EAX <- q
  mov [edi], eax //q_j <- q
  cmp edx, 0
  je @D7
@D6:
  dec eax
  mov [edi], eax //q_j <- q - 1
  mov [esp + $10], 0 //i <- 0
  mov ecx, [esp + $4]
  mov [esp + $14], ecx //(i + j) <- j
@1D6: //addition
  mov ecx, [esp + $10]
  cmp ecx, nB
  jae @D7
  mov eax, 0
@2D6:
  mov ecx, [esp + $10]
  mov ebx, [esp]
  add eax, [ebx + 4 * ecx] //EAX += v_{i}
  mov ecx, [esp + $14] //ECX <- (i + j)
  add eax, [esi + 4 * ecx] //EAX += u_{i + j}
  mov [esi + 4 * ecx], eax
  inc [esp + $10] //Inc(i)
  inc [esp + $14] //Inc(i + j)
  jnc @1D6
  mov eax, 1
  mov ecx, [esp + $10]
  cmp ecx, nB
  jna @2D6 //if i <= nB then @2D6
@D7: //j-loop
  mov ecx, [esp + $4]
  dec ecx
  sub edi, 4
  mov [esp + $4], ecx
  cmp ecx, 0
  jge @D3
  add esp, $1c //skip local vars
  pop edi
  pop esi
  pop ebx
end;
{$elseif defined(ASMx64)}
asm
  push rbx
  push rsi
  push rdi
  push r12
  mov rsi, pA
  mov rdi, pQ // (quotient result)
  mov r10, 1 // R10 <- carry
  mov r11, 0 // R11 <- i + j
  mov r12, 0 // R12 <- i
  push 0 // [RSP + $18] <- r
  push 0 // [RSP + $10] <- q
  mov rax, nA
  sub rax, nB
  push rax // j in [RSP + 8] <- m = nA - nB
  push pB // [RSP] -> v_0
  shl rax, 3
  add rdi, rax // [RDI] -> @Q[m]

@D3: //q evaluation
  mov rcx, nB
  add rcx, [rsp + $8] //RCX <- n + j
  mov rdx, [rsi + 8 * rcx] //RDX <- u_{n + j}
  mov rax, [rsi + 8 * rcx - 8] //RAX <- u_{n+j-1} => (RDX:RAX)<- u_{n+j}b + u_{n+j-1}
  mov rcx, nB
  mov rbx, [rsp]
  mov rbx, [rbx + 8 * rcx - 8] // RBX <- v_{n - 1}
  cmp rdx, rbx
  jae @1D3
  div rbx
  mov [rsp + $10], rax //store q
  mov [rsp + $18], rdx //store r
  jmp @2D3
@1D3:
  mov rcx, nB
  add rcx, [rsp + $8] //RCX <- n + j
  mov rdx, [rsi + 8 * rcx - 8] //RDX <- u_{j + n - 1}
  mov rax, $ffffffffffffffff //(RDX:RAX) <- u_{j + n - 1} * b + (b - 1)
  jmp @4D3
@3D3:
  mov rax, [rsp + $10] //RAX <- q
  dec rax //Dec(q)
  mov rdx, [rsp + $18] //RDX <- r
@4D3:
  mov [rsp + $10], rax //store q
  mov rcx, nB
  mov rbx, [rsp]
  add rdx, [rbx + 8 * rcx - 8] //RDX <- r + v_{n - 1}
  jc @D4
  mov [rsp + $18], rdx //store r
  mov rax, [rsp + $10] //RAX <- q
@2D3: //Test whether q*v_{n - 2} <= r*b + u _{j + n - 2}
  mov rcx, nB
  mov rbx, [rsp]
  mul qword ptr [rbx + 8 * rcx - 16] //(RDX:RAX) <- q * v_{n - 2}
  cmp rdx, [rsp + $18] //cmp(RDX, r)
  jb @D4
  ja @3D3
  mov rcx, nB
  cmp rax, [rsi + 8 * rcx - 16] //cmp(RAX, u_{j + n - 2}
  ja @3D3
@D4: //Multiplication and subtraction
  mov r10, 0 //set carry
  mov r12, 0 //i <- 0
  mov rcx, [rsp + $8]
  mov r11, rcx //(i + j) <- j
@2D4:
  mov rcx, r12 //RCX <- i
  mov rbx, [rsp]
  mov rax, [rbx + 8 * rcx] // RAX <- v_i
  mul qword ptr [rsp + $10] //(RDX: RAX) <- q * v_i
  mov rcx, r11 //RCX <- i + j
  sub [rsi + 8 * rcx], rax //u_{i + j} - q * v_i
  adc rdx, 0
  mov rbx, r10 //load last carry
  sub [rsi + 8 * rcx], rbx //subtract the last carry flag
  adc rdx, 0
  mov r10, rdx //store carry
  inc r12 //Inc(i)
  inc r11 //Inc(i + j)
  mov rcx, r12 //RCX <- i
  cmp rcx, nB
  jbe @2D4
@D5: //Remainder test
  mov rax, [rsp + $10] //RAX <- q
  mov [rdi], rax //q_j <- q
  cmp rdx, 0
  je @D7
@D6:
  dec rax
  mov [rdi], rax //q_j <- q - 1
  mov r12, 0 //i <- 0
  mov rcx, [rsp + $8]
  mov r11, rcx //(i + j) <- j
@1D6: //addition
  mov rcx, r12
  cmp rcx, nB
  jae @D7
  mov rax, 0
@2D6:
  mov rcx, r12
  mov rbx, [rsp]
  add rax, [rbx + 8 * rcx] //RAX += v_{i}
  mov rcx, r11 //RCX <- (i + j)
  add rax, [rsi + 8 * rcx] //RAX += u_{i + j}
  mov [rsi + 8 * rcx], rax
  inc r12 //Inc(i)
  inc r11 //Inc(i + j)
  jnc @1D6
  mov rax, 1
  mov rcx, r12
  cmp rcx, nB
  jna @2D6 //if i <= nB then @2D6
@D7: //j-loop
  mov rcx, [rsp + $8]
  dec rcx
  sub rdi, 8
  mov [rsp + $8], rcx
  cmp rcx, 0
  jge @D3
  add rsp, $20 //skip local vars
  pop r12
  pop rdi
  pop rsi
  pop rbx
end;
{$else}
var I, J, m, carry: Integer;
    tmp, tmp2, q, r: UInt64;
    pair: array [0..1] of Cardinal absolute tmp;
    pU: PCardinal;
const wsz = SizeOf(Cardinal); //word size
      cBase: UInt64 = $100000000;
begin
  m := nA - nB;
  Inc(PCardinal(pQ), m);
  for J := m downto 0 do begin
    DivMod(PUInt64(pA + (J + nB - 1) * wsz)^, PCardinal(pB + (nB - 1) * wsz)^, q, r);
    while (q >= cBase) or (q * PCardinal(pB + (nB - 2) * wsz)^ > cBase * r + PCardinal(pA + (J + nB - 2) * wsz)^) do
    begin
      Inc(r, PCardinal(pB + (nB - 1) * wsz)^);
      Dec(q);
      if r >= cBase then break;
    end;
    // (u_{j+n},u_{j+n-1},...,u_j} - q*{0,v_{n-1},...,v_0}
    carry := 0;
    tmp := 0; tmp2 := 0;
    for I := 0 to nB - 1 do begin
      pU := PCardinal(pA + (J + I) * wsz);
      tmp := pU^;
      pair[1] := 1;
      tmp2 := q * PCardinal(pB + I * wsz)^ + PCardinal(PByte(@tmp2) + wsz)^; //q * v_i
      Dec(tmp, PCardinal(@tmp2)^);
      Dec(tmp, carry);
      carry := 1 xor pair[1];
      pU^ := pair[0]; //U_{j+i} - (q * v_i) mod cBase
    end;
    Inc(pU);
    pU^ := pU^ - PCardinal(PByte(@tmp2) + wsz)^ - Cardinal(carry);
    PCardinal(pQ)^ := Cardinal(q);
    if carry < 0 then begin
      Dec(PCardinal(pQ)^, 1);
      carry := 0;
      for I := 0 to nB - 1 do begin
        pU := PCardinal(pA + (J + I) * wsz);
        tmp := Cardinal(pU^) + PCardinal(pB + I * wsz)^;
      end;
    end;
    Dec(PCardinal(pQ));
  end;
end;
{$endif}

function _shl(pSrc, pDst: PByte; count: Byte; Nsrc: NativeInt): TLimb;
{$if defined(ASMx86)}
asm
  //EAX <- pSrc, EDX <- pDst, ECX <- count
  push ebx
  push edi
  push esi
  mov ebx, Nsrc
  dec ebx
  shl ebx, 2
  add eax, ebx
  add edx, ebx
  mov esi, eax //ESI points to the highest value of the source buffer
  mov edi, edx //EDI points to the highest value of the destination buffer
  xor eax, eax
  mov edx, eax

  mov eax, [esi]
  shld edx, eax, cl
  sub esi, 4
  push edx //push the carry from the heighest limb
  cmp ebx, 0
  jz @E
@1:
  mov edx, eax
  mov eax, [esi]
  shld edx, eax, cl
  mov [edi], edx
  sub esi, 4
  sub edi, 4
  sub ebx, 4
  jnz @1
@E:
  shl eax, cl
  mov [edi], eax
  pop eax
  pop esi
  pop edi
  pop ebx
end;
{$elseif defined(ASMx64)}
asm
  lea r10, [rcx + 8 * r9 - 8] // R10 points to the highest limb of the source buffer
  lea r11, [rdx + 8 * r9 - 8] // R11 points to the highes limb of the destionation buffer
  mov rcx, r8
  xor r8, r8
  mov rax, [r10]
  shld r8, rax, cl // R8 <- carry from the heighest limb
  sub r10, 8
  dec r9
  jz @E
@L:
  mov rdx, rax
  mov rax, [r10]
  shld rdx, rax, cl
  mov [r11], rdx
  sub r10, 8
  sub r11, 8
  dec r9
  jnz @L
@E:
  shl rax, cl
  mov [r11], rax
  mov rax, r8
end;
{$else}
var tmp: UInt64;
    pair: array [0..1] of Cardinal absolute tmp;
    pEnd: PByte;
    D: Cardinal;
begin
  pEnd := pSrc;
  pSrc := pSrc + (Nsrc - 1) * cI32Sz;
  pDst := pDst + (Nsrc - 1) * cI32Sz;
  pair[0] := PCardinal(pSrc)^;
  pair[1] := 0;
  tmp := tmp shl count;
  Result := pair[1];
  D := pair[0];
  Dec(pSrc, cI32Sz);
  while pSrc >= pEnd do begin
    pair[0] := PCardinal(pSrc)^;
    pair[1] := 0;
    tmp := tmp shl count;
    pair[1] := pair[1] or D;
    D := pair[0];
    PCardinal(pDst)^ := pair[1];
    Dec(pSrc, cI32Sz);
    Dec(pDSt, cI32Sz);
  end;
  PCardinal(pDst)^ := D;
end;
{$endif}

function _shr(pSrc, pDst: PByte; count: Byte; Nsrc: NativeInt): TLimb;
{$if defined(ASMx86)}
asm
  //EAX <- pSrc, EDX <- pDst, ECX <- count
  push ebx
  push esi
  push edi
  mov ebx, Nsrc
  mov esi, eax
  mov edi, edx
  xor edx, edx
  mov eax, [esi]
  shrd edx, eax, cl
  push edx //store carry from the lowest limb
  add esi, 4
  dec ebx
  jz @E
@1:
  mov edx, eax
  mov eax, [esi]
  shrd edx, eax, cl
  mov [edi], edx
  add esi, 4
  add edi, 4
  dec ebx
  jnz @1
@E:
  shr eax, cl
  mov [edi], eax
  pop eax
  pop edi
  pop esi
  pop ebx
end;
{$elseif defined(ASMx64)}
asm
  mov r10, rcx
  mov r11, rdx
  mov rcx, r8
  xor r8, r8
  mov rax, [r10]
  shrd r8, rax, cl // R8 <- carry from the lowest limb
  add r10, 8
  dec r9
  jz @E
@L:
  mov rdx, rax
  mov rax, [r10]
  shrd rdx, rax, cl
  mov [r11], rdx
  add r10, 8
  add r11, 8
  dec r9
  jnz @L
@E:
  shr rax, cl
  mov [r11], rax
  mov rax, r8 // RAX <- carry
end;
{$else}
var tmp: UInt64;
    pair: array [0..1] of Cardinal absolute tmp;
    pEnd: PByte;
    D: Cardinal;
begin
  pEnd := pSrc + Nsrc * cLimbSize;
  pair[1] := PCardinal(pSrc)^;
  pair[0] := 0;
  tmp := tmp shr count;
  Result := pair[0];
  D := pair[1];
  Inc(pSrc, cI32Sz);
  while pSrc < pEnd do begin
    pair[1] := PCardinal(pSrc)^;
    pair[0] := 0;
    tmp := tmp shr count;
    PCardinal(pDst)^ := pair[0] or D;
    D := pair[1];
    Inc(pSrc, cI32Sz);
    Inc(pDst, cI32Sz);
  end;
  PCardinal(pDst)^ := D;
end;
{$endif}

procedure _ipLongShift(pA: PLimb; aCount: Integer; aDigits: NativeInt);
var offset: NativeInt;
    p: PLimb;
    bRight: Boolean;
const wSz = SizeOf(TLimb);
begin
  if aDigits = 0 then exit;

  bRight := (aDigits < 0);
  aDigits := System.Abs(aDigits);
  offset := aDigits shr W_LOG2_BITS;
  if offset >= aCount then begin
    FillChar(pA^, aCount * cLimbSize, 0);
    exit;
  end;
  Dec(aCount, offset);
  aDigits := aDigits and cLimbRemMask;
  p := pA;

  if bRight then begin
    Inc(p, offset);
    if aDigits > 0 then
      _shr(PByte(p), PByte(pA), aDigits, aCount)
    else
      Move(p^, pA^, aCount * wSz);
    p := pA;
    Inc(p, aCount);
    FillChar(p^, offset * wSz, 0);
  end else begin
    Inc(pA, offset);
    if aDigits > 0 then
      _shl(PByte(p), PByte(pA), aDigits, aCount)
    else
      Move(p^, pA^, aCount * wSz);
    FillChar(p^, offset * wSz, 0);
  end;
end;

function _TopBitPos(A: TLimb): NativeInt;
{$if defined(ASMx86)}
asm
  cmp eax, 0
  jnz @Scan
  xor eax, eax
  ret
@Scan:
  bsr edx, eax
  inc edx
  mov eax, edx
end;
{$elseif defined(ASMx64)}
asm
  cmp rcx, 0
  jnz @Scan
  xor rax, rax
  ret
@Scan:
  bsr rax, rcx
  inc rax
end;
{$else}
begin
  Result := 0;
  if A = 0 then exit;
  while A and cTopBit = 0 do begin
    Inc(Result);
    A := A shl 1;
  end;
  Result := 32 - Result;
end;
{$endif}

function _TopBitPos(pA: PLimb; N: NativeInt): NativeInt;
{$if defined(ASMx86)}
asm
  lea ecx, [eax + 4 * edx - 4] // ESI points to the highest limb
  shl edx, 5 // EDX contains number of bits of the whole number
@Words:
  mov eax, [ecx]
  cmp eax, 0
  jnz @Bits
  sub ecx, 4
  sub edx, 32
  and edx, edx
  jz @E
  jmp @Words
@Bits:
  bsr ecx, eax
  inc ecx
  sub edx, 32
  add edx, ecx
@E:
  mov eax, edx
end;
{$elseif defined(ASMx64)}
asm
  // RCX <- pA, RDX <- N
  lea r10, [rcx + 8 * rdx - 8] // R10 points to the highest limb
  shl rdx, 6   // RDX contains bit count of the whole number
@Limbs:
  mov rax, [r10]
  cmp rax, 0
  jnz @Bits
  sub r10, 8
  sub rdx, 64
  and rdx, rdx
  jz @E
  jmp @Limbs
@Bits:
  bsr rcx, rax
  inc rcx
  sub rdx, 64
  add rdx, rcx
@E:
  mov rax, rdx
end;
{$else}
var pEnd: PByte;
    pos: Integer;
begin
  pEnd := PByte(pA);
  Inc(pA, N - 1);
  Result := N shl 5;
  while PByte(pA) >= pEnd do begin
    pos := _TopBitPos(PLimb(pA)^);
    Dec(Result, 32);
    if pos > 0 then exit(Result + pos);
    Dec(pA);
  end;
end;
{$endif}

function _GetLimb(pA: PByte; I: NativeInt): TLimb; inline;
begin
  Result := PLimb(pA + I * cLimbSize)^;
end;

function _CountLeadingZeros(A: TLimb): NativeInt;
{$if defined(ASMx86)}
asm
  bsr edx, eax
  jnz @compl
  mov eax, 32
  ret
@compl:
  mov eax, 31
  sub eax, edx
end;
{$elseif defined(ASMx64)}
asm
  bsr rdx, rcx
  jnz @compl
  mov rax, 64
  ret
@compl:
  mov rax, 63
  sub rax, rdx
end;
{$else}
begin
  Result := cLimbBits - _TopBitPos(A);
end;
{$endif}

function _CountLeadingZeros(pA: PLimb; N: NativeInt): NativeInt;
var pEnd: PByte;
    v: TLimb;
begin
  Result := 0;
  pEnd := PByte(pA);
  pA := PLimb(PByte(pA) + (N - 1) * cLimbSize);
  while PByte(pA) >= pEnd do begin
    v := pA^;
    if v <> 0 then begin
      Inc(Result, _CountLeadingZeros(v));
      exit;
    end;
    Inc(Result, cLimbBits);
    Dec(pA);
  end;
end;

function _CountTrailingZeros(A: TLimb): NativeInt;
{$if defined(ASMx86)}
asm
  bsf eax, eax
  jnz @end
  mov eax, 32
@end:
end;
{$elseif defined(ASMx64)}
asm
  bsf rax, rcx
  jnz @end
  mov rax, 64
@end:
end;
{$else}
 begin
   if A = 0 then exit(32);
   Result := 0;
   if (A and $FFFF) = 0 then begin
     Inc(Result, 16);
     A := (A shr 16) or $10000;
   end;

   while (A and 1) = 0 do begin
     A := A shr 1;
     Inc(Result);
   end;
 end;
{$endif}

function _IsZero(pA: PLimb; N: NativeInt): Boolean;
var pEnd: PByte;
begin
  pEnd := PByte(pA) + N * cLimbSize;
  while PByte(pA) < pEnd do begin
    if pA^ <> 0 then exit(False);
    Inc(pA);
  end;
  Result := True;
end;

function _Length(pA: PByte; N: NativeInt): NativeInt;
var p: PByte;
begin
  p := pA + (N - 1) * SizeOf(TLimb);
  while p >= pA do begin
    if p^ <> 0 then break;
    Dec(p, SizeOf(TLimb));
  end;
  Result := (p - pA) div SizeOf(TLimb) + 1;
end;

{$endregion}

{$region 'TInt128'}

procedure TInt128.Init(const aLo, aHi: UInt64);
begin
  fLimbs[0] := aLo;
  fLimbs[1] := aHi;
end;

function TInt128.Lo: UInt64;
begin
  Result := fLimbs[0];
end;

function TInt128.Hi: UInt64;
begin
  Result := fLimbs[1];
end;

function TInt128.Hi32: Cardinal;
begin
  Result := PCardinal(PByte(@Self) + 12)^
end;

class operator TInt128.Implicit(const aValue: Int64): TInt128;
begin
  Int64(Result.fLimbs[0]) := aValue;
  if aValue < 0 then
    Result.fLimbs[1] := High(UInt64)
  else
    Result.fLimbs[1] := 0;
end;

class operator TInt128.Implicit(const aValue: UInt64): TInt128;
begin
  Result.fLimbs[0] := aValue;
  Result.fLimbs[1] := 0;
end;

class operator TInt128.Explicit(const aValue: TInt128): Int64;
begin
  Result := PInt64(@aValue)^;
end;

class operator TInt128.Add(const A, B: TInt128): TInt128;
begin
  _AddI128(@A, @B, @Result);
{$ifopt Q+}
  if
    ((A.Hi32 xor Result.Hi32) and (B.Hi32 xor Result.Hi32)) and cBit32 > 0
  then
    RaiseInt128Overflow;
{$endif}
end;

class operator TInt128.Subtract(const A, B: TInt128): TInt128;
begin
  _SubI128(@A, @B, @Result);
{$ifopt Q+}
  if
    ((A.Hi32 xor Result.Hi32) and (not (B.Hi32 xor Result.Hi32))) and cBit32 > 0
  then
    RaiseInt128Overflow;
{$endif}
end;

class operator TInt128.Negative(const A: TInt128): TInt128;
begin
  _NegI128(@A, @Result);
{$ifopt Q+}
  if (A.Hi32 and Result.Hi32 and cBit32) > 0 then RaiseInt128Overflow;
{$endif}
end;

class operator TInt128.Multiply(const A, B: TInt128): TInt128;
var NA, NB: TInt128;
    IsANeg, IsBNeg: Boolean;
    pA, pB, pRes: Pointer;
    buff: array [0..3] of UInt64;
begin
  pA := @A;
  pB := @B;
  IsANeg := ((A.fLimbs[1] and cBit64) <> 0);
  IsBNeg := ((B.fLimbs[1] and cBit64) <> 0);
  if IsANeg then begin
    NA := -A;
    pA := @NA;
  end;
  if IsBNeg then begin
    NB := -B;
    pB := @NB;
  end;
  buff[0] := 0;
  buff[1] := 0;
  buff[2] := 0;
  buff[3] := 0;
  pRes := @buff;
{$if defined(ASMx64)}
  _MulUI128(pA, pB, pRes);
{$else}
  _Mul(pA, pB, pRes, LCnt, LCnt);
{$endif}
{$ifopt Q+}
  if (buff[2] or buff[3]) <> 0 then
    RaiseInt128Overflow;
{$endif}
  Result.fLimbs[0] := buff[0];
  Result.fLimbs[1] := buff[1];
  if IsBNeg <> IsANeg then
    Result := -Result;
end;

{$ifdef RANGEON}
   {$R-}
{$endif}
class procedure TInt128.LongDivMod(var A, B: TInt128);
var nA, nB, pwr: Integer;
    Adata, Bdata: array [0..2] of UInt64;
    AArr: array [0..0] of TLimb absolute Adata;
    BArr: array [0..0] of TLimb absolute Bdata;
    Q: array [0..1] of UInt64;
begin
  AData[0] := 0; AData[1] := 0;
  BData[0] := 0; Bdata[1] := 0;
  Q[0] := 0; Q[1] := 0;
  nA := _Length128(@A);
  nB := _Length128(@B);
  pwr := W_BIT_COUNT - _TopBitPos(_GetLimb(@B, nB - 1));
  //normalize A
  AArr[nA] := _shl(@A, @AData, pwr, nA);
  //normalize B
  BArr[nB] := _shl(@B, @BData, pwr, nB);
  //execute DivMod
  _LongDivMod(@AData, @BData, @Q, nA, nB);
  //denormalize B
  B.fLimbs[0] := AData[0];
  B.fLimbs[1] := AData[1];
  B := B shr pwr;
  A.fLimbs[0] := Q[0];
  A.fLimbs[1] := Q[1];
end;
{$ifdef RANGEON}
  {$R+}
{$endif}

class procedure TInt128.ShortDivMod(var A, B: TInt128);
var r: TLimb;
    nA: Integer;
    data: array [0..1] of UInt64;
begin
  data[0] := 0; data[1] := 0;
  nA := _Length128(@A);
  r := _ShortDivMod(@A, @B, @data, nA);
  A.fLimbs[0] := 0; A.fLimbs[1] := 0;
  Move(data, A, nA * cLimbSize);
  B.fLimbs[0] := 0; B.fLimbs[1] := 0;
  PLimb(@B)^ := r;
end;


class operator TInt128.IntDivide(const A, B: TInt128): TInt128;
var q, r: TInt128;
    IsANeg, IsBNeg: Boolean;
begin
  isANeg := (A.Hi and cBit64 <> 0);
  isBNeg := (B.Hi and cBit64 <> 0);
  if IsANeg then q := -A
  else q := A;
  if IsBNeg then r := -B
  else r := B;
  if _Length128(@r) = 1 then
    ShortDivMod(q, r)
  else
    LongDivMod(q, r);
  Result := q;
  if IsBNeg <> IsANeg then
    Result := -Result;
  Result := q;
end;

class operator TInt128.Modulus(const A, B: TInt128): TInt128;
var q, r: TInt128;
    isANeg, isBNeg: Boolean;
begin
  isANeg := (A.Hi and cTopBit <> 0);
  isBNeg := (B.Hi and cTopBit <> 0);
  if isANeg then q := -A
  else q := A;
  if isBNeg then r := -B
  else r := B;
  if _Length128(@r) = 1 then
    ShortDivMod(q, r)
  else
    LongDivMod(q, r);
  Result := r;
end;

class operator TInt128.RightShift(const A: TInt128; const B: Cardinal): TInt128;
var offset, r: Integer;
    p: PByte;
begin
  Result.Init(0, 0);
  offset := B shr W_LOG2_BITS;
  if offset >= LCnt then exit;

  r := B and cLimbRemMask;
  p := PByte(@A) + offset * cLimbSize;
  _shr(p, @Result, r, LCnt - offset);
end;

class operator TInt128.LeftShift(const A: TInt128; const B: Cardinal): TInt128;
var offset, r: Integer;
    p: PByte;
begin
  Result.Init(0, 0);
  offset := B shr W_LOG2_BITS;
  if offset >= LCnt then exit;

  r := B and cLimbRemMask;
  p := PByte(@Result) + offset * cLimbSize;
  _shl(@A, p, r, LCnt - offset);
end;

class function TInt128.Compare(const A, B: TInt128): Integer;
var C: TInt128;
begin
  C := A - B;
  if (C.Hi and cTopBit) <> 0 then exit(-1);
  if C.ZeroQ() then exit(0);
  Result := 1;
end;

class operator TInt128.LessThan(const A, B: TInt128): Boolean;
begin
  Result := (Compare(A, B) < 0);
end;

class operator TInt128.GreaterThan(const A, B: TInt128): Boolean;
begin
  Result := (Compare(A, B) > 0);
end;

class operator TInt128.LessThanOrEqual(const A, B: TInt128): Boolean;
begin
  Result := (Compare(A, B) <= 0);
end;

class operator TInt128.GreaterThanOrEqual(const A, B: TInt128): Boolean;
begin
  Result := (Compare(A, B) >= 0);
end;

class operator TInt128.Equal(const A, B: TInt128): Boolean;
begin
  Result := (Compare(A, B) = 0);
end;

class operator TInt128.NotEqual(const A, B: TInt128): Boolean;
begin
  Result := (Compare(A, B) <> 0);
end;

function TInt128.ZeroQ: Boolean;
begin
  Result := (fLimbs[0] = 0) and (fLimbs[1] = 0);
end;

{$endregion}

{$region 'TUInt128'}

procedure TUInt128.Init(const aLo, aHi: UInt64);
begin
  fLimbs[0] := aLo;
  fLimbs[1] := aHi;
end;

function TUInt128.Lo: UInt64;
begin
  Result := fLimbs[0];
end;

function TUInt128.Hi: UInt64;
begin
  Result := fLimbs[1];
end;

function TUInt128.Hi32: Cardinal;
begin
  Result := PCardinal(PByte(@Self) + 12)^
end;

class operator TUInt128.Implicit(const aValue: UInt64): TUInt128;
begin
  Result.fLimbs[0] := aValue;
  Result.fLimbs[1] := 0;
end;

class operator TUInt128.Explicit(const aValue: TUInt128): UInt64;
begin
  Result := PUInt64(@aValue)^;
end;

class operator TUInt128.Add(const A, B: TUInt128): TUInt128;
var cr: Integer;
begin
  cr := _AddI128(@A, @B, @Result);
{$ifopt Q+}
  if cr > 0 then RaiseInt128Overflow;
{$endif}
end;

class operator TUInt128.Subtract(const A, B: TUInt128): TUInt128;
var cr: Integer;
begin
  cr := _SubI128(@A, @B, @Result);
{$ifopt Q+}
  if cr > 0 then RaiseInt128Overflow;
{$endif}
end;

class operator TUInt128.Inc(const A: TUInt128): TUInt128;
var cr: Integer;
begin
  Result := A;
  cr := _IncUI128(@Result, 1);
{$ifopt Q+}
  if cr > 0 then RaiseInt128Overflow;
{$endif}
end;

class operator TUInt128.Negative(const A: TUInt128): TUInt128;
begin
  _NegI128(@A, @Result);
{$ifopt Q+}
  if (A.Hi32 and Result.Hi32 and cBit32) > 0 then RaiseInt128Overflow;
{$endif}
end;

class operator TUInt128.Multiply(const A, B: TUInt128): TUInt128;
var buff: array [0..3] of UInt64;
begin
  buff[0] := 0;
  buff[1] := 0;
  buff[2] := 0;
  buff[3] := 0;
{$if defined(ASMx64)}
  _MulUI128(@A, @B, @buff);
{$else}
  _Mul(@A, @B, @buff, LCnt, LCnt);
{$endif}
{$ifopt Q+}
  if (buff[2] or buff[3]) <> 0 then
    RaiseInt128Overflow;
{$endif}
  Result.fLimbs[0] := buff[0];
  Result.fLimbs[1] := buff[1];
end;

{$ifdef RANGEON}
   {$R-}
{$endif}

class procedure TUInt128.LongDivMod(var A, B: TUInt128);
var nA, nB, pwr: Integer;
    Adata, Bdata: array [0..2] of UInt64;
    AArr: array [0..0] of TLimb absolute Adata;
    BArr: array [0..0] of TLimb absolute Bdata;
    Q: array [0..1] of UInt64;
begin
  AData[0] := 0; AData[1] := 0;
  BData[0] := 0; Bdata[1] := 0;
  Q[0] := 0; Q[1] := 0;
  nA := _Length128(@A);
  nB := _Length128(@B);
  pwr := W_BIT_COUNT - _TopBitPos(_GetLimb(@B, nB - 1));
  //normalize A
  AArr[nA] := _shl(@A, @AData, pwr, nA);
  //normalize B
  BArr[nB] := _shl(@B, @BData, pwr, nB);
  //execute DivMod
  _LongDivMod(@AData, @BData, @Q, nA, nB);
  //denormalize B
  B.fLimbs[0] := AData[0];
  B.fLimbs[1] := AData[1];
  B := B shr pwr;
  A.fLimbs[0] := Q[0];
  A.fLimbs[1] := Q[1];
end;
{$ifdef RANGEON}
  {$R+}
{$endif}

class procedure TUInt128.ShortDivMod(var A, B: TUInt128);
var r: TLimb;
    nA: Integer;
    data: array [0..1] of UInt64;
begin
  data[0] := 0; data[1] := 0;
  nA := _Length128(@A);
  r := _ShortDivMod(@A, @B, @data, nA);
  A.fLimbs[0] := 0; A.fLimbs[1] := 0;
  Move(data, A, nA * cLimbSize);
  B.fLimbs[0] := 0; B.fLimbs[1] := 0;
  PLimb(@B)^ := r;
end;


class operator TUInt128.IntDivide(const A, B: TUInt128): TUInt128;
var r: TUInt128;
begin
  Result := A;
  r := B;
  if _Length128(@r) = 1 then
    ShortDivMod(Result, r)
  else
    LongDivMod(Result, r);
end;

class operator TUInt128.Modulus(const A, B: TUInt128): TUInt128;
var q: TUInt128;
begin
  q := A;
  Result := B;
  if _Length128(@B) = 1 then
    ShortDivMod(q, Result)
  else
    LongDivMod(q, Result);
end;

class operator TUInt128.RightShift(const A: TUInt128; const B: Cardinal): TUInt128;
var offset, r: Integer;
    p: PByte;
begin
  Result.Init(0, 0);
  offset := B shr W_LOG2_BITS;
  if offset >= 2 then exit;

  r := B and cLimbRemMask;
  p := PByte(@A) + offset * cLimbSize;
  _shr(p, @Result, r, 2 - offset);
end;

class operator TUInt128.LeftShift(const A: TUInt128; const B: Cardinal): TUInt128;
var offset, r: Integer;
    p: PByte;
begin
  Result.Init(0, 0);
  offset := B shr W_LOG2_BITS;
  if offset >= 2 then exit;

  r := B and cLimbRemMask;
  p := PByte(@Result) + offset * cLimbSize;
  _shl(@A, p, r, 2 - offset);
end;

class function TUInt128.Compare(const A, B: TUInt128): Integer;
begin
  if A.fLimbs[1] > B.fLimbs[1] then exit(1);
  if A.fLimbs[1] < B.fLimbs[1] then exit(-1);
  if A.fLimbs[0] > B.fLimbs[0] then exit(1);
  if A.fLimbs[0] < B.fLimbs[0] then exit(-1);
  Result := 0;
end;

class operator TUInt128.LessThan(const A, B: TUInt128): Boolean;
begin
  Result := (Compare(A, B) < 0);
end;

class operator TUInt128.GreaterThan(const A, B: TUInt128): Boolean;
begin
  Result := (Compare(A, B) > 0);
end;

class operator TUInt128.LessThanOrEqual(const A, B: TUInt128): Boolean;
begin
  Result := (Compare(A, B) <= 0);
end;

class operator TUInt128.GreaterThanOrEqual(const A, B: TUInt128): Boolean;
begin
  Result := (Compare(A, B) >= 0);
end;

class operator TUInt128.Equal(const A, B: TUInt128): Boolean;
begin
  Result := (Compare(A, B) = 0);
end;

class operator TUInt128.NotEqual(const A, B: TUInt128): Boolean;
begin
  Result := (Compare(A, B) <> 0);
end;

class operator TUInt128.BitwiseAnd(const A, B: TUInt128): TUInt128;
begin
  Result.fLimbs[0] := A.fLimbs[0] and B.fLimbs[0];
  Result.fLimbs[1] := A.fLimbs[1] and B.fLimbs[1];
end;

class operator TUInt128.BitwiseOr(const A, B: TUInt128): TUInt128;
begin
  Result.fLimbs[0] := A.fLimbs[0] or B.fLimbs[0];
  Result.fLimbs[1] := A.fLimbs[1] or B.fLimbs[1];
end;

class operator TUInt128.BitwiseXor(const A, B: TUInt128): TUInt128;
begin
  Result.fLimbs[0] := A.fLimbs[0] xor B.fLimbs[0];
  Result.fLimbs[1] := A.fLimbs[1] xor B.fLimbs[1];
end;

class operator TUInt128.LogicalNot(const A: TUInt128): TUInt128;
begin
  Result.fLimbs[0] := not A.fLimbs[0];
  Result.fLimbs[1] := not A.fLimbs[1];
end;

function TUInt128.ZeroQ: Boolean;
begin
  Result := (fLimbs[0] = 0) and (fLimbs[1] = 0);
end;

{$endregion}

{$region 'TReal128'}

function Same(const A, B: TReal128): Boolean; inline;
begin
  Result := (A.fLimbs[0] = B.fLimbs[0]) and (A.fLimbs[1] = B.fLimbs[1]);
end;

procedure TReal128.Init(const aValue: Double);
var iVal: UInt64 absolute aValue;
    e: Int64;
begin
  e := (iVal shr 52) and $7FF;
  if e <> 0 then begin
    //normalized numbers
    fLimbs[0] := iVal shl 60;
    fLimbs[1] :=
      ((iVal and FRAC_MASK_F64) shr 4) or // fraction
      UInt64(((e - $3FF) + EXP_BIAS) shl 48) or // exponent
      (iVal and cBit64); // signum
  end else begin
    //subnormal numbers
    if (iVal and (not cBit64)) = 0 then begin
      fLimbs[0] := 0;
      fLimbs[1] := 0;
    end else begin
      fLimbs[0] := 0;
      fLimbs[1] := iVal and FRAC_MASK_F64;
      e := _CountLeadingZeros(@fLimbs, cLCnt);
      _ipLongShift(@fLimbs, cLCnt, e - FRAC_OFFSET + 1);
      fLimbs[1] := fLimbs[1] and HI_FRAC_MASK;
      e := EXP_BIAS - EXP_BIAS_F64 + FRAC_OFFSET_F64 - e;
      fLimbs[1] := fLimbs[1] or (UInt64(e) shl 48);
    end;
    fLimbs[1] := fLimbs[1] or (iVal and SGN_MASK);
  end;
end;

procedure TReal128.Init(const aLo, aHi: UInt64);
begin
  fLimbs[0] := aLo;
  fLimbs[1] := aHi;
end;

class operator TReal128.Implicit(const A: Double): TReal128;
begin
  Result.Init(A);
end;

class operator TReal128.Explicit(const A: TReal128): Double;
begin
  Result := A.AsDouble;
end;

class function TReal128.Round(const aBuff): Integer;
var buff: array [0..3] of UInt64 absolute aBuff;
const REST_MASK = not cBit64;
begin
  Result := 0;
  if (buff[1] and cBit64) = 0 then exit; // remainder < 1/2*ULP -> round down
  if (buff[0] or (buff[1] and REST_MASK)) <> 0 then begin // remainder > 1/2*ULP -> round up
    _IncUI128(@buff[2], 1);
    if (buff[3] and OF_QUARD) > 0 then begin
      Inc(Result);
      _shr(@buff[2], @buff[2], Result, cLCnt);
    end;
  end else  // remainder = 1/2*ULP -> tie to even
  if (buff[2] and 1) > 0 then begin // round up
    _IncUI128(@buff[2], 1);
    if (buff[3] and OF_QUARD) > 0 then begin
      Inc(Result);
      _shr(@buff[2], @buff[2], Result, cLCnt);
    end;
  end; // else round down
end;

class function TReal128.AddPositive(const A, B: TReal128): TReal128;
var ea, eb, de, i: Integer;
    ra, rb: PReal128;
    buff: array [0..7] of UInt64;
begin
  ea := A.Exponent;
  eb := B.Exponent;
  if ea < eb then begin
    i := ea; ea := eb; eb := i;
    ra := @B;
    rb := @A;
  end else begin
    ra := @A;
    rb := @B;
  end;
  if ra^.IsExactZero then begin
    Result := rb^;
    exit;
  end;

  de := (ea - eb);
  if de > 113 then exit(ra^);

  buff[0] := 0;
  buff[1] := 0;
  buff[2] := ra^.fLimbs[0];
  buff[3] := (ra^.fLimbs[1] and HI_FRAC_MASK) or LEADING_ONE;
  buff[4] := 0;
  buff[5] := 0;
  buff[6] := rb^.fLimbs[0];
  buff[7] := (rb^.fLimbs[1] and HI_FRAC_MASK) or LEADING_ONE;

  _ipLongShift(@buff[4], 2 * cLCnt, -de);
  _ipAdd(@buff[4], @buff[0], 2 * cLCnt);
  if (buff[3] and OF_QUARD) > 0 then begin
    _shr(@buff[2], @buff[2], 1, cLCnt);
    Inc(ea);
  end;
  ea := ea + Round(buff[0]);
  Result.fLimbs[0] := buff[2];
  Result.fLimbs[1] := (buff[3] and HI_FRAC_MASK);
  Result.Exponent := ea;
end;

class function TReal128.SubtractPositive(const A, B: TReal128): TReal128;
var ea, eb, de, sgn: Integer;
    ra, rb: PReal128;
    buff: array [0..7] of UInt64;
begin
  de := AbsCompare(A, B);
  if de < 0 then begin
    ra := @B;
    rb := @A;
    sgn := -1;
  end else
  if de > 0 then begin
    ra := @A;
    rb := @B;
    sgn := 1;
  end else
    exit(cZeroF128);

  if rb^.IsExactZero then begin
    Result := ra^;
    Result.Signum := sgn;
    exit;
  end;

  ea := ra^.Exponent;
  eb := rb^.Exponent;
  de := ea - eb;
  if de > 113 then exit(ra^);

  buff[0] := 0;
  buff[1] := 0;
  buff[2] := rb^.fLimbs[0];
  buff[3] := (rb^.fLimbs[1] and HI_FRAC_MASK) or LEADING_ONE;
  buff[4] := 0;
  buff[5] := 0;
  buff[6] := ra^.fLimbs[0];
  buff[7] := (ra^.fLimbs[1] and HI_FRAC_MASK) or LEADING_ONE;

  _ipLongShift(@buff, 2 * cLCnt, -de);
  _ipSub(@buff[4], @buff, 2 * cLCnt);
  de := _CountLeadingZeros(@buff, 2 * cLCnt) - FRAC_OFFSET + 1;
  if de > 0 then begin
    _ipLongShift(@buff, 2 * cLCnt, de);
    Dec(ea, de);
  end;
  ea := ea + Round(buff[0]);
  Result.fLimbs[0] := buff[2];
  Result.fLimbs[1] := (buff[3] and HI_FRAC_MASK);
  Result.Exponent := ea;
  Result.Signum := sgn;
end;

class operator TReal128.Add(const A, B: TReal128): TReal128;
var sa, sb: Integer;
begin
  sa := A.Signum;
  sb := B.Signum;
  if sa * sb > 0 then begin
    Result := AddPositive(A, B);
    Result.Signum := sa;
  end else begin
    Result := SubtractPositive(A, B);
    Result.Signum := Result.Signum * sa;
  end;
end;

class operator TReal128.Subtract(const A, B: TReal128): TReal128;
begin
  Result := A + (-B);
end;

class operator TReal128.Negative(const A: TReal128): TReal128;
begin
  Result.fLimbs[0] := A.fLimbs[0];
  Result.fLimbs[1] := ((not A.fLimbs[1]) and SGN_MASK) or (A.fLimbs[1] and (not SGN_MASK));
end;

class operator TReal128.Multiply(const A, B: TReal128): TReal128;
var buff: array [0.. 7] of UInt64;
    ec, de: Integer;
begin
  if A.IsExactZero or B.IsExactZero then exit(cZeroF128);

  ec := A.Exponent + B.Exponent;

  buff[0] := A.fLimbs[0];
  buff[1] := (A.fLimbs[1] and HI_FRAC_MASK) or LEADING_ONE;
  buff[2] := B.fLimbs[0];
  buff[3] := (B.fLimbs[1] and HI_FRAC_MASK) or LEADING_ONE;
  buff[4] := 0;
  buff[5] := 0;
  buff[6] := 0;
  buff[7] := 0;

{$if defined(ASMx64)}
  _MulUI128(@buff[0], @buff[2], @buff[4]);
{$else}
  _Mul(@buff[0], @buff[2], @buff[4], cLCnt, cLCnt);
{$endif}
  de := _CountLeadingZeros(@buff[4], 2 * cLCnt);
  _ipLongShift(@buff[4], 2 * cLCnt, de - FRAC_OFFSET + 1);
  Inc(ec, (2 * FRAC_OFFSET - 1) - de);
  ec := ec + Round(buff[4]);
  Result.fLimbs[0] := buff[6];
  Result.fLimbs[1] := buff[7] and HI_FRAC_MASK;
  Result.Exponent := ec;
  Result.Signum := A.Signum * B.Signum;
end;

class operator TReal128.Divide(const A, B: TReal128): TReal128;
begin
  Result := A * B.InternalReciprocal;
  AdjustByReminder(A, B, Result);
end;

class procedure TReal128.AdjustByReminder(const A, B: TReal128; var Q: TReal128);
var buff: array [0..7] of UInt64;
    ulp: TReal128;
    de: Integer;
    bNeg: Boolean;
begin
  buff[0] := B.fLimbs[0];
  buff[1] := B.fLimbs[1] and HI_FRAC_MASK or LEADING_ONE;
  buff[2] := Q.fLimbs[0];
  buff[3] := Q.fLimbs[1] and HI_FRAC_MASK or LEADING_ONE;
  buff[4] := 0;
  buff[5] := 0;
  buff[6] := 0;
  buff[7] := 0;
{$if defined(ASMx64)}
  _MulUI128(@buff[0], @buff[2], @buff[4]);
{$else}
  _Mul(@buff[0], @buff[2], @buff[4], cLCnt, cLCnt);
{$endif}
  de := _CountLeadingZeros(@buff[4], 2 * cLCnt);
  _ipLongShift(@buff[4], 2 * cLCnt, de - FRAC_OFFSET + 1);
  buff[0] := 0;
  buff[1] := 0;
  buff[2] := A.fLimbs[0];
  buff[3] := A.fLimbs[1] and HI_FRAC_MASK or LEADING_ONE;
  _ipSub(@buff[0], @buff[4], 2 * cLCnt);  // buff[4..7] <- a - b * q

  bNeg := (buff[7] and cBit64) > 0;
  if bNeg then
    _NegMPI(@buff[4], @buff[4], 2 * cLCnt);

  if (buff[5] and cBit64) > 0 then begin
    ulp.Init(0, 0);
    ulp.Exponent := -FRAC_BIT_CNT + Q.Exponent;
    if bNeg then ulp.Signum := -1;
    Q := Q + ulp;
  end;
end;

function TReal128.InternalReciprocal: TReal128;
var d, x, u: TReal128;
    iter, e: Integer;
const
  t1: TReal128 = (fLimbs: ($6969696969696969, $4000696969696969)); //  48/17
  t2: TReal128 = (fLimbs: ($E1E1E1E1E1E1E1E2, $BFFFE1E1E1E1E1E1)); // -32/17
begin
  u := cOneF128;
  e := Exponent;
  d.fLimbs[0] := fLimbs[0];
  d.fLimbs[1] := fLimbs[1] and HI_FRAC_MASK;
  d.Exponent := -1;
  x := t1 + t2 * d;
  iter := 5;
  while iter > 0 do begin
    x := x + x * (u - d * x);
    Dec(iter);
  end;
  Result.fLimbs[0] := x.fLimbs[0];
  Result.fLimbs[1] := x.fLimbs[1] and HI_FRAC_MASK;
  Result.Exponent := x.Exponent - e - 1;
  Result.Signum := Signum;
end;

function TReal128.Reciprocal: TReal128;
begin
  Result := InternalReciprocal();
  AdjustByReminder(cOneF128, Self, Result);
end;

function TReal128.Sqrt: TReal128;
var e: Integer;
    m, y: TReal128;
const cHalf: TReal128 = (fLimbs: (0, $3FFE000000000000));
begin
  if (Self.Signum < 0) or (Self = cNaNF128) then exit(cNaNF128);
  if Same(Self, cZeroF128) then exit(cZeroF128);

  e := Exponent;
  m := Self;
  if (e and 1) = 0 then
    m.Exponent := 0
  else begin
    m.Exponent := 1;
    Dec(e);
  end;

  y := System.Sqrt(m.AsDouble);
  y := cHalf * (y + m / y);
  y := cHalf * (y + m / y);
  Result := y;
  Result.Exponent := y.Exponent + (e div 2);
//  AdjustByReminder(Self, Result, Result);
end;

function TReal128.AsDouble: Double;
var iVal: UInt64 absolute Result;
    buff: array [0..3] of UInt64;
    e: Integer;
const
   OF_GUARD  = UInt64(1) shl 53;
begin
  e := Exponent;

  if e < -1022 then begin // subnormal numbers
    if e < -1074 then exit(0);
    Inc(e, 1023);
    buff[0] := 0;
    buff[1] := 0;
    buff[2] := fLimbs[0];
    buff[3] := fLimbs[1] and HI_FRAC_MASK or LEADING_ONE;
    _ipLongShift(@buff, 2 * cLCnt, e - 61);
    Round(buff);
    iVal := buff[2] or (fLimbs[1] and SGN_MASK);
    exit;
  end;

  buff[0] := 0;
  buff[1] := fLimbs[0];
  buff[2] := fLimbs[1] and HI_FRAC_MASK or LEADING_ONE;
  buff[3] := 0;
  _shl(@buff, @buff, 4, 2 * cLCnt);
  Round(buff);
  if (buff[2] and OF_GUARD) > 0 then Inc(e);
  if 1023 < e then
    raise ERangeError.Create('TReal128.AsDouble exponent overlflows Double exponent range.');
  Inc(e, 1023);
  iVal := buff[2] and FRAC_MASK_F64;
  iVal := iVal or (UInt64(e) shl 52);  // exponent
  iVal := iVal or (fLimbs[1] and cBit64); // signum
end;

function TReal128.Abs: TReal128;
begin
  Result.fLimbs[0] := fLimbs[0];
  Result.fLimbs[1] := fLimbs[1] and (not SGN_MASK);
end;

function TReal128.IsExactZero: Boolean;
begin
  Result := (fLimbs[0] = 0) and ((fLimbs[1] and (not SGN_MASK)) = 0);
end;

function TReal128.IsZero: Boolean;
begin
  Result := IsZero(0);
end;

function TReal128.IsZero(const aEps: TReal128): Boolean;
begin
  if aEps.IsExactZero then begin
    Result := (AbsCompare(Self, cEpsF128) < 0)
  end else
    Result := (AbsCompare(Self, aEps) < 0);
end;

function TReal128.Lo: UInt64;
begin
  Result := fLimbs[0];
end;

function TReal128.Hi: UInt64;
begin
  Result := fLimbs[1];
end;

class function TReal128.AbsCompare(const A, B: TReal128): Integer;
var da, db: UInt64;
begin
  if A.Exponent < B.Exponent then exit(-1);
  if A.Exponent > B.Exponent then exit(1);

  da := A.fLimbs[1] and HI_FRAC_MASK;
  db := B.fLimbs[1] and HI_FRAC_MASK;
  if da > db then exit(1);
  if da < db then exit(-1);

  da := A.fLimbs[0];
  db := B.fLimbs[0];
  if da > db then exit(1);
  if da < db then exit(-1);

  Result := 0;
end;

class function TReal128.Compare(const A, B: TReal128): Integer;
begin
  if A.Signum < B.Signum then exit(-1);
  if A.Signum > B.Signum then exit(1);
  Result := AbsCompare(A, B);
end;

class operator TReal128.LessThan(const A, B: TReal128): Boolean;
begin
  Result := (Compare(A, B) < 0);
end;

class operator TReal128.GreaterThan(const A, B: TReal128): Boolean;
begin
  Result := (Compare(A, B) > 0);
end;

class operator TReal128.LessThanOrEqual(const A, B: TReal128): Boolean;
begin
  Result := (Compare(A, B) <= 0);
end;

class operator TReal128.GreaterThanOrEqual(const A, B: TReal128): Boolean;
begin
  Result := (Compare(A, B) >= 0);
end;

class operator TReal128.Equal(const A, B: TReal128): Boolean;
begin
  Result := (Compare(A, B) = 0);
end;

class operator TReal128.NotEqual(const A, B: TReal128): Boolean;
begin
  Result := (Compare(A, B) <> 0);
end;

{$region 'Getters/Setters'}

function TReal128.GetSignum: Integer;
begin
  if (fLimbs[1] and SGN_MASK) <> 0 then
    Result := -1
  else
    Result := 1;
end;

procedure TReal128.SetSignum(aValue: Integer);
begin
  if aValue >= 0 then
    fLimbs[1] := fLimbs[1] and (not SGN_MASK)
  else
    fLimbs[1] := fLimbs[1] or SGN_MASK;
end;

function TReal128.GetExponent: Integer;
begin
  Result := Int64((fLimbs[1] and EXP_MASK) shr 48) - EXP_BIAS;
end;

procedure TReal128.SetExponent(aValue: Integer);
begin
  fLimbs[1] := (fLimbs[1] and (SGN_MASK or HI_FRAC_MASK)) or
    ((UInt64(aValue + EXP_BIAS) shl 48) and EXP_MASK);
end;

{$endregion}

{$endregion}

{$region 'TCmplx256'}

procedure TCmplx256.Init(const aRe, aIm: TReal128);
begin
  Re := aRe;
  Im := aIm;
end;

class operator TCmplx256.Add(const A, B: TCmplx256): TCmplx256;
begin
  Result.Re := A.Re + B.Re;
  Result.Im := A.Im + B.Im;
end;

class operator TCmplx256.Subtract(const A, B: TCmplx256): TCmplx256;
begin
  Result.Re := A.Re - B.Re;
  Result.Im := A.Im - B.Im;
end;

class operator TCmplx256.Multiply(const A, B: TCmplx256): TCmplx256;
begin
  Result.Re := A.Re * B.Re - A.Im * B.Im;
  Result.Im := A.Re * B.Im + A.Im * B.Re;
end;

class operator TCmplx256.Multiply(const A: TReal128; const B: TCmplx256): TCmplx256;
begin
  Result.Re := A * B.Re;
  Result.Im := A * B.Im;
end;

class operator TCmplx256.Divide(const A, B: TCmplx256): TCmplx256;
var f: TReal128;
begin
  f := (B.Re * B.Re + B.Im * B.Im).Reciprocal;
  Result.Re := (A.Re * B.Re + A.Im * B.Im) * f;
  Result.Im := (A.Im * B.Re - A.Re * B.Im) * f;
end;

class operator TCmplx256.Divide(const A: TCmplx256; const B: TReal128): TCmplx256;
var f: TReal128;
begin
  f := B.Reciprocal;
  Result.Re := A.Re * f;
  Result.Im := A.Im * f;
end;

class operator TCmplx256.Negative(const A: TCmplx256): TCmplx256;
begin
  Result := A;
  Result.Re.SetSignum(-A.Re.Signum);
  Result.Im.SetSignum(-A.Im.Signum);
end;

class operator TCmplx256.Implicit(const A: TReal128): TCmplx256;
begin
  Result.Re := A;
  Result.Im := cZeroF128;
end;

class operator TCmplx256.Explicit(const A: Double): TCmplx256;
begin
  Result.Re := TReal128(A);
  Result.Im := cZeroF128;
end;

function TCmplx256.Reciprocal: TCmplx256;
var f: TReal128;
begin
  f := (Re * Re + Im * Im).Reciprocal;
  Result.Re := Re * f;
  Result.Im := Im * f;
  Result.Im.SetSignum(-1);
end;

function TCmplx256.IsReal: Boolean;
begin
  Result := Im.IsZero;
end;

function TCmplx256.IsReal(const aEps: TReal128): Boolean;
begin
  Result := Im.IsZero(aEps);
end;

function TCmplx256.IsZero: Boolean;
begin
  Result := IsZero(cEpsF128);
end;

function TCmplx256.IsZero(const aEps: TReal128): Boolean;
begin
  Result := Re.IsZero(aEps) and Im.IsZero(aEps);
end;

function TCmplx256.Abs: TReal128;
var A, B, r: TReal128;
begin
  if TReal128.AbsCompare(Re, Im) < 0 then begin
    A := Im;
    B := Re;
  end else begin
    A := Re;
    B := Im;
  end;

  if A.IsExactZero then exit(cZeroF128);

  r := B / A;
  Result := A.Abs * (cOneF128 + r * r).Sqrt;
end;

{$endregion}

end.
