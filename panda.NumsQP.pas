unit panda.NumsQP;

interface

uses
    System.Math
  , System.SysUtils
  , panda.Nums
  , panda.NumsLowLvl
  ;

{$I AsmDefs.inc}

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
    function Signum: Integer; inline;
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

  TUInt256 = record
  private const
    LCnt = {$ifdef Limb64}4{$else}8{$endif};
  public type
    TLimbs = array [0..3] of UInt64;
  private
    fLimbs: TLimbs;
    class procedure LongDivMod(var A, B: TUInt256); static;
    class procedure ShortDivMod(var A, B: TUInt256); static;
  public
    procedure Init(const L0, L1, L2, L3: UInt64); overload; inline;
    procedure SetToZero; inline;
    class operator Multiply(const A, B: TUInt256): TUInt256;
    class operator IntDivide(const A, B: TUInt256): TUInt256;
    class operator RightShift(const A: TUInt256; const B: Cardinal): TUInt256;
  end;
  PUInt256 = ^TUInt256;

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

    cLCnt = {$ifdef Limb64}2{$else}4{$endif};
  private
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
    fLimbs: array [0..1] of UInt64;
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

    class function SchoolDiv(const A, B: TReal128): TReal128; static;

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
    function IsExactZero: Boolean; inline;
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

function _AddI128(pA, pB, pRes: PByte): Integer;
function _SubI128(pA, pB, pRes: PByte): Integer;
procedure _NegI128(pA, pRes: PByte);

function _IncUI128(pA: PByte; D: TLimb): TLimb;
{$if defined(ASMx64)}
procedure _MulUI128(pA, pB, pRes: PByte);
{$endif}

{$endregion}

{$region 'exception helpers'}

procedure RaiseInt128Overflow;

{$endregion}

implementation

{$region 'exception helpers'}

procedure RaiseInt128Overflow;
begin
  raise EIntOverflow.Create('TInt128 overflow.');
end;

procedure RaiseUInt256Overflow;
begin
  raise EIntOverflow.Create('TUInt256 overflow.');
end;

{$endregion}

{$region 'low-level functions'}

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

function _Length256(pA: PByte): Integer; inline;
{$if SizeOf(TLimb) = 8}
begin
  if PUInt64(pA + 24)^ > 0 then exit(4);
  if PUInt64(pA + 16)^ > 0 then exit(3);
  if PUInt64(pA + 8)^ > 0 then exit(2);
  if PUInt64(pA)^ > 0 then exit(1);
  Result := 0;
end;
{$else}
begin
  if PUInt32(pA + 28)^ > 0 then exit(8);
  if PUInt32(pA + 24)^ > 0 then exit(7);
  if PUInt32(pA + 20)^ > 0 then exit(6);
  if PUInt32(pA + 16)^ > 0 then exit(5);
  if PUInt32(pA + 12)^ > 0 then exit(4);
  if PUInt32(pA + 8)^ > 0 then exit(3);
  if PUInt32(pA + 4)^ > 0 then exit(2);
  if PUInt32(pA)^ > 0 then exit(1);
  Result := 0;
end;
{$endif}

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

function TInt128.Signum: Integer;
begin
  if (fLimbs[1] and I64_HI_BIT) = 0 then
    Result := 1
  else
    Result := -1;
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
    ((A.Hi32 xor Result.Hi32) and (B.Hi32 xor Result.Hi32)) and I32_HI_BIT > 0
  then
    RaiseInt128Overflow;
{$endif}
end;

class operator TInt128.Subtract(const A, B: TInt128): TInt128;
begin
  _SubI128(@A, @B, @Result);
{$ifopt Q+}
  if
    ((A.Hi32 xor Result.Hi32) and (not (B.Hi32 xor Result.Hi32))) and I32_HI_BIT > 0
  then
    RaiseInt128Overflow;
{$endif}
end;

class operator TInt128.Negative(const A: TInt128): TInt128;
begin
  _NegI128(@A, @Result);
{$ifopt Q+}
  if (A.Hi32 and Result.Hi32 and I32_HI_BIT) > 0 then RaiseInt128Overflow;
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
  IsANeg := ((A.fLimbs[1] and I64_HI_BIT) <> 0);
  IsBNeg := ((B.fLimbs[1] and I64_HI_BIT) <> 0);
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
  isANeg := (A.Hi and I64_HI_BIT <> 0);
  isBNeg := (B.Hi and I64_HI_BIT <> 0);
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
  if (A.Hi32 and Result.Hi32 and I32_HI_BIT) > 0 then RaiseInt128Overflow;
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
  if offset >= LCnt then exit;

  r := B and cLimbRemMask;
  p := PByte(@A) + offset * cLimbSize;
  _shr(p, @Result, r, LCnt - offset);
end;

class operator TUInt128.LeftShift(const A: TUInt128; const B: Cardinal): TUInt128;
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

{$region 'TUInt256'}

procedure TUInt256.Init(const L0, L1, L2, L3: UInt64);
begin
  fLimbs[0] := L0;
  fLimbs[1] := L1;
  fLimbs[2] := L2;
  fLimbs[3] := L3;
end;

procedure TUInt256.SetToZero;
begin
  fLimbs[0] := 0;
  fLimbs[1] := 0;
  fLimbs[2] := 0;
  fLimbs[3] := 0;
end;

{$ifdef RANGEON}
   {$R-}
{$endif}

class procedure TUInt256.LongDivMod(var A, B: TUInt256);
var nA, nB, pwr: Integer;
    Adata, Bdata: array [0..5] of UInt64;
    AArr: array [0..0] of TLimb absolute Adata;
    BArr: array [0..0] of TLimb absolute Bdata;
    Q: array [0..3] of UInt64;
begin
  AData[0] := 0; AData[1] := 0; AData[2] := 0; AData[3] := 0; AData[4] := 0;
  BData[0] := 0; BData[1] := 0; BData[2] := 0; BData[3] := 0; AData[4] := 0;
  Q[0] := 0; Q[1] := 0; Q[2] := 0; Q[3] := 0;
  nA := _Length256(@A);
  nB := _Length256(@B);
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
  B.fLimbs[2] := AData[2];
  B.fLimbs[3] := AData[3];
  B := B shr pwr;
  A.fLimbs[0] := Q[0];
  A.fLimbs[1] := Q[1];
  A.fLimbs[2] := Q[2];
  A.fLimbs[3] := Q[3];
end;
{$ifdef RANGEON}
  {$R+}
{$endif}

class procedure TUInt256.ShortDivMod(var A, B: TUInt256);
var r: TLimb;
    nA: Integer;
    data: array [0..3] of UInt64;
begin
  data[0] := 0; data[1] := 0; data[2] := 0; data[3] := 0;
  nA := _Length256(@A);
  r := _ShortDivMod(@A, @B, @data, nA);
  A.SetToZero;
  Move(data, A, nA * cLimbSize);
  B.SetToZero;
  PLimb(@B)^ := r;
end;

class operator TUInt256.IntDivide(const A, B: TUInt256): TUInt256;
var r: TUInt256;
begin
  Result := A;
  r := B;
  if _Length256(@r) = 1 then
    ShortDivMod(Result, r)
  else
    LongDivMod(Result, r);
end;

class operator TUInt256.Multiply(const A, B: TUInt256): TUInt256;
var buff: array [0..7] of UInt64;
begin
  buff[0] := 0;
  buff[1] := 0;
  buff[2] := 0;
  buff[3] := 0;
  buff[4] := 0;
  buff[5] := 0;
  buff[6] := 0;
  buff[7] := 0;
  _Mul(@A, @B, @buff, LCnt, LCnt);
{$ifopt Q+}
  if (buff[4] or buff[5] or buff[6] or buff[7]) <> 0 then
    RaiseUInt256Overflow;
{$endif}
  Result.fLimbs[0] := buff[0];
  Result.fLimbs[1] := buff[1];
  Result.fLimbs[2] := buff[2];
  Result.fLimbs[3] := buff[4];
end;

class operator TUInt256.RightShift(const A: TUInt256; const B: Cardinal): TUInt256;
var offset, r: Integer;
    p: PByte;
begin
  Result.SetToZero;
  offset := B shr W_LOG2_BITS;
  if offset >= LCnt then exit;

  r := B and cLimbRemMask;
  p := PByte(@A) + offset * cLimbSize;
  _shr(p, @Result, r, LCnt - offset);
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
      ((iVal and F64_FRAC_MASK) shr 4) or // fraction
      UInt64(((e - $3FF) + EXP_BIAS) shl 48) or // exponent
      (iVal and I64_HI_BIT); // signum
  end else begin
    //subnormal numbers
    if (iVal and (not I64_HI_BIT)) = 0 then begin
      fLimbs[0] := 0;
      fLimbs[1] := 0;
    end else begin
      fLimbs[0] := 0;
      fLimbs[1] := iVal and F64_FRAC_MASK;
      e := _CountLeadingZeros(@fLimbs, cLCnt);
      _ipLongShift(@fLimbs, cLCnt, e - FRAC_OFFSET + 1);
      fLimbs[1] := fLimbs[1] and HI_FRAC_MASK;
      e := EXP_BIAS - F64_EXP_BIAS + F64_FRAC_OFFSET - e;
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
const REST_MASK = not I64_HI_BIT;
begin
  Result := 0;
  if (buff[1] and I64_HI_BIT) = 0 then exit; // remainder < 1/2*ULP -> round down
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
//  Result := A * B.InternalReciprocal;
//  AdjustByReminder(A, B, Result);
  Result := SchoolDiv(A, B);
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

  bNeg := (buff[7] and I64_HI_BIT) > 0;
  if bNeg then
    _NegMPI(@buff[4], @buff[4], 2 * cLCnt);

  if (buff[5] and I64_HI_BIT) > 0 then begin
    ulp.Init(0, 0);
    ulp.Exponent := -FRAC_BIT_CNT + Q.Exponent;
    if bNeg then ulp.Signum := -1;
    Q := Q + ulp;
  end;
end;

class function TReal128.SchoolDiv(const A, B: TReal128): TReal128;
var fa, fb, fc: TUInt256;
    de: Integer;
begin
  if B.IsExactZero then
    raise EDivByZero.Create('Division by zero.');

  if A.IsExactZero then exit(cZeroF128);

  fa.Init(0, 0, A.fLimbs[0], (A.fLimbs[1] and HI_FRAC_MASK) or LEADING_ONE);
  fb.Init(B.fLimbs[0], (B.fLimbs[1] and HI_FRAC_MASK) or LEADING_ONE, 0, 0);
  fc := fa div fb;

  de := _CountLeadingZeros(@fc.fLimbs[0], 2 * cLCnt) - (FRAC_OFFSET - 1);
  _ipLongShift(@fc.fLimbs[0], 2 * cLCnt, de);
  Round(fc.fLimbs);

  Result.fLimbs[0] := fc.fLimbs[2];
  Result.fLimbs[1] := fc.fLimbs[3] and HI_FRAC_MASK;
  Result.Exponent := A.Exponent - B.Exponent - (de - cLCnt*W_BIT_COUNT + FRAC_OFFSET);
  Result.Signum := A.Signum * B.Signum;
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
//  Result := SchoolDiv(cOneF128, Self);
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
  iVal := buff[2] and F64_FRAC_MASK;
  iVal := iVal or (UInt64(e) shl 52);  // exponent
  iVal := iVal or (fLimbs[1] and I64_HI_BIT); // signum
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
  Result := IsZero(cZeroF128);
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

function TCmplx256.IsExactZero: Boolean;
begin
  Result := Re.IsExactZero and Im.IsExactZero;
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
