unit panda.Poly;

interface

uses
    panda.Nums
  , panda.MPNums
  , System.Math
  ;

{$I AsmDefs.inc}

type
  /// <summary>
  ///   Represents polynomial <c>Sum[a_i * x^i, {i, 0, n}]</c> of a real variable <c>x</c>
  ///   where coefficients <c>a_i</c> are real numbers.
  /// </summary>
  TPolyF64 = record
  private
    fCoeffs: TArray<Double>;
    function GetHighCoeff: Double; inline;
    function GetCoeff(I: NativeInt): Double; inline;
    procedure SetCoeff(I: NativeInt; const aValue: Double); inline;
    function GetValue(const aX: Double): Double; inline;
    /// <summary> Remove small leading coefficients from a polynomial</summary>
    class procedure TrimZeros(var aValue: TArray<Double>; aEps: Double = 0); static;
    class function PolyAdd(const aP, aQ: TArray<Double>): TArray<Double>; overload; static;
    class function PolyAdd(const aP: TArray<Double>; const aV: Double): TArray<Double>; overload; static;
    class function PolySub(const aP, aQ: TArray<Double>): TArray<Double>; static;
    class function PolyMul(const aP, aQ: TArray<Double>): TArray<Double>; overload; static;
    class function PolyMul(const aP: TArray<Double>; const aV: Double): TArray<Double>; overload; static;
    class function PolyNeg(const aP: TArray<Double>): TArray<Double>; static;
  public
    procedure Init(const aCoeffs: array of Double); overload;
    procedure Init(const aCoeffs: TArray<Double>); overload;
    function Clone: TPolyF64;

    class operator Add(const aP, aQ: TPolyF64): TPolyF64; inline;
    class operator Add(const aP: TPolyF64; const aV: Double): TPolyF64; inline;
    class operator Subtract(const aP, aQ: TPolyF64): TPolyF64; inline;
    class operator Subtract(const aP: TPolyF64; const aV: Double): TPolyF64; inline;
    class operator Multiply(const aP, aQ: TPolyF64): TPolyF64; inline;
    class operator Multiply(const aV: Double; const aP: TPolyF64): TPolyF64; inline;
    class operator IntDivide(const aP, aQ: TPolyF64): TPolyF64; inline;
    class operator Modulus(const aP, aQ: TPolyF64): TPolyF64; inline;
    class operator Divide(const aP: TPolyF64; const aV: Double): TPolyF64; inline;
    class operator Negative(const aP: TPolyF64): TPolyF64; inline;
    class operator Explicit(const aV: Double): TPolyF64; inline;

    function Order: NativeInt; inline;
    function IsZero(const aEps: Double = cEpsF64): Boolean;
    class function PolyZero: TPolyF64; inline; static;
    class function PolyOne: TPolyF64; inline; static;
    class function PolyConst(const aValue: Double): TPolyF64; inline; static;
    /// <summary>Create a polynomial <c>X</c>.</summary>
    class function PolyX: TPolyF64; inline; static;
    /// <summary>Create a linear polynomial <c>a0 + a1*x</c>.</summary>
    class function PolyLine(const a0, a1: Double): TPolyF64; inline; static;
    /// <summary>Divide a polynomial <c>aP</c> by a polynomial <c>aQ</c>.</summary>
    /// <param name="aQu">Quotient of the division <c>aP/aQ</c></param>
    /// <param name="aRe">Remainder of the division <c>aP/aQ</c></param>
    class procedure GetQuotientRemainder(const aP, aQ: TPolyF64; out aQu, aRe: TPolyF64); static;
    /// <summary>
    ///   Multiple a polynoimal by the monom <c>z^N</c>. Elements
    ///   with the negative power will be dropped.
    /// </summary>
    function Shift(N: NativeInt): TPolyF64;
    /// <summary>
    ///   Replace all occurences of the free variable in a polynomial <c>aP</c>
    ///   by a polynomial <c>aQ</c>.
    /// </summary>
    function Substitute(const aQ: TPolyF64): TPolyF64;
    function Derivative: TPolyF64;
    /// <summary>Evaluate <c>aP^n</c>.</summary>
    function Power(N: Cardinal): TPolyF64;
    class function GCD(const aP, aQ: TPolyF64; const aZeroTol: Double = cEpsF64): TPolyF64; static;

    property Coeff[I: NativeInt]: Double read GetCoeff write SetCoeff;
    property CoeffList: TArray<Double> read fCoeffs;
    property HighCoeff: Double read GetHighCoeff;
    /// <summary>Evaluate a polynomial at point <c>aX</c>.</summary>
    property Value[const aX: Double]: Double read GetValue; default;
  end;

  /// <summary>
  ///   Represents polynomial <c>Sum[a_i * x^i, {i, 0, n}]</c> of a complex variable <c>x</c>
  ///   where coefficients <c>a_i</c> are complex numbers.
  /// </summary>
  TPolyC128 = record
  private
    fCoeffs: TArray<TCmplx128>;
    function GetHighCoeff: TCmplx128; inline;
    function GetCoeff(I: NativeInt): TCmplx128; inline;
    procedure SetCoeff(I: NativeInt; const aValue: TCmplx128); inline;
    function GetValue(const aX: TCmplx128): TCmplx128; inline;
    /// <summary> Remove small leading coefficients from a polynomial</summary>
    class procedure TrimZeros(var aValue: TArray<TCmplx128>; aEps: Double = 0); static;
    class function PolyAdd(const aP, aQ: TArray<TCmplx128>): TArray<TCmplx128>; overload; static;
    class function PolyAdd(const aP: TArray<TCmplx128>; const aV: TCmplx128): TArray<TCmplx128>; overload; static;
    class function PolySub(const aP, aQ: TArray<TCmplx128>): TArray<TCmplx128>; static;
    class function PolyMul(const aP, aQ: TArray<TCmplx128>): TArray<TCmplx128>; overload; static;
    class function PolyMul(const aP: TArray<TCmplx128>; const aV: TCmplx128): TArray<TCmplx128>; overload; static;
    class function PolyNeg(const aP: TArray<TCmplx128>): TArray<TCmplx128>; static;
  public
    procedure Init(const aCoeffs: array of TCmplx128); overload;
    procedure Init(const aCoeffs: TArray<TCmplx128>); overload;
    procedure InitRealCoeffs(const aCoeffs: array of Double);
    function Clone: TPolyC128;

    class operator Add(const aP, aQ: TPolyC128): TPolyC128; inline;
    class operator Add(const aP: TPolyC128; const aV: TCmplx128): TPolyC128; inline;
    class operator Subtract(const aP, aQ: TPolyC128): TPolyC128; inline;
    class operator Subtract(const aP: TPolyC128; const aV: TCmplx128): TPolyC128; inline;
    class operator Multiply(const aP, aQ: TPolyC128): TPolyC128; inline;
    class operator Multiply(const aV: TCmplx128; const aP: TPolyC128): TPolyC128; inline;
    class operator IntDivide(const aP, aQ: TPolyC128): TPolyC128; inline;
    class operator Modulus(const aP, aQ: TPolyC128): TPolyC128; inline;
    class operator Divide(const aP: TPolyC128; const aV: TCmplx128): TPolyC128; inline;
    class operator Negative(const aP: TPolyC128): TPolyC128; inline;
    class operator Explicit(const aV: TCmplx128): TPolyC128; inline;
    class operator Explicit(const aV: Double): TPolyC128; inline;

    function Order: NativeInt; inline;
    function IsZero(const aEps: Double = cEpsF64): Boolean;
    class function PolyZero: TPolyC128; inline; static;
    class function PolyOne: TPolyC128; inline; static;
    class function PolyConst(const aValue: TCmplx128): TPolyC128; overload; static;
    class function PolyConst(const aRe, aIm: Double): TPolyC128; overload; static;
    /// <summary>Create a polynomial <c>X</c>.</summary>
    class function PolyX: TPolyC128; inline; static;
    function HasRealCoeffs(const aEps: Double = 0): Boolean;
    function GetCoeffsRealPart: TArray<Double>;
    function GetCoeffsImagPart: TArray<Double>;

    /// <summary>Divide a polynomial <c>aP</c> by a polynomial <c>aQ</c>.</summary>
    /// <param name="aQu">Quotient of the division <c>aP/aQ</c></param>
    /// <param name="aRe">Remainder of the division <c>aP/aQ</c></param>
    class procedure GetQuotientRemainder(const aP, aQ: TPolyC128; out aQu, aRe: TPolyC128); static;
    /// <summary>
    ///   Multiple a polynoimal by the monom <c>z^N</c>. Elements
    ///   with the negative power will be dropped.
    /// </summary>
    function Shift(N: NativeInt): TPolyC128;
    /// <summary>
    ///   Replace all occurences of the free variable in a polynomial <c>aP</c>
    ///   by a polynomial <c>aQ</c>.
    /// </summary>
    function Substitute(const aQ: TPolyC128): TPolyC128;
    function Derivative: TPolyC128;
    /// <summary>Return a reciprocal polynomial <c>x^n * aP(1/x)</c></summary>
    function Reciprocal: TPolyC128;
    class function GCD(const aP, aQ: TPolyC128; const aZeroTol: Double = cEpsF64): TPolyC128; static;

    property Coeff[I: NativeInt]: TCmplx128 read GetCoeff write SetCoeff;
    property CoeffList: TArray<TCmplx128> read fCoeffs;
    property HighCoeff: TCmplx128 read GetHighCoeff;
    /// <summary>Evaluate a polynomial at point <c>aX</c>.</summary>
    property Value[const aX: TCmplx128]: TCmplx128 read GetValue; default;
  end;

  TPolyC256 = record
  private
    fCoeffs: TArray<TCmplx256>;
    function GetHighCoeff: TCmplx256; inline;
    function GetCoeff(I: NativeInt): TCmplx256; inline;
    procedure SetCoeff(I: NativeInt; const aValue: TCmplx256); inline;
    function GetValue(const aX: TCmplx256): TCmplx256; inline;
    /// <summary> Remove small leading coefficients from a polynomial</summary>
    class procedure TrimZeros(var aValue: TArray<TCmplx256>; aEps: TReal128); overload; static;
    class function PolyAdd(const aP, aQ: TArray<TCmplx256>): TArray<TCmplx256>; overload; static;
    class function PolyAdd(const aP: TArray<TCmplx256>; const aV: TCmplx256): TArray<TCmplx256>; overload; static;
    class function PolySub(const aP, aQ: TArray<TCmplx256>): TArray<TCmplx256>; static;
    class function PolyMul(const aP, aQ: TArray<TCmplx256>): TArray<TCmplx256>; overload; static;
    class function PolyMul(const aP: TArray<TCmplx256>; const aV: TCmplx256): TArray<TCmplx256>; overload; static;
    class function PolyNeg(const aP: TArray<TCmplx256>): TArray<TCmplx256>; static;
  public
    procedure Init(const aCoeffs: array of TCmplx256); overload;
    procedure Init(const aCoeffs: TArray<TCmplx256>); overload;
    function Clone: TPolyC256;

    class operator Add(const aP, aQ: TPolyC256): TPolyC256; inline;
    class operator Add(const aP: TPolyC256; const aV: TCmplx256): TPolyC256; inline;
    class operator Subtract(const aP, aQ: TPolyC256): TPolyC256; inline;
    class operator Subtract(const aP: TPolyC256; const aV: TCmplx256): TPolyC256; inline;
    class operator Multiply(const aP, aQ: TPolyC256): TPolyC256; inline;
    class operator Multiply(const aV: TCmplx256; const aP: TPolyC256): TPolyC256; inline;
    class operator IntDivide(const aP, aQ: TPolyC256): TPolyC256;
    class operator Modulus(const aP, aQ: TPolyC256): TPolyC256;
    class operator Divide(const aP: TPolyC256; const aV: TCmplx256): TPolyC256; inline;
    class operator Negative(const aP: TPolyC256): TPolyC256; inline;
    class operator Explicit(const aV: TCmplx256): TPolyC256; inline;
    class operator Explicit(const aP: TPolyF64): TPolyC256;
    class operator Explicit(const aP: TPolyC128): TPolyC256;
    class operator Explicit(const aP: TPolyC256): TPolyC128;

    function Order: NativeInt; inline;
    function IsZero: Boolean; overload;
    function IsZero(const aEps: TReal128): Boolean; overload;
    class function PolyZero: TPolyC256; inline; static;
    class function PolyOne: TPolyC256; inline; static;
    class function PolyConst(const aValue: TCmplx256): TPolyC256; overload; static;
    class function PolyConst(const aRe, aIm: TReal128): TPolyC256; overload; static;
    /// <summary>Create a polynomial <c>X</c>.</summary>
    class function PolyX: TPolyC256; inline; static;

    /// <summary>Divide a polynomial <c>aP</c> by a polynomial <c>aQ</c>.</summary>
    /// <param name="aQu">Quotient of the division <c>aP/aQ</c></param>
    /// <param name="aRe">Remainder of the division <c>aP/aQ</c></param>
    class procedure GetQuotientRemainder(const aP, aQ: TPolyC256; out aQu, aRe: TPolyC256); static;
    /// <summary>
    ///   Multiple a polynoimal by the monom <c>z^N</c>. Elements
    ///   with the negative power will be dropped.
    /// </summary>
    function Shift(N: NativeInt): TPolyC256;
    /// <summary>
    ///   Replace all occurences of the free variable in a polynomial <c>aP</c>
    ///   by a polynomial <c>aQ</c>.
    /// </summary>
    function Substitute(const aQ: TPolyC256): TPolyC256;
    function Derivative: TPolyC256;
    /// <summary>Return a reciprocal polynomial <c>x^n * aP(1/x)</c></summary>
    function Reciprocal: TPolyC256;
    class function GCD(const aP, aQ: TPolyC256): TPolyC256; overload; static;
    class function GCD(const aP, aQ: TPolyC256; const aZeroTol: TReal128): TPolyC256; overload; static;

    property Coeff[I: NativeInt]: TCmplx256 read GetCoeff write SetCoeff;
    property CoeffList: TArray<TCmplx256> read fCoeffs;
    property HighCoeff: TCmplx256 read GetHighCoeff;
    /// <summary>Evaluate a polynomial at point <c>aX</c>.</summary>
    property Value[const aX: TCmplx256]: TCmplx256 read GetValue; default;
  end;

procedure PolyEval(var aValue: Double; pCoeffs: PDouble; N: NativeInt); overload;
procedure PolyEval(var aValue: TCmplx128; pCoeffs: PCmplx128; N: NativeInt); overload;

implementation

{$region 'low-level functions'}

procedure PolyEval(var aValue: Double; pCoeffs: PDouble; N: NativeInt);
{$if defined(ASMx86)}
// EAX <- @X (@Result), EDX <- @Coeff[0], ECX <- N
asm
  movddup xmm2, [eax] // xmm2 <- (X, X)
  movapd xmm3, xmm2   // xmm3 <- (X, X)
  mulpd xmm2, xmm2    // xmm2 <- (X^2, X^2)
  test ecx, 1
  jz @E
@O: // odd coefficent count
  lea edx, [edx + 8 * ecx - 8]
  movq xmm0, [edx]
  inc ecx
  jmp @S
@E: // even coefficient count
  lea edx, [edx + 8 * ecx - 16]
  movupd xmm0, [edx]
@S:
  sub edx, 16
  shr ecx, 1
  dec ecx
  jz @Sum
@L:
  mulpd xmm0, xmm2
  movupd xmm1, [edx]
  addpd xmm0, xmm1
  sub edx, 16
  dec ecx
  jnz @L

@Sum:
  movhlps xmm1, xmm0
  mulsd xmm1, xmm3
  addsd xmm0, xmm1
  movlpd [eax], xmm0
end;
{$elseif defined(ASMx64)}
// RCX <- @X (@Result), RDX <- @Coeff[0], R8 <- N
asm
  movddup xmm2, [rcx] // xmm2 <- (X, X)
  movapd xmm3, xmm2   // xmm3 <- (X, X)
  mulpd xmm2, xmm2    // xmm2 <- (X^2, X^2)
  test r8, 1
  jz @E
@O: // odd coefficent count
  lea rdx, [rdx + 8 * r8 - 8]
  movq xmm0, [rdx]
  inc r8
  jmp @S
@E: // even coefficient count
  lea rdx, [rdx + 8 * r8 - 16]
  movupd xmm0, [rdx]
@S:
  sub rdx, 16
  shr r8, 1
  dec r8
  jz @Sum
@L:
  mulpd xmm0, xmm2
  movupd xmm1, [rdx]
  addpd xmm0, xmm1
  sub rdx, 16
  dec r8
  jnz @L

@Sum:
  movhlps xmm1, xmm0
  mulsd xmm1, xmm3
  addsd xmm0, xmm1
  movlpd [rcx], xmm0
end;
{$else}
var b: Double;
    C: PDouble;
begin
  Dec(N);
  if N = 0 then begin
    aValue := pCoeffs^;
    exit;
  end;
  // Horner's method
  C := pCoeffs;
  Inc(C, N);
  b := C^;
  repeat
    Dec(C);
    b := C^ + b * aValue;
  until C = pCoeffs;
  aValue := b;
end;
{$endif}

procedure PolyEval(var aValue: TCmplx128; pCoeffs: PCmplx128; N: NativeInt);
{$if defined(ASMx86)}
// EAX <- @X (@Redsult), EDX <- @Coeffs[0], ECX <- N
asm
  cmp ecx, 1
  jg @H
  movupd xmm0, [edx]
  movupd [eax], xmm0
  ret
@H: //Horner
  movddup xmm0, [eax]     // xmm0 <- (XRe, XRe)
  movddup xmm1, [eax + 8] // xmm1 <- (XIm, XIm)
  shl ecx, 1
  lea ecx, [edx + 8 * ecx - 16] // ECX <- @Coeff[N - 1]
  movupd xmm2, [ecx]
  movapd xmm3, xmm2 // xmm2, xmm3 <- b := Coeff[N - 1]
@L:
  mulpd xmm2, xmm0  // xmm2 <- (XRe*bRe, XRe*bIm)
  mulpd xmm3, xmm1  // xmm3 <- (XIm*bRe, XIm*bIm)
  pshufd xmm3, xmm3, $4e // xmm3 <- (XIm*bIm, XIm*bRe)
  addsubpd xmm2, xmm3 // xmm2 <- X*b
  sub ecx, 16
  movupd xmm3, [ecx]
  addpd xmm2, xmm3
  movapd xmm3, xmm2  // xmm2, xmm3 <- b*X + Coeff[I]
  cmp ecx, edx
  jg @L

  movupd [eax], xmm2
end;
{$elseif defined(ASMx64)}
// RCX <- @X (@Result), RDX <- @Coeff[0], R8 <- N
asm
  cmp r8, 1
  jg @H
  movupd xmm0, [rdx]
  movupd [rcx], xmm0
  ret
@H: // Horner
  movddup xmm0, [rcx]     // xmm0 <- (XRe, XRe)
  movddup xmm1, [rcx + 8] // xmm1 <- (XIm, XIm)
  shl r8, 1
  lea rax, [rdx + 8 * r8 - 16] // RAX <- @Coeff[N - 1]
  movupd xmm2, [rax]
  movapd xmm3, xmm2  // xmm2, xmm3 <- b := Coeff[N - 1]
@L:
  mulpd xmm2, xmm0   // xmm2 <- (XRe*bRe, XRe*bIm)
  mulpd xmm3, xmm1   // xmm3 <- (XIm*bRe, XIm*bIm)
  pshufd xmm3, xmm3, $4e // xmm3 <- (XIm*bIm, XIm*bRe)
  addsubpd xmm2, xmm3 // xmm2 <- X*b
  sub rax, 16
  movupd xmm3, [rax]
  addpd xmm2, xmm3
  movapd xmm3, xmm2  // xmm2, xmm3 <- b*X + Coeff[I]
  cmp rax, rdx
  jg @L

  movupd [rcx], xmm2
end;
{$else}
var b: TCmplx128;
    C: PCmplx128;
begin
  Dec(N);
  if N = 0 then begin
    aValue := pCoeffs^;
    exit;
  end;
  C := pCoeffs;
  Inc(C, N);
  b := C^;
  repeat
    Dec(C);
    b := C^ + b * aValue;
  until C = pCoeffs;
  aValue := b;
end;
{$endif}

{$endregion}

{$region 'TPolyF64'}

procedure TPolyF64.Init(const aCoeffs: array of Double);
var len: NativeInt;
begin
  len := Length(aCoeffs);
  Assert(aCoeffs[len - 1] <> 0);
  SetLength(fCoeffs, len);
  Move(aCoeffs[0], fCoeffs[0], len * SizeOf(Double));
end;

procedure TPolyF64.Init(const aCoeffs: TArray<Double>);
begin
  Assert(aCoeffs[High(aCoeffs)] <> 0);
  fCoeffs := aCoeffs;
end;

function TPolyF64.Clone: TPolyF64;
begin
  Result.fCoeffs := Copy(fCoeffs, 0, Length(fCoeffs));
end;

function TPolyF64.GetCoeff(I: NativeInt): Double;
begin
  if InRange(I, 0, High(fCoeffs)) then Result := fCoeffs[I]
  else Result := 0;
end;

function TPolyF64.GetHighCoeff: Double;
begin
  Result := fCoeffs[High(fCoeffs)];
end;

procedure TPolyF64.SetCoeff(I: NativeInt; const aValue: Double);
begin
  if I > High(fCoeffs) then begin
    if IsZero(aValue) then exit;
    SetLength(fCoeffs, I + 1);
  end;
  fCoeffs[I] := aValue;
end;

function TPolyF64.GetValue(const aX: Double): Double;
begin
  Result := aX;
  PolyEval(REsult, PDouble(fCoeffs), Length(fCoeffs));
end;

class procedure TPolyF64.TrimZeros(var aValue: TArray<Double>; aEps: Double);
var n: NativeInt;
begin
  n := High(aValue);
  while (n > 0) and System.Math.IsZero(aValue[n], aEps) do Dec(n);
  if n <> High(aValue) then SetLength(aValue, n + 1);
end;

class function TPolyF64.PolyAdd(const aP, aQ: TArray<Double>): TArray<Double>;
var I, J, lenP, lenQ: NativeInt;
    c: TArray<Double>;
begin
  lenP := Length(aP);
  lenQ := Length(aQ);
  SetLength(c, lenP + lenQ);
  if lenP > lenQ then begin
    SetLength(Result, lenP);
    for I := lenQ to lenP - 1 do
      Result[I] := aP[I];
    J := lenQ - 1;
  end else begin // lenP <= lenQ
    SetLength(Result, lenQ);
    for I := lenP to lenQ - 1 do
      Result[I] := aQ[I];
    J := lenP - 1;
  end;
  for I := 0 to J do
    Result[I] := aP[I] + aQ[I];
  TrimZeros(Result);
end;

class function TPolyF64.PolyAdd(const aP: TArray<Double>; const aV: Double): TArray<Double>;
begin
  Result := Copy(aP, 0, Length(aP));
  Result[0] := Result[0] + aV;
  TrimZeros(Result);
end;

class function TPolyF64.PolySub(const aP, aQ: TArray<Double>): TArray<Double>;
var I, J, lenP, lenQ: NativeInt;
    c: TArray<Double>;
begin
  lenP := Length(aP);
  lenQ := Length(aQ);
  SetLength(c, lenP + lenQ);
  if lenP > lenQ then begin
    SetLength(Result, lenP);
    for I := lenQ to lenP - 1 do
      Result[I] := aP[I];
    J := lenQ - 1;
  end else begin // lenP <= lenQ
    SetLength(Result, lenQ);
    for I := lenP to lenQ - 1 do
      Result[I] := -aQ[I];
    J := lenP - 1;
  end;
  for I := 0 to J do
    Result[I] := aP[I] - aQ[I];
  TrimZeros(Result);
end;

class function TPolyF64.PolyMul(const aP, aQ: TArray<Double>): TArray<Double>;
var lenP, lenQ, I, J, lo, hi: NativeInt;
    tmp: Double;
begin
  lenP := Length(aP);
  lenQ := Length(aQ);

  if lenP = 1 then begin
    Result := PolyMul(aQ, aP[0]);
    exit;
  end;

  if lenQ = 1 then begin
    Result := PolyMul(aP, aQ[0]);
    exit;
  end;

  SetLength(Result, lenP + lenQ - 1);
  for I := 0 to lenP + lenQ - 2 do begin
    tmp := 0;
    lo := Max(0, I - lenQ + 1);
    hi := Min(I, lenP - 1);
    for J := lo to hi do
      tmp := tmp + aP[J] * aQ[I - J];
    Result[I] := tmp;
  end;
end;

class function TPolyF64.PolyMul(const aP: TArray<Double>; const aV: Double): TArray<Double>;
var I: NativeInt;
begin
  SetLength(Result, Length(aP));
  for I := 0 to High(Result) do
    Result[I] := aP[I] * aV;
end;

class function TPolyF64.PolyNeg(const aP: TArray<Double>): TArray<Double>;
var I: NativeInt;
begin
  SetLength(Result, Length(aP));
  for I := 0 to High(Result) do
    Result[I] := -aP[I];
end;

class operator TPolyF64.Add(const aP, aQ: TPolyF64): TPolyF64;
begin
  Result.fCoeffs := PolyAdd(aP.fCoeffs, aQ.fCoeffs);
end;

class operator TPolyF64.Add(const aP: TPolyF64; const aV: Double): TPolyF64;
begin
  if aV <> 0 then
    Result.fCoeffs := PolyAdd(aP.fCoeffs, aV)
  else
    Result := aP;
end;

class operator TPolyF64.Subtract(const aP, aQ: TPolyF64): TPolyF64;
begin
  Result.fCoeffs := PolySub(aP.fCoeffs, aQ.fCoeffs);
end;

class operator TPolyF64.Subtract(const aP: TPolyF64; const aV: Double): TPolyF64;
begin
  Result.fCoeffs := PolyAdd(aP.fCoeffs, -aV);
end;

class operator TPolyF64.Multiply(const aP, aQ: TPolyF64): TPolyF64;
begin
  Result.fCoeffs := PolyMul(aP.fCoeffs, aQ.fCoeffs);
end;

class operator TPolyF64.Multiply(const aV: Double; const aP: TPolyF64): TPolyF64;
begin
  if aV <> 1 then
    Result.fCoeffs := PolyMul(aP.fCoeffs, aV)
  else
    Result := aP;
end;

class operator TPolyF64.IntDivide(const aP, aQ: TPolyF64): TPolyF64;
var r: TPolyF64;
begin
  GetQuotientRemainder(aP, aQ, Result, r);
end;

class operator TPolyF64.Modulus(const aP, aQ: TPolyF64): TPolyF64;
var q: TPolyF64;
begin
  GetQuotientRemainder(aP, aQ, q, Result);
end;

class operator TPolyF64.Divide(const aP: TPolyF64; const aV: Double): TPolyF64;
begin
  if aV <> 1 then
    Result.fCoeffs := PolyMul(aP.fCoeffs, 1/aV)
  else
    Result := aP;
end;

class operator TPolyF64.Negative(const aP: TPolyF64): TPolyF64;
begin
  Result.fCoeffs := PolyNeg(aP.fCoeffs);
end;

class operator TPolyF64.Explicit(const aV: Double): TPolyF64;
begin
  Result.Init([aV]);
end;

function TPolyF64.Order: NativeInt;
begin
  Result := High(fCoeffs);
end;

function TPolyF64.IsZero(const aEps: Double): Boolean;
var I, count: NativeInt;
begin
  count := Length(fCoeffs);
  for I := 0 to count - 1 do
    if not System.Math.IsZero(fCoeffs[I], aEps) then exit(False);
  Result := True;
end;

class function TPolyF64.PolyZero: TPolyF64;
begin
  with Result do begin
    SetLength(fCoeffs, 1);
    fCoeffs[0] := 0;
  end;
end;

class function TPolyF64.PolyOne: TPolyF64;
begin
  with Result do begin
    SetLength(fCoeffs, 1);
    fCoeffs[0] := 1;
  end;
end;

class function TPolyF64.PolyConst(const aValue: Double): TPolyF64;
begin
  with Result do begin
    SetLength(fCoeffs, 1);
    fCoeffs[0] := aValue;
  end;
end;

class function TPolyF64.PolyX: TPolyF64;
begin
  with Result do begin
    SetLength(fCoeffs, 2);
    fCoeffs[0] := 0;
    fCoeffs[1] := 1;
  end;
end;

class function TPolyF64.PolyLine(const a0, a1: Double): TPolyF64;
begin
  with Result do begin
    SetLength(fCoeffs, 2);
    fCoeffs[0] := a0;
    fCoeffs[1] := a1;
  end;
end;

class procedure TPolyF64.GetQuotientRemainder(const aP, aQ: TPolyF64;
  out aQu, aRe: TPolyF64);
var M, N, I: NativeInt;
    p: TPolyF64;
    tmp, b0: Double;
    c: TArray<Double>;
begin
  M := aQ.Order;
  N := aP.Order;
  if M = 0 then begin
    aQu := aP / aQ.fCoeffs[0];
    aRe := PolyZero;
    exit;
  end;
  if M > N then begin
    aQu := PolyZero;
    aRe := aP.Clone;
    exit;
  end;
  p := aP;
  I := p.Order - M;
  SetLength(c, I + 1);
  b0 := aQ.fCoeffs[aQ.Order];
  while I >= 0 do begin
    tmp := p.fCoeffs[p.Order] / b0;
    p := p - (tmp * aQ).Shift(I);
    c[I] := tmp;
    I := p.Order - M;
  end;
  aQu.Init(c);
  aRe := p;
end;

function TPolyF64.Shift(N: NativeInt): TPolyF64;
var res: TArray<Double>;
    I: NativeInt;
begin
  if N = 0 then exit(Clone);
  if N > 0 then begin
    SetLength(res, Order + N + 1);
    for I := 0 to Order do res[I + N] := Coeff[I];
  end else begin
    N := Abs(N);
    if N > Order then exit(PolyZero);
    SetLength(res, Order - N + 1);
    for I := N to Order do res[I - N] := Coeff[I];
  end;
  Result.Init(res);
end;

function TPolyF64.Substitute(const aQ: TPolyF64): TPolyF64;
var p: TPolyF64;
    I: NativeInt;
begin
  if aQ.Order = 0 then begin
    Result.Init([GetValue(aQ.fCoeffs[0])]); //aQ.Coeff[0] == aQ(0)
    exit;
  end;

  p := PolyOne;
  Result := PolyZero;
  for I := 0 to Order do begin
    Result := Result + fCoeffs[I] * p;
    p := p * aQ;
  end;
end;

function TPolyF64.Derivative: TPolyF64;
var res: TArray<Double>;
    k: NativeInt;
begin
  if Order = 0 then exit(PolyZero);

  SetLength(res, Order);
  for k := 1 to Length(res) do
    res[k - 1] := k * fCoeffs[k];
  Result.Init(res);
end;

function TPolyF64.Power(N: Cardinal): TPolyF64;
var q: TPolyF64;
begin
  Result := PolyOne;
  if n = 0 then exit;
  if n = 1 then exit(Clone);
  q := Self;
  while True do begin
    if (n and 1) <> 0 then Result := q * Result;
    n := n shr 1;
    if n = 0 then break;
    q := q * q;
  end;
end;

class function TPolyF64.GCD(const aP, aQ: TPolyF64; const aZeroTol: Double): TPolyF64;
var a, b, t: TPolyF64;
begin
  if aP.Order >= aQ.Order then begin
    a := aP;
    b := aQ;
  end else begin
    a := aQ;
    b := aP;
  end;

  while not b.IsZero(aZeroTol) do begin
    t := b;
    b := a mod b;
    a := t;
  end;
  Result := a / a.HighCoeff;
end;

{$endregion}

{$region 'TPolyC128'}

procedure TPolyC128.Init(const aCoeffs: array of TCmplx128);
var len: NativeInt;
begin
  len := Length(aCoeffs);
  Assert(aCoeffs[len - 1] <> 0);
  SetLength(fCoeffs, len);
  Move(aCoeffs[0], fCoeffs[0], len * SizeOf(TCmplx128));
end;

procedure TPolyC128.Init(const aCoeffs: TArray<TCmplx128>);
begin
  Assert(aCoeffs[High(aCoeffs)] <> 0);
  fCoeffs := aCoeffs;
end;

procedure TPolyC128.InitRealCoeffs(const aCoeffs: array of Double);
var I, count: NativeInt;
begin
  count := Length(aCoeffs);
  SetLength(fCoeffs, count);
  for I := 0 to count - 1 do
    fCoeffs[I].Init(aCoeffs[I], 0);
end;

function TPolyC128.Clone: TPolyC128;
begin
  Result.fCoeffs := Copy(fCoeffs, 0, Length(fCoeffs));
end;

function TPolyC128.GetHighCoeff: TCmplx128;
begin
  Result := fCoeffs[High(fCoeffs)];
end;

function TPolyC128.GetCoeff(I: NativeInt): TCmplx128;
begin
  if InRange(I, 0, High(fCoeffs)) then Result := fCoeffs[I]
  else Result := 0;
end;

procedure TPolyC128.SetCoeff(I: NativeInt; const aValue: TCmplx128);
begin
  if I > High(fCoeffs) then begin
    if aValue.IsZero then exit;
    SetLength(fCoeffs, I + 1);
  end;
  fCoeffs[I] := aValue;
end;

function TPolyC128.GetValue(const aX: TCmplx128): TCmplx128;
begin
  Result := aX;
  PolyEval(Result, PCmplx128(fCoeffs), Length(fCoeffs));
end;

class procedure TPolyC128.TrimZeros(var aValue: TArray<TCmplx128>; aEps: Double);
var n: NativeInt;
begin
  n := High(aValue);
  while (n > 0) and aValue[n].IsZero(aEps) do Dec(n);
  if n <> High(aValue) then SetLength(aValue, n + 1);
end;

class function TPolyC128.PolyAdd(const aP, aQ: TArray<TCmplx128>): TArray<TCmplx128>;
var I, J, lenP, lenQ: NativeInt;
    c: TArray<TCmplx128>;
begin
  lenP := Length(aP);
  lenQ := Length(aQ);
  SetLength(c, lenP + lenQ);
  if lenP > lenQ then begin
    SetLength(Result, lenP);
    for I := lenQ to lenP - 1 do
      Result[I] := aP[I];
    J := lenQ - 1;
  end else begin // lenP <= lenQ
    SetLength(Result, lenQ);
    for I := lenP to lenQ - 1 do
      Result[I] := aQ[I];
    J := lenP - 1;
  end;
  for I := 0 to J do
    Result[I] := aP[I] + aQ[I];
  TrimZeros(Result);
end;

class function TPolyC128.PolyAdd(const aP: TArray<TCmplx128>; const aV: TCmplx128): TArray<TCmplx128>;
begin
  Result := Copy(aP, 0, Length(aP));
  Result[0] := Result[0] + aV;
  TrimZeros(Result);
end;

class function TPolyC128.PolySub(const aP, aQ: TArray<TCmplx128>): TArray<TCmplx128>;
var I, J, lenP, lenQ: NativeInt;
    c: TArray<TCmplx128>;
begin
  lenP := Length(aP);
  lenQ := Length(aQ);
  SetLength(c, lenP + lenQ);
  if lenP > lenQ then begin
    SetLength(Result, lenP);
    for I := lenQ to lenP - 1 do
      Result[I] := aP[I];
    J := lenQ - 1;
  end else begin // lenP <= lenQ
    SetLength(Result, lenQ);
    for I := lenP to lenQ - 1 do
      Result[I] := -aQ[I];
    J := lenP - 1;
  end;
  for I := 0 to J do
    Result[I] := aP[I] - aQ[I];
  TrimZeros(Result);

end;

class function TPolyC128.PolyMul(const aP, aQ: TArray<TCmplx128>): TArray<TCmplx128>;
var lenP, lenQ, I, J, lo, hi: NativeInt;
    tmp: TCmplx128;
begin
  lenP := Length(aP);
  lenQ := Length(aQ);

  if lenP = 1 then begin
    Result := PolyMul(aQ, aP[0]);
    exit;
  end;

  if lenQ = 1 then begin
    Result := PolyMul(aP, aQ[0]);
    exit;
  end;

  SetLength(Result, lenP + lenQ - 1);
  for I := 0 to lenP + lenQ - 2 do begin
    tmp := 0;
    lo := Max(0, I - lenQ + 1);
    hi := Min(I, lenP - 1);
    for J := lo to hi do
      tmp := tmp + aP[J] * aQ[I - J];
    Result[I] := tmp;
  end;
end;

class function TPolyC128.PolyMul(const aP: TArray<TCmplx128>; const aV: TCmplx128): TArray<TCmplx128>;
var I: NativeInt;
begin
  SetLength(Result, Length(aP));
  for I := 0 to High(Result) do
    Result[I] := aP[I] * aV;
end;

class function TPolyC128.PolyNeg(const aP: TArray<TCmplx128>): TArray<TCmplx128>;
var I: NativeInt;
begin
  SetLength(Result, Length(aP));
  for I := 0 to High(Result) do
    Result[I] := -aP[I];
end;

class operator TPolyC128.Add(const aP, aQ: TPolyC128): TPolyC128;
begin
  Result.fCoeffs := PolyAdd(aP.fCoeffs, aQ.fCoeffs);
end;

class operator TPolyC128.Add(const aP: TPolyC128; const aV: TCmplx128): TPolyC128;
begin
  Result.fCoeffs := PolyAdd(aP.fCoeffs, aV);
end;

class operator TPolyC128.Subtract(const aP, aQ: TPolyC128): TPolyC128;
begin
  Result.fCoeffs := PolySub(aP.fCoeffs, aQ.fCoeffs);
end;

class operator TPolyC128.Subtract(const aP: TPolyC128; const aV: TCmplx128): TPolyC128;
begin
  Result.fCoeffs := PolyAdd(aP.fCoeffs, -aV);
end;

class operator TPolyC128.Multiply(const aP, aQ: TPolyC128): TPolyC128;
begin
  Result.fCoeffs := PolyMul(aP.fCoeffs, aQ.fCoeffs);
end;

class operator TPolyC128.Multiply(const aV: TCmplx128; const aP: TPolyC128): TPolyC128;
begin
  Result.fCoeffs := PolyMul(aP.fCoeffs, aV);
end;

class operator TPolyC128.IntDivide(const aP, aQ: TPolyC128): TPolyC128;
var r: TPolyC128;
begin
  GetQuotientRemainder(aP, aQ, Result, r);
end;

class operator TPolyC128.Modulus(const aP, aQ: TPolyC128): TPolyC128;
var q: TPolyC128;
begin
  GetQuotientRemainder(aP, aQ, q, Result);
end;

class operator TPolyC128.Divide(const aP: TPolyC128; const aV: TCmplx128): TPolyC128;
begin
  Result.fCoeffs := PolyMul(aP.fCoeffs, aV.Reciprocal);
end;

class operator TPolyC128.Negative(const aP: TPolyC128): TPolyC128;
begin
  Result.fCoeffs := PolyNeg(aP.fCoeffs);
end;

class operator TPolyC128.Explicit(const aV: TCmplx128): TPolyC128;
begin
  Result.Init([aV]);
end;

class operator TPolyC128.Explicit(const aV: Double): TPolyC128;
begin
  Result.Init([TCmplx128(aV)]);
end;

function TPolyC128.Order: NativeInt;
begin
  Result := High(fCoeffs);
end;

function TPolyC128.IsZero(const aEps: Double): Boolean;
var I, count: NativeInt;
begin
  count := Length(fCoeffs);
  for I := 0 to count - 1 do
    if not fCoeffs[I].IsZero(aEps) then exit(False);
  Result := True;
end;

class function TPolyC128.PolyZero: TPolyC128;
begin
  with Result do begin
    SetLength(fCoeffs, 1);
    fCoeffs[0].Init(0, 0);
  end;
end;

class function TPolyC128.PolyOne: TPolyC128;
begin
  with Result do begin
    SetLength(fCoeffs, 1);
    fCoeffs[0].Init(1, 0);
  end;
end;

class function TPolyC128.PolyConst(const aValue: TCmplx128): TPolyC128;
begin
  with Result do begin
    SetLength(fCoeffs, 1);
    fCoeffs[0] := aValue;
  end;
end;

class function TPolyC128.PolyConst(const aRe, aIm: Double): TPolyC128;
begin
  with Result do begin
    SetLength(fCoeffs, 1);
    fCoeffs[0].Init(aRe, aIm);
  end;
end;

class function TPolyC128.PolyX: TPolyC128;
begin
  with Result do begin
    SetLength(fCoeffs, 2);
    fCoeffs[0].Init(0, 0);
    fCoeffs[1].Init(1, 0);
  end;
end;

function TPolyC128.HasRealCoeffs(const aEps: Double): Boolean;
var I: NativeInt;
begin
  for I := 0 to High(fCoeffs) do
    if not System.Math.IsZero(fCoeffs[I].Im, aEps) then exit(False);
  Result := True;
end;

function TPolyC128.GetCoeffsRealPart: TArray<Double>;
var I: Integer;
begin
  SetLength(Result, Length(fCoeffs));
  for I := 0 to High(Result) do Result[I] := fCoeffs[I].Re;
end;

function TPolyC128.GetCoeffsImagPart: TArray<Double>;
var I: Integer;
begin
  SetLength(Result, Length(fCoeffs));
  for I := 0 to High(Result) do Result[I] := fCoeffs[I].Im;
end;

class procedure TPolyC128.GetQuotientRemainder(const aP, aQ: TPolyC128;
  out aQu, aRe: TPolyC128);
var M, N, I: NativeInt;
    p, q: TPolyC128;
    tmp, b0: TCmplx128;
    c: TArray<TCmplx128>;
begin
  M := aQ.Order;
  N := aP.Order;
  if M = 0 then begin
    aQu := aP / aQ.fCoeffs[0];
    aRe := PolyZero;
    exit;
  end;
  if M > N then begin
    aQu := PolyZero;
    aRe := aP.Clone;
    exit;
  end;
  p := aP;
  I := p.Order - M;
  SetLength(c, I + 1);
  b0 := aQ.fCoeffs[aQ.Order];
  while I >= 0 do begin
    tmp := p.fCoeffs[p.Order] / b0;
    q := (tmp * aQ).Shift(I);
    p := p - q;
    if p.Order = q.Order then // small number can remain in the highest coeff
      SetLength(p.fCoeffs, p.Order);
    c[I] := tmp;
    I := p.Order - M;
  end;
  aQu.Init(c);
  aRe := p;
end;

function TPolyC128.Shift(N: NativeInt): TPolyC128;
var res: TArray<TCmplx128>;
    I: NativeInt;
begin
  if N = 0 then exit(Clone);
  if N > 0 then begin
    SetLength(res, Order + N + 1);
    for I := 0 to Order do res[I + N] := Coeff[I];
  end else begin
    N := Abs(N);
    if N > Order then exit(PolyZero);
    SetLength(res, Order - N + 1);
    for I := N to Order do res[I - N] := Coeff[I];
  end;
  Result.Init(res);
end;

function TPolyC128.Substitute(const aQ: TPolyC128): TPolyC128;
var p: TPolyC128;
    I: NativeInt;
begin
  if aQ.Order = 0 then begin
    Result.Init([GetValue(aQ.fCoeffs[0])]); //aQ.Coeff[0] == aQ(0)
    exit;
  end;

  p := PolyOne;
  Result := PolyZero;
  for I := 0 to Order do begin
    Result := Result + fCoeffs[I] * p;
    p := p * aQ;
  end;
end;

function TPolyC128.Derivative: TPolyC128;
var res: TArray<TCmplx128>;
    k: NativeInt;
begin
  if Order = 0 then exit(PolyZero);

  SetLength(res, Order);
  for k := 1 to Length(res) do
    res[k - 1] := k * fCoeffs[k];
  Result.Init(res);
end;

function TPolyC128.Reciprocal: TPolyC128;
var coeffs: TArray<TCmplx128>;
    I, N: NativeInt;
begin
  N := Order;
  SetLength(coeffs, N + 1);
  for I := 0 to N do
    coeffs[I] := fCoeffs[N - I];
  Result.Init(coeffs);
end;

class function TPolyC128.GCD(const aP, aQ: TPolyC128; const aZeroTol: Double): TPolyC128;
var a, b, t: TPolyC128;
begin
  if aP.Order >= aQ.Order then begin
    a := aP;
    b := aQ;
  end else begin
    a := aQ;
    b := aP;
  end;

  while not b.IsZero(aZeroTol) do begin
    t := b;
    b := a mod b;
    a := t;
  end;
  Result := a / a.HighCoeff;
end;

{$endregion}

{$region 'TPolyC256'}

procedure TPolyC256.Init(const aCoeffs: array of TCmplx256);
var len: NativeInt;
begin
  len := Length(aCoeffs);
  Assert(not aCoeffs[len - 1].IsZero);
  SetLength(fCoeffs, len);
  Move(aCoeffs[0], fCoeffs[0], len * SizeOf(TCmplx256));
end;

procedure TPolyC256.Init(const aCoeffs: TArray<TCmplx256>);
begin
  Assert(not aCoeffs[High(aCoeffs)].IsZero);
  fCoeffs := aCoeffs;
end;

function TPolyC256.Clone: TPolyC256;
begin
  Result.fCoeffs := Copy(fCoeffs, 0, Length(fCoeffs));
end;

function TPolyC256.GetHighCoeff: TCmplx256;
begin
  Result := fCoeffs[High(fCoeffs)];
end;

function TPolyC256.GetCoeff(I: NativeInt): TCmplx256;
begin
  if InRange(I, 0, High(fCoeffs)) then Result := fCoeffs[I]
  else Result := cZeroC256;
end;

procedure TPolyC256.SetCoeff(I: NativeInt; const aValue: TCmplx256);
begin
  if I > High(fCoeffs) then begin
    if aValue.IsZero then exit;
    SetLength(fCoeffs, I + 1);
  end;
  fCoeffs[I] := aValue;
end;

function TPolyC256.GetValue(const aX: TCmplx256): TCmplx256;
begin

end;

class procedure TPolyC256.TrimZeros(var aValue: TArray<TCmplx256>; aEps: TReal128);
var n: NativeInt;
begin
  n := High(aValue);
  while (n > 0) and aValue[n].IsZero(aEps) do Dec(n);
  if n <> High(aValue) then SetLength(aValue, n + 1);
end;

class function TPolyC256.PolyAdd(const aP, aQ: TArray<TCmplx256>): TArray<TCmplx256>;
var I, J, lenP, lenQ: NativeInt;
    c: TArray<TCmplx256>;
begin
  lenP := Length(aP);
  lenQ := Length(aQ);
  SetLength(c, lenP + lenQ);
  if lenP > lenQ then begin
    SetLength(Result, lenP);
    for I := lenQ to lenP - 1 do
      Result[I] := aP[I];
    J := lenQ - 1;
  end else begin // lenP <= lenQ
    SetLength(Result, lenQ);
    for I := lenP to lenQ - 1 do
      Result[I] := aQ[I];
    J := lenP - 1;
  end;
  for I := 0 to J do
    Result[I] := aP[I] + aQ[I];
  TrimZeros(Result, cEpsF128);
end;

class function TPolyC256.PolyAdd(const aP: TArray<TCmplx256>; const aV: TCmplx256): TArray<TCmplx256>;
begin
  Result := Copy(aP, 0, Length(aP));
  Result[0] := Result[0] + aV;
  TrimZeros(Result, cEpsF128);
end;

class function TPolyC256.PolySub(const aP, aQ: TArray<TCmplx256>): TArray<TCmplx256>;
var I, J, lenP, lenQ: NativeInt;
    c: TArray<TCmplx256>;
begin
  lenP := Length(aP);
  lenQ := Length(aQ);
  SetLength(c, lenP + lenQ);
  if lenP > lenQ then begin
    SetLength(Result, lenP);
    for I := lenQ to lenP - 1 do
      Result[I] := aP[I];
    J := lenQ - 1;
  end else begin // lenP <= lenQ
    SetLength(Result, lenQ);
    for I := lenP to lenQ - 1 do
      Result[I] := -aQ[I];
    J := lenP - 1;
  end;
  for I := 0 to J do
    Result[I] := aP[I] - aQ[I];
  TrimZeros(Result, cEpsF128);
end;

class function TPolyC256.PolyMul(const aP, aQ: TArray<TCmplx256>): TArray<TCmplx256>;
var lenP, lenQ, I, J, lo, hi: NativeInt;
    tmp: TCmplx256;
begin
  lenP := Length(aP);
  lenQ := Length(aQ);

  if lenP = 1 then begin
    Result := PolyMul(aQ, aP[0]);
    exit;
  end;

  if lenQ = 1 then begin
    Result := PolyMul(aP, aQ[0]);
    exit;
  end;

  SetLength(Result, lenP + lenQ - 1);
  for I := 0 to lenP + lenQ - 2 do begin
    tmp := TCmplx256(0);
    lo := Max(0, I - lenQ + 1);
    hi := Min(I, lenP - 1);
    for J := lo to hi do
      tmp := tmp + aP[J] * aQ[I - J];
    Result[I] := tmp;
  end;
end;

class function TPolyC256.PolyMul(const aP: TArray<TCmplx256>; const aV: TCmplx256): TArray<TCmplx256>;
var I: NativeInt;
begin
  SetLength(Result, Length(aP));
  for I := 0 to High(Result) do
    Result[I] := aP[I] * aV;
end;

class function TPolyC256.PolyNeg(const aP: TArray<TCmplx256>): TArray<TCmplx256>;
var I: NativeInt;
begin
  SetLength(Result, Length(aP));
  for I := 0 to High(Result) do
    Result[I] := -aP[I];
end;

class operator TPolyC256.Add(const aP, aQ: TPolyC256): TPolyC256;
begin
  Result.fCoeffs := PolyAdd(aP.fCoeffs, aQ.fCoeffs);
end;

class operator TPolyC256.Add(const aP: TPolyC256; const aV: TCmplx256): TPolyC256;
begin
  Result.fCoeffs := PolyAdd(aP.fCoeffs, aV);
end;

class operator TPolyC256.Subtract(const aP, aQ: TPolyC256): TPolyC256;
begin
  Result.fCoeffs := PolySub(aP.fCoeffs, aQ.fCoeffs);
end;

class operator TPolyC256.Subtract(const aP: TPolyC256; const aV: TCmplx256): TPolyC256;
begin
  Result.fCoeffs := PolyAdd(aP.fCoeffs, -aV);
end;

class operator TPolyC256.Multiply(const aP, aQ: TPolyC256): TPolyC256;
begin
  Result.fCoeffs := PolyMul(aP.fCoeffs, aQ.fCoeffs);
end;

class operator TPolyC256.Multiply(const aV: TCmplx256; const aP: TPolyC256): TPolyC256;
begin
  Result.fCoeffs := PolyMul(aP.fCoeffs, aV);
end;

class operator TPolyC256.IntDivide(const aP, aQ: TPolyC256): TPolyC256;
var r: TPolyC256;
begin
  GetQuotientRemainder(aP, aQ, Result, r);
end;

class operator TPolyC256.Modulus(const aP, aQ: TPolyC256): TPolyC256;
var q: TPolyC256;
begin
  GetQuotientRemainder(aP, aQ, q, Result);
end;

class operator TPolyC256.Divide(const aP: TPolyC256; const aV: TCmplx256): TPolyC256;
begin
  Result.fCoeffs := PolyMul(aP.fCoeffs, aV.Reciprocal);
end;

class operator TPolyC256.Negative(const aP: TPolyC256): TPolyC256;
begin
  Result.fCoeffs := PolyNeg(aP.fCoeffs);
end;

class operator TPolyC256.Explicit(const aV: TCmplx256): TPolyC256;
begin
  Result.Init([aV]);
end;

class operator TPolyC256.Explicit(const aP: TPolyF64): TPolyC256;
var coeffs: TArray<TCmplx256>;
    I: NativeInt;
begin
  SetLength(coeffs, Length(aP.fCoeffs));
  for I := 0 to High(coeffs) do
    coeffs[I].Init(aP.fCoeffs[I], 0);
  Result.Init(coeffs);
end;

class operator TPolyC256.Explicit(const aP: TPolyC128): TPolyC256;
var coeffs: TArray<TCmplx256>;
    I: NativeInt;
begin
  SetLength(coeffs, Length(aP.fCoeffs));
  for I := 0 to High(coeffs) do with aP.fCoeffs[I] do
    coeffs[I].Init(TReal128(Re), TReal128(Im));
  Result.Init(coeffs);
end;

class operator TPolyC256.Explicit(const aP: TPolyC256): TPolyC128;
var coeffs: TArray<TCmplx128>;
    I: NativeInt;
begin
  SetLength(coeffs, Length(aP.fCoeffs));
  for I := 0 to High(coeffs) do with aP.fCoeffs[I] do
    coeffs[I].Init(Re.AsDouble, Im.AsDouble);
  Result.Init(coeffs);
end;

function TPolyC256.Order: NativeInt;
begin
  Result := High(fCoeffs);
end;

function TPolyC256.IsZero: Boolean;
begin
  Result := IsZero(cEpsF128);
end;

function TPolyC256.IsZero(const aEps: TReal128): Boolean;
var I, count: NativeInt;
begin
  count := Length(fCoeffs);
  for I := 0 to count - 1 do
    if not fCoeffs[I].IsZero(aEps) then exit(False);
  Result := True;
end;

class function TPolyC256.PolyZero: TPolyC256;
begin
  with Result do begin
    SetLength(fCoeffs, 1);
    fCoeffs[0].Init(0, 0);
  end;
end;

class function TPolyC256.PolyOne: TPolyC256;
begin
  with Result do begin
    SetLength(fCoeffs, 1);
    fCoeffs[0].Init(1, 0);
  end;
end;

class function TPolyC256.PolyConst(const aValue: TCmplx256): TPolyC256;
begin
  with Result do begin
    SetLength(fCoeffs, 1);
    fCoeffs[0] := aValue;
  end;
end;

class function TPolyC256.PolyConst(const aRe, aIm: TReal128): TPolyC256;
begin
  with Result do begin
    SetLength(fCoeffs, 1);
    fCoeffs[0].Init(aRe, aIm);
  end;
end;

class function TPolyC256.PolyX: TPolyC256;
begin
  with Result do begin
    SetLength(fCoeffs, 2);
    fCoeffs[0].Init(0, 0);
    fCoeffs[1].Init(1, 0);
  end;
end;

class procedure TPolyC256.GetQuotientRemainder(const aP, aQ: TPolyC256;
  out aQu, aRe: TPolyC256);
var M, N, I: NativeInt;
    p, q: TPolyC256;
    tmp, b0: TCmplx256;
    c: TArray<TCmplx256>;
begin
  M := aQ.Order;
  N := aP.Order;
  if M = 0 then begin
    aQu := aP / aQ.fCoeffs[0];
    aRe := PolyZero;
    exit;
  end;
  if M > N then begin
    aQu := PolyZero;
    aRe := aP.Clone;
    exit;
  end;
  p := aP;
  I := p.Order - M;
  SetLength(c, I + 1);
  b0 := aQ.fCoeffs[aQ.Order];
  while I >= 0 do begin
    tmp := p.fCoeffs[p.Order] / b0;
    q := (tmp * aQ).Shift(I);
    p := p - q;
    if p.Order = q.Order then // small number can remain in the highest coeff
      SetLength(p.fCoeffs, p.Order);
    c[I] := tmp;
    I := p.Order - M;
  end;
  aQu.Init(c);
  aRe := p;
end;

function TPolyC256.Shift(N: NativeInt): TPolyC256;
var res: TArray<TCmplx256>;
    I: NativeInt;
begin
  if N = 0 then exit(Clone);
  if N > 0 then begin
    SetLength(res, Order + N + 1);
    for I := 0 to Order do res[I + N] := Coeff[I];
  end else begin
    N := Abs(N);
    if N > Order then exit(PolyZero);
    SetLength(res, Order - N + 1);
    for I := N to Order do res[I - N] := Coeff[I];
  end;
  Result.Init(res);
end;

function TPolyC256.Substitute(const aQ: TPolyC256): TPolyC256;
var p: TPolyC256;
    I: NativeInt;
begin
  if aQ.Order = 0 then begin
    Result.Init([GetValue(aQ.fCoeffs[0])]); //aQ.Coeff[0] == aQ(0)
    exit;
  end;

  p := PolyOne;
  Result := PolyZero;
  for I := 0 to Order do begin
    Result := Result + fCoeffs[I] * p;
    p := p * aQ;
  end;
end;

function TPolyC256.Derivative: TPolyC256;
var res: TArray<TCmplx256>;
    k: NativeInt;
begin
  if Order = 0 then exit(PolyZero);

  SetLength(res, Order);
  for k := 1 to Length(res) do
    res[k - 1] := k * fCoeffs[k];
  Result.Init(res);
end;

function TPolyC256.Reciprocal: TPolyC256;
var coeffs: TArray<TCmplx256>;
    I, N: NativeInt;
begin
  N := Order;
  SetLength(coeffs, N + 1);
  for I := 0 to N do
    coeffs[I] := fCoeffs[N - I];
  Result.Init(coeffs);
end;

class function TPolyC256.GCD(const aP, aQ: TPolyC256): TPolyC256;
begin
  Result := GCD(aP, aQ, cEpsF128);
end;

class function TPolyC256.GCD(const aP, aQ: TPolyC256; const aZeroTol: TReal128): TPolyC256;
var a, b, t: TPolyC256;
begin
  if aP.Order >= aQ.Order then begin
    a := aP;
    b := aQ;
  end else begin
    a := aQ;
    b := aP;
  end;

  while not b.IsZero(aZeroTol) do begin
    t := b;
    b := a mod b;
    a := t;
  end;
  Result := a / a.HighCoeff;
end;

{$endregion}

end.
