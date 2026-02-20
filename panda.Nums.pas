unit panda.Nums;

interface

uses
    System.Math
  , System.SysUtils
  ;

{$I AsmDefs.inc}

type
  PUInt8 = ^UInt8;
  PUInt16 = ^UInt16;

  TCmplx64 = record
    Re, Im: Single;
    procedure Init(aRe, aIm: Single); inline;
    class operator Implicit(const A: Single): TCmplx64; inline;
    class operator Add(const A, B: TCmplx64): TCmplx64;{$ifndef ASMx64}inline;{$endif}
    class operator Subtract(const A, B: TCmplx64): TCmplx64;{$ifndef ASMx64}inline;{$endif}
    class operator Negative(const A: TCmplx64): TCmplx64;{$ifndef ASMx64}inline;{$endif}
    class operator Multiply(const A, B: TCmplx64): TCmplx64;{$ifdef NoASM}inline;{$endif}
    class operator Divide(const A, B: TCmplx64): TCmplx64;{$ifdef NoASM}inline;{$endif}
    class operator Equal(const A, B: TCmplx64): Boolean; inline;
    class operator NotEqual(const A, B: TCmplx64): Boolean; inline;
  end;
  PCmplx64 = ^TCmplx64;

  TCmplx128 = record
    Re, Im: Double;
    procedure Init(const aRe, aIm: Double); inline;
    class operator Implicit(const A: Double): TCmplx128; inline;
    class operator Explicit(const A: TCmplx64): TCmplx128; inline;
    class operator Explicit(const A: TCmplx128): TCmplx64; inline;
    function Abs: Double; inline;
    function Arg: Double; inline;
    function Conjugate: TCmplx128; inline;
    function Reciprocal: TCmplx128; inline;
    function IsZero(const aEpsilon: Double = 0): Boolean; inline;
    function IsExactZero: Boolean; inline;
    class function Zero: TCmplx128; static;
    class function ImUnit: TCmplx128; static;
    class function ReUnit: TCmplx128; static;
    class operator Add(const A, B: TCmplx128): TCmplx128; inline;
    class operator Add(const A: Double; const B: TCmplx128): TCmplx128; inline;
    class operator Subtract(const A, B: TCmplx128): TCmplx128; inline;
    class operator Negative(const A: TCmplx128): TCmplx128; inline;
    class operator Multiply(const A, B: TCmplx128): TCmplx128; inline;
    class operator Multiply(const A: Double; const B: TCmplx128): TCmplx128; inline;
    class operator Divide(const A, B: TCmplx128): TCmplx128;{$ifdef NoASM}inline;{$endif}
    class operator Divide(const A: TCmplx128; const B: Double): TCmplx128; inline;
    class operator Equal(const A, B: TCmplx128): Boolean; inline;
    class operator NotEqual(const A, B: TCmplx128): Boolean; inline;
  end;
  PCmplx128 = ^TCmplx128;

const
  cI32Sz  = SizeOf(Integer);
  cI64Sz  = SizeOf(Int64);
  cNISz   = SizeOf(NativeInt);
  cF32Sz  = SizeOf(Single);
  cF64Sz  = SizeOf(Double);
  cC64Sz  = SizeOf(TCmplx64);
  cC128Sz = SizeOf(TCmplx128);

  MAX_UINT8   = 255;
  MAX_UINT16  = 65535;
  MAX_NAT_INT = High(NativeInt);

const
  cEpsF32 = 2.3841857910156250e-7;  // Power(2, -22)
  cEpsF64 = 2.2204460492503131e-16; // Power(2, -52)
  
  cAbsMaskF32: UInt64 = $7FFFFFFF;
  cAbsMaskF64: UInt64 = $7FFFFFFFFFFFFFFF;
  cSgnMaskF32: UInt64 = $80000000;
  cSgnMaskF64: UInt64 = $8000000000000000;

  cZeroC64:  TCmplx64  = (Re: 0; Im: 0);
  cZeroC128: TCmplx128 = (Re: 0; Im: 0);

  function Cmplx(const aRe, aIm: Double): TCmplx128; inline;

implementation

function Cmplx(const aRe, aIm: Double): TCmplx128;
begin
  Result.Re := aRe;
  Result.Im := aIm;
end;

{$region 'TCmplx64'}

procedure TCmplx64.Init(aRe, aIm: Single);
begin
  Re := aRe;
  Im := aIm;
end;

class operator TCmplx64.Implicit(const A: Single): TCmplx64;
begin
  Result.Re := A;
  Result.Im := 0;
end;

class operator TCmplx64.Add(const A, B: TCmplx64): TCmplx64;
{$if defined(ASMx64)}
asm
  movq xmm0, [rcx]
  movq xmm1, [rdx]
  addps xmm0, xmm1
  movq rax, xmm0
end;
{$else}
begin
  Result.Re := A.Re + B.Re;
  Result.Im := A.Im + B.Im;
end;
{$endif}

class operator TCmplx64.Subtract(const A, B: TCmplx64): TCmplx64;
{$if defined(ASMx64)}
asm
  movq xmm0, [rcx]
  movq xmm1, [rdx]
  subps xmm0, xmm1
  movq rax, xmm0
end;
{$else}
begin
  Result.Re := A.Re - B.Re;
  Result.Im := A.Im - B.Im;
end;
{$endif}

class operator TCmplx64.Negative(const A: TCmplx64): TCmplx64;
{$if defined(ASMx64)}
asm
  xorpd xmm0, xmm0
  movq xmm1, [rcx]
  subps xmm0, xmm1
  movq rax, xmm0
end;
{$else}
begin
  Result.Re := -A.Re;
  Result.Im := -A.Im;
end;
{$endif}

class operator TCmplx64.Multiply(const A, B: TCmplx64): TCmplx64;
{$if defined(ASMx86)}
// EAX <- @A, ECX <- @Result, EDX <- @B
asm
  movq xmm0, [eax]  // xmm0 <- (ar, ai)
  movsldup xmm2, xmm0 // xmm2 <- (ar, ar)
  movshdup xmm3, xmm0 // xmm3 <- (ai, ai)
  movq xmm1, [edx]    // xmm1 <- (br, bi)
  mulps xmm2, xmm1   // xmm2 <- (br*ar, bi*ar)
  mulps xmm3, xmm1   // xmm3 <- (br*ai, bi*ai)
  pshufd xmm3, xmm3, $1 // xmm3 <- (bi*ai, br*ai)
  addsubps xmm2, xmm3 // xmm2 <- a * b
  movq [ecx], xmm2
end;
{$elseif defined(ASMx64)}
asm
  movq xmm0, [rcx]  // xmm0 <- (ar, ai)
  movsldup xmm2, xmm0 // xmm2 <- (ar, ar)
  movshdup xmm3, xmm0 // xmm3 <- (ai, ai)
  movq xmm1, [rdx]    // xmm1 <- (br, bi)
  mulps xmm2, xmm1   // xmm2 <- (br*ar, bi*ar)
  mulps xmm3, xmm1   // xmm3 <- (br*ai, bi*ai)
  pshufd xmm3, xmm3, $1 // xmm3 <- (bi*ai, br*ai)
  addsubps xmm2, xmm3 // xmm2 <- a * b
  movq rax, xmm2
end;
{$else}
begin
  Result.Re := A.Re * B.Re - A.Im * B.Im;
  Result.Im := A.Im * B.Re + A.Re * B.Im;
end;
{$endif}

class operator TCmplx64.Divide(const A, B: TCmplx64): TCmplx64;
{$if defined(ASMx86)}
// EAX <- @A, ECX <- @Result, EDX <- @B
asm
  movq xmm2, [eax]
  movshdup xmm0, xmm2    // xmm0 <- (ai, ai)
  movsldup xmm2, xmm2    // xmm2 <- (ar, ar)
  movq xmm1, [edx]       // xmm1 <- (br, bi)
  mulps xmm0, xmm1       // xmm0 <- (ai*br, ai*bi)
  pshufd xmm1, xmm1, $1  // xmm1 <- (bi, br)
  mulps xmm2, xmm1       // xmm2 <- (ar*bi, ar*br)
  addsubps xmm0, xmm2    // xmm0 <- (ai*br-ar*bi, ar*br+ai*bi)
  pshufd xmm0, xmm0, $1  // xmm0 <- (ar*br+ai*bi, ai*br-ar*bi)

  mulps xmm1, xmm1       // xmm1 <- (bi^2, br^2)
  pshufd xmm2, xmm1, $1  // xmm2 <- (br^2, bi^2)
  addps xmm1, xmm2       // xmm1 <- (b^2, b^2)
  divps xmm0, xmm1       // xmm0 <- xmm3 / b^2
  movq [ecx], xmm0
end;
{$elseif defined(ASMx64)}
// RCX <- @A, RDX <- @A, RAX <- Result
asm
  movq xmm2, [rcx]
  movshdup xmm0, xmm2    // xmm0 <- (ai, ai)
  movsldup xmm2, xmm2    // xmm2 <- (ar, ar)
  movq xmm1, [rdx]       // xmm1 <- (br, bi)
  mulps xmm0, xmm1       // xmm0 <- (ai*br, ai*bi)
  pshufd xmm1, xmm1, $1  // xmm1 <- (bi, br)
  mulps xmm2, xmm1       // xmm2 <- (ar*bi, ar*br)
  addsubps xmm0, xmm2    // xmm0 <- (ai*br-ar*bi, ar*br+ai*bi)
  pshufd xmm0, xmm0, $1  // xmm0 <- (ar*br+ai*bi, ai*br-ar*bi)

  mulps xmm1, xmm1       // xmm1 <- (bi^2, br^2)
  pshufd xmm2, xmm1, $1  // xmm2 <- (br^2, bi^2)
  addps xmm1, xmm2       // xmm1 <- (b^2, b^2)
  divps xmm0, xmm1       // xmm0 <- xmm3 / b^2
  movq rax, xmm0
end;
{$else}
var absB: Double;
begin
  absB := B.Re * B.Re + B.Im * B.Im;
  Result.Re := (A.Re * B.Re + A.Im * B.Im) / absB;
  Result.Im := (A.Im * B.Re - A.Re * B.Im) / absB;
end;
{$endif}

class operator TCmplx64.Equal(const A, B: TCmplx64): Boolean;
begin
  Result := System.Math.IsZero(A.Re - B.Re) and System.Math.IsZero(A.Im - B.Im);
end;

class operator TCmplx64.NotEqual(const A, B: TCmplx64): Boolean;
begin
  Result := not (System.Math.IsZero(A.Re - B.Re) and System.Math.IsZero(A.Im - B.Im));
end;

{$endregion}

{$region 'TCmplx128'}

procedure TCmplx128.Init(const aRe, aIm: Double);
begin
  Re := aRe;
  Im := aIm;
end;

class operator TCmplx128.Implicit(const A: Double): TCmplx128;
begin
  Result.Re := A;
  Result.Im := 0;
end;

class operator TCmplx128.Explicit(const A: TCmplx64): TCmplx128;
begin
  Result.Re := A.Re;
  Result.Im := A.Im;
end;

class operator TCmplx128.Explicit(const A: TCmplx128): TCmplx64;
begin
  Result.Re := A.Re;
  Result.Im := A.Im;
end;

function TCmplx128.Abs: Double;
begin
  Result := Sqrt(Re * Re + Im * Im);
end;

function TCmplx128.Arg: Double;
begin
  Result := ArcTan2(Im, Re)
end;

function TCmplx128.Conjugate: TCmplx128;
begin
  Result.Re := Re;
  Result.Im := -Im;
end;

function TCmplx128.Reciprocal: TCmplx128;
var r: Double;
begin
  r := Re * Re + Im * Im;
  Result.Re := Re / r;
  Result.Im := -Im / r;
end;

function TCmplx128.IsZero(const aEpsilon: Double): Boolean;
begin
  Result := System.Math.IsZero(Self.Re, aEpsilon) and System.Math.IsZero(Self.Im, aEpsilon);
end;

function TCmplx128.IsExactZero: Boolean;
begin
  Result := (Re = 0) and (Im = 0);
end;

class function TCmplx128.Zero: TCmplx128;
begin
  with Result do begin
    Re := 0;
    Im := 0;
  end;
end;

class function TCmplx128.ImUnit: TCmplx128;
begin
  with Result do begin
    Re := 0;
    Im := 1;
  end;
end;

class function TCmplx128.ReUnit: TCmplx128;
begin
  with Result do begin
    Re := 1;
    Im := 0;
  end;
end;

class operator TCmplx128.Add(const A, B: TCmplx128): TCmplx128;
begin
  Result.Re := A.Re + B.Re;
  Result.Im := A.Im + B.Im;
end;

class operator TCmplx128.Add(const A: Double; const B: TCmplx128): TCmplx128;
begin
  Result.Re := A + B.Re;
  Result.Im := B.Im;
end;

class operator TCmplx128.Subtract(const A, B: TCmplx128): TCmplx128;
begin
  Result.Re := A.Re - B.Re;
  Result.Im := A.Im - B.Im;
end;

class operator TCmplx128.Negative(const A: TCmplx128): TCmplx128;
begin
  Result.Re := -A.Re;
  Result.Im := -A.Im;
end;

class operator TCmplx128.Multiply(const A, B: TCmplx128): TCmplx128;
begin
  Result.Re := A.Re * B.Re - A.Im * B.Im;
  Result.Im := A.Im * B.Re + A.Re * B.Im;
end;

class operator TCmplx128.Multiply(const A: Double; const B: TCmplx128): TCmplx128;
begin
  Result.Re := A * B.Re;
  Result.Im := A * B.Im;
end;

class operator TCmplx128.Divide(const A, B: TCmplx128): TCmplx128;
{$if defined(ASMx86)}
// EAX <- @A, ECX <- @Result, EDX <- @B
asm
  movddup xmm2, [eax]       // xmm2 <- (ar, ar)
  movddup xmm0, [eax + 8]   // xmm0 <- (ai, ai)
  movupd xmm1, [edx]        // xmm1 <- (br, bi)
  mulpd xmm0, xmm1          // xmm0 <- (ai*br, ai*bi)
  pshufd xmm1, xmm1, $4e    // xmm1 <- (bi, br)
  mulpd xmm2, xmm1          // xmm2 <- (ar*bi, ar*br)
  addsubpd xmm0, xmm2       // xmm0 <- (ai*br-ar*bi, ar*br+ai*bi)
  pshufd xmm0, xmm0, $4e    // xmm0 <- (ar*br+ai*bi, ai*br-ar*bi)

  mulpd xmm1, xmm1
  movhlps xmm2, xmm1
  addsd xmm1, xmm2
  movlhps xmm1, xmm1
  divpd xmm0, xmm1          // xmm3 <- xmm0 / b^2
  movupd [ecx], xmm0
end;
{$elseif defined(ASMx64)}
// RCX <- @Result, RDX <- @A, R8 <- @B
asm
  movddup xmm2, [rdx]       // xmm2 <- (ar, ar)
  movddup xmm0, [rdx + 8]   // xmm0 <- (ai, ai)
  movupd xmm1, [r8]         // xmm1 <- (br, bi)
  mulpd xmm0, xmm1          // xmm0 <- (ai*br, ai*bi)
  pshufd xmm1, xmm1, $4e    // xmm1 <- (bi, br)
  mulpd xmm2, xmm1          // xmm2 <- (ar*bi, ar*br)
  addsubpd xmm0, xmm2       // xmm0 <- (ai*br-ar*bi, ar*br+ai*bi)
  pshufd xmm0, xmm0, $4e    // xmm0 <- (ar*br+ai*bi, ai*br-ar*bi)

  mulpd xmm1, xmm1
  movhlps xmm2, xmm1
  addsd xmm1, xmm2
  movlhps xmm1, xmm1
  divpd xmm0, xmm1          // xmm0 <- xmm3 / b^2
  movupd [rcx], xmm0
end;
{$else}
var absB: Double;
begin
  absB := B.Re * B.Re + B.Im * B.Im;
  Result.Re := (A.Re * B.Re + A.Im * B.Im) / absB;
  Result.Im := (A.Im * B.Re - A.Re * B.Im) / absB;
end;
{$endif}

class operator TCmplx128.Divide(const A: TCmplx128; const B: Double): TCmplx128;
begin
  Result.Re := A.Re / B;
  Result.Im := A.Im / B;
end;

class operator TCmplx128.Equal(const A, B: TCmplx128): Boolean;
begin
  Result := System.Math.IsZero(A.Re - B.Re) and System.Math.IsZero(A.Im - B.Im);
end;

class operator TCmplx128.NotEqual(const A, B: TCmplx128): Boolean;
begin
  Result := not (System.Math.IsZero(A.Re - B.Re) and System.Math.IsZero(A.Im - B.Im));
end;

{$endregion}

end.
