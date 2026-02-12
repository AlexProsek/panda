unit panda.cvArithmetic;

interface

uses
    panda.Nums
  ;

//{$define NoASM}

{$I AsmDefs.inc}

procedure VecAdd(pA, pB, pRes: PSingle; aCount: NativeInt); overload;
procedure VecAdd(pA, pB, pRes: PDouble; aCount: NativeInt); overload;
procedure VecAdd(pA: PSingle; B: Single; pRes: PSingle; aCount: NativeInt); overload;
procedure VecAdd(pA: PDouble; const B: Double; pRes: PDouble; aCount: NativeInt); overload;
procedure VecAdd(pA: PCmplx128; const B: TCmplx128; pRes: PCmplx128; aCount: NativeInt); overload;

// pA^ <- pA^ + pB^
procedure VecAddInPlace(pA, pB: PDouble; aAStep, aBStep, aCount: NativeInt); overload;
procedure VecAddInPlace(pA, pB: PSingle; aAStep, aBStep, aCount: NativeInt); overload;

procedure VecSub(pA, pB, pRes: PDouble; aCount: NativeInt); overload;
procedure VecSub(pA, pB, pRes: PSingle; aCount: NativeInt); overload;
procedure VecSub(pA: PDouble; const B: Double; pRes: PDouble; aCount: NativeInt); overload;
procedure VecSub(pA: PSingle; B: Single; pRes: PSingle; aCount: NativeInt); overload;
procedure VecSub(A: Single; pB: PSingle; pRes: PSingle; aCount: NativeInt); overload;

// pA^ <- pA^ - pB^
procedure VecSubInPlace(pA, pB: PDouble; aAStep, aBStep, aCount: NativeInt); overload;
procedure VecSubInPlace(pA, pB: PSingle; aAStep, aBStep, aCount: NativeInt); overload;

procedure VecNeg(pA, pRes: PDouble; aCount: NativeInt); overload;
procedure VecNeg(pA, pRes: PSingle; aCount: NativeInt); overload;

procedure VecMul(pA, pB, pRes: PSingle; aCount: NativeInt); overload;
procedure VecMul(pA, pB, pRes: PDouble; aCount: NativeInt); overload;
procedure VecMul(pA, pB, pRes: PCmplx128; aCount: NativeInt); overload;
procedure VecMul(pA: PSingle; B: Single; pRes: PSingle; aCount: NativeInt); overload;
procedure VecMul(pA: PDouble; const B: Double; pRes: PDouble; aCount: NativeInt); overload;
procedure VecMul(pA: PCmplx128; const B: TCmplx128; pRes: PCmplx128; aCount: NativeInt); overload;

procedure VecDiv(pA, pB, pRes: PCmplx128; aCount: NativeInt); overload;

procedure VecAddWithSat(pA, pB, pRes: PUInt8; aCount: NativeInt); overload;
procedure VecAddWithSat(pA, pB, pRes: PUInt16; aCount: NativeInt); overload;
procedure VecAddWithSat(pA: PUInt8; B: UInt8; pRes: PUInt8; aCount: NativeInt); overload;
procedure VecAddWithSat(pA: PUInt16; B: UInt16; pRes: PUInt16; aCount: NativeInt); overload;

procedure VecSubWithSat(pA, pB, pRes: PUInt8; aCount: NativeInt); overload;
procedure VecSubWithSat(pA, pB, pRes: PUInt16; aCount: NativeInt); overload;
procedure VecSubWithSat(pA: PUInt8; B: UInt8; pRes: PUInt8; aCount: NativeInt); overload;
procedure VecSubWithSat(pA: PUInt16; B: UInt16; pRes: PUInt16; aCount: NativeInt); overload;

procedure VecAnd(pA, pB, pRes: PByte; aCount: NativeInt); overload;
procedure VecOr(pA, pB, pRes: PByte; aCount: NativeInt); overload;
procedure VecNot(pA, pRes: PByte; aCount: NativeInt); overload;

function SatAdd_U8(a, b: UInt8): UInt8; inline;
function SatSub_U8(a, b: UInt8): UInt8; inline;

function Min_2U8(a, b: UInt8): UInt8; inline;
function Min_3U8(a, b, c: UInt8): UInt8; inline;
function Min_4U8(a, b, c, d: UInt8): UInt8; inline;
function Min_NU8(pA: PUInt8; N: NativeInt): UInt8;

function Max_2U8(a, b: UInt8): UInt8; inline;

// X <- a * X
procedure sscal(pX: PSingle; aCount: NativeInt; a: Single);
procedure dscal(pX: PDouble; aCount: NativeInt; a: Double);

// Y <- X
procedure scopy(pX, pY: PSingle; aCount, XStep, YStep: NativeInt);
procedure dcopy(pX, pY: PDouble; aCount, XStep, YStep: NativeInt);

/// <remarks> <c> Y <- a * X + Y </c> </remarks>
procedure axpy(const a: Double; pX, pY: PDouble; aCount: NativeInt); overload;
procedure axpy(const a: Single; pX, pY: PSingle; aCount: NativeInt); overload;
procedure axpy(const a: TCmplx128; pX, pY: PCmplx128; aCount: NativeInt); overload;

procedure dot(pX, pY: PSingle; aCount: NativeInt; out aRes: Single); overload;
procedure dot(pX, pY: PDouble; aCount: NativeInt; out aRes: Double); overload;
procedure dot(pX, pY: PCmplx128; aCount: NativeInt; out aRes: TCmplx128); overload;

function sdot(aN: NativeInt; pX: PByte; aXInc: NativeInt; pY: PByte; aYInc: NativeInt): Single;
function ddot(aN: NativeInt; pX: PByte; aXInc: NativeInt; pY: PByte; aYInc: NativeInt): Double;

procedure Differences(pX, pRes: PDouble; aCount: NativeInt); overload;
procedure Differences(pX, pRes: PSingle; aCount: NativeInt); overload;
procedure Differences(pX, pRes: PDouble; aXStep, aResStep, aCount: NativeInt); overload;
procedure Differences(pX, pRes: PSingle; aXStep, aResStep, aCount: NativeInt); overload;

function SquaredEuclideanDistance(pX, pY: PDouble; aCount: NativeInt): Double; overload;
function SquaredEuclideanDistance(pX, pY: PSingle; aCount: NativeInt): Single; overload;
function Norm(pX: PDouble; aCount: NativeInt; p: Double = 2): Double; overload;
function Norm(const X: TArray<Double>; p: Double = 2): Double; overload;
function Norm(pX: PSingle; aCount: NativeInt; p: Single = 2): Single; overload;
function Norm(const X: TArray<Single>; p: Single = 2): Single; overload;

var
  cUnrollThreshold: NativeInt = 10000;

implementation

uses
    Math
  , SysUtils
  ;

procedure RaiseLengthOutOfRange;
begin
  raise EArgumentOutOfRangeException.Create('Length is less than zero');
end;

procedure _dup_I8_xmm0(value: UInt8);
asm
  mov dl, al
  shl edx, 8
  mov dl, al
  mov ax, dx
  shl eax, 16
  mov ax, dx
  mov [esp - 4], eax
  mov [esp - 8], eax
  movddup xmm0, [esp - 8]
end;

procedure _dup_I8_xmm1(value: UInt8);
asm
  mov dl, al
  shl edx, 8
  mov dl, al
  mov ax, dx
  shl eax, 16
  mov ax, dx
  mov [esp - 4], eax
  mov [esp - 8], eax
  movddup xmm1, [esp - 8]
end;

procedure _dup_I8_xmm2(value: UInt8);
asm
  mov dl, al
  shl edx, 8
  mov dl, al
  mov ax, dx
  shl eax, 16
  mov ax, dx
  mov [esp - 4], eax
  mov [esp - 8], eax
  movddup xmm2, [esp - 8]
end;

procedure _dup_I16_xmm0(value: UInt16);
asm
  mov dx, ax
  shl eax, 16
  mov ax, dx
  mov [esp - 4], eax
  mov [esp - 8], eax
  movddup xmm0, [esp - 8]
end;

procedure _dup_I16_xmm1(value: UInt16);
asm
  mov dx, ax
  shl eax, 16
  mov ax, dx
  mov [esp - 4], eax
  mov [esp - 8], eax
  movddup xmm1, [esp - 8]
end;

procedure VecAdd(pA, pB, pRes: PSingle; aCount: NativeInt);
{$if defined(ASMx86)}
asm
  //EAX <- pA, EDX <- pB, ECX <- pRes, [EBP + 8] <- aCount
  push edi
  mov edi, ecx
  mov ecx, [ebp + 8]
  shr ecx, 2
  jz @rest
@mainLoop:
  movups xmm0, [eax]
  movups xmm1, [edx]
  addps xmm0, xmm1
  movups [edi], xmm0
  add eax, 16
  add edx, 16
  add edi, 16
  dec ecx
  jnz @mainLoop

@rest:
  mov ecx, [ebp + 8]
  and ecx, 3
  jz @end
@restLoop:
  movd xmm0, [eax]
  movd xmm1, [edx]
  addss xmm0, xmm1
  movd [edi], xmm0
  add eax, 4
  add edx, 4
  add edi, 4
  dec ecx
  jnz @restLoop
@end:
  pop edi
end;
{$elseif defined(ASMx64)}
asm
  //RCX <- pA, RDX <- pB, R8 <- pRes, R9 <- aCount
  mov rax, rcx // RAX <- pA
  mov rcx, r9
  shr rcx, 2
  jz @rest
@mainLoop:
  movups xmm0, [rax]
  movups xmm1, [rdx]
  addps xmm0, xmm1
  movups [r8], xmm0
  add rax, 16
  add rdx, 16
  add r8, 16
  dec rcx
  jnz @mainLoop

@rest:
  mov rcx, r9
  and rcx, 3
  jz @end
@restLoop:
  movd xmm0, [rax]
  movd xmm1, [rdx]
  addss xmm0, xmm1
  movd [r8], xmm0
  add rax, 4
  add rdx, 4
  add r8, 4
  dec rcx
  jnz @restLoop
@end:
end;
{$else}
var pEnd: PByte;
begin
  pEnd := PByte(pA) + aCount * SizeOf(Single);
  while PByte(pA) < pEnd do begin
    pRes^  := pA^ + pB^;
    Inc(pRes);
    Inc(pA);
    Inc(pB);
  end;
end;
{$endif}

procedure VecAdd(pA, pB, pRes: PDouble; aCount: NativeInt);
{$if defined(ASMx86)}
asm
  // EAX <- pA, EDX <- pB, ECX <- pRes, [EBP + 8] <- aCount
  push edi
  mov edi, ecx
  mov ecx, [ebp + 8]
  shr ecx, 1
  jz @rest
@Loop:
  movupd xmm0, [eax]
  movupd xmm1, [edx]
  addpd xmm0, xmm1
  movupd [edi], xmm0
  add eax, 16
  add edx, 16
  add edi, 16
  dec ecx
  jnz @Loop

@rest:
  mov ecx, [ebp + 8]
  and ecx, 1
  jz @end
  movq xmm0, [eax]
  movq xmm1, [edx]
  addsd xmm0, xmm1
  movq [edi], xmm0
@end:
  pop edi
end;
{$elseif defined(ASMx64)}
asm
  //RCX <- pA, RDX <- pB, R8 <- pRes, R9 <- aCount
  mov rax, rcx // RAX <- pA
  mov rcx, r9
  shr rcx, 1
  jz @rest
@Loop:
  movupd xmm0, [rax]
  movupd xmm1, [rdx]
  addpd xmm0, xmm1
  movupd [r8], xmm0
  add rax, 16
  add rdx, 16
  add r8, 16
  dec rcx
  jnz @Loop

@rest:
  mov rcx, r9
  and rcx, 1
  jz @end
  movq xmm0, [rax]
  movq xmm1, [rdx]
  addsd xmm0, xmm1
  movq [r8], xmm0
@end:
end;
{$else}
var pEnd: PByte;
begin
  pEnd := PByte(pA) + aCount * SizeOf(Double);
  while PByte(pA) < pEnd do begin
    pRes^  := pA^ + pB^;
    Inc(pRes);
    Inc(pA);
    Inc(pB);
  end;
end;
{$endif}

procedure VecAdd(pA: PSingle; B: Single; pRes: PSingle; aCount: NativeInt);
{$if defined(ASMx86)}
asm
  //EAX <- pA, EDX <- pRes, ECX <- aCount, [EBP + 8] <- B
  movd xmm1, [ebp + 8]
  movss xmm0, xmm1
  shufps xmm1, xmm0, 0  //xmm1 <- 4 x B
  mov [ebp + 8], ecx
  shr ecx, 2
  jz @rest
@mainLoop:
  movups xmm0, [eax]
  addps xmm0, xmm1
  movups [edx], xmm0
  add eax, 16
  add edx, 16
  dec ecx
  jnz @mainLoop

@rest:
  mov ecx, [ebp + 8]
  and ecx, 3
  jz @end
@restLoop:
  movd xmm0, [eax]
  addss xmm0, xmm1
  movd [edx], xmm0
  add eax, 4
  add edx, 4
  dec ecx
  jnz @restLoop
@end:
end;
{$elseif defined(ASMx64)}
asm
  //RCX <- pA, XMM1 <- B, R8 <- pRes, R9 <- aCount
  movss xmm0, xmm1
  shufps xmm1, xmm0, 0  //xmm1 <- 4 x B
  mov rax, rcx // RAX <- pA
  mov rdx, r8  // RDX <- pRes
  mov rcx, r9
  shr rcx, 2
  jz @rest
@mainLoop:
  movups xmm0, [rax]
  addps xmm0, xmm1
  movups [rdx], xmm0
  add rax, 16
  add rdx, 16
  dec rcx
  jnz @mainLoop

@rest:
  mov rcx, r9
  and rcx, 3
  jz @end
@restLoop:
  movd xmm0, [rax]
  addss xmm0, xmm1
  movd [rdx], xmm0
  add rax, 4
  add rdx, 4
  dec rcx
  jnz @restLoop
@end:
end;
{$else}
var pEnd: PByte;
begin
  pEnd := PByte(pA) + aCount * SizeOf(Single);
  while PByte(pA) < pEnd do begin
    pRes^  := pA^ + B;
    Inc(pRes);
    Inc(pA);
  end;
end;
{$endif}

procedure VecAdd(pA: PDouble; const B: Double; pRes: PDouble; aCount: NativeInt);
{$if defined(ASMx86)}
asm
  //EAX <- pA, EDX <- pRes, ECX <- aCount, [EBP + 8] <- B
  movddup xmm1, [ebp + 8]
  mov [ebp + 8], ecx
  shr ecx, 1
  jz @rest
@Loop:
  movupd xmm0, [eax]
  addpd xmm0, xmm1
  movupd [edx], xmm0
  add eax, 16
  add edx, 16
  dec ecx
  jnz @Loop

@rest:
  mov ecx, [ebp + 8]
  and ecx, 1
  jz @end
  movq xmm0, [eax]
  addsd xmm0, xmm1
  movq [edx], xmm0
@end:
end;
{$elseif defined(ASMx64)}
asm
  //RCX <- pA, XMM1 <- B, R8 <- pRes, R9 <- aCount
  movddup xmm1, xmm1
  mov rax, rcx // RAX <- pA
  mov rdx, r8  // RDX <- pRes
  mov rcx, r9
  shr rcx, 1
  jz @rest
@Loop:
  movupd xmm0, [rax]
  addpd xmm0, xmm1
  movupd [rdx], xmm0
  add rax, 16
  add rdx, 16
  dec rcx
  jnz @Loop

@rest:
  mov rcx, r9
  and rcx, 1
  jz @end
  movq xmm0, [rax]
  addsd xmm0, xmm1
  movq [rdx], xmm0
@end:
end;
{$else}
var pEnd: PByte;
begin
  pEnd := PByte(pA) + aCount * SizeOf(Double);
  while PByte(pA) < pEnd do begin
    pRes^  := pA^ + B;
    Inc(pRes);
    Inc(pA);
  end;
end;
{$endif}

procedure VecAdd(pA: PCmplx128; const B: TCmplx128; pRes: PCmplx128; aCount: NativeInt);
{$if defined(ASMx64)}
//RCX <- pA, RDX <- pB, R8 <- pRes, R9 <- aCount
asm
  test r9, r9
  jz @end
  movupd xmm1, [rdx]
@L:
  movupd xmm0, [rcx]
  addpd xmm0, xmm1
  movupd [r8], xmm0
  add rcx, 16
  add r8, 16
  dec r9
  jnz @L
@end:
end;
{$else}
var pEnd: PByte;
begin
  pEnd := PByte(pA) + aCount * SizeOf(TCmplx128);
  while PByte(pA) < pEnd do begin
    with pRes^ do begin
      Re := pA^.Re + B.Re;
      Im := pA^.Im + B.Im;
    end;
    Inc(pRes);
    Inc(pA);
  end;
end;
{$endif}

procedure VecAddInPlace(pA, pB: PDouble; aAStep, aBStep, aCount: NativeInt);
var pEnd: PByte;
begin
  pEnd := PByte(pA) + aCount * SizeOf(Double);
  while PByte(pA) < pEnd do begin
    pA^ := pA^ + pB^;
    Inc(pA, aAStep);
    Inc(pB, aBStep);
  end;
end;

procedure VecAddInPlace(pA, pB: PSingle; aAStep, aBStep, aCount: NativeInt);
var pEnd, pEndu: PByte;
begin
  pEnd := PByte(pA) + aCount * SizeOf(Single);
  if (aCount div aAStep) > cUnrollThreshold then begin
    pEndu := PByte(pA) + (aCount and (not NativeInt(3))) * SizeOf(Single);
    while PByte(pA) < pEndu do begin
      pA^ := pA^ + pB^;
      Inc(pA, aAStep);
      Inc(pB, aBStep);
      pA^ := pA^ + pB^;
      Inc(pA, aAStep);
      Inc(pB, aBStep);
      pA^ := pA^ + pB^;
      Inc(pA, aAStep);
      Inc(pB, aBStep);
      pA^ := pA^ + pB^;
      Inc(pA, aAStep);
      Inc(pB, aBStep);
    end;
  end;
  while PByte(pA) < pEnd do begin
    pA^ := pA^ + pB^;
    Inc(pA, aAStep);
    Inc(pB, aBStep);
  end;
end;

procedure VecSub(pA, pB, pRes: PDouble; aCount: NativeInt);
var pEnd: PByte;
begin
  pEnd := PByte(pA) + aCount * SizeOf(Double);
  while PByte(pA) < pEnd do begin
    pRes^ := pA^ - pB^;
    Inc(pRes);
    Inc(pA);
    Inc(pB);
  end;
end;

procedure VecSub(pA, pB, pRes: PSingle; aCount: NativeInt);
{$if defined(ASMx86)}
asm
  //EAX <- pA, EDX <- pB, ECX <- pRes, [EBP + 8] <- aCount
  push edi
  mov edi, ecx
  mov ecx, [ebp + 8]
  shr ecx, 3
  jz @rest
@mainLoop:
  movups xmm0, [eax]
  movups xmm1, [edx]
  subps xmm0, xmm1
  movups [edi], xmm0
  add eax, 16
  add edx, 16
  add edi, 16
  movups xmm0, [eax]
  movups xmm1, [edx]
  subps xmm0, xmm1
  movups [edi], xmm0
  add eax, 16
  add edx, 16
  add edi, 16
  dec ecx
  jnz @mainLoop

@rest:
  mov ecx, [ebp + 8]
  and ecx, 7
  jz @end
@restLoop:
  movd xmm0, [eax]
  movd xmm1, [edx]
  subss xmm0, xmm1
  movd [edi], xmm0
  add eax, 4
  add edx, 4
  add edi, 4
  dec ecx
  jnz @restLoop
@end:
  pop edi
end;
{$elseif defined(ASMx64)}
asm
  //RCX <- pA, RDX <- pB, R8 <- pRes, R9 <- aCount
  mov rax, rcx // RAX <- pA
  mov rcx, r9  // RCX <- aCount
  shr rcx, 3
  jz @rest

@mainLoop:
  movups xmm0, [rax]
  movups xmm1, [rdx]
  subps xmm0, xmm1
  movups [r8], xmm0
  add rax, 16
  add rdx, 16
  add r8, 16
  movups xmm0, [rax]
  movups xmm1, [rdx]
  subps xmm0, xmm1
  movups [r8], xmm0
  add rax, 16
  add rdx, 16
  add r8, 16
  dec rcx
  jnz @mainLoop

@rest:
  mov rcx, r9
  and rcx, 7
  jz @end

@restLoop:
  movd xmm0, [rax]
  movd xmm1, [rdx]
  subss xmm0, xmm1
  movd [r8], xmm0
  add rax, 4
  add rdx, 4
  add r8, 4
  dec rcx
  jnz @restLoop

@end:
end;
{$else}
var pEnd, pEndu: PByte;
begin
  pEnd := PByte(pA) + aCount * SizeOf(Single);
  if aCount > cUnrollThreshold then begin
    pEndu := PByte(pA) + (aCount and (not NativeInt(3))) * SizeOf(Single);
    while PByte(pA) < pEndu do begin
      pRes^ := pA^ - pB^;
      Inc(pRes);
      Inc(pA);
      Inc(pB);
      pRes^ := pA^ - pB^;
      Inc(pRes);
      Inc(pA);
      Inc(pB);
      pRes^ := pA^ - pB^;
      Inc(pRes);
      Inc(pA);
      Inc(pB);
      pRes^ := pA^ - pB^;
      Inc(pRes);
      Inc(pA);
      Inc(pB);
    end;
  end;
  while PByte(pA) < pEnd do begin
    pRes^ := pA^ - pB^;
    Inc(pRes);
    Inc(pA);
    Inc(pB);
  end;
end;
{$endif}

procedure VecSub(pA: PDouble; const B: Double; pRes: PDouble; aCount: NativeInt);
var pEnd: PByte;
begin
  pEnd := PByte(pA) + aCount * SizeOf(Double);
  while PByte(pA) < pEnd do begin
    pRes^ := pA^ - B;
    Inc(pRes);
    Inc(pA);
  end;
end;

procedure VecSub(pA: PSingle; B: Single; pRes: PSingle; aCount: NativeInt);
{$if defined(ASMx64)}
asm
  //RCX <- pA, XMM1 <- B, R8 <- pRes, R9 <- aCount
  movss xmm0, xmm1
  shufps xmm1, xmm0, 0  //xmm1 <- 4 x B
  mov rax, rcx // RAX <- pA
  mov rdx, r8  // RDX <- pRes
  mov rcx, r9
  shr rcx, 2
  jz @rest
@mainLoop:
  movups xmm0, [rax]
  subps xmm0, xmm1
  movups [rdx], xmm0
  add rax, 16
  add rdx, 16
  dec rcx
  jnz @mainLoop

@rest:
  mov rcx, r9
  and rcx, 3
  jz @end
@restLoop:
  movd xmm0, [rax]
  subss xmm0, xmm1
  movd [rdx], xmm0
  add rax, 4
  add rdx, 4
  dec rcx
  jnz @restLoop
@end:
end;
{$else}
var pEnd: PByte;
begin
  pEnd := PByte(pA) + aCount * SizeOf(Single);
  while PByte(pA) < pEnd do begin
    pRes^ := pA^ - B;
    Inc(pRes);
    Inc(pA);
  end;
end;
{$endif}

procedure VecSub(A: Single; pB: PSingle; pRes: PSingle; aCount: NativeInt);
{$if defined(ASMx64)}
asm
  //XMM0 <- A, RDX <- pB, R8 <- pRes, R9 <- aCount
  movss xmm1, xmm0
  shufps xmm1, xmm0, 0  //xmm1 <- 4 x B
  movaps xmm2, xmm1
  mov rax, rdx // RAX <- pB
  mov rdx, r8  // RDX <- pRes
  mov rcx, r9
  shr rcx, 2
  jz @rest
@mainLoop:
  movaps xmm0, xmm2
  movups xmm1, [rax]
  subps xmm0, xmm1
  movups [rdx], xmm0
  add rax, 16
  add rdx, 16
  dec rcx
  jnz @mainLoop

@rest:
  mov rcx, r9
  and rcx, 3
  jz @end
@restLoop:
  movss xmm0, xmm2
  movd xmm1, [rax]
  subss xmm0, xmm1
  movd [rdx], xmm0
  add rax, 4
  add rdx, 4
  dec rcx
  jnz @restLoop
@end:
end;
{$else}
var pEnd: PByte;
begin
  pEnd := PByte(pB) + aCount * SizeOf(Single);
  while PByte(pB) < pEnd do begin
    pRes^ := A - pB^;
    Inc(pRes);
    Inc(pB);
  end;
end;
{$endif}

procedure VecSubInPlace(pA, pB: PDouble; aAStep, aBStep, aCount: NativeInt);
var pEnd: PByte;
begin
  pEnd := PByte(pA) + aCount * SizeOf(Double);
  while PByte(pA) < pEnd do begin
    pA^ := pA^ - pB^;
    Inc(pA, aAStep);
    Inc(pB, aBStep);
  end;
end;

procedure VecSubInPlace(pA, pB: PSingle; aAStep, aBStep, aCount: NativeInt);
var pEnd: PByte;
begin
  pEnd := PByte(pA) + aCount * SizeOf(Single);
  while PByte(pA) < pEnd do begin
    pA^ := pA^ - pB^;
    Inc(pA, aAStep);
    Inc(pB, aBStep);
  end;
end;

procedure VecNeg(pA, pRes: PDouble; aCount: NativeInt);
var pEnd: PByte;
begin
  pEnd := PByte(pA) + aCount * SizeOf(Double);
  while PByte(pA) < pEnd do begin
    pRes^  := -pA^;
    Inc(pRes);
    Inc(pA);
  end;
end;

procedure VecNeg(pA, pRes: PSingle; aCount: NativeInt);
{$if defined(ASMx86)}
asm
  // EAX <- pA, EDX <- pRes, ECX <- aCount
  push ebx
  mov ebx, ecx
  shr ecx, 3
  jz @rest
@mainLoop:
  xorps xmm0, xmm0
  movups xmm1, [eax]
  subps xmm0, xmm1
  movups [edx], xmm0
  add eax, 16
  add edx, 16
  xorps xmm0, xmm0
  movups xmm1, [eax]
  subps xmm0, xmm1
  movups [edx], xmm0
  add eax, 16
  add edx, 16
  dec ecx
  jnz @mainLoop

@rest:
  mov ecx, ebx
  and ecx, 7
  jz @end
@restLoop:
  xorps xmm0, xmm0
  movd xmm1, [eax]
  subss xmm0, xmm1
  movd [edx], xmm0
  add eax, 4
  add edx, 4
  dec ecx
  jnz @restLoop
@end:
  pop ebx
end;
{$elseif defined(ASMx64)}
asm
  // RCX <- pA, RDX <- pRes, R8 <- aCount
  mov rax, rcx
  mov rcx, r8
  shr rcx, 3
  jz @rest

@mainLoop:
  xorps xmm0, xmm0
  movups xmm1, [rax]
  subps xmm0, xmm1
  movups [rdx], xmm0
  add rax, 16
  add rdx, 16
  xorps xmm0, xmm0
  movups xmm1, [rax]
  subps xmm0, xmm1
  movups [rdx], xmm0
  add rax, 16
  add rdx, 16
  dec rcx
  jnz @mainLoop

@rest:
  mov rcx, r8
  and rcx, 7
  jz @end

@restLoop:
  xorps xmm0, xmm0
  movd xmm1, [rax]
  subss xmm0, xmm1
  movd [rdx], xmm0
  add rax, 4
  add rdx, 4
  dec rcx
  jnz @restLoop

@end:
end;
{$else}
var pEnd, pEndu: PByte;
begin
  pEnd := PByte(pA) + aCount * SizeOf(Single);
  pEndu := PByte(pA) + (aCount and (not NativeInt(3))) * SizeOf(Single);
  while PByte(pA) < pEndu do begin
    pRes^ := -pA^;
    Inc(pRes);
    Inc(pA);
    pRes^ := -pA^;
    Inc(pRes);
    Inc(pA);
    pRes^ := -pA^;
    Inc(pRes);
    Inc(pA);
    pRes^ := -pA^;
    Inc(pRes);
    Inc(pA);
  end;
  while PByte(pA) < pEnd do begin
    pRes^ := -pA^;
    Inc(pRes);
    Inc(pA);
  end;
end;
{$endif}

procedure VecMul(pA, pB, pRes: PSingle; aCount: NativeInt);
{$if defined(ASMx86)}
asm
  //EAX <- pA, EDX <- pB, ECX <- pRes, [EBP + 8] <- aCount
  push edi
  mov edi, ecx
  mov ecx, [ebp + 8]
  shr ecx, 2
  jz @rest
@mainLoop:
  movups xmm0, [eax]
  movups xmm1, [edx]
  mulps xmm0, xmm1
  movups [edi], xmm0
  add eax, 16
  add edx, 16
  add edi, 16
  dec ecx
  jnz @mainLoop

@rest:
  mov ecx, [ebp + 8]
  and ecx, 3
  jz @end
@restLoop:
  movd xmm0, [eax]
  movd xmm1, [edx]
  mulss xmm0, xmm1
  movd [edi], xmm0
  add eax, 4
  add edx, 4
  add edi, 4
  dec ecx
  jnz @restLoop
@end:
  pop edi
end;
{$elseif defined(ASMx64)}
asm
  //RCX <- pA, RDX <- pB, R8 <- pRes, R9 <- aCount
  mov rax, rcx
  mov rcx, r9
  shr rcx, 2
  jz @rest
@mainLoop:
  movups xmm0, [rax]
  movups xmm1, [rdx]
  mulps xmm0, xmm1
  movups [r8], xmm0
  add rax, 16
  add rdx, 16
  add r8, 16
  dec rcx
  jnz @mainLoop

@rest:
  mov rcx, r9
  and rcx, 3
  jz @end
@restLoop:
  movd xmm0, [rax]
  movd xmm1, [rdx]
  mulss xmm0, xmm1
  movd [r8], xmm0
  add rax, 4
  add rdx, 4
  add r8, 4
  dec rcx
  jnz @restLoop
@end:
end;
{$else}
var pEnd: PByte;
begin
  pEnd := PByte(pA) + aCount * SizeOf(Single);
  while PByte(pA) < pEnd do begin
    pRes^  := pA^ * pB^;
    Inc(pRes);
    Inc(pA);
    Inc(pB);
  end;
end;
{$endif}

procedure VecMul(pA, pB, pRes: PDouble; aCount: NativeInt);
{$if defined(ASMx86)}
asm
  //EAX <- pA, EDX <- pB, ECX <- pRes, [EBP + 8] <- aCount
  push edi
  mov edi, ecx
  mov ecx, [ebp + 8]
  shr ecx, 1
  jz @rest
@Loop:
  movupd xmm0, [eax]
  movupd xmm1, [edx]
  mulpd xmm0, xmm1
  movupd [edi], xmm0
  add eax, 16
  add edx, 16
  add edi, 16
  dec ecx
  jnz @Loop

@rest:
  mov ecx, [ebp + 8]
  and ecx, 1
  jz @end
  movq xmm0, [eax]
  movq xmm1, [edx]
  mulsd xmm0, xmm1
  movq [edi], xmm0
@end:
  pop edi
end;
{$elseif defined(ASMx64)}
asm
  //RCX <- pA, RDX <- pB, R8 <- pRes, R9 <- aCount
  mov rax, rcx
  mov rcx, r9
  shr rcx, 1
  jz @rest
@Loop:
  movupd xmm0, [rax]
  movupd xmm1, [rdx]
  mulpd xmm0, xmm1
  movupd [r8], xmm0
  add rax, 16
  add rdx, 16
  add r8, 16
  dec rcx
  jnz @Loop

@rest:
  mov rcx, r9
  and rcx, 1
  jz @end
  movq xmm0, [rax]
  movq xmm1, [rdx]
  mulsd xmm0, xmm1
  movq [r8], xmm0
@end:
end;
{$else}
var pEnd: PByte;
begin
  pEnd := PByte(pA) + aCount * SizeOf(Double);
  while PByte(pA) < pEnd do begin
    pRes^  := pA^ * pB^;
    Inc(pRes);
    Inc(pA);
    Inc(pB);
  end;
end;
{$endif}

procedure VecMul(pA, pB, pRes: PCmplx128; aCount: NativeInt);
{$if defined(ASMx86)}
asm
  //EAX <- pA, EDX <- pB, ECX <- pRes, [EBP + 8] <- aCount
  push edi
  mov edi, ecx
  mov ecx, [ebp + 8]
  test ecx, ecx
  jz @end
@loop:
  movddup xmm2, [eax] // xmm2 <- (ar, ar)
  movddup xmm3, [eax + 8] // xmm3 <- (ai, ai)
  movupd xmm1, [edx]  // xmm1 <- (br, bi)
  mulpd xmm2, xmm1   // xmm2 <- (br*ar, bi*ar)
  mulpd xmm3, xmm1   // xmm3 <- (br*ai, bi*ai)
  pshufd xmm3, xmm3, $4e // xmm3 <- (bi*ai, br*ai)
  addsubpd xmm2, xmm3 // xmm2 <- a * b
  movupd [edi], xmm2
  add eax, 16
  add edx, 16
  add edi, 16
  dec ecx
  jnz @loop
@end:
  pop edi
end;
{$elseif defined(ASMx64)}
asm
  //RCX <- pA, RDX <- pB, R8 <- pRes, R9 <- aCount
  test r9, r9
  jz @end
@loop:
  movddup xmm2, [rcx] // xmm2 <- (ar, ar)
  movddup xmm3, [rcx + 8] // xmm3 <- (ai, ai)
  movupd xmm1, [rdx]  // xmm1 <- (br, bi)
  mulpd xmm2, xmm1   // xmm2 <- (br*ar, bi*ar)
  mulpd xmm3, xmm1   // xmm3 <- (br*ai, bi*ai)
  pshufd xmm3, xmm3, $4e // xmm3 <- (bi*ai, br*ai)
  addsubpd xmm2, xmm3 // xmm2 <- a * b
  movupd [r8], xmm2
  add rcx, 16
  add rdx, 16
  add r8, 16
  dec r9
  jnz @loop
@end:
end;
{$else}
var pEnd: PByte;
begin
  pEnd := PByte(pA) + aCount * SizeOf(TCmplx128);
  while PByte(pA) < pEnd do begin
    with pRes^ do begin
      Re := pA^.Re * pB^.Re - pA^.Im * pB^.Im;
      Im := pA^.Im * pB^.Re + pA^.Re * pB^.Im;
    end;
    Inc(pRes);
    Inc(pA);
    Inc(pB);
  end;
end;
{$endif}

procedure VecMul(pA: PSingle; B: Single; pRes: PSingle; aCount: NativeInt);
{$if defined(ASMx86)}
asm
  //EAX <- pA, EDX <- pRes, ECX <- aCount, [EBP + 8] <- B
  movd xmm1, [ebp + 8]
  movss xmm0, xmm1
  shufps xmm1, xmm0, 0  //xmm1 <- 4 x B
  mov [ebp + 8], ecx
  shr ecx, 2
  jz @rest
@mainLoop:
  movups xmm0, [eax]
  mulps xmm0, xmm1
  movups [edx], xmm0
  add eax, 16
  add edx, 16
  dec ecx
  jnz @mainLoop

@rest:
  mov ecx, [ebp + 8]
  and ecx, 3
  jz @end
@restLoop:
  movd xmm0, [eax]
  mulss xmm0, xmm1
  movd [edx], xmm0
  add eax, 4
  add edx, 4
  dec ecx
  jnz @restLoop
@end:
end;
{$elseif defined(ASMx64)}
asm
  //RCX <- pA, XMM1 <- B, R8 <- pRes, R9 <- aCount
  mov rax, rcx // RAX <- pA
  mov rdx, r8  // RDX <- pRes
  movss xmm0, xmm1
  shufps xmm1, xmm0, 0  //xmm1 <- 4 x B
  mov rcx, r9
  shr rcx, 2
  jz @rest
@mainLoop:
  movups xmm0, [rax]
  mulps xmm0, xmm1
  movups [rdx], xmm0
  add rax, 16
  add rdx, 16
  dec rcx
  jnz @mainLoop

@rest:
  mov rcx, r9
  and rcx, 3
  jz @end
@restLoop:
  movd xmm0, [rax]
  mulss xmm0, xmm1
  movd [rdx], xmm0
  add rax, 4
  add rdx, 4
  dec rcx
  jnz @restLoop
@end:
end;
{$else}
var pEnd: PByte;
begin
  pEnd := PByte(pA) + aCount * SizeOf(Single);
  while PByte(pA) < pEnd do begin
    pRes^  := pA^ * B;
    Inc(pRes);
    Inc(pA);
  end;
end;
{$endif}

procedure VecMul(pA: PDouble; const B: Double; pRes: PDouble; aCount: NativeInt);
{$if defined(ASMx86)}
asm
  //EAX <- pA, EDX <- pRes, ECX <- aCount, [EBP + 8] <- B
  movddup xmm1, [ebp + 8]
  mov [ebp + 8], ecx
  shr ecx, 1
  jz @rest
@Loop:
  movupd xmm0, [eax]
  mulpd xmm0, xmm1
  movupd [edx], xmm0
  add eax, 16
  add edx, 16
  dec ecx
  jnz @Loop

@rest:
  mov ecx, [ebp + 8]
  and ecx, 1
  jz @end
  movq xmm0, [eax]
  mulsd xmm0, xmm1
  movq [edx], xmm0
@end:
end;
{$else}
var pEnd: PByte;
begin
  pEnd := PByte(pA) + aCount * SizeOf(Double);
  while PByte(pA) < pEnd do begin
    pRes^  := pA^ * B;
    Inc(pRes);
    Inc(pA);
  end;
end;
{$endif}

procedure VecMul(pA: PCmplx128; const B: TCmplx128; pRes: PCmplx128; aCount: NativeInt);
{$if defined(ASMx86)}
// EAX <- pA, EDX <- @B, ECX <- pRes, [EBP + 8] <- aCount
asm
  movupd xmm2, [edx]        // xmm2 <- (br, bi)
  mov edx, ecx              // edx <- pRes
  mov ecx, [ebp + 8]        // ecx <- aCount
  test ecx, ecx
  jz @end
  pshufd xmm3, xmm2, $4e    // xmm3 <- (bi, br)
@L:
  movddup xmm0, [eax]       // xmm0 <- (ar, ar)
  movddup xmm1, [eax + 8]   // xmm1 <- (ai, ai)
  mulpd xmm0, xmm2          // xmm0 <- (ar*br, ar*bi)
  mulpd xmm1, xmm3          // xmm1 <- (ai*bi, ai*br)
  addsubpd xmm0, xmm1       // xmm0 <- (ai*br-ar*bi, ar*br+ai*bi)
  movupd [edx], xmm0
  add eax, 16
  add edx, 16
  dec ecx
  jnz @L
@end:
end;
{$elseif defined(ASMx64)}
// RCX <- pA, RDX <- @B, R8 <- pRes, R9 <- aCount
asm
  test r9, r9
  jz @end
  movupd xmm2, [rdx]        // xmm2 <- (br, bi)
  pshufd xmm3, xmm2, $4e    // xmm3 <- (bi, br)
@L:
  movddup xmm0, [rcx]       // xmm0 <- (ar, ar)
  movddup xmm1, [rcx + 8]   // xmm1 <- (ai, ai)
  mulpd xmm0, xmm2          // xmm0 <- (ar*br, ar*bi)
  mulpd xmm1, xmm3          // xmm1 <- (ai*bi, ai*br)
  addsubpd xmm0, xmm1       // xmm0 <- (ai*br-ar*bi, ar*br+ai*bi)
  movupd [r8], xmm0
  add rcx, 16
  add r8, 16
  dec r9
  jnz @L
@end:
end;
{$else}
var pEnd: PByte;
begin
  pEnd := PByte(pA) + aCount * SizeOf(TCmplx128);
  while PByte(pA) < pEnd do begin
    with pRes^ do begin
      Re := pA^.Re * B.Re - pA^.Im * B.Im;
      Im := pA^.Re * B.Im + pA^.Im * B.Re;
    end;
    Inc(pRes);
    Inc(pA);
  end;
end;
{$endif}

procedure VecDiv(pA, pB, pRes: PCmplx128; aCount: NativeInt);
{$if defined(ASMx86)}
asm
  //EAX <- pA, EDX <- pB, ECX <- pRes, [EBP + 8] <- aCount
  push edi
  mov edi, ecx
  mov ecx, [ebp + 8]
  test ecx, ecx
  jz @end
@loop:
  movddup xmm2, [eax]       // xmm2 <- (ar, ar)
  movddup xmm3, [eax + 8]   // xmm3 <- (ai, ai)
  movupd xmm1, [edx]        // xmm1 <- (br, bi)
  mulpd xmm3, xmm1          // xmm3 <- (ai*br, ai*bi)
  pshufd xmm1, xmm1, $4e    // xmm1 <- (bi, br)
  mulpd xmm2, xmm1          // xmm2 <- (ar*bi, ar*br)
  addsubpd xmm3, xmm2       // xmm3 <- (ai*br-ar*bi, ar*br+ai*bi)
  pshufd xmm3, xmm3, $4e    // xmm3 <- (ar*br+ai*bi, ai*br-ar*bi)

  mulpd xmm1, xmm1
  movhlps xmm2, xmm1
  addsd xmm1, xmm2
  movlhps xmm1, xmm1
  divpd xmm3, xmm1          // xmm3 <- xmm3 / b^2
  movupd [edi], xmm3

  add eax, 16
  add edx, 16
  add edi, 16
  dec ecx
  jnz @loop
@end:
  pop edi
end;
{$elseif defined(ASMx64)}
asm
  test r9, r9
  jz @end
@loop:
  movddup xmm2, [rcx]       // xmm2 <- (ar, ar)
  movddup xmm3, [rcx + 8]   // xmm3 <- (ai, ai)
  movupd xmm1, [rdx]        // xmm1 <- (br, bi)
  mulpd xmm3, xmm1          // xmm3 <- (ai*br, ai*bi)
  pshufd xmm1, xmm1, $4e    // xmm1 <- (bi, br)
  mulpd xmm2, xmm1          // xmm2 <- (ar*bi, ar*br)
  addsubpd xmm3, xmm2       // xmm3 <- (ai*br-ar*bi, ar*br+ai*bi)
  pshufd xmm3, xmm3, $4e    // xmm3 <- (ar*br+ai*bi, ai*br-ar*bi)

  mulpd xmm1, xmm1
  movhlps xmm2, xmm1
  addsd xmm1, xmm2
  movlhps xmm1, xmm1
  divpd xmm3, xmm1          // xmm3 <- xmm3 / b^2
  movupd [r8], xmm3

  add rcx, 16
  add rdx, 16
  add r8, 16
  dec r9
  jnz @loop
@end:
end;
{$else}
var pEnd: PByte;
    denom: Double;
begin
  pEnd := PByte(pA) + aCount * SizeOf(TCmplx128);
  while PByte(pA) < pEnd do begin
    with pB^ do begin
      denom := Re * Re + Im * Im;
      pRes^.Re := (pA^.Re * Re + pA^.Im * Im) / denom;
      pRes^.Im := (pA^.Im * Re - pA^.Re * Im) / denom;
    end;
    Inc(pRes);
    Inc(pA);
    Inc(pB);
  end;
end;
{$endif}

procedure VecAddWithSat(pA, pB, pRes: PUInt8; aCount: NativeInt);
{$if defined(ASMx86)}
asm
  // EAX <- pA, EDX <- pB, ECX <- pRes, [EBP + 8] <- aCount
  push edi
  mov edi, ecx
  mov ecx, [ebp + 8]
  shr ecx, 4
  jz @rest
@loop:
  movupd xmm0, [eax]
  movupd xmm1, [edx]
  paddusb xmm0, xmm1
  movupd [edi], xmm0
  add eax, 16
  add edx, 16
  add edi, 16
  dec ecx
  jnz @loop

@rest:
  mov ecx, [ebp + 8]
  and ecx, 15
  jz @end
  push ebx
  push esi
  mov esi, eax
@restloop:
  mov bl, 0
  mov al, [esi]
  add al, [edx]
  adc bl, 0
  jnz @sat
  mov [edi], al
  jmp @testEnd
@sat:
  mov byte ptr [edi], $FF
@testEnd:
  inc esi
  inc edx
  inc edi
  dec ecx
  jnz @restLoop

  pop esi
  pop ebx
@end:
  pop edi
end;
{$else}
var pEnd: PByte;
    v: UInt16;
begin
  pEnd := PByte(pA) + aCount;
  while PByte(pA) < pEnd do begin
    v := pA^ + pB^;
    if v <= MAX_UINT8 then
      pRes^ := v
    else
      pRes^ := MAX_UINT8;
    Inc(pRes);
    Inc(pA);
    Inc(pB);
  end;
end;
{$endif}

procedure VecAddWithSat(pA, pB, pRes: PUInt16; aCount: NativeInt);
{$if defined(ASMx86)}
asm
  // EAX <- pA, EDX <- pB, ECX <- pRes, [EBP + 8] <- aCount
  push edi
  mov edi, ecx
  mov ecx, [ebp + 8]
  shr ecx, 3
  jz @rest
@loop:
  movupd xmm0, [eax]
  movupd xmm1, [edx]
  paddusw xmm0, xmm1
  movupd [edi], xmm0
  add eax, 16
  add edx, 16
  add edi, 16
  dec ecx
  jnz @loop

@rest:
  mov ecx, [ebp + 8]
  and ecx, 7
  jz @end
  push ebx
  push esi
  mov esi, eax
@restloop:
  mov bx, 0
  mov ax, [esi]
  add ax, [edx]
  adc bx, 0
  jnz @sat
  mov [edi], ax
  jmp @testEnd
@sat:
  mov word ptr [edi], $FFFF
@testEnd:
  add esi, 2
  add edx, 2
  add edi, 2
  dec ecx
  jnz @restLoop

  pop esi
  pop ebx
@end:
  pop edi
end;
{$else}
var pEnd: PByte;
    v: UInt32;
begin
  pEnd := PByte(pA) + aCount * SizeOf(UInt16);
  while PByte(pA) < pEnd do begin
    v := pA^ + pB^;
    if v <= MAX_UINT16 then
      pRes^ := v
    else
      pRes^ := MAX_UINT16;
    Inc(pRes);
    Inc(pA);
    Inc(pB);
  end;
end;
{$endif}

procedure VecAddWithSat(pA: PUInt8; B: UInt8; pRes: PUInt8; aCount: NativeInt);
{$if defined(ASMx86)}
asm
  // EAX <- pA, EDX <- B, ECX <- pRes, [EBP + 8] <- aCount
  push edi
  mov edi, ecx
  mov ecx, [ebp + 8]
  shr ecx, 4
  jz @rest
  push eax
  mov eax, edx
  call _dup_I8_xmm1
  pop eax
@loop:
  movupd xmm0, [eax]
  paddusb xmm0, xmm1
  movupd [edi], xmm0
  add eax, 16
  add edi, 16
  dec ecx
  jnz @loop

@rest:
  mov ecx, [ebp + 8]
  and ecx, 15
  jz @end
  push ebx
  push esi
  mov esi, eax
@restloop:
  mov bl, 0
  mov al, [esi]
  add al, dl
  adc bl, 0
  jnz @sat
  mov [edi], al
  jmp @testEnd
@sat:
  mov byte ptr [edi], $FF
@testEnd:
  inc esi
  inc edi
  dec ecx
  jnz @restLoop

  pop esi
  pop ebx
@end:
  pop edi
end;
{$else}
var pEnd: PByte;
    v: UInt16;
begin
  pEnd := PByte(pA) + aCount;
  while PByte(pA) < pEnd do begin
    v := pA^ + B;
    if v <= MAX_UINT8 then
      pRes^ := v
    else
      pRes^ := MAX_UINT8;
    Inc(pRes);
    Inc(pA);
  end;
end;
{$endif}

procedure VecAddWithSat(pA: PUInt16; B: UInt16; pRes: PUInt16; aCount: NativeInt);
{$if defined(ASMx86)}
asm
  // EAX <- pA, EDX <- B, ECX <- pRes, [EBP + 8] <- aCount
  push edi
  mov edi, ecx
  mov ecx, [ebp + 8]
  shr ecx, 3
  jz @rest
  push eax
  mov eax, edx
  call _dup_I16_xmm1
  pop eax
@loop:
  movupd xmm0, [eax]
  paddusw xmm0, xmm1
  movupd [edi], xmm0
  add eax, 16
  add edi, 16
  dec ecx
  jnz @loop

@rest:
  mov ecx, [ebp + 8]
  and ecx, 7
  jz @end
  push ebx
  push esi
  mov esi, eax
@restloop:
  mov bx, 0
  mov ax, [esi]
  add ax, dx
  adc bx, 0
  jnz @sat
  mov [edi], ax
  jmp @testEnd
@sat:
  mov word ptr [edi], $FFFF
@testEnd:
  add esi, 2
  add edi, 2
  dec ecx
  jnz @restloop

  pop esi
  pop ebx
@end:
  pop edi
end;
{$else}
var pEnd: PByte;
    v: UInt32;
begin
  pEnd := PByte(pA) + aCount * SizeOf(UInt16);
  while PByte(pA) < pEnd do begin
    v := pA^ + B;
    if v <= MAX_UINT16 then
      pRes^ := v
    else
      pRes^ := MAX_UINT16;
    Inc(pRes);
    Inc(pA);
  end;
end;
{$endif}

procedure VecSubWithSat(pA, pB, pRes: PUInt8; aCount: NativeInt);
{$if defined(ASMx86)}
asm
  // EAX <- pA, EDX <- pB, ECX <- pRes, [EBP + 8] <- aCount
  push edi
  mov edi, ecx
  mov ecx, [ebp + 8]
  shr ecx, 4
  jz @rest
@loop:
  movupd xmm0, [eax]
  movupd xmm1, [edx]
  psubusb xmm0, xmm1
  movupd [edi], xmm0
  add eax, 16
  add edx, 16
  add edi, 16
  dec ecx
  jnz @loop

@rest:
  mov ecx, [ebp + 8]
  and ecx, 15
  jz @end
  push ebx
  push esi
  mov esi, eax
@restloop:
  mov bl, 0
  mov al, [esi]
  sub al, [edx]
  adc bl, 0
  jnz @sat
  mov [edi], al
  jmp @testEnd
@sat:
  mov byte ptr [edi], 0
@testEnd:
  inc esi
  inc edx
  inc edi
  dec ecx
  jnz @restloop

  pop esi
  pop ebx
@end:
  pop edi
end;
{$else}
var pEnd: PByte;
    v: Int16;
begin
  pEnd := PByte(pA) + aCount;
  while PByte(pA) < pEnd do begin
    v := Int16(pA^) - pB^;
    if v >= 0 then
      pRes^ := v
    else
      pRes^ := 0;
    Inc(pRes);
    Inc(pA);
    Inc(pB);
  end;
end;
{$endif}

procedure VecSubWithSat(pA, pB, pRes: PUInt16; aCount: NativeInt);
{$if defined(ASMx86)}
asm
  // EAX <- pA, EDX <- pB, ECX <- pRes, [EBP + 8] <- aCount
  push edi
  mov edi, ecx
  mov ecx, [ebp + 8]
  shr ecx, 3
  jz @rest
@loop:
  movupd xmm0, [eax]
  movupd xmm1, [edx]
  psubusw xmm0, xmm1
  movupd [edi], xmm0
  add eax, 16
  add edx, 16
  add edi, 16
  dec ecx
  jnz @loop

@rest:
  mov ecx, [ebp + 8]
  and ecx, 7
  jz @end
  push ebx
  push esi
  mov esi, eax
@restloop:
  mov bx, 0
  mov ax, [esi]
  sub ax, [edx]
  adc bx, 0
  jnz @sat
  mov [edi], ax
  jmp @testEnd
@sat:
  mov word ptr [edi], 0
@testEnd:
  add esi, 2
  add edx, 2
  add edi, 2
  dec ecx
  jnz @restloop

  pop esi
  pop ebx
@end:
  pop edi
end;
{$else}
var pEnd: PByte;
    v: Int32;
begin
  pEnd := PByte(pA) + aCount * SizeOf(UInt16);
  while PByte(pA) < pEnd do begin
    v := Int32(pA^) - pB^;
    if v >= 0 then
      pRes^ := v
    else
      pRes^ := 0;
    Inc(pRes);
    Inc(pA);
    Inc(pB);
  end;
end;
{$endif}

procedure VecSubWithSat(pA: PUInt8; B: UInt8; pRes: PUInt8; aCount: NativeInt);
{$if defined(ASMx86)}
asm
  // EAX <- pA, EDX <- B, ECX <- pRes, [EBP + 8] <- aCount
  push edi
  mov edi, ecx
  mov ecx, [ebp + 8]
  shr ecx, 4
  jz @rest
  push eax
  mov eax, edx
  call _dup_I8_xmm1
  pop eax
@loop:
  movupd xmm0, [eax]
  psubusb xmm0, xmm1
  movupd [edi], xmm0
  add eax, 16
  add edi, 16
  dec ecx
  jnz @loop

@rest:
  mov ecx, [ebp + 8]
  and ecx, 15
  jz @end
  push ebx
  push esi
  mov esi, eax
@restloop:
  mov bl, 0
  mov al, [esi]
  sub al, dl
  adc bl, 0
  jnz @sat
  mov [edi], al
  jmp @testEnd
@sat:
  mov byte ptr [edi], 0
@testEnd:
  inc esi
  inc edi
  dec ecx
  jnz @restloop

  pop esi
  pop ebx
@end:
  pop edi
end;
{$else}
var pEnd: PByte;
    v: Int16;
begin
  pEnd := PByte(pA) + aCount;
  while PByte(pA) < pEnd do begin
    v := Int16(pA^) - B;
    if v >= 0 then
      pRes^ := v
    else
      pRes^ := 0;
    Inc(pRes);
    Inc(pA);
  end;
end;
{$endif}

procedure VecSubWithSat(pA: PUInt16; B: UInt16; pRes: PUInt16; aCount: NativeInt);
{$if defined(ASMx86)}
asm
  // EAX <- pA, EDX <- B, ECX <- pRes, [EBP + 8] <- aCount
  push edi
  mov edi, ecx
  mov ecx, [ebp + 8]
  shr ecx, 3
  jz @rest
  push eax
  mov eax, edx
  call _dup_I16_xmm1
  pop eax
@loop:
  movupd xmm0, [eax]
  psubusw xmm0, xmm1
  movupd [edi], xmm0
  add eax, 16
  add edi, 16
  dec ecx
  jnz @loop

@rest:
  mov ecx, [ebp + 8]
  and ecx, 7
  jz @end
  push ebx
  push esi
  mov esi, eax
@restloop:
  mov bx, 0
  mov ax, [esi]
  sub ax, dx
  adc bx, 0
  jnz @sat
  mov [edi], ax
  jmp @testEnd
@sat:
  mov word ptr [edi], 0
@testEnd:
  add esi, 2
  add edi, 2
  dec ecx
  jnz @restloop

  pop esi
  pop ebx
@end:
  pop edi
end;
{$else}
var pEnd: PByte;
    v: Int32;
begin
  pEnd := PByte(pA) + aCount * SizeOf(UInt16);
  while PByte(pA) < pEnd do begin
    v := Int32(pA^) - B;
    if v >= 0 then
      pRes^ := v
    else
      pRes^ := 0;
    Inc(pRes);
    Inc(pA);
  end;
end;
{$endif}

procedure VecAnd(pA, pB, pRes: PByte; aCount: NativeInt);
{$ifdef ASMx86}
asm
  // EAX <- pA, EDX <- pB, ECX <- pRes, [EBP + 8] <- aCount
  push edi
  mov edi, ecx
  mov ecx, [ebp + 8]
  shr ecx, 4
  jz @rest
@loop:
  movupd xmm0, [eax]
  movupd xmm1, [edx]
  andpd xmm0, xmm1
  movupd [edi], xmm0
  add eax, 16
  add edx, 16
  add edi, 16
  dec ecx
  jnz @loop

@rest:
  mov ecx, [ebp + 8]
  and ecx, 15
  jz @end
  push ebx
  push esi
  mov esi, eax
@restloop:
  mov al, [esi]
  mov bl, [edx]
  and al, bl
  mov [edi], al
  inc esi
  inc edx
  inc edi
  dec ecx
  jnz @restloop

  pop esi
  pop ebx
@end:
  pop edi
end;
{$else}
var pEnd: PByte;
const
  cMask = {$if SizeOf(NativeInt) = 4}3{$else}7{$endif};
begin
  pEnd := pA + aCount and (not NativeInt(cMask));
  while pA < pEnd do begin
    PNativeInt(pRes)^ := PNativeInt(pA)^ and PNativeInt(pB)^;
    Inc(pRes, SizeOf(NativeInt));
    Inc(pA, SizeOf(NativeInt));
    Inc(pB, SizeOf(NativeInt));
  end;

  pEnd := pA + (aCount and cMask);
  while pA < pEnd do begin
    pRes^ := pA^ and pB^;
    Inc(pRes);
    Inc(pA);
    Inc(pB);
  end;
end;
{$endif}

procedure VecOr(pA, pB, pRes: PByte; aCount: NativeInt);
var pEnd: PByte;
const
  cMask = {$if SizeOf(NativeInt) = 4}3{$else}7{$endif};
begin
  pEnd := pA + aCount and (not NativeInt(cMask));
  while pA < pEnd do begin
    PNativeInt(pRes)^ := PNativeInt(pA)^ or PNativeInt(pB)^;
    Inc(pRes, SizeOf(NativeInt));
    Inc(pA, SizeOf(NativeInt));
    Inc(pB, SizeOf(NativeInt));
  end;

  pEnd := pA + (aCount and cMask);
  while pA < pEnd do begin
    pRes^ := pA^ or pB^;
    Inc(pRes);
    Inc(pA);
    Inc(pB);
  end;
end;

procedure VecNot(pA, pRes: PByte; aCount: NativeInt);
var pEnd: PByte;
const
  cMask = {$if SizeOf(NativeInt) = 4}3{$else}7{$endif};
begin
  pEnd := pA + aCount and (not NativeInt(cMask));
  while pA < pEnd do begin
    PNativeInt(pRes)^ := not PNativeInt(pA)^;
    Inc(pRes, SizeOf(NativeInt));
    Inc(pA, SizeOf(NativeInt));
  end;

  pEnd := pA + (aCount and cMask);
  while pA < pEnd do begin
    pRes^ := not pA^;
    Inc(pRes);
    Inc(pA);
  end;
end;

{$ifdef RANGEON}
   {$R-}
{$endif}

{$ifdef OVERFLOWON}
  {$Q-}
{$endif}

procedure sscal(pX: PSingle; aCount: NativeInt; a: Single);
{$if defined(ASMx86)}
asm
  // EAX <- pData, EDX <- aCount, [EBP + 8] <- aScale
  movd xmm0, [ebp + 8]
  movss xmm1, xmm0
  shufps xmm0, xmm1, 0 // xmm0 <- 4 x scale
  mov ecx, edx
  and edx, 3 // EDX <- rest count
  shr ecx, 2
  jz @rest
@mainLoop:
  movups xmm1, [eax]
  mulps xmm1, xmm0
  movups [eax], xmm1
  add eax, 16
  dec ecx
  jnz @mainLoop

@rest:
  cmp edx, 0
  jz @end
  mov ecx, edx
@restLoop:
  movd xmm1, [eax]
  mulss xmm1, xmm0
  movd [eax], xmm1
  add eax, 4
  dec ecx
  jnz @restLoop
@end:
end;
{$elseif defined(ASMx64)}
asm
  // RCX <- pData, RDX <- aCount, XMM2 <- aScale
  movss xmm0, xmm2
  shufps xmm0, xmm2, 0 // xmm0 <- 4 x scale
  mov rax, rcx // RAX <- pData
  mov rcx, rdx
  and rdx, 3 // RDX <- rest count
  shr rcx, 2
  jz @rest
@mainLoop:
  movups xmm1, [rax]
  mulps xmm1, xmm0
  movups [rax], xmm1
  add rax, 16
  dec rcx
  jnz @mainLoop

@rest:
  cmp rdx, 0
  jz @end
  mov rcx, rdx
@restLoop:
  movd xmm1, [rax]
  mulss xmm1, xmm0
  movd [rax], xmm1
  add rax, 4
  dec rcx
  jnz @restLoop
@end:
end;
{$else}
var pEnd: PByte;
begin
  pEnd := PByte(pX) + aCount * SizeOf(Single);
  while PByte(pX) < pEnd do begin
    PX^ := a * pX^;
    Inc(pX);
  end;
end;
{$endif}

procedure dscal(pX: PDouble; aCount: NativeInt; a: Double);
{$if defined(ASMx86)}
asm
  // EAX <- pData, EDX <- aCount, [EBP + 8] <- aScale
  movddup xmm0, [ebp + 8] // xmm0 <- 2 x a
  mov ecx, edx
  shr ecx, 3
  jz @rest

@L:
  movupd xmm1, [eax]
  mulpd xmm1, xmm0
  movupd [eax], xmm1
  add eax, 16

  movupd xmm1, [eax]
  mulpd xmm1, xmm0
  movupd [eax], xmm1
  add eax, 16

  movupd xmm1, [eax]
  mulpd xmm1, xmm0
  movupd [eax], xmm1
  add eax, 16

  movupd xmm1, [eax]
  mulpd xmm1, xmm0
  movupd [eax], xmm1
  add eax, 16
  dec ecx
  jnz @L

@rest:
  mov ecx, edx
  and ecx, 7
  jz @end

@RL:
  movq xmm1, [eax]
  mulsd xmm1, xmm0
  movq [eax], xmm1
  add eax, 8
  dec ecx
  jnz @RL

@end:
end;
{$else}
var pEnd, pEndu: PByte;
begin
  pEnd := PByte(pX) + aCount * SizeOf(Double);
  pEndu := PByte(pX) + (aCount and (not NativeInt(3))) * SizeOf(Double);
  while PByte(pX) < pEndu do begin
    pX^ := a * pX^;
    Inc(pX);
    pX^ := a * pX^;
    Inc(pX);
    pX^ := a * pX^;
    Inc(pX);
    pX^ := a * pX^;
    Inc(pX);
  end;
  while PByte(pX) < pEnd do begin
    pX^ := a * pX^;
    Inc(pX);
  end;
end;
{$endif}

procedure scopy(pX, pY: PSingle; aCount, XStep, YStep: NativeInt);
{$if defined(ASMx86)}
asm
  // EAX <- pX, EDX <- pY, ECX <- aCount, [EBP + 8] <- YStep, [EBP + 12] <- XStep
  push esi
  push edi
  push ebx

  mov esi, eax
  mov edi, edx
  mov eax, [ebp + 12]
  shl eax, 2 // EAX <- XStep * SizeOf(Single)
  mov edx, [ebp + 8]
  shl edx, 2 // EDX <- YStep * SizeOf(Single)
  mov [ebp + 8], ecx

  and eax, eax
  jz @ccopy

  shr ecx, 2
  jz @rest
@L:
  mov ebx, [esi]
  mov [edi], ebx
  add esi, eax
  add edi, edx

  mov ebx, [esi]
  mov [edi], ebx
  add esi, eax
  add edi, edx

  mov ebx, [esi]
  mov [edi], ebx
  add esi, eax
  add edi, edx

  mov ebx, [esi]
  mov [edi], ebx
  add esi, eax
  add edi, edx
  dec ecx
  jnz @L

@rest:
  mov ecx, [ebp + 8]
  and ecx, 3
  jz @end
@Lrest:
  mov ebx, [esi]
  mov [edi], ebx
  add esi, eax
  add edi, edx
  dec ecx
  jnz @Lrest

  jmp @end

@ccopy: // single value -> Y
  mov ebx, [esi]
  shr ecx, 2
  jz @rest
@cL:
  mov [edi], ebx
  add edi, edx
  mov [edi], ebx
  add edi, edx
  mov [edi], ebx
  add edi, edx
  mov [edi], ebx
  add edi, edx
  dec ecx
  jnz @cL

@crest:
  mov ecx, [ebp + 8]
  and ecx, 3
  jz @end
@cLrest:
  mov [edi], ebx
  add edi, edx
  dec ecx
  jnz @cLrest

@end:
  pop ebx
  pop edi
  pop esi
end;
{$else}
var pEnd, pEndu: PByte;
    s: Single;
begin
  if XStep = 0 then begin
    s := pX^;
    pEnd := PByte(pY) + aCount * YStep * SizeOf(Single);
    pEndu := PByte(pY) + (aCount and (not NativeInt(3))) * YStep * SizeOf(Single);
    while PByte(pY) < pEndu do begin
      pY^ := s;
      Inc(pY, YStep);
      pY^ := s;
      Inc(pY, YStep);
      pY^ := s;
      Inc(pY, YStep);
      pY^ := s;
      Inc(pY, YStep)
    end;
    while PByte(pY) < pEnd do begin
      pY^ := s;
      Inc(pY, YStep);
    end;
    exit;
  end;

  pEnd := PByte(pX) + aCount * XStep * SizeOf(Single);
  pEndu := PByte(pX) + (aCount and (not NativeInt(3))) * XStep * SizeOf(Single);
  if (XStep = 1) and (YStep = 1) then begin
    while PByte(pX) < pEndu do begin
      pY^ := pX^;
      Inc(pX);
      Inc(pY);
      pY^ := pX^;
      Inc(pX);
      Inc(pY);
      pY^ := pX^;
      Inc(pX);
      Inc(pY);
      pY^ := pX^;
      Inc(pX);
      Inc(pY);
    end;
    while PByte(pX) < pEnd do begin
      pY^ := pX^;
      Inc(pX);
      Inc(pY);
    end;
  end else begin
    while PByte(pX) < pEndu do begin
      pY^ := pX^;
      Inc(pX, XStep);
      Inc(pY, YStep);
      pY^ := pX^;
      Inc(pX, XStep);
      Inc(pY, YStep);
      pY^ := pX^;
      Inc(pX, XStep);
      Inc(pY, YStep);
      pY^ := pX^;
      Inc(pX, XStep);
      Inc(pY, YStep);
    end;
    while PByte(pX) < pEnd do begin
      pY^ := pX^;
      Inc(pX, XStep);
      Inc(pY, YStep);
    end;
  end;
end;
{$endif}

procedure dcopy(pX, pY: PDouble; aCount, XStep, YStep: NativeInt);
var pEnd, pEndu: PByte;
begin
  pEnd := PByte(pX) + aCount * XStep * SizeOf(Double);
  pEndu := PByte(pX) + (aCount and (not NativeInt(3))) * XStep * SizeOf(Double);
  if (XStep = 1) and (YStep = 1) then begin
    while PByte(pX) < pEndu do begin
      pY^ := pX^;
      Inc(pX);
      Inc(pY);
      pY^ := pX^;
      Inc(pX);
      Inc(pY);
      pY^ := pX^;
      Inc(pX);
      Inc(pY);
      pY^ := pX^;
      Inc(pX);
      Inc(pY);
    end;
    while PByte(pX) < pEnd do begin
      pY^ := pX^;
      Inc(pX);
      Inc(pY);
    end;
  end else begin
    while PByte(pX) < pEndu do begin
      pY^ := pX^;
      Inc(pX, XStep);
      Inc(pY, YStep);
      pY^ := pX^;
      Inc(pX, XStep);
      Inc(pY, YStep);
      pY^ := pX^;
      Inc(pX, XStep);
      Inc(pY, YStep);
      pY^ := pX^;
      Inc(pX, XStep);
      Inc(pY, YStep);
    end;
    while PByte(pX) < pEnd do begin
      pY^ := pX^;
      Inc(pX, XStep);
      Inc(pY, YStep);
    end;
  end;
end;

procedure axpy(const a: Double; pX, pY: PDouble; aCount: NativeInt);
{$if defined(ASMx86)}
//EAX <- pX, EDX <- pY, ECX <- aCount, [EBP + 8] <- a
const OneVal: Double = 1.0;
asm
  push esi
  push edi
  mov esi, eax
  mov edi, edx
  mov eax, ecx
  movddup xmm2, [ebp + 8]  // xmm2 <- (a, a)

  movsd xmm1, [OneVal] // Move constant 1.0 to XMM1 register
  comisd xmm2, xmm1      // Compare 'a' to 1.0
  je @A1            // Jump if ZF is set (values are equal)

  shr ecx, 2
  jz @rest
@L:
  movupd xmm0, [esi]
  movupd xmm1, [edi]
  mulpd xmm0, xmm2 // xmm0 <- a * x
  addpd xmm0, xmm1 // xmm0 <- a * x + y
  movupd [edi], xmm0
  add esi, 16
  add edi, 16

  movupd xmm0, [esi]
  movupd xmm1, [edi]
  mulpd xmm0, xmm2
  addpd xmm0, xmm1
  movupd [edi], xmm0
  add esi, 16
  add edi, 16

  dec ecx
  jnz @L

@rest:
  and eax, 3
  jz @end
  mov ecx, eax
@Lr:
  movsd xmm0, [esi]
  movsd xmm1, [edi]
  mulsd xmm0, xmm2
  addsd xmm0, xmm1
  movsd [edi], xmm0
  add esi, 8
  add edi, 8
  dec ecx
  jnz @Lr

  jmp @end;

@A1:
  shr ecx, 2
  jz @restA1
@LA1:
  movupd xmm0, [esi]
  movupd xmm1, [edi]
  addpd xmm0, xmm1 // xmm0 <- x + y
  movupd [edi], xmm0
  add esi, 16
  add edi, 16

  movupd xmm0, [esi]
  movupd xmm1, [edi]
  addpd xmm0, xmm1
  movupd [edi], xmm0
  add esi, 16
  add edi, 16

  dec ecx
  jnz @LA1

@restA1:
  and eax, 3
  jz @end
  mov ecx, eax
@LrA1:
  movsd xmm0, [esi]
  movsd xmm1, [edi]
  addsd xmm0, xmm1
  movsd [edi], xmm0
  add esi, 8
  add edi, 8
  dec ecx
  jnz @LrA1

@end:
  pop edi
  pop esi
end;
{$elseif defined(ASMx64)}
{$CODEALIGN 16}
asm
  // XMM0 <- a, RDX <- pX, R8 <- pY, R9 <- aCount
  mov rcx, r9
  shr rcx, 2
  jz @rest
{$ifdef AVX}
  vbroadcastsd ymm0, xmm0
@L:
  vmovups ymm1, [rdx] // ymm1 <- x
  vmovups ymm2, [r8] // ymm2 <- y
  vmulpd ymm1, ymm1, ymm0 // ymm1 <- a * x
  vaddpd ymm1, ymm1, ymm2 // ymm1 <- a * x + y
  vmovups [r8], ymm1
  add rdx, 32
  add r8, 32
  dec rcx
  jnz @L

  vzeroupper
{$else}
  movddup xmm0, xmm0
@L:
  movupd xmm1, [rdx]
  movupd xmm2, [r8]
  mulpd xmm1, xmm0 // xmm0 <- a * x
  addpd xmm1, xmm2 // xmm0 <- a * x + y
  movupd [r8], xmm1
  add rdx, 16
  add r8, 16

  movupd xmm1, [rdx]
  movupd xmm2, [r8]
  mulpd xmm1, xmm0
  addpd xmm1, xmm2
  movupd [r8], xmm1
  add rdx, 16
  add r8, 16

  dec rcx
  jnz @L
{$endif}

@rest:
  and r9, 3
  jz @end
@Lr:
  movsd xmm1, [rdx]
  movsd xmm2, [r8]
  mulsd xmm1, xmm0
  addsd xmm1, xmm2
  movsd [r8], xmm1
  add rdx, 8
  add r8, 8
  dec r9
  jnz @Lr

  jmp @end;
@end:
end;
{$else}
var pEnd, pEndu: PByte;
begin
  pEnd := PByte(pX) + aCount * SizeOf(Double);
  pEndu := PByte(pX) + (aCount and (not NativeInt(3))) * SizeOf(Double);
  if a = 1 then begin
    while PByte(pX) < pEndu do begin
      pY^ := pX^ + pY^;
      Inc(pX);
      Inc(pY);
      pY^ := pX^ + pY^;
      Inc(pX);
      Inc(pY);
      pY^ := pX^ + pY^;
      Inc(pX);
      Inc(pY);
      pY^ := pX^ + pY^;
      Inc(pX);
      Inc(pY);
    end;
    while PByte(pX) < pEnd do begin
      pY^ := pX^ + pY^;
      Inc(pX);
      Inc(pY);
    end;
  end else begin
    while PByte(pX) < pEndu do begin
      pY^ := a * pX^ + pY^;
      Inc(pX);
      Inc(pY);
      pY^ := a * pX^ + pY^;
      Inc(pX);
      Inc(pY);
      pY^ := a * pX^ + pY^;
      Inc(pX);
      Inc(pY);
      pY^ := a * pX^ + pY^;
      Inc(pX);
      Inc(pY);
    end;
    while PByte(pX) < pEnd do begin
      pY^ := a * pX^ + pY^;
      Inc(pX);
      Inc(pY);
    end;
  end;
end;
{$endif}

procedure axpy(const a: Single; pX, pY: PSingle; aCount: NativeInt);
{$if defined(ASMx86)}
//EAX <- pX, EDX <- pY, ECX <- aCount, [EBP + 8] <- a
asm
  push esi
  push edi
  mov esi, eax
  mov edi, edx
  mov eax, ecx
  shr ecx, 2
  movd xmm1, [ebp + 8]
  movss xmm2, xmm1
  jz @rest
  shufps xmm2, xmm1, 0 // xmm2 <- (a, a, a, a)
@L:
  movupd xmm0, [esi]
  movupd xmm1, [edi]
  mulps xmm0, xmm2 // xmm0 <- a * x
  addps xmm0, xmm1 // xmm0 <- a * x + y
  movups [edi], xmm0
  add esi, 16
  add edi, 16
  dec ecx
  jnz @L

@rest:
  and eax, 3
  jz @end
  mov ecx, eax
@Lrest:
  movss xmm0, [esi]
  movss xmm1, [edi]
  mulss xmm0, xmm2
  addss xmm0, xmm1
  movd [edi], xmm0
  add esi, 4
  add edi, 4
  dec ecx
  jnz @Lrest

@end:
  pop edi
  pop esi
end;
{$elseif defined(ASMx64)}
{$CODEALIGN 16}
asm
  // XMM0 <- a, RDX <- pX, R8 <- pY, R9 <- aCount
  mov rax, r8 // (RAX, RDX) <- (pY, pX)
  mov rcx, r9
{$ifdef AVX}
  shr rcx, 3
  jz @rest
  vbroadcastss ymm0, xmm0
@L:
  vmovups ymm1, [rdx] // ymm1 <- x
  vmovups ymm2, [rax] // ymm2 <- y
  vmulps ymm1, ymm1, ymm0 // ymm1 <- a * x
  vaddps ymm1, ymm1, ymm2 // ymm1 <- a * x + y
  vmovups [rax], ymm1
  add rax, 32
  add rdx, 32
  dec rcx
  jnz @L

  vzeroupper
@rest:
  and r9, 7
{$else}
  movss xmm2, xmm0
  shr rcx, 2
  jz @rest
  shufps xmm0, xmm0, 0  // xmm0 <- (a, a, a, a)
@L:
  movupd xmm1, [rdx] // xmm0 <- x
  movupd xmm2, [rax] // xmm1 <- y
  mulps xmm1, xmm0 // xmm0 <- a * x
  addps xmm1, xmm2 // xmm0 <- a * x + y
  movups [rax], xmm1
  add rax, 16
  add rdx, 16
  dec rcx
  jnz @L

@rest:
  and r9, 3
{$endif}
  jz @end
  mov rcx, r9
@Lrest:
  movss xmm1, [rdx]
  movss xmm2, [rax]
  mulss xmm1, xmm0
  addss xmm1, xmm2
  movd [rax], xmm1
  add rax, 4
  add rdx, 4
  dec rcx
  jnz @Lrest
@end:
end;
{$else}
var pEnd: PByte;
begin
  pEnd := PByte(pX) + aCount * SizeOf(Single);
  while PByte(pX) < pEnd do begin
    pY^  := a * pX^  + pY^;
    Inc(pX);
    Inc(pY);
  end;
end;
{$endif}

procedure axpy(const a: TCmplx128; pX, pY: PCmplx128; aCount: NativeInt);
{$if defined(ASMx86)}
//EAX <- @a, EDX <- pX, ECX <- pY, [EBP + 8] <- aCount
asm
  movddup xmm3, [eax] // xmm3 <- (ar, ar)
  movddup xmm4, [eax + 8] // xmm4 <- (ai, ai)
  mov eax, ecx // EAX <- pY
  mov ecx, [ebp + 8]
  shr ecx, 1
  cmp ecx, 0
  jz @rest
@L:
  movupd xmm0, [edx]  // xmm0 <- (xr, xi)
  movapd xmm1, xmm3
  movapd xmm2, xmm4
  mulpd xmm1, xmm0   // xmm1 <- (xr*ar, xi*ar)
  mulpd xmm2, xmm0   // xmm2 <- (xr*ai, xi*ai)
  shufpd xmm2, xmm2, 1 // xmm3 <- (xi*ai, xr*ai)
  addsubpd xmm1, xmm2 // xmm1 <- a * x
  movupd xmm0, [eax]  // xmm0 <- y
  addpd xmm0, xmm1    // xmm0 <- y + a * x
  movupd [eax], xmm0  // y <- y + a * y
  add eax, 16
  add edx, 16

  movupd xmm0, [edx]
  movapd xmm1, xmm3
  movapd xmm2, xmm4
  mulpd xmm1, xmm0
  mulpd xmm2, xmm0
  shufpd xmm2, xmm2, 1
  addsubpd xmm1, xmm2
  movupd xmm0, [eax]
  addpd xmm0, xmm1
  movupd [eax], xmm0
  add eax, 16
  add edx, 16

  dec ecx
  jnz @L

@rest:
  mov ecx, [ebp + 8]
  and ecx, 1
  jz @end

  movupd xmm0, [edx]
  movapd xmm1, xmm3
  movapd xmm2, xmm4
  mulpd xmm1, xmm0
  mulpd xmm2, xmm0
  shufpd xmm2, xmm2, 1
  addsubpd xmm1, xmm2
  movupd xmm0, [eax]
  addpd xmm0, xmm1
  movupd [eax], xmm0
@end:
end;
{$elseif defined(ASMx64)}
{$CODEALIGN 16}
// RCX <- @a, RDX <- pX, R8 <- pY, R9 <- aCount
asm
{$ifdef AVX}
  vbroadcastsd ymm3, [rcx]     // ymm3 <- (ar, ar, ar, ar)
  vbroadcastsd ymm4, [rcx + 8] // ymm4 <- (ai, ai, ai, ai)
{$else}
  movddup xmm3, [rcx] // xmm3 <- (ar, ar)
  movddup xmm4, [rcx + 8] // xmm4 <- (ai, ai)
{$endif}
  mov rcx, r9
  shr rcx, 1
  cmp rcx, 0
  jz @rest
{$ifdef AVX}
@L:
  vmovups ymm0, [rdx] // ymm0 <- (xr[i], xi[i], xr[i+1], xi[i+1])
  vmovaps ymm1, ymm3
  vmovaps ymm2, ymm4
  vmulpd ymm1, ymm1, ymm0
  vmulpd ymm2, ymm2, ymm0
  vshufpd ymm2, ymm2, ymm2, 5
  vaddsubpd ymm1, ymm1, ymm2
  vmovupd ymm0, [r8]
  vaddpd ymm0, ymm0, ymm1
  vmovupd [r8], ymm0
  add rdx, 32
  add r8, 32
  dec rcx
  jnz @L
{$else}
@L:
  movupd xmm0, [rdx]  // xmm0 <- (xr, xi)
  movapd xmm1, xmm3
  movapd xmm2, xmm4
  mulpd xmm1, xmm0   // xmm1 <- (xr*ar, xi*ar)
  mulpd xmm2, xmm0   // xmm2 <- (xr*ai, xi*ai)
  shufpd xmm2, xmm2, 1 // xmm3 <- (xi*ai, xr*ai)
  addsubpd xmm1, xmm2 // xmm1 <- a * x
  movupd xmm0, [r8]  // xmm0 <- y
  addpd xmm0, xmm1    // xmm0 <- y + a * x
  movupd [r8], xmm0  // y <- y + a * y
  add rdx, 16
  add r8, 16

  movupd xmm0, [rdx]
  movapd xmm1, xmm3
  movapd xmm2, xmm4
  mulpd xmm1, xmm0
  mulpd xmm2, xmm0
  shufpd xmm2, xmm2, 1
  addsubpd xmm1, xmm2
  movupd xmm0, [r8]
  addpd xmm0, xmm1
  movupd [r8], xmm0
  add rdx, 16
  add r8, 16

  dec rcx
  jnz @L
{$endif}

@rest:
{$ifdef AVX}
  vzeroupper
{$endif}
  and r9, 1
  jz @end

  movupd xmm0, [rdx]
  movapd xmm1, xmm3
  movapd xmm2, xmm4
  mulpd xmm1, xmm0
  mulpd xmm2, xmm0
  shufpd xmm2, xmm2, 1
  addsubpd xmm1, xmm2
  movupd xmm0, [r8]
  addpd xmm0, xmm1
  movupd [r8], xmm0
@end:
end;
{$else}
var pEnd: PByte;
begin
  pEnd := PByte(pX) + aCount * SizeOf(TCmplx128);

  if (a.Re = 1) and (a.Im = 0) then begin
    while PByte(pX) < pEnd do begin
      with pY^ do begin
        Re := Re + pX^.Re;
        Im := Im + pX^.Im;
      end;
      Inc(pX);
      Inc(pY);
    end;
  end else begin
    while PByte(pX) < pEnd do begin
      with pY^ do begin
        Re := Re + pX^.Re * a.Re - pX^.Im * a.Im;
        Im := Im + pX^.Re * a.Im + pX^.Im * a.Re;
      end;
      Inc(pX);
      Inc(pY);
    end;
  end;
end;
{$endif}

procedure dot(pX, pY: PSingle; aCount: NativeInt; out aRes: Single);
{$if defined(ASMx86)}
asm
  test ecx, ecx
  jns @Begin
  call RaiseLengthOutOfRange

@Begin:
  push edi
  push esi

  mov esi, eax         // Move pA to ESI (source pointer A)
  mov edi, edx         // Move pB to EDI (source pointer B)

  xorps xmm0, xmm0    // Clear accumulator

  // Check alignment
  test esi, 15
  test edi, 15
  jnz @Unaligned      // Jump if either pointer is unaligned

  // Process 8 elements per loop (aligned memory)
  mov eax, ecx
  shr ecx, 3          // Divide count by 8
  jz @Remainder

@LoopAligned:
  prefetchnta [esi+64] // Prefetch next cache line from pA
  prefetchnta [edi+64] // Prefetch next cache line from pB

  movups xmm1, [esi]
  movups xmm2, [edi]
  mulps xmm1, xmm2 // xmm0 <- X * Y
  addps xmm0, xmm1
  add esi, 16
  add edi, 16

  movups xmm1, [esi]
  movups xmm2, [edi]
  mulps xmm1, xmm2 // xmm0 <- X * Y
  addps xmm0, xmm1
  add esi, 16
  add edi, 16

  dec ecx
  jnz @LoopAligned

  jmp @Remainder      // Jump to handle remaining elements

@Unaligned:
  mov eax, ecx
  shr ecx, 3          // Divide count by 8
  jz @Remainder

@LoopUnaligned:
  prefetchnta [esi+64] // Prefetch next cache line from pA
  prefetchnta [edi+64] // Prefetch next cache line from pB

  movups xmm1, [esi]
  movups xmm2, [edi]
  mulps xmm1, xmm2 // xmm0 <- X * Y
  addps xmm0, xmm1
  add esi, 16
  add edi, 16

  movups xmm1, [esi]
  movups xmm2, [edi]
  mulps xmm1, xmm2 // xmm0 <- X * Y
  addps xmm0, xmm1
  add esi, 16
  add edi, 16

  dec ecx
  jnz @LoopUnaligned

@Remainder:
  // Process remaining elements (1-7)
  and eax, 7          // Get remainder count
  jz @Finish

  xorps xmm1, xmm1  // Clear xmm1 for scalar sum
@ScalarLoop:
  movss xmm2, [esi] // Load single pA[i]
  movss xmm3, [edi] // Load single pB[i]
  mulss xmm2, xmm3  // Multiply
  addss xmm1, xmm2  // Accumulate

  add esi, 4
  add edi, 4
  dec eax
  jnz @ScalarLoop

  addss xmm0, xmm1  // Add scalar sum to vector sum

@Finish:
  // Horizontal sum of xmm0
  movhlps xmm1, xmm0
  addps xmm0, xmm1
  movaps xmm1, xmm0
  shufps xmm1, xmm1, 1
  addss xmm0, xmm1

  mov eax, [ebp + 8]  // Load aResult pointer from stack
  movss [eax], xmm0 // Store result in aResult

  pop esi
  pop edi
end;
{$elseif defined(ASMx64)}
{$CODEALIGN 16}
//RCX <- pA, RDX <- pB, R8 <- aCount, R9 <- pRes
asm
  test r8, r8
  jns @begin
  call RaiseLengthOutOfRange

@begin:
  push rsi
  push rdi
  mov rsi, rcx
  mov rdi, rdx
  mov rcx, r8
  xorpd xmm0, xmm0
  shr rcx, 3
  jz @rest

{$ifdef AVX}
  vxorpd ymm0, ymm0, ymm0
@L:
  vmovups ymm1, [rsi]
  vmovups ymm2, [rdi]
  vmulps ymm1, ymm1, ymm2 // ymm1 <- X * Y
  vaddps ymm0, ymm0, ymm1
  add rsi, 32
  add rdi, 32
  dec rcx
  jnz @L

  vextractf128 xmm1, ymm0, 1
  vaddps xmm0, xmm0, xmm1

  vzeroupper
{$else}
@L:
  movups xmm1, [rsi]
  movups xmm2, [rdi]
  mulps xmm1, xmm2 // xmm0 <- X * Y
  addps xmm0, xmm1
  add rsi, 16
  add rdi, 16

  movups xmm1, [rsi]
  movups xmm2, [rdi]
  mulps xmm1, xmm2 // xmm0 <- X * Y
  addps xmm0, xmm1
  add rsi, 16
  add rdi, 16

  dec rcx
  jnz @L
{$endif}

  // Horizontal sum of xmm0
  movhlps xmm1, xmm0
  addps xmm0, xmm1
  movaps xmm1, xmm0
  shufps xmm1, xmm1, 1
  addss xmm0, xmm1

@rest:
  and r8, 7
  jz @end

@Lrest:
  movss xmm1, [rsi]
  movss xmm2, [rdi]
  mulss xmm1, xmm2
  addss xmm0, xmm1
  add rsi, 4
  add rdi, 4
  dec r8
  jnz @Lrest

@end:
  movss [r9], xmm0

  pop rdi
  pop rsi
end;
{$else}
var pEnd: PByte;
begin
  aRes := 0;
  pEnd := PByte(pX) + aCount * SizeOf(Single);
  while PByte(pX) < pEnd do begin
    aRes := aRes + pX^ * pY^;
    Inc(pX);
    Inc(pY);
  end;
end;
{$endif}

procedure dot(pX, pY: PDouble; aCount: NativeInt; out aRes: Double);
{$if defined(ASMx86)}
//EAX <- pX, EDX <- pY, ECX <- aCount; [EBP + 8] <- aRes
asm
  test ecx, ecx
  jns @begin
  call RaiseLengthOutOfRange

@begin:
  push esi
  push edi
  mov esi, eax
  mov edi, edx
  mov eax, ecx
  xorpd xmm0, xmm0
  shr ecx, 1
  jz @rest
@L:
  movupd xmm1, [esi]
  movupd xmm2, [edi]
  mulpd xmm1, xmm2 // xmm0 <- X * Y
  addpd xmm0, xmm1
  add esi, 16
  add edi, 16
  dec ecx
  jnz @L

@rest:
  and eax, 1
  jz @end

  movsd xmm1, [esi]
  movsd xmm2, [edi]
  mulsd xmm1, xmm2
  addsd xmm0, xmm1

@end:
  movhlps xmm1, xmm0
  addsd xmm0, xmm1
  mov eax, [ebp + 8]
  movlpd [eax], xmm0

  pop edi
  pop esi
end;
{$elseif defined(ASMx64)}
//RCX <- pX, RDX <- pY, R8 <- aCount; R9 <- aRes
asm
  test r8, r8
  jns @begin
  call RaiseLengthOutOfRange

@begin:
  mov rax, rcx
  mov rcx, r8
  xorpd xmm0, xmm0
  shr rcx, 1
  jz @rest
@L:
  movupd xmm1, [rax]
  movupd xmm2, [rdx]
  mulpd xmm1, xmm2 // xmm0 <- X * Y
  addpd xmm0, xmm1
  add rax, 16
  add rdx, 16
  dec rcx
  jnz @L

@rest:
  and r8, 1
  jz @end

  movsd xmm1, [rax]
  movsd xmm2, [rdx]
  mulsd xmm1, xmm2
  addsd xmm0, xmm1

@end:
  movhlps xmm1, xmm0
  addsd xmm0, xmm1
  movlpd [r9], xmm0
end;
{$else}
var pEnd: PByte;
begin
  aRes := 0;
  pEnd := PByte(pX) + aCount * SizeOf(Double);
  while PByte(pX) < pEnd do begin
    aRes := aRes + pX^ * pY^;
    Inc(pX);
    Inc(pY);
  end;
end;
{$endif}

procedure dot(pX, pY: PCmplx128; aCount: NativeInt; out aRes: TCmplx128);
{$if defined(ASMx86)}
//EAX <- pX, EDX <- pY, ECX <- aCount; [EBP + 8] <- aRes
asm
  xorpd xmm0, xmm0
  test ecx, ecx
  jns @L
  call RaiseLengthOutOfRange
@L:
  movddup xmm2, [eax]     // xmm2 <- (ar, ar)
  movddup xmm3, [eax + 8] // xmm3 <- (ai, ai)
  movupd xmm1, [edx]      // xmm1 <- (br, bi)
  mulpd xmm2, xmm1        // xmm2 <- (br*ar, bi*ar)
  mulpd xmm3, xmm1        // xmm3 <- (br*ai, bi*ai)
  pshufd xmm3, xmm3, $4e  // xmm3 <- (bi*ai, br*ai)
  addsubpd xmm2, xmm3     // xmm2 <- a * b
  addpd xmm0, xmm2
  add eax, 16
  add edx, 16
  dec ecx
  jnz @L

  mov eax, [ebp + 8]
  movupd [eax], xmm0
end;
{$elseif defined(ASMx64)}
// RCX <- pX, RDX <- pY, R8 <- aCount, R9 <- @aRes
asm
  xorpd xmm0, xmm0
  test r8, r8
  jns @L
  call RaiseLengthOutOfRange
@L:
  movddup xmm2, [rcx]     // xmm2 <- (ar, ar)
  movddup xmm3, [rcx + 8] // xmm3 <- (ai, ai)
  movupd xmm1, [rdx]      // xmm1 <- (br, bi)
  mulpd xmm2, xmm1        // xmm2 <- (br*ar, bi*ar)
  mulpd xmm3, xmm1        // xmm3 <- (br*ai, bi*ai)
  pshufd xmm3, xmm3, $4e  // xmm3 <- (bi*ai, br*ai)
  addsubpd xmm2, xmm3     // xmm2 <- a * b
  addpd xmm0, xmm2
  add rcx, 16
  add rdx, 16
  dec r8
  jnz @L

  movupd [r9], xmm0
end;
{$else}
var pEnd: PByte;
begin
  aRes.Init(0, 0);
  pEnd := PByte(pX) + aCount * SizeOf(TCmplx128);
  while PByte(pX) < pEnd do begin
    with aRes do begin
      Re := Re + pX^.Re * pY^.Re - pX^.Im * pY^.Im;
      Im := Im + pX^.Re * pY^.Im + pX^.Im * pY^.Re;
    end;
    Inc(pX);
    Inc(pY);
  end;
end;
{$endif}

function sdot(aN: NativeInt; pX: PByte; aXInc: NativeInt; pY: PByte; aYInc: NativeInt): Single;
var pEnd, pEndu: PByte;
begin
  if (aXInc <> SizeOf(Single)) or (aYInc <> SizeOf(Single)) then begin
    Result := 0;
    pEnd := pX + aN * aXInc;
    pEndu := pX + (aN and (not NativeInt(3))) * aXInc;
    while pX < pEndu do begin
      Result := Result + PSingle(pX)^ * PSingle(pY)^;
      Inc(pX, aXInc);
      Inc(pY, aYInc);
      Result := Result + PSingle(pX)^ * PSingle(pY)^;
      Inc(pX, aXInc);
      Inc(pY, aYInc);
      Result := Result + PSingle(pX)^ * PSingle(pY)^;
      Inc(pX, aXInc);
      Inc(pY, aYInc);
      Result := Result + PSingle(pX)^ * PSingle(pY)^;
      Inc(pX, aXInc);
      Inc(pY, aYInc);
    end;
    while pX < pEnd do begin
      Result := Result + PSingle(pX)^ * PSingle(pY)^;
      Inc(pX, aXInc);
      Inc(pY, aYInc);
    end;
  end else
    dot(PSingle(pX), PSingle(pY), aN, Result);
end;

function ddot(aN: NativeInt; pX: PByte; aXInc: NativeInt; pY: PByte; aYInc: NativeInt): Double;
var pEnd, pEndu: PByte;
begin
  if (aXInc <> SizeOf(Double)) or (aYInc <> SizeOf(Double)) then begin
    Result := 0;
    pEnd := pX + aN * aXInc;
    pEndu := pX + (aN and (not NativeInt(3))) * aXInc;
    while pX < pEndu do begin
      Result := Result + PDouble(pX)^ * PDouble(pY)^;
      Inc(pX, aXInc);
      Inc(pY, aYInc);
      Result := Result + PDouble(pX)^ * PDouble(pY)^;
      Inc(pX, aXInc);
      Inc(pY, aYInc);
      Result := Result + PDouble(pX)^ * PDouble(pY)^;
      Inc(pX, aXInc);
      Inc(pY, aYInc);
      Result := Result + PDouble(pX)^ * PDouble(pY)^;
      Inc(pX, aXInc);
      Inc(pY, aYInc);
    end;
    while pX < pEnd do begin
      Result := Result + PDouble(pX)^ * PDouble(pY)^;
      Inc(pX, aXInc);
      Inc(pY, aYInc);
    end;
  end else
    dot(PDouble(pX), PDouble(pY), aN, Result);
end;

procedure Differences(pX, pRes: PDouble; aCount: NativeInt);
var pXn: PDouble;
    pEnd: PByte;
begin
  pXn := pX;
  Inc(pXn);
  pEnd := PByte(pX) + aCount * SizeOf(Double);
  while PByte(pXn) < pEnd do begin
    pRes^ := pXn^ - pX^;
    Inc(pRes);
    pX := pXn;
    Inc(pXn);
  end;
end;

procedure Differences(pX, pRes: PSingle; aCount: NativeInt);
{$if defined(ASMx86)}
asm
  // EAX <- pX, EDX <- pRes, ECX <- aCount
  push ebx
  dec ecx
  mov ebx, ecx
  shr ecx, 2
  jz @rest

@mainLoop:
  movups xmm0, [eax]
  movups xmm1, [eax + 4]
  subps xmm1, xmm0
  movups [edx], xmm1
  add eax, 16
  add edx, 16
  dec ecx
  jnz @mainLoop

@rest:
  mov ecx, ebx
  and ecx, 3
  jz @end

@restLoop:
  movss xmm0, [eax]
  movss xmm1, [eax + 4]
  subss xmm1, xmm0
  movss [edx], xmm1
  add eax, 4
  add edx, 4
  dec ecx
  jnz @restLoop

@end:
  pop ebx
end;
{$elseif defined(ASMx64)}
asm
  // RCX <- pX, RDX <- pRes, R8 <- aCount
  mov rax, rcx
  dec r8
  mov rcx, r8
  shr rcx, 2
  jz @rest

@mainLoop:
  movups xmm0, [rax]
  movups xmm1, [rax + 4]
  subps xmm1, xmm0
  add rax, 16
  movups [rdx], xmm1
  add rdx, 16
  dec rcx
  jnz @mainLoop

@rest:
  mov rcx, r8
  and rcx, 3
  jz @end

@restLoop:
  movss xmm0, [rax]
  movss xmm1, [rax + 4]
  subss xmm1, xmm0
  movss [rdx], xmm1
  add rax, 4
  add rdx, 4
  dec rcx
  jnz @restLoop

@end:
end;
{$else}
var pXn: PSingle;
    pEnd: PByte;
begin
  pXn := pX;
  Inc(pXn);
  pEnd := PByte(pX) + aCount * SizeOf(Single);
  while PByte(pXn) < pEnd do begin
    pRes^ := pXn^ - pX^;
    Inc(pRes);
    pX := pXn;
    Inc(pXn);
  end;
end;
{$endif}

procedure Differences(pX, pRes: PDouble; aXStep, aResStep, aCount: NativeInt);
var pXn: PDouble;
    pEnd: PByte;
begin
  pXn := pX;
  Inc(pXn, aXStep);
  pEnd := PByte(pX) + aCount * SizeOf(Double);
  while PByte(pXn) < pEnd do begin
    pRes^ := pXn^ - pX^;
    Inc(pRes, aResStep);
    pX := pXn;
    Inc(pXn, aXStep);
  end;
end;

procedure Differences(pX, pRes: PSingle; aXStep, aResStep, aCount: NativeInt);
{$if defined(ASMx86)}
asm
  // EAX <- pX, EDX <- pRes, ECX <- aXStep, [EBP + 8] <- aCount, [EBP + 12] <- aResStep
  push ebx
  push esi
  push edi
  mov esi, eax // ESI <- pX
  mov edi, edx // EDI <- pRes

  mov eax, [ebp + 8]
  dec eax
  xor edx, edx
  div ecx
  mov ebx, eax // ECX <- (aCount - 1) div aXStep

  mov eax, ecx // EAX <- aXStep
  mov edx, [ebp + 12] // EDX <- aResStep
  shl eax, 2
  shl edx, 2
  mov ecx, ebx
  shr ecx, 2
  jz @rest

@mainLoop:
  movss xmm0, [esi]
  add esi, eax
  movss xmm2, [esi]
  insertps xmm0, xmm2, 10h
  insertps xmm1, xmm2, 00h
  add esi, eax
  movss xmm2, [esi]
  insertps xmm0, xmm2, 20h
  insertps xmm1, xmm2, 10h
  add esi, eax
  movss xmm2, [esi]
  insertps xmm0, xmm2, 30h
  insertps xmm1, xmm2, 20h
  add esi, eax
  movss xmm2, [esi]
  insertps xmm1, xmm2, 30h

  subps xmm1, xmm0 // xmm1 <- X[i .. i+3] - X[i+1 .. i+4]

  movss [edi], xmm1
  add edi, edx
  extractps [edi], xmm1, 1
  add edi, edx
  extractps [edi], xmm1, 2
  add edi, edx
  extractps [edi], xmm1, 3
  add edi, edx

  dec ecx
  jnz @mainLoop

@rest:
  mov ecx, ebx
  and ecx, 3
  jz @end

@restLoop:
  movss xmm0, [esi]
  add esi, eax
  movss xmm1, [esi]
  subss xmm1, xmm0
  movss [edi], xmm1
  add edi, edx
  dec ecx
  jnz @restLoop

@end:
  pop edi
  pop esi
  pop ebx
end;
{$else}
var pXn: PSingle;
    pEnd: PByte;
begin
  pXn := pX;
  Inc(pXn, aXStep);
  pEnd := PByte(pX) + aCount * SizeOf(Single);
  while PByte(pXn) < pEnd do begin
    pRes^ := pXn^ - pX^;
    Inc(pRes, aResStep);
    pX := pXn;
    Inc(pXn, aXStep);
  end;
end;
{$endif}

function SquaredEuclideanDistance(pX, pY: PDouble; aCount: NativeInt): Double;
var pEnd: PByte;
    d: Double;
begin
  Assert(aCount > 0);
  Result := 0;
  pEnd := PByte(pX) + aCount * SizeOf(Double);
  while PByte(pX) < pEnd do begin
    d := pX^ - pY^;
    Result := Result + d * d;
    Inc(pX);
    Inc(pY);
  end;
end;

function SquaredEuclideanDistance(pX, pY: PSingle; aCount: NativeInt): Single;
var pEnd: PByte;
    d: Single;
begin
  Assert(aCount > 0);
  Result := 0;
  pEnd := PByte(pX) + aCount * SizeOf(Single);
  while PByte(pX) < pEnd do begin
    d := pX^ - pY^;
    Result := Result + d * d;
    Inc(pX);
    Inc(pY);
  end;
end;

function Norm(pX: PDouble; aCount: NativeInt; p: Double = 2): Double;
var pEnd: PByte;
    v: Double;
begin
  Assert((aCount > 0) and (p >= 1));
  pEnd := PByte(pX) + aCount * SizeOf(Double);
  Result := 0;
  if SameValue(p, 1) then begin
    while PByte(pX) < pEnd do begin
      Result := Result + Abs(pX^);
      Inc(pX);
    end;
  end else
  if p = Infinity then begin
    while PByte(pX) < pEnd do begin
      v := Abs(pX^);
      if v > Result then
        Result := v;
      Inc(pX);
    end;
  end else begin
    while PByte(pX) < pEnd do begin
      Result := Result + Power(Abs(pX^), p);
      Inc(pX);
    end;
    Result := Power(Result, 1/p);
  end;
end;

function Norm(const X: TArray<Double>; p: Double = 2): Double;
begin
  Result := Norm(PDouble(X), Length(X), p);
end;

function Norm(pX: PSingle; aCount: NativeInt; p: Single = 2): Single;
var pEnd: PByte;
    v: Double;
begin
  Assert((aCount > 0) and (p >= 1));
  pEnd := PByte(pX) + aCount * SizeOf(Single);
  Result := 0;
  if SameValue(p, 1) then begin
    while PByte(pX) < pEnd do begin
      Result := Result + Abs(pX^);
      Inc(pX);
    end;
  end else
  if p = Infinity then begin
    while PByte(pX) < pEnd do begin
      v := Abs(pX^);
      if v > Result then
        Result := v;
      Inc(pX);
    end;
  end else begin
    while PByte(pX) < pEnd do begin
      Result := Result + Power(Abs(pX^), p);
      Inc(pX);
    end;
    Result := Power(Result, 1/p);
  end;
end;

function Norm(const X: TArray<Single>; p: Single = 2): Single;
begin
  Result := Norm(PSingle(X), Length(X), p);
end;

{$ifdef RANGEON}
   {$R-}
{$endif}

{$ifdef OVERFLOWON}
  {$Q-}
{$endif}

function SatAdd_U8(a, b: UInt8): UInt8;
begin
  Result := a + b;
  Result := (Result or (-Int8(Result < a)));
end;

function SatSub_U8(a, b: UInt8): UInt8;
begin
  Result := a - b;
  Result := (Result and (-Int8(Result <= a)));
end;

function Min_2U8(a, b: UInt8): UInt8;
{$if defined(zzASMx64)}
asm
  // RCX <- a, RDX <- b
  mov al, dl // AL <- b
  sub al, cl // AL <- b - a
  setnc cl
  neg cl     // CL <- -(AL <= b)
  and al, cl
  sub dl, al
  mov al, dl
end;
//asm
//  movzx   eax, cl       // zero-extend a
//  movzx   edx, dl       // zero-extend b
//  cmp     eax, edx      // unsigned compare
//  cmova   eax, edx      // if a > b, eax := b
//end;
{$else}
begin
  // Result := b - Max(b - a, 0)
  Result := b - a;
  Result := Result and (-Int8(Result <= b)); // Result <- Max(b - a, 0)
  Result := b - Result;
end;
{$endif}

function Min_3U8(a, b, c: UInt8): UInt8;
begin
  Result := b - a;
  Result := Result and (-Int8(Result <= b));
  Result := b - Result;
  Result := c - Result;
  Result := Result and (-Int8(Result <= c));
  Result := c - Result;
end;

function Min_4U8(a, b, c, d: UInt8): UInt8;
begin
  Result := b - a;
  Result := Result and (-Int8(Result <= b));
  Result := b - Result;
  Result := c - Result;
  Result := Result and (-Int8(Result <= c));
  Result := c - Result;
  Result := d - Result;
  Result := Result and (-Int8(Result <= d));
  Result := d - Result;
end;

function Min_NU8(pA: PUInt8; N: NativeInt): UInt8;
{$if defined(ASMx64)}
asm
  // RCX <- pA, RDX <- N, Return in AL.
  test    rdx, rdx
  jnz     @init
  xor     eax, eax            // N=0 -> return 0
  ret

@init:
  mov     r8, rcx             // r8 := ptr
  pcmpeqb xmm0, xmm0          // xmm0 := 0xFF in all lanes (start max for unsigned)

@loop16:
  cmp     rdx, 16
  jb      @reduce

  movdqu  xmm1, [r8]          // load 16 bytes (unaligned ok)
  pminub  xmm0, xmm1          // running per-byte min
  add     r8, 16
  sub     rdx, 16
  jmp     @loop16

@reduce:
  // Horizontal reduce xmm0 (16 lanes) -> 1 byte in AL
  movdqa  xmm1, xmm0
  psrldq  xmm1, 8
  pminub  xmm0, xmm1
  movdqa  xmm1, xmm0
  psrldq  xmm1, 4
  pminub  xmm0, xmm1
  movdqa  xmm1, xmm0
  psrldq  xmm1, 2
  pminub  xmm0, xmm1
  movdqa  xmm1, xmm0
  psrldq  xmm1, 1
  pminub  xmm0, xmm1

  movd    eax, xmm0           // AL now holds min so far (or 0xFF if no blocks)

  // Tail: rdx = remaining (<16). Compare scalarly (unsigned).
  test    rdx, rdx
  jz      @end

@rest:
  movzx   r9d, byte ptr [r8]  // load next byte to r9d
  cmp     eax, r9d
  cmova   eax, r9d            // if AL > byte, AL := byte (unsigned)
  inc     r8
  dec     rdx
  jnz     @rest

@end:
  ret
end;
{$else}
begin
  Result := pA^;
  while N > 1 do begin
    Inc(pA);
    if pA^ < Result then
      Result := pA^;
    Dec(N);
  end;
end;
{$endif}

function Max_2U8(a, b: UInt8): UInt8;
begin
  Result := a - b;
  Result := Result and (-Int8(Result <= a)); // Result <- Max(a - b, 0);
  Result := b + Result;
end;

{$ifdef RANGEON}
  {$R+}
{$endif}

{$ifdef OVERFLOWON}
  {$Q+}
{$endif}

end.
