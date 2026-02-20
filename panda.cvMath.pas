unit panda.cvMath;

interface

uses
    System.Math
  , panda.Nums
  ;

//{$define NoASM}

{$I AsmDefs.inc}

procedure cvRange(aData: PInteger; aCount: NativeInt; aLo, aStep: Integer); overload;
procedure cvRange(aData: PInt64; aCount: NativeInt; aLo, aStep: Int64); overload;
procedure cvRange(aData: PSingle; aCount: NativeInt; aLo, aStep: Single); overload;
procedure cvRange(aData: PDouble; aCount: NativeInt; aLo, aStep: Double); overload;

procedure cvExp(aData: PSingle; aCount: NativeInt); overload;
procedure cvExp(aData: PDouble; aCount: NativeInt); overload;

function cvTotal(aData: PInteger; aCount: NativeInt): Integer; overload;
function cvTotal(aData: PSingle; aCount: NativeInt): Single; overload;
function cvTotal(aData: PDouble; aCount: NativeInt): Double; overload;

procedure cvMinMax(pData: PSingle; aCount: NativeInt; var aMin, aMax: Single); overload;
procedure cvMinMax(pData: PDouble; aCount: NativeInt; var aMin, aMax: Double); overload;

procedure cvAbsMax(pData: PSingle; aCount: NativeInt; var aMax: Single); overload;
procedure cvAbsMax(pData: PDouble; aCount: NativeInt; var aMax: Double); overload;

implementation

uses
    panda.cvArithmetic
  ;

{$region 'cvRange'}

procedure cvRange(aData: PInteger; aCount: NativeInt; aLo, aStep: Integer);
var pEnd: PByte;
begin
  pEnd := PByte(aData) + aCount * SizeOf(Integer);
  while PByte(aData) < pEnd do begin
    aData^ := aLo;
    Inc(aLo, aStep);
    Inc(aData);
  end;
end;

procedure cvRange(aData: PInt64; aCount: NativeInt; aLo, aStep: Int64);
var pEnd: PByte;
begin
  pEnd := PByte(aData) + aCount * SizeOf(Int64);
  while PByte(aData) < pEnd do begin
    aData^ := aLo;
    Inc(aLo, aStep);
    Inc(aData);
  end;
end;

procedure cvRange(aData: PSingle; aCount: NativeInt; aLo, aStep: Single);
var pEnd: PByte;
begin
  pEnd := PByte(aData) + aCount * SizeOf(Single);
  while PByte(aData) < pEnd do begin
    aData^ := aLo;
    aLo := aLo + aStep;
    Inc(aData);
  end;
end;

procedure cvRange(aData: PDouble; aCount: NativeInt; aLo, aStep: Double);
var pEnd: PByte;
begin
  pEnd := PByte(aData) + aCount * SizeOf(Double);
  while PByte(aData) < pEnd do begin
    aData^ := aLo;
    aLo := aLo + aStep;
    Inc(aData);
  end;
end;

{$endregion}

{$region 'cvExp'}

procedure cvExp(aData: PSingle; aCount: NativeInt);
var pEnd: PByte;
begin
  pEnd := PByte(aData) + aCount * SizeOf(Single);
  while PByte(aData) < pEnd do begin
    aData^ := System.Exp(aData^);
    Inc(aData);
  end;
end;

procedure cvExp(aData: PDouble; aCount: NativeInt);
var pEnd: PByte;
begin
  pEnd := PByte(aData) + aCount * SizeOf(Double);
  while PByte(aData) < pEnd do begin
    aData^ := System.Exp(aData^);
    Inc(aData);
  end;
end;

{$endregion}

{$region 'cvTotal'}

function cvTotal(aData: PInteger; aCount: NativeInt): Integer;
{$if defined(ASMx64)}
// RCX <- aData, RDX <- aCount, RAX <- result
asm
  xorps xmm0, xmm0
  mov r8, rdx
  shr rdx, 3
  jz @rest
@L:
  movupd xmm1, [rcx]
  paddd xmm0, xmm1
  add rcx, 16
  movupd xmm1, [rcx]
  paddd xmm0, xmm1
  add rcx, 16
  dec rdx
  jnz @L

  // Horizontal sum of xmm0
  movhlps xmm1, xmm0
  paddd xmm0, xmm1
  movaps xmm1, xmm0
  shufps xmm1, xmm1, 1
  paddd xmm0, xmm1

@rest:
  and r8, 7
  jz @end
@Lrest:
  movss xmm1, [rcx]
  paddd xmm0, xmm1
  add rcx, 4
  dec r8
  jnz @Lrest

@end:
  movd eax, xmm0
end;
{$else}
var pEnd: PByte;
begin
  Result := 0;
  pEnd := PByte(aData) + aCount * SizeOf(Integer);
  while PByte(aData) < pEnd do begin
    Inc(Result, aData^);
    Inc(aData);
  end;
end;
{$endif}

function cvTotal(aData: PSingle; aCount: NativeInt): Single;
{$if defined(ASMx64)}
// RCX <- aData, RDX <- aCount, xmm0 <- result
asm
  xorps xmm0, xmm0
  mov r8, rdx
  shr rdx, 3
  jz @rest
@L:
  movups xmm1, [rcx]
  addps xmm0, xmm1
  add rcx, 16
  movups xmm1, [rcx]
  addps xmm0, xmm1
  add rcx, 16
  dec rdx
  jnz @L

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
  movss xmm1, [rcx]
  addss xmm0, xmm1
  add rcx, 4
  dec r8
  jnz @Lrest

@end:
end;
{$else}
var pEnd: PByte;
    res: Double;
begin
  res := 0;
  pEnd := PByte(aData) + aCount * SizeOf(Single);
  while PByte(aData) < pEnd do begin
    res := res + aData^;
    Inc(aData);
  end;
  Result := Single(res);
end;
{$endif}

function cvTotal(aData: PDouble; aCount: NativeInt): Double;
{$if defined(ASMx64)}
// RCX <- aData, RDX <- aCount, xmm0 <- result
asm
  xorps xmm0, xmm0
  mov r8, rdx
  shr rdx, 2
  jz @rest
@L:
  movups xmm1, [rcx]
  addpd xmm0, xmm1
  add rcx, 16
  movups xmm1, [rcx]
  addpd xmm0, xmm1
  add rcx, 16
  dec rdx
  jnz @L

  // Horizontal sum of xmm0
  movhlps xmm1, xmm0
  addsd xmm0, xmm1

@rest:
  and r8, 3
  jz @end
@Lrest:
  movsd xmm1, [rcx]
  addsd xmm0, xmm1
  add rcx, 8
  dec r8
  jnz @Lrest

@end:
end;
{$else}
var pEnd: PByte;
begin
  Result := 0;
  pEnd := PByte(aData) + aCount * SizeOf(Double);
  while PByte(aData) < pEnd do begin
    Result := Result + aData^;
    Inc(aData);
  end;
end;
{$endif}

{$endregion}

{$region 'cvMinMax'}

procedure cvMinMax(pData: PSingle; aCount: NativeInt; var aMin, aMax: Single);
{$if defined(ASMx86)}
asm
  // EAX <- pData, EDX <- aCount, ECX <- @aMin, [EBP + 8] <- @aMax
{$ifdef DEBUG}
  cmp edx, 0
  jg @begin
  call RaiseNotPositiveLength
{$endif}
@begin:
  push ecx

  movd xmm0, [eax] // init Min value
  movss xmm1, xmm0
  shufps xmm0, xmm1, 0 // xmm0 <- 4 x init Min value
  movapd xmm1, xmm0    // xmm1 <- 4 x init Max value

  mov ecx, edx
  shr ecx, 2
  jz @rest
@L:
  movups xmm2, [eax]
  minps xmm0, xmm2
  maxps xmm1, xmm2
  add eax, 16
  dec ecx
  jnz @L

  movhlps xmm2, xmm0
  minps xmm0, xmm2
  movsd xmm2, xmm0
  shufps xmm2, xmm0, $1
  minss xmm0, xmm2  // xmm0[0:31] <- current Min

  movhlps xmm2, xmm1
  maxps xmm1, xmm2
  movsd xmm2, xmm1
  shufps xmm2, xmm1, $1
  maxss xmm1, xmm2 // xmm1[0:31] <- current Max

@rest:
  and edx, 3
  jz @end
@Lrest:
  movd xmm2, [eax]
  minss xmm0, xmm2
  maxss xmm1, xmm2
  add eax, 4
  dec edx
  jnz @Lrest
@end:
  pop ecx
  movd [ecx], xmm0 // Min
  mov ecx, [ebp + 8]
  movd [ecx], xmm1 // Max
end;
{$elseif defined(ASMx64)}
asm
  // RCX <- pData, RDX <- aCount, R8 <- @aMin, R9 <- @aMax
{$ifdef DEBUG}
  cmp rdx, 0
  jg @begin
  sub rsp, 40
  call RaiseNotPositiveLength
{$endif}
@begin:
  mov rax, rcx
  movd xmm0, [rax] // init Min value
  movss xmm1, xmm0
  shufps xmm0, xmm1, 0 // xmm0 <- 4 x init Min value
  movapd xmm1, xmm0    // xmm1 <- 4 x init Max value

  mov rcx, rdx
  shr rcx, 2
  jz @rest
@L:
  movups xmm2, [rax]
  minps xmm0, xmm2
  maxps xmm1, xmm2
  add rax, 16
  dec rcx
  jnz @L

  movhlps xmm2, xmm0
  minps xmm0, xmm2
  movsd xmm2, xmm0
  shufps xmm2, xmm0, $1
  minss xmm0, xmm2  // xmm0[0:31] <- current Min

  movhlps xmm2, xmm1
  maxps xmm1, xmm2
  movsd xmm2, xmm1
  shufps xmm2, xmm1, $1
  maxss xmm1, xmm2 // xmm1[0:31] <- current Max

@rest:
  and rdx, 3
  jz @end
@Lrest:
  movd xmm2, [rax]
  minss xmm0, xmm2
  maxss xmm1, xmm2
  add rax, 4
  dec rdx
  jnz @Lrest
@end:
  movd [r8], xmm0 // Min
  movd [r9], xmm1 // Max
end;
{$else}
var pEnd: PByte;
    v: Single;
begin
  Assert(aCount > 0);
  aMin := pData^;
  aMax := aMin;
  pEnd := PByte(pData) + aCount * SizeOf(Single);
  Inc(pData);
  while PByte(pData) < pEnd do begin
    v := pData^;
    if v < aMin then
      aMin := v
    else
    if v > aMax then
      aMax := v;
    Inc(pData);
  end;
end;
{$endif}

procedure cvMinMax(pData: PDouble; aCount: NativeInt; var aMin, aMax: Double);
{$if defined(ASMx64)}
{$CODEALIGN 16}
// RCX <- pData, RDX <- aCount, R8 <- @aMin, R9 <- @aMax
asm
{$ifdef DEBUG}
  cmp rdx, 0
  jg @begin
  sub rsp, 40
  call RaiseNotPositiveLength
@begin:
{$endif}
  movddup xmm0, [rcx] // xmm0 <- pData[0] as min
  movapd xmm1, xmm0   // xmm1 <- pData[0] as max
  mov rax, rdx
  shr rdx, 2
  jz @rest
@L:
  movupd xmm2, [rcx]
  minpd xmm0, xmm2
  maxpd xmm1, xmm2
  add rcx, 16
  movupd xmm2, [rcx]
  minpd xmm0, xmm2
  maxpd xmm1, xmm2
  add rcx, 16
  dec rdx
  jnz @L

  movhlps xmm2, xmm0
  minsd xmm0, xmm2
  movhlps xmm2, xmm1
  maxsd xmm1, xmm2
@rest:
  and rax, 3
  jz @end
@Lrest:
  movq xmm2, [rcx]
  minsd xmm0, xmm2
  maxsd xmm1, xmm2
  add rcx, 8
  dec rax
  jnz @Lrest
@end:
  movq [r8], xmm0
  movq [r9], xmm1
end;
{$else}
var pEnd: PByte;
    v: Double;
begin
  Assert(aCount > 0);
  aMin := pData^;
  aMax := aMin;
  pEnd := PByte(pData) + aCount * SizeOf(Double);
  Inc(pData);
  while PByte(pData) < pEnd do begin
    v := pData^;
    if v < aMin then
      aMin := v
    else
    if v > aMax then
      aMax := v;
    Inc(pData);
  end;
end;
{$endif}

{$endregion}

{$region 'cvAbsMax'}

procedure cvAbsMax(pData: PSingle; aCount: NativeInt; var aMax: Single);
var pEnd: PByte;
    v: Single;
begin
  Assert(aCount > 0);
  pEnd := PByte(pData) + aCount * SizeOf(Single);
  aMax := Abs(pData^);
  Inc(pData);
  while PByte(pData) < pEnd do begin
    v := Abs(pData^);
    if v > aMax then
      aMax := v;
    Inc(pData);
  end;
end;

procedure cvAbsMax(pData: PDouble; aCount: NativeInt; var aMax: Double);
{$if defined(ASMx86)}
// EAX <- pData, EDX <- aCount, ECX <- @aMax
asm
{$ifdef DEBUG}
  cmp edx, 0
  jg @begin
  call RaiseNotPositiveLength
{$endif}
@begin:
  push edi
  mov edi, ecx
  movddup xmm0, [eax]
  movddup xmm1, cAbsMaskF64
  andpd xmm0, xmm1
  mov ecx, edx
  shr ecx, 2
  jz @rest
@L:
  movupd xmm2, [eax]
  andpd xmm2, xmm1
  maxpd xmm0, xmm2
  add eax, 16

  movupd xmm2, [eax]
  andpd xmm2, xmm1
  maxpd xmm0, xmm2
  add eax, 16
  dec ecx
  jnz @L

  movhlps xmm2, xmm0
  maxsd xmm0, xmm2
@rest:
  and edx, 3
  jz @end
@Lrest:
  movq xmm2, [eax]
  andpd xmm2, xmm1
  maxps xmm0, xmm2
  add eax, 8
  dec edx
  jnz @Lrest
@end:
  movq [edi], xmm0
  pop edi
end;
{$elseif defined(ASMx64)}
{$CODEALIGN 16}
// RCX <- pData, RDX <- aCount, R8 <- @aMax
asm
{$ifdef DEBUG}
  cmp rdx, 0
  jg @begin
  sub rsp, 40
  call RaiseNotPositiveLength
{$endif}
@begin:
  movddup xmm0, [rcx]
  movddup xmm1, cAbsMaskF64
  andpd xmm0, xmm1
  mov rax, rdx
  shr rax, 2
  jz @rest
@L:
  movupd xmm2, [rcx]
  andpd xmm2, xmm1
  maxpd xmm0, xmm2
  add rcx, 16

  movupd xmm2, [rcx]
  andpd xmm2, xmm1
  maxpd xmm0, xmm2
  add rcx, 16
  dec eax
  jnz @L

  movhlps xmm2, xmm0
  maxsd xmm0, xmm2
@rest:
  mov rax, rdx
  and rax, 3
  jz @end
@Lrest:
  movq xmm2, [rcx]
  andpd xmm2, xmm1
  maxps xmm0, xmm2
  add rcx, 8
  dec eax
  jnz @Lrest
@end:
  movq [r8], xmm0
end;
{$else}
var pEnd: PByte;
    v: Double;
begin
  Assert(aCount > 0);
  pEnd := PByte(pData) + aCount * SizeOf(Double);
  aMax := Abs(pData^);
  Inc(pData);
  while PByte(pData) < pEnd do begin
    v := Abs(pData^);
    if v > aMax then
      aMax := v;
    Inc(pData);
  end;
end;
{$endif}

{$endregion}

end.
