unit panda.cvCmp;

interface

{$I AsmDefs.inc}

// pRes <- X < Y
procedure CmpLT(pX: PInteger; Y: Integer; pRes: PBoolean; aCount: NativeInt); overload;
procedure CmpLT(pX, pY: PInteger; pRes: PBoolean; aCount: NativeInt); overload;

// pRes <- X <= Y
procedure CmpLTE(pX: PInteger; Y: Integer; pRes: PBoolean; aCount: NativeInt); overload;
procedure CmpLTE(pX, pY: PInteger; pRes: PBoolean; aCount: NativeInt); overload;

// pRes <- X > Y
procedure CmpGT(pX: PInteger; Y: Integer; pRes: PBoolean; aCount: NativeInt); overload;
procedure CmpGT(pX, pY: PInteger; pRes: PBoolean; aCount: NativeInt); overload;

// pRes <- X >= Y
procedure CmpGTE(pX: PInteger; Y: Integer; pRes: PBoolean; aCount: NativeInt); overload;
procedure CmpGTE(pX, pY: PInteger; pRes: PBoolean; aCount: NativeInt); overload;

procedure VecThreshold(pX, pRes: PByte; aT: Byte; aCount: NativeInt); overload;
procedure VecThreshold(pX, pRes, pT: PByte; aCount: NativeInt); overload;
procedure VecThreshold(pX, pRes: PSingle; aT: Single; aCount: NativeInt); overload;
procedure VecThreshold(pX, pRes, pT: PSingle; aCount: NativeInt); overload;

procedure VecThresholdInv(pX, pRes: PByte; aT: Byte; aCount: NativeInt); overload;

procedure VecBinarize(pX, pRes: PByte; aT: Byte; aCount: NativeInt); overload;
procedure VecBinarize(pX, pRes, pT: PSingle; aCount: NativeInt); overload;

procedure VecBinarizeInv(pX, pRes: PByte; aT: Byte; aCount: NativeInt); overload;

implementation

uses
    panda.Nums
  ;

const
  cOnesUI8x16: array [0..15] of Byte = (  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1);
  cSgnsUI8x16: array [0..15] of Byte = ($80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80);

  cOnesF32x4: array [0..3] of Single = (1, 1, 1, 1);
  cFullMask:  array [0..3] of Cardinal = ($FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF);

procedure CmpLT(pX: PInteger; Y: Integer; pRes: PBoolean; aCount: NativeInt);
{$if defined(ASMx64)}
// RCX <- pX, RDX <- Y, R8 <- pRes, R9 <- aCount
asm
  mov r10, r9
  shr r9, 3
  jz @rest
  movd xmm0, edx
  movss xmm1, xmm0
  pshufd xmm1, xmm0, 0 // xmm1 <- 4 x Y
  movdqu xmm4, cOnesUI8x16
@L:
  movdqu xmm0, [rcx]
  movdqa xmm2, xmm1
  pcmpgtd xmm2, xmm0
  add rcx, 16

  movdqu xmm0, [rcx]
  movdqa xmm3, xmm1
  pcmpgtd xmm3, xmm0
  add rcx, 16

  packssdw xmm2, xmm2
  packssdw xmm3, xmm3
  movlhps xmm2, xmm3
  packsswb xmm2, xmm2
  pand xmm2, xmm4
  movq [r8], xmm2
  add r8, 8

  dec r9
  jnz @L

@rest:
  and r10, 7
  jz @end
@Lrest:
  movzx rax, [rcx]
  cmp rax, rdx
  jl @cmpLT
  mov byte ptr [r8], 0
  jmp @cmpEnd
@cmpLT:
  mov byte ptr [r8], 1
@cmpEnd:
  add rcx, 4
  inc r8
  dec r10
  jnz @LRest
@end:
end;
{$else}
var pEnd: PByte;
begin
  pEnd := PByte(pRes) + aCount;
  while PByte(pRes) < pEnd do begin
    pRes^ := (pX^ < Y);
    Inc(pRes);
    Inc(pX);
  end;
end;
{$endif}

procedure CmpLT(pX, pY: PInteger; pRes: PBoolean; aCount: NativeInt);
{$if defined(ASMx64)}
// RCX <- pX, RDX <- pY, R8 <- pRes, R9 <- aCount
asm
  mov r10, r9
  shr r9, 3
  jz @rest
  movdqu xmm4, cOnesUI8x16
@L:
  movdqu xmm0, [rcx]
  movdqu xmm1, [rdx]
  pcmpgtd xmm1, xmm0
  add rcx, 16
  add rdx, 16

  movdqu xmm0, [rcx]
  movdqa xmm2, [rdx]
  pcmpgtd xmm2, xmm0
  add rcx, 16
  add rdx, 16

  packssdw xmm1, xmm1
  packssdw xmm2, xmm2
  movlhps xmm1, xmm2
  packsswb xmm1, xmm1
  pand xmm1, xmm4
  movq [r8], xmm1
  add r8, 8

  dec r9
  jnz @L

@rest:
  and r10, 7
  jz @end
@Lrest:
  movzx rax, [rcx]
  movzx r9, [rdx]
  cmp rax, r9
  jl @cmpLT
  mov byte ptr [r8], 0
  jmp @cmpEnd
@cmpLT:
  mov byte ptr [r8], 1
@cmpEnd:
  add rcx, 4
  add rdx, 4
  inc r8
  dec r10
  jnz @LRest
@end:
end;
{$else}
var pEnd: PByte;
begin
  pEnd := PByte(pRes) + aCount;
  while PByte(pRes) < pEnd do begin
    pRes^ := (pX^ < pY^);
    Inc(pRes);
    Inc(pX);
    Inc(pY);
  end;
end;
{$endif}

procedure CmpLTE(pX: PInteger; Y: Integer; pRes: PBoolean; aCount: NativeInt);
{$if defined(ASMx64)}
// (X <= Y) = !(Y > X)
// RCX <- pX, RDX <- Y, R8 <- pRes, R9 <- aCount
asm
  mov r10, r9
  shr r9, 3
  jz @rest
  movd xmm0, edx
  movss xmm1, xmm0
  pshufd xmm1, xmm0, 0 // xmm1 <- 4 x Y
  movdqu xmm4, cOnesUI8x16
@L:
  movdqu xmm2, [rcx]
  pcmpgtd xmm2, xmm1
  add rcx, 16

  movdqu xmm3, [rcx]
  pcmpgtd xmm3, xmm1
  add rcx, 16

  packssdw xmm2, xmm2
  packssdw xmm3, xmm3
  movlhps xmm2, xmm3
  packsswb xmm2, xmm2
  pand xmm2, xmm4
  pxor xmm2, xmm4
  movq [r8], xmm2
  add r8, 8

  dec r9
  jnz @L

@rest:
  and r10, 7
  jz @end
@Lrest:
  movzx rax, [rcx]
  cmp rax, rdx
  jg @GT
  mov byte ptr [r8], 1
  jmp @cmpEnd
@GT:
  mov byte ptr [r8], 0
@cmpEnd:
  add rcx, 4
  inc r8
  dec r10
  jnz @LRest
@end:
end;
{$else}
var pEnd: PByte;
begin
  pEnd := PByte(pRes) + aCount;
  while PByte(pRes) < pEnd do begin
    pRes^ := (pX^ <= Y);
    Inc(pRes);
    Inc(pX);
  end;
end;
{$endif}

procedure CmpLTE(pX, pY: PInteger; pRes: PBoolean; aCount: NativeInt);
{$if defined(ASMx64)}
// (X <= Y) = !(Y > X)
// RCX <- pX, RDX <- pY, R8 <- pRes, R9 <- aCount
asm
  mov r10, r9
  shr r9, 3
  jz @rest
  movdqu xmm4, cOnesUI8x16
@L:
  movdqu xmm0, [rdx]
  movdqu xmm1, [rcx]
  pcmpgtd xmm1, xmm0
  add rcx, 16
  add rdx, 16

  movdqu xmm0, [rdx]
  movdqa xmm2, [rcx]
  pcmpgtd xmm2, xmm0
  add rcx, 16
  add rdx, 16

  packssdw xmm1, xmm1
  packssdw xmm2, xmm2
  movlhps xmm1, xmm2
  packsswb xmm1, xmm1
  pand xmm1, xmm4
  pxor xmm1, xmm4
  movq [r8], xmm1
  add r8, 8

  dec r9
  jnz @L

@rest:
  and r10, 7
  jz @end
@Lrest:
  movzx rax, [rdx]
  movzx r9, [rcx]
  cmp rax, r9
  jl @cmpLT
  mov byte ptr [r8], 1
  jmp @cmpEnd
@cmpLT:
  mov byte ptr [r8], 0
@cmpEnd:
  add rcx, 4
  add rdx, 4
  inc r8
  dec r10
  jnz @LRest
@end:
end;
{$else}
var pEnd: PByte;
begin
  pEnd := PByte(pRes) + aCount;
  while PByte(pRes) < pEnd do begin
    pRes^ := (pX^ <= pY^);
    Inc(pRes);
    Inc(pX);
    Inc(pY);
  end;
end;
{$endif}

procedure CmpGT(pX: PInteger; Y: Integer; pRes: PBoolean; aCount: NativeInt);
{$if defined(ASMx64)}
// RCX <- pX, RDX <- Y, R8 <- pRes, R9 <- aCount
asm
  mov r10, r9
  shr r9, 3
  jz @rest
  movd xmm0, edx
  movss xmm1, xmm0
  pshufd xmm1, xmm0, 0 // xmm1 <- 4 x Y
  movdqu xmm4, cOnesUI8x16
@L:
  movdqu xmm2, [rcx]
  pcmpgtd xmm2, xmm1
  add rcx, 16

  movdqu xmm3, [rcx]
  pcmpgtd xmm3, xmm1
  add rcx, 16

  packssdw xmm2, xmm2
  packssdw xmm3, xmm3
  movlhps xmm2, xmm3
  packsswb xmm2, xmm2
  pand xmm2, xmm4
  movq [r8], xmm2
  add r8, 8

  dec r9
  jnz @L

@rest:
  and r10, 7
  jz @end
@Lrest:
  movzx rax, [rcx]
  cmp rax, rdx
  jg @GT
  mov byte ptr [r8], 0
  jmp @cmpEnd
@GT:
  mov byte ptr [r8], 1
@cmpEnd:
  add rcx, 4
  inc r8
  dec r10
  jnz @LRest
@end:
end;
{$else}
var pEnd: PByte;
begin
  pEnd := PByte(pRes) + aCount;
  while PByte(pRes) < pEnd do begin
    pRes^ := (pX^ > Y);
    Inc(pRes);
    Inc(pX);
  end;
end;
{$endif}

procedure CmpGT(pX, pY: PInteger; pRes: PBoolean; aCount: NativeInt);
{$if defined(ASMx64)}
// RCX <- pX, RDX <- pY, R8 <- pRes, R9 <- aCount
asm
  mov r10, r9
  shr r9, 3
  jz @rest
  movdqu xmm4, cOnesUI8x16
@L:
  movdqu xmm0, [rdx]
  movdqu xmm1, [rcx]
  pcmpgtd xmm1, xmm0
  add rcx, 16
  add rdx, 16

  movdqu xmm0, [rdx]
  movdqa xmm2, [rcx]
  pcmpgtd xmm2, xmm0
  add rcx, 16
  add rdx, 16

  packssdw xmm1, xmm1
  packssdw xmm2, xmm2
  movlhps xmm1, xmm2
  packsswb xmm1, xmm1
  pand xmm1, xmm4
  movq [r8], xmm1
  add r8, 8

  dec r9
  jnz @L

@rest:
  and r10, 7
  jz @end
@Lrest:
  movzx rax, [rdx]
  movzx r9, [rcx]
  cmp rax, r9
  jl @cmpLT
  mov byte ptr [r8], 0
  jmp @cmpEnd
@cmpLT:
  mov byte ptr [r8], 1
@cmpEnd:
  add rcx, 4
  add rdx, 4
  inc r8
  dec r10
  jnz @LRest
@end:
end;
{$else}
var pEnd: PByte;
begin
  pEnd := PByte(pRes) + aCount;
  while PByte(pRes) < pEnd do begin
    pRes^ := (pX^ > pY^);
    Inc(pRes);
    Inc(pX);
    Inc(pY);
  end;
end;
{$endif}

procedure CmpGTE(pX: PInteger; Y: Integer; pRes: PBoolean; aCount: NativeInt);
{$if defined(ASMx64)}
// (X >= Y) = !(X < Y)
// RCX <- pX, RDX <- pY, R8 <- pRes, R9 <- aCount
asm
  mov r10, r9
  shr r9, 3
  jz @rest
  movd xmm0, edx
  movss xmm1, xmm0
  pshufd xmm1, xmm0, 0 // xmm1 <- 4 x Y
  movdqu xmm4, cOnesUI8x16
@L:
  movdqu xmm0, [rcx]
  movdqa xmm2, xmm1
  pcmpgtd xmm2, xmm0
  add rcx, 16

  movdqu xmm0, [rcx]
  movdqa xmm3, xmm1
  pcmpgtd xmm3, xmm0
  add rcx, 16

  packssdw xmm2, xmm2
  packssdw xmm3, xmm3
  movlhps xmm2, xmm3
  packsswb xmm2, xmm2
  pand xmm2, xmm4
  pxor xmm2, xmm4
  movq [r8], xmm2
  add r8, 8

  dec r9
  jnz @L

@rest:
  and r10, 7
  jz @end
@Lrest:
  movzx rax, [rcx]
  cmp rax, rdx
  jl @cmpLT
  mov byte ptr [r8], 1
  jmp @cmpEnd
@cmpLT:
  mov byte ptr [r8], 0
@cmpEnd:
  add rcx, 4
  inc r8
  dec r10
  jnz @LRest
@end:
end;
{$else}
var pEnd: PByte;
begin
  pEnd := PByte(pRes) + aCount;
  while PByte(pRes) < pEnd do begin
    pRes^ := (pX^ >= Y);
    Inc(pRes);
    Inc(pX);
  end;
end;
{$endif}

procedure CmpGTE(pX, pY: PInteger; pRes: PBoolean; aCount: NativeInt);
{$if defined(ASMx64)}
// (X >= Y) = !(X < Y)
// RCX <- pX, RDX <- pY, R8 <- pRes, R9 <- aCount
asm
  mov r10, r9
  shr r9, 3
  jz @rest
  movdqu xmm4, cOnesUI8x16
@L:
  movdqu xmm0, [rcx]
  movdqu xmm1, [rdx]
  pcmpgtd xmm1, xmm0
  add rcx, 16
  add rdx, 16

  movdqu xmm0, [rcx]
  movdqa xmm2, [rdx]
  pcmpgtd xmm2, xmm0
  add rcx, 16
  add rdx, 16

  packssdw xmm1, xmm1
  packssdw xmm2, xmm2
  movlhps xmm1, xmm2
  packsswb xmm1, xmm1
  pand xmm1, xmm4
  pxor xmm1, xmm4
  movq [r8], xmm1
  add r8, 8

  dec r9
  jnz @L

@rest:
  and r10, 7
  jz @end
@Lrest:
  movzx rax, [rcx]
  movzx r9, [rdx]
  cmp rax, r9
  jl @cmpLT
  mov byte ptr [r8], 1
  jmp @cmpEnd
@cmpLT:
  mov byte ptr [r8], 0
@cmpEnd:
  add rcx, 4
  add rdx, 4
  inc r8
  dec r10
  jnz @LRest
@end:
end;
{$else}
var pEnd: PByte;
begin
  pEnd := PByte(pRes) + aCount;
  while PByte(pRes) < pEnd do begin
    pRes^ := (pX^ >= pY^);
    Inc(pRes);
    Inc(pX);
    Inc(pY);
  end;
end;
{$endif}

procedure VecThreshold(pX, pRes: PByte; aT: Byte; aCount: NativeInt);
{$if defined(ASMx64)}
// RCX <- pX, RDX <- pRes, R8 <- aT, R9 <- aCount
asm
  mov r10, r9
  shr r9, 4
  jz @rest
  movd xmm0, r8d
  punpcklbw xmm0, xmm0
  punpcklwd xmm0, xmm0
  shufps xmm0, xmm0, 0
  movdqu xmm4, cSgnsUI8x16
  pxor xmm0, xmm4
@L:
  movdqu xmm1, [rcx]
  movdqa xmm2, xmm1
  pxor xmm2, xmm4
  pcmpgtb xmm2, xmm0
  pand xmm1, xmm2
  movdqu [rdx], xmm1
  add rcx, 16
  add rdx, 16
  dec r9
  jnz @L

@rest:
  and r10, 15
  jz @end
  and r8, $FF
@Lrest:
  movzx eax, byte ptr [rcx]
  cmp eax, r8d
  jg @SelX
  mov byte ptr [rdx], 0
  jmp @SelEnd
@SelX:
  mov byte ptr [rdx], al
@SelEnd:
  inc rcx
  inc rdx
  dec r10
  jnz @Lrest
@end:
end;
{$else}
var pEnd: PByte;
begin
  pEnd := pX + aCount;
  while pX < pEnd do begin
    if pX^ > aT then
      pRes^ := pX^
    else
      pRes^ := 0;
    Inc(pRes);
    Inc(pX);
  end;
end;
{$endif}

procedure VecThreshold(pX, pRes, pT: PByte; aCount: NativeInt);
var  pEnd: PByte;
begin
  pEnd := pX + aCount;
  while pX < pEnd do begin
    if pX^ > pT^ then
      pRes^ := pX^
    else
      pRes^ := 0;
    Inc(pRes);
    Inc(pX);
    Inc(pT);
  end;
end;

procedure VecThreshold(pX, pRes: PSingle; aT: Single; aCount: NativeInt);
var pEnd: PByte;
begin
  pEnd := PByte(pX) + aCount * cF32Sz;
  while PByte(pX) < pEnd do begin
    if pX^ > aT then
      pRes^ := pX^
    else
      pRes^ := 0;
    Inc(pRes);
    Inc(pX);
  end;
end;

procedure VecThreshold(pX, pRes, pT: PSingle; aCount: NativeInt);
var pEnd: PByte;
begin
  pEnd := PByte(pX) + aCount * cF32Sz;
  while PByte(pX) < pEnd do begin
    if pX^ > pT^ then
      pRes^ := pX^
    else
      pRes^ := 0;
    Inc(pRes);
    Inc(pX);
    Inc(pT);
  end;
end;

procedure VecThresholdInv(pX, pRes: PByte; aT: Byte; aCount: NativeInt);
{$if defined(ASMx64)}
// RCX <- pX, RDX <- pRes, R8 <- aT, R9 <- aCount
asm
  mov r10, r9
  shr r9, 4
  jz @rest
  movd xmm0, r8d
  punpcklbw xmm0, xmm0
  punpcklwd xmm0, xmm0
  shufps xmm0, xmm0, 0
  movdqu xmm4, cSgnsUI8x16
  pxor xmm0, xmm4
  movdqu xmm1, cOnesUI8x16
  paddb xmm0, xmm1
@L:
  movdqu xmm1, [rcx]
  movdqa xmm2, xmm1
  pxor xmm2, xmm4
  movdqa xmm3, xmm0
  pcmpgtb xmm3, xmm2
  pand xmm1, xmm3
  movdqu [rdx], xmm1
  add rcx, 16
  add rdx, 16
  dec r9
  jnz @L

@rest:
  and r10, 15
  jz @end
  and r8, $FF
@Lrest:
  movzx eax, byte ptr [rcx]
  cmp eax, r8d
  jle @SelX
  mov byte ptr [rdx], 0
  jmp @SelEnd
@SelX:
  mov byte ptr [rdx], al
@SelEnd:
  inc rcx
  inc rdx
  dec r10
  jnz @Lrest
@end:
end;
{$else}
var pEnd: PByte;
begin
  pEnd := pX + aCount;
  while pX < pEnd do begin
    if pX^ > aT then
      pRes^ := 0
    else
      pRes^ := pX^;
    Inc(pRes);
    Inc(pX);
  end;
end;
{$endif}

procedure VecBinarize(pX, pRes: PByte; aT: Byte; aCount: NativeInt);
{$if defined(ASMx64)}
// RCX <- pX, RDX <- pRes, R8 <- aT, R9 <- aCount
asm
  mov r10, r9
  shr r9, 4
  jz @rest
  movd xmm0, r8d
  punpcklbw xmm0, xmm0
  punpcklwd xmm0, xmm0
  shufps xmm0, xmm0, 0
  movdqu xmm4, cSgnsUI8x16
  pxor xmm0, xmm4
@L:
  movdqu xmm1, [rcx]
  pxor xmm1, xmm4
  pcmpgtb xmm1, xmm0
  movdqu [rdx], xmm1
  add rcx, 16
  add rdx, 16
  dec r9
  jnz @L

@rest:
  and r10, 15
  jz @end
  and r8, $FF
@Lrest:
  movzx eax, byte ptr [rcx]
  cmp eax, r8d
  jg @SelX
  mov byte ptr [rdx], 0
  jmp @SelEnd
@SelX:
  mov byte ptr [rdx], 255
@SelEnd:
  inc rcx
  inc rdx
  dec r10
  jnz @Lrest
@end:
end;
{$else}
var pEnd: PByte;
begin
  pEnd := pX + aCount;
  while pX < pEnd do begin
    if pX^ > aT then
      pRes^ := 255
    else
      pRes^ := 0;
    Inc(pRes);
    Inc(pX);
  end;
end;
{$endif}

procedure VecBinarize(pX, pRes, pT: PSingle; aCount: NativeInt);
{$if defined(ASMx64)}
// RCX <- pX, RDX <- pRes, R8 <- pT, R9 <- aCount
asm
  mov r10, r9
  shr r9, 3
  jz @rest
  movups xmm4, cOnesF32x4
@L:
  movups xmm0, [rcx]
  movups xmm1, [r8]
  cmpnleps xmm0, xmm1
  pand xmm0, xmm4
  movups [rdx], xmm0
  add rcx, 16
  add rdx, 16
  add r8, 16

  movups xmm0, [rcx]
  movups xmm1, [r8]
  cmpnleps xmm0, xmm1
  pand xmm0, xmm4
  movups [rdx], xmm0
  add rcx, 16
  add rdx, 16
  add r8, 16

  dec r9
  jnz @L
@rest:
  and r10, 7
  jz @end
@Lrest:
  movss xmm0, [rcx]
  movss xmm1, [r8]
  cmpnless xmm0, xmm1
  pand xmm0, xmm4
  movss [rdx], xmm0
  add rcx, 4
  add rdx, 4
  add r8, 4
  dec r10
  jnz @Lrest
@end:
end;
{$else}
var pEnd: PByte;
begin
  pEnd := PByte(pX) + aCount * cF32Sz;
  while PByte(pX) < pEnd do begin
    if pX^ > pT^ then
      pRes^ := 1
    else
      pRes^ := 0;
    Inc(pRes);
    Inc(pX);
    Inc(pT);
  end;
end;
{$endif}

procedure VecBinarizeInv(pX, pRes: PByte; aT: Byte; aCount: NativeInt);
{$if defined(ASMx64)}
// RCX <- pX, RDX <- pRes, R8 <- aT, R9 <- aCount
asm
  mov r10, r9
  shr r9, 4
  jz @rest
  movd xmm0, r8d
  punpcklbw xmm0, xmm0
  punpcklwd xmm0, xmm0
  shufps xmm0, xmm0, 0
  movdqu xmm4, cSgnsUI8x16
  pxor xmm0, xmm4
  movdqu xmm1, cOnesUI8x16
  paddb xmm0, xmm1
@L:
  movdqu xmm1, [rcx]
  pxor xmm1, xmm4
  movdqu xmm2, xmm0
  pcmpgtb xmm2, xmm1
  movdqu [rdx], xmm2
  add rcx, 16
  add rdx, 16
  dec r9
  jnz @L

@rest:
  and r10, 15
  jz @end
  and r8, $FF
@Lrest:
  movzx eax, byte ptr [rcx]
  cmp eax, r8d
  jg @SelX
  mov byte ptr [rdx], 255
  jmp @SelEnd
@SelX:
  mov byte ptr [rdx], 0
@SelEnd:
  inc rcx
  inc rdx
  dec r10
  jnz @Lrest
@end:
end;
{$else}
var pEnd: PByte;
begin
  pEnd := pX + aCount;
  while pX < pEnd do begin
    if pX^ > aT then
      pRes^ := 0
    else
      pRes^ := 255;
    Inc(pRes);
    Inc(pX);
  end;
end;
{$endif}

end.
