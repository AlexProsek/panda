unit panda.cvCvt;

interface

{$I AsmDefs.inc}

procedure cvt(pSrc: PByte; pDst: PSingle; aCount: NativeInt); overload;
procedure cvt(pSrc: PSingle; pDst: PByte; aCount: NativeInt); overload;
procedure cvt(pSrc: PInteger; pDst: PSingle; aCount: NativeInt); overload;
procedure cvt(pSrc: PInteger; pDst: PInt64; aCount: NativeInt); overload;
procedure cvt(pSrc: PInt64; pDst: PDouble; aCount: NativeInt); overload;
procedure cvt(pSrc: PSingle; pDst: PDouble; aCount: NativeInt); overload;
procedure cvt(pSrc: PDouble; pDst: PSingle; aCount: NativeInt); overload;

procedure bswap16(pSrc, pDst: PByte; aCount: NativeInt);
procedure bswap32(pSrc, pDst: PByte; aCount: NativeInt);
procedure bswap64(pSrc, pDst: PByte; aCount: NativeInt);

implementation

uses
    System.SysUtils
  ;

procedure bswap16(pSrc, pDst: PByte; aCount: NativeInt);
{$if defined(ASMx86)}
asm
  push esi
  push edi
  mov esi, eax  // ESI <- pSrc
  mov edi, edx  // EDI <- pDst
  mov edx, ecx  // EDX <- aCount
  shr ecx, 2
  jz @rest
@L:
  mov eax, [esi]
  bswap eax
  ror eax, 16
  mov [edi], eax
  add esi, 4
  add edi, 4
  dec ecx
  jnz @L
@rest:
  mov ecx, edx
  and ecx, 3
  jz @end
  mov ax, [esi]
  xchg ah, al
  mov [edi], ax
@end:
  pop edi
  pop esi
end;
{$elseif defined(ASMx64)}
// RCX <- pSrc, RDX <- pDst, R8 <- aCount
asm
  push rsi
  push rdi
  mov rsi, rcx
  mov rdi, rdx
  mov rcx, r8
  shr rcx, 2
  jz @rest
@L:
  mov eax, [rsi]
  bswap eax
  ror eax, 16
  mov [rdi], eax
  add rsi, 4
  add rdi, 4
  dec rcx
  jnz @L
@rest:
  mov rcx, r8
  and rcx, 3
  jz @end
  mov ax, [rsi]
  xchg ah, al
  mov [rdi], ax
@end:
  pop rsi
  pop rdi
end;
{$else}
var i: Integer;
    pEnd: PByte;
begin
  pEnd := pSrc + (aCount and (not NativeInt(1)));
  while pSrc < pEnd do begin
    i := PWord(pSrc)^;
    i := swap(i);
    PWord(pDst)^ := i;
    Inc(pSrc, 2);
    Inc(pDst, 2);
  end;
end;
{$endif}

procedure bswap32(pSrc, pDst: PByte; aCount: NativeInt);
{$if defined(ASMx86)}
asm
  push esi
  push edi
  mov esi, eax // ESI <- aSrc
  mov edi, edx // EDI <- aDst
  shr ecx, 2
  jz @end
@L:
  mov eax, [esi]
  bswap eax
  mov [edi], eax
  add esi, 4
  add edi, 4
  dec ecx
  jnz @L
@end:
  pop edi
  pop esi
end;
{$elseif defined(ASMx64)}
// RCX <- pSrc, RDX <- pDst, R8 <- aCount
asm
  push rsi
  push rdi
  mov rsi, rcx
  mov rdi, rdx
  mov rcx, r8
  shr rcx, 2
  jz @end
@L:
  mov eax, [rsi]
  bswap eax
  mov [rdi], eax
  add rsi, 4
  add rdi, 4
  dec rcx
  jnz @L
@end:
  pop rsi
  pop rdi
end;
{$else}
type PLongRec = ^LongRec;
var i: Cardinal;
    pEnd: PByte;
begin
  pEnd := pSrc + (aCount and (not NativeInt(3)));
  while pSrc < pEnd do begin
    i := PCardinal(pSrc)^;
    PCardinal(pDst)^ := (swap(i) shl 16) or (swap(i shr 16));
    Inc(pSrc, 4);
    Inc(pDst, 4);
  end;
end;
{$endif}

procedure bswap64(pSrc, pDst: PByte; aCount: NativeInt);
{$if defined(ASMx86)}
asm
  push esi
  push edi
  mov esi, eax  // ESI <- aSrc
  mov edi, edx  // EDI <- aDst
  shr ecx, 3
  jz @end
@L:
  mov eax, [esi]
  add esi, 4
  bswap eax
  mov edx, eax
  mov eax, [esi]
  add esi, 4
  bswap eax
  mov [edi], eax
  add edi, 4
  mov [edi], edx
  add edi, 4
  dec ecx
  jnz @L
@end:
  pop edi
  pop esi
end;
{$elseif defined(ASMx64)}
// RCX <- pSrc, RDX <- pDst, R8 <- aCount
asm
  push rsi
  push rdi
  mov rsi, rcx
  mov rdi, rdx
  mov rcx, r8
  shr rcx, 3
  jz @end
@L:
  mov rax, [rsi]
  bswap rax
  mov [rdi], rax
  add rsi, 8
  add rdi, 8
  dec rcx
  jnz @L
@end:
  pop rsi
  pop rdi
end;
{$else}
type PInt64Rec = ^Int64Rec;
var i1, i2: Cardinal;
    pEnd: PByte;
begin
  pEnd := pSrc + (aCount and (not NativeInt(7)));
  while pSrc < pEnd do begin
    i1 := PCardinal(pSrc)^;
    Inc(pSrc, 4);
    i2 := PCardinal(pSrc)^;
    Inc(pSrc, 4);
    PCardinal(pDst)^ := (swap(i2) shl 16) or (swap(i2 shr 16));
    Inc(pDst, 4);
    PCardinal(pDst)^ := (swap(i1) shl 16) or (swap(i1 shr 16));
    Inc(pDst, 4);
  end;
end;
{$endif}

procedure cvt(pSrc: PByte; pDst: PSingle; aCount: NativeInt);
{$if defined(ASMx64)}
// RCX <- pSrc, RDX <- pDst, R8 <- aCount
asm
  xorps xmm0, xmm0
      //load Color into xmm6
  mov rax, r8
  shr rax, 2
  jz @rest
@L:
  movd xmm1, [rcx]
  punpcklbw xmm1, xmm0 // byte -> word
  punpcklbw xmm1, xmm0 // word -> integer
  cvtdq2ps xmm1, xmm1  // integer -> single
  movups [rdx], xmm1
  add rcx, 4
  add rdx, 16
  dec rax
  jnz @L
@rest:
  and r8, 3
  jz @end
  xor rax, rax
@Lrest:
  mov al, [rcx]
  movd xmm0, eax
  cvtdq2ps xmm0, xmm0
  movss [rdx], xmm0
  add rcx, 1
  add rdx, 4
  dec r8
  jnz @Lrest
@end:
end;
{$else}
var pEnd: PByte;
begin
  pEnd := pSrc + aCount;
  while pSrc < pEnd do begin
    pDst^ := pSrc^;
    Inc(pSrc);
    Inc(pDst);
  end;
end;
{$endif}

procedure cvt(pSrc: PSingle; pDst: PByte; aCount: NativeInt);
{$if defined(ASMx64)}
// RCX <- pSrc, RDX <- pDst, R8 <- aCount
const cMax: Single = 255;
asm
  xorps xmm0, xmm0
  movss xmm1, cMax
  shufps xmm1, xmm1, 0   // xmm1 <- (255, 255, 255, 255)
  mov rax, r8
  shr rax, 2
  jz @rest
@L:
  movups xmm2, [rcx]
  maxps xmm2, xmm0
  minps xmm2, xmm1
  cvtps2dq xmm2, xmm2
  packssdw xmm2, xmm0 // dwords -> words
  packuswb xmm2, xmm0 // words -> bytes
  movd [rdx], xmm2
  add rcx, 16
  add rdx, 4
  dec rax
  jnz @L
@rest:
  and r8, 3
  jz @end
@Lrest:
  movd xmm2, [rcx]
  maxss xmm2, xmm0
  minss xmm2, xmm1
  cvtps2dq xmm2, xmm2
  movd eax, xmm2
  mov [rdx], al
  add rcx, 4
  add rdx, 1
  dec r8
  jnz @Lrest
@end:
end;
{$else}
var pEnd: PByte;
    s: NativeInt;
begin
  pEnd := pDst + aCount;
  while pDst < pEnd do begin
    s := Round(pSrc^);
    if s < 0 then
      pDst^ := 0
    else
    if s > 255 then
      pDst^ := 255
    else
      pDst^ := s;
    Inc(pSrc);
    Inc(pDst);
  end;
end;
{$endif}

procedure cvt(pSrc: PInteger; pDst: PSingle; aCount: NativeInt);
{$if defined(ASMx64)}
// RCX <- pSrc, RDX <- pDst, R8 <- aCount
asm
  mov rax, r8
  shr rax, 2
  jz @rest
@L:
  cvtdq2ps xmm0, [rcx]
  movupd [rdx], xmm0
  add rcx, 16
  add rdx, 16
  dec rax
  jnz @L
@rest:
  mov rax, r8
  and rax, 3
  jz @end
@Lrest:
  cvtsi2ss xmm0, [rcx]
  movss [rdx], xmm0
  add rcx, 4
  add rdx, 4
  dec rax
  jnz @Lrest
@end:
end;
{$else}
var pEnd: PByte;
begin
  pEnd := PByte(pSrc) + aCount * SizeOf(Integer);
  while PByte(pSrc) < pEnd do begin
    pDst^ := pSrc^;
    Inc(pSrc);
    Inc(pDst);
  end;
end;
{$endif}

procedure cvt(pSrc: PInteger; pDst: PInt64; aCount: NativeInt);
{$if defined(ASMx64)}
// RCX <- pSrc, RDX <- pDst, R8 <- aCount
asm
  mov rax, r8
  shr rax, 2
  jz @rest
@L:
  movq xmm0, [rcx]     // load 2xInt32
  pmovsxdq xmm0, xmm0  // sign-extend to 2xInt64
  movdqu [rdx], xmm0   // store 2xInt64
  add rcx, 8
  add rdx, 16
  movq xmm0, [rcx]
  pmovsxdq xmm0, xmm0
  movdqu [rdx], xmm0
  add rcx, 8
  add rdx, 16
  dec rax
  jnz @L
@rest:
  and r8, 3
  jz @end
@Lrest:
  mov eax, [rcx]
  cdqe
  mov [rdx], rax
  add rcx, 4
  add rdx, 8
  dec r8
  jnz @Lrest
@end:
end;
{$else}
var pEnd: PByte;
begin
  pEnd := PByte(pSrc) + aCount * SizeOf(Integer);
  while PByte(pSrc) < pEnd do begin
    pDst^ := pSrc^;
    Inc(pSrc);
    Inc(pDst);
  end;
end;
{$endif}

procedure cvt(pSrc: PInt64; pDst: PDouble; aCount: NativeInt);
{$if defined(ASMx64)}
{$ifdef AVX}
  {$define AVXCVT_I64F64}
{$endif}
// RCX <- pSrc, RDX <- pDst, R8 <- aCount
const
  cSgnBit32Mask: UInt32 = $80000000;
  cTwo31: Double = 2147483648.0;
  cTwo32: Double = 4294967296.0;
asm
  sub rsp, 56 // 48 for xmm backup + 8 stack alignment
  movupd [rsp], xmm6
  movupd [rsp + 16], xmm7
  movupd [rsp + 32], xmm8

  mov r9, r8
  shr r8, 2
  jz @rest
{$ifdef AVXCVT_I64F64}
  vmovd xmm8, cSgnBit32Mask
  vpshufd ymm8, ymm8, 0
  vmovddup xmm7, cTwo32
  vinsertf128 ymm7, ymm7, xmm7, 1
  vmovddup xmm6, cTwo31
  vinsertf128 ymm6, ymm6, xmm6, 1
{$else}
  movd xmm8, cSgnBit32Mask
  pshufd xmm8, xmm8, 0
  movddup xmm7, cTwo32
  movddup xmm6, cTwo31
{$endif}
@L:
{$ifdef AVXCVT_I64F64}
  vmovdqu ymm0, [rcx]         // ymm0 <- x[k:k+3]
  vpshufd ymm1, ymm0, $08     // ymm1 <- (x[k:k+1].lo, ?, ?, x[k+2:k+3].lo, ?, ?)
  vextracti128 xmm2, ymm1, 1  // xmm2 <- x[k+2:k+3].lo
  vmovlhps xmm1, xmm1, xmm2   // xmm1 <- x[k:k+3].lo
  vpxor ymm1, ymm1, ymm8
  vcvtdq2pd ymm2, xmm1        // ymm2 <- Double(x[k:k+3].lo)
  vaddpd ymm2, ymm2, ymm6     // ymm2 <- ymm2 * 2^31

  vpshufd ymm1, ymm0, $0D     // ymm1 <- (x[k:k+1].hi, ?, ?, x[k+2:k+3].hi, ?, ?)
  vextracti128 xmm3, ymm1, 1  // xmm3 <- x[k+2:k+3].hi
  vmovlhps xmm1, xmm1, xmm3   // xmm1 <- x[k:k+3].hi
  vcvtdq2pd ymm3, xmm1        // ymm3 <- Double(x[k:k+3].hi)
  vmulpd ymm3, ymm3, ymm7     // ymm3 <- ymm3 * 2^32
  vaddpd ymm2, ymm2, ymm3
  vmovupd [rdx], ymm2
{$else}
  movdqu xmm0, [rcx]        // xmm0 <- (x[k], x[k+1])
  pshufd xmm1, xmm0, $08    // xmm1 <- (x[k].lo, x[k+1].lo)
  pxor xmm1, xmm8
  cvtdq2pd xmm1, xmm1       // xmm1 < (Double(x[k].lo), Double(x[k+1].lo))
  addpd xmm1, xmm6          // xmm1 <- xmm1 * 2^31

  pshufd xmm2, xmm0, $0D    // xmm2 <- (x[k].hi, x[k+1].hi)
  cvtdq2pd xmm2, xmm2       // xmm2 <- (Double(x[k].hi), Doulbe(x[k+1].hi))
  mulpd xmm2, xmm7          // xmm2 <- xmm2 * 2^32
  addpd xmm1, xmm2          // xmm1 <- (Double(x[k]), Double(x[k+1]))
  movupd [rdx], xmm1

  movdqu xmm3, [rcx + 16]   // xmm3 <- (x[k+2], x[k+3])
  pshufd xmm4, xmm3, $08    // xmm4 <- (x[k+2].lo, x[k+3].lo)
  pxor xmm4, xmm8
  cvtdq2pd xmm4, xmm4       // xmm4 < (Double(x[k+2].lo), Double(x[k+3].lo))
  addpd xmm4, xmm6          // xmm4 <- xmm4 * 2^31

  pshufd xmm5, xmm3, $0D    // xmm5 <- (x[k+2].hi, x[k+3].hi)
  cvtdq2pd xmm5, xmm5       // xmm5 <- (Double(x[k+2].hi), Doulbe(x[k+3].hi))
  mulpd xmm5, xmm7          // xmm5 <- xmm5 * 2^32
  addpd xmm4, xmm5          // xmm4 <- (Double(x[k+2]), Double(x[k+3]))
  movupd [rdx + 16], xmm4
{$endif}
  add rcx, 32
  add rdx, 32
  dec r8
  jnz @L

{$ifdef AVXCVT_I64F64}
  vzeroupper
{$endif}

@rest:
  and r9, 3
  jz @end
@Lrest:
  mov rax, [rcx]
  cvtsi2sd xmm0, rax
  movsd [rdx], xmm0
  add rcx, 8
  add rdx, 8
  dec r9
  jnz @Lrest
@end:
  movupd xmm8, [rsp + 32]
  movupd xmm7, [rsp + 16]
  movupd xmm6, [rsp]
  add rsp, 56
end;
{$else}
var pEnd: PByte;
begin
  pEnd := PByte(pSrc) + aCount * SizeOf(Int64);
  while PByte(pSrc) < pEnd do begin
    pDst^ := pSrc^;
    Inc(pSrc);
    Inc(pDst);
  end;
end;
{$endif}

procedure cvt(pSrc: PSingle; pDst: PDouble; aCount: NativeInt);
{$if defined(ASMx64)}
// RCX <- pSrc, RDX <- pDst, R8 <- aCount
asm
  mov rax, r8
  shr rax, 2
  jz @rest
@L:
  cvtps2pd xmm0, [rcx]
  movupd [rdx], xmm0
  add rcx, 8
  add rdx, 16
  cvtps2pd xmm0, [rcx]
  movupd [rdx], xmm0
  add rcx, 8
  add rdx, 16
  dec rax
  jnz @L

@rest:
  and r8, 3
  jz @end
@Lrest:
  cvtss2sd xmm0, [rcx]
  movsd [rdx], xmm0
  add rcx, 4
  add rdx, 8
  dec r8
  jnz @Lrest
@end:
end;
{$else}
var pEnd: PByte;
begin
  pEnd := PByte(pSrc) + aCount * SizeOf(Single);
  while PByte(pSrc) < pEnd do begin
    pDst^ := pSrc^;
    Inc(pSrc);
    Inc(pDst);
  end;
end;
{$endif}

procedure cvt(pSrc: PDouble; pDst: PSingle; aCount: NativeInt);
{$if defined(ASMx64)}
// RCX <- pSrc, RDX <- pDst, R8 <- aCount
asm
  mov rax, r8
  shr rax, 2
  jz @rest
@L:
  cvtpd2ps xmm0, [rcx]
  movsd [rdx], xmm0
  add rcx, 16
  add rdx, 8
  cvtpd2ps xmm0, [rcx]
  movsd [rdx], xmm0
  add rcx, 16
  add rdx, 8
  dec rax
  jnz @L
@rest:
  and r8, 3
  jz @end
@Lrest:
  cvtsd2ss xmm0, [rcx]
  movss [rdx], xmm0
  add rcx, 8
  add rdx, 4
  dec r8
  jnz @Lrest
@end:
end;
{$else}
var pEnd: PByte;
begin
  pEnd := PByte(pSrc) + aCount * SizeOf(Double);
  while PByte(pSrc) < pEnd do begin
    pDst^ := pSrc^;
    Inc(pSrc);
    Inc(pDst);
  end;
end;
{$endif}

end.
