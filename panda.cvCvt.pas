unit panda.cvCvt;

interface

{$I AsmDefs.inc}

procedure cvt(pSrc: PInteger; pDst: PSingle; aCount: NativeInt); overload;
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
