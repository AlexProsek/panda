unit panda.cvCvt;

interface

{$I AsmDefs.inc}

procedure cvt(pSrc: PInteger; pDst: PSingle; aCount: NativeInt); overload;
procedure cvt(pSrc: PSingle; pDst: PDouble; aCount: NativeInt); overload;
procedure cvt(pSrc: PDouble; pDst: PSingle; aCount: NativeInt); overload;

implementation

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
