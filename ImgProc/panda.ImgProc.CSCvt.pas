unit panda.ImgProc.CSCvt;

interface

uses
    panda.Intfs
  , panda.Arrays
  , panda.ImgProc.Types
  , panda.ImgProc.Images
  ;

{$I AsmDefs.inc}

  procedure ColorConvert(const aSrc: IImage<TRGB24>; var aDst: IImage<Byte>); overload;
  procedure ColorConvert(const aSrc: IImage<TRGB24>; var aDst: IImage<Single>); overload;
  procedure ColorConvert(const aSrc: IImage<Byte>; var aDst: IImage<TRGB24>); overload;
  procedure ColorConvert(const aSrc: IImage<Byte>; var aDst: IImage<Single>); overload;
  procedure ColorConvert(const aSrc: IImage<Single>; var aDst: IImage<Byte>); overload;

type
  TCSUt = class
  public
    class function MatchQ<T>(const aImg: IImage): Boolean; inline;
  end;

{$region 'low-level functions'}

type
  TCSCvtProc = procedure (pSrc, pDst: PByte; aCount: NativeInt);

// RGB conversion
procedure cscvtRGB24ToUI8(pSrc, pDst: PByte; aCount: NativeInt);
procedure cscvtRGB24ToF32(pSrc, pDst: PByte; aCount: NativeInt);

// UI8 conversion
procedure cscvtUI8ToRGB24(pSrc, pDst: PByte; aCount: NativeInt);
procedure cscvtUI8ToF32(pSrc, pDst: PByte; aCount: NativeInt);

// F32 conversion
procedure cscvtF32ToUI8(pSrc, pDst: PByte; aCount: NativeInt);
//procedure cscvtF32ToRGB24(pSrc, pDst: PByte; aCount: NativeInt);


// creates a linear combination of 3 channels:
// pDst[I] := aC[0] * pSrc[3*I] + aC[1] * pSrc[3*I + 1] + aC[2] * pSrc[3*I + 2]
procedure _combineUI83CToF32(pSrc, pDst: PByte; aCount: NativeInt; aC: PSingle);
procedure _combineUI83CToUI8(pSrc, pDst: PByte; aCount: NativeInt; aC: PSingle);

{$endregion}

implementation

uses
    panda.cvCvt
  ;

{$EXCESSPRECISION OFF} // to prevent Single -> Double conversion by x64 compiler

{$region 'low-level functions'}

const
  cSepMask4x3UI8: array [0..15] of Byte = (0, 3, 6, 9, 1, 4, 7, 10, 2, 5, 8, 11, $80, $80, $80, $80);
  cBGRToGrayFactors: array [0..2] of Single = (0.114, 0.587, 0.299);

procedure _combineUI83CToF32(pSrc, pDst: PByte; aCount: NativeInt; aC: PSingle);
const
  s: Single = 1/255;
{$if defined(ASMx86)}
// EAX <- pSrc, EDX <- pDst, ECX <- aCount, [EBP + 8] <- @aC
asm
  push esi
  push ebx
  sub esp, 32
  movupd [esp], xmm6
  movupd [esp + 16], xmm7

  mov esi, eax
  mov eax, [ebp + 8] // eax <- @aC
  movss xmm3, [eax]
  movss xmm4, [eax + 4]
  movss xmm5, [eax + 8]
  movss xmm6, s
  movdqu xmm7, cSepMask4x3UI8
  mov ebx, ecx
  shr ecx, 2
  jz @rest

  shufps xmm3, xmm3, 0 // xmm3 <- 4 x aC[0]
  shufps xmm4, xmm4, 0
  shufps xmm5, xmm5, 0
@L:
  movq xmm0, [esi]
  movd xmm1, [esi + 8]
  movlhps xmm0, xmm1
  pshufb xmm0, xmm7
  movss xmm1, xmm0
  xorps xmm6, xmm6
  punpcklbw xmm1, xmm6
  punpcklbw xmm1, xmm6
  cvtdq2ps xmm1, xmm1
  mulps xmm1, xmm3       // xmm1 <- B * cB
  pshufd xmm2, xmm0, $55 // xmm2[0..3] <- xmm0[4..7]
  punpcklbw xmm2, xmm6
  punpcklbw xmm2, xmm6
  cvtdq2ps xmm2, xmm2
  mulps xmm2, xmm4
  addps xmm1, xmm2       // xmm1 <- B * cB + G * cG
  movhlps xmm2, xmm0
  punpcklbw xmm2, xmm6
  punpcklbw xmm2, xmm6
  cvtdq2ps xmm2, xmm2
  mulps xmm2, xmm5
  addps xmm1, xmm2      // xmm1 <- B * cB + G * cG + R * cR
  movss xmm6, s
  shufps xmm6, xmm6, 0
  mulps xmm1, xmm6
  movups [edx], xmm1
  add esi, 12
  add edx, 16
  dec ecx
  jnz @L
@rest:
  and ebx, 3
  jz @end
  xor eax, eax
@Lrest:
  mov al, [esi]
  cvtsi2ss xmm0, eax
  mulss xmm0, xmm3
  mov al, [esi + 1]
  cvtsi2ss xmm1, eax
  mulss xmm1, xmm4
  addss xmm0, xmm1
  mov al, [esi + 2]
  cvtsi2ss xmm1, eax
  mulss xmm1, xmm5
  addss xmm0, xmm1
  mulss xmm0, xmm6
  movss [edx], xmm0
  add esi, 3
  add edx, 4
  dec ebx
  jnz @Lrest
@end:
  movupd xmm7, [esp + 16]
  movupd xmm6, [esp]
  add esp, 32
  pop ebx
  pop esi
end;
{$elseif defined(ASMx64)}
// RCX <- pSrc, RDX <- pDst, R8 <- aCount, R9 <- @aC
asm
  sub rsp, 56 // 48 for xmm6..xmm8 backup + 8 stack alignment
  movupd [rsp], xmm6
  movupd [rsp + 16], xmm7
  movupd [rsp + 32], xmm8

  movss xmm3, [r9]
  movss xmm4, [r9 + 4]
  movss xmm5, [r9 + 8]
  movss xmm6, s
  movdqu xmm7, cSepMask4x3UI8
  xorps xmm8, xmm8
  mov r9, r8
  shr r8, 2
  jz @rest

  shufps xmm3, xmm3, 0 // xmm3 <- 4 x aC[0]
  shufps xmm4, xmm4, 0
  shufps xmm5, xmm5, 0
  shufps xmm6, xmm6, 0
@L:
  movq xmm0, [rcx]
  movd xmm1, [rcx + 8]
  movlhps xmm0, xmm1
  pshufb xmm0, xmm7
  movss xmm1, xmm0
  punpcklbw xmm1, xmm8
  punpcklbw xmm1, xmm8
  cvtdq2ps xmm1, xmm1
  mulps xmm1, xmm3       // xmm1 <- B * cB
  pshufd xmm2, xmm0, $55 // xmm2[0..3] <- xmm0[4..7]
  punpcklbw xmm2, xmm8
  punpcklbw xmm2, xmm8
  cvtdq2ps xmm2, xmm2
  mulps xmm2, xmm4
  addps xmm1, xmm2       // xmm1 <- B * cB + G * cG
  movhlps xmm2, xmm0
  punpcklbw xmm2, xmm8
  punpcklbw xmm2, xmm8
  cvtdq2ps xmm2, xmm2
  mulps xmm2, xmm5
  addps xmm1, xmm2      // xmm1 <- B * cB + G * cG + R * cR
  mulps xmm1, xmm6
  movups [rdx], xmm1
  add rcx, 12
  add rdx, 16
  dec r8
  jnz @L
@rest:
  and r9, 3
  jz @end
  xor eax, eax
@Lrest:
  mov al, [rcx]
  cvtsi2ss xmm0, eax
  mulss xmm0, xmm3
  mov al, [rcx + 1]
  cvtsi2ss xmm1, eax
  mulss xmm1, xmm4
  addss xmm0, xmm1
  mov al, [rcx + 2]
  cvtsi2ss xmm1, eax
  mulss xmm1, xmm5
  addss xmm0, xmm1
  mulss xmm0, xmm6
  movss [rdx], xmm0
  add rcx, 3
  add rdx, 4
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
    s1, s2, s3: Single;
begin
  s1 := aC^;
  Inc(aC);
  s2 := aC^;
  Inc(aC);
  s3 := aC^;
  pEnd := pDst + aCount * SizeOf(Single);
  while pDst < pEnd do begin
    PSingle(pDst)^ := (s1 * pSrc[0] + s2 * pSrc[1] + s3 * pSrc[2]) / 255;
    Inc(pDst, SizeOf(Single));
    Inc(pSrc, 3);
  end;
end;
{$endif}

procedure _combineUI83CToUI8(pSrc, pDst: PByte; aCount: NativeInt; aC: PSingle);
{$if defined(ASMx86)}
// EAX <- pSrc, EDX <- pDst, ECX <- aCount, [EBP + 8] <- @aC
asm
  push esi
  push ebx
  sub esp, 32
  movupd [esp], xmm6
  movupd [esp + 16], xmm7

  mov esi, eax
  mov eax, [ebp + 8]
  movss xmm3, [eax]
  movss xmm4, [eax + 4]
  movss xmm5, [eax + 8]
  movdqu xmm6, cSepMask4x3UI8
  xorps xmm7, xmm7
  mov ebx, ecx
  shr ecx, 2
  jz @rest

  shufps xmm3, xmm3, 0 // xmm3 <- 4 x aC[0]
  shufps xmm4, xmm4, 0
  shufps xmm5, xmm5, 0
@L:
  movq xmm0, [esi]
  movd xmm1, [esi + 8]
  movlhps xmm0, xmm1
  pshufb xmm0, xmm6
  movss xmm1, xmm0
  punpcklbw xmm1, xmm7
  punpcklbw xmm1, xmm7
  cvtdq2ps xmm1, xmm1
  mulps xmm1, xmm3       // xmm1 <- B * cB
  pshufd xmm2, xmm0, $55 // xmm2[0..3] <- xmm0[4..7]
  punpcklbw xmm2, xmm7
  punpcklbw xmm2, xmm7
  cvtdq2ps xmm2, xmm2
  mulps xmm2, xmm4
  addps xmm1, xmm2       // xmm1 <- B * cB + G * cG
  movhlps xmm2, xmm0
  punpcklbw xmm2, xmm7
  punpcklbw xmm2, xmm7
  cvtdq2ps xmm2, xmm2
  mulps xmm2, xmm5
  addps xmm1, xmm2      // xmm1 <- B * cB + G * cG + R * cR
  cvtps2dq xmm1, xmm1
  packssdw xmm1, xmm7 // dwords -> words
  packuswb xmm1, xmm7 // words -> bytes
  movd [edx], xmm1
  add esi, 12
  add edx, 4
  dec ecx
  jnz @L
@rest:
  and ebx, 3
  jz @end
  xor eax, eax
@Lrest:
  mov al, [esi]
  cvtsi2ss xmm0, eax
  mulss xmm0, xmm3
  mov al, [esi + 1]
  cvtsi2ss xmm1, eax
  mulss xmm1, xmm4
  addss xmm0, xmm1
  mov al, [esi + 2]
  cvtsi2ss xmm1, eax
  mulss xmm1, xmm5
  addss xmm0, xmm1
  cvtss2si eax, xmm0
  mov byte ptr [edx], al
  add esi, 3
  inc edx
  dec ebx
  jnz @Lrest
@end:
  movupd xmm7, [esp + 16]
  movupd xmm6, [esp]
  add esp, 32
  pop ebx
  pop esi
end;
{$elseif defined(ASMx64)}
// RCX <- pSrc, RDX <- pDst, R8 <- aCount, R9 <- @aC
asm
  sub rsp, 40 // 40 for xmm6..xmm7 backup + 8 stack alignment
  movupd [rsp], xmm6
  movupd [rsp + 16], xmm7

  movss xmm3, [r9]
  movss xmm4, [r9 + 4]
  movss xmm5, [r9 + 8]
  movdqu xmm6, cSepMask4x3UI8
  xorps xmm7, xmm7
  mov r9, r8
  shr r8, 2
  jz @rest

  shufps xmm3, xmm3, 0 // xmm3 <- 4 x aC[0]
  shufps xmm4, xmm4, 0
  shufps xmm5, xmm5, 0
@L:
  movq xmm0, [rcx]
  movd xmm1, [rcx + 8]
  movlhps xmm0, xmm1
  pshufb xmm0, xmm6
  movss xmm1, xmm0
  punpcklbw xmm1, xmm7
  punpcklbw xmm1, xmm7
  cvtdq2ps xmm1, xmm1
  mulps xmm1, xmm3       // xmm1 <- B * cB
  pshufd xmm2, xmm0, $55 // xmm2[0..3] <- xmm0[4..7]
  punpcklbw xmm2, xmm7
  punpcklbw xmm2, xmm7
  cvtdq2ps xmm2, xmm2
  mulps xmm2, xmm4
  addps xmm1, xmm2       // xmm1 <- B * cB + G * cG
  movhlps xmm2, xmm0
  punpcklbw xmm2, xmm7
  punpcklbw xmm2, xmm7
  cvtdq2ps xmm2, xmm2
  mulps xmm2, xmm5
  addps xmm1, xmm2      // xmm1 <- B * cB + G * cG + R * cR
  cvtps2dq xmm1, xmm1
  packssdw xmm1, xmm7 // dwords -> words
  packuswb xmm1, xmm7 // words -> bytes
  movd [rdx], xmm1
  add rcx, 12
  add rdx, 4
  dec r8
  jnz @L
@rest:
  and r9, 3
  jz @end
  xor eax, eax
@Lrest:
  mov al, [rcx]
  cvtsi2ss xmm0, eax
  mulss xmm0, xmm3
  mov al, [rcx + 1]
  cvtsi2ss xmm1, eax
  mulss xmm1, xmm4
  addss xmm0, xmm1
  mov al, [rcx + 2]
  cvtsi2ss xmm1, eax
  mulss xmm1, xmm5
  addss xmm0, xmm1
  cvtss2si eax, xmm0
  mov byte ptr [rdx], al
  add rcx, 3
  inc rdx
  dec r9
  jnz @Lrest
@end:
  movupd xmm7, [rsp + 16]
  movupd xmm6, [rsp]
  add rsp, 40
end;
{$else}
var pEnd: PByte;
    s1, s2, s3: Single;
begin
  s1 := aC^;
  Inc(aC);
  s2 := aC^;
  Inc(aC);
  s3 := aC^;
  pEnd := pDst + aCount;
  while pDst < pEnd do begin
    pDst^ := Byte(Round(s1 * pSrc[0] + s2 * pSrc[1] + s3 * pSrc[2]));
    Inc(pSrc, 3);
    Inc(pDst);
  end;
end;
{$endif}

procedure cscvtRGB24ToUI8(pSrc, pDst: PByte; aCount: NativeInt);
begin
  _combineUI83CToUI8(pSrc, pDst, aCount, @cBGRToGrayFactors);
end;

procedure cscvtRGB24ToF32(pSrc, pDst: PByte; aCount: NativeInt);
begin
  _combineUI83CToF32(pSrc, pDst, aCount, @cBGRToGrayFactors);
end;

procedure cscvtUI8ToRGB24(pSrc, pDst: PByte; aCount: NativeInt);
{$ifdef ASM}
const
  mask0: array [0..15] of Byte = (0, 0, 0, 1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5);
  mask1: array [0..15] of Byte = (5, 5, 6, 6, 6, 7, 7, 7, 8, 8, 8, 9, 9, 9, 10, 10);
  mask2: array [0..15] of Byte = (10, 11, 11, 11, 12, 12, 12, 13, 13, 13, 14, 14, 14, 15, 15, 15);
{$if defined(ASMx86)}
// EAX <- pSrc, EDX <- pDst, ECX <- aCount
asm
  push ebx
  mov ebx, ecx
  shr ecx, 4
  jz @rest
  movupd xmm0, mask0
  movupd xmm1, mask1
  movupd xmm2, mask2
@L:
  movdqu xmm3, [eax]
  movdqa xmm4, xmm3
  pshufb xmm3, xmm0
  movdqu [edx], xmm3
  add edx, 16
  movdqu xmm3, xmm4
  pshufb xmm3, xmm1
  movdqu [edx], xmm3
  add edx, 16
  pshufb xmm4, xmm2
  movdqu [edx], xmm4
  add edx, 16
  add eax, 16
  dec ecx
  jnz @L
@rest:
  mov ecx, ebx
  and ecx, 15
  jz @end
@Lrest:
  mov bl, [eax]
  mov [edx], bl
  mov [edx + 1], bl
  mov [edx + 2], bl
  add edx, 3
  inc eax
  dec ecx
  jnz @Lrest
@end:
  pop ebx
end;
{$elseif defined(ASMx64)}
// RCX <- pSrc, RDX <- pDst, R8 <- aCount
asm
  mov r9, r8
  shr r8, 4
  jz @rest
  movupd xmm0, mask0
  movupd xmm1, mask1
  movupd xmm2, mask2
@L:
  movdqu xmm3, [rcx]
  movdqa xmm4, xmm3
  pshufb xmm3, xmm0
  movdqu [rdx], xmm3
  add rdx, 16
  movdqu xmm3, xmm4
  pshufb xmm3, xmm1
  movdqu [rdx], xmm3
  add rdx, 16
  pshufb xmm4, xmm2
  movdqu [rdx], xmm4
  add rdx, 16
  add rcx, 16
  dec r8
  jnz @L
@rest:
  and r9, 15
  jz @end
@Lrest:
  mov al, [rcx]
  mov [rdx], al
  mov [rdx + 1], al
  mov [rdx + 2], al
  add rdx, 3
  inc rcx
  dec r9
  jnz @Lrest
@end:
end;
{$endif}
{$else}
var pEnd: PByte;
    v: Byte;
begin
  pEnd := pSrc + aCount;
  while pSrc < pEnd do begin
    v := pSrc^;
    pDst[0] := v;
    pDst[1] := v;
    pDst[2] := v;
    Inc(pDst, 3);
    Inc(pSrc);
  end;
end;
{$endif}

procedure cscvtUI8ToF32(pSrc, pDst: PByte; aCount: NativeInt);
{$if defined(ASMx86)}
// EAX <- pSrc, EDX <- pDst, ECX <- aCount
const s: Single = 1/255;
asm
  push ebx
  movd xmm2, s
  mov ebx, ecx
  shr ecx, 2
  jz @rest
  xorps xmm0, xmm0
  shufps xmm2, xmm2, 0 // xmm2 <- 4 x scale
@L:
  movd xmm1, [eax]
  punpcklbw xmm1, xmm0 // byte -> word
  punpcklbw xmm1, xmm0 // word -> integer
  cvtdq2ps xmm1, xmm1  // integer -> single
  mulps xmm1, xmm2
  movups [edx], xmm1
  add eax, 4
  add edx, 16
  dec ecx
  jnz @L
@rest:
  mov ecx, ebx
  and ecx, 3
  jz @end
@Lrest:
  movzx ebx, byte ptr [eax]
  cvtsi2ss xmm1, ebx
  mulss xmm1, xmm2
  movss [edx], xmm1
  inc eax
  add edx, 4
  dec ecx
  jnz @Lrest
@end:
  pop ebx
end;
{$elseif defined(ASMx64)}
// RCX <- pSrc, RDX <- pDst, R8 <- aCount
const s: Single = 1/255;
asm
  movd xmm2, s
  mov r9, r8
  shr r8, 2
  jz @rest
  xorps xmm0, xmm0
  shufps xmm2, xmm2, 0 // xmm2 <- 4 x scale
@L:
  movd xmm1, [rcx]
  punpcklbw xmm1, xmm0 // byte -> word
  punpcklbw xmm1, xmm0 // word -> integer
  cvtdq2ps xmm1, xmm1  // integer -> single
  mulps xmm1, xmm2
  movups [rdx], xmm1
  add rcx, 4
  add rdx, 16
  dec r8
  jnz @L
@rest:
  and r9, 3
  jz @end
@Lrest:
  movzx eax, byte ptr [rcx]
  cvtsi2ss xmm1, eax
  mulss xmm1, xmm2
  movss [rdx], xmm1
  inc rcx
  add rdx, 4
  dec r9
  jnz @Lrest
@end:
end;
{$else}
var p: PSingle;
    pEnd: PByte;
begin
  p := PSingle(pDst);
  pEnd := pSrc + aCount;
  while pSrc < pEnd do begin
    p^ := pSrc^ / 255;
    Inc(pSrc);
    Inc(p);
  end;
end;
{$endif}

procedure cscvtF32ToUI8(pSrc, pDst: PByte; aCount: NativeInt);
const s: Single = 255;
{$if defined(ASMx86)}
// EAX <- pSrc, EDX <- pDst, ECX <- aCount
asm
  push ebx
  movss xmm1, s
  mov ebx, ecx
  shr ecx, 2
  jz @rest
  shufps xmm1, xmm1, 0 // xmm2 <- 4 x s
@L:
  movups xmm0, [eax]
  mulps xmm0, xmm1
  cvtps2dq xmm0, xmm0
  packssdw xmm0, xmm0
  packuswb xmm0, xmm0
  movd [edx], xmm0
  add eax, 16
  add edx, 4
  dec ecx
  jnz @L
@rest:
  mov ecx, ebx
  and ecx, 3
  jz @end
  push esi
  mov esi, eax
@Lrest:
  movss xmm0, [esi]
  mulss xmm0, xmm1
  cvtss2si eax, xmm0
  xor ebx, ebx
  cmp eax, ebx
  cmovl eax, ebx
  mov ebx, 255
  cmp eax, ebx
  cmovg eax, ebx
  mov [edx], al
  add esi, 4
  inc edx
  dec ecx
  jnz @Lrest
  pop esi
@end:
  pop ebx
end;
{$elseif defined(ASMx64)}
// RCX <- pSrc, RDX <- pDst, R8 <- aCount
asm
  movss xmm1, s
  mov r9, r8
  shr r8, 2
  jz @rest
  shufps xmm1, xmm1, 0 // xmm2 <- 4 x s
@L:
  movups xmm0, [rcx]
  mulps xmm0, xmm1
  cvtps2dq xmm0, xmm0
  packssdw xmm0, xmm0
  packuswb xmm0, xmm0
  movd [rdx], xmm0
  add rcx, 16
  add rdx, 4
  dec r8
  jnz @L
@rest:
  and r9, 3
  jz @end
@Lrest:
  movss xmm0, [rcx]
  mulss xmm0, xmm1
  cvtss2si eax, xmm0
  xor r8d, r8d
  cmp eax, r8d
  cmovl eax, r8d
  mov r8d, 255
  cmp eax, r8d
  cmovg eax, r8d
  mov [rdx], eax
  add rcx, 4
  inc rdx
  dec r9
  jnz @Lrest
@end:
end;
{$else}
var p: PSingle;
    pEnd: PByte;
    v: Single;
begin
  p := PSingle(pSrc);
  pEnd := pDst + aCount;
  while pDst < pEnd do begin
    v := p^;
    if v > 1 then
      pDst^ := 255
    else
    if v < 0 then
      pDSt^ := 0
    else
      pDst^ := Round(255 * v);
    Inc(p);
    Inc(pDst);
  end;
end;
{$endif}

{$endregion}

{$region 'image conversion functions'}

procedure _CSCvt(aCvtProc: TCSCvtProc; const aSrc, aDst: IImage);
var pSrc, pDst: PByte;
    I, w, h, srcWs, dstWs: NativeInt;
begin
  w := aSrc.Width;
  h := aSrc.Height;
  Assert((w = aDst.Width) and (h = aDst.Height));
  pSrc := aSrc.Data;
  pDst := aDst.Data;
  srcWs := aSrc.WidthStep;
  dstWs := aDst.WidthStep;

  for I := 0 to h - 1 do begin
    aCvtProc(pSrc, pDst, w);
    Inc(pSrc, srcWs);
    Inc(pDst, dstWs);
  end;
end;

procedure ColorConvert(const aSrc: IImage<TRGB24>; var aDst: IImage<Byte>);
begin
  if not Assigned(aDst) then
    aDst := TNDAImg<Byte>.Create(aSrc.Width, aSrc.Height);
  _CSCvt(cscvtRGB24ToUI8, aSrc, aDst);
end;

procedure ColorConvert(const aSrc: IImage<TRGB24>; var aDst: IImage<Single>);
begin
  if not Assigned(aDst) then
    aDst := TNDAImg<Single>.Create(aSrc.Width, aSrc.Height);
  _CSCvt(cscvtRGB24ToF32, aSrc, aDst);
end;

procedure ColorConvert(const aSrc: IImage<Byte>; var aDst: IImage<TRGB24>);
begin
  if not Assigned(aDst) then
    aDst := TNDAImg<TRGB24>.Create(aSrc.Width, aSrc.Height);
  _CSCvt(cscvtUI8ToRGB24, aSrc, aDst);
end;

procedure ColorConvert(const aSrc: IImage<Byte>; var aDst: IImage<Single>);
begin
  if not Assigned(aDst) then
    aDst := TNDAImg<Single>.Create(aSrc.Width, aSrc.Height);
  _CSCvt(cscvtUI8ToF32, aSrc, aDst);
end;

procedure ColorConvert(const aSrc: IImage<Single>; var aDst: IImage<Byte>);
begin
  if not Assigned(aDst) then
    aDst := TNDAImg<Byte>.Create(aSrc.Width, aSrc.Height);
  _CSCvt(cscvtF32ToUI8, aSrc, aDst);
end;

{$endregion}

{$region 'TCSUt'}

class function TCSUt.MatchQ<T>(const aImg: IImage): Boolean;
begin
  Result := (TypeInfo(T) = aImg.GetItemType);
end;

{$endregion}

end.
