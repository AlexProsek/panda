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

  procedure HueSeparate(const aSrc: IImage<TRGB24>; var aDst: IImage<Single>); overload;
  procedure HSVSeparate(const aSrc: IImage<TRGB24>; var H, S, V: IImage<Single>); overload;

type
  TCSUt = class
  public
    class function MatchQ<T>(const aImg: IImage): Boolean; inline;
  end;

{$region 'low-level functions'}

type
  TVec16UI8 = array [0..15] of UInt8;
  PVec16UI8 = ^TVec16UI8;

  TVec16F32 = array [0..15] of Single;
  PVec16F32 = ^TVec16F32;

  TCSCvtProc = procedure (pSrc, pDst: PByte; aCount: NativeInt);
  // procedure for separating interleaved 3 color channels
  TIC3SepProc = procedure (pSrc, pCh0, pCh1, pCh2: PByte; aCount: NativeInt);

// RGB conversion
procedure cscvtRGB24ToUI8(pSrc, pDst: PByte; aCount: NativeInt);
procedure cscvtRGB24ToF32(pSrc, pDst: PByte; aCount: NativeInt);
procedure cscvtRGB24ToHue(pSrc, pDst: PByte; aCount: NativeInt);

// UI8 conversion
procedure cscvtUI8ToRGB24(pSrc, pDst: PByte; aCount: NativeInt);
procedure cscvtUI8ToF32(pSrc, pDst: PByte; aCount: NativeInt);

// F32 conversion
procedure cscvtF32ToUI8(pSrc, pDst: PByte; aCount: NativeInt);
//procedure cscvtF32ToRGB24(pSrc, pDst: PByte; aCount: NativeInt);

// RGB separation
procedure cssepRGB24ToHSV(pSrc, pH, pS, pV: PByte; aCount: NativeInt);


// creates a linear combination of 3 channels:
// pDst[I] := aC[0] * pSrc[3*I] + aC[1] * pSrc[3*I + 1] + aC[2] * pSrc[3*I + 2]
procedure _combineUI8C3ToF32(pSrc, pDst: PByte; aCount: NativeInt; aC: PSingle);
procedure _combineUI8C3ToUI8(pSrc, pDst: PByte; aCount: NativeInt; aC: PSingle);


{$if defined(ASMx64)}

// This functions separates interleaved 16 RGB colors into three contiguos arrays
// pSrc - points to an array [0..15] of TRGB24
// pChannes - separated B, G, R channels in an array [0..2] of TVec16UI8
procedure _RGB24Sep16(pSrc: PByte; pChannels: PVec16UI8);
// This function precomputes values needed for HSV calculation.
// pSrc - points to three separated B, G, R channels stored in the array [0..2] of TVec16UI8
// pMaRng - points to the result array [0..1] of TVec16UI8 = [Max(R,G,B), Max(R,G,B) - Min(R,G,B)]
procedure _GetMaxAndRngFromRGBCh(pSrc, pMaRng: PVec16UI8);
// pSrc - points to an array [0..4] of TVec16UI8 which contains these vectors:
//    [B, G, R, Max(R,G,B), Max(R,G,B) - Min(R,G,B)]
// pDst - points to a single array [0..15] of Single
procedure _Hue16(pSrc, pDst: PByte);
// This function evaluates saturation and value
// pSrc - points to an array [0..1] of TVec16UI8 = [Max(R,G,B), Max(R,G,B) - Min(R,G,B)]
// pS, pV - points to sigle arrays [0..15] of Single
procedure _SV16(pSrc, pS, pV: PByte);

{$endif}

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

procedure _combineUI8C3ToF32(pSrc, pDst: PByte; aCount: NativeInt; aC: PSingle);
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

procedure _combineUI8C3ToUI8(pSrc, pDst: PByte; aCount: NativeInt; aC: PSingle);
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
  _combineUI8C3ToUI8(pSrc, pDst, aCount, @cBGRToGrayFactors);
end;

procedure cscvtRGB24ToF32(pSrc, pDst: PByte; aCount: NativeInt);
begin
  _combineUI8C3ToF32(pSrc, pDst, aCount, @cBGRToGrayFactors);
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

procedure _cscvtRGB24ToHue(pSrc, pDst: PByte; aCount: NativeInt);
var h: Single;
    mi, ma, d: Integer;
    pEnd: PByte;
const
  c120: Single = 120.0;
  c240: Single = 240.0;
  c360: Single = 360.0;
begin
  pEnd := pSrc + aCount * SizeOf(TRGB24);

  while pSrc < pEnd do begin

    with PRGB24(pSrc)^ do begin
      if R > G then begin
        if R > B then begin
          ma := R;
          if B < G then
            mi := B
          else
            mi := G;
        end else begin
          ma := B;
          mi := G;
        end;
      end else begin
        if G > B then begin
          ma := G;
          if R > B then
            mi := B
          else
            mi := R;
        end else begin
          ma := B;
          mi := R;
        end;
      end;

      if mi = ma then
        h := 0
      else begin
        if ma = R then begin
          d := G - B;
          h := (60 * d) / (ma - mi);
          if d < 0 then h := h + c360;
        end else
        if ma = G then
          h := c120 + (60 * (B - R)) / (ma - mi)
        else
          h := c240 + (60 * (R - G)) / (ma - mi);
      end;
      PSingle(pDst)^ := h;
    end;

    Inc(pSrc, SizeOf(TRGB24));
    Inc(pDst, SizeOf(Single));
  end;
end;

procedure _cssepRGB24ToHSV(pSrc, pH, pS, pV: PByte; aCount: NativeInt);
var h: Single;
    mi, ma, d: Integer;
    pEnd: PByte;
const
  c120: Single = 120.0;
  c240: Single = 240.0;
  c360: Single = 360.0;
  c255: Single = 255.0;
begin
  pEnd := pSrc + aCount * SizeOf(TRGB24);

  while pSrc < pEnd do begin

    with PRGB24(pSrc)^ do begin
      if R > G then begin
        if R > B then begin
          ma := R;
          if B < G then
            mi := B
          else
            mi := G;
        end else begin
          ma := B;
          mi := G;
        end;
      end else begin
        if G > B then begin
          ma := G;
          if R > B then
            mi := B
          else
            mi := R;
        end else begin
          ma := B;
          mi := R;
        end;
      end;

      PSingle(pV)^ := ma/c255;

      if ma <> 0 then
        PSingle(pS)^ := (ma - mi) / ma
      else
        PSingle(pS)^ := 0;

      if mi = ma then
        h := 0
      else begin
        if ma = R then begin
          d := G - B;
          h := (60 * d) / (ma - mi);
          if d < 0 then h := h + c360;
        end else
        if ma = G then
          h := c120 + (60 * (B - R)) / (ma - mi)
        else
          h := c240 + (60 * (R - G)) / (ma - mi);
      end;
      PSingle(pH)^ := h;
    end;

    Inc(pSrc, SizeOf(TRGB24));
    Inc(pH, SizeOf(Single));
    Inc(pS, SizeOf(Single));
    Inc(pV, SizeOf(Single));
  end;
end;

{$if defined(ASMx64)}

procedure _RGB24Sep16(pSrc: PByte; pChannels: PVec16UI8);
// RCX <- pSrc, RDX <- pChannels
asm
  movdqu xmm5, cSepMask4x3UI8
  movdqu xmm0, [rcx]
  pshufb xmm0, xmm5
  movdqu xmm1, [rcx + 12]
  pshufb xmm1, xmm5
  movdqu xmm2, [rcx + 24]
  pshufb xmm2, xmm5
  movq xmm3, [rcx + 36]
  movd xmm4, [rcx + 44]
  movlhps xmm3, xmm4
  pshufb xmm3, xmm5

  movdqa xmm4, xmm0     // xmm4 <- (a00, a01, a02, _)
  punpckldq xmm4, xmm1  // xmm4 <- (a00, a10, a01, a11)
  movdqa xmm5, xmm2     // xmm5 <- (a20, a21, a22, _)
  punpckldq xmm5, xmm3  // xmm6 <- (a20, a30, a21, a31)

  psrldq xmm0, 8
  psrldq xmm1, 8
  psrldq xmm2, 8
  psrldq xmm3, 8
  punpckldq xmm0, xmm1
  punpckldq xmm2, xmm3
  movlhps xmm0, xmm2
  movdqu [rdx + 32], xmm0 // Channels[2] <- (a02, a12, a22, a32)

  movdqa xmm0, xmm4
  punpcklqdq xmm0, xmm5   // xmm0 <- (a00, a10, a20, a30)
  punpckhqdq xmm4, xmm5   // xmm4 <- (a01, a11, a21, a31)

  movdqu [rdx], xmm0
  movdqu [rdx + 16], xmm4
end;

procedure _GetMaxAndRngFromRGBCh(pSrc, pMaRng: PVec16UI8);
// RCX <- pSrc, RDX <- pDst
asm
  movdqu xmm0, [rcx]
  movdqu xmm1, [rcx + 16]
  movdqu xmm2, [rcx + 32]

  movdqa xmm3, xmm0
  pminub xmm0, xmm1
  pmaxub xmm1, xmm3

  movdqa xmm3, xmm1
  pminub xmm1, xmm2
  pmaxub xmm2, xmm3

  movdqa xmm3, xmm0
  pminub xmm0, xmm1
  pmaxub xmm1, xmm3

  movdqu [rdx], xmm2
  psubb xmm2, xmm0
  movdqu [rdx + 16], xmm2
end;

procedure _getMaRng16UI8(pSrc: PByte; var aMa, aRng: TVec16UI8);
// RCX <- pSrc, RDX <- @aMa, R8 <- @aRng
asm
  movdqu xmm5, cSepMask4x3UI8
  movdqu xmm0, [rcx]
  pshufb xmm0, xmm5
  movdqu xmm1, [rcx + 12]
  pshufb xmm1, xmm5
  movdqu xmm2, [rcx + 24]
  pshufb xmm2, xmm5
  movdqu xmm3, [rcx + 36]
  pshufb xmm3, xmm5

  movdqa xmm4, xmm0     // xmm4 <- (a00, a01, a02, _)
  punpckldq xmm4, xmm1  // xmm4 <- (a00, a10, a01, a11)
  movdqa xmm5, xmm2     // xmm5 <- (a20, a21, a22, _)
  punpckldq xmm5, xmm3  // xmm6 <- (a20, a30, a21, a31)

  psrldq xmm0, 8
  psrldq xmm1, 8
  psrldq xmm2, 8
  psrldq xmm3, 8
  punpckldq xmm0, xmm1
  punpckldq xmm2, xmm3
  movlhps xmm0, xmm2
  movdqa xmm2, xmm0     // xmm0 <- (a02, a12, a22, a32)

  movdqa xmm0, xmm4
  punpcklqdq xmm0, xmm5 // xmm0 <- (a00, a10, a20, a30)
  movdqa xmm1, xmm4
  punpckhqdq xmm1, xmm5 // xmm1 <- (a01, a11, a21, a31)

  movdqa xmm3, xmm0
  pminub xmm0, xmm1
  pmaxub xmm1, xmm3

  movdqa xmm3, xmm1
  pminub xmm1, xmm2
  pmaxub xmm2, xmm3

  movdqa xmm3, xmm0
  pminub xmm0, xmm1
  pmaxub xmm1, xmm3

  movdqu [rdx], xmm2
  psubb  xmm2, xmm0
  movdqu [r8], xmm2
end;

type
  THue16Consts = record
    c120, c240, c360: array [0..3] of Single;
    c60: array [0..3] of Word;
  end align 16;

{$define _hsv_fast_div}

procedure _Hue16(pSrc, pDst: PByte);
// RCX <- pSrc, RDX <- pDst
const
  consts: THue16Consts = (
    c120: (120, 120, 120, 120);
    c240: (240, 240, 240, 240);
    c360: (360, 360, 360, 360);
    c60: (60, 60, 60, 60)
  );
asm
  sub rsp, 168 // 64 for xmm6..xmm9 backup + 96 for local buffers + 8 stack alignment
  movdqa [rsp], xmm6
  movdqa [rsp + 16], xmm7
  movdqa [rsp + 32], xmm8
  movdqa [rsp + 48], xmm9

  movdqu xmm4, [rcx + 48] // xmm4 <- ma := Max(R,G,B)
  movdqu xmm3, [rcx + 32] // xmm3 <- R
  movdqu xmm2, [rcx + 16] // xmm2 <- G
  movdqu xmm1, [rcx]      // xmm1 <- B

  pxor xmm0, xmm0         // xmm0 <- 0, ..
  pcmpeqd xmm6, xmm6      // xmm6 < $FF, ..

  // masks for branching
  movdqa xmm5, xmm4       // xmm5 <- ma
  pcmpeqb xmm5, xmm3
  movdqa [rsp + 64], xmm5 // [rsp + 64] <- mask(ma = R)
  movdqa xmm7, xmm5
  pxor xmm7, xmm6         // xmm7 <- mask(ma <> R)

  movdqa xmm5, xmm4
  pcmpeqb xmm5, xmm2      // xmm5 <- mask(ma = G)
  movdqa xmm8, xmm5
  pxor xmm8, xmm6         // xmm8 <- mask(ma <> G)
  pand xmm5, xmm7         // xmm5 <- mask((ma = G) and (ma <> R))
  movdqa [rsp + 80], xmm5 // [rsp + 80] <- mask((ma = G) and (ma <> R))
  pand xmm7, xmm8         // xmm7 <- mask((ma <> R) and (ma <> G))
  movdqa [rsp + 96], xmm7 // [rsp + 96] <- mask((ma <> R) and (ma <> G))

  movdqu xmm5, [rcx + 64] // xmm5 <- rng := Max(R,G,B) - Min(R,G,B)
  pcmpeqb xmm5, xmm0
  pcmpeqd xmm6, xmm6
  pxor xmm5, xmm6
  movdqa [rsp + 144], xmm5// [rsp + 144] <- mask(rng <> 0)

  mov rax, 2
  mov r8, rsp
@L:
  pxor xmm0, xmm0
  movq xmm8, [rcx + 64]
  punpcklbw xmm8, xmm0    // xmm8 <- word(rng[0..7])
  movhlps xmm9, xmm8
  punpcklwd xmm8, xmm0    // xmm8 <- int32(rng[0..3])
  punpcklwd xmm9, xmm0    // xmm9 <- int32(rng[4..7])
  cvtdq2ps xmm8, xmm8     // xmm8 <- single(rng[0..3])
  cvtdq2ps xmm9, xmm9     // xmm9 <- single(rng[4..7])
{$ifdef hsv_fast_div}
  rcpps xmm8, xmm8        // xmm8 <- 1/xmm8
  rcpps xmm9, xmm9        // xmm9 <- 1/xmm9
{$endif}
  punpcklbw xmm1, xmm0    // xmm1 <- word(B[0..7])
  punpcklbw xmm2, xmm0    // xmm2 <- word(G[0..7])
  punpcklbw xmm3, xmm0    // xmm3 <- word(R[0..7])
  movdqa xmm4, xmm2
  movdqa xmm5, xmm1
  psubw xmm4, xmm1        // xmm4 <- G - B
  psubw xmm5, xmm3        // xmm5 <- B - R
  psubw xmm3, xmm2        // xmm3 <- R - G

  // (ma == R) branch

  movq xmm1, [r8 + 64]
  punpcklbw xmm1, xmm1    // xmm1 <- Word(mask(ma = R))
  pand xmm4, xmm1         // xmm4 <- ((G - B) & mask(ma = R))[0..7]

  movq xmm0, consts.c60
  movq xmm1, xmm4
  pmullw xmm1, xmm0       // xmm1.Lo <- 60*(G - B)[0..3]
  movhlps xmm2, xmm4
  pmullw xmm2, xmm0       // xmm2.Lo <- 60*(G - B)[4..7]
  punpcklqdq xmm1, xmm2   // xmm1 <- 60*(G - B)

  pxor xmm4, xmm4
  pcmpgtw xmm4, xmm1      // xmm4 <- mask(0 > (G - B))

  movq xmm2, xmm4
  punpcklwd xmm2, xmm4    // xmm2 <- int32(mask[0..3])
  pand xmm2, consts.c360

  movq xmm6, xmm1         // xmm6 <- 60(G - B)[0..3]
  punpcklwd xmm6, xmm4    // int32(60(G - B)[0..3])
  cvtdq2ps xmm6, xmm6     // single(60(G - B)[0..3])
{$ifdef hsv_fast_div}
  mulps xmm6, xmm8
{$else}
  divps xmm6, xmm8
{$endif}
  addps xmm6, xmm2
  movaps [rsp + 112], xmm6

  psrldq xmm4, 8
  movq xmm2, xmm4
  punpcklwd xmm2, xmm4    // xmm2 <- int32(mask[4..7])
  pand xmm2, consts.c360

  movhlps xmm6, xmm1         // xmm6 <- 60(G - B)[4..7]
  punpcklwd xmm6, xmm4
  cvtdq2ps xmm6, xmm6
{$ifdef hsv_fast_div}
  mulps xmm6, xmm9
{$else}
  divps xmm6, xmm9
{$endif}
  addps xmm6, xmm2
  movups [rsp + 128], xmm6

  // (ma == G) branch

  movq xmm1, xmm5
  pmullw xmm1, xmm0       // xmm1.Lo <- 60(B - R)[0..3]
  movhlps xmm2, xmm5
  pmullw xmm2, xmm0       // xmm2.Lo <- 60(B - R)[4..7]
  punpcklqdq xmm1, xmm2   // xmm1 <- 60(B - R)

  movaps xmm7, consts.c120
  movdqa xmm2, xmm1
  psraw xmm2, 15          // xmm2 <- sign_mask(60(B - R)) (0 or -1 per word)
  movdqa xmm6, xmm1
  punpcklwd xmm6, xmm2    // xmm6 <- int32(60(B - R)[0..3])
  cvtdq2ps xmm6, xmm6     // xmm6 <- single(60(B - R)[0..3])
{$ifdef hsv_fast_div}
  mulps xmm6, xmm8
{$else}
  divps xmm6, xmm8
{$endif}
  addps xmm6, xmm7        // xmm6 += 120.0
  movd xmm4, [r8 + 80]   // xmm4 <- mask(ma = G)[0..3]
  punpcklbw xmm4, xmm4
  punpcklwd xmm4, xmm4
  pand xmm6, xmm4         // xmm6 <- (120 + 60(B - R) & mask(ma = G))[0..3]
  addps xmm6, [rsp + 112]
  movaps [rsp + 112], xmm6

  movhlps xmm2, xmm2
  movhlps xmm6, xmm1      // xmm6 <- 60(B - R)[4..7]
  punpcklwd xmm6, xmm2    // xmm6 <- int32(60(B - R)[4..7])
  cvtdq2ps xmm6, xmm6     // xmm6 <- single(60(B - R)[4..7])
{$ifdef hsv_fast_div}
  mulps xmm6, xmm9
{$else}
  divps xmm6, xmm9
{$endif}
  addps xmm6, xmm7
  movd xmm4, [r8 + 84]
  punpcklbw xmm4, xmm4
  punpcklwd xmm4, xmm4
  pand xmm6, xmm4         // xmm6 <- (120 + 60(B - R) & mask(ma = G))[0..3]
  addps xmm6, [rsp + 128]
  movaps [rsp + 128], xmm6

  // (ma == B) branch

  movq xmm1, xmm3
  pmullw xmm1, xmm0       // xmm1.Lo <- 60(R - G)[0..3]
  movhlps xmm2, xmm3
  pmullw xmm2, xmm0       // xmm2.Lo <- 40(R - G)[4..7]
  punpcklqdq xmm1, xmm2   // xmm1 <- 60(R - G)

  movaps xmm7, consts.c240
  movdqa xmm2, xmm3
  psraw xmm2, 15          // xmm2 <- sign_mask(60(R - G))
  movdqa xmm6, xmm1
  punpcklwd xmm6, xmm2    // xmm4 <- int32(60(R - G)[0..3])
  cvtdq2ps xmm6, xmm6     // xmm6 <- single(60(R - G)[0..3])
{$ifdef hsv_fast_div}
  mulps xmm6, xmm8
{$else}
  divps xmm6, xmm8
{$endif}
  addps xmm6, xmm7        // xmm6 += 240.0
  movd xmm4, [r8 + 96]   // xmm4 <- mask(ma = B)[0..3]
  punpcklbw xmm4, xmm4
  punpcklwd xmm4, xmm4
  pand xmm6, xmm4
  addps xmm6, [rsp + 112]

  movhlps xmm2, xmm2
  movhlps xmm5, xmm1      // xmm5 <- 60(R - G)[4..7]
  punpcklwd xmm5, xmm2    // xmm5 <- int32(60(R - G)[4..7])
  cvtdq2ps xmm5, xmm5     // xmm5 <- single(60(R - G)[4..7])
{$ifdef hsv_fast_div}
  mulps xmm5, xmm9
{$else}
  divps xmm5, xmm9
{$endif}
  addps xmm5, xmm7
  movd xmm4, [r8 + 100]
  punpcklbw xmm4, xmm4
  punpcklwd xmm4, xmm4
  pand xmm5, xmm4
  addps xmm5, [rsp + 128]

  // (rng <> 0) masking
  movd xmm1, [r8 + 144]
  punpcklbw xmm1, xmm1
  punpcklwd xmm1, xmm1    // xmm1 <- int32(mask(rng <> 0))
  pand xmm6, xmm1
  movd xmm1, [r8 + 148]
  punpcklbw xmm1, xmm1
  punpcklwd xmm1, xmm1
  pand xmm5, xmm1

  // store result
  movups [rdx], xmm6
  movups [rdx + 16], xmm5

  dec rax
  jz @end

  add rcx, 8
  movq xmm3, [rcx + 32]   // xmm3 <- R
  movq xmm2, [rcx + 16]   // xmm2 <- G
  movq xmm1, [rcx]        // xmm1 <- B

  add rdx, 32
  add r8, 8
  jmp @L

@end:
  // cleanup
  movdqa xmm9, [rsp + 48]
  movdqa xmm8, [rsp + 32]
  movdqa xmm7, [rsp + 16]
  movdqa xmm6, [rsp]
  add rsp, 168
end;

type
  TSV16Consts = record
    c255: array [0..3] of Single;
  end align 16;

procedure _SV16(pSrc, pS, pV: PByte);
// RCX <- pSrc, RDX <- pS, R8 <- pV
const consts: TSV16Consts = (c255: (1/255, 1/255, 1/255, 1/255));
asm
  sub rsp, 88 // 16 for xmm6 backup + 64 for local buffers + 8 stack alignment
  movdqa [rsp + 64], xmm6

  pxor xmm0, xmm0
  movdqu xmm1, [rcx]        // xmm1 <- ma

  // Value

  movaps xmm2, consts.c255  // xmm2 <- 4x single(1/255)
  movdqa xmm3, xmm1
  punpcklbw xmm3, xmm0      // xmm3 <- int16(ma[0..7])
  movdqa xmm4, xmm3
  punpcklwd xmm4, xmm0      // xmm4 <- int32(ma[0..3])
  cvtdq2ps xmm4, xmm4       // xmm4 <- single(ma[0..3])
  movaps [rsp], xmm4
  mulps xmm4, xmm2
  movups [r8], xmm4   // [rdx + 64] <- V[0..3] := ma/255

  psrldq xmm3, 8
  punpcklwd xmm3, xmm0      // xmm3 <- int32(ma[4..7])
  cvtdq2ps xmm3, xmm3
  movaps [rsp + 16], xmm3
  mulps xmm3, xmm2
  movups [r8 + 16], xmm3   // [rdx + 80] <- V[4..7]

  movhlps xmm3, xmm1
  punpcklbw xmm3, xmm0      // xmm3 <- int16(ma[8..15])
  movdqa xmm4, xmm3
  punpcklwd xmm4, xmm0      // xmm4 <- int32(ma[8..11])
  cvtdq2ps xmm4, xmm4       // xmm4 <- single(ma[8..11])
  movaps [rsp + 32], xmm4
  mulps xmm4, xmm2
  movups [r8 + 32], xmm4   // [rdx + 96] <- V[8..11]

  psrldq xmm3, 8
  punpcklwd xmm3, xmm0      // xmm3 <- int32(ma[12..15])
  cvtdq2ps xmm3, xmm3
  movaps [rsp + 48], xmm3
  mulps xmm3, xmm2
  movups [r8 + 48], xmm3  // [rdx + 112] <- V[12..15])


  // Saturation

  movdqa xmm2, xmm1
  pcmpeqb xmm2, xmm0        // xmm2 <- mask(ma = 0)
  pcmpeqb xmm3, xmm3
  pxor xmm2, xmm3           // xmm2 <- mask(ma <> 0)

  movdqu xmm1, [rcx + 16]   // xmm1 <- rng

  movdqa xmm3, xmm1
  punpcklbw xmm3, xmm0      // xmm3 <- int16(rng[0..7])
  movdqa xmm4, xmm3
  punpcklwd xmm4, xmm0      // xmm4 <- int32(rng[0..3])
  cvtdq2ps xmm4, xmm4
  divps xmm4, [rsp]         // xmm4 <- (rng/(255V))[0..3]
  movdqa xmm5, xmm2
  punpcklbw xmm5, xmm5
  movdqa xmm6, xmm5
  punpcklwd xmm5, xmm5      // xmm5 <- mask(ma <> 0)[0..3]
  pand xmm4, xmm5
  movups [rdx], xmm4        // [rdx] <- (rng/V & mask(m <> 0))[0..3]

  movhlps xmm3, xmm3
  punpcklwd xmm3, xmm0      // xmm3 <- int32(rng[4..7])
  cvtdq2ps xmm3, xmm3
  divps xmm3, [rsp + 16]    // xmm3 <- (rng/(255V))[4..7]
  movhlps xmm5, xmm6
  punpcklwd xmm5, xmm5      // xmm5 <- mask(ma <> 0)[4..7]
  pand xmm3, xmm5
  movups [rdx + 16], xmm3

  movhlps xmm3, xmm1        // xmm3.Lo <- rng[8..15]
  punpcklbw xmm3, xmm0      // xmm3 <- int16(rng[8..15])
  movdqa xmm4, xmm3
  punpcklwd xmm4, xmm0      // xmm4 <- int32(rng[8..11])
  cvtdq2ps xmm4, xmm4
  divps xmm4, [rsp + 32]    // xmm4 <- (rng/(255V))[8..11]
  movdqa xmm5, xmm2
  punpcklbw xmm5, xmm5
  movdqa xmm6, xmm5
  punpcklwd xmm5, xmm5      // xmm5 <- mask(ma <> 0)[8..11]
  pand xmm4, xmm5
  movups [rdx + 32], xmm4   // [rdx] <- (rng/V & mask(m <> 0))[8..11]

  movhlps xmm3, xmm3
  punpcklwd xmm3, xmm0      // xmm3 <- int32(rng[12..15])
  cvtdq2ps xmm3, xmm3
  divps xmm3, [rsp + 48]    // xmm3 <- (rng/(255V))[12..15]
  movhlps xmm5, xmm6
  punpcklwd xmm5, xmm5      // xmm5 <- mask(ma <> 0)[12..15]
  pand xmm3, xmm5
  movups [rdx + 48], xmm3

  // cleanup
  movdqa xmm6, [rsp + 64]
  add rsp, 88
end;

procedure _cscvtRGB24ToHue_V16(pSrc, pDst: PByte; aCount: NativeInt);
var vbuff: array [0..4] of TVec16UI8;
    cnt: NativeInt;
    pEnd: PByte;
begin
  cnt := (aCount shr 4) shl 4;
  pEnd := pSrc + cnt * SizeOf(TRGB24);

  while pSrc < pEnd do begin
    _RGB24Sep16(pSrc, @vbuff[0]);
    _GetMaxAndRngFromRGBCh(@vbuff[0], @vbuff[3]);
    _Hue16(@vbuff[0], pDst);
    Inc(pSrc, SizeOf(TRGB24) * 16);
    Inc(pDst, SizeOf(Single) * 16);
  end;

  cnt := (aCount and 15);
  if cnt > 0 then
    _cscvtRGB24ToHue(pSrc, pDst, cnt);
end;

procedure _cssepRGB24ToHSV_V16(pSrc, pH, pS, pV: PByte; aCount: NativeInt);
var vbuff: array [0..4] of TVec16UI8;
    cnt: NativeInt;
    pEnd: PByte;
begin
  cnt := (aCount shr 4) shl 4;
  pEnd := pSrc + cnt * SizeOf(TRGB24);

  while pSrc < pEnd do begin
    _RGB24Sep16(pSrc, @vbuff[0]);
    _GetMaxAndRngFromRGBCh(@vbuff[0], @vbuff[3]);
    _Hue16(@vbuff[0], pH);
    _SV16(@vbuff[3], pS, pV);
    Inc(pSrc, SizeOf(TRGB24) * 16);
    Inc(pH, SizeOf(Single) * 16);
    Inc(pS, SizeOf(Single) * 16);
    Inc(pV, SizeOf(Single) * 16);
  end;

  cnt := (aCount and 15);
  if cnt > 0 then
    _cssepRGB24ToHSV(pSrc, pH, pS, pV, cnt);
end;

{$endif}

procedure cscvtRGB24ToHue(pSrc, pDst: PByte; aCount: NativeInt);
begin
{$if defined(ASMx64)}
  _cscvtRGB24ToHue_V16(pSrc, pDst, aCount);
{$else}
  _cscvtRGB24ToHue(pSrc, pDst, aCount);
{$endif}
end;

procedure cssepRGB24ToHSV(pSrc, pH, pS, pV: PByte; aCount: NativeInt);
begin
{$if defined(ASMx64)}
  _cssepRGB24ToHSV_V16(pSrc, pH, pS, pV, aCount);
{$else}
  _cssepRGB24ToHSV(pSrc, pH, pS, pV, aCount);
{$endif}
end;

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

procedure _CSSepC3(aSepProc: TIC3SepProc; const aSrc, aCh0, aCh1, aCh2: IImage);
var pSrc, pCh0, pCh1, pCh2: PByte;
    I, w, h, srcWs, ch0Ws, ch1Ws, ch2Ws: Integer;
begin
  w := aSrc.Width;
  h := aSrc.Height;
  Assert(
    (w = aCh0.Width) and  (w = aCh1.Width) and  (w = aCh2.Width) and
    (h = aCh0.Height) and (h = aCh1.Height) and (h = aCh2.Height)
  );
  pSrc := aSrc.Data;
  pCh0 := aCh0.Data;
  pCh1 := aCh1.Data;
  pCh2 := aCh2.Data;
  srcWs := aSrc.WidthStep;
  ch0Ws := aCh0.WidthStep;
  ch1Ws := aCh1.WidthStep;
  ch2Ws := aCh2.WidthStep;

  for I := 0 to h - 1 do begin
    aSepProc(pSrc, pCh0, pCh1, pCh2, w);
    Inc(pSrc, srcWs);
    Inc(pCh0, ch0Ws);
    Inc(pCh1, ch1Ws);
    Inc(pCh2, ch2Ws);
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

procedure HueSeparate(const aSrc: IImage<TRGB24>; var aDst: IImage<Single>);
begin
  if not Assigned(aDst) then
    aDst := TNDAImg<Single>.Create(aSrc.Width, aSrc.Height);
  _CSCvt(cscvtRGB24ToHue, aSrc, aDst);
end;

procedure HSVSeparate(const aSrc: IImage<TRGB24>; var H, S, V: IImage<Single>);
begin
  if not Assigned(H) then
    H := TNDAImg<Single>.Create(aSrc.Width, aSrc.Height);
  if not Assigned(S) then
    S := TNDAImg<Single>.Create(aSrc.Width, aSrc.Height);
  if not Assigned(V) then
    V := TNDAImg<Single>.Create(aSrc.Width, aSrc.Height);
  _CSSepC3(cssepRGB24ToHSV, aSrc, H, S, V);
end;

{$endregion}

{$region 'TCSUt'}

class function TCSUt.MatchQ<T>(const aImg: IImage): Boolean;
begin
  Result := (TypeInfo(T) = aImg.GetItemType);
end;

{$endregion}

end.
