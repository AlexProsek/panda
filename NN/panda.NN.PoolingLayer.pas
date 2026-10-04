unit panda.NN.PoolingLayer;

interface

uses
    panda.Intfs
  , panda.Arrays
  , panda.NN
  , panda.Nums
  , System.Math
  , System.SysUtils
  ;

{$I AsmDefs.inc}

type
  TMaxPoolingLayer = class(TPoolingLayer)
  public
  end;

  TMaxPoolingLayer1D = class(TMaxPoolingLayer)
  public
    procedure Execute(const aInput: INDArray<Single>); override;
  end;

{$region 'low-level functions'}

type
  TVec4F32 = array [0..3] of Single;
  PVec4F32 = ^TVec4F32;

procedure Init(var aV: TVec4F32; aValue: Single); inline; overload;
procedure Init(pData: PSingle; const aValue: Single; aCount: NativeInt); overload;

function GetOutputSize(aInSize, aStride, aPoolSz: NativeInt): NativeInt; inline;

// pDst array has to be filled by sufficient low value (Single.MinValue for example)
procedure _MaxPool1D(pSrc, pDst: PSingle; aCount: NativeInt; aStride, aPoolSz: NativeInt);
procedure _MaxPool2D(pSrc, pDst: PSingle; aW, aH, aSrcWStep, aDstWStep: NativeInt;
  aStrideW, aStrideH, aPoolSzW, aPoolSzH: NativeInt);

{$endregion}

implementation

{$EXCESSPRECISION OFF} // to prevent Single -> Double conversion by x64 compiler

{$region 'low-level fucntions'}

procedure Init(var aV: TVec4F32; aValue: Single);
begin
  aV[0] := aValue;
  aV[1] := aValue;
  aV[2] := aValue;
  aV[3] := aValue;
end;

procedure Init(pData: PSingle; const aValue: Single; aCount: NativeInt);
var pEnd: PByte;
begin
  pEnd := PByte(pData) + aCount * cF32Sz;
  while PByte(pData) < pEnd do begin
    pData^ := aValue;
    Inc(pData);
  end;
end;

function GetOutputSize(aInSize, aStride, aPoolSz: NativeInt): NativeInt;
begin
  Assert((aStride > 0) and (aPoolSz > 0) and (aInSize > aPoolSz));
  Result := Floor((aInSize - aPoolSz) / aStride + 1);
end;

{$if defined(ASMx64)}

procedure _MaxPool1D4(pSrc, pDst: PSingle; aCount: NativeInt; aStride, aPoolSz: NativeInt);
// RCX <- pSrc, RDX <- pDst, R8 <- aCount, R9 <- aStride, [RBP + $30] <- aPoolSz
asm
  push rsi
  push rdi
  push r12
  push r13
  push r14
  mov rsi, rcx          // RSI <- pSrc
  mov rdi, rdx          // RDI <- pDst
  mov rdx, r9
  shl rdx, 2            // RDX <- aStride * SizeOf(Single)
  mov r10, r8           // R10 <- aCount
  shr r8, 2             // R8 <- aCount div 4
  jz @end
  mov rax, [rbp + $30]  // RAX <- aPoolSize
  mov rcx, rax
  and rcx, 1
  jnz @PoolOdd

@LE:
  mov r11, rsi          // R11 <- p0 := @src[0]
  lea r12, r11 + rdx    // R12 <- p1 :=@src[aStride]
  lea r13, r12 + rdx    // R13 <- p2 :=@src[2*aStride]
  lea r14, r13 + rdx    // R14 <- p3 :=@src[3*aStride]

  movups xmm0, [rdi]
  mov rcx, rax
  shr rcx, 1
@LPoolE:
  movq xmm1, [r11]
  movq xmm2, [r12]
  unpcklps xmm1, xmm2  // xmm1 <- (p0[0], p1[0], p0[1], p1[1])
  movq xmm3, [r13]
  movq xmm4, [r14]
  unpcklps xmm3, xmm4 // xmm3 <- (p2[0], p3[0], p2[1], p3[1])
  movaps xmm2, xmm3
  movhlps xmm2, xmm1  // xmm2 <- (p0[1], p1[1], p2[1], p3[1])
  movlhps xmm1, xmm3  // xmm1 <- (p0[0], p1[0], p2[0], p3[0])
  maxps xmm0, xmm1
  maxps xmm0, xmm2
  add r11, 8
  add r12, 8
  add r13, 8
  add r14, 8
  dec rcx
  jnz @LPoolE

  movups [rdi], xmm0
  add rdi, 16
  lea rsi, rsi + 4*rdx
  dec r8
  jnz @LE

  jmp @end

@PoolOdd:

@LO:
  mov r11, rsi          // R11 <- p0 := @src[0]
  lea r12, r11 + rdx    // R12 <- p1 :=@src[aStride]
  lea r13, r12 + rdx    // R13 <- p2 :=@src[2*aStride]
  lea r14, r13 + rdx    // R14 <- p3 :=@src[3*aStride]

  movups xmm0, [rdi]
  mov rcx, rax
  shr rcx, 1
@LPoolO:
  movq xmm1, [r11]
  movq xmm2, [r12]
  unpcklps xmm1, xmm2  // xmm1 <- (p0[0], p1[0], p0[1], p1[1])
  movq xmm3, [r13]
  movq xmm4, [r14]
  unpcklps xmm3, xmm4 // xmm3 <- (p2[0], p3[0], p2[1], p3[1])
  movaps xmm2, xmm3
  movhlps xmm2, xmm1  // xmm2 <- (p0[1], p1[1], p2[1], p3[1])
  movlhps xmm1, xmm3  // xmm1 <- (p0[0], p1[0], p2[0], p3[0])
  maxps xmm0, xmm1
  maxps xmm0, xmm2
  add r11, 8
  add r12, 8
  add r13, 8
  add r14, 8
  dec rcx
  jnz @LPoolO

  movss xmm1, [r11]
  movss xmm2, [r12]
  unpcklps xmm1, xmm2
  movss xmm3, [r13]
  movss xmm4, [r14]
  unpcklps xmm3, xmm4
  movlhps xmm1, xmm3
  maxps xmm0, xmm1

  movups [rdi], xmm0
  add rdi, 16
  lea rsi, rsi + 4*rdx
  dec r8
  jnz @LO

@end:
  pop r14
  pop r13
  pop r12
  pop rdi
  pop rsi
end;

{$endif}

procedure _MaxPool1D(pSrc, pDst: PSingle; aCount: NativeInt; aStride, aPoolSz: NativeInt);
var dstCnt: NativeInt;
    p: PSingle;
    pEnd, pPEnd: PByte;
    ma: Single;
begin
  dstCnt := GetOutputSize(aCount, aStride, aPoolSz);
  pEnd := PByte(pDst) + dstCnt * cF32Sz;

{$if defined(ASMx64)}
  aCount := dstCnt div 4;
  if aCount > 0 then begin
    _MaxPool1D4(pSrc, pDst, dstCnt, aStride, aPoolSz);
    Inc(pSrc, 4 * aCount * aStride);
    Inc(pDst, 4 * aCount);
  end;
{$endif}

  while PByte(pDst) < pEnd do begin
    p := pSrc;
    pPEnd := PByte(p) + aPoolSz * cF32Sz;
    ma := p^;
    Inc(p);
    while PByte(p) < pPEnd do begin
      if p^ > ma then ma := p^;
      Inc(p);
    end;
    if ma > pDst^ then
      pDst^ := ma;
    Inc(pSrc, aStride);
    Inc(pDst);
  end;
end;

procedure _MaxPool2D(pSrc, pDst: PSingle; aW, aH, aSrcWStep, aDstWStep: NativeInt;
  aStrideW, aStrideH, aPoolSzW, aPoolSzH: NativeInt);
var I, J, dstW, dstH: NativeInt;
    p: PSingle;
begin
  dstW := GetOutputSize(aW, aStrideW, aPoolSzW);
  dstH := GetOutputSize(aH, aStrideH, aPoolSzH);

  for I := 0 to dstH - 1 do begin
    p := pSrc;
    FillChar(pDst^, dstW * cF32Sz, 0);
    for J := 0 to aPoolSzH - 1 do begin
      _MaxPool1D(p, pDst, aW, aStrideW, aPoolSzW);
      Inc(PByte(p), aSrcWStep);
    end;
    Inc(PByte(pSrc), aStrideH * aSrcWStep);
    Inc(PByte(pDst), aDstWStep);
  end;
end;

{$endregion}

{$region 'TMaxPoolingLayer1D'}

procedure TMaxPoolingLayer1D.Execute(const aInput: INDArray<Single>);
begin
  Assert(aInput.NDim = 1);
end;

{$endregion}

end.
