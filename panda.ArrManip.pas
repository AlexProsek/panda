unit panda.ArrManip;

interface

uses
    panda.Intfs
  , panda.Arrays
  , panda.Nums
  , System.TypInfo
  , System.Math
  , System.SysUtils
  ;

{$I AsmDefs.inc}

type
  TNDAMan = class(TNDAUt)
  public const
    UNSUPPORTED_TYPE      = -1;
    UNSUPPORTED_TYPE_CVT  = -2;
    INV_CAST              = -3;
  protected
    class function FlippedAxesIdx(aDim: Integer; const aAxes: array of Integer): INDIndexSeq; static;
    class procedure _MatCpy<T>(const aSrc, aDst: TMatSpec); static;
    class procedure _Tr<T>(const aSrc, aDst: INDArray<T>; const aAxes: TArray<Integer>); static;
    class function _GetConcatShape(const aArrs: TArray<INDArray>; aAxis: Integer;
      out aOffsets: TNDAShape): TNDAShape;
    class function _GetPaddingShape(const aArrShape, aPadWidths: array of NativeInt): TArray<NativeInt>; static;
    class procedure _Riffle(const aArr, aItems, aRes: INDArray; aAxis: Integer; aFillFnc: TIPProcVV); static;
  public
    class function GetPart(const aArr: INDArray; const aIdx: INDIndexSeq): INDArray; static;
    class function SetPart(const aArr: INDArray; const aIdx: INDIndexSeq; const aValue: INDArray): Integer; static;

    /// <summary>
    ///  <c>Flatten<T>(aArr)</c> returns a copy of the array collapsed into one dimesion.
    /// </summary>
    class function Flatten<T>(const aArr: INDArray<T>): INDArray<T>; static;
    class function Transpose<T>(const aArr: INDArray<T>): INDArray<T>; overload; static;
    class function Transpose<T>(const aArr: INDArray<T>; const aAxes: TArray<Integer>): INDArray<T>; overload; static;
    class procedure Transpose<T>(const aSrc, aDst: INDArray<T>; const aAxes: TArray<Integer>); overload; static;
    class function TransposedView<T>(const aArr: INDArray<T>; const aAxes: TArray<Integer> = nil): INDArray<T>; static;

    /// <summary>
    ///   <c>Flip<T>(aArr, axis)</c> reverses the order of elements in an array along the given axis.
    /// </summary>
    class function Flip<T>(const aArr: INDArray<T>; aAxis: Integer = 0): INDArray<T>; overload; static;
    class function Flip<T>(const aArr: INDArray<T>; aAxes: array of Integer): INDArray<T>; overload; static;
    class function FlipAll<T>(const aArr: INDArray<T>): INDArray<T>; static;

    class function Concat<T>(const aArrs: array of INDArray<T>; aAxis: Integer = 0): INDArray<T>; static;

    class function Append<T>(const aArr: INDArray<T>; const aValue: T): INDArray<T>; overload; static;
    class function Append<T>(const aArr, aItem: INDArray<T>; aAxis: Integer = 0): INDArray<T>; overload; static; inline;

    class function Prepend<T>(const aArr: INDArray<T>; const aValue: T): INDArray<T>; overload; static;
    class function Prepend<T>(const aArr, aItem: INDArray<T>; aAxis: Integer = 0): INDArray<T>; overload; static; inline;

    class function Insert<T>(const aArr: INDArray<T>; const aItem: T; aPos: NativeInt): INDArray<T>; overload; static; inline;
    class function Insert<T>(const aArr: INDArray<T>; const aItems: array of T; const aPos: array of NativeInt): INDArray<T>; overload; static;
    class function Insert<T>(const aArr, aItem: INDArray<T>; aPos: NativeInt; aAxis: Integer = 0): INDArray<T>; overload; static; inline;
    class function Insert<T>(const aArr: INDArray<T>; const aItems: array of INDArray<T>;
      const aPos: array of NativeInt; aAxis: Integer = 0): INDArray<T>; overload; static;

    // Riffle[{e0, e1, ...}, x] gives {e0, x, e1, x, ...}
    // Riffle[{e0, e1, ...}, {x0, x1, ...}] gives {e0, x0, e1, x1, ...}
    class function Riffle<T>(const aArr: INDArray<T>; const aItem: T): INDArray<T>; overload; static;
    class function Riffle<T>(const aArr, aItems: INDArray<T>; aAxis: Integer = 0): INDArray<T>; overload; static;

    class function Pad<T>(const aArr: INDArray<T>; aPadWidth: NativeInt): INDArray<T>; overload; static; inline;
    class function Pad<T>(const aArr: INDArray<T>; aPadWidth: NativeInt; const aValue: T): INDArray<T>; overload; static; inline;
    class function Pad<T>(const aArr: INDArray<T>; aPadWidths: array of NativeInt; const aValue: T): INDArray<T>; overload; static;
  end;

{$region 'low-level functions'}

// Returns True if a matrix has continuous rows
function CRowsQ(const aMat: TMatSpec): Boolean; inline;
// Returns True if a matrix has continuous columns
function CColsQ(const aMat: TMatSpec): Boolean; inline;

procedure Tr4x4_4B(pSrc, pDst: PByte; aSrcStep, aDstStep: NativeInt);
procedure TrRxC_4B(pSrc, pDst: PByte; aSrcStep, aDstStep: NativeInt; R, C: NativeInt);
procedure CTr_4B(pSrc, pDst: PByte; aSrcRCnt, aSrcCCnt, aSrcRStep, aDstRStep: NativeInt);

procedure Tr8x8_2B(pSrc, pDst: PByte; aSrcStep, aDstStep: NativeInt);
procedure TrRxC_2B(pSrc, pDst: PByte; aSrcStep, aDstStep: NativeInt; R, C: NativeInt);
procedure CTr_2B(pSrc, pDst: PByte; aSrcRCnt, aSrcCCnt, aSrcRStep, aDstRStep: NativeInt);

procedure Tr8x8_1B(pSrc, pDst: PByte; aSrcStep, aDstStep: NativeInt);
procedure TrRxC_1B(pSrc, pDst: PByte; aSrcStep, aDstStep: NativeInt; R, C: NativeInt);
procedure CTr_1B(pSrc, pDst: PByte; aSrcRCnt, aSrcCCnt, aSrcRStep, aDstRStep: NativeInt);

{$endregion}

implementation

uses
    panda.DynArrayUtils
  ;

{$region 'low-level functions'}

function CRowsQ(const aMat: TMatSpec): Boolean;
begin
  Result := (aMat.ElSize = aMat.CStep);
end;

function CColsQ(const aMat: TMatSpec): Boolean;
begin
  Result := (aMat.ElSize = aMat.RStep);
end;

procedure Tr4x4_4B(pSrc, pDst: PByte; aSrcStep, aDstStep: NativeInt);
{$if defined(ASMx64)}
// RCX <- pSrc, RDX <- pDst, R8 <- aSrcStep, R9 <- aDstStep
asm
  movupd xmm0, [rcx]
  add rcx, r8
  movupd xmm1, [rcx]
  add rcx, r8
  movupd xmm2, [rcx]
  add rcx, r8
  movupd xmm3, [rcx]

  movaps  xmm4, xmm0
  unpcklps xmm4, xmm1        // xmm4 = a0 b0 a1 b1

  movaps  xmm5, xmm0
  unpckhps xmm5, xmm1        // xmm5 = a2 b2 a3 b3

  movaps  xmm0, xmm2
  unpcklps xmm0, xmm3        // xmm0 = c0 d0 c1 d1

  movaps  xmm1, xmm2
  unpckhps xmm1, xmm3        // xmm1 = c2 d2 c3 d3

  movaps  xmm2, xmm4
  shufps  xmm2, xmm0, $44    // col0 = a0 b0 c0 d0

  movaps  xmm3, xmm4
  shufps  xmm3, xmm0, $EE    // col1 = a1 b1 c1 d1

  movaps  xmm4, xmm5
  shufps  xmm4, xmm1, $44    // col2 = a2 b2 c2 d2

  movaps  xmm5, xmm5
  shufps  xmm5, xmm1, $EE    // col3 = a3 b3 c3 d3

  // xmm2, xmm3, xmm4, xmm5 = transposed rows
  movupd [rdx], xmm2
  add rdx, r9
  movupd [rdx], xmm3
  add rdx, r9
  movupd [rdx], xmm4
  add rdx, r9
  movupd [rdx], xmm5
end;
{$else}
begin
  PSingle(pDst)^              := PSingle(pSrc)^;
  PSingle(pDst + aDstStep)^   := PSingle(pSrc + cF32Sz)^;
  PSingle(pDst + 2*aDstStep)^ := PSingle(pSrc + 2*cF32Sz)^;
  pSingle(pDst + 3*aDstStep)^ := PSingle(pSrc + 3*cF32Sz)^;

  Inc(pSrc, aSrcStep);
  Inc(pDst, cF32Sz);

  PSingle(pDst)^              := PSingle(pSrc)^;
  PSingle(pDst + aDstStep)^   := PSingle(pSrc + cF32Sz)^;
  PSingle(pDst + 2*aDstStep)^ := PSingle(pSrc + 2*cF32Sz)^;
  pSingle(pDst + 3*aDstStep)^ := PSingle(pSrc + 3*cF32Sz)^;

  Inc(pSrc, aSrcStep);
  Inc(pDst, cF32Sz);

  PSingle(pDst)^              := PSingle(pSrc)^;
  PSingle(pDst + aDstStep)^   := PSingle(pSrc + cF32Sz)^;
  PSingle(pDst + 2*aDstStep)^ := PSingle(pSrc + 2*cF32Sz)^;
  pSingle(pDst + 3*aDstStep)^ := PSingle(pSrc + 3*cF32Sz)^;

  Inc(pSrc, aSrcStep);
  Inc(pDst, cF32Sz);

  PSingle(pDst)^              := PSingle(pSrc)^;
  PSingle(pDst + aDstStep)^   := PSingle(pSrc + cF32Sz)^;
  PSingle(pDst + 2*aDstStep)^ := PSingle(pSrc + 2*cF32Sz)^;
  pSingle(pDst + 3*aDstStep)^ := PSingle(pSrc + 3*cF32Sz)^;
end;
{$endif}

procedure TrRxC_4B(pSrc, pDst: PByte; aSrcStep, aDstStep: NativeInt; R, C: NativeInt);
var I: NativeInt;
    pEnd: PByte;
begin
  pEnd := pSrc + R * aSrcStep;
  while pSrc < pEnd do begin
    for I := 0 to C - 1 do
      PSingle(pDst + I*aDstStep)^ := PSingle(pSrc + I*cF32Sz)^;
    Inc(pSrc, aSrcStep);
    Inc(pDst, cF32Sz);
  end;
end;

procedure CTr_4B(pSrc, pDst: PByte; aSrcRCnt, aSrcCCnt, aSrcRStep, aDstRStep: NativeInt);
var pEnd, pSrcBlock, pDstBlock, pRowEnd: PByte;
    RRest, CRest, srcRWb: NativeInt;
const cBlockSz = 4;
begin
  srcRWb := ((aSrcCCnt shr 2) shl 2) * cF32Sz;
  CRest := aSrcCCnt and (cBlockSz - 1);
  RRest := aSrcRcnt and (cBlockSz - 1);
  pEnd := pSrc + ((aSrcRCnt shr 2) shl 2) * aSrcRStep;
  while pSrc < pEnd do begin
    pRowEnd := pSrc + srcRWb;
    pSrcBlock := pSrc;
    pDstBlock := pDst;
    while pSrcBlock < pRowEnd do begin
      Tr4x4_4B(pSrcBlock, pDstBlock, aSrcRStep, aDstRStep);
      Inc(pDstBlock, 4 * aDstRStep);
      Inc(pSrcBlock, 16);
    end;
    // remaining right block transposition
    if CRest > 0 then
      TrRxC_4B(pSrcBlock, pDstBlock, aSrcRStep, aDstRStep, 4, CRest);
    Inc(pSrc, 4 * aSrcRStep);
    Inc(pDst, 16);
  end;
  // remaining bottom blocks transposition
  if RRest > 0 then begin
    pRowEnd := pSrc + srcRWb;
    while pSrc < pRowEnd do begin
      TrRxC_4B(pSrc, pDst, aSrcRStep, aDstRStep, RRest, 4);
      Inc(pDst, 4 * aDstRStep);
      Inc(pSrc, 16);
    end;
    // right-bottom corner transposition
    TrRxC_4B(pSrc, pDst, aSrcRStep, aDstRStep, RRest, CRest);
  end;
end;

procedure Tr8x8_2B(pSrc, pDst: PByte; aSrcStep, aDstStep: NativeInt);
{$if defined(ASMx64)}
// RCX <- pSrc, RDX <- pDst, R8 <- aSrcStep, R9 <- aDstStep
asm
  mov rax, rdx
  // 1st tile
  // load src[:3,:3]
  mov r10, rcx
  movq xmm0, [r10]
  movq xmm1, [r10 + r8]
  lea r11,[r8*2]
  movq xmm2, [r10 + r11]
  lea r11,[r11 + r8]
  movq xmm3, [r10 + r11]

  // interleave rows
  punpcklwd xmm0, xmm2 // xmm0 <- (src[0,0], src[2,0], src[0,1], src[2,1],...)
  punpcklwd xmm1, xmm3 // xmm1 <- (src[1,0], src[3,0], src[1,1], src[3,1],...)
  movhlps xmm2, xmm0
  movhlps xmm3, xmm1
  punpcklwd xmm0, xmm1 // xmm0 <- (src[0,0],src[1,0],src[2,0],src[3,0], ...)
  punpcklwd xmm2, xmm3 // xmm1 <- (src[0,2],src[1,2],src[2,2],src[3,2], ...)
  movhlps xmm1, xmm0
  movhlps xmm3, xmm2

  // store dst[:3,:3]
  movq [rdx], xmm0
  add rdx, r9
  movq [rdx], xmm1
  add rdx, r9
  movq [rdx], xmm2
  add rdx, r9
  movq [rdx], xmm3
  add rdx, r9

  // 2nd tile
  // load src[:3,4:]
  lea r10, [rcx + 8]
  movq xmm0, [r10]
  movq xmm1, [r10 + r8]
  lea r11, [r8*2]
  movq xmm2, [r10 + r11]
  lea r11, [r11 + r8]
  movq xmm3, [r10 + r11]

  // interleave rows
  punpcklwd xmm0, xmm2
  punpcklwd xmm1, xmm3
  movhlps xmm2, xmm0
  movhlps xmm3, xmm1
  punpcklwd xmm0, xmm1
  punpcklwd xmm2, xmm3
  movhlps xmm1, xmm0
  movhlps xmm3, xmm2

  // store dst[4:,:3]
  movq [rdx], xmm0
  add rdx, r9
  movq [rdx], xmm1
  add rdx, r9
  movq [rdx], xmm2
  add rdx, r9
  movq [rdx], xmm3

  // 3rd tile
  // load src[4:,:3]
  lea rcx, [rcx + 4*r8]
  mov r10, rcx
  movq xmm0, [r10]
  movq xmm1, [r10 + r8]
  lea r11, [r8*2]
  movq xmm2, [r10 + r11]
  lea r11, [r11 + r8]
  movq xmm3, [r10 + r11]

  // interleave rows
  punpcklwd xmm0, xmm2
  punpcklwd xmm1, xmm3
  movhlps xmm2, xmm0
  movhlps xmm3, xmm1
  punpcklwd xmm0, xmm1
  punpcklwd xmm2, xmm3
  movhlps xmm1, xmm0
  movhlps xmm3, xmm2

  // store dst[:3,4:]
  lea rdx, [rax + 8]
  movq [rdx], xmm0
  add rdx, r9
  movq [rdx], xmm1
  add rdx, r9
  movq [rdx], xmm2
  add rdx, r9
  movq [rdx], xmm3
  add rdx, r9

  // 4th tile
  // load src[4:,4:]
  lea rcx, [rcx + 8]
  mov r10, rcx
  movq xmm0, [r10]
  movq xmm1, [r10 + r8]
  lea r11, [r8*2]
  movq xmm2, [r10 + r11]
  lea r11, [r11 + r8]
  movq xmm3, [r10 + r11]

  // interleave rows
  punpcklwd xmm0, xmm2
  punpcklwd xmm1, xmm3
  movhlps xmm2, xmm0
  movhlps xmm3, xmm1
  punpcklwd xmm0, xmm1
  punpcklwd xmm2, xmm3
  movhlps xmm1, xmm0
  movhlps xmm3, xmm2

  // store dst[4:,4:]
  movq [rdx], xmm0
  add rdx, r9
  movq [rdx], xmm1
  add rdx, r9
  movq [rdx], xmm2
  add rdx, r9
  movq [rdx], xmm3
end;
{$else}
var I: Integer;
begin
  for I := 0 to 7 do begin
    PWord(pDst)^               := PWord(pSrc)^;
    PWord(pDst +   aDstStep)^  := PWord(pSrc + 2)^;
    PWord(pDst + 2*aDstStep)^  := PWord(pSrc + 4)^;
    PWord(pDst + 3*aDstStep)^  := PWord(pSrc + 6)^;
    PWord(pDst + 4*aDstStep)^  := PWord(pSrc + 8)^;
    PWord(pDst + 5*aDstStep)^  := PWord(pSrc + 10)^;
    PWord(pDst + 6*aDstStep)^  := PWord(pSrc + 12)^;
    PWord(pDst + 7*aDstStep)^  := PWord(pSrc + 14)^;

    Inc(pSrc, aSrcStep);
    Inc(pDst, 2);
  end;
end;
{$endif}

procedure TrRxC_2B(pSrc, pDst: PByte; aSrcStep, aDstStep: NativeInt; R, C: NativeInt);
var I: NativeInt;
    pEnd: PByte;
begin
  pEnd := pSrc + R * aSrcStep;
  while pSrc < pEnd do begin
    for I := 0 to C - 1 do
      PWord(pDst + I*aDstStep)^ := PWord(pSrc + 2*I)^;
    Inc(pSrc, aSrcStep);
    Inc(pDst, 2);
  end;
end;

procedure CTr_2B(pSrc, pDst: PByte; aSrcRCnt, aSrcCCnt, aSrcRStep, aDstRStep: NativeInt);
var pEnd, pSrcBlock, pDstBlock, pRowEnd: PByte;
    RRest, CRest, srcRWb: NativeInt;
const cBlockSz = 8;
begin
  srcRWb := ((aSrcCCnt shr 3) shl 3) * 2;
  CRest := aSrcCCnt and (cBlockSz - 1);
  RRest := aSrcRcnt and (cBlockSz - 1);
  pEnd := pSrc + ((aSrcRCnt shr 3) shl 3) * aSrcRStep;
  while pSrc < pEnd do begin
    pRowEnd := pSrc + srcRWb;
    pSrcBlock := pSrc;
    pDstBlock := pDst;
    while pSrcBlock < pRowEnd do begin
      Tr8x8_2B(pSrcBlock, pDstBlock, aSrcRStep, aDstRStep);
      Inc(pDstBlock, 8 * aDstRStep);
      Inc(pSrcBlock, 16);
    end;
    // remaining right block transposition
    if CRest > 0 then
      TrRxC_2B(pSrcBlock, pDstBlock, aSrcRStep, aDstRStep, 8, CRest);
    Inc(pSrc, 8 * aSrcRStep);
    Inc(pDst, 16);
  end;
  // remaining bottom blocks transposition
  if RRest > 0 then begin
    pRowEnd := pSrc + srcRWb;
    while pSrc < pRowEnd do begin
      TrRxC_2B(pSrc, pDst, aSrcRStep, aDstRStep, RRest, 8);
      Inc(pDst, 8 * aDstRStep);
      Inc(pSrc, 16);
    end;
    // right-bottom corner transposition
    TrRxC_2B(pSrc, pDst, aSrcRStep, aDstRStep, RRest, CRest);
  end;
end;

procedure Tr8x8_1B(pSrc, pDst: PByte; aSrcStep, aDstStep: NativeInt);
{$if defined(ASMx64)}
// RCX <- pSrc, RDX <- pDst, R8 <- aSrcStep, R9 <- aDstStep
asm
  push rdi
  sub rsp, 32
  movdqa [rsp], xmm6
  movdqa [rsp + 16], xmm7

  // load 8 rows
  movq xmm0,[rcx]
  movq xmm1,[rcx + r8]
  lea r10,[r8*2]
  movq xmm2,[rcx + r10]
  lea r11,[r10 + r8]
  movq xmm3,[rcx + r11]
  lea r10,[r11 + r8]
  movq xmm4,[rcx + r10]
  lea r11,[r10 + r8]
  movq xmm5,[rcx + r11]
  lea r10,[r11 + r8]
  movq xmm6,[rcx + r10]
  lea r11,[r10 + r8]
  movq xmm7,[rcx + r11]

  // stage 1
  punpcklbw xmm0,xmm1
  punpcklbw xmm2,xmm3
  punpcklbw xmm4,xmm5
  punpcklbw xmm6,xmm7

  // stage 2
  movdqa xmm1,xmm0
  punpcklwd xmm0,xmm2
  punpckhwd xmm1,xmm2

  movdqa xmm3,xmm4
  punpcklwd xmm4,xmm6
  punpckhwd xmm3,xmm6

  // stage 3
  movdqa xmm2,xmm0
  punpckldq xmm0,xmm4
  punpckhdq xmm2,xmm4

  movdqa xmm6,xmm1
  punpckldq xmm1,xmm3
  punpckhdq xmm6,xmm3

  // store rows
  mov rdi, rdx
  movq [rdi],xmm0
  psrldq xmm0,8
  movq [rdx + r9],xmm0

  lea rdi, [rdx + 4*r9]
  movq [rdi],xmm1
  psrldq xmm1,8
  movq [rdi + r9],xmm1

  lea rdi, [rdx + 2*r9]
  movq [rdi],xmm2
  psrldq xmm2,8
  movq [rdi + r9],xmm2

  lea rdi, [rdi + 4*r9]
  movq [rdi],xmm6
  psrldq xmm6,8
  movq [rdi + r9],xmm6

  movdqa xmm6, [rsp]
  movdqa xmm7, [rsp + 16]
  add rsp, 32
  pop rdi
end;
{$else}
var I: Integer;
begin
  for I := 0 to 7 do begin
    pDst^                 := pSrc^;
    (pDst +   aDstStep)^  := (pSrc + 1)^;
    (pDst + 2*aDstStep)^  := (pSrc + 2)^;
    (pDst + 3*aDstStep)^  := (pSrc + 3)^;
    (pDst + 4*aDstStep)^  := (pSrc + 4)^;
    (pDst + 5*aDstStep)^  := (pSrc + 5)^;
    (pDst + 6*aDstStep)^  := (pSrc + 6)^;
    (pDst + 7*aDstStep)^  := (pSrc + 7)^;

    Inc(pSrc, aSrcStep);
    Inc(pDst);
  end;
end;
{$endif}

procedure TrRxC_1B(pSrc, pDst: PByte; aSrcStep, aDstStep: NativeInt; R, C: NativeInt);
var I: NativeInt;
    pEnd: PByte;
begin
  pEnd := pSrc + R * aSrcStep;
  while pSrc < pEnd do begin
    for I := 0 to C - 1 do
      (pDst + I*aDstStep)^ := (pSrc + I)^;
    Inc(pSrc, aSrcStep);
    Inc(pDst);
  end;
end;

procedure CTr_1B(pSrc, pDst: PByte; aSrcRCnt, aSrcCCnt, aSrcRStep, aDstRStep: NativeInt);
var pEnd, pSrcBlock, pDstBlock, pRowEnd: PByte;
    RRest, CRest, srcRWb: NativeInt;
const cBlockSz = 8;
begin
  srcRWb := ((aSrcCCnt shr 3) shl 3);
  CRest := aSrcCCnt and (cBlockSz - 1);
  RRest := aSrcRcnt and (cBlockSz - 1);
  pEnd := pSrc + ((aSrcRCnt shr 3) shl 3) * aSrcRStep;
  while pSrc < pEnd do begin
    pRowEnd := pSrc + srcRWb;
    pSrcBlock := pSrc;
    pDstBlock := pDst;
    while pSrcBlock < pRowEnd do begin
      Tr8x8_1B(pSrcBlock, pDstBlock, aSrcRStep, aDstRStep);
      Inc(pDstBlock, 8 * aDstRStep);
      Inc(pSrcBlock, 8);
    end;
    // remaining right block transposition
    if CRest > 0 then
      TrRxC_1B(pSrcBlock, pDstBlock, aSrcRStep, aDstRStep, 8, CRest);
    Inc(pSrc, 8 * aSrcRStep);
    Inc(pDst, 8);
  end;
  // remaining bottom blocks transposition
  if RRest > 0 then begin
    pRowEnd := pSrc + srcRWb;
    while pSrc < pRowEnd do begin
      TrRxC_1B(pSrc, pDst, aSrcRStep, aDstRStep, RRest, 8);
      Inc(pDst, 8 * aDstRStep);
      Inc(pSrc, 8);
    end;
    // right-bottom corner transposition
    TrRxC_1B(pSrc, pDst, aSrcRStep, aDstRStep, RRest, CRest);
  end;
end;

{$endregion}

{$region 'TNDAMan'}

class function TNDAMan.GetPart(const aArr: INDArray; const aIdx: INDIndexSeq): INDArray;
begin
  with aArr.GetItemType^ do begin
    case Kind of
      tkInteger:
        case TypeData^.OrdType of
          otSByte: Result := (aArr as INDArray<Int8>)[aIdx];
          otUByte: Result := (aArr as INDArray<UInt8>)[aIdx];
          otSWord: Result := (aArr as INDArray<Int16>)[aIdx];
          otUWord: Result := (aArr as INDArray<UInt16>)[aIdx];
          otSLong: Result := (aArr as INDArray<Int32>)[aIdx];
          otULong: Result := (aArr as INDArray<UInt32>)[aIdx];
        else
          exit(nil);
        end;

      tkFloat:
        case TypeData^.FloatType of
          ftSingle:   Result := (aArr as INDArray<Single>)[aIdx];
          ftDouble:   Result := (aArr as INDArray<Double>)[aIdx];
        else
          exit(nil);
        end;

      tkInt64:
        if TypeData^.MinInt64Value = 0 then
          Result := (aArr as INDArray<UInt64>)[aIdx]
        else
          Result := (aArr as INDArray<Int64>)[aIdx];
    else
      exit(nil);
    end;
  end;
end;

class function TNDAMan.SetPart(const aArr: INDArray; const aIdx: INDIndexSeq; const aValue: INDArray): Integer;
var cvtFnc: TIPProcVV;
    val: INDArray;
begin
  if SameQ(aArr.GetItemType, aValue.GetItemType) then
    val := aValue
  else if TryGetCvtFunc(aValue.GetItemType, aArr.GetItemType, cvtFnc) then begin
    val := Empty(aArr.GetItemType, aValue.Shape);
    MapSS(aValue, val, cvtFnc);
  end else
    exit(INV_CAST);

  with aArr.GetItemType^ do begin
    case Kind of
      tkInteger:
        case TypeData^.OrdType of
          otSByte, otUByte: (aArr as INDArray<Byte>)[aIdx] := (val as INDArray<Byte>);
          otSWord, otUWord: (aArr as INDArray<Word>)[aIdx] := (val as INDArray<Word>);
          otSLong, otULong: (aArr as INDArray<Integer>)[aIdx] := (val as INDArray<Integer>);
        end;

      tkFloat:
        case TypeData^.FloatType of
          ftSingle: (aArr as INDArray<Single>)[aIdx] := (val as INDArray<Single>);
          ftDouble: (aArr as INDArray<Double>)[aIdx] := (val as INDArray<Double>);
        end;

      tkInt64:
        (aArr as INDArray<Int64>)[aIdx] := (val as INDArray<Int64>);
    else
      exit(UNSUPPORTED_TYPE);
    end;
  end;

  Result := 0;
end;

class function TNDAMan.Flatten<T>(const aArr: INDArray<T>): INDArray<T>;
begin
  Result := TNDABuffer<T>.Create([aArr.Size]);
  Copy<T>(aArr, Result.Data);
end;

class procedure TNDAMan._MatCpy<T>(const aSrc, aDst: TMatSpec);
var pSrc, pDst, pSrcRow, pDstRow: PByte;
    pEnd, pRowEnd: PByte;
    rCnt, cCnt: NativeInt;
    srcRStep, srcCStep, dstRStep, dstCStep, srcRWidth: NativeInt;
begin
  rCnt := aSrc.NRows;
  cCnt := aSrc.NCols;
  srcRStep := aSrc.RStep;
  srcCStep := aSrc.CStep;
  dstRStep := aDst.RStep;
  dstCStep := aDst.CStep;
  pSrcRow := aSrc.Data;
  pDstRow := aDst.Data;
  srcRWidth := cCnt * srcCStep;
  pEnd := pSrcRow + rCnt * srcRStep;
  while pSrcRow <> pEnd do begin
    pSrc := pSrcRow;
    pDst := pDstRow;
    pRowEnd := pSrc + srcRWidth;
    while pSrc <> pRowEnd do begin
      TNDA<T>.PT(pDst)^ := TNDA<T>.PT(pSrc)^;
      Inc(pSrc, srcCStep);
      Inc(pDst, dstCStep);
    end;
    Inc(pSrcRow, srcRStep);
    Inc(pDstRow, dstRStep);
  end;
end;

class procedure TNDAMan._Tr<T>(const aSrc, aDst: INDArray<T>; const aAxes: TArray<Integer>);
var itSrc, itDst: TNDASliceIt;
    srcMat, dstMat: TMatSpec;
    nDim: Integer;
begin
  nDim := aSrc.NDim;
  if nDim < 2 then begin
    TNDAUt.Fill<T>(aDst, aSrc);
    exit;
  end;

  if nDim = 2 then begin
    GetMatSpec(aSrc, srcMat);
    GetMatSpec(aDst, dstMat);
    if CRowsQ(srcMat) and CRowsQ(dstMat) then begin
      case srcMat.ElSize of
        1: CTr_1B(srcMat.data, dstMat.Data, srcMat.NRows, srcMat.NCols,
          srcMat.RStep, dstMat.RStep
        );
        2: CTr_2B(srcMat.data, dstMat.Data, srcMat.NRows, srcMat.NCols,
          srcMat.RStep, dstMat.RStep
        );
        cF32Sz: CTr_4B(srcMat.data, dstMat.Data, srcMat.NRows, srcMat.NCols,
          srcMat.RStep, dstMat.RStep
        );
      else
        SwapMatAxes(dstMat);
        _MatCpy<T>(srcMat, dstMat);
      end;
    end else begin
      SwapMatAxes(dstMat);
      _MatCpy<T>(srcMat, dstMat);
    end;
    exit;
  end;

  itSrc := TNDASliceIt.Create(aSrc, 0, -3, aAxes);
  itDst := TNDASliceIt.Create(aDst, 0, -3);
  try
    GetMatSpec(itSrc.CurrentSlice, srcMat);
    GetMatSpec(itDst.CurrentSlice, dstMat);
    if CColsQ(srcMat) and CRowsQ(dstMat) then begin
      case srcMat.ElSize of
        1:
          while itSrc.MoveNext and itDst.MoveNext do begin
            CTr_1B(itSrc.Current, itDst.Current,
              // swap source axes back because they are swapped by iterator
              srcMat.NCols, srcMat.NRows,
              srcMat.CStep, dstMat.RStep
            );
          end;

        2:
          while itSrc.MoveNext and itDst.MoveNext do begin
            CTr_2B(itSrc.Current, itDst.Current,
              // swap source axes back because they are swapped by iterator
              srcMat.NCols, srcMat.NRows,
              srcMat.CStep, dstMat.RStep
            );
          end;

        cF32Sz:
          while itSrc.MoveNext and itDst.MoveNext do begin
            CTr_4B(itSrc.Current, itDst.Current,
              // swap source axes back because they are swapped by iterator
              srcMat.NCols, srcMat.NRows,
              srcMat.CStep, dstMat.RStep
            );
          end;
      else
        while itSrc.MoveNext and itDst.MoveNext do begin
          srcMat.Data := itSrc.Current;
          dstMat.Data := itDst.Current;
          _MatCpy<T>(srcMat, dstMat);
        end;
      end;
    end else begin
      while itSrc.MoveNext and itDst.MoveNext do begin
        srcMat.Data := itSrc.Current;
        dstMat.Data := itDst.Current;
        _MatCpy<T>(srcMat, dstMat);
      end;
    end;
  finally
    itSrc.Free;
    itDst.Free;
  end;
end;

class function TNDAMan.Transpose<T>(const aArr: INDArray<T>): INDArray<T>;
begin
  Result := Transpose<T>(aArr, TDynAUt.Range_I32(aArr.NDim - 1, 0, -1));
end;

class function TNDAMan.Transpose<T>(const aArr: INDArray<T>; const aAxes: TArray<Integer>): INDArray<T>;
begin
  Result := TNDABuffer<T>.Create(Permute<NativeInt>(aArr.Shape, aAxes));
  _Tr<T>(aArr, Result, aAxes);
end;

class procedure TNDAMan.Transpose<T>(const aSrc, aDst: INDArray<T>; const aAxes: TArray<Integer>);
var shape: TArray<NativeInt>;
const sErrMsg = 'Transpose error: output array shape doesn''t match.';
begin
  if aSrc.NDim <> aDst.NDim then
    raise ENDAShapeError.Create(sErrMsg);
  shape := Permute<NativeInt>(aSrc.Shape, aAxes);
  if not SameQ(shape, aDst.Shape) then
    raise ENDAShapeError.Create(sErrMsg);

  _Tr<T>(aSrc, aDst, aAxes);
end;

class function TNDAMan.TransposedView<T>(const aArr: INDArray<T>; const aAxes: TArray<Integer>): INDArray<T>;
var axes: TArray<Integer>;
begin
  if aAxes = nil then
    axes := TDynAUt.Range_I32(aArr.NDim - 1, 0, -1)
  else
    axes := aAxes;

  Result := TNDArrayWrapper<T>.CreatePermuted(aArr, axes);
end;

class function TNDAMan.FlippedAxesIdx(aDim: Integer; const aAxes: array of Integer): INDIndexSeq;
var I: Integer;
begin
  SetLength(Result, aDim);
  for I := 0 to High(Result) do
    Result[I] := TNDSpanIdx.Create(0, -1, 1);
  for I := 0 to High(aAxes) do
    with TNDSpanIdx(Result[aAxes[I]]) do begin
      Low := -1;
      High := 0;
      Step := -1;
    end;
end;

class function TNDAMan.Flip<T>(const aArr: INDArray<T>; aAxis: Integer = 0): INDArray<T>;
begin
  Result := Flip<T>(aArr, [aAxis]);
end;

class function TNDAMan.Flip<T>(const aArr: INDArray<T>; aAxes: array of Integer): INDArray<T>;
var idx: INDIndexSeq;
begin
  idx := FlippedAxesIdx(aArr.NDim, aAxes);
  Result := TNDArrayView<T>.Create(aArr, idx);
end;

class function TNDAMan.FlipAll<T>(const aArr: INDArray<T>): INDArray<T>;
begin
  Result := Flip<T>(aArr, TDynAUt.Range_I32(0, aArr.NDim - 1));
end;

{$region 'Concatenation functions'}

class function TNDAMan._GetConcatShape(const aArrs: TArray<INDArray>; aAxis: Integer;
  out aOffsets: TNDAShape): TNDAShape;
var I, J, arrCnt: NativeInt;
    sh: TNDAShape;
    nDim: Integer;
begin
  arrCnt := Length(aArrs);
  if arrCnt = 0 then
    raise ENDAValueError.Create('Need at least one array to concatenate.');

  Result := System.Copy(aArrs[0].Shape);
  nDim := Length(Result);
  if (aAxis < 0) or (nDim <= aAxis) then
    raise ENDAIndexError.Create('Axis index is out of range for concatenation.');

  SetLength(aOffsets, arrCnt);
  aOffsets[0] := Result[aAxis];
  for I := 1 to arrCnt - 1 do begin
    sh := aArrs[I].Shape;
    if Length(sh) <> nDim then
      raise ENDAShapeError.CreateFmt(
        'All the input arrays mut have same number of dimensions, but the array ' +
        'at index 0 has %d dimension(s) and the array at index %d has %d dimension(s).',
        [nDim, I, Length(sh)]
      );
    for J := 0 to aAxis - 1 do
      if (J <> aAxis) and (sh[J] <> Result[J]) then
        raise ENDAShapeError.CreateFmt(
          'All the input arrays dimensions except for the concatenation axis ' +
          'must match exactly, but along dimension %d, the array at index 0 has size %d ' +
          'and the array at index %d has size %d.',
          [J, Result[J], I, sh[J]]
        );
    Inc(Result[aAxis], sh[aAxis]);
    aOffsets[I] := Result[aAxis];
  end;
end;

class function TNDAMan.Concat<T>(const aArrs: array of INDArray<T>; aAxis: Integer): INDArray<T>;
var I, J, K: NativeInt;
    shRes, ao: TNDAShape;
    seq: INDIndexSeq;
begin
  shRes := _GetConcatShape(_ToUntypedArrays<T>(aArrs), aAxis, ao);
  Result := TNDABuffer<T>.Create(shRes);
  seq := NDIAllSeq(Length(shRes));
  J := 0;
  for I := 0 to High(aArrs) do begin
    K := ao[I];
    seq[aAxis] := NDISpan(J, K - 1);
    Result[seq] := aArrs[I];
    J := K;
  end;
end;

class function TNDAMan.Append<T>(const aArr: INDArray<T>; const aValue: T): INDArray<T>;
var len: NativeInt;
begin
  if aArr.NDim <> 1 then
    ENDAValueError.Create('Scalar value can be appended to a 1D array only.');

  len := aArr.Shape[0];
  Result := TNDABuffer<T>.Create([len + 1]);
  Result[[NDISpan(0, -2)]] := aArr;
  Result.SetItem([len], aValue);
end;

class function TNDAMan.Append<T>(const aArr, aItem: INDArray<T>; aAxis: Integer = 0): INDArray<T>;
begin
  if ScalarQ(aItem) then
    Result := Append<T>(aArr, TNDA<T>.PT(aItem.Data)^)
  else
    Result := Concat<T>([aArr, aItem], aAxis);
end;

class function TNDAMan.Prepend<T>(const aArr: INDArray<T>; const aValue: T): INDArray<T>;
var len: NativeInt;
begin
  if aArr.NDim <> 1 then
    ENDAValueError.Create('Scalar value can be prepended to a 1D array only.');

  len := aArr.Shape[0];
  Result := TNDABuffer<T>.Create([len + 1]);
  Result.SetItem([0], aValue);
  Result[[NDISpan(1, -1)]] := aArr;
end;

class function TNDAMan.Prepend<T>(const aArr, aItem: INDArray<T>; aAxis: Integer = 0): INDArray<T>;
begin
  if ScalarQ(aItem) then
    Result := Prepend<T>(aArr, TNDA<T>.PT(aItem.Data)^)
  else
    Result := Concat<T>([aItem, aArr], aAxis);
end;

class function TNDAMan.Insert<T>(const aArr: INDArray<T>; const aItem: T; aPos: NativeInt): INDArray<T>;
begin
  Result := Insert<T>(aArr, [aItem], [aPos]);
end;

class function TNDAMan.Insert<T>(const aArr: INDArray<T>; const aItems: array of T;
  const aPos: array of NativeInt): INDArray<T>;
var I, I0, I1, count, len: NativeInt;
    items: TArray<TIndexedValue<T>>;
    v: TNDAVecItems<T>;
begin
  if aArr.NDim <> 1 then
    ENDAValueError.Create('Scalar value can be inserted to a 1D array only.');

  count := Length(aItems);
  if count <> Length(aPos) then
    ENDAValueError.CreateFmt('Index array has different size (%d) than an items array (%d).',
      [Length(aPos), count]
    );

  if count = 0 then exit(aArr);

  len := aArr.Shape[0];
  Result := TNDABuffer<T>.Create([len + count]);
  SetLength(items, count);
  for I := 0 to count - 1 do
    items[I].Init((aPos[I] + len) mod len, aItems[I]);
  TDynAUt.MergeSortIndexedValuesByIndex<T>(items);
  v := Result;
  I0 := 0;
  for I := 0 to count - 1 do with items[I] do begin
    I1 := Idx - 1;
    if I1 >= I0 then
      Result[[NDISpan(I0 + I, I1 + I)]] := aArr[[NDISpan(I0, I1)]];
    v[Idx + I] := Value;
    I0 := Idx;
  end;
  I1 := len - 1;
  if I1 >= I0 then
    Result[[NDISpan(I0 + count, I1 + count)]] := aArr[[NDISpan(I0, I1)]];
end;

class function TNDAMan.Insert<T>(const aArr, aItem: INDArray<T>; aPos: NativeInt; aAxis: Integer): INDArray<T>;
begin
  Result := Insert<T>(aArr, [aItem], [aPos], aAxis);
end;

class function TNDAMan.Insert<T>(const aArr: INDArray<T>; const aItems: array of INDArray<T>;
  const aPos: array of NativeInt; aAxis: Integer): INDArray<T>;
var I, I0, I1, count, len: NativeInt;
    items: TArray<TIndexedValue<INDArray<T>>>;
    srcIdx, dstIdx: INDIndexSeq;
    sh: TNDAShape;
    vals: TArray<T>;
begin
  if (aArr.NDim = 1) and TNDAUt.TryAsScalars<T>(aItems, vals) then begin
    Result := Insert<T>(aArr, vals, aPos);
    exit;
  end;

  count := Length(aItems);
  if count <> Length(aPos) then
    ENDAValueError.CreateFmt('Index array has different size (%d) than an items array (%d).',
      [Length(aPos), count]
    );

  if count = 0 then exit(aArr);

  sh := System.Copy(aArr.Shape);
  len := sh[aAxis];
  sh[aAxis] := len + count;
  Result := TNDABuffer<T>.Create(sh);
  SetLength(items, count);
  for I := 0 to count - 1 do
    items[I].Init((aPos[I] + len) mod len, aItems[I]);
  TDynAUt.MergeSortIndexedValuesByIndex<INDArray<T>>(items);
  I0 := 0;
  srcIdx := NDIAllSeq(Length(sh));
  dstIdx := NDIAllSeq(Length(sh));
  for I := 0 to count - 1 do with items[I] do begin
    I1 := Idx - 1;
    if I1 >= I0 then begin
      srcIdx[aAxis] := NDISpan(I0, I1);
      dstIdx[aAxis] := NDISpan(I0 + I, I1 + I);
      Result[dstIdx] := aArr[srcIdx];
    end;
    dstIdx[aAxis] := NDI(Idx + I);
    Result[dstIdx] := Value;
    I0 := Idx;
  end;
  I1 := len - 1;
  if I1 >= I0 then begin
    srcIdx[aAxis] := NDISpan(I0, I1);
    dstIdx[aAxis] := NDISpan(I0 + count, I1 + count);
    Result[dstIdx] := aArr[srcIdx];
  end;
end;

class function TNDAMan.Riffle<T>(const aArr: INDArray<T>; const aItem: T): INDArray<T>;
var len, srcStep: NativeInt;
    pSrc, pDst, pEnd: PByte;
begin
  if aArr.NDim <> 1 then
    ENDAValueError.Create('Scalar value can be interleaved only with a 1D array.');

  len := aArr.Shape[0];
  Result := TNDABuffer<T>.Create([2 * len]);
  pSrc := aArr.Data;
  pDst := Result.Data;
  pEnd := pDst + (2 * len) * SizeOf(T);
  srcStep := aArr.Strides[0];
  while pDst < pEnd do  begin
    TNDA<T>.PT(pDst)^ := TNDA<T>.PT(pSrc)^;
    Inc(pDst, SizeOf(T));
    Inc(pSrc, srcStep);
    TNDA<T>.PT(pDst)^ := aItem;
    Inc(pDst, SizeOf(T));
  end;
end;

class procedure TNDAMan._Riffle(const aArr, aItems, aRes: INDArray; aAxis: Integer;
  aFillFnc: TIPProcVV);
var itArr, itItems, itRes: TNDASliceIt;
    axes: TArray<Integer>;
begin
  if aAxis > 0 then begin
    axes := TDynAUt.Range_I32(0, aArr.NDim - 1);
    axes[0] := aAxis;
    axes[aAxis] := 0;
  end;
  itArr := TNDASliceIt.Create(aArr, 0, 0, axes);
  itRes := TNDASliceIt.Create(aRes, 0, 0, axes);
  itItems := TNDASliceIt.Create(aItems, 0, 0);
  try
    while itArr.MoveNext and itRes.MoveNext do begin
      if not itItems.MoveNext then begin
        itItems.Reset;
        itItems.MoveNext;
      end;
      MapR(itRes.CurrentSlice, itArr.CurrentSlice, aFillFnc);
      itRes.MoveNext;
      MapR(itRes.CurrentSlice, itItems.CurrentSlice, aFillFnc);
    end;
  finally
    itArr.Free;
    itRes.Free;
    itItems.Free;
  end;
end;

class function TNDAMan.Riffle<T>(const aArr, aItems: INDArray<T>; aAxis: Integer): INDArray<T>;
var sh: TNDAShape;
begin
  sh := System.Copy(aArr.Shape);
  sh[aAxis] := 2 * sh[aAxis];
  Result := TNDABuffer<T>.Create(sh);
  _Riffle(aArr, aItems, Result, aAxis, TFuncUt<T>.FillR);
end;

{$endregion}

{$region 'Padding functions'}

class function TNDAMan.Pad<T>(const aArr: INDArray<T>; aPadWidth: NativeInt): INDArray<T>;
begin
  Result := Pad<T>(aArr, aPadWidth, Default(T));
end;

class function TNDAMan.Pad<T>(const aArr: INDArray<T>; aPadWidth: NativeInt; const aValue: T): INDArray<T>;
begin
  Result := Pad<T>(aArr, TDynAUt.ConstantArray<NativeInt>(aPadWidth, aArr.NDim), aValue)
end;

class function TNDAMan._GetPaddingShape(const aArrShape, aPadWidths: array of NativeInt): TArray<NativeInt>;
var pw: NativeInt;
    I: Integer;
begin
  if Length(aPadWidths) <> Length(aArrShape) then
    raise ENDAValueError.CreateFmt(
      'The pad widths array length (%d) and the array dimension (%d) are not the same.',
      [Length(aPadWidths), Length(aArrShape)]
    );

  SetLength(Result, Length(aPadWidths));
  for I := 0 to High(aPadWidths) do begin
    pw := aPadWidths[I];
    if pw < 0 then
      raise ENDAValueError.Create('Pad width cannot be negative value.');
    Result[I] := aArrShape[I] + 2 * pw;
  end;
end;

class function TNDAMan.Pad<T>(const aArr: INDArray<T>; aPadWidths: array of NativeInt; const aValue: T): INDArray<T>;
var arrSh, sh: TNDAShape;
    idx: INDIndexSeq;
    pw: NativeInt;
    I: Integer;
begin
  arrSh := aArr.Shape;
  sh := _GetPaddingShape(arrSh, aPadWidths);
  Result := TNDABuffer<T>.Create(sh);

  idx := NDIBlockSeq(aPadWidths, arrSh);
  Result[idx] := aArr;
  for I := 0 to High(arrSh) do begin
    pw := aPadWidths[I];
    idx[I] := NDISpan(0, pw - 1);
    TNDAUt.Fill<T>(Result[idx], aValue);
    idx[I] := NDISpan(pw + arrSh[I], -1);
    TNDAUt.Fill<T>(Result[idx], aValue);
    idx[I] := NDIAll();
  end;
end;

{$endregion}

{$endregion}

end.
