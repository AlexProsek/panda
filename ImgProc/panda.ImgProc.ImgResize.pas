unit panda.ImgProc.ImgResize;

interface

uses
    System.Math
  , panda.Intfs
  , panda.Arrays
  , panda.ImgProc.Types
  , panda.ImgProc.Images
  ;

{$I AsmDefs.inc}

type
  TInterpMethod = (imNN (* nearest neighbour *), imBilinear);

  // base class for image size transformation
  TImgSizeTr = class abstract
  protected
    fInW, fInH, fInWs: NativeInt;
    fOutW, fOutH, fOutWs: NativeInt;
    fSx, fSy: Double;
    function GetPxSz: Integer; virtual; abstract;
  public
    procedure Init(aInW, aInH, aInWs, aOutW, aOutH, aOutWs: NativeInt); overload; virtual;
    procedure Init(aInW, aInH, aOutW, aOutH: NativeInt); overload;
    procedure Execute(aSrc, aDst: PByte); virtual; abstract;
    procedure SetInputSize(aInW, aInH, aInWs: NativeInt);
    procedure SetOutputSize(aOutW, aOutH, aOutWs: NativeInt);

    property InputWidth: NativeInt read fInW;
    property InputHeight: NativeInt read fInH;
    property InputWidthStep: NativeInt read fInWs;
    property OutputWidth: NativeInt read fOutW;
    property OutputHeight: NativeInt read fOutH;
    property OutputWidthStep: Nativeint read fOutWs;
  end;

  TNNImgSizeTr = class abstract(TImgSizeTr)
  protected
    function GetIdxTable(aScale: Double; aCount: NativeInt;
      aElemSize: Integer): TArray<NativeInt>;
    procedure TakeRowPixels(pSrc, pDst: PByte; pIdxs: PNativeInt; aCount: NativeInt); virtual; abstract;
  public
    procedure Init(aInW, aInH, aInWs, aOutW, aOutH, aOutWs: NativeInt); override;
    procedure Execute(aSrc, aDst: PByte); override;
  end;

  TNNImgSizeTr<T> = class(TNNImgSizeTr)
  protected const
    cPxSz = SizeOf(T);
  protected type
    PT = ^T;
  protected
    function GetPxSz: Integer; override;
    procedure TakeRowPixels(pSrc, pDst: PByte; pIdxs: PNativeInt; aCount: NativeInt); override;
  end;

  TNNImgSizeTrUI8   = TNNImgSizeTr<Byte>;
  TNNImgSizeTrUI16  = TNNImgSizeTr<Word>;
  TNNImgSizeTrRGB24 = TNNImgSizeTr<TRGB24>;

  // base class for bilinear image transformation
  TBLImgSizeTr = class abstract(TImgSizeTr)
  protected type
    TRowInterpProc = procedure (pR1, pR2: PByte; dXY: PDouble; pDst: PByte; aDstW: NativeInt);
  protected
    fRowInterpProc: TRowInterpProc;
    procedure InterpolateLastColumn(pSrc, pDst: PByte); virtual; abstract;
    procedure InterpolateLastRow(pSrc, pDst: PByte); virtual; abstract;
    procedure SetRightBottomCorner(pSrc, pDst: PByte); virtual; abstract;
  public
    procedure Execute(aSrc, aDst: PByte); override;
  end;

  TBLImgSizeTr<T> = class abstract(TBLImgSizeTr)
  protected const
    cPxSz = SizeOf(T);
  protected type
    PT = ^T;
  protected
    function GetPxSz: Integer; override;
    procedure SetRightBottomCorner(pSrc, pDst: PByte); override;
  end;

  TBLImgSizeTrUI8 = class(TBLImgSizeTr<Byte>)
  protected
    procedure InterpolateLastColumn(pSrc, pDst: PByte); override;
    procedure InterpolateLastRow(pSrc, pDst: PByte); override;
  public
    procedure AfterConstruction; override;
  end;

  TBLImgSizeTrRGB24 = class(TBLImgSizeTr<TRGB24>)
  protected const
    cPxSz = SizeOf(TRGB24);
  protected
    procedure InterpolateLastColumn(pSrc, pDst: PByte); override;
    procedure InterpolateLastRow(pSrc, pDst: PByte); override;
  public
    procedure AfterConstruction; override;
  end;


  procedure ImageResize(const aSrc: IImage<Byte>; var aDst: IImage<Byte>;
    aMethod: TInterpMethod = imBilinear); overload;
  procedure ImageResize(const aSrc: IImage<TRGB24>; var aDst: IImage<TRGB24>;
    aMethod: TInterpMethod = imBilinear); overload;

{$region 'low-level functions}

procedure RowBLInterp_UI8(pR1, pR2: PByte; dXY: PDouble; pDst: PByte; aDstW: NativeInt);
procedure RowBLInterp_RGB24(pR1, pR2: PByte; dXY: PDouble; pDst: PByte; aDstW: NativeInt);

{$endregion}

implementation

procedure ApplyTr(aTr: TImgSizeTr; const aSrc: IImage; var aDst: IImage);
begin
  aTr.Init(aSrc.Width, aSrc.Height, aSrc.WidthStep, aDst.Width, aDst.Height, aDst.WidthStep);
  aTr.Execute(aSrc.Data, aDst.Data);
end;

procedure ImageResize(const aSrc: IImage<Byte>; var aDst: IImage<Byte>;
  aMethod: TInterpMethod);
var tr: TImgSizeTr;
    dst: IImage;
begin
  Assert(Assigned(aSrc) and Assigned(aDst));

  if SameSizeQ(aSrc, aDst) then begin
    TImgUt.CopyTo<Byte>(aSrc, aDst);
    exit;
  end;

  case aMethod of
    imNN:       tr := TNNImgSizeTrUI8.Create;
    imBilinear: tr := TBLImgSizeTrUI8.Create;
  else

    exit;
  end;

  try
    dst := aDst;
    ApplyTr(tr, aSrc, dst);
  finally
    tr.Free;
  end;
end;

procedure ImageResize(const aSrc: IImage<TRGB24>; var aDst: IImage<TRGB24>;
  aMethod: TInterpMethod); overload;
var tr: TImgSizeTr;
    dst: IImage;
begin
  Assert(Assigned(aSrc) and Assigned(aDst));

  if SameSizeQ(aSrc, aDst) then begin
    TImgUt.CopyTo<TRGB24>(aSrc, aDst);
    exit;
  end;

  case aMethod of
    imNN:       tr := TNNImgSizeTrRGB24.Create;
    imBilinear: tr := TBLImgSizeTrRGB24.Create;
  else

    exit;
  end;

  try
    dst := aDst;
    ApplyTr(tr, aSrc, dst);
  finally
    tr.Free;
  end;
end;

{$region 'low-level functions}

procedure RowBLInterp_UI8(pR1, pR2: PByte; dXY: PDouble; pDst: PByte; aDstW: NativeInt);
{$if defined(ASMx64)}
{$CODEALIGN 16}
const cOnes: array [0..1] of Double = (1.0, 1.0);
// RCX <- pR1, RDX <- pR2, R8 <- @dXY, R9 <- pDst, [RBP + $30] <- aDstW
asm
  mov r10, [rbp + $30]
  test r10, r10
  jz @end

  push rbx
  sub rsp, 56 // 48 for xmm backup + 8 stack alignment
  movupd [rsp], xmm6
  movupd [rsp + 16], xmm7
  movupd [rsp + 32], xmm8

  movddup xmm0, [r8]  // xmm0 <- sx
  movddup xmm5, [r8 + 8] // xmm5 <- (dy, dy)
  movupd xmm6, cOnes
  xorps xmm8, xmm8
  movq xmm8, xmm6     // xmm8 <- (1, 0)
  movlhps xmm0, xmm6  // xmm0[64:] <- 1.0
  subpd xmm6, xmm5    // xmm6 <- (1 - dy, 1 - dy)
  xorps xmm7, xmm7    // xmm7 <- (0, 0)
  xorps xmm1, xmm1    // xmm1 <- x := 0
  xor r11, r11        // rax <- Jin := 0
@L:
  cvttsd2si r11, xmm1 // r11 <- Jin := trunc(x)
  cvtsi2sd xmm2, r11  // xmm2 <- Double(trunc(x))
  movsd xmm4, xmm1
  subsd xmm4, xmm2    // xmm4 <- dx := x - trunc(x);
  movddup xmm2, xmm4  // xmm2 <- (dx, dx)
  movapd xmm4, xmm8   // xmm4 <- (1, 0)
  addsubpd xmm4, xmm2 // xmm4 <- (1 - dx, dx)

  mov ax, [rdx + r11]
  shl eax, 16
  mov ax, [rcx + r11]
  movd xmm2, eax        // xmm2[:31] <- (v_00, v_01, v_10, v_11)
  punpcklbw xmm2, xmm7
  punpcklbw xmm2, xmm7
  movhlps xmm3, xmm2
  cvtdq2pd xmm2, xmm2   // xmm2 <- (v_00, v_01)
  cvtdq2pd xmm3, xmm3   // xmm3 <- (v_10, v_11)

  mulpd xmm2, xmm6      // xmm2 <- ((1-dy)*v_00, (1-dy)*v_01)
  mulpd xmm3, xmm5      // xmm3 <- (dy*v_10, dy*v_11)
  addpd xmm2, xmm3      // xmm2 <- (v_0, v_1)

  mulpd xmm4, xmm2      // xmm4 <- ((1-dx)*v_0, dx*v_1)
  movhlps xmm0, xmm4    // xmm2[:63] <- dx*v_1
  addsd xmm0, xmm4      // xmm2[:64] <- (1-dx)*v_0 + dx*v_1
  cvtsd2si ebx, xmm0
  mov [r9], bl
  inc r9

  movq xmm2, [r8]       // xmm0 <- sx
  addsd xmm1, xmm2      // x += sx
  dec r10
  jnz @L

  movupd xmm8, [rsp + 32]
  movupd xmm7, [rsp + 16]
  movupd xmm6, [rsp]
  add rsp, 56
  pop rbx
@end:
end;
{$else}
var J, Jin: NativeInt;
    x, dx, dy, dyc, v0, v1: Double;
    v: array [0..1, 0..1] of Double;
    p: PByte;
begin
  dx := dXY^;
  Inc(dXY);
  dy := dXY^;
  dyc := 1 - dy;
  for J := 0 to aDstW - 1 do begin
    x := dx * J;
    Jin := Floor(x);
    x := x - Jin;
    p := pR1 + Jin;
    v[0, 0] := p^;
    v[0, 1] := (p + 1)^;
    p := pR2 + Jin;
    v[1, 0] := p^;
    v[1, 1] := (p + 1)^;
    v0 := dyc * v[0, 0] + dy * v[1, 0];
    v1 := dyc * v[0, 1] + dy * v[1, 1];
    pDst^ := Byte(Round((1 - x) * v0 + x * v1));
    Inc(pDst);
  end;
end;
{$endif}

procedure RowBLInterp_RGB24(pR1, pR2: PByte; dXY: PDouble; pDst: PByte; aDstW: NativeInt);
{$if defined(ASMx64)}
{$CODEALIGN 16}
const cOnes: array [0..1] of Double = (1.0, 1.0);
      cBMask: UInt64 = $FF000000FF;
// RCX <- pR1, RDX <- pR2, R8 <- @dXY, R9 <- pDst, [RBP + $30] <- aDstW
asm
  mov r10, [rbp + $30]
  test r10, r10
  jz @end

  push rsi
  push rdi
  push rbx
  push r12
  push r13
  sub rsp, 72 // 64 for xmm6..xmm9 backup + 8 stack alignment
  movupd [rsp], xmm6
  movupd [rsp + 16], xmm7
  movupd [rsp + 32], xmm8
  movupd [rsp + 48], xmm9

  movddup xmm0, [r8]  // xmm0 <- sx
  movddup xmm5, [r8 + 8] // xmm5 <- (dy, dy)
  movupd xmm6, cOnes
  xorps xmm8, xmm8
  movq xmm8, xmm6     // xmm8 <- (1, 0)
  movlhps xmm0, xmm6  // xmm0[64:] <- 1.0
  subpd xmm6, xmm5    // xmm6 <- (1 - dy, 1 - dy)
  xorps xmm7, xmm7    // xmm7 <- (0, 0)
  xorps xmm1, xmm1    // xmm1 <- x := 0
  xor r11, r11        // rax <- Jin := 0
  mov r13, cBMask
@L:
  cvttsd2si r11, xmm1 // r11 <- Jin := trunc(x)
  cvtsi2sd xmm2, r11  // xmm2 <- Double(trunc(x))
  movsd xmm4, xmm1
  subsd xmm4, xmm2    // xmm4 <- dx := x - trunc(x);
  movddup xmm2, xmm4  // xmm2 <- (dx, dx)
  movapd xmm9, xmm8   // xmm9 <- (1, 0)
  addsubpd xmm9, xmm2 // xmm9 <- (1 - dx, dx)

  lea r11, 2*r11 + r11
  mov eax, [rcx + r11 + 2]
  mov rsi, rax
  and rsi, $FFFFFF00
  shl rsi, 24         // rsi <- (0, 0, 0, 0, r01, g01, b01, 0)
  shl rax, 16
  mov ax, [rcx + r11]
  and rax, $FFFFFF
  or rsi, rax         // rsi <- (r00, g00, b00, 0, r01, g01, b01, 0)

  mov eax, [rdx + r11 + 2]
  mov rdi, rax
  and rdi, $FFFFFF00
  shl rdi, 24
  shl rax, 16
  mov ax, [rdx + r11]
  and rax, $FFFFFF
  or rdi, rax         // rdi <- (r10, g10, b10, 0, r11, g11, b11, 0)

  // red channel interpolation
  mov r12, rsi
  and r12, r13
  movq xmm2, r12
  cvtdq2pd xmm2, xmm2   // xmm2 <- (v_00, v_01) := (double(r00), double(r01))
  mov r12, rdi
  and r12, r13
  movq xmm3, r12
  cvtdq2pd xmm3, xmm3   // xmm2 <- (v_00, v_01) := (double(r00), double(r01))

  mulpd xmm2, xmm6      // xmm2 <- ((1-dy)*v_00, (1-dy)*v_01)
  mulpd xmm3, xmm5      // xmm3 <- (dy*v_10, dy*v_11)
  addpd xmm2, xmm3      // xmm2 <- (v_0, v_1)

  movapd xmm4, xmm9     // xmm4 <- (1-dx, dx)
  mulpd xmm4, xmm2      // xmm4 <- ((1-dx)*v_0, dx*v_1)
  movhlps xmm0, xmm4    // xmm2[:63] <- dx*v_1
  addsd xmm0, xmm4      // xmm2[:64] <- (1-dx)*v_0 + dx*v_1
  cvtsd2si ebx, xmm0
  mov [r9], bl
  inc r9

//  // green channel interpolation
  shr rsi, 8
  shr rdi, 8
  mov r12, rsi
  and r12, r13
  movq xmm2, r12
  cvtdq2pd xmm2, xmm2   // xmm2 <- (v_00, v_01) := (double(r00), double(r01))
  mov r12, rdi
  and r12, r13
  movq xmm3, r12
  cvtdq2pd xmm3, xmm3   // xmm2 <- (v_00, v_01) := (double(r00), double(r01))

  mulpd xmm2, xmm6      // xmm2 <- ((1-dy)*v_00, (1-dy)*v_01)
  mulpd xmm3, xmm5      // xmm3 <- (dy*v_10, dy*v_11)
  addpd xmm2, xmm3      // xmm2 <- (v_0, v_1)

  movapd xmm4, xmm9     // xmm4 <- (1-dx, dx)
  mulpd xmm4, xmm2      // xmm4 <- ((1-dx)*v_0, dx*v_1)
  movhlps xmm0, xmm4    // xmm2[:63] <- dx*v_1
  addsd xmm0, xmm4      // xmm2[:64] <- (1-dx)*v_0 + dx*v_1
  cvtsd2si ebx, xmm0
  mov [r9], bl
  inc r9

//  // blue channel interpolation
  shr rsi, 8
  shr rdi, 8
  mov r12, rsi
  and r12, r13
  movq xmm2, r12
  cvtdq2pd xmm2, xmm2   // xmm2 <- (v_00, v_01) := (double(r00), double(r01))
  mov r12, rdi
  and r12, r13
  movq xmm3, r12
  cvtdq2pd xmm3, xmm3   // xmm2 <- (v_00, v_01) := (double(r00), double(r01))

  mulpd xmm2, xmm6      // xmm2 <- ((1-dy)*v_00, (1-dy)*v_01)
  mulpd xmm3, xmm5      // xmm3 <- (dy*v_10, dy*v_11)
  addpd xmm2, xmm3      // xmm2 <- (v_0, v_1)

  movapd xmm4, xmm9     // xmm4 <- (1-dx, dx)
  mulpd xmm4, xmm2      // xmm4 <- ((1-dx)*v_0, dx*v_1)
  movhlps xmm0, xmm4    // xmm2[:63] <- dx*v_1
  addsd xmm0, xmm4      // xmm2[:64] <- (1-dx)*v_0 + dx*v_1
  cvtsd2si ebx, xmm0
  mov [r9], bl
  inc r9

  movq xmm2, [r8]       // xmm0 <- sx
  addsd xmm1, xmm2      // x += sx
  dec r10
  jnz @L

  movupd xmm9, [rsp + 48]
  movupd xmm8, [rsp + 32]
  movupd xmm7, [rsp + 16]
  movupd xmm6, [rsp]
  add rsp, 72
  pop r13
  pop r12
  pop rbx
  pop rdi
  pop rsi
@end:
end;
{$else}
var J, Jin: NativeInt;
    x, dx, dy, dyc, v0, v1: Double;
    v: array [0..1, 0..1] of TRGB24;
    p: PRGB24;
    res: TRGB24;
const cPxSz = SizeOf(TRGB24);
begin
  dx := dXY^;
  Inc(dXY);
  dy := dXY^;
  dyc := 1 - dy;
  for J := 0 to aDstW - 1 do begin
    x := dx * J;
    Jin := Floor(x);
    x := x - Jin;
    p := PRGB24(pR1 + Jin * cPxSz);
    v[0, 0] := p^;
    Inc(p);
    v[0, 1] := p^;
    p := PRGB24(pR2 + Jin * cPxSz);
    v[1, 0] := p^;
    Inc(p);
    v[1, 1] := p^;
    v0 := dyc * v[0, 0].R + dy * v[1, 0].R;
    v1 := dyc * v[0, 1].R + dy * v[1, 1].R;
    res.R := Byte(Round((1 - x) * v0 + x * v1));
    v0 := dyc * v[0, 0].G + dy * v[1, 0].G;
    v1 := dyc * v[0, 1].G + dy * v[1, 1].G;
    res.G := Byte(Round((1 - x) * v0 + x * v1));
    v0 := dyc * v[0, 0].B + dy * v[1, 0].B;
    v1 := dyc * v[0, 1].B + dy * v[1, 1].B;
    res.B := Byte(Round((1 - x) * v0 + x * v1));
    PRGB24(pDst)^ := res;
    Inc(pDst, cPxSz);
  end;
end;
{$endif}

{$endregion}

{$region 'TImgSizeTr'}

procedure TImgSizeTr.Init(aInW, aInH, aInWs, aOutW, aOutH, aOutWs: NativeInt);
begin
  Assert(
    (aInW > 0) and (aInH > 0) and (aOutW > 0) and (aOutH > 0) and
    (aInWs >= aInW) and (aOutWs >= aOutW)
  );

  fInW := aInW;
  fInH := aInH;
  fOutW := aOutW;
  fOutH := aOutH;
  fInWs := aInWs;
  fOutWs := aOutWs;
  if fOutW <> fInW then
    fSx := (fInW - 1) / (fOutW - 1)
  else
    fSx := 1;
  if fOutH <> fInH then
    fSy := (fInH - 1) / (fOutH - 1)
  else
    fSy := 1;
end;

procedure TImgSizeTr.Init(aInW, aInH, aOutW, aOutH: NativeInt);
var pxSz: Integer;
begin
  pxSz := GetPxSz;
  Init(aInW, aInH, aInW * pxSz, aOutW, aOutH, aOutW * pxSz);
end;

procedure TImgSizeTr.SetInputSize(aInW, aInH, aInWs: NativeInt);
begin
  Init(aInW, aInH, aInWs, fOutW, fOutH, fOutWs);
end;

procedure TImgSizeTr.SetOutputSize(aOutW, aOutH, aOutWs: NativeInt);
begin
  Init(fInW, fInH, fInWs, aOutW, aOutH, aOutWs);
end;

{$endregion}

{$region 'TNNImgSizeTr'}

procedure TNNImgSizeTr.Init(aInW, aInH, aInWs, aOutW, aOutH, aOutWs: NativeInt);
begin
  Assert(
    (aInW > 0) and (aInH > 0) and (aOutW > 0) and (aOutH > 0) and
    (aInWs >= aInW) and (aOutWs >= aOutW)
  );

  fInW := aInW;
  fInH := aInH;
  fOutW := aOutW;
  fOutH := aOutH;
  fInWs := aInWs;
  fOutWs := aOutWs;
  fSx := fInW / fOutW;
  fSy := fInH / fOutH;
end;

function TNNImgSizeTr.GetIdxTable(aScale: Double; aCount: NativeInt;
  aElemSize: Integer): TArray<NativeInt>;
var I: NativeInt;
begin
  SetLength(Result, aCount);
  for I := 0 to High(Result) do
    Result[I] := Floor(aScale * I) * aElemSize;
end;

procedure TNNImgSizeTr.Execute(aSrc, aDst: PByte);
var srcIdxs: TArray<NativeInt>;
    I: NativeInt;
begin
  srcIdxs := GetIdxTable(fSx, fOutW, GetPxSz);
  for I := 0 to fOutH - 1 do
    TakeRowPixels(
      aSrc + Floor(fSy * I) * fInWs,
      aDst + I * fOutWs,
      PNativeInt(srcIdxs), fOutW
    );
end;

{$endregion}

{$region 'TNNResizeAlgorithm<T>'}

function TNNImgSizeTr<T>.GetPxSz: Integer;
begin
  Result := cPxSz;
end;

procedure TNNImgSizeTr<T>.TakeRowPixels(pSrc, pDst: PByte; pIdxs: PNativeInt;
  aCount: NativeInt);
var pEnd, pEndu: PByte;
type PT = ^T;
begin
  pEnd := PByte(pIdxs) + aCount * SizeOf(NativeInt);
  pEndu := PByte(pIdxs) + (aCount and (not NativeInt(3))) * SizeOf(NativeInt);
  while PByte(pIdxs) < pEndu do begin
    PT(pDst)^  := PT(pSrc + pIdxs^)^;
    Inc(PT(pDst));
    Inc(pIdxs);
    PT(pDst)^  := PT(pSrc + pIdxs^)^;
    Inc(PT(pDst));
    Inc(pIdxs);
    PT(pDst)^  := PT(pSrc + pIdxs^)^;
    Inc(PT(pDst));
    Inc(pIdxs);
    PT(pDst)^  := PT(pSrc + pIdxs^)^;
    Inc(PT(pDst));
    Inc(pIdxs);
  end;
  while PByte(pIdxs) < pEnd do begin
    PT(pDst)^  := PT(pSrc + pIdxs^)^;
    Inc(PT(pDst));
    Inc(pIdxs);
  end;
end;

{$endregion}

{$region 'TBLImgSizeTr'}

procedure TBLImgSizeTr.Execute(aSrc, aDst: PByte);
var I, Iin: NativeInt;
    y: Double;
    dxy: array [0..1] of Double;
    pR1, pR2, pDst: PByte;
begin
  dxy[0] := fSx;
  for I := 0 to fOutH - 2 do begin
    pDst := aDst + I * fOutWs;
    y := fSy * I;
    Iin := Floor(y);
    dxy[1] := y - Iin;
    pR1 := aSrc + Iin * fInWs;
    pR2 := pR1 + fInWs;
    fRowInterpProc(pR1, pR2, @dxy, pDst, fOutW - 1);
  end;

  InterpolateLastColumn(aSrc, aDst);
  InterpolateLastRow(aSrc, aDst);
  SetRightBottomCorner(aSrc, aDst);
end;

{$endregion}

{$region 'TBLImgSizeTr<T>'}

function TBLImgSizeTr<T>.GetPxSz: Integer;
begin
  Result := cPxSz;
end;

procedure TBLImgSizeTr<T>.SetRightBottomCorner(pSrc, pDst: PByte);
begin
  Inc(pSrc, fInWs * fInH - cPxSz);
  Inc(pDst, fOutWs * fOutH - cPxSz);
  PT(pDst)^ := PT(pSrc)^;
end;

{$endregion}

{$region 'TBLImgSizeTrUI8'}

procedure TBLImgSizeTrUI8.AfterConstruction;
begin
  inherited;
  fRowInterpProc := RowBLInterp_UI8;
end;

procedure TBLImgSizeTrUI8.InterpolateLastColumn(pSrc, pDst: PByte);
var I, Iin: NativeInt;
    v: array [0..1] of Double;
    y: Double;
    pv: PByte;
begin
  Inc(pSrc, fInW - 1);
  Inc(pDst, fOutW - 1);
  for I := 0 to fOutH - 2 do begin
    y := fSy * I;
    Iin := Floor(y);
    y := y - Iin;
    pv := pSrc + Iin * fInWs;
    v[0] := pv^;
    Inc(pv, fInWs);
    v[1] := pv^;
    pDst^ := Byte(Round((1 - y) * v[0] + y * v[1]));
    Inc(pDst, fOutWs);
  end;
end;

procedure TBLImgSizeTrUI8.InterpolateLastRow(pSrc, pDst: PByte);
var I, Iin: NativeInt;
    v: array [0..1] of Double;
    x: Double;
    pv: PByte;
begin
  Inc(pSrc, fInWs * (fInH - 1));
  Inc(pDst, fOutWs * (fOutH - 1));
  for I := 0 to fOutW - 2 do begin
    x := fSx * I;
    Iin := Floor(x);
    x := x - Iin;
    pv := pSrc + Iin;
    v[0] := pv^;
    Inc(pv);
    v[1] := pv^;
    pDst^ := Byte(Round((1 - x) * v[0] + x * v[1]));
    Inc(pDst);
  end;
end;

{$endregion}

{$region 'TBLImgSizeTrRGB24'}

procedure TBLImgSizeTrRGB24.AfterConstruction;
begin
  inherited;
  fRowInterpProc := RowBLInterp_RGB24;
end;

procedure TBLImgSizeTrRGB24.InterpolateLastColumn(pSrc, pDst: PByte);
var I, Iin: NativeInt;
    v: array [0..1] of TRGB24;
    y: Double;
    pv: PRGB24;
    res: TRGB24;
begin
  Inc(pSrc, (fInW - 1) * cPxSz);
  Inc(pDst, (fOutW - 1) * cPxSz);
  for I := 0 to fOutH - 2 do begin
    y := fSy * I;
    Iin := Floor(y);
    y := y - Iin;
    pv := PRGB24(pSrc + Iin * fInWs);
    v[0] := pv^;
    Inc(PByte(pv), fInWs);
    v[1] := pv^;
    res.R := Byte(Round((1 - y) * v[0].R + y * v[1].R));
    res.G := Byte(Round((1 - y) * v[0].G + y * v[1].G));
    res.B := Byte(Round((1 - y) * v[0].B + y * v[1].B));
    PRGB24(pDst)^ := res;
    Inc(pDst, fOutWs);
  end;
end;

procedure TBLImgSizeTrRGB24.InterpolateLastRow(pSrc, pDst: PByte);
var I, Iin: NativeInt;
    v: array [0..1] of TRGB24;
    x: Double;
    pv: PRGB24;
    res: TRGB24;
begin
  Inc(pSrc, fInWs * (fInH - 1));
  Inc(pDst, fOutWs * (fOutH - 1));
  for I := 0 to fOutW - 2 do begin
    x := fSx * I;
    Iin := Floor(x);
    x := x - Iin;
    pv := PRGB24(pSrc + Iin * cPxSz);
    v[0] := pv^;
    Inc(pv);
    v[1] := pv^;
    res.R := Byte(Round((1 - x) * v[0].R + x * v[1].R));
    res.G := Byte(Round((1 - x) * v[0].G + x * v[1].G));
    res.B := Byte(Round((1 - x) * v[0].B + x * v[1].B));
    PRGB24(pDst)^ := res;
    Inc(pDst, cPxSz);
  end;
end;

{$endregion}

end.
