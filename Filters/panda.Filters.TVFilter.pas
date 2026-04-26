unit panda.Filters.TVFilter;

interface

uses
    panda.Intfs
  , panda.Arrays
  , panda.cvArithmetic
  , System.SysUtils
  , System.Math
  ;

{$I AsmDefs.inc}

type
  TTVFilter = class abstract
  public type
    TStepMonitor = function (const aObjFuncValue: Double): Boolean of object;
  protected
    fNIter: Integer;
    fLambda: Double;
    fInitialized: Boolean;
    fStepMonitor: TStepMonitor;
  {$region 'Getters/Setters'}
    procedure SetNIter(aValue: Integer);
    procedure SetLambda(const aValue: Double);
  {$endregion}
  public
    procedure AfterConstruction; override;
    procedure Assign(aSrc: TTVFilter); virtual;

    property Lambda: Double read fLambda write SetLambda;
    property IterationCount: Integer read fNIter write SetNIter;
    property Initialized: Boolean read fInitialized;
    property StepMonitor: TStepMonitor read fStepMonitor write fStepMonitor;
  end;

  TTVFilter1D = class abstract(TTVFilter)
  protected
    fCount: NativeInt;
    function CheckInArg(const aArr: INDArray): Boolean; virtual;
    function CheckOutArg(const aArr: INDArray): Boolean; virtual;
  public
    procedure Init(aCount: NativeInt); virtual;
  end;

  TTVFilter1D<T> = class(TTVFilter1D)
  protected
    fZ, fW, fDX: TArray<T>;
    fBuffs: array [0..1] of INDArray<T>;
    function InitBuffer(I: Integer; const aArr: INDArray<T>): PByte;
  public
    procedure Init(aCount: NativeInt); override;
  end;

  TTVFilter1DF32 = class(TTVFilter1D<Single>)
  public
    function Execute(const aSrc, aDst: INDArray<Single>): Boolean;
  end;

  TTVFilter2D = class abstract(TTVFilter)
  protected
    fW, fH: Integer;
  public
    procedure Init(aW, aH: Integer); virtual;
  end;

  TTVFilter2D<T> = class abstract(TTVFilter2D)
  protected
    fNim, fP, fUxy, fDiv, fUnew, fU: TArray<T>;
    fL2, fTau, fSigma, fTheta: T;
  public
    procedure Init(aW, aH: Integer); override;
  end;

  TTVFilter2DF32 = class(TTVFilter2D<Single>)
  protected
    function CheckInArg(const aArr: INDArray<Single>): Boolean;
    function CheckOutArg(const aArr: INDArray<Single>): Boolean;
    // Y[i] <- Y[i] + (X[i+1] - X[i])
    class procedure AddDiffTo(pX, pY: PSingle; aYCount: NativeInt); static;
    // u -> [u(:, [2:w, w]) - u , u([2:h, h], :) - u]
    procedure EvalGrad(const aData: TArray<Single>; var aGrad: TArray<Single>);
    procedure EvalDiv(const aA, aDiv: TArray<Single>);
    class procedure Normalize(pX, pY: PSingle; aCount: NativeInt); static;
    class procedure ProjNorm(aSigma: Single; pUxy, pX, pY: PSingle; aCount: NativeInt); static;
    class procedure EvalNewU(pU, pDiv, pNim, pUNew: PSingle; aCount: NativeInt;
      aTau, aLt: Single); static;
    class procedure ExtraGradStep(aTheta: Single; pUnew, pU: PSingle;
      aCount: NativeInt); static;
    function EvalObjFunc: Double;
    procedure InitInData(pData: PSingle);
  public
    procedure AfterConstruction; override;
    function Execute(const aSrc, aDst: INDArray<Single>): Boolean;
  end;


  procedure TotalVariationFilter(const aSrc: INDArray<Single>; var aDst: INDArray<Single>;
    aLambda: Single = 1.0; aIterCount: Integer = 100);

implementation

{$EXCESSPRECISION OFF} // to prevent Single -> Double conversion by x64 compiler

procedure TotalVariationFilter(const aSrc: INDArray<Single>; var aDst: INDArray<Single>;
  aLambda: Single; aIterCount: Integer);
var f1d: TTVFilter1DF32;
    f2d: TTVFilter2DF32;
begin
  case aSrc.NDim of
    1: begin
      f1d := TTVFilter1DF32.Create;
      try
        f1d.Lambda := aLambda;
        f1d.IterationCount := aIterCount;
        f1d.Init(aSrc.Shape[1]);
        f1d.Execute(aSrc, aDst);
      finally
        f1d.Free;
      end;
    end;

    2: begin
      f2d := TTVFilter2DF32.Create;
      try
        f2d.Lambda := aLambda;
        f2d.IterationCount := aIterCount;
        f2d.Init(aSrc.Shape[1], aSrc.Shape[1]);
        f2d.Execute(aSrc, aDst);
      finally
        f2d.Free;
      end;
    end;
  else
    raise ENotImplemented.Create('TotalVariationFilter is implemented only for dimension 1 or 2.');
  end;
end;

{$region 'TTVFilter'}

procedure TTVFilter.AfterConstruction;
begin
  inherited;
  fInitialized := False;
  fLambda := 1;
  fNIter := 100;
  fStepMonitor := nil;
end;

procedure TTVFilter.Assign(aSrc: TTVFilter);
begin
  fNIter := aSrc.IterationCount;
  fLambda := aSrc.Lambda;
  fStepMonitor := aSrc.StepMonitor;
end;

{$region 'Getters/Setters'}

procedure TTVFilter.SetNIter(aValue: Integer);
begin
  if aValue > 0 then fNIter := aValue;
end;

procedure TTVFilter.SetLambda(const aValue: Double);
begin
  if aValue > 0 then fLambda := aValue;
end;

{$endregion}

{$endregion}

{$region 'TTVFilter1D'}

function TTVFilter1D.CheckInArg(const aArr: INDArray): Boolean;
begin
  Result := (aArr.NDim = 1) and (aArr.Shape[0] = fCount);
end;

function TTVFilter1D.CheckOutArg(const aArr: INDArray): Boolean;
begin
  Result := (aArr.NDim = 1) and (aArr.Shape[0] = fCount);
end;

procedure TTVFilter1D.Init(aCount: NativeInt);
begin
  Assert(aCount > 0);
  fCount := aCount;
end;

{$endregion}

{$region 'TTVFilter1D<T>'}

procedure TTVFilter1D<T>.Init(aCount: NativeInt);
begin
  inherited;
  if fCount <> Length(fW) then begin
    fBuffs[0] := TNDABuffer<T>.Create([fCount]);
    fBuffs[1] := TNDABuffer<T>.Create([fCount]);
    SetLength(fZ, fCount - 1);
    SetLength(fDX, fCount - 1);
    SetLength(fW, fCount);
  end;

  fInitialized := True;
end;

function TTVFilter1D<T>.InitBuffer(I: Integer; const aArr: INDArray<T>): PByte;
begin
  if CContiguousQ(aArr) then exit(aArr.Data);

  TNDAUt.Fill<T>(fBuffs[I], aArr);
  Result := fBuffs[I].Data;
end;

{$endregion}

{$region 'TTVFilter1DF32'}

function TTVFilter1DF32.Execute(const aSrc, aDst: INDArray<Single>): Boolean;
var alpha, t, J: Single;
    pSrc, pDst: PSingle;
    Nit: Integer;
begin
  if not (Initialized and CheckInArg(aSrc) and CheckInArg(aDst)) then
    exit(False);

  pSrc := PSingle(InitBuffer(0, aSrc));
  pDst := PSingle(InitBuffer(1, aDst));
  Nit := fNIter;
  alpha := 4;
  t := fLambda / 2;
  while Nit > 0 do begin
    fW[0] := fZ[0];
    Differences(PSingle(fZ), @fW[1], fCount - 1);
    fW[fCount - 1] := -fZ[fCount - 2];
    VecAdd(pSrc, PSingle(fW), pDst, fCount);
    Differences(pDst, PSingle(fDX), fCount);
    if Assigned(StepMonitor) then begin
      J := SquaredEuclideanDistance(pSrc, pDst, fCount) + Norm(fDX, 1);
      if not StepMonitor(J) then break;
    end;
    sscal(PSingle(fDX), fCount - 1, 1/alpha);
    VecAdd(PSingle(fZ), PSingle(fDX), PSingle(fZ), fCount - 1);
    VecClip(PSingle(fZ), fCount - 1, -t, t);
    Dec(Nit);
  end;

  if not CContiguousQ(aDst) then
    TNDAUt.Fill<Single>(aDst, fBuffs[1]);
  Result := True;
end;

{$endregion}

{$region 'TTVFilter2D'}

procedure TTVFilter2D.Init(aW, aH: Integer);
begin
  Assert((aW > 0) and (aH > 0));
  fW := aW;
  fH := aH;
end;

{$endregion}

{$region 'TTVFilter2D<T>'}

procedure TTVFilter2D<T>.Init(aW, aH: Integer);
var count: NativeInt;
begin
  inherited;
  count := aW * aH;
  if count <> Length(fNim) then begin
    SetLength(fNim, count);
    SetLength(fU, count);
    SetLength(fUnew, count);
    SetLength(fP, 2 * count);
    SetLength(fUxy, 2 * count);
    SetLength(fDiv, count);
  end;

  fInitialized := True;
end;

{$endregion}

{$region 'TTVFilter2DF32'}

procedure TTVFilter2DF32.AfterConstruction;
begin
  inherited;
  fL2 := 8.0;
  fTau := 0.02;
  fSigma := 1.0 / (fL2 * fTau);
  fTheta := 1.0;
end;

procedure TTVFilter2DF32.InitInData(pData: PSingle);
begin
  // temp
  Move(pData^, fNim[0], fW * fH * SizeOf(Single));
end;

function TTVFilter2DF32.CheckInArg(const aArr: INDArray<Single>): Boolean;
begin
  Result := (aArr.NDim = 2) and (aArr.Shape[0] = fH) and (aArr.Shape[1] = fW);

  if Result then
    InitInData(PSingle(aArr.Data));
end;

function TTVFilter2DF32.CheckOutArg(const aArr: INDArray<Single>): Boolean;
begin
  Result := (aArr.NDim = 2) and (aArr.Shape[0] = fH) and (aArr.Shape[1] = fW);
end;

procedure TTVFilter2DF32.EvalGrad(const aData: TArray<Single>; var aGrad: TArray<Single>);
var pX, pRes, px1, px2: PSingle;
    I: Integer;
begin
  // ux=u(:, [2:width, width]) - u;
  pX := PSingle(aData);
  pRes := PSingle(aGrad);
  px1 := pRes;
  Inc(px1, fW - 1);
  for I := 0 to fH - 1 do begin
    Differences(pX, pRes, fW);
    px1^ := 0;
    Inc(pX, fW);
    Inc(pRes, fW);
    Inc(px1, fW);
  end;

  // uy=u([2:height, height], :) - u;
  pX := PSingle(aData);
  pRes := PSingle(@aGrad[fW * fH]);
  px1 := pX;
  px2 := px1;
  Inc(px1, fW);
  for I := 0 to fH - 2 do begin
    VecSub(px1, px2, pRes, fW);
    Inc(pRes, fW);
    px2 := px1;
    Inc(px1, fW);
  end;
  FillChar(pRes^, fW * SizeOf(Single), 0);
end;

class procedure TTVFilter2DF32.AddDiffTo(pX, pY: PSingle; aYCount: NativeInt);
{$if defined(ASMx86)}
// EAX <- pX, EDX <- pY, ECX <- aYCount
asm
  push esi
  push edi
  mov esi, eax
  mov edi, edx
  mov eax, ecx
  shr ecx, 3
  jz @rest
@L:
{$ifdef AVX}
  vmovups ymm0, [esi]
  vmovups ymm1, [esi + 4]
  vmovups ymm2, [edi]
  vsubps ymm1, ymm1, ymm0
  vaddps ymm2, ymm2, ymm1
  vmovups [edi], ymm2
  add esi, 32
  add edi, 32
{$else}
  movups xmm0, [esi]
  movups xmm1, [esi + 4]
  movups xmm2, [edi]
  subps xmm1, xmm0
  addps xmm2, xmm1
  movups [edi], xmm2
  add esi, 16
  add edi, 16

  movups xmm0, [esi]
  movups xmm1, [esi + 4]
  movups xmm2, [edi]
  subps xmm1, xmm0
  addps xmm2, xmm1
  movups [edi], xmm2
  add esi, 16
  add edi, 16
{$endif}
  dec ecx
  jnz @L

{$ifdef AVX}
  vzeroupper
{$endif}
@rest:
  and eax, 7
  jz @end
@Lrest:
  movss xmm0, [esi]
  movss xmm1, [esi + 4]
  movss xmm2, [edi]
  subss xmm1, xmm0
  addss xmm2, xmm1
  movss [edi], xmm2
  add esi, 4
  add edi, 4
  dec eax
  jnz @Lrest

@end:
  pop edi
  pop esi
end;
{$elseif defined(ASMx64)}
// RCX <- pX, RDX <- pY, R8 <- aYCount
asm
  mov r9, r8
  shr r8, 3
  jz @rest
@L:
{$ifdef AVX}
  vmovups ymm0, [rcx]
  vmovups ymm1, [rcx + 4]
  vmovups ymm2, [rdx]
  vsubps ymm1, ymm1, ymm0
  vaddps ymm2, ymm2, ymm1
  vmovups [rdx], ymm2
  add rcx, 32
  add rdx, 32
{$else}
  movups xmm0, [rcx]
  movups xmm1, [rcx + 4]
  movups xmm2, [rdx]
  subps xmm1, xmm0
  addps xmm2, xmm1
  movups [rdx], xmm2
  add rcx, 16
  add rdx, 16

  movups xmm0, [rcx]
  movups xmm1, [rcx + 4]
  movups xmm2, [rdx]
  subps xmm1, xmm0
  addps xmm2, xmm1
  movups [rdx], xmm2
  add rcx, 16
  add rdx, 16
{$endif}
  dec r8
  jnz @L

{$ifdef AVX}
  vzeroupper
{$endif}
@rest:
  and r9, 7
  jz @end
@Lrest:
  movss xmm0, [rcx]
  movss xmm1, [rcx + 4]
  movss xmm2, [rdx]
  subss xmm1, xmm0
  addss xmm2, xmm1
  movss [rdx], xmm2
  add rcx, 4
  add rdx, 4
  dec r9
  jnz @Lrest

@end:
end;
{$else}
var pEnd: PByte;
    s0, s1: Single;
begin
  pEnd := PByte(pY) + aYCount * SizeOf(Single);
  s0 := pX^;
  Inc(pX);
  while PByte(pY) < pEnd do begin
     s1 := pX^;
     pY^ := pY^ + (s1 - s0);
     s0 := s1;
     Inc(pX);
     Inc(pY);
  end;
end;
{$endif}

procedure TTVFilter2DF32.EvalDiv(const aA, aDiv: TArray<Single>);
var p, p1, p2, pRes: PSingle;
    I, count: NativeInt;
begin
  // div = [p([1:h-1,:,2); zeros(1,w)] - [zeros(1,w); p([1:h-1,:,2)]
  p := PSingle(@aA[fW * fH]);
  p1 := p;
  pRes := PSingle(aDiv);
  Move(p1^, pRes^, fW * SizeOf(Single));
  p2 := p1;
  Inc(p1, fW);
  Inc(pRes, fW);
  for I := 1 to fH - 2 do begin
    VecSub(p1, p2, pRes, fW);
    Inc(pRes, fW);
    p2 := p1;
    Inc(p1, fW);
  end;
  VecNeg(p2, pRes, fW);

  // div = div + [p(:,[1:w-1],1) zeros(h,1)] - [zeros(h,1) p(:,[1:w-1],1)]
  p := PSingle(aA);
  pRes := PSingle(aDiv);
  count := fW * fH;
  VecAddInPlace(pRes, p, fW, fW, count);
  Inc(pRes);
  for I := 0 to fH - 1 do begin
    AddDiffTo(p, pRes, fW - 2);
    Inc(pRes, fW);
    Inc(p, fW);
  end;
  p := PSingle(aA);
  pRes := PSingle(aDiv);
  Inc(p, fW - 2);
  Inc(pRes, fW - 1);
  VecSubInPlace(pRes, p, fW, fW, count);
end;

class procedure TTVFilter2DF32.Normalize(pX, pY: PSingle; aCount: NativeInt);
{$if defined(ASMx86)}
const sOne: Single = 1.0;
asm
  // EAX <- pX, EDX <- pY, ECX <- aCount
  push ebx
  mov ebx, offset sOne
  movss xmm4, [ebx] // xmm4 <- (0, 0, 0, 1)
  shufps xmm4, xmm4, 0 // xmm4 <- (1, 1, 1, 1)
  mov ebx, ecx
  shr ecx, 2
  jz @rest

@L:
  movups xmm0, [eax]
  movups xmm1, [edx]
  movaps xmm2, xmm0
  movaps xmm3, xmm1
  mulps xmm2, xmm0
  mulps xmm3, xmm1
  addps xmm2, xmm3
  sqrtps xmm3, xmm2
  maxps xmm3, xmm4
  divps xmm0, xmm3
  divps xmm1, xmm3
  movups [eax], xmm0
  movups [edx], xmm1
  add eax, 16
  add edx, 16
  dec ecx
  jnz @L

@rest:
  mov ecx, ebx
  and ecx, 3
  jz @end

@Lrest:
  movss xmm0, [eax]
  movss xmm1, [edx]
  movss xmm2, xmm0
  movss xmm3, xmm1
  mulss xmm2, xmm0
  mulss xmm3, xmm1
  addss xmm2, xmm3
  sqrtss xmm3, xmm2
  maxss xmm3, xmm4
  divss xmm0, xmm3
  divss xmm1, xmm3
  movss [eax], xmm0
  movss [edx], xmm1
  add eax, 4
  add edx, 4
  dec ecx
  jnz @Lrest

@end:
  pop ebx
end;
{$elseif defined(ASMx64)}
const sOne: Single = 1.0;
asm
  // RCX <- pX, RDX <- pY, R8 <- aCount
  mov rax, offset sOne
  movss xmm4, [rax] // xmm4 <- (0, 0, 0, 1)
  shufps xmm4, xmm4, 0 // xmm4 <- (1, 1, 1, 1)
  mov rax, rcx // RAX <- pX
  mov rcx, r8  // RCX <- aCount
  shr rcx, 3
  jz @rest

@L:
{$ifdef AVX}
  vmovups ymm0, [rax]
  vmovups ymm1, [rdx]
  vmovaps ymm2, ymm0
  vmovaps ymm3, ymm1
  vmulps ymm2, ymm2, ymm0
  vmulps ymm3, ymm3, ymm1
  vaddps ymm2, ymm2, ymm3
  vsqrtps ymm3, ymm2
  vmaxps ymm3, ymm3, ymm4
  vdivps ymm0, ymm0, ymm3
  vdivps ymm1, ymm1, ymm3
  vmovups [rax], ymm0
  vmovups [rdx], ymm1
  add rax, 32
  add rdx, 32
{$else}
  movups xmm0, [rax]
  movups xmm1, [rdx]
  movaps xmm2, xmm0
  movaps xmm3, xmm1
  mulps xmm2, xmm0
  mulps xmm3, xmm1
  addps xmm2, xmm3
  sqrtps xmm3, xmm2
  maxps xmm3, xmm4
  divps xmm0, xmm3
  divps xmm1, xmm3
  movups [rax], xmm0
  movups [rdx], xmm1
  add rax, 16
  add rdx, 16

  movups xmm0, [rax]
  movups xmm1, [rdx]
  movaps xmm2, xmm0
  movaps xmm3, xmm1
  mulps xmm2, xmm0
  mulps xmm3, xmm1
  addps xmm2, xmm3
  sqrtps xmm3, xmm2
  maxps xmm3, xmm4
  divps xmm0, xmm3
  divps xmm1, xmm3
  movups [rax], xmm0
  movups [rdx], xmm1
  add rax, 16
  add rdx, 16
{$endif}
  dec rcx
  jnz @L

{$ifdef AVX}
   vzeroupper
{$endif}
@rest:
  mov rcx, r8
  and rcx, 7
  jz @end

@Lrest:
  movss xmm0, [rax]
  movss xmm1, [rdx]
  movss xmm2, xmm0
  movss xmm3, xmm1
  mulss xmm2, xmm0
  mulss xmm3, xmm1
  addss xmm2, xmm3
  sqrtss xmm3, xmm2
  maxss xmm3, xmm4
  divss xmm0, xmm3
  divss xmm1, xmm3
  movss [rax], xmm0
  movss [rdx], xmm1
  add rax, 4
  add rdx, 4
  dec rcx
  jnz @Lrest

@end:
end;
{$else}
var pEnd: PByte;
    n: Single;
begin
  pEnd := PByte(pX) + aCount * SizeOf(Single);
  while PByte(pX) < pEnd do begin
    n := max(1, Sqrt(pX^ * pX^ + pY^ * pY^));
    pX^ := pX^ / n;
    pY^ := pY^ / n;
    Inc(pX);
    Inc(pY);
  end;
end;
{$endif}

class procedure TTVFilter2DF32.ProjNorm(aSigma: Single; pUxy, pX, pY: PSingle; aCount: NativeInt);
const sOne: Single = 1.0;
{$if defined(ASMx86)}
// EAX <- pUxy, EDX <- pX, ECX <- pY, [EBP + 8] <- aCount, [EBP + 12] <- aSigma
asm
  push esi
  push edi
  xorps xmm5, xmm5      // xmm5 <- (0, 0, 0, 0)
  mov esi, offset sOne
  movss xmm5, [esi]     // xmm5 <- (0, 0, 0, 1)
{$ifdef AVX}
  vbroadcastss ymm5, xmm5
{$else}
  shufps xmm5, xmm5, 0  // xmm5 <- (1, 1, 1, 1)
{$endif}
  movss xmm0, [ebp + 12]// xmm0 <- sigma
  mov edi, eax
  mov eax, edx          // eax <- pX
  mov edx, ecx          // edx <- pY
  mov ecx, [ebp + 8]    // ecx <- aCount
  mov esi, edi          // rsi <- pUx
  lea edi, edi + 4*ecx  // rdi <- pUy
  shr ecx, 3
  jz @rest
{$ifdef AVX}
  vbroadcastss ymm0, xmm0 // ymm0 <- 8x sigma
{$else}
  shufps xmm0, xmm0, 0  // xmm0 <- 4x sigma
{$endif}
@L:
{$ifdef AVX}
  vmovups ymm1, [eax]     // ymm1 <- pX^
  vmovups ymm2, [edx]     // ymm2 <- pY^
  vmovups ymm3, [esi]     // ymm3 <- pUx^
  vmovups ymm4, [edi]     // ymm4 <- pUy^
  vmulps ymm3, ymm3, ymm0 // ymm3 <- sigma * ux
  vmulps ymm4, ymm4, ymm0 // ymm4 <- sigma * uy
  vaddps ymm3, ymm3, ymm1 // ymm3 <- x := x + sigma * ux
  vaddps ymm4, ymm4, ymm2 // ymm4 <- y := y + sigma * uy
  vmovups ymm1, ymm3
  vmovups ymm2, ymm4
  vmulps ymm1, ymm1, ymm1 // ymm1 <- x^2
  vmulps ymm2, ymm2, ymm2 // ymm2 <- y^2
  vaddps ymm1, ymm1, ymm2 // ymm1 <- x^2 + y^2
  vsqrtps ymm1, ymm1      // ymm1 <- sqrt(x^2 + y^2)
  vmaxps ymm1, ymm1, ymm5 // ymm1 <- n := Max(1, sqrt(x^2 + y^2))
  vdivps ymm3, ymm3, ymm1 // ymm3 <- x/n
  vdivps ymm4, ymm4, ymm1 // ymm4 <- y/n
  vmovups [eax], ymm3
  vmovups [edx], ymm4
  add eax, 32
  add edx, 32
  add esi, 32
  add edi, 32
{$else}
  movups xmm1, [eax]     // xmm1 <- pX^
  movups xmm2, [edx]     // xmm2 <- pY^
  movups xmm3, [esi]    // xmm3 <- pUx^
  movups xmm4, [edi]    // xmm4 <- pUy^
  mulps xmm3, xmm0      // xmm3 <- sigma * ux
  mulps xmm4, xmm0      // xmm4 <- sigma * uy
  addps xmm3, xmm1      // xmm3 <- x := x + sigma * ux
  addps xmm4, xmm2      // xmm4 <- y := y + sigma * uy
  movups xmm1, xmm3
  movups xmm2, xmm4
  mulps xmm1, xmm1      // xmm1 <- x^2
  mulps xmm2, xmm2      // xmm2 <- y^2
  addps xmm1, xmm2      // xmm1 <- x^2 + y^2
  sqrtps xmm1, xmm1     // xmm1 <- sqrt(x^2 + y^2)
  maxps xmm1, xmm5      // xmm1 <- n := Max(1, sqrt(x^2 + y^2))
  divps xmm3, xmm1      // xmm3 <- x/n
  divps xmm4, xmm1      // xmm4 <- y/n
  movups [eax], xmm3
  movups [edx], xmm4
  add eax, 16
  add edx, 16
  add esi, 16
  add edi, 16

  movups xmm1, [eax]     // xmm1 <- pX^
  movups xmm2, [edx]     // xmm2 <- pY^
  movups xmm3, [esi]    // xmm3 <- pUx^
  movups xmm4, [edi]    // xmm4 <- pUy^
  mulps xmm3, xmm0      // xmm3 <- sigma * ux
  mulps xmm4, xmm0      // xmm4 <- sigma * uy
  addps xmm3, xmm1      // xmm3 <- x := x + sigma * ux
  addps xmm4, xmm2      // xmm4 <- y := y + sigma * uy
  movups xmm1, xmm3
  movups xmm2, xmm4
  mulps xmm1, xmm1      // xmm1 <- x^2
  mulps xmm2, xmm2      // xmm2 <- y^2
  addps xmm1, xmm2      // xmm1 <- x^2 + y^2
  sqrtps xmm1, xmm1     // xmm1 <- sqrt(x^2 + y^2)
  maxps xmm1, xmm5      // xmm1 <- n := Max(1, sqrt(x^2 + y^2))
  divps xmm3, xmm1      // xmm3 <- x/n
  divps xmm4, xmm1      // xmm4 <- y/n
  movups [eax], xmm3
  movups [edx], xmm4
  add eax, 16
  add edx, 16
  add esi, 16
  add edi, 16
{$endif}
  dec ecx
  jnz @L

@rest:
{$ifdef AVX}
  vzeroupper
{$endif}
  mov ecx, [ebp + 8]
  and ecx, 7
  jz @end
@Lrest:
  movss xmm1, [eax]     // xmm1 <- pX^
  movss xmm2, [edx]     // xmm2 <- pY^
  movss xmm3, [esi]    // xmm3 <- pUx^
  movss xmm4, [edi]    // xmm4 <- pUy^
  mulss xmm3, xmm0      // xmm3 <- sigma * ux
  mulss xmm4, xmm0      // xmm4 <- sigma * uy
  addss xmm3, xmm1      // xmm3 <- x := x + sigma * ux
  addss xmm4, xmm2      // xmm4 <- y := y + sigma * uy
  movss xmm1, xmm3
  movss xmm2, xmm4
  mulss xmm1, xmm1      // xmm1 <- x^2
  mulss xmm2, xmm2      // xmm2 <- y^2
  addss xmm1, xmm2      // xmm1 <- x^2 + y^2
  sqrtss xmm1, xmm1     // xmm1 <- sqrt(x^2 + y^2)
  maxss xmm1, xmm5      // xmm1 <- n := Max(1, sqrt(x^2 + y^2))
  divss xmm3, xmm1      // xmm3 <- x/n
  divss xmm4, xmm1      // xmm4 <- y/n
  movss [eax], xmm3
  movss [edx], xmm4
  add eax, 4
  add edx, 4
  add esi, 4
  add edi, 4
  dec ecx
  jnz @Lrest

@end:
  pop edi
  pop esi
end;
{$elseif defined(ASMx64)}
// XMM0 <- aSigma, RDX <- pUxy, R8 <- pX, R9 <- pY, [RBP + $30] <- aCount
asm
  xorps xmm5, xmm5      // xmm5 <- (0, 0, 0, 0)
  mov rax, offset sOne
  movss xmm5, [rax]     // xmm5 <- (0, 0, 0, 1)
{$ifdef AVX}
  vbroadcastss ymm5, xmm5
{$else}
  shufps xmm5, xmm5, 0  // xmm5 <- (1, 1, 1, 1)
{$endif}
  mov r10, [rbp + $30]
  mov r11, r10          // r11 <- aCount
  mov rcx, rdx          // rcx <- pUx
  lea rdx, rdx + 4*r10  // rdx <- pUy
  shr r10, 3
  jz @rest
{$ifdef AVX}
  vbroadcastss ymm0, xmm0 // ymm0 <- 8x sigma
{$else}
  shufps xmm0, xmm0, 0  // xmm0 <- 4x sigma
{$endif}
@L:
{$ifdef AVX}
  vmovups ymm1, [r8]      // ymm1 <- pX^
  vmovups ymm2, [r9]      // ymm2 <- pY^
  vmovups ymm3, [rcx]     // ymm3 <- pUx^
  vmovups ymm4, [rdx]     // ymm4 <- pUy^
  vmulps ymm3, ymm3, ymm0 // ymm3 <- sigma * ux
  vmulps ymm4, ymm4, ymm0 // ymm4 <- sigma * uy
  vaddps ymm3, ymm3, ymm1 // ymm3 <- x := x + sigma * ux
  vaddps ymm4, ymm4, ymm2 // ymm4 <- y := y + sigma * uy
  vmovups ymm1, ymm3
  vmovups ymm2, ymm4
  vmulps ymm1, ymm1, ymm1 // ymm1 <- x^2
  vmulps ymm2, ymm2, ymm2 // ymm2 <- y^2
  vaddps ymm1, ymm1, ymm2 // ymm1 <- x^2 + y^2
  vsqrtps ymm1, ymm1      // ymm1 <- sqrt(x^2 + y^2)
  vmaxps ymm1, ymm1, ymm5 // ymm1 <- n := Max(1, sqrt(x^2 + y^2))
  vdivps ymm3, ymm3, ymm1 // ymm3 <- x/n
  vdivps ymm4, ymm4, ymm1 // ymm4 <- y/n
  vmovups [r8], ymm3
  vmovups [r9], ymm4
  add rcx, 32
  add rdx, 32
  add r8, 32
  add r9, 32
{$else}
  movups xmm1, [r8]     // xmm1 <- pX^
  movups xmm2, [r9]     // xmm2 <- pY^
  movups xmm3, [rcx]    // xmm3 <- pUx^
  movups xmm4, [rdx]    // xmm4 <- pUy^
  mulps xmm3, xmm0      // xmm3 <- sigma * ux
  mulps xmm4, xmm0      // xmm4 <- sigma * uy
  addps xmm3, xmm1      // xmm3 <- x := x + sigma * ux
  addps xmm4, xmm2      // xmm4 <- y := y + sigma * uy
  movups xmm1, xmm3
  movups xmm2, xmm4
  mulps xmm1, xmm1      // xmm1 <- x^2
  mulps xmm2, xmm2      // xmm2 <- y^2
  addps xmm1, xmm2      // xmm1 <- x^2 + y^2
  sqrtps xmm1, xmm1     // xmm1 <- sqrt(x^2 + y^2)
  maxps xmm1, xmm5      // xmm1 <- n := Max(1, sqrt(x^2 + y^2))
  divps xmm3, xmm1      // xmm3 <- x/n
  divps xmm4, xmm1      // xmm4 <- y/n
  movups [r8], xmm3
  movups [r9], xmm4
  add rcx, 16
  add rdx, 16
  add r8, 16
  add r9, 16

  movups xmm1, [r8]     // xmm1 <- pX^
  movups xmm2, [r9]     // xmm2 <- pY^
  movups xmm3, [rcx]    // xmm3 <- pUx^
  movups xmm4, [rdx]    // xmm4 <- pUy^
  mulps xmm3, xmm0      // xmm3 <- sigma * ux
  mulps xmm4, xmm0      // xmm4 <- sigma * uy
  addps xmm3, xmm1      // xmm3 <- x := x + sigma * ux
  addps xmm4, xmm2      // xmm4 <- y := y + sigma * uy
  movups xmm1, xmm3
  movups xmm2, xmm4
  mulps xmm1, xmm1      // xmm1 <- x^2
  mulps xmm2, xmm2      // xmm2 <- y^2
  addps xmm1, xmm2      // xmm1 <- x^2 + y^2
  sqrtps xmm1, xmm1     // xmm1 <- sqrt(x^2 + y^2)
  maxps xmm1, xmm5      // xmm1 <- n := Max(1, sqrt(x^2 + y^2))
  divps xmm3, xmm1      // xmm3 <- x/n
  divps xmm4, xmm1      // xmm4 <- y/n
  movups [r8], xmm3
  movups [r9], xmm4
  add rcx, 16
  add rdx, 16
  add r8, 16
  add r9, 16
{$endif}
  dec r10
  jnz @L

@rest:
{$ifdef AVX}
  vzeroupper
{$endif}
  and r11, 7
  jz @end
@Lrest:
  movss xmm1, [r8]     // xmm1 <- pX^
  movss xmm2, [r9]     // xmm2 <- pY^
  movss xmm3, [rcx]    // xmm3 <- pUx^
  movss xmm4, [rdx]    // xmm4 <- pUy^
  mulss xmm3, xmm0      // xmm3 <- sigma * ux
  mulss xmm4, xmm0      // xmm4 <- sigma * uy
  addss xmm3, xmm1      // xmm3 <- x := x + sigma * ux
  addss xmm4, xmm2      // xmm4 <- y := y + sigma * uy
  movss xmm1, xmm3
  movss xmm2, xmm4
  mulss xmm1, xmm1      // xmm1 <- x^2
  mulss xmm2, xmm2      // xmm2 <- y^2
  addss xmm1, xmm2      // xmm1 <- x^2 + y^2
  sqrtss xmm1, xmm1     // xmm1 <- sqrt(x^2 + y^2)
  maxss xmm1, xmm5      // xmm1 <- n := Max(1, sqrt(x^2 + y^2))
  divss xmm3, xmm1      // xmm3 <- x/n
  divss xmm4, xmm1      // xmm4 <- y/n
  movss [r8], xmm3
  movss [r9], xmm4
  add rcx, 4
  add rdx, 4
  add r8, 4
  add r9, 4
  dec r11
  jnz @Lrest

@end:
end;
{$else}
var pEnd: PByte;
    n, x, y: Single;
    pUx, pUy: PSingle;
begin
  pUx := pUxy;
  pUy := pUxy;
  Inc(pUy, aCount);
  pEnd := PByte(pX) + aCount * SizeOf(Single);
  while PByte(pX) < pEnd do begin
    x := pX^;
    y := pY^;
    x := x + aSigma * pUx^;
    y := y + aSigma * pUy^;
    n := max(1, Sqrt(x * x + y * y));
    pX^ := x / n;
    pY^ := y / n;
    Inc(pUx);
    Inc(pUy);
    Inc(pX);
    Inc(pY);
  end;
end;
{$endif}

class procedure TTVFilter2DF32.EvalNewU(pU, pDiv, pNim, pUNew: PSingle; aCount: NativeInt;
  aTau, aLt: Single);
// v = u + tau*div
// unew=(v-lt).*(v-nim>lt) + (v+lt).*(v-nim<-lt) + nim.*(abs(v-nim)<=lt)
{$if defined(ASMx86)}
// EAX <- pU, EDX <- pDiv, ECX <- pNim, [EBP + 20] <- pUNew, [EBP + 16] <- aCount,
// [EBP + 12] <- aTau, [EBP + 8] <- aLt
asm
  push esi
  push edi
  mov esi, ecx         // ESI <- pNim
  mov edi, [ebp + 20]  // EDI <- pUNew
  movss xmm0, [ebp + 8] // xmm0[:31] <- Lt
  movss xmm5, [ebp + 12] // xmm1[:31] <- Tau
{$ifdef AVX}
  vbroadcastss ymm0, xmm0
  vbroadcastss ymm5, xmm5
{$else}
  shufps xmm0, xmm0, 0 // xmm0 <- (Lt, Lt, Lt, Lt)
  shufps xmm5, xmm5, 0 // xmm5 <- (Tau, Tau, Tau, Tau)
{$endif}
  mov ecx, [ebp + 16] // ECX <- aCount
  shr ecx, 3
  jz @rest

@L:
{$ifdef AVX}
  vmovups ymm1, [eax]     // xmm1 <- U
  vmovups ymm2, [edx]     // xmm2 <- Div
  vmulps ymm2, ymm2, ymm5 // xmm2 <- Tau * Div
  vaddps ymm1, ymm1, ymm2 // xmm1 <- V := U + Tau * Div
  vmovaps ymm2, ymm1      // ymm2 <- V
  vmovups ymm3, [esi]     // ymm3 <- Nim
  vaddps ymm2, ymm2, ymm0 // ymm2 <- v + Lt
  vminps ymm3, ymm3, ymm2 // ymm3 <- Min(Nim, v + Lt)
  vsubps ymm1, ymm1, ymm0 // ymm1 <- v - Lt
  vmaxps ymm1, ymm1, ymm3 // xmm1 <- Max(v - Lt, Min(Nim, v + Lt)
  vmovups [edi], ymm1
  add eax, 32
  add edx, 32
  add esi, 32
  add edi, 32
{$else}
  movups xmm1, [eax] // xmm1 <- U
  movups xmm2, [edx] // xmm2 <- Div
  mulps xmm2, xmm5   // xmm2 <- Tau * Div
  addps xmm1, xmm2   // xmm1 <- V := U + Tau * Div
  movaps xmm2, xmm1  // xmm2 <- V
  movups xmm3, [esi] // xmm3 <- Nim
  addps  xmm2, xmm0  // xmm2 <- v + Lt
  minps xmm3, xmm2   // xmm3 <- Min(Nim, v + Lt)
  subps xmm1, xmm0   // xmm1 <- v - Lt
  maxps xmm1, xmm3   // xmm1 <- Max(v - Lt, Min(Nim, v + Lt)
  movups [edi], xmm1
  add eax, 16
  add edx, 16
  add esi, 16
  add edi, 16

  movups xmm1, [eax] // xmm1 <- U
  movups xmm2, [edx] // xmm2 <- Div
  mulps xmm2, xmm5   // xmm2 <- Tau * Div
  addps xmm1, xmm2   // xmm1 <- V := U + Tau * Div
  movaps xmm2, xmm1  // xmm2 <- V
  movups xmm3, [esi] // xmm3 <- Nim
  addps  xmm2, xmm0  // xmm2 <- v + Lt
  minps xmm3, xmm2   // xmm3 <- Min(Nim, v + Lt)
  subps xmm1, xmm0   // xmm1 <- v - Lt
  maxps xmm1, xmm3   // xmm1 <- Max(v - Lt, Min(Nim, v + Lt)
  movups [edi], xmm1
  add eax, 16
  add edx, 16
  add esi, 16
  add edi, 16
{$endif}
  dec ecx
  jnz @L

@rest:
{$ifdef AVX}
  vzeroupper
{$endif}
  mov ecx, [ebp + 16]
  and ecx, 7
  jz @end

@Lrest:
  movss xmm1, [eax]
  movss xmm2, [edx]
  mulss xmm2, xmm5
  addss xmm1, xmm2   // xmm1 <- V := U + Tau * Div
  movss xmm2, xmm1
  movss xmm3, [esi]
  addss xmm2, xmm0
  minss xmm3, xmm2
  subss xmm1, xmm0
  maxss xmm1, xmm3
  movss [edi], xmm1
  add eax, 4
  add edx, 4
  add esi, 4
  add edi, 4
  dec ecx
  jnz @Lrest

@end:
  pop edi
  pop esi
end;
{$elseif defined(ASMx64)}
// RCX <- pU, RDX <- pDiv, R8 <- pNim, R9 <- pUNew, [RBP + $30] <- aCount,
// [RBP + $38] <- aTau, [RBP + $40] <- aLt
asm
  push rsi
  push rdi
  mov rax, rcx // RAX <- pU
  mov rsi, r8  // ESI <- pNim
  mov rdi, r9  // EDI <- pUNew
  movss xmm0, [rbp + $40] // xmm0[:31] <- Lt
  movss xmm5, [rbp + $38] // xmm1[:31] <- Tau
{$ifdef AVX}
  vbroadcastss ymm0, xmm0
  vbroadcastss ymm5, xmm5
{$else}
  shufps xmm0, xmm0, 0 // xmm0 <- (Lt, Lt, Lt, Lt)
  shufps xmm5, xmm5, 0 // xmm5 <- (Tau, Tau, Tau, Tau)
{$endif}
  mov rcx, [rbp + $30] // ECX <- aCount
  shr rcx, 3
  jz @rest

@L:
{$ifdef AVX}
  vmovups ymm1, [rax]     // xmm1 <- U
  vmovups ymm2, [rdx]     // xmm2 <- Div
  vmulps ymm2, ymm2, ymm5 // xmm2 <- Tau * Div
  vaddps ymm1, ymm1, ymm2 // xmm1 <- V := U + Tau * Div
  vmovaps ymm2, ymm1      // ymm2 <- V
  vmovups ymm3, [rsi]     // ymm3 <- Nim
  vaddps ymm2, ymm2, ymm0 // ymm2 <- v + Lt
  vminps ymm3, ymm3, ymm2 // ymm3 <- Min(Nim, v + Lt)
  vsubps ymm1, ymm1, ymm0 // ymm1 <- v - Lt
  vmaxps ymm1, ymm1, ymm3 // xmm1 <- Max(v - Lt, Min(Nim, v + Lt)
  vmovups [rdi], ymm1
  add rax, 32
  add rdx, 32
  add rsi, 32
  add rdi, 32
{$else}
  movups xmm1, [rax] // xmm1 <- U
  movups xmm2, [rdx] // xmm2 <- Div
  mulps xmm2, xmm5   // xmm2 <- Tau * Div
  addps xmm1, xmm2   // xmm1 <- V := U + Tau * Div
  movaps xmm2, xmm1  // xmm2 <- V
  movups xmm3, [rsi] // xmm3 <- Nim
  addps  xmm2, xmm0  // xmm2 <- v + Lt
  minps xmm3, xmm2   // xmm3 <- Min(Nim, v + Lt)
  subps xmm1, xmm0   // xmm1 <- v - Lt
  maxps xmm1, xmm3   // xmm1 <- Max(v - Lt, Min(Nim, v + Lt)
  movups [rdi], xmm1
  add rax, 16
  add rdx, 16
  add rsi, 16
  add rdi, 16

  movups xmm1, [rax] // xmm1 <- U
  movups xmm2, [rdx] // xmm2 <- Div
  mulps xmm2, xmm5   // xmm2 <- Tau * Div
  addps xmm1, xmm2   // xmm1 <- V := U + Tau * Div
  movaps xmm2, xmm1  // xmm2 <- V
  movups xmm3, [rsi] // xmm3 <- Nim
  addps  xmm2, xmm0  // xmm2 <- v + Lt
  minps xmm3, xmm2   // xmm3 <- Min(Nim, v + Lt)
  subps xmm1, xmm0   // xmm1 <- v - Lt
  maxps xmm1, xmm3   // xmm1 <- Max(v - Lt, Min(Nim, v + Lt)
  movups [rdi], xmm1
  add rax, 16
  add rdx, 16
  add rsi, 16
  add rdi, 16
{$endif}
  dec rcx
  jnz @L

@rest:
{$ifdef AVX}
  vzeroupper
{$endif}
  mov rcx, [rbp + $30]
  and rcx, 7
  jz @end

@Lrest:
  movss xmm1, [rax]
  movss xmm2, [rdx]
  mulss xmm2, xmm5
  addss xmm1, xmm2   // xmm1 <- V := U + Tau * Div
  movss xmm2, xmm1
  movss xmm3, [rsi]
  addss xmm2, xmm0
  minss xmm3, xmm2
  subss xmm1, xmm0
  maxss xmm1, xmm3
  movss [rdi], xmm1
  add rax, 4
  add rdx, 4
  add rsi, 4
  add rdi, 4
  dec rcx
  jnz @Lrest

@end:
  pop rdi
  pop rsi
end;
{$else}
var v, im, d: Single;
    pEnd: PByte;
begin
  pEnd := PByte(pU) + aCount * SizeOf(Single);
  while PByte(pU) < pEnd do begin
    v := pU^ + aTau * pDiv^;
    im := pNim^;
    d := v - im;
    if d > aLt then
      pUnew^ := v - aLt
    else
    if d < -aLt then
      pUnew^ := v + aLt
    else
      pUnew^ := im;
    Inc(pNim);
    Inc(pU);
    Inc(pDiv);
    Inc(pUnew);
  end;
end;
{$endif}

class procedure TTVFilter2DF32.ExtraGradStep(aTheta: Single; pUnew, pU: PSingle;
  aCount: NativeInt);
// u <- unew + theta * (unew - u)
{$if defined(ASMx86)}
// EAX <- pUnew, EDX <- pU, ECX <- aCount, [EBP + 8] <- aTheta
asm
  push ebx
  movss xmm0, [ebp + 8]
  mov ebx, ecx
  shr ecx, 3
  jz @rest
{$ifdef AVX}
  vbroadcastss ymm0, xmm0 // xmm0 <- 8x theta
{$else}
  shufps xmm0, xmm0, 0  // xmm0 <- 4x theta
{$endif}
@L:
{$ifdef AVX}
  vmovups ymm1, [eax] // ymm1 <- Unew
  vmovups ymm2, [edx]  // ymm2 <- U
  vsubps ymm3, ymm1, ymm2   // ymm3 <- Unew - U
  vmulps ymm3, ymm3, ymm0   // ymm3 <- theta * (Unew - U)
  vaddps ymm1, ymm1, ymm3   // ymm1 <- Unew + theta * (Unew - U)
  vmovups [edx], ymm1
  add eax, 32
  add edx, 32
{$else}
  movups xmm1, [eax] // xmm1 <- Unew
  movups xmm2, [edx] // xmm2 <- U
  movaps xmm3, xmm1  // xmm3 <- Unew
  subps xmm3, xmm2   // xmm3 <- Unew - U
  mulps xmm3, xmm0   // xmm3 <- theta * (Unew - U)
  addps xmm1, xmm3   // xmm1 <- Unew + theta * (Unew - U)
  movups [edx], xmm1
  add eax, 16
  add edx, 16

  movups xmm1, [eax] // xmm1 <- Unew
  movups xmm2, [edx]  // xmm2 <- U
  movaps xmm3, xmm1  // xmm3 <- Unew
  subps xmm3, xmm2   // xmm3 <- Unew - U
  mulps xmm3, xmm0   // xmm3 <- theta * (Unew - U)
  addps xmm1, xmm3   // xmm1 <- Unew + theta * (Unew - U)
  movups [edx], xmm1
  add eax, 16
  add edx, 16
{$endif}
  dec ecx
  jnz @L

{$ifdef AVX}
  vzeroupper
{$endif}
@rest:
  and ebx, 7
  jz @end
@Lrest:
  movd xmm1, [eax]   // xmm1 <- Unew
  movd xmm2, [edx]   // xmm2 <- U
  movss xmm3, xmm1   // xmm3 <- Unew
  subss xmm3, xmm2   // xmm3 <- Unew - U
  mulss xmm3, xmm0   // xmm3 <- theta * (Unew - U)
  addss xmm1, xmm3   // xmm1 <- Unew + theta * (Unew - U)
  movd [edx], xmm1
  add eax, 4
  add edx, 4
  dec ebx
  jnz @Lrest

@end:
  pop ebx
end;
{$elseif defined(ASMx64)}
 // XMM0 <- aTheta, RDX <- pUNew, R8 <- pU, R9 <- aCount
 asm
  mov r10, r9
  shr r9, 3
  jz @rest
{$ifdef AVX}
  vbroadcastss ymm0, xmm0 // xmm0 <- 8x theta
{$else}
  shufps xmm0, xmm0, 0  // xmm0 <- 4x theta
{$endif}
@L:
{$ifdef AVX}
  vmovups ymm1, [rdx] // ymm1 <- Unew
  vmovups ymm2, [r8]  // ymm2 <- U
{$ifdef AVX2}
  vmovups     ymm1, [rdx]       // ymm1 = Unew
  vmovups     ymm2, [r8]        // ymm2 = U
  vsubps      ymm2, ymm1, ymm2  // ymm2 = Unew - U
  vfmadd231ps ymm1, ymm2, ymm0  // ymm1 += ymm2 * theta ( = Unew + theta*(Unew - U))
  vmovups     [r8], ymm1
{$else}
  vsubps ymm3, ymm1, ymm2   // ymm3 <- Unew - U
  vmulps ymm3, ymm3, ymm0   // ymm3 <- theta * (Unew - U)
  vaddps ymm1, ymm1, ymm3   // ymm1 <- Unew + theta * (Unew - U)
  vmovups [r8], ymm1
{$endif}
  add rdx, 32
  add r8, 32
{$else}
  movups xmm1, [rdx] // xmm1 <- Unew
  movups xmm2, [r8]  // xmm2 <- U
  movaps xmm3, xmm1  // xmm3 <- Unew
  subps xmm3, xmm2   // xmm3 <- Unew - U
  mulps xmm3, xmm0   // xmm3 <- theta * (Unew - U)
  addps xmm1, xmm3   // xmm1 <- Unew + theta * (Unew - U)
  movups [r8], xmm1
  add rdx, 16
  add r8, 16

  movups xmm1, [rdx] // xmm1 <- Unew
  movups xmm2, [r8]  // xmm2 <- U
  movaps xmm3, xmm1  // xmm3 <- Unew
  subps xmm3, xmm2   // xmm3 <- Unew - U
  mulps xmm3, xmm0   // xmm3 <- theta * (Unew - U)
  addps xmm1, xmm3   // xmm1 <- Unew + theta * (Unew - U)
  movups [r8], xmm1
  add rdx, 16
  add r8, 16
{$endif}
  dec r9
  jnz @L

{$ifdef AVX}
  vzeroupper
{$endif}
@rest:
  and r10, 7
  jz @end
@Lrest:
  movd xmm1, [rdx]   // xmm1 <- Unew
  movd xmm2, [r8]    // xmm2 <- U
  movss xmm3, xmm1   // xmm3 <- Unew
  subss xmm3, xmm2   // xmm3 <- Unew - U
  mulss xmm3, xmm0   // xmm3 <- theta * (Unew - U)
  addss xmm1, xmm3   // xmm1 <- Unew + theta * (Unew - U)
  movd [r8], xmm1
  add rdx, 4
  add r8, 4
  dec r10
  jnz @Lrest
@end:
 end;
{$else}
var pEnd: PByte;
begin
  pEnd := PByte(pU) + aCount * SizeOf(Single);
  while PByte(pU) < pEnd do begin
     pU^ := pUNew^ + aTheta * (pUNew^ - pU^);
     Inc(pUNew);
     Inc(pU);
  end;
end;
{$endif}

function TTVFilter2DF32.EvalObjFunc: Double;
// ux=u(:, [2:width, width]) - u;
// uy=u([2:height, height], :) - u;
// E=sum(sqrt(ux(:).^2 + uy(:).^2)) + lambda*sum(abs(u(:) - nim(:)))
var pUx, pUy, pU, pNim: PSingle;
    pEnd: PByte;
    s1, s2: Single;
begin
  pUx := PSingle(fUxy);
  pUy := PSingle(@fUxy[fW * fH]);
  pU := PSingle(fU);
  pNim := PSingle(fNim);
  pEnd := PByte(pUx) + fW * fH * SizeOf(Single);
  s1 := 0;
  s2 := 0;
  while PByte(pUx) < pEnd do begin
    s2 := s2 + Sqrt(Sqr(pUx^) + Sqr(pUy^));
    s1 := s1 + Abs(pU^ - pNim^);
    Inc(pUx);
    Inc(pUy);
    Inc(pU);
    Inc(pNim);
  end;
  Result := s2 + fLambda * s1;
end;

function TTVFilter2DF32.Execute(const aSrc, aDst: INDArray<Single>): Boolean;
var I: Integer;
begin
  if not (Initialized and CheckInArg(aSrc) and CheckOutArg(aDst)) then
    exit(False);

  Move(fNim[0], fU[0], Length(fNim) * SizeOf(Single));
  EvalGrad(fU, fP);
  for I := 0 to fNIter - 1 do begin
    EvalGrad(fU, fUxy);
    if Assigned(StepMonitor) then if not StepMonitor(EvalObjFunc()) then break;

    // p=p + sigma*cat(3, ux, uy);
    // % project
    // normep=max(1, sqrt(p(:, :, 1).^2 + p(:, :, 2).^2));
    // p(:, :, 1)=p(:, :, 1)./normep;
    // p(:, :, 2)=p(:, :, 2)./normep;
    ProjNorm(fSigma, PSingle(fUxy), PSingle(@fP[0]), PSingle(@fP[fW * fH]), fW * fH);

    // % shrinkage
    // % compute divergence in div
    EvalDiv(fP, fDiv);

    // % TV-L1 model
    // v = u + tau*div
    // unew=(v-lt).*(v-nim>lt) + (v+lt).*(v-nim<-lt) + nim.*(abs(v-nim)<=lt)
    EvalNewU(PSingle(fU), PSingle(fDiv), PSingle(fNim), PSingle(fUNew), Length(fUNew), fTau, fLambda * fTau);

    // % extragradient step
    // u=unew + theta*(unew-u);
    ExtraGradStep(fTheta, PSingle(fUNew), PSingle(fU), Length(fU));
  end;
  TNDAUt.Fill<Single>(aDst, TDynArrWrapper<Single>.Create(fU, [fH, fW]));
  Result := True;
end;

{$endregion}

end.
