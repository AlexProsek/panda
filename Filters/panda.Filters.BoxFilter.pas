unit panda.Filters.BoxFilter;

interface

uses
    panda.Intfs
  , panda.Arrays
  , panda.Nums
  , panda.Filters.OrderStatFilters
  ;

{$I AsmDefs.inc}

type
  TBoxFilter1D = class abstract(TFilter1D)
  protected
    fNormalize: Boolean;
    fBlockSz: Integer;
    fBuff: TArray<Byte>;
    function GetElemSize: Integer; virtual; abstract;
    // Sub(A, B, dst): dst = A - B
    procedure Sub(pA, pB, pDst: PByte; aCount: NativeInt); virtual; abstract;
    procedure Accum(pSrc, pDst: PByte; aCount: NativeInt); virtual; abstract;
    procedure Normalize(pSrc, pDst: PByte; aKernelSize, aCount: NativeInt); virtual; abstract;
    procedure ExecMargin(pSrc, pDst: PByte; aStep: NativeInt); virtual; abstract;
  public
    procedure AfterConstruction; override;
    procedure Execute(aSrc, aDst: PByte; aCount: NativeInt); override;
  end;

  TBoxFilter1D<T> = class abstract(TBoxFilter1D)
  protected
    function GetElemSize: Integer; override;
  end;

  TBoxFilter1DF32 = class(TBoxFilter1D<Single>)
  protected
    procedure Sub(pA, pB, pDst: PByte; aCount: NativeInt); override;
    procedure Accum(pSrc, pDst: PByte; aCount: NativeInt); override;
    procedure Normalize(pSrc, pDst: PByte; aKernelSize, aCount: NativeInt); override;
    procedure ExecMargin(pSrc, pDst: PByte; aStep: NativeInt); override;
  end;

  TBoxFilter2D = class abstract(TFilter2D)
  protected
    fRowFilter: TBoxFilter1D;
    fBuff: TArray<Byte>;
    fW, fH: NativeInt;
    fMKerSz, fMKerIncr: TArray<Integer>;
    function GetElemSize: Integer; virtual; abstract;
    procedure Normalize(pSrc, pDst: PByte; aKernelSz: Integer); virtual; abstract;
    // pDst <- pSrc / fMKerSz
    procedure NormalizeMargins(pSrc, pDst: PByte); virtual; abstract;
    // Add(src, dst): dst += src
    procedure Add(pSrc, pDst: PByte; aCount: NativeInt); virtual; abstract;
    // Sub(src, dst): dst -= src
    procedure Sub(pSrc, pDst: PByte; aCount: NativeInt); virtual; abstract;
    procedure InitMarginKerSizes; virtual;
    procedure IncMarginKerSizes; virtual;
    procedure DecMarginKerSizes; virtual;
  public
    procedure BeforeDestruction; override;
    procedure Execute(pSrc, pDst: PByte; aSrcWStep, aDstWStep, aW, aH: NativeInt); override;
  end;

  TBoxFilter2D<T> = class abstract(TBoxFilter2D)
  protected
    function GetElemSize: Integer; override;
  end;

  TBoxFilter2DF32 = class(TBoxFilter2D<Single>)
  protected
    fMKerScales: TArray<Single>;
    procedure Normalize(pSrc, pDst: PByte; aKernelSz: Integer); override;
    procedure NormalizeMargins(pSrc, pDst: PByte); override;
    procedure Add(pSrc, pDst: PByte; aCount: NativeInt); override;
    procedure Sub(pSrc, pDst: PByte; aCount: NativeInt); override;
    procedure InitMarginKerSizes; override;
    procedure IncMarginKerSizes; override;
    procedure DecMarginKerSizes; override;
    procedure UpdateMarginKerSizes;
  public
    procedure AfterConstruction; override;
  end;

procedure Integral2D(const aSrc: INDArray<Single>; var aDst: INDArray<Single>);

{$region 'low-level functions'}

// It gives a list of length aCount - aKerSz + 1
procedure MovingSum_F32(pSrc, pDst: PByte; aKerSz: Integer; aCount: NativeInt);
procedure MovingAvg_F32(pSrc, pDst: PByte; aKerSz: Integer; aCount: NativeInt);

{$endregion}

implementation

uses
    panda.cvArithmetic
  , panda.cvMath
  ;

{$EXCESSPRECISION OFF}

{$region 'low-level functions'}

procedure MovingSum_F32(pSrc, pDst: PByte; aKerSz: Integer; aCount: NativeInt);
var pSrcOld, pEnd: PByte;
    s: Single;
begin
  Assert(aKerSz <= aCount);

  pSrcOld := pSrc;
  pEnd := pSrc + aKerSz * cF32Sz;
  s := 0;
  while pSrc < pEnd do begin
    s := s + PSingle(pSrc)^;
    Inc(pSrc, cF32Sz);
  end;

  pEnd := pSrcOld + aCount * cF32Sz;
  while pSrc < pEnd do begin
    PSingle(pDst)^ := s;
    s := s - PSingle(pSrcOld)^;
    s := s + PSingle(pSrc)^;
    Inc(pSrcOld, cF32Sz);
    Inc(pSrc, cF32Sz);
    Inc(pDst, cF32Sz);
  end;
  PSingle(pDst)^ := s;
end;

procedure MovingAvg_F32(pSrc, pDst: PByte; aKerSz: Integer; aCount: NativeInt);
var pSrcOld, pEnd: PByte;
    s, f: Single;
begin
  Assert(aKerSz <= aCount);

  pSrcOld := pSrc;
  pEnd := pSrc + aKerSz * cF32Sz;
  s := 0;
  while pSrc < pEnd do begin
    s := s + PSingle(pSrc)^;
    Inc(pSrc, cF32Sz);
  end;

  f := 1/aKerSz;
  pEnd := pSrcOld + aCount * cF32Sz;
  while pSrc < pEnd do begin
    PSingle(pDst)^ := s * f;
    s := s - PSingle(pSrcOld)^;
    s := s + PSingle(pSrc)^;
    Inc(pSrcOld, cF32Sz);
    Inc(pSrc, cF32Sz);
    Inc(pDst, cF32Sz);
  end;
  PSingle(pDst)^ := s * f;
end;

{$endregion}

{$region 'TBoxFilter1D'}

procedure TBoxFilter1D.AfterConstruction;
begin
  inherited;
  fNormalize := True;
  fBlockSz := -1;
end;

procedure TBoxFilter1D.Execute(aSrc, aDst: PByte; aCount: NativeInt);
var I, kSz, elSz, bCnt, cnt: Integer;
    pSrc, pDst: PByte;
begin
  ExecMargin(aSrc, aDst, 1);

  elSz := GetElemSize();
  kSz := 2*fRadius + 1;
  cnt := aCount - 2*fRadius;
  pDst := aDst + elSz * fRadius;

  if fBlockSz <= 0 then begin
    SetLength(fBuff, (aCount + 1) * elSz);
    Accum(aSrc, @fBuff[elSz], aCount);
    Sub(@fBuff[ksz*elSz], @fBuff[0], pDst, cnt);
  end else begin
    SetLength(fBuff, fBlockSz * elSz);
    bCnt := (aCount div fBlockSz);
    for I := 0 to bCnt - 1 do begin
      // todo
    end;
  end;

  pDst := aDst + elSz * fRadius;
  if fNormalize then
    Normalize(pDst, pDst, kSz, cnt);

  ExecMargin(aSrc + (aCount - 1) * elSz, aDst + (aCount - 1) * elSz, -1);
end;

{$endregion}

{$region 'TBoxFilter1D<T>'}

function TBoxFilter1D<T>.GetElemSize: Integer;
begin
  Result := SizeOf(T);
end;

{$endregion}

{$region 'TBoxFilter1DF32'}

procedure TBoxFilter1DF32.Sub(pA, pB, pDst: PByte; aCount: NativeInt);
begin
  VecSub(PSingle(pA), PSingle(pB), PSingle(pDst), aCount);
end;

procedure TBoxFilter1DF32.Accum(pSrc, pDst: PByte; aCount: NativeInt);
begin
  cvAccum(PSingle(pSrc), PSingle(pDst), aCount);
end;

procedure TBoxFilter1DF32.Normalize(pSrc, pDst: PByte; aKernelSize, aCount: NativeInt);
begin
  VecMul(PSingle(pSrc), 1/aKernelSize, PSingle(pDst), aCount);
end;

procedure TBoxFilter1DF32.ExecMargin(pSrc, pDst: PByte; aStep: NativeInt);
var pEnd: PByte;
    ksz: Integer;
    s: Single;
begin
  aStep := aStep * cF32Sz;
  pEnd := pSrc + fRadius * aStep;
  s := 0;
  while pSrc <> pEnd do begin
    s := s + PSingle(pSrc)^;
    Inc(pSrc, aStep);
  end;

  pEnd := pSrc + fRadius * aStep;
  if fNormalize then begin
    ksz := fRadius + 1;
    while pSrc <> pEnd do begin
      s := s + PSingle(pSrc)^;
      PSingle(pDst)^ := s / ksz;
      Inc(pSrc, aStep);
      Inc(pDst, aStep);
      Inc(ksz);
    end;
  end else begin
    while pSrc <> pEnd do begin
      s := s + PSingle(pSrc)^;
      PSingle(pDst)^ := s;
      Inc(pSrc, aStep);
      Inc(pDst, aStep);
    end;
  end;
end;

{$endregion}

{$region 'TBoxFilter2D'}

procedure TBoxFilter2D.BeforeDestruction;
begin
  fRowFilter.Free;
  inherited;
end;

procedure TBoxFilter2D.InitMarginKerSizes;
var I, J, sz: Integer;
begin
  SetLength(fMKerSz, 2*fHRadius);
  SetLength(fMKerIncr, 2*fHRadius);
  J := High(fMKerSz);
  sz := fHRadius + 1;
  for I := 0 to fHRadius - 1 do begin
    fMKerIncr[I] := sz + I;
    fMKerIncr[J] := fMKerIncr[I];
    fMKerSz[I] := fMKerIncr[I] * sz;
    fMKerSz[J] := fMKerSz[I];
    Dec(J);
  end;
end;

procedure TBoxFilter2D.IncMarginKerSizes;
var I: Integer;
begin
  for I := 0 to High(fMKerSz) do
    Inc(fMKerSz[I], fMKerIncr[I]);
end;

procedure TBoxFilter2D.DecMarginKerSizes;
var I: Integer;
begin
  for I := 0 to High(fMKerSz) do
    Dec(fMKerSz[I], fMKerIncr[I]);
end;

procedure TBoxFilter2D.Execute(pSrc, pDst: PByte; aSrcWStep, aDstWStep, aW, aH: NativeInt);
var pSrcRow, pDstRow: PByte;
    pBuffRow, pAccum: PByte;
    I, J, rx, ry, kx, ky, ksz, elSz, buffWs: NativeInt;
begin
  elSz := GetElemSize();
  rx := fHRadius;
  ry := fVRadius;
  kx := 2*rx + 1;
  ky := 2*ry + 1;
  pSrcRow := pSrc;
  pDstRow := pDst;
  fW := aW;
  fH := aH;

  fRowFilter.Radius := rx;
  fRowFilter.fNormalize := False;
  buffWs := aW * elSz;
  SetLength(fBuff, buffWs * (ky + 1));
  pBuffRow := PByte(fBuff);

  pAccum := @fBuff[ky * buffWs];
  FillChar(pAccum^, buffWs, 0);
  InitMarginKerSizes;

  for I := 0 to ry do begin
    fRowFilter.Execute(pSrcRow, pBuffRow, aW);
    Add(pBuffRow, pAccum, aW);
    Inc(pSrcRow, aSrcWStep);
    Inc(pBuffRow, buffWs);
  end;

  ksz := (ry + 1) * kx;
  for I := ry + 1 to ky - 1 do begin
    Normalize(pAccum, pDstRow, ksz);
    NormalizeMargins(pAccum, pDstRow);
    IncMarginKerSizes;
    fRowFilter.Execute(pSrcRow, pBuffRow, aW);
    Add(pBuffRow, pAccum, aW);
    Inc(pSrcRow, aSrcWStep);
    Inc(pDstRow, aDstWStep);
    Inc(pBuffRow, buffWs);
    Inc(ksz, kx);
  end;

  J := 0;
  for I := 0 to aH - ky - 1 do begin
    Normalize(pAccum, pDstRow, ksz);
    NormalizeMargins(pAccum, pDstRow);
    pBuffRow := PByte(fBuff) + J * buffWs;
    J := (J + 1) mod ky;
    Sub(pBuffRow, pAccum, aW);
    fRowFilter.Execute(pSrcRow, pBuffRow, aW);
    Add(pBuffRow, pAccum, aW);
    Inc(pSrcRow, aSrcWStep);
    Inc(pDstRow, aDstWStep);
  end;
  Normalize(pAccum, pDstRow, ksz);
  NormalizeMargins(pAccum, pDstRow);
  Inc(pDstRow, aDstWStep);

  for I := 0 to ry - 1 do begin
    pBuffRow := PByte(fBuff) + J * buffWs;
    J := (J + 1) mod ky;
    Sub(pBuffRow, pAccum, aW);
    Dec(ksz, kx);
    DecMarginKerSizes;
    Normalize(pAccum, pDstRow, ksz);
    NormalizeMargins(pAccum, pDstRow);
    Inc(pDstRow, aDstWStep);
  end;
end;

{$endregion}

{$region 'TBoxFilter2D<T>'}

function TBoxFilter2D<T>.GetElemSize: Integer;
begin
  Result := SizeOf(T);
end;

{$endregion}

{$region 'TBoxFilter2DF32'}

procedure TBoxFilter2DF32.AfterConstruction;
begin
  inherited;
  fRowFilter := TBoxFilter1DF32.Create;
end;

procedure TBoxFilter2DF32.Normalize(pSrc, pDst: PByte; aKernelSz: Integer);
begin
  VecMul(PSingle(pSrc + fHRadius * cF32Sz), 1/aKernelSz, PSingle(pDst + fHRadius * cF32Sz), fW - 2 * fHRadius);
end;

procedure TBoxFilter2DF32.NormalizeMargins(pSrc, pDst: PByte);
var offset: NativeInt;
begin
  VecMul(PSingle(pSrc), PSingle(fMKerScales), PSingle(pDst), HRadius);
  offset := (fW - fHRadius) * cF32Sz;
  VecMul(PSingle(pSrc + offset), PSingle(@fMKerScales[HRadius]), PSingle(pDst + offset), HRadius);
end;

procedure TBoxFilter2DF32.Add(pSrc, pDst: PByte; aCount: NativeInt);
begin
  VecAdd(PSingle(pSrc), PSingle(pDst), PSingle(pDst), aCount);
end;

procedure TBoxFilter2DF32.Sub(pSrc, pDst: PByte; aCount: NativeInt);
begin
  VecSub(PSingle(pDst), PSingle(pSrc), PSingle(pDst), aCount);
end;

procedure TBoxFilter2DF32.InitMarginKerSizes;
begin
  inherited;
  SetLength(fMKerScales, 2*fHRadius);
  UpdateMarginKerSizes;
end;

procedure TBoxFilter2DF32.IncMarginKerSizes;
begin
  inherited;
  UpdateMarginKerSizes;
end;

procedure TBoxFilter2DF32.DecMarginKerSizes;
begin
  inherited;
  UpdateMarginKerSizes;
end;

procedure TBoxFilter2DF32.UpdateMarginKerSizes;
var I: Integer;
begin
  for I := 0 to High(fMKerScales) do
    fMKerScales[I] := 1 / fMKerSz[I];
end;

{$endregion}

{$region 'Integral'}

procedure _Integral2D(aSrc, aDst: INDArray; aAccumFunc, aAddRowFunc: TIPProcVV);
var I, w, h, srcWs, dstWs, srcS, dstS: NativeInt;
    pSrc, pDst0, pDst1: PByte;
    s: TArray<NativeInt>;
begin
  s := aSrc.Shape;
  w := s[1];
  h := s[0];
  s := aSrc.Strides;
  srcWs := s[0];
  srcS  := s[1];
  s := aDst.Strides;
  dstWs := s[0];
  dstS  := s[1];
  pSrc := aSrc.Data;
  pDst0 := aDst.Data;

  aAccumFunc(w, pSrc, srcS, pDst0, dstS);
  Inc(pSrc, srcWs);
  pDst1 := pDst0 + dstWs;
  for I := 1 to h - 1 do begin
    aAccumFunc(w, pSrc, srcS, pDst1, dstS);
    aAddRowFunc(w, pDst0, srcS, pDst1, dstS);
    pDst0 := pDst1;
    Inc(pDst1, dstWs);
    Inc(pSrc, srcWs);
  end;
end;

procedure accum_f32(N: NativeInt; X: PByte; IncX: NativeInt; Y: PByte; IncY: NativeInt);
begin
  if (IncX = cF32Sz) and (IncY = cF32Sz) then
    cvAccum(PSingle(X), PSingle(Y), N);
end;

procedure add_f32(N: NativeInt; X: PByte; IncX: NativeInt; Y: PByte; IncY: NativeInt);
begin
  if (IncX = cF32Sz) and (IncY = cF32Sz) then
    VecAdd(PSingle(X), PSingle(Y), PSingle(Y), N);
end;

procedure Integral2D(const aSrc: INDArray<Single>; var aDst: INDArray<Single>);
begin
  Assert(Assigned(aSrc) and (aSrc.NDim > 1));

  if not Assigned(aDst) then
    aDst := TNDAUt.Empty<Single>(aSrc.Shape)
  else if not SameShapeQ(aSrc, aDst) then
    raise ENDAShapeError.Create('Integral2D: Output array has to have the same shape as an input.');

  if aSrc.NDim = 2 then begin
    _Integral2D(aSrc, aDst, accum_f32, add_f32);
    exit;
  end;

  // todo
end;

{$endregion}

end.
