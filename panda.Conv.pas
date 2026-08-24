unit panda.Conv;

interface

uses
    System.Math
  , System.SysUtils
  , panda.Nums
  , panda.Intfs
  , panda.Arrays
  , panda.cvArithmetic
{$ifdef BLAS}
  , LibCBLAS
{$endif}
  ;

{$I AsmDefs.inc}

type
  TCorr = class abstract
  protected type
    // pK - kernel
    // aKSz - kernel size
    // pIn - input array
    // pOut - output array
    // aOutSz - output array size
    TCorr1DProc = procedure (pK: PByte; aKStep, aKSz: NativeInt; pIn, pOut: PByte; aOutSz: NativeInt);
  protected
    fNDim: Integer;
    fMapLvl: Integer;
    fInSh, fOutSh: TNDAShape;
    fInitialized: Boolean;
    fCorrProc: TCorr1DProc;
    fItRes, fItArr: TNDAIt;
    fItK, fItAb: TNDAIt;
    fSItK, fSItRes: TNDASliceIt;
    fKer, fRes, fRs: INDArray;
    fAbIdx, fArIdx: INDIndexSeq;
    fKnSt, fKnSz, fRnSz: NativeInt;
    procedure InternalInit(const aK: INDArray; const aInShape: TNDAShape); virtual;
    procedure InternalEval1D(const aInput: INDArray); virtual;
    procedure InternalEvalND(const aInput: INDArray); virtual;
    procedure InternalEval(const aInput: INDArray); virtual;
    function CreateBuffer(const aShape: TNDAShape): INDArray; virtual; abstract;
    function AsContiguous(const aArr: INDArray): INDArray; virtual; abstract;
    procedure CheckInput(const aInput: INDArray);
  public
    procedure BeforeDestruction; override;
    procedure Finalize; virtual;
    class function OutShape(const aLShape, aKerShape: TNDAShape): TNDAShape;

    property Initialized: Boolean read fInitialized;
  end;

  TCorr<T> = class abstract(TCorr)
  protected
    function CreateBuffer(const aShape: TNDAShape): INDArray; override;
    function AsContiguous(const aArr: INDArray): INDArray; override;
    function GetOutput: INDArray<T>;
  public
    constructor Create(const aK: INDArray<T>; aInShape: TNDAShape); virtual;
    procedure Evaluate(const aArr: INDArray<T>);
    
    property Output: INDArray<T> read GetOutput;
  end;
  
  TCorrF32 = class(TCorr<Single>)
  public
    procedure AfterConstruction; override;
  end;

  TCorrF64 = class(TCorr<Double>)
  public
    procedure AfterConstruction; override;
  end;

function ndaCorrelate(const aKer, aArr: INDArray<Single>): INDArray<Single>; overload;
function ndaCorrelate(const aKer, aArr: INDArray<Double>): INDArray<Double>; overload;
function ndaConvolve(const aKer, aArr: INDArray<Single>): INDArray<Single>; overload;
function ndaConvolve(const aKer, aArr: INDArray<Double>): INDArray<Double>; overload;

implementation

{$EXCESSPRECISION OFF} // to prevent Single -> Double conversion by x64 compiler

uses
    panda.ArrManip
  ;

{$region 'TBlockView'}

type
  TBlockView = class(TNDArrayView)
  public
    procedure SetOrigin(aValue: PByte);
  end;

procedure TBlockView.SetOrigin(aValue: PByte);
begin
  fOffset := aValue - fArray.Data;
end;

{$endregion}

{$region 'TCorr'}

procedure TCorr.BeforeDestruction;
begin
  if fInitialized then
    Finalize;
  inherited;
end;

procedure TCorr.CheckInput(const aInput: INDArray);
var sh: TNDAShape;
    I: Integer;
begin
  sh := aInput.Shape;
  if Length(sh) <> Length(fInSh) then
    raise ENDAShapeError.Create('Invalid input dimension.');
  for I := 0 to fNDim - 1 do
    if sh[I] <> fInSh[I] then
      raise ENDAShapeError.Create('Incorrect input shape.');
end;

class function TCorr.OutShape(const aLShape, aKerShape: TNDAShape): TNDAShape;
var I, dim, lvl0: Integer;
begin
  Assert((Length(aLShape) > 0) and (Length(aKerShape) > 0));

  lvl0 := Abs(Length(aLShape) - Length(aKerShape));
  dim := Min(Length(aLShape), Length(aKerShape));
  SetLength(Result, lvl0 + dim);
  if Length(aLShape) >= Length(aKerShape) then begin
    for I := 0 to lvl0 - 1 do
      Result[I] := aLShape[I];
    for I := 0 to dim - 1 do
      Result[lvl0 + I] := aLShape[lvl0 + I] - aKerShape[I] + 1;
  end else begin
    for I := 0 to lvl0 - 1 do
      Result[I] := aKerShape[I];
    for I := 0 to dim - 1 do
      Result[lvl0 + I] := aLShape[I] - aKerShape[lvl0 + I] + 1;
  end;
end;

procedure TCorr.InternalInit(const aK: INDArray; const aInShape: TNDAShape);
var kSh, rbSh: TNDAShape;
    I, inLvl0, kLvl0: Integer;
begin
  Assert(not Initialized);

  fMapLvl := aK.NDim - Length(aInShape);
  kLvl0 := Max(0, fMapLvl);
  inLvl0 := Max(0, -fMapLvl);
  fNDim := Min(aK.NDim, Length(aInShape));
  if kLvl0 > 0 then begin
    fSItK := TNDASliceIt.Create(aK, 0, Abs(fMapLvl) - 1);
    fKer := fSItK.CurrentSlice;
  end else
    fKer := aK;
  fInSh := aInShape;
  kSh := aK.Shape;
  fOutSh := OutShape(aInShape, kSh);
  if not ValidShapeQ(fOutSh) then
    raise ENDAShapeError.Create('Too large kernel for correlation.');
  fRes := CreateBuffer(fOutSh);
  if (kLvl0 + inLvl0) > 0 then begin
    fSItRes := TNDASliceIt.Create(fRes, 0, Abs(fMapLvl) - 1);
    fRs := fSItRes.CurrentSlice;
  end else
    fRs := fRes;

  if fNDim = 1 then begin
    fKnSz := kSh[kLvl0];
    fKnSt := aK.Strides[kLvl0 + fNDim - 1];
    fRnSz := aInShape[inLvl0] - kSh[kLvl0] + 1;
    fInitialized := True;
    exit;
  end;

  SetLength(fAbIdx, fNDim - 1);
  for I := 0 to High(fAbIdx) do
    fAbIdx[I] := NDISpan(0, kSh[kLvl0 + I] - 1);

  SetLength(fArIdx, fNDim - 1);
  for I := 0 to High(fArIdx) do
    fArIdx[I] := NDISpan(0, aInShape[inLvl0 + I] - kSh[kLvl0 + I]);

  fRnSz := aInShape[inLvl0 + fNDim - 1] - kSh[kLvl0 + fNDim - 1] + 1;
  rbSh := Copy(kSh, kLvl0, fNDim);
  rbSh[fNDim - 1] := fRnSz;

  if fMapLvl <> 0 then
    fItRes := TNDAIt.Create(fSItRes.CurrentSlice, 0, fNDim - 2)
  else
    fItRes := TNDAIt.Create(fRes, 0, fNDim - 2);
  if kLvl0 > 0 then
    fItK  := TNDAIt.Create(fSItK.CurrentSlice, 0, fNDim - 2)
  else
    fItK  := TNDAIt.Create(fKer, 0, fNDim - 2);

  fKnSz := kSh[kLvl0 + fNDim - 1];
  fKnSt := aK.Strides[kLvl0 + fNDim - 1];
  fInitialized := True;
end;

procedure TCorr.Finalize;
begin
  FreeAndNil(fItRes);
  FreeAndNil(fSItRes);
  FreeAndNil(fItK);
  FreeAndNil(fSItK);
  fKer := nil;
  fRes := nil;
  fRs := nil;
  fInitialized := False;
  inherited;
end;

procedure TCorr.InternalEval1D(const aInput: INDArray);
begin
  FillChar(fRs.Data^, fRnSz * fRs.ItemSize, 0);
  fCorrProc(fKer.Data, fKnSt, fKnSz, aInput.Data, fRs.Data, fRnSz);
end;

//           _____________________________
//          /____________________________/|
//         /        /                   /|| 
//        /________/___________________/ || 
//       |        |                   |  || 
//       |    K   |         Ab        | / |
//       |________|___________________|/  /
//       |                            |  /
//       |             A              | /     
//       |____________________________|/
//
procedure TCorr.InternalEvalND(const aInput: INDArray);
var itArr, itAb: TNDAIt;
    Ar, Ab: INDArray;
    elSz: Integer;
begin
  Ab := TBlockView.Create(aInput, fAbIdx);
  Ar := TBlockView.Create(aInput, fArIdx);

  itArr := TNDAIt.Create(Ar, 0, fNDim - 2);
  itAb := TNDAIt.Create(Ab, 0, fNDim - 2);
  try
    elSz := fRes.ItemSize;
    fItRes.Reset;
    while fItRes.MoveNext and itArr.MoveNext do begin
      TBlockView(Ab).SetOrigin(itArr.Current);
      itAb.Reset;
      fItK.Reset;
      FillChar(fItRes.Current^, fRnSz * elSz, 0);
      while fItK.MoveNext and itAb.MoveNext do begin
        fCorrProc(fItK.Current, fKnSt, fKnSz, itAb.Current, fItRes.Current, fRnSz);
      end;
    end;
  finally
    itArr.Free;
    itAb.Free;
  end;
end;

procedure TCorr.InternalEval(const aInput: INDArray);
var arr: INDArray;
    itA: TNDASliceIt;
begin
  CheckInput(aInput);

  if not CheckCContLvl(aInput, Abs(fMapLvl) + fNDim - 1) then
    arr := AsContiguous(aInput)
  else
    arr := aInput;

  if fMapLvl = 0 then begin
    if fNDim = 1 then
      InternalEval1D(arr)
    else
      InternalEvalND(arr);
    exit;
  end;

  if fMapLvl < 0 then begin
    // fMapLvl < 0 -> mapping over array slices
    itA := TNDASliceIt.Create(aInput, 0, Abs(fMapLvl) - 1);
    try
      fSItRes.Reset;
      if fNDim = 1 then begin
        while itA.MoveNext and fSItRes.MoveNext do
          InternalEval1D(itA.CurrentSlice);
      end else begin
        while itA.MoveNext and fSItRes.MoveNext do
          InternalEvalND(itA.CurrentSlice);
      end;
    finally
      itA.Free;
    end;
    exit;
  end;

  if fMapLvl > 0 then begin
    // fMapLvl > 0 -> mapping over kernel slices
    fSItK.Reset;
    fSItRes.Reset;
    if fNDim = 1 then begin
      while fSItK.MoveNext and fSItRes.MoveNext do
        InternalEval1D(aInput);
    end else begin
      while fSItK.MoveNext and fSItRes.MoveNext do
        InternalEvalND(aInput);
    end;
  end;
end;

{$endregion}

{$region 'TCorr<T>'}

constructor TCorr<T>.Create(const aK: INDArray<T>; aInShape: TNDAShape);
begin
  InternalInit(aK, aInShape);
end;

procedure TCorr<T>.Evaluate(const aArr: INDArray<T>);
begin
  InternalEval(aArr);
end;

function TCorr<T>.CreateBuffer(const aShape: TNDAShape): INDArray; 
begin
  Result := TNDABuffer<T>.Create(aShape);
end;

function TCorr<T>.AsContiguous(const aArr: INDArray): INDArray;
begin
  Assert(SameQ(aArr.GetItemType, TypeInfo(T)));
  Result := TNDAUt.AsContiguousArray<T>(aArr as INDArray<T>);
end;

function TCorr<T>.GetOutput: INDArray<T>;
begin
  Result := (fRes as INDArray<T>);
end;

{$endregion}

{$region 'TCorrF32'}

procedure _corr3F32(pK: PByte; aKStep, aKSz: NativeInt; pIn, pOut: PByte; aOutSz: NativeInt);
{$if defined(ASMx64)}
// RCX <- pK, RDX <- aKStep, R8 <- aKSz, R9 <- pIn, [RBP + $30] <- pOut, [RBP + $38] <- aOutSz
asm
  movd xmm0, [rcx]
  pshufd xmm0, xmm0, 0          // xmm0 <- 4x k1
  movd xmm1, [rcx + rdx]
  pshufd xmm1, xmm1, 0          // xmm1 <- 4x k2
  movd xmm2, [rcx + 2*rdx]
  pshufd xmm2, xmm2, 0          // xmm2 <- 4x k3
  mov rcx, [rbp + $38]          // RCX <- aOutSz
  mov r10, [rbp + $30]          // R10 <- pOut
  shr rcx, 2
  jz @rest
@L:
  movups xmm3, [r9]             // xmm3 <- In[k:k+3]
  mulps xmm3, xmm0              // xmm3 <- k1*In[k:k+3]
  movups xmm4, [r9 + 4]         // xmm4 <- In[k+1:k+4]
  mulps xmm4, xmm1              // xmm4 <- k2*In[k+1:k+4]
  addps xmm3, xmm4
  movups xmm5, [r9 + 8]         // xmm5 <- In[k+2:k+5]
  mulps xmm5, xmm2              // xmm5 <- k3*In[k+2:k+5]
  addps xmm3, xmm5
  movups xmm4, [r10]            // xmm4 <- Out[k:k+3]
  addps xmm4, xmm3
  movups [r10], xmm4
  add r9, 16
  add r10, 16
  dec rcx
  jnz @L

@rest:
  mov rcx, [rbp + $38]          // R10 <- aOutSz
  and rcx, 3
  jz @end
@Lrest:
  movd xmm3, [r9]
  mulss xmm3, xmm0
  movd xmm4, [r9 + 4]
  mulss xmm4, xmm1
  addss xmm3, xmm4
  movd xmm5, [r9 + 8]
  mulss xmm5, xmm2
  addss xmm3, xmm5
  movd xmm4, [r10]
  addss xmm4, xmm3
  movd [r10], xmm4
  add r9, 4
  add r10, 4
  dec rcx
  jnz @Lrest
@end:
end;
{$else}
var pEnd: PByte;
    k1, k2, k3: Single;
begin
  k1 := PSingle(pK)^;
  k2 := PSingle(pK + aKStep)^;
  k3 := PSingle(pK + 2*aKStep)^;
  pEnd := pOut + aOutSz * cF32Sz;
  while pOut < pEnd do begin
    PSingle(pOut)^ := PSingle(pOut)^ + k1 * PSingle(pIn)^ + k2 * PSingle(pIn + cF32Sz)^ + k3 * PSingle(pIn + 2*cF32Sz)^;
    Inc(pOut, cF32Sz);
    Inc(pIn, cF32Sz);
  end;
end;
{$endif}

procedure corr1dF32(pK: PByte; aKStep, aKSz: NativeInt; pArr, pRes: PByte; aRSz: NativeInt);
var pEnd: PByte;
begin
  pEnd := pK + aKSz * aKStep;
  while pK <> pEnd do begin
  {$ifdef BLAS}
    cblas.saxpy(aRSz, PSingle(pK)^, PSingle(pArr), 1, PSingle(pRes), 1);
  {$else}
    axpy(PSingle(pK)^, PSingle(pArr), PSingle(pRes), aRSz);
  {$endif}
    Inc(pArr, cF32Sz);
    Inc(pK, aKStep);
  end;
end;

procedure TCorrF32.AfterConstruction;
begin
  inherited;

  case fKnSz of
    3: fCorrProc := _corr3F32;
  else
    fCorrProc := corr1dF32;
  end;
end;

{$endregion}

{$region 'TCorrF64'}

procedure corr1dF64(pK: PByte; aKStep, aKSz: NativeInt; pArr, pRes: PByte; aRSz: NativeInt);
var pEnd: PByte;
begin
  pEnd := pK + aKSz * aKStep;
  while pK <> pEnd do begin
  {$ifdef BLAS}
    cblas.daxpy(aRSz, PDouble(pK)^, PDouble(pArr), 1, PDouble(pRes), 1);
  {$else}
    axpy(PDouble(pK)^, PDouble(pArr), PDouble(pRes), aRSz);
  {$endif}
    Inc(pArr, cF64Sz);
    Inc(pK, aKStep);
  end;
end;

procedure TCorrF64.AfterConstruction;
begin
  inherited;
  fCorrProc := corr1df64;
end;

{$endregion}

{$region 'ndaCorrelate'}

function ndaCorrelate(const aKer, aArr: INDArray<Single>): INDArray<Single>;
var corr: TCorrF32;
begin
  corr := TCorrF32.Create(aKer, aArr.Shape);
  try
    corr.Evaluate(aArr);
    Result := corr.Output;
  finally
    corr.Free;
  end;
end;

function ndaCorrelate(const aKer, aArr: INDArray<Double>): INDArray<Double>;
var corr: TCorrF64;
begin
  corr := TCorrF64.Create(aKer, aArr.Shape);
  try
    corr.Evaluate(aArr);
    Result := corr.Output;
  finally
    corr.Free;
  end;
end;

{$endregion}

{$region 'ndaConvolve'}

function ndaConvolve(const aKer, aArr: INDArray<Single>): INDArray<Single>;
begin
  Result := ndaCorrelate(TNDAMan.FlipAll<Single>(aKer), aArr);
end;

function ndaConvolve(const aKer, aArr: INDArray<Double>): INDArray<Double>;
begin
  Result := ndaCorrelate(TNDAMan.FlipAll<Double>(aKer), aArr);
end;

{$endregion}

end.
