unit panda.Conv;

interface

uses
    System.Math
  , System.SysUtils
  , panda.Nums
  , panda.Intfs
  , panda.Arrays
  , panda.cvArithmetic
  , panda.cvMath
{$ifdef BLAS}
  , LibCBLAS
{$endif}
  ;

type
  TCorrBase = class abstract
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
    function CreateBuffer(const aShape: TNDAShape): INDArray; virtual; abstract;
    function AsContiguous(const aArr: INDArray): INDArray; virtual; abstract;
    procedure CheckInput(const aInput: INDArray);
  public
    procedure BeforeDestruction; override;
    procedure Finalize; virtual;
    class function OutShape(const aLShape, aKerShape: TNDAShape): TNDAShape;

    property Initialized: Boolean read fInitialized;
  end;

  // This class is introduced only to reduce Shape() and Strides() calling
  TBlockIt = class(TNDAIt)
  protected
    fShape: TArray<NativeInt>;
    fStrides: TArray<NativeInt>;
  public
    procedure AfterConstruction; override;
    
    property Shape: TArray<NativeInt> read fShape;
    property Strides: TArray<NativeInt> read fStrides;
  end;  

  TCorr = class abstract(TCorrBase)
  protected type
      TTotalProc = procedure (aIt: TBlockIt; pRes: PByte) of object;
  protected
    fItRes, fItArr: TNDAIt;
    fItK, fItAb, fItRb: TNDAIt;
    fSItKs, fSItK, fSItRes: TNDASliceIt;
    fItKs: TBlockIt;
    fKer, fRes, fRs, fRb, fKs: INDArray;
    fKsPtr: PByte;
    fAbIdx, fArIdx: INDIndexSeq;
    fKnSt, fKnSz, fKsSz, fRnSz: NativeInt;
    fTotalProc: TTotalProc;
    procedure InternalInit(const aK: INDArray; const aInShape: TNDAShape); virtual;
    procedure InternalEval1D(const aInput: INDArray); virtual;
    procedure InternalEvalND(const aInput: INDArray); virtual;
    procedure InternalEval(const aInput: INDArray); virtual;
    procedure Total1D(aIt: TBlockIt; pRes: PByte); virtual; abstract;
    procedure TotalND(aIt: TBlockIt; pRes: PByte); virtual; abstract;
  public
    procedure Finalize; override;
  end;

  TCorr<T> = class abstract(TCorr)
  protected
    function CreateBuffer(const aShape: TNDAShape): INDArray; override;
    function AsContiguous(const aArr: INDArray): INDArray; override;
    function GetOutput: INDArray<T>;
    procedure Copy1D(aIt: TBlockIt; pDst: PByte);
    procedure CopyND(aIt: TBlockIt; pDst: PByte);
  public
    constructor Create(const aK: INDArray<T>; aInShape: TNDAShape); virtual;
    procedure Evaluate(const aArr: INDArray<T>);
    
    property Output: INDArray<T> read GetOutput;
  end;
  
  TCorrF32 = class(TCorr<Single>)
  protected
    procedure Total1D(aIt: TBlockIt; pRes: PByte); override;
    procedure TotalND(aIt: TBlockIt; pRes: PByte); override;
  public
    procedure AfterConstruction; override;
  end;

  TCorrF64 = class(TCorr<Double>)
  protected
    procedure Total1D(aIt: TBlockIt; pRes: PByte); override;
    procedure TotalND(aIt: TBlockIt; pRes: PByte); override;
  public
    procedure AfterConstruction; override;
  end;

function ndaCorrelate(const aKer, aArr: INDArray<Single>): INDArray<Single>; overload;
function ndaCorrelate(const aKer, aArr: INDArray<Double>): INDArray<Double>; overload;
function ndaConvolve(const aKer, aArr: INDArray<Single>): INDArray<Single>; overload;
function ndaConvolve(const aKer, aArr: INDArray<Double>): INDArray<Double>; overload;

implementation

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

{$region 'TCorrBase'}

procedure TCorrBase.BeforeDestruction; 
begin
  if fInitialized then
    Finalize;
  inherited;
end;

procedure TCorrBase.Finalize; 
begin
  fInitialized := False;
end;

procedure TCorrBase.CheckInput(const aInput: INDArray);
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

class function TCorrBase.OutShape(const aLShape, aKerShape: TNDAShape): TNDAShape;
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

{$endregion}

{$region 'TBlockIt'}

procedure TBlockIt.AfterConstruction;
begin
  fShape := fArr.Shape;
  fStrides := fArr.Strides;
end;

{$endregion}

{$region 'TCorr'}

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

  case fNDim of
    1: begin
      fKnSz := kSh[kLvl0];
      fKnSt := aK.Strides[kLvl0 + fNDim - 1];
      fRnSz := aInShape[inLvl0] - kSh[kLvl0] + 1;
      fInitialized := True;
      exit;
    end;
    2: fTotalProc := Total1D;
  else
    fTotalProc := TotalND;
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
  fRb := CreateBuffer(rbSh); // Rb - buffer for sums over the last axis

  if fMapLvl <> 0 then
    fItRes := TNDAIt.Create(fSItRes.CurrentSlice, 0, fNDim - 2)
  else
    fItRes := TNDAIt.Create(fRes, 0, fNDim - 2);
  if kLvl0 > 0 then
    fItK  := TNDAIt.Create(fSItK.CurrentSlice, 0, fNDim - 2)
  else
    fItK  := TNDAIt.Create(fKer, 0, fNDim - 2);
  fItRb := TNDAIt.Create(fRb, 0, fNDim - 2);
  fSItKs := TNDASliceIt.Create(fRb, fNDim - 1, -1);
  fItKs := TBlockIt.Create(fSItKs.CurrentSlice);

  fKs := CreateBuffer(fSItKs.CurrentSlice.Shape);
  fKsPtr := fKs.Data;
  fKsSz := GetSize(fKs);
  fKnSz := kSh[kLvl0 + fNDim - 1];
  fKnSt := aK.Strides[kLvl0 + fNDim - 1];
  fInitialized := True;
end;

procedure TCorr.Finalize;
begin
  FreeAndNil(fItRes);
  FreeAndNil(fSItRes);
  FreeAndNil(fItK);
  FreeAndNil(fItRb);
  FreeAndNil(fSItK);
  FreeAndNil(fSItKs);
  FreeAndNil(fItKs);
  fKer := nil;
  fRes := nil;
  fRs := nil;
  fRb := nil;
  fKs := nil;
  inherited;
end;

procedure TCorr.InternalEval1D(const aInput: INDArray);
begin
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
    p: PByte;
begin
  Ab := TBlockView.Create(aInput, fAbIdx);
  Ar := TBlockView.Create(aInput, fArIdx);

  itArr := TNDAIt.Create(Ar, 0, fNDim - 2);
  itAb := TNDAIt.Create(Ab, 0, fNDim - 2); 
  try
    elSz := fRs.ItemSize;
    fItRes.Reset;
    while fItRes.MoveNext and itArr.MoveNext do begin
      TBlockView(Ab).SetOrigin(itArr.Current);
      itAb.Reset;
      fItRb.Reset;
      fItK.Reset;
      while fItK.MoveNext and itAb.MoveNext and fItRb.MoveNext do
        fCorrProc(fItK.Current, fKnSt, fKnSz, itAb.Current, fItRb.Current, fRnSz);
      p := fItRes.Current;
      fSItKs.Reset;
      while fSItKs.MoveNext do begin
        fTotalProc(fItKs, p);
        Inc(p, elSz);
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

procedure TCorr<T>.Copy1D(aIt: TBlockIt; pDst: PByte);
var pSrc, pEnd: PByte;
    srcStep: NativeInt;
begin
  pSrc := aIt.fArr.Data;
  pEnd := pDst + aIt.Shape[0] * SizeOf(T);
  srcStep := aIt.Strides[0];
  while pDst < pEnd do begin
    TNDA<T>.PT(pDst)^ := TNDA<T>.PT(pSrc)^;
    Inc(pSrc, srcStep);
    Inc(pDst, SizeOf(T));
  end;
end;

procedure TCorr<T>.CopyND(aIt: TBlockIt; pDst: PByte);
begin
  aIt.Reset;
  while aIt.MoveNext do begin
    TNDA<T>.PT(pDst)^ := TNDA<T>.PT(aIt.Current)^;
    Inc(pDst, SizeOf(T));
  end;
end;

{$endregion}

{$region 'TCorrF32'}

procedure corr1df32(pK: PByte; aKStep, aKSz: NativeInt; pArr, pRes: PByte; aRSz: NativeInt);
var pEnd: PByte;
begin
  FillChar(pRes^, aRSz * cF32Sz, 0);
  pEnd := pK + aKSz * aKStep;
  while pK <> pEnd do begin
  {$ifdef BLAS}
    cblas.saxpy(aRSz, PSingle(pArr), 1, PSingle(pRes), 1); 
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
  fCorrProc := corr1df32;
end;

procedure TCorrF32.Total1D(aIt: TBlockIt; pRes: PByte); 
begin
  Copy1D(aIt, fKsPtr);
  PSingle(pRes)^ := cvTotal(PSingle(fKsPtr), fKsSz);
end;

procedure TCorrF32.TotalND(aIt: TBlockIt; pRes: PByte); 
begin
  CopyND(aIt, fKsPtr);
  PSingle(pRes)^ := cvTotal(PSingle(fKsPtr), fKsSz);
end;

{$endregion}

{$region 'TCorrF64'}

procedure corr1df64(pK: PByte; aKStep, aKSz: NativeInt; pArr, pRes: PByte; aRSz: NativeInt);
var pEnd: PByte;
begin
  FillChar(pRes^, aRSz * cF64Sz, 0);
  pEnd := pK + aKSz * aKStep;
  while pK <> pEnd do begin
  {$ifdef BLAS}
    cblas.daxpy(aRSz, PDouble(pArr), 1, PDouble(pRes), 1);
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

procedure TCorrF64.Total1D(aIt: TBlockIt; pRes: PByte);
begin
  Copy1D(aIt, fKsPtr);
  PDouble(pRes)^ := cvTotal(PDouble(fKsPtr), fKsSz);
end;

procedure TCorrF64.TotalND(aIt: TBlockIt; pRes: PByte);
begin
  CopyND(aIt, fKsPtr);
  PDouble(pRes)^ := cvTotal(PDouble(fKsPtr), fKsSz);
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
