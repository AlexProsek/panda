unit panda.Filters.GuidedFilter;

interface


uses
    panda.Intfs
  , panda.Arrays
  , panda.Filters.BoxFilter
  , panda.Filters.OrderStatFilters
  ;

type
  TGuidedFilter2D = class abstract(TFilter2D)
  protected
    fBoxFilter: TBoxFilter2D;
    fMeanI, fMeanP, fMeanA, fMeanB: INDArray;
    fCorrI, fCorrIp, fVarI, fCovIp: INDArray;
    fI, fP, fTmp, fA, fB: INDArray;
    fH, fW, fWs: NativeInt;
    fEps: Double;
    function CreateBuffer(aW, aH: NativeInt): INDArray; virtual; abstract;
    function WrapBuffer(pSrc: PByte; aW, aH, aWStep: NativeInt): INDArray; virtual; abstract;
    // aRes <- aCorrXY - aMeanX * aMeanY
    procedure Cov(const aCorrXY, aMeanX, aMeanY: INDArray; var aRes: INDArray); virtual; abstract;
    // aRes <- A * B
    procedure Mul(const A, B: INDArray; var aRes: INDArray); virtual; abstract;
    // aRes <- A * X + B
    procedure FMA(const A, X, B: INDArray; var aRes: INDArray); virtual; abstract;
    // (A, B) <- (convIp/(varI + eps), meanP - a*meanI)
    procedure EvalModel(var A, B: INDArray); virtual; abstract;
  {$region 'Getters/Setters'}
    procedure SetEps(const aValue: Double);
  {$endregion}
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure InitBuffers(aW, aH: NativeInt);
    procedure Execute(pSrc, pDst: PByte; aSrcWStep, aDstWStep, aW, aH: NativeInt); override;

    property Epsilon: Double read fEps write SetEps;
  end;

  TGuidedFilter2D<T> = class(TGuidedFilter2D)
  protected
    function CreateBuffer(aW, aH: NativeInt): INDArray; override;
    function WrapBuffer(pSrc: PByte; aW, aH, aWStep: NativeInt): INDArray; override;
  public
    procedure Init(const aGuide: INDArray<T>); virtual;
  end;

  TGuidedFilter2DF32 = class(TGuidedFilter2D<Single>)
  protected
    procedure Cov(const aCorrXY, aMeanX, aMeanY: INDArray; var aRes: INDArray); override;
    procedure Mul(const A, B: INDArray; var aRes: INDArray); override;
    procedure FMA(const A, X, B: INDArray; var aRes: INDArray); override;
    procedure EvalModel(var A, B: INDArray); override;
  public
    procedure AfterConstruction; override;
  end;


implementation

uses
    panda.cvArithmetic
  ;

{$region 'TGuidedFilter2D'}

procedure TGuidedFilter2D.AfterConstruction;
begin
  inherited;
  fEps := 1e-2;
  fH := -1;
  fW := -1;
end;

procedure TGuidedFilter2D.BeforeDestruction;
begin
  fBoxFilter.Free;
  inherited;
end;

procedure TGuidedFilter2D.InitBuffers(aW, aH: NativeInt);
begin
  Assert((aW > 0) and (aH > 0));

  if (fW = aW) and (fH = aH) then exit;

  fMeanI := CreateBuffer(aW, aH);
  fMeanP := CreateBuffer(aW, aH);
  fMeanA := CreateBuffer(aW, aH);
  fMeanB := CreateBuffer(aW, aH);
  fCorrI := CreateBuffer(aW, aH);
  fCorrIp := CreateBuffer(aW, aH);
  fVarI := CreateBuffer(aW, aH);
  fCovIp := CreateBuffer(aW, aH);
  fA := CreateBuffer(aW, aH);
  fB := CreateBuffer(aW, aH);
  fTmp := CreateBuffer(aW, aH);

  fWs := fTmp.Strides[0];
  fW := aW;
  fH := aH;
end;

procedure TGuidedFilter2D.Execute(pSrc, pDst: PByte; aSrcWStep, aDstWStep, aW, aH: NativeInt);
var res: INDArray;
begin
  InitBuffers(aW, aH);

  fP := WrapBuffer(pSrc, fW, fH, fWs);
  res := WrapBuffer(pDst, fW, fH, aDstWStep);
  fBoxFilter.HRadius := fHRadius;
  fBoxFilter.VRadius := fVRadius;

  fBoxFilter.Execute(fI.Data, fMeanI.Data, fWs, fWs, aW, aH);
  fBoxFilter.Execute(fP.Data, fMeanP.Data, fWs, fWs, aW, aH);
  Mul(fI, fI, fTmp);
  fBoxFilter.Execute(fTmp.Data, fCorrI.Data, fWs, fWs, aW, aH);
  Mul(fI, fP, fTmp);
  fBoxFilter.Execute(fTmp.Data, fCorrIp.Data, fWs, fWs, aW, aH);

  Cov(fCorrI, fMeanI, fMeanI, fVarI);
  Cov(fCorrIp, fMeanI, fMeanP, fCovIp);

  EvalModel(fA, fB);

  fBoxFilter.Execute(fA.Data, fMeanA.Data, fWs, fWs, aW, aH);
  fBoxFilter.Execute(fB.Data, fMeanB.Data, fWs, fWs, aW, aH);

  FMA(fMeanA, fI, fMeanB, res);
end;

{$region 'Getters/Setters'}

procedure TGuidedFilter2D.SetEps(const aValue: Double);
begin
  if aValue > 0 then
    fEps := aValue;
end;

{$endregion}

{$endregion}

{$region 'TGuidedFilter2D<T>'}

function TGuidedFilter2D<T>.CreateBuffer(aW, aH: NativeInt): INDArray;
begin
  Result := TNDABuffer<T>.Create([aH, aW]);
end;

function TGuidedFilter2D<T>.WrapBuffer(pSrc: PByte; aW, aH, aWStep: NativeInt): INDArray;
begin
  Assert(aWStep >= aW * SizeOf(T));
  Result := TNDArray<T>.Create(pSrc, [aH, aW], [aWStep, SizeOf(T)]);
end;

procedure TGuidedFilter2D<T>.Init(const aGuide: INDArray<T>);
begin
  fI := TNDAUt.AsContiguousArray<T>(aGuide);
end;

{$endregion}

{$region 'TGuidedFilter2DF32'}

procedure TGuidedFilter2DF32.AfterConstruction;
begin
  inherited;
  fBoxFilter := TBoxFilter2DF32.Create;
end;

procedure TGuidedFilter2DF32.Cov(const aCorrXY, aMeanX, aMeanY: INDArray; var aRes: INDArray);
var pCorr, pMX, pMY, pRes: PByte;
    I: NativeInt;
begin
  pCorr := aCorrXY.Data;
  pMX := aMeanX.Data;
  pMY := aMeanY.Data;
  pRes := aRes.Data;

  for I := 0 to fH - 1 do begin
    VecMul(PSingle(pMX), PSingle(pMY), PSingle(pRes), fW);
    VecSub(PSingle(pCorr), PSingle(pRes), pSingle(pRes), fW);
    Inc(pCorr, fWs);
    Inc(pMX, fWs);
    Inc(pMY, fWs);
    Inc(pRes, fWs);
  end;
end;

procedure TGuidedFilter2DF32.Mul(const A, B: INDArray; var aRes: INDArray);
var pA, pB, pRes: PByte;
    I: NativeInt;
begin
  pA := A.Data;
  pB := B.Data;
  pRes := aRes.Data;

  for I := 0 to fH - 1 do begin
    VecMul(PSingle(pA), PSingle(pB), PSingle(pRes), fW);
    Inc(pA, fWs);
    Inc(pB, fWs);
    Inc(pRes, fWs);
  end;
end;

procedure TGuidedFilter2DF32.FMA(const A, X, B: INDArray; var aRes: INDArray);
var pA, pX, pB, pRes: PByte;
    I: NativeInt;
begin
  pA := A.Data;
  pX := X.Data;
  pB := B.Data;
  pRes := aRes.Data;

  for I := 0 to fH - 1 do begin
    VecMul(PSingle(pA), PSingle(pX), PSingle(pRes), fW);
    VecAdd(PSingle(pRes), PSingle(pB), PSingle(pRes), fW);
    Inc(pA, fWs);
    Inc(pX, fWs);
    Inc(pB, fWs);
    Inc(pRes, fWs);
  end;
end;

procedure TGuidedFilter2DF32.EvalModel(var A, B: INDArray);
var pA, pB, pVarI, pCovIp, pMeanI, pMeanP: PByte;
    I: NativeInt;
begin
  pA := A.Data;
  pB := B.Data;
  pVarI := fVarI.Data;
  pCovIp := fCovIp.Data;
  pMeanI := fMeanI.Data;
  pMeanP := fMeanP.Data;

  for I := 0 to fH - 1 do begin
    VecAdd(PSingle(pVarI), Single(fEps), PSingle(pA), fW);
    VecDiv(PSingle(pCovIp), PSingle(pA), PSingle(pA), fW);
    VecMul(PSingle(pA), PSingle(pMeanI), PSingle(pB), fW);
    VecSub(PSingle(pMeanP), PSingle(pB), PSingle(pB), fW);
    Inc(pA, fWs);
    Inc(pB, fWs);
    Inc(pVarI, fWs);
    Inc(pCovIp, fWs);
    Inc(pMeanI, fWs);
    Inc(pMeanP, fWs);
  end;
end;

{$endregion}

end.
