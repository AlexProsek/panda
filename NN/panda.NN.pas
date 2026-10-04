unit panda.NN;

interface

uses

    panda.Intfs
  , panda.Arrays
  , panda.ArrManip
  , panda.Arithmetic
  , panda.ArrCmp
  , panda.Math
  , panda.Conv
  , System.Generics.Collections
  , System.SysUtils
  , System.Math
  ;

type
  TNNLayer = class abstract
  protected
    fOutput: INDArray<Single>;
    fInitialized: Boolean;
  public
    procedure Execute(const aInput: INDArray<Single>); virtual; abstract;
    function Initialize(const aInputShape: TNDAShape): Boolean; virtual; abstract;

    property Output: INDArray<Single> read fOutput;
    property Initialized: Boolean read fInitialized;
  end;

  TLinearLayer = class(TNNLayer)
  protected
    fWeights, fBiases: INDArray<Single>;
    fOutVec: INDArray<Single>;
    fInShape, fOutShape: TNDAShape;
    fInSz: NativeInt;
  {$region 'Getters/Setters'}
    procedure SetWeights(const aValue: INDArray<Single>);
    procedure SetBiases(const aValue: INDArray<Single>);
    procedure SetOutShape(const aValue: TNDAShape);
  {$endregion}
  public
    constructor Create(const aWeights, aBiases: INDArray<Single>);
    procedure Execute(const aInput: INDArray<Single>); override;
    function Initialize(const aInputShape: TNDAShape): Boolean; overload; override;
    function Initialize(const aInShape, aOutShape: TNDAShape): Boolean; reintroduce; overload;

    property Weights: INDArray<Single> read fWeights write SetWeights;
    property Biases: INDArray<Single> read fBiases write fBiases;
    property OutputShape: TNDAShape read fOutShape write SetOutShape;
  end;

  TElementwiseLayer = class abstract(TNNLayer)
  public
    function Initialize(const aInputShape: TNDAShape): Boolean; override;
  end;

  TRampLayer = class(TElementwiseLayer)
  public
    procedure Execute(const aInput: INDArray<Single>); override;
  end;

  TSigmoidLayer = class(TElementwiseLayer)
  public
    procedure Execute(const aInput: INDArray<Single>); override;
  end;

  TFlattenLayer = class(TNNLayer)
  protected
    fLevel: Integer;
    fInputShape, fOutputShape: TNDAShape;
    procedure SetLevel(aValue: Integer);
  public
    /// <summary>Sentinel for flattening every input axis.</summary>
    const AllLevels = High(Integer);
    procedure AfterConstruction; override;
    procedure Execute(const aInput: INDArray<Single>); override;
    function Initialize(const aInputShape: TNDAShape): Boolean; override;

    /// <summary>Axes to combine: positive values count from the first axis, negative from the last.</summary>
    property Level: Integer read fLevel write SetLevel;
  end;

  TNetEncoder = class abstract
  public
    /// <summary>Converts external image data to the network's Single tensor.</summary>
    function Encode(const aInput: IInterface): INDArray<Single>; virtual; abstract;
  end;

  INetClassResult = interface(IInterface)
    ['{74AC7E5F-2BE1-446E-AF63-4DA58A2F606B}']
    function GetIndices: TArray<Integer>;
    property Indices: TArray<Integer> read GetIndices;
  end;

  TNetDecoder = class abstract
  public
    /// <summary>Converts the network output tensor to an external value.</summary>
    function Decode(const aInput: INDArray<Single>): IInterface; virtual; abstract;
  end;

  TClassNetDecoder = class(TNetDecoder)
  protected
  public
    /// <summary>Returns zero-based indices of the first maximum on the final axis.</summary>
    function Decode(const aInput: INDArray<Single>): IInterface; override;
  end;

  TSoftmaxLayer = class(TNNLayer)
  protected
    fLevel: Integer;
  {$region 'Getters/Setters'}
    procedure SetLevel(aValue: Integer);
  {$endregion}
  public
    procedure AfterConstruction; override;
    procedure Execute(const aInput: INDArray<Single>); override;
    function Initialize(const aInputShape: TNDAShape): Boolean; override;

    property Level: Integer read fLevel write SetLevel;
  end;

  TConvLayer = class(TNNLayer)
  protected
    fWeights, fBiases: INDArray<Single>;
    fCorr: TCorrF32;
    procedure AddBiases;
  public
    constructor Create(const aWeights, aBiases: INDArray<Single>);
    destructor Destroy; override;
    procedure Execute(const aInput: INDArray<Single>); override;
    function Initialize(const aInputShape: TNDAShape): Boolean; override;
  end;

  TPoolingLayer = class(TNNLayer)
  protected
    fStrides: TArray<NativeInt>;
    fPoolSz: TArray<NativeInt>;
    fInterleaving: Boolean;
    procedure Exec(const aSrc, aDst: INDArray<Single>);
  {$region 'Getters/Setters'}
    procedure SetStrides(const aValue: TArray<NativeInt>); virtual;
    procedure SetPoolSz(const aValue: TArray<NativeInt>); virtual;
    procedure SetInterleaving(aValue: Boolean);
  {$endregion}
  public
    procedure AfterConstruction; override;
    procedure Execute(const aInput: INDArray<Single>); override;
    function Initialize(const aInputShape: TNDAShape): Boolean; override;

    property Strides: TArray<NativeInt> read fStrides write SetStrides;
    property PoolSize: TArray<NativeInt> read fPoolSz write SetPoolSz;
    property Interleaving: Boolean read fInterleaving write SetInterleaving;
  end;

  TNNetChain = class
  protected
    fLayers: TObjectList<TNNLayer>;
    fOutput: INDArray<Single>;
    fInitialized: Boolean;
    fInputShape: TNDAShape;
    fInputEncoder: TNetEncoder;
    fOutputDecoder: TNetDecoder;
  {$region 'Getters/Setters'}
    function GetLayer(I: Integer): TNNLayer;
    function GetLayerCount: Integer;
    procedure SetInputEncoder(aValue: TNetEncoder);
    procedure SetOutputDecoder(aValue: TNetDecoder);
  {$endregion}
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure AddLayer(aLayer: TNNLayer);
    function Initialize(const aInputShape: TNDAShape): Boolean;
    /// <summary>
    ///   Executes on a tensor or configured encoder input and returns decoded or raw output.
    /// </summary>
    function Execute(const aInput: IInterface): IInterface;

    property Layer[I: Integer]: TNNLayer read GetLayer; default;
    property LayerCount: Integer read GetLayerCount;
    property Output: INDArray<Single> read fOutput;
    property InputEncoder: TNetEncoder read fInputEncoder write SetInputEncoder;
    property OutputDecoder: TNetDecoder read fOutputDecoder write SetOutputDecoder;
  end;

  ENNUninitNetError = class(ENDAError);

implementation

{$EXCESSPRECISION OFF} // to prevent Single -> Double conversion by x64 compiler

uses
    panda.cvMath
  , panda.DynArrayUtils
  , panda.NN.PoolingLayer
  , System.TypInfo
  ;

resourcestring
  csUninitLayer = 'Layer is not initialized.';

{$region 'TLinearLayer'}

constructor TLinearLayer.Create(const aWeights, aBiases: INDArray<Single>);
begin
  Weights := aWeights;
  Biases := aBiases;
end;

procedure TLinearLayer.Execute(const aInput: INDArray<Single>);
var input: INDArray<Single>;
begin
  if not fInitialized then
    raise ENNUninitNetError.Create(csUninitLayer);

  if Length(fInShape) > 1 then begin
    Assert(CContiguousQ(aInput));
    input := TNDArray<Single>.Create(aInput.Data, [fInSz], [SizeOf(Single)]);
  end else
    input := aInput;

  ndaDot(fWeights, input, fOutVec);
  if Assigned(fBiases) then
    TTensorF32(fOutVec).AddTo(fBiases);
end;

function TLinearLayer.Initialize(const aInputShape: TNDAShape): Boolean;
var sz: NativeInt;
begin
  if fInitialized then exit(SameQ(fInShape, aInputShape));

  fInShape := Copy(aInputShape);
  fInSz := GetSize(fInShape);
  if Assigned(fWeights) then begin
    if fWeights.Shape[1] <> fInSz then exit(False);

  end else begin
    // create random weights
  end;

  if Assigned(fBiases) then begin
    if fBiases.Shape[0] <> fWeights.Shape[0] then exit(False);


  end else begin
    // create random biases
  end;

  if fOutShape <> nil then begin
    sz := GetSize(fOutShape);
    if fWeights.Shape[0] <> sz then exit(False);

    fOutput := TNDABuffer<Single>.Create(fOutShape);
    fOutVec := TNDArray<Single>.Create(fOutput.Data, [sz], [SizeOf(Single)]);
    fOutVec.SetFlags(NDAF_WRITEABLE);
  end else begin
    SetLength(fOutShape, 1);
    fOutShape[0] := fWeights.Shape[0];
    fOutput := TNDABuffer<Single>.Create(fOutShape);
    fOutVec := fOutput;
  end;

  fInitialized := True;
  Result := True;
end;

function TLinearLayer.Initialize(const aInShape, aOutShape: TNDAShape): Boolean;
begin
  if fInitialized then
    exit(SameQ(aInShape, fInShape) and SameQ(aOutShape, fOutShape));

  fOutShape := aOutShape;
  Result := Initialize(aInShape);
end;

{$region 'Getters/Setters'}

procedure TLinearLayer.SetWeights(const aValue: INDArray<Single>);
begin
  if not fInitialized then begin
    if Assigned(aValue) and (aValue.NDim <> 2) then
      raise ENDAShapeError.Create('The weights should be an array of rank 2.');

    fWeights := TNDAUt.AsContiguousArray<Single>(aValue);
  end;
end;

procedure TLinearLayer.SetBiases(const aValue: INDArray<Single>);
begin
  if not fInitialized then begin
    if Assigned(aValue) then begin
      if aValue.NDim <> 1 then
        raise ENDAShapeError.Create('The biases should be an array of rank 1.');
      fBiases := TNDAUt.AsContiguousArray<Single>(aValue);
    end else
      fBiases := nil;
  end;
end;

procedure TLinearLayer.SetOutShape(const aValue: TNDAShape);
begin
  if not fInitialized then
    fOutShape := aValue;
end;

{$endregion}

{$endregion}

{$region 'TElementwiseLayer'}

function TElementwiseLayer.Initialize(const aInputShape: TNDAShape): Boolean;
begin
  fOutput := TNDAUt.Empty<Single>(aInputShape);
  fInitialized := True;
  Result := True;
end;

{$endregion}

{$region 'TRampLayer'}

procedure TRampLayer.Execute(const aInput: INDArray<Single>);
begin
  if not fInitialized then
    raise ENNUninitNetError.Create(csUninitLayer);

  Assert(CContiguousQ(aInput));
  ndaMax(0, aInput, fOutput);
end;

{$endregion}

{$region 'TSigmoidLayer'}

procedure TSigmoidLayer.Execute(const aInput: INDArray<Single>);
var pIn, pOut: PSingle;
    pEnd: PByte;
    x, e: Single;
begin
  if not fInitialized then
    raise ENNUninitNetError.Create(csUninitLayer);

  Assert(CContiguousQ(aInput));
  pIn := PSingle(aInput.Data);
  pOut := PSingle(fOutput.Data);
  pEnd := PByte(pIn) + aInput.Size * SizeOf(Single);
  while PByte(pIn) < pEnd do begin
    x := pIn^;
    if x >= 0 then
      pOut^ := 1 / (1 + Exp(-x))
    else begin
      e := Exp(x);
      pOut^ := e / (1 + e);
    end;
    Inc(pIn);
    Inc(pOut);
  end;
end;

{$endregion}

{$region 'TFlattenLayer'}

procedure TFlattenLayer.AfterConstruction;
begin
  inherited;
  fLevel := AllLevels;
end;

procedure TFlattenLayer.SetLevel(aValue: Integer);
begin
  if not fInitialized then
    fLevel := aValue;
end;

function TFlattenLayer.Initialize(const aInputShape: TNDAShape): Boolean;
var flattenCount, flattenStart, I, outIndex: NativeInt;
    flattenedSize: NativeInt;
begin
  if fInitialized then
    exit(SameQ(fInputShape, aInputShape));
  if Length(aInputShape) = 0 then exit(False);
  for I := 0 to High(aInputShape) do
    if aInputShape[I] <= 0 then exit(False);

  if fLevel = AllLevels then begin
    flattenStart := 0;
    flattenCount := Length(aInputShape);
  end else if fLevel >= 0 then begin
    flattenStart := 0;
    flattenCount := NativeInt(fLevel) + 1;
  end else begin
    flattenCount := NativeInt(-Int64(fLevel)) + 1;
    flattenStart := Length(aInputShape) - flattenCount;
  end;
  if (flattenCount <= 0) or (flattenCount > Length(aInputShape)) then
    exit(False);

  flattenedSize := 1;
  for I := flattenStart to flattenStart + flattenCount - 1 do
    flattenedSize := flattenedSize * aInputShape[I];

  SetLength(fOutputShape, Length(aInputShape) - flattenCount + 1);
  outIndex := 0;
  for I := 0 to flattenStart - 1 do begin
    fOutputShape[outIndex] := aInputShape[I];
    Inc(outIndex);
  end;
  fOutputShape[outIndex] := flattenedSize;
  Inc(outIndex);
  for I := flattenStart + flattenCount to High(aInputShape) do begin
    fOutputShape[outIndex] := aInputShape[I];
    Inc(outIndex);
  end;

  fInputShape := Copy(aInputShape);
  fOutput := TNDABuffer<Single>.Create(fOutputShape);
  fInitialized := True;
  Result := True;
end;

procedure TFlattenLayer.Execute(const aInput: INDArray<Single>);
var input: INDArray<Single>;
begin
  if not fInitialized then
    raise ENNUninitNetError.Create(csUninitLayer);
  if not SameQ(aInput.Shape, fInputShape) then
    raise ENDAShapeError.Create('Input shape does not match the initialized flatten layer.');

  input := TNDAUt.AsContiguousArray<Single>(aInput);
  Move(input.Data^, fOutput.Data^, input.Size * SizeOf(Single));
end;

{$endregion}

{$region 'TClassNetDecoder'}

type
  TNetClassResult = class(TInterfacedObject, INetClassResult)
  private
    fIndices: TArray<Integer>;
    function GetIndices: TArray<Integer>;
  public
    constructor Create(const aIndices: TArray<Integer>);
  end;

constructor TNetClassResult.Create(const aIndices: TArray<Integer>);
begin
  inherited Create;
  fIndices := Copy(aIndices);
end;

function TNetClassResult.GetIndices: TArray<Integer>;
begin
  Result := Copy(fIndices);
end;

function TClassNetDecoder.Decode(const aInput: INDArray<Single>): IInterface;
var input: INDArray<Single>;
    decoded: TArray<Integer>;
    classCount, vectorCount, I, bestIdx, s: NativeInt;
    nDim: Integer;
    p: PByte;
begin
  Assert(Assigned(aInput));

  nDim := aInput.NDim;
  if not (((nDim = 1) or (nDim = 2)) or (aInput.Size > 0)) then
    raise ENDAShapeError.Create('Input to decoder has to be a vector or a batch of vectors.');

  classCount := aInput.Shape[aInput.NDim - 1];
  input := TNDAUt.AsContiguousArray<Single>(aInput);
  vectorCount := input.Size div classCount;
  SetLength(decoded, vectorCount);
  p := input.Data;
  s := input.Strides[0];
  for I := 0 to High(decoded) do begin
    cvMaxPos(PSingle(p), classCount, bestIdx);
    decoded[I] := bestIdx;
    Inc(p, s);
  end;

  Result := TNetClassResult.Create(decoded);
end;

{$endregion}

{$region 'TSoftmaxLayer'}

procedure TSoftmaxLayer.AfterConstruction;
begin
  inherited;
  fLevel := -1;
end;

procedure TSoftmaxLayer.Execute(const aInput: INDArray<Single>);
var tmp: INDArray<Single>;
    sh: TArray<NativeInt>;
begin
  if not fInitialized then
    raise ENNUninitNetError.Create(csUninitLayer);

  TNDAUt.Fill<Single>(fOutput, aInput);
  cvExp(PSingle(fOutput.Data), fOutput.Size);
  tmp := ndaTotalAtLvl(fOutput, fLevel);
  if tmp.NDim > 0 then begin
    if fLevel < 0 then
      sh := TDynAUt.Append<NativeInt>(tmp.Shape, 1)
    else
      sh := TDynAUt.Insert<NativeInt>(tmp.Shape, 1, fLevel);
    tmp := tmp.Reshape(sh);
  end;
  TTensorF32(fOutput).DivideBy(tmp);
end;

function TSoftmaxLayer.Initialize(const aInputShape: TNDAShape): Boolean;
begin
  if fLevel >= Length(aInputShape) then exit(False);

  fOutput := TNDABuffer<Single>.Create(aInputShape);
  fInitialized := True;
  Result := True;
end;

{$region 'Getters/Setters'}

procedure TSoftmaxLayer.SetLevel(aValue: Integer);
begin
  if not fInitialized then
    fLevel := aValue;
end;

{$endregion}

{$endregion}

{$region 'TConvLayer'}

constructor TConvLayer.Create(const aWeights, aBiases: INDArray<Single>);
begin
  fWeights := aWeights;
  fBiases := aBiases;
  fCorr := nil;
end;

destructor TConvLayer.Destroy;
begin
  fCorr.Free;
  inherited;
end;

procedure TConvLayer.Execute(const aInput: INDArray<Single>);
begin
  fCorr.Evaluate(aInput);
  AddBiases;
end;

procedure TConvLayer.AddBiases;
var k: SNDIntIndex;
begin
  if not Assigned(fBiases) then exit;

  for k in NDIRange(0, fOutput.Shape[0] - 1) do
    TTensorF32(fOutput[[k]]).AddTo(fBiases[[k]]);
end;

function TConvLayer.Initialize(const aInputShape: TNDAShape): Boolean;
var arr: INDArray<Single>;
begin
  if 
    Assigned(fBiases) and
    ((fBiases.NDim <> 1) or (fBiases.Shape[0] <> fWeights.Shape[0])) 
  then
    exit(False);

  fCorr.Free;
  fCorr := TCorrF32.Create(fWeights, aInputShape);
  arr := fCorr.Output;
  fOutput := TNDArrayWrapper<Single>.Create(arr, TDynAUt.Drop<NativeInt>(arr.Shape, [1]));
  Result := True;
end;

{$endregion}

{$region 'TPoolingLayer'}

procedure TPoolingLayer.AfterConstruction;
begin
  inherited;
  fPoolSz := TArray<NativeInt>.Create(2);
  fInterleaving := False;
end;

procedure TPoolingLayer.Exec(const aSrc, aDst: INDArray<Single>);
begin
  case Length(fPoolSz) of
    1: begin
     _MaxPool1D(
       PSingle(aSrc.Data), PSingle(aDst.Data),
       aSrc.Shape[0],
       fStrides[0],
       fPoolSz[0]
     )
    end;

    2: begin
      _MaxPool2D(
        PSingle(aSrc.Data), PSingle(aDst.Data),
        aSrc.Shape[0], aSrc.Shape[1],
        aSrc.Strides[0], aDst.Strides[0],
        fStrides[0], fStrides[1],
        fPoolSz[0], fPoolSz[1]
      );
    end
  else
    raise ENotImplemented.CreateFmt('Pooling layer is not implemented for dimension %d.', [fOutput.NDim]);
  end;
end;

procedure TPoolingLayer.Execute(const aInput: INDArray<Single>);
var I: NativeInt;
begin
  if aInput.NDim > Length(fPoolSz) then begin
    Assert(aInput.Shape[0] = fOutput.Shape[0]);
    for I := 0 to fOutput.Shape[0] - 1 do
      Exec(aInput[[NDI(I)]], fOutput[[NDI(I)]]);
  end else
    Exec(aInput, fOutput);
end;

function TPoolingLayer.Initialize(const aInputShape: TNDAShape): Boolean;
var I, s, dim, o: NativeInt;
    outSh: TNDAShape;
    bBatch: Boolean;
begin
  if fInitialized then exit(False);

  dim := Length(fPoolSz);
  bBatch := (Length(aInputShape) = dim + 1);
  if not ((Length(aInputShape) = dim) or bBatch) then exit(False);

  SetLength(outSh, Length(aInputShape));
  if bBatch then begin
    outSh[0] := aInputShape[0];
    o := 1;
  end else
    o := 0;

  for I := 0 to High(outSh) - o do begin
    s := GetOutputSize(aInputShape[I + o], fStrides[I], fPoolSz[I]);
    if s <= 0 then exit(False);
    outSh[I + o] := s;
  end;
  fOutput := TNDABuffer<Single>.Create(outSh);
  Result := True;
end;

{$region 'Getters/Setters'}

procedure TPoolingLayer.SetStrides(const aValue: TArray<NativeInt>);
begin
  if (not fInitialized) and (Length(aValue) = Length(fPoolSz)) then begin
    fStrides := aValue;
  end;
end;

procedure TPoolingLayer.SetPoolSz(const aValue: TArray<NativeInt>);
begin
  if not fInitialized then begin
    Assert(Length(aValue) > 0);
    fPoolSz := aValue;
    if not Assigned(fStrides) then
      fStrides := fPoolSz;
  end;
end;

procedure TPoolingLayer.SetInterleaving(aValue: Boolean);
begin
  if not fInitialized then
    fInterleaving := aValue;
end;

{$endregion}

{$endregion}

{$region 'TNNetChain'}

procedure TNNetChain.AfterConstruction;
begin
  inherited;
  fLayers := TObjectList<TNNLayer>.Create;
end;

procedure TNNetChain.BeforeDestruction;
begin
  inherited;
  fOutputDecoder.Free;
  fInputEncoder.Free;
  fLayers.Free;
end;

procedure TNNetChain.AddLayer(aLayer: TNNLayer);
begin
  fLayers.Add(aLayer);
end;

function TNNetChain.Initialize(const aInputShape: TNDAShape): Boolean;
var inSh: TNDAShape;
    I: Integer;
begin
  fInitialized := False;
  fInputShape := Copy(aInputShape);
  inSh := aInputShape;
  for I := 0 to fLayers.Count - 1 do begin
    if not fLayers[I].Initialize(inSh) then exit(False);
    inSh := fLayers[I].Output.Shape;
  end;
  fInitialized := True;
  Result := True;
end;

function TNNetChain.Execute(const aInput: IInterface): IInterface;
var input: INDArray<Single>;
    I: Integer;
    tensorInput: INDArray<Single>;
begin
  if not fInitialized then
    raise ENNUninitNetError.Create('Network is not initialized.');

  if Assigned(fInputEncoder) then
    input := fInputEncoder.Encode(aInput)
  else
  if Supports(aInput, INDArray<Single>, tensorInput) then
    input := tensorInput
  else
    raise EArgumentException.Create('Network without an input encoder requires an INDArray<Single> input.');

  if not SameQ(input.Shape, fInputShape) then
    raise ENDAShapeError.Create('Input shape does not match the initialized network input shape.');

  for I := 0 to fLayers.Count - 1 do begin
    fLayers[I].Execute(input);
    input := fLayers[I].Output;
  end;
  fOutput := input;

  if Assigned(fOutputDecoder) then
    Result := fOutputDecoder.Decode(fOutput)
  else
    Result := fOutput;
end;

{$region 'Getters/Setters'}

function TNNetChain.GetLayer(I: Integer): TNNLayer;
begin
  Result := fLayers[I];
end;

function TNNetChain.GetLayerCount: Integer;
begin
  Result := fLayers.Count;
end;

procedure TNNetChain.SetInputEncoder(aValue: TNetEncoder);
begin
  if fInputEncoder <> aValue then begin
    fInputEncoder.Free;
    fInputEncoder := aValue;
  end;
end;

procedure TNNetChain.SetOutputDecoder(aValue: TNetDecoder);
begin
  if fOutputDecoder <> aValue then begin
    fOutputDecoder.Free;
    fOutputDecoder := aValue;
  end;
end;

{$endregion}

{$endregion}

end.
