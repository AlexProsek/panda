unit panda.NN.Importer;

interface

uses
    panda.MAT4io
  , panda.NN
  , panda.NN.ImageEncoder
  , panda.Intfs
  , System.Classes
  , System.SysUtils
  , System.Math
  ;

type
  ENNImportError = class(Exception);

  /// <summary>
  /// Reads a sequential neural network from a MAT4 file. The first matrix must
  /// be the UTF-8 JSON manifest named __panda_nn__; parameter matrices are
  /// referenced by their MAT4 variable names in that manifest.
  /// Only sequential dense and supported activation layers are imported.
  /// </summary>
  TNNMAT4Importer = class(TMAT4Importer)
  private
    function ReadManifest: string;
    function ReadParameter(const aName: string): INDArray<Single>;
  public
    /// <summary>Opens a MAT4 file for network import.</summary>
    /// <param name="aFileName">Path to the MAT4 file.</param>
    constructor Create(const aFileName: string); overload;

    /// <summary>Reads a MAT4 stream for network import.</summary>
    /// <param name="aStream">Stream positioned at the first MAT4 matrix.</param>
    /// <param name="aOwnStream">If true, the importer frees the stream.</param>
    constructor Create(aStream: TStream; aOwnStream: Boolean = False); overload;

    /// <summary>Builds and initializes the network described by the MAT4 data.</summary>
    /// <returns>A caller-owned sequential network chain.</returns>
    /// <exception cref="ENNImportError">The manifest or parameters are invalid or unsupported.</exception>
    function ReadNetwork: TNNetChain;
  end;

implementation

uses
    panda.Arrays
  , panda.ArrManip
  , System.Generics.Collections
  , System.JSON
  , System.Rtti
  ;

const
  cManifestName = '__panda_nn__';
  cManifestFormat = 'panda.nn.mat4';
  cManifestVersion = 1;

function JsonObject(aValue: TJSONValue; const aContext: string): TJSONObject;
begin
  if not (aValue is TJSONObject) then
    raise ENNImportError.CreateFmt('%s must be a JSON object.', [aContext]);
  Result := TJSONObject(aValue);
end;

function JsonArray(aValue: TJSONValue; const aContext: string): TJSONArray;
begin
  if not (aValue is TJSONArray) then
    raise ENNImportError.CreateFmt('%s must be a JSON array.', [aContext]);
  Result := TJSONArray(aValue);
end;

function JsonString(aObject: TJSONObject; const aName, aContext: string;
  aRequired: Boolean = True): string;
var value: TJSONValue;
begin
  value := aObject.Values[aName];
  if value = nil then begin
    if aRequired then
      raise ENNImportError.CreateFmt('Missing %s.%s.', [aContext, aName]);
    Result := '';
    exit;
  end;
  if not (value is TJSONString) then
    raise ENNImportError.CreateFmt('%s.%s must be a string.', [aContext, aName]);
  Result := value.Value;
end;

function JsonInteger(aValue: TJSONValue; const aContext: string): Integer;
begin
  if (aValue = nil) or not TryStrToInt(aValue.Value, Result) then
    raise ENNImportError.CreateFmt('%s must be an integer.', [aContext]);
end;

function ReadShape(aValue: TJSONValue; const aContext: string): TNDAShape;
var shape: TJSONArray;
    I: Integer;
begin
  shape := JsonArray(aValue, aContext);
  if shape.Count = 0 then
    raise ENNImportError.CreateFmt('%s cannot be empty.', [aContext]);
  SetLength(Result, shape.Count);
  for I := 0 to shape.Count - 1 do begin
    Result[I] := JsonInteger(shape.Items[I], Format('%s[%d]', [aContext, I]));
    if Result[I] <= 0 then
      raise ENNImportError.CreateFmt('%s[%d] must be positive.', [aContext, I]);
  end;
end;

procedure ValidateManifestFormat(aRoot: TJSONObject);
var manifestFormat: string;
    value: TJSONValue;
begin
  manifestFormat := JsonString(aRoot, 'format', 'manifest');
  if manifestFormat <> cManifestFormat then
    raise ENNImportError.CreateFmt('Unsupported network manifest format "%s".', [manifestFormat]);
  value := aRoot.Values['version'];
  if JsonInteger(value, 'manifest.version') <> cManifestVersion then
    raise ENNImportError.CreateFmt('Unsupported network manifest version %s.', [value.Value]);
end;

function NormalizeActivation(const aActivation, aContext: string): string;
begin
  Result := LowerCase(aActivation);
  if Result = '' then Result := 'linear';
  if not ((Result = 'linear') or (Result = 'none') or (Result = 'relu') or
          (Result = 'ramp') or (Result = 'sigmoid') or (Result = 'softmax')) then
    raise ENNImportError.CreateFmt('Unsupported activation "%s" in %s.',
      [aActivation, aContext]);
end;

function ReadFlattenLevel(aValue: TJSONValue; const aContext: string): Integer;
begin
  if aValue = nil then
    exit(TFlattenLayer.AllLevels);
  if (aValue is TJSONString) and SameText(aValue.Value, 'Infinity') then
    exit(TFlattenLayer.AllLevels);
  Result := JsonInteger(aValue, aContext);
end;

function JsonBoolean(aObject: TJSONObject; const aName, aContext: string;
  aDefault: Boolean): Boolean;
var value: TJSONValue;
begin
  value := aObject.Values[aName];
  if value = nil then exit(aDefault);
  if not (value is TJSONBool) then
    raise ENNImportError.CreateFmt('%s.%s must be a Boolean.', [aContext, aName]);
  Result := TJSONBool(value).AsBoolean;
end;

procedure ReadNetworkAdapters(aRoot: TJSONObject; aNetwork: TNNetChain);
var adapterObject: TJSONObject;
    imageSize: TNDAShape;
    adapterType, colorSpace: string;
    imageEncoder: TImageNetEncoder;
    classDecoder: TClassNetDecoder;
begin
  if aRoot.Values['input_encoder'] <> nil then begin
    adapterObject := JsonObject(aRoot.Values['input_encoder'], 'input_encoder');
    adapterType := LowerCase(JsonString(adapterObject, 'type', 'input_encoder'));
    if adapterType <> 'image' then
      raise ENNImportError.CreateFmt('Unsupported input encoder type "%s".', [adapterType]);

    if adapterObject.Values['image_size'] <> nil then begin
      imageSize := ReadShape(adapterObject.Values['image_size'], 'input_encoder.image_size');
      if Length(imageSize) <> 2 then
        raise ENNImportError.Create('input_encoder.image_size must contain width and height.');
    end else
      imageSize := TNDAShape.Create(128, 128);

    colorSpace := LowerCase(JsonString(adapterObject, 'color_space', 'input_encoder', False));
    if (colorSpace <> '') and (colorSpace <> 'rgb') and (colorSpace <> 'grayscale') then
      raise ENNImportError.CreateFmt('Unsupported image color space "%s".', [colorSpace]);
    if colorSpace = 'grayscale' then
      imageEncoder := TImageNetEncoder.Create(imageSize[0], imageSize[1], nicsGrayscale)
    else
      imageEncoder := TImageNetEncoder.Create(imageSize[0], imageSize[1], nicsRGB);
    try
      imageEncoder.Interleaving := JsonBoolean(adapterObject, 'interleaving', 'input_encoder', False);
      imageEncoder.DataTransposed := JsonBoolean(adapterObject, 'data_transposed', 'input_encoder', False);
      aNetwork.InputEncoder := imageEncoder;
    except
      imageEncoder.Free;
      raise;
    end;
  end;

  if aRoot.Values['output_decoder'] <> nil then begin
    adapterObject := JsonObject(aRoot.Values['output_decoder'], 'output_decoder');
    adapterType := LowerCase(JsonString(adapterObject, 'type', 'output_decoder'));
    if adapterType <> 'class' then
      raise ENNImportError.CreateFmt('Unsupported output decoder type "%s".', [adapterType]);
    classDecoder := TClassNetDecoder.Create;
    aNetwork.OutputDecoder := classDecoder;
  end;
end;

procedure AddActivation(aNetwork: TNNetChain; const aActivation: string;
  aLevel: Integer = -1);
var layer: TNNLayer;
    softmaxLayer: TSoftmaxLayer;
begin
  if (aActivation = 'linear') or (aActivation = 'none') then exit;

  if (aActivation = 'relu') or (aActivation = 'ramp') then
    layer := TRampLayer.Create
  else if aActivation = 'sigmoid' then
    layer := TSigmoidLayer.Create
  else if aActivation = 'softmax' then begin
    softmaxLayer := TSoftmaxLayer.Create;
    softmaxLayer.Level := Max(-1, aLevel - 1);
    layer := softmaxLayer;
  end
  else
    raise ENNImportError.CreateFmt('Unsupported activation "%s".', [aActivation]);
  aNetwork.AddLayer(layer);
end;

{ TNNMAT4Importer }

constructor TNNMAT4Importer.Create(const aFileName: string);
begin
  inherited Create(aFileName);
end;

constructor TNNMAT4Importer.Create(aStream: TStream; aOwnStream: Boolean);
begin
  inherited Create(aStream, aOwnStream);
end;

function TNNMAT4Importer.ReadManifest: string;
var labels: TArray<string>;
    bytesArray: INDArray<Byte>;
    bytes: TBytes;
    I, manifestIndex: NativeInt;
begin
  manifestIndex := 0;
  labels := ReadLabels;
  if (Length(labels) = 0) or (labels[0] <> cManifestName) then
    raise ENNImportError.CreateFmt('The first MAT4 matrix must be named "%s".', [cManifestName]);
  if (MatType(0) <> mtNumeric) or (MatElementType(0) <> etUInt8) then
    raise ENNImportError.Create('The network manifest matrix must be a numeric uint8 vector.');
  if not TryRead<Byte>(0, bytesArray) or (bytesArray.NDim <> 1) then
    raise ENNImportError.Create('The network manifest matrix must be a uint8 vector.');

  SetLength(bytes, bytesArray.Size);
  if bytesArray.Size > 0 then
    Move(bytesArray.Data^, bytes[0], bytesArray.Size);
  Result := TEncoding.UTF8.GetString(bytes);

  for I := 0 to High(labels) do
    if labels[I] = cManifestName then
      Inc(manifestIndex);
  if manifestIndex > 1 then
    raise ENNImportError.CreateFmt('MAT4 contains more than one "%s" matrix.', [cManifestName]);
end;

function TNNMAT4Importer.ReadParameter(const aName: string): INDArray<Single>;
var labels: TArray<string>;
    I, foundIndex: NativeInt;
begin
  Result := nil;
  labels := ReadLabels;
  foundIndex := -1;
  for I := 1 to High(labels) do
    if labels[I] = aName then begin
      if foundIndex >= 0 then
        raise ENNImportError.CreateFmt('MAT4 contains duplicate parameter matrix "%s".', [aName]);
      foundIndex := I;
    end;

  if foundIndex < 0 then begin
    raise ENNImportError.CreateFmt('MAT4 parameter matrix "%s" was not found.', [aName]);
  end;

  if MatType(foundIndex) <> mtNumeric then
    raise ENNImportError.CreateFmt('MAT4 parameter "%s" must be numeric.', [aName]);
  if MatElementType(foundIndex) in [etCmplx64, etCmplx128] then
    raise ENNImportError.CreateFmt('Complex MAT4 parameter "%s" is not supported.', [aName]);
  if not TryRead<Single>(foundIndex, Result) then
    raise ENNImportError.CreateFmt('Could not read MAT4 parameter "%s" as Single.', [aName]);
end;

function TNNMAT4Importer.ReadNetwork: TNNetChain;
var manifestText: string;
    manifestValue: TJSONValue;
    root, layerObject, weightsObject: TJSONObject;
    layers: TJSONArray;
    inputShape, kernelShape: TNDAShape;
    I, J: Integer;
    activationLevel: Integer;
    kernelSize: NativeInt;
    layerType, activation, kernelName, biasName, layout, context: string;
    poolStride: TNDAShape;
    kernel, bias, reshapedKernel: INDArray<Single>;
    layer: TLinearLayer;
    convLayer: TConvLayer;
    poolLayer: TPoolingLayer;
    flattenLayer: TFlattenLayer;
begin
  Result := nil;
  manifestText := ReadManifest;
  manifestValue := TJSONObject.ParseJSONValue(manifestText);
  if manifestValue = nil then
    raise ENNImportError.Create('The network manifest is not valid JSON.');
  try
    root := JsonObject(manifestValue, 'manifest');
    ValidateManifestFormat(root);
    inputShape := ReadShape(root.Values['input_shape'], 'input_shape');
    layers := JsonArray(root.Values['layers'], 'layers');
    if layers.Count = 0 then
      raise ENNImportError.Create('The network must contain at least one layer.');

    Result := TNNetChain.Create;
    try
      ReadNetworkAdapters(root, Result);
      for I := 0 to layers.Count - 1 do begin
        context := Format('layers[%d]', [I]);
        layerObject := JsonObject(layers.Items[I], context);
        layerType := LowerCase(JsonString(layerObject, 'type', context));
        if layerType = 'dense' then begin
          weightsObject := JsonObject(layerObject.Values['weights'], context + '.weights');
          kernelName := JsonString(weightsObject, 'kernel', context + '.weights');
          kernel := ReadParameter(kernelName);
          if kernel.NDim <> 2 then
            raise ENNImportError.CreateFmt('%s kernel "%s" must be a matrix.', [context, kernelName]);

          biasName := JsonString(weightsObject, 'bias', context + '.weights', False);
          if biasName <> '' then begin
            bias := ReadParameter(biasName);
            if bias.NDim <> 1 then
              raise ENNImportError.CreateFmt('%s bias "%s" must be a vector.', [context, biasName]);
          end else
            bias := nil;

          layout := LowerCase(JsonString(layerObject, 'kernel_layout', context, False));
          if layout = '' then layout := 'input_output';
          if (layout = 'input_output') or (layout = 'io') then
            kernel := TNDAMan.Transpose<Single>(kernel)
          else if not ((layout = 'output_input') or (layout = 'oi')) then
            raise ENNImportError.CreateFmt('Unsupported kernel_layout "%s" in %s.', [layout, context]);

          activation := NormalizeActivation(JsonString(layerObject, 'activation', context, False), context);
          activationLevel := -1;
          if (activation = 'softmax') and (layerObject.Values['activation_level'] <> nil) then
            activationLevel := JsonInteger(layerObject.Values['activation_level'], context + '.activation_level');
          layer := TLinearLayer.Create(kernel, bias);
          Result.AddLayer(layer);
          AddActivation(Result, activation, activationLevel);
        end else if layerType = 'conv' then begin
          weightsObject := JsonObject(layerObject.Values['weights'], context + '.weights');
          kernelName := JsonString(weightsObject, 'kernel', context + '.weights');
          kernel := ReadParameter(kernelName);
          if kernel.NDim <> 2 then
            raise ENNImportError.CreateFmt('%s kernel "%s" must be a matrix.', [context, kernelName]);

          kernelShape := ReadShape(layerObject.Values['kernel_shape'], context + '.kernel_shape');
          if not (Length(kernelShape) in [3, 4]) then
            raise ENNImportError.CreateFmt('%s kernel_shape must have rank 3 or 4.', [context]);
          kernelSize := 1;
          for J := 0 to High(kernelShape) do
            kernelSize := kernelSize * kernelShape[J];
          if kernelSize <> kernel.Size then
            raise ENNImportError.CreateFmt('%s kernel_shape element count does not match kernel "%s".',
              [context, kernelName]);

          reshapedKernel := TNDABuffer<Single>.Create(kernelShape);
          Move(kernel.Data^, reshapedKernel.Data^, kernel.Size * SizeOf(Single));

          biasName := JsonString(weightsObject, 'bias', context + '.weights', False);
          if biasName <> '' then begin
            bias := ReadParameter(biasName);
            if bias.NDim <> 1 then
              raise ENNImportError.CreateFmt('%s bias "%s" must be a vector.', [context, biasName]);
          end else
            bias := nil;

          convLayer := TConvLayer.Create(reshapedKernel, bias);
          Result.AddLayer(convLayer);
        end else if layerType = 'maxpool' then begin
          kernelShape := ReadShape(layerObject.Values['pool_size'], context + '.pool_size');
          if not (Length(kernelShape) in [1, 2]) then
            raise ENNImportError.CreateFmt('%s pool_size must have rank 1 or 2.', [context]);

          if layerObject.Values['stride'] <> nil then begin
            poolStride := ReadShape(layerObject.Values['stride'], context + '.stride');
            if Length(poolStride) <> Length(kernelShape) then
              raise ENNImportError.CreateFmt('%s stride rank must match pool_size.', [context]);
          end else
            poolStride := kernelShape;

          poolLayer := TPoolingLayer.Create;
          poolLayer.PoolSize := kernelShape;
          poolLayer.Strides := poolStride;
          Result.AddLayer(poolLayer);
        end else if layerType = 'flatten' then begin
          flattenLayer := TFlattenLayer.Create;
          flattenLayer.Level := ReadFlattenLevel(layerObject.Values['level'], context + '.level');
          Result.AddLayer(flattenLayer);
        end else if (layerType = 'relu') or (layerType = 'ramp') or
                    (layerType = 'sigmoid') or (layerType = 'softmax') then begin
          activation := NormalizeActivation(layerType, context);
          activationLevel := -1;
          if (activation = 'softmax') and (layerObject.Values['level'] <> nil) then
            activationLevel := JsonInteger(layerObject.Values['level'], context + '.level');
          AddActivation(Result, activation, activationLevel);
        end else
          raise ENNImportError.CreateFmt('Unsupported layer type "%s" in %s.', [layerType, context]);
      end;

      if not Result.Initialize(inputShape) then
        raise ENNImportError.Create('Network layers are incompatible with the declared input shape.');
    except
      FreeAndNil(Result);
      raise;
    end;
  finally
    manifestValue.Free;
  end;
end;

end.
