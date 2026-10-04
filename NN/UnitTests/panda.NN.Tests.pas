unit panda.NN.Tests;

interface

uses
    TestFramework
  , panda.Intfs
  , panda.Arrays
  , panda.NN
  , panda.MAT4io
  , panda.NN.Importer
  , panda.NN.ImageEncoder
  , panda.ImgProc.Types
  , panda.ImgProc.Images
  , panda.Tests.NDATestCase
  , System.Classes
  , System.IOUtils
  , System.Rtti
  , System.SysUtils
  ;

type
  TNNLinLayerTests = class(TNDATestCase)
  protected const
    stol = 1e-6;
  published
    procedure I1D_O1D; // I - input , O - output
    procedure I1D_O2D;
    procedure I2D_O2D;
  end;

  TNNRampLayerTests = class(TNDATestCase)
  protected const
    stol = 1e-6;
  published
    procedure Ramp_1D;
    procedure Ramp_2D;
  end;

  TNNSoftmaxLayerTests = class(TNDATestCase)
  protected const
    stol = 1e-6;
  published
    procedure Softmax_1D;
    procedure Softmax_2D;
    procedure Softmax_2D_Lvl0;
  end;

  TNNConvLayerTests = class(TNDATestCase)
  protected const
    stol = 1e-6;
  published
    procedure Conv_1D;
    procedure Conv_1D_WithBias;
    procedure Conv_2D;
    procedure Conv_2D_TwoFilters;
    procedure Conv_2D_TwoFilters_NoBias;
  end;

  TNNChainTests = class(TNDATestCase)
  protected const
    stol = 1e-6;
  published
    procedure LinThenRamp;
    procedure ExecuteReturnsRawTensor;
    procedure ExecuteRejectsMismatchedShape;
    procedure ExecuteImageEncoderRejectsTensor;
    procedure ImageEncoderGrayToRGBLayouts;
    procedure ClassNetDecoderVecInput;
    procedure ClassNetDecoderBatch;
  end;

  TNNFlattenLayerTests = class(TNDATestCase)
  published
    procedure FlattenAllLevels;
    procedure FlattenLeadingAxes;
    procedure FlattenTrailingAxes;
  end;

  TNNMAT4ImporterTests = class(TNDATestCase)
  protected const
    stol = 1e-6;
  published
    procedure ImportDenseSigmoid;
    procedure DecodeClassBatch;
    procedure ImportFlattenWithImageClassAdapters;
    procedure ImportMNISTStructure;
    procedure ImportKnownLinear;
    procedure ImportKnownRampChain;
    procedure ImportKnownSigmoid;
    procedure ImportKnownSoftmax;
    procedure ImportKnownSoftmaxLevel;
    procedure ImportKnownConv2D;
    procedure ImportKnownConv1D;
    procedure ImportKnownPool2D;
    procedure ImportKnownPool1D;
    procedure KnownLinearOutput;
    procedure KnownRampChainOutput;
    procedure KnownSigmoidOutput;
    procedure KnownSoftmaxOutput;
    procedure KnownSoftmaxLevelOutput;
    procedure KnownConv2DOutput;
    procedure KnownConv1DOutput;
    procedure KnownPool2DOutput;
    procedure KnownPool1DOutput;
  end;

implementation

function NNTestDataFile(const aFileName: string): string;
begin
  Result := TPath.GetFullPath(TPath.Combine(ExtractFilePath(ParamStr(0)),
    '..\..\TestData\' + aFileName));
end;

function ReadNNMAT4Fixture(const aFileName: string): TNNetChain;
var importer: TNNMAT4Importer;
begin
  importer := TNNMAT4Importer.Create(NNTestDataFile(aFileName));
  try
    Result := importer.ReadNetwork;
  finally
    importer.Free;
  end;
end;

{$region 'TNNFlattenLayerTests'}

procedure TNNFlattenLayerTests.FlattenAllLevels;
var layer: TFlattenLayer;
    values: TArray<Single>;
begin
  layer := TFlattenLayer.Create;
  try
    CheckEquals(TFlattenLayer.AllLevels, layer.Level);
    CheckTrue(layer.Initialize([2, 3, 2]));
    layer.Execute(TNDAUt.AsArray<Single>([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12], [2, 3, 2]));

    CheckEquals([12], layer.Output.Shape);
    CheckTrue(TNDAUt.TryAsDynArray<Single>(layer.Output, values));
    CheckEquals([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12], values, 0);
  finally
    layer.Free;
  end;
end;

procedure TNNFlattenLayerTests.FlattenLeadingAxes;
var layer: TFlattenLayer;
    values: TArray<TArray<Single>>;
begin
  layer := TFlattenLayer.Create;
  try
    layer.Level := 1;
    CheckTrue(layer.Initialize([2, 3, 2]));
    layer.Execute(TNDAUt.AsArray<Single>([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12], [2, 3, 2]));

    CheckEquals([6, 2], layer.Output.Shape);
    CheckTrue(TNDAUt.TryAsDynArray2D<Single>(layer.Output, values));
    CheckEquals([1, 2], values[0], 0);
    CheckEquals([11, 12], values[5], 0);
  finally
    layer.Free;
  end;
end;

procedure TNNFlattenLayerTests.FlattenTrailingAxes;
var layer: TFlattenLayer;
    values: TArray<TArray<Single>>;
begin
  layer := TFlattenLayer.Create;
  try
    layer.Level := -1;
    CheckTrue(layer.Initialize([2, 3, 2]));
    layer.Execute(TNDAUt.AsArray<Single>([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12], [2, 3, 2]));

    CheckEquals([2, 6], layer.Output.Shape);
    CheckTrue(TNDAUt.TryAsDynArray2D<Single>(layer.Output, values));
    CheckEquals([1, 2, 3, 4, 5, 6], values[0], 0);
    CheckEquals([7, 8, 9, 10, 11, 12], values[1], 0);
  finally
    layer.Free;
  end;
end;

{$endregion}

{$region 'TNNLinLayerTests'}

procedure TNNLinLayerTests.I1D_O1D;
var w, b: INDArray<Single>;
    l: TLinearLayer;
    v: TArray<Single>;
begin
  w := TNDAUt.AsArray<Single>([[1, 2, 3], [4, 5, 6]]);
  b := TNDAUt.AsArray<Single>([3, 4]);

  l := TLinearLayer.Create(w, b);
  try
    CheckTrue(l.Initialize([3]));

    l.Execute(TNDAUt.AsArray<Single>([1, 2, 3]));

    CheckTrue(TNDAUt.TryAsDynArray<Single>(l.Output, v));
    CheckEquals([17, 36], v, stol);
  finally
    l.Free;
  end;
end;

procedure TNNLinLayerTests.I1D_O2D;
var w, b: INDArray<Single>;
    l: TLinearLayer;
    m: TArray<TArray<Single>>;
begin
  w := TNDAUt.AsArray<Single>([[1, 2, 3], [4, 5, 6], [7, 8, 9], [10, 11, 12]]);
  b := TNDAUt.AsArray<Single>([1, 2, 3, 4]);

  l := TLinearLayer.Create(w, b);
  try
    CheckTrue(l.Initialize([3], [2, 2]));

    l.Execute(TNDAUt.AsArray<Single>([1, 2, 3]));

    CheckTrue(TNDAUt.TryAsDynArray2D<Single>(l.Output, m));
    CheckEquals(2, Length(m));
    CheckEquals([15, 34], m[0], stol);
    CheckEquals([53, 72], m[1], stol);
  finally
    l.Free;
  end;
end;

procedure TNNLinLayerTests.I2D_O2D;
var w, b: INDArray<Single>;
    l: TLinearLayer;
    m: TArray<TArray<Single>>;
begin
  w := TNDAUt.AsArray<Single>([[1, 2, 3, 4], [4, 5, 6, 7], [7, 8, 9, 10], [10, 11, 12, 13]]);
  b := TNDAUt.AsArray<Single>([1, 2, 3, 4]);

  l := TLinearLayer.Create(w, b);
  try
    CheckTrue(l.Initialize([2, 2], [2, 2]));

    l.Execute(TNDAUt.AsArray<Single>([[1, 2], [3, 4]]));

    CheckTrue(TNDAUt.TryAsDynArray2D<Single>(l.Output, m));
    CheckEquals(2, Length(m));
    CheckEquals([31,  62], m[0], stol);
    CheckEquals([93, 124], m[1], stol);
  finally
    l.Free;
  end;
end;

{$endregion}

{$region 'TNNRampLayerTests'}

procedure TNNRampLayerTests.Ramp_1D;
var l: TRampLayer;
    v: TArray<Single>;
begin
  l := TRampLayer.Create;
  try
    CheckTrue(l.Initialize([3]));

    l.Execute(TNDAUt.AsArray<Single>([-1, 2, -3]));

    CheckTrue(TNDAUt.TryAsDynArray<Single>(l.Output, v));
    CheckEquals([0, 2, 0], v, stol);
  finally
    l.Free;
  end;
end;

procedure TNNRampLayerTests.Ramp_2D;
var l: TRampLayer;
    m: TArray<TArray<Single>>;
begin
  l := TRampLayer.Create;
  try
    CheckTrue(l.Initialize([2, 3]));

    l.Execute(TNDAUt.AsArray<Single>([[-1, 2, -3], [4, -2, 1]]));

    CheckTrue(TNDAUt.TryAsDynArray2D<Single>(l.Output, m));
    CheckEquals(2, Length(m));
    CheckEquals([0, 2, 0], m[0], stol);
    CheckEquals([4, 0, 1], m[1], stol);
  finally
    l.Free;
  end;
end;

{$endregion}

{$region 'TNNSoftmaxLayerTests'}

procedure TNNSoftmaxLayerTests.Softmax_1D;
var l: TSoftmaxLayer;
    v: TArray<Single>;
begin
  l := TSoftmaxLayer.Create;
  try
    CheckTrue(l.Initialize([3]));

    l.Execute(TNDAUt.AsArray<Single>([1, 2, 3]));

    CheckTrue(TNDAUt.TryAsDynArray<Single>(l.Output, v));
    CheckEquals([0.09003057, 0.24472848, 0.66524088], v, stol);
  finally
    l.Free;
  end;
end;

procedure TNNSoftmaxLayerTests.Softmax_2D;
var l: TSoftmaxLayer;
    m: TArray<TArray<Single>>;
begin
  l := TSoftmaxLayer.Create;
  try
    CheckTrue(l.Initialize([2, 3]));

    l.Execute(TNDAUt.AsArray<Single>([[1, 0, 1], [0, 0, 1]]));

    CheckTrue(TNDAUt.TryAsDynArray2D<Single>(l.Output, m));
    CheckEquals(2, Length(m));
    CheckEquals([0.422318816, 0.155362427, 0.422318816], m[0], stol);
    CheckEquals([0.211941570, 0.211941570, 0.576116860], m[1], stol);
  finally
    l.Free;
  end;
end;

procedure TNNSoftmaxLayerTests.Softmax_2D_Lvl0;
var l: TSoftmaxLayer;
    m: TArray<TArray<Single>>;
begin
  l := TSoftmaxLayer.Create;
  try
    l.Level := 0;
    CheckTrue(l.Initialize([2, 3]));

    l.Execute(TNDAUt.AsArray<Single>([[1, 0, 1], [0, 0, 1]]));

    CheckTrue(TNDAUt.TryAsDynArray2D<Single>(l.Output, m));
    CheckEquals(2, Length(m));
    CheckEquals([0.731058597, 0.5, 0.5], m[0], stol);
    CheckEquals([0.268941432, 0.5, 0.5], m[1], stol);
  finally
    l.Free;
  end;
end;

{$endregion}

{$region 'TNNConvLayerTests'}

procedure TNNConvLayerTests.Conv_1D;
var w: INDArray<Single>;
    l: TConvLayer;
    m: TArray<TArray<Single>>;
begin
  // kernel rank is input rank + 2, with a size-1 axis at index 1
  w := TNDAUt.AsArray<Single>([1, 0, -1], [1, 1, 3]);

  l := TConvLayer.Create(w, nil);
  try
    CheckTrue(l.Initialize([6]));

    l.Execute(TNDAUt.AsArray<Single>([1, 2, 3, 4, 5, 6]));

    CheckEquals([1, 4], l.Output.Shape);
    CheckTrue(TNDAUt.TryAsDynArray2D<Single>(l.Output, m));
    CheckEquals(1, Length(m));
    CheckEquals([-2, -2, -2, -2], m[0], stol);
  finally
    l.Free;
  end;
end;

procedure TNNConvLayerTests.Conv_1D_WithBias;
var w, b: INDArray<Single>;
    l: TConvLayer;
    m: TArray<TArray<Single>>;
begin
  w := TNDAUt.AsArray<Single>([1, 0, -1], [1, 1, 3]);
  b := TNDAUt.AsArray<Single>([2.5]);

  l := TConvLayer.Create(w, b);
  try
    CheckTrue(l.Initialize([6]));

    l.Execute(TNDAUt.AsArray<Single>([1, 2, 3, 4, 5, 6]));

    CheckTrue(TNDAUt.TryAsDynArray2D<Single>(l.Output, m));
    CheckEquals([0.5, 0.5, 0.5, 0.5], m[0], stol);
  finally
    l.Free;
  end;
end;

procedure TNNConvLayerTests.Conv_2D;
var w: INDArray<Single>;
    l: TConvLayer;
    t: TArray<TArray<TArray<Single>>>;
begin
  w := TNDAUt.AsArray<Single>([1, 0, 0, 1], [1, 1, 2, 2]);

  l := TConvLayer.Create(w, nil);
  try
    CheckTrue(l.Initialize([4, 4]));

    l.Execute(TNDAUt.AsArray<Single>([
      [ 1,  2,  3,  4],
      [ 5,  6,  7,  8],
      [ 9, 10, 11, 12],
      [13, 14, 15, 16]
    ]));

    CheckEquals([1, 3, 3], l.Output.Shape);
    CheckTrue(TNDAUt.TryAsDynArray3D<Single>(l.Output, t));
    CheckEquals(1, Length(t));
    CheckEquals([ 7,  9, 11], t[0, 0], stol);
    CheckEquals([15, 17, 19], t[0, 1], stol);
    CheckEquals([23, 25, 27], t[0, 2], stol);
  finally
    l.Free;
  end;
end;

procedure TNNConvLayerTests.Conv_2D_TwoFilters;
var w, b: INDArray<Single>;
    l: TConvLayer;
    t: TArray<TArray<TArray<Single>>>;
begin
  w := TNDAUt.AsArray<Single>([
    1, 0,
    0, 1,

    0, 1,
    1, 1
  ], [2, 1, 2, 2]);
  b := TNDAUt.AsArray<Single>([1, 2]);

  l := TConvLayer.Create(w, b);
  try
    CheckTrue(l.Initialize([3, 3]));

    l.Execute(TNDAUt.AsArray<Single>([
      [1, 2, 3],
      [4, 5, 6],
      [7, 8, 9]
    ]));

    CheckEquals([2, 2, 2], l.Output.Shape);
    CheckTrue(TNDAUt.TryAsDynArray3D<Single>(l.Output, t));
    CheckEquals(2, Length(t));
    CheckEquals([ 7,  9], t[0, 0], stol);
    CheckEquals([13, 15], t[0, 1], stol);
    CheckEquals([13, 16], t[1, 0], stol);
    CheckEquals([22, 25], t[1, 1], stol);
  finally
    l.Free;
  end;
end;

procedure TNNConvLayerTests.Conv_2D_TwoFilters_NoBias;
var w: INDArray<Single>;
    l: TConvLayer;
    t: TArray<TArray<TArray<Single>>>;
begin
  w := TNDAUt.AsArray<Single>([
    1, 0,
    0, 1,

    0, 1,
    1, 1
  ], [2, 1, 2, 2]);

  l := TConvLayer.Create(w, nil);
  try
    CheckTrue(l.Initialize([3, 3]));

    l.Execute(TNDAUt.AsArray<Single>([
      [1, 2, 3],
      [4, 5, 6],
      [7, 8, 9]
    ]));

    CheckEquals([2, 2, 2], l.Output.Shape);
    CheckTrue(TNDAUt.TryAsDynArray3D<Single>(l.Output, t));
    CheckEquals(2, Length(t));
    CheckEquals([ 6,  8], t[0, 0], stol);
    CheckEquals([12, 14], t[0, 1], stol);
    CheckEquals([11, 14], t[1, 0], stol);
    CheckEquals([20, 23], t[1, 1], stol);
  finally
    l.Free;
  end;
end;

{$endregion}

{$region 'TNNChainTests'}

procedure TNNChainTests.LinThenRamp;
var net: TNNetChain;
    v: TArray<Single>;
begin
  net := TNNetChain.Create;
  try
    net.AddLayer(TLinearLayer.Create(
      TNDAUt.AsArray<Single>([[1, -1], [2, 0]]),
      TNDAUt.AsArray<Single>([0, -1])));
    net.AddLayer(TRampLayer.Create);

    CheckTrue(net.Initialize([2]));

    net.Execute(TNDAUt.AsArray<Single>([1, 3]));

    CheckTrue(TNDAUt.TryAsDynArray<Single>(net.Output, v));
    CheckEquals([0, 1], v, stol);
  finally
    net.Free;
  end;
end;

procedure TNNChainTests.ExecuteReturnsRawTensor;
var net: TNNetChain;
    input, output: INDArray<Single>;
    resultInterface: IInterface;
begin
  net := TNNetChain.Create;
  try
    CheckTrue(net.Initialize([2]));
    input := TNDAUt.AsArray<Single>([1, 2]);

    resultInterface := net.Execute(input);

    CheckTrue(Supports(resultInterface, INDArray<Single>, output));
    CheckTrue(output = input);
  finally
    net.Free;
  end;
end;

procedure TNNChainTests.ExecuteRejectsMismatchedShape;
var net: TNNetChain;
    input: INDArray<Single>;
    raised: Boolean;
begin
  net := TNNetChain.Create;
  try
    CheckTrue(net.Initialize([2]));
    input := TNDAUt.AsArray<Single>([1, 2, 3]);
    raised := False;

    try
      net.Execute(input);
    except
      on ENDAShapeError do raised := True;
    end;

    CheckTrue(raised);
  finally
    net.Free;
  end;
end;

procedure TNNChainTests.ExecuteImageEncoderRejectsTensor;
var net: TNNetChain;
    input: INDArray<Single>;
    raised: Boolean;
begin
  net := TNNetChain.Create;
  try
    net.InputEncoder := TImageNetEncoder.Create(2, 2, nicsGrayscale);
    CheckTrue(net.Initialize([1, 2, 2]));
    input := TNDAUt.AsArray<Single>([[1, 2], [3, 4]]);
    raised := False;

    try
      net.Execute(input);
    except
      on EArgumentException do raised := True;
    end;

    CheckTrue(raised);
  finally
    net.Free;
  end;
end;

procedure TNNChainTests.ImageEncoderGrayToRGBLayouts;
var image: IImage<Byte>;
    encoder: TImageNetEncoder;
    encoded: INDArray<Single>;
    values: TArray<Single>;
    interleaving, transposed: Boolean;
    x, y, index, channel, pixel: NativeInt;
begin
  image := TImgUt.AsImage<Byte>(TNDAUt.AsArray<Byte>([[10, 20], [30, 40], [50, 60]]));

  for interleaving := False to True do
    for transposed := False to True do begin
      encoder := TImageNetEncoder.Create(2, 3, nicsRGB);
      try
        encoder.Interleaving := interleaving;
        encoder.DataTransposed := transposed;
        encoded := encoder.Encode(image);

        if interleaving then begin
          if transposed then
            CheckEquals([2, 3, 3], encoded.Shape)
          else
            CheckEquals([3, 2, 3], encoded.Shape);
        end else begin
          if transposed then
            CheckEquals([3, 2, 3], encoded.Shape)
          else
            CheckEquals([3, 3, 2], encoded.Shape);
        end;
        SetLength(values, encoded.Size);
        Move(encoded.Data^, values[0], encoded.Size * SizeOf(Single));

        for y := 0 to 2 do
          for x := 0 to 1 do begin
            if transposed then pixel := x * 3 + y
            else pixel := y * 2 + x;
            if interleaving then index := pixel * 3
            else index := pixel;
            case y * 2 + x of
              0: pixel := 10;
              1: pixel := 20;
              2: pixel := 30;
              3: pixel := 40;
              4: pixel := 50;
            else
              pixel := 60;
            end;
            for channel := 0 to 2 do begin
              if interleaving then
                CheckEquals(pixel / 255, values[index + channel], 1e-6)
              else
                CheckEquals(pixel / 255, values[index + channel * 6], 1e-6);
            end;
          end;
      finally
        encoder.Free;
      end;
    end;
end;

procedure TNNChainTests.ClassNetDecoderVecInput;
var dec: TClassNetDecoder;
    input: INDArray<Single>;
    res: INetClassResult;
begin
  dec := TClassNetDecoder.Create;
  try
    input := TNDAUt.AsArray<Single>([1, 3, 2]);

    CheckTrue(Supports(dec.Decode(input), INetClassResult, res));
    CheckEquals(1, Length(res.Indices));
    CheckEquals(1, res.Indices[0]);
  finally
    dec.Free;
  end;
end;

procedure TNNChainTests.ClassNetDecoderBatch;
var dec: TClassNetDecoder;
    input: INDArray<Single>;
    res: INetClassResult;
begin
  dec := TClassNetDecoder.Create;
  try
    input := TNDAUt.AsArray<Single>([[1, 3, 2], [3, 2, 1]]);

    CheckTrue(Supports(dec.Decode(input), INetClassResult, res));
    CheckEquals([1, 0], res.Indices);
  finally
    dec.Free;
  end;
end;

{$endregion}

{$region 'TNNMAT4ImporterTests'}

procedure TNNMAT4ImporterTests.ImportDenseSigmoid;
const
  Manifest = '{"format":"panda.nn.mat4","version":1,"input_shape":[2],' +
    '"layers":[{"type":"dense","weights":{"kernel":"dense.kernel",' +
    '"bias":"dense.bias"},"activation":"sigmoid"}]}';
var stream: TMemoryStream;
    exporter: TMAT4Exporter;
    importer: TNNMAT4Importer;
    net: TNNetChain;
    manifestArray: INDArray<Byte>;
    manifestBytes: TBytes;
    output: TArray<Single>;
begin
  stream := TMemoryStream.Create;
  try
    exporter := TMAT4Exporter.Create(stream);
    try
      manifestBytes := TEncoding.UTF8.GetBytes(Manifest);
      manifestArray := TNDABuffer<Byte>.Create([Length(manifestBytes)]);
      Move(manifestBytes[0], manifestArray.Data^, Length(manifestBytes));
      exporter.WriteMatrix(manifestArray, '__panda_nn__');
      exporter.WriteMatrix(TNDAUt.AsArray<Single>([[1, 2], [3, 4]]), 'dense.kernel');
      exporter.WriteMatrix(TNDAUt.AsArray<Single>([0, 0]), 'dense.bias');
    finally
      exporter.Free;
    end;

    stream.Position := 0;
    importer := TNNMAT4Importer.Create(stream);
    try
      net := importer.ReadNetwork;
      try
        net.Execute(TNDAUt.AsArray<Single>([1, 1]));
        CheckTrue(TNDAUt.TryAsDynArray<Single>(net.Output, output));
        CheckEquals([0.98201379, 0.99752738], output, stol);
      finally
        net.Free;
      end;
    finally
      importer.Free;
    end;
  finally
    stream.Free;
  end;
end;

procedure TNNMAT4ImporterTests.DecodeClassBatch;
var decoder: TClassNetDecoder;
    decoded: IInterface;
    classResult: INetClassResult;
begin
  decoder := TClassNetDecoder.Create;
  try
    decoded := decoder.Decode(TNDAUt.AsArray<Single>([[0.1, 0.2, 0.7], [0.8, 0.1, 0.1]]));
    CheckTrue(Supports(decoded, INetClassResult, classResult));

    CheckEquals([2, 0], classResult.Indices);

    decoded := decoder.Decode(TNDAUt.AsArray<Single>([0.8, 0.8, 0.1]));
    CheckTrue(Supports(decoded, INetClassResult, classResult));
    CheckEquals([0], classResult.Indices);
  finally
    decoder.Free;
  end;
end;

procedure TNNMAT4ImporterTests.ImportFlattenWithImageClassAdapters;
const
  Manifest = '{"format":"panda.nn.mat4","version":1,"input_shape":[1,2,2],' +
    '"input_encoder":{"type":"image","image_size":[2,2],' +
    '"color_space":"grayscale"},"layers":[{"type":"flatten",' +
    '"level":"Infinity"}],"output_decoder":{"type":"class",' +
    '"labels":[0,1,2,3]}}';
var stream: TMemoryStream;
    exporter: TMAT4Exporter;
    importer: TNNMAT4Importer;
    network: TNNetChain;
    manifestArray: INDArray<Byte>;
    manifestBytes: TBytes;
    image: IImage<Byte>;
    encoded: INDArray<Single>;
    encodedValues: TArray<TArray<TArray<Single>>>;
    decoded: IInterface;
    classResult: INetClassResult;
begin
  stream := TMemoryStream.Create;
  try
    exporter := TMAT4Exporter.Create(stream);
    try
      manifestBytes := TEncoding.UTF8.GetBytes(Manifest);
      manifestArray := TNDABuffer<Byte>.Create([Length(manifestBytes)]);
      Move(manifestBytes[0], manifestArray.Data^, Length(manifestBytes));
      exporter.WriteMatrix(manifestArray, '__panda_nn__');
    finally
      exporter.Free;
    end;

    stream.Position := 0;
    importer := TNNMAT4Importer.Create(stream);
    try
      network := importer.ReadNetwork;
      try
        CheckTrue(network.Layer[0] is TFlattenLayer);
        CheckTrue(network.InputEncoder is TImageNetEncoder);
        CheckTrue(network.OutputDecoder is TClassNetDecoder);

        image := TImgUt.AsImage<Byte>(TNDAUt.AsArray<Byte>([[0, 128], [255, 64]]));
        encoded := network.InputEncoder.Encode(image);
        CheckEquals([1, 2, 2], encoded.Shape);
        CheckTrue(TNDAUt.TryAsDynArray3D<Single>(encoded, encodedValues));
        CheckEquals([0, 128 / 255], encodedValues[0, 0], stol);
        CheckEquals([1, 64 / 255], encodedValues[0, 1], stol);

        decoded := network.Execute(image);
        CheckTrue(Supports(decoded, INetClassResult, classResult));
        CheckEquals([2], classResult.Indices);
      finally
        network.Free;
      end;
    finally
      importer.Free;
    end;
  finally
    stream.Free;
  end;
end;

procedure TNNMAT4ImporterTests.ImportMNISTStructure;
var network: TNNetChain;
    imageEncoder: TImageNetEncoder;
    classDecoder: TClassNetDecoder;
begin
  network := ReadNNMAT4Fixture('nn-mnist.mat');
  try
    CheckEquals(11, network.LayerCount);
    CheckTrue(network.Layer[0] is TConvLayer);
    CheckTrue(network.Layer[1] is TRampLayer);
    CheckTrue(network.Layer[2] is TPoolingLayer);
    CheckTrue(network.Layer[3] is TConvLayer);
    CheckTrue(network.Layer[4] is TRampLayer);
    CheckTrue(network.Layer[5] is TPoolingLayer);
    CheckTrue(network.Layer[6] is TFlattenLayer);
    CheckTrue(network.Layer[7] is TLinearLayer);
    CheckTrue(network.Layer[8] is TRampLayer);
    CheckTrue(network.Layer[9] is TLinearLayer);
    CheckTrue(network.Layer[10] is TSoftmaxLayer);

    CheckEquals(TFlattenLayer.AllLevels, TFlattenLayer(network.Layer[6]).Level);
    CheckEquals([10], network.Layer[10].Output.Shape);

    CheckTrue(network.InputEncoder is TImageNetEncoder);
    imageEncoder := TImageNetEncoder(network.InputEncoder);
    CheckEquals(28, imageEncoder.Width);
    CheckEquals(28, imageEncoder.Height);
    CheckTrue(imageEncoder.ColorSpace = nicsGrayscale);
    CheckFalse(imageEncoder.Interleaving);
    CheckFalse(imageEncoder.DataTransposed);

    CheckTrue(network.OutputDecoder is TClassNetDecoder);
    classDecoder := TClassNetDecoder(network.OutputDecoder);
    CheckTrue(classDecoder is TClassNetDecoder);
  finally
    network.Free;
  end;
end;

procedure TNNMAT4ImporterTests.ImportKnownLinear;
var net: TNNetChain;
begin
  net := ReadNNMAT4Fixture('nn-linear.mat');
  try
    CheckEquals(1, net.LayerCount);
    CheckTrue(net.Layer[0] is TLinearLayer);
  finally
    net.Free;
  end;
end;

procedure TNNMAT4ImporterTests.ImportKnownRampChain;
var net: TNNetChain;
begin
  net := ReadNNMAT4Fixture('nn-ramp.mat');
  try
    CheckEquals(3, net.LayerCount);
    CheckTrue(net.Layer[0] is TLinearLayer);
    CheckTrue(net.Layer[1] is TRampLayer);
    CheckTrue(net.Layer[2] is TLinearLayer);
  finally
    net.Free;
  end;
end;

procedure TNNMAT4ImporterTests.ImportKnownSigmoid;
var net: TNNetChain;
begin
  net := ReadNNMAT4Fixture('nn-sigmoid.mat');
  try
    CheckEquals(2, net.LayerCount);
    CheckTrue(net.Layer[0] is TLinearLayer);
    CheckTrue(net.Layer[1] is TSigmoidLayer);
  finally
    net.Free;
  end;
end;

procedure TNNMAT4ImporterTests.ImportKnownSoftmax;
var net: TNNetChain;
begin
  net := ReadNNMAT4Fixture('nn-softmax.mat');
  try
    CheckEquals(2, net.LayerCount);
    CheckTrue(net.Layer[0] is TLinearLayer);
    CheckTrue(net.Layer[1] is TSoftmaxLayer);
    CheckEquals(-1, TSoftmaxLayer(net.Layer[1]).Level);
  finally
    net.Free;
  end;
end;

procedure TNNMAT4ImporterTests.ImportKnownSoftmaxLevel;
var net: TNNetChain;
begin
  net := ReadNNMAT4Fixture('nn-softmax-level1.mat');
  try
    CheckEquals(1, net.LayerCount);
    CheckTrue(net.Layer[0] is TSoftmaxLayer);
    CheckEquals(0, TSoftmaxLayer(net.Layer[0]).Level);
  finally
    net.Free;
  end;
end;

procedure TNNMAT4ImporterTests.ImportKnownConv2D;
var net: TNNetChain;
begin
  net := ReadNNMAT4Fixture('nn-conv2d.mat');
  try
    CheckEquals(1, net.LayerCount);
    CheckTrue(net.Layer[0] is TConvLayer);
  finally
    net.Free;
  end;
end;

procedure TNNMAT4ImporterTests.ImportKnownConv1D;
var net: TNNetChain;
begin
  net := ReadNNMAT4Fixture('nn-conv1d.mat');
  try
    CheckEquals(1, net.LayerCount);
    CheckTrue(net.Layer[0] is TConvLayer);
  finally
    net.Free;
  end;
end;

procedure TNNMAT4ImporterTests.ImportKnownPool2D;
var net: TNNetChain;
begin
  net := ReadNNMAT4Fixture('nn-pool2d.mat');
  try
    CheckEquals(1, net.LayerCount);
    CheckTrue(net.Layer[0] is TPoolingLayer);
    CheckEquals([2, 2], TPoolingLayer(net.Layer[0]).PoolSize);
    CheckEquals([2, 2], TPoolingLayer(net.Layer[0]).Strides);
  finally
    net.Free;
  end;
end;

procedure TNNMAT4ImporterTests.ImportKnownPool1D;
var net: TNNetChain;
begin
  net := ReadNNMAT4Fixture('nn-pool1d.mat');
  try
    CheckEquals(1, net.LayerCount);
    CheckTrue(net.Layer[0] is TPoolingLayer);
    CheckEquals([2], TPoolingLayer(net.Layer[0]).PoolSize);
    CheckEquals([1], TPoolingLayer(net.Layer[0]).Strides);
  finally
    net.Free;
  end;
end;

procedure TNNMAT4ImporterTests.KnownLinearOutput;
var net: TNNetChain;
    output: TArray<Single>;
begin
  net := ReadNNMAT4Fixture('nn-linear.mat');
  try
    net.Execute(TNDAUt.AsArray<Single>([1, 2]));

    CheckTrue(TNDAUt.TryAsDynArray<Single>(net.Output, output));
    CheckEquals([5.5, -1], output, stol);
  finally
    net.Free;
  end;
end;

procedure TNNMAT4ImporterTests.KnownRampChainOutput;
var net: TNNetChain;
    output: TArray<Single>;
begin
  net := ReadNNMAT4Fixture('nn-ramp.mat');
  try
    net.Execute(TNDAUt.AsArray<Single>([2, 1]));

    CheckTrue(TNDAUt.TryAsDynArray<Single>(net.Output, output));
    CheckEquals([5.25, 2], output, stol);
  finally
    net.Free;
  end;
end;

procedure TNNMAT4ImporterTests.KnownSigmoidOutput;
var net: TNNetChain;
    output: TArray<Single>;
begin
  net := ReadNNMAT4Fixture('nn-sigmoid.mat');
  try
    net.Execute(TNDAUt.AsArray<Single>([2, 1]));

    CheckTrue(TNDAUt.TryAsDynArray<Single>(net.Output, output));
    CheckEquals([0.81757444, 0.32082129], output, stol);
  finally
    net.Free;
  end;
end;

procedure TNNMAT4ImporterTests.KnownSoftmaxOutput;
var net: TNNetChain;
    output: TArray<Single>;
begin
  net := ReadNNMAT4Fixture('nn-softmax.mat');
  try
    net.Execute(TNDAUt.AsArray<Single>([1, 2]));

    CheckTrue(TNDAUt.TryAsDynArray<Single>(net.Output, output));
    CheckEquals([0.26762316, 0.72747511, 0.00490169], output, stol);
  finally
    net.Free;
  end;
end;

procedure TNNMAT4ImporterTests.KnownSoftmaxLevelOutput;
var net: TNNetChain;
    output: TArray<TArray<Single>>;
begin
  net := ReadNNMAT4Fixture('nn-softmax-level1.mat');
  try
    net.Execute(TNDAUt.AsArray<Single>([[1, 2], [3, 4]]));

    CheckTrue(TNDAUt.TryAsDynArray2D<Single>(net.Output, output));
    CheckEquals([0.11920292, 0.11920292], output[0], stol);
    CheckEquals([0.88079709, 0.88079709], output[1], stol);
  finally
    net.Free;
  end;
end;

procedure TNNMAT4ImporterTests.KnownConv2DOutput;
var net: TNNetChain;
    output: TArray<TArray<TArray<Single>>>;
begin
  net := ReadNNMAT4Fixture('nn-conv2d.mat');
  try
    net.Execute(TNDAUt.AsArray<Single>([
      [1, 2, 3],
      [4, 5, 6],
      [7, 8, 9]
    ]));

    CheckTrue(TNDAUt.TryAsDynArray3D<Single>(net.Output, output));
    CheckEquals(2, Length(output));
    CheckEquals([6, 8], output[0, 0], stol);
    CheckEquals([12, 14], output[0, 1], stol);
    CheckEquals([6, 8], output[1, 0], stol);
    CheckEquals([12, 14], output[1, 1], stol);
  finally
    net.Free;
  end;
end;

procedure TNNMAT4ImporterTests.KnownConv1DOutput;
var net: TNNetChain;
    output: TArray<TArray<Single>>;
begin
  net := ReadNNMAT4Fixture('nn-conv1d.mat');
  try
    net.Execute(TNDAUt.AsArray<Single>([1, 2, 3, 4, 5, 6]));

    CheckTrue(TNDAUt.TryAsDynArray2D<Single>(net.Output, output));
    CheckEquals(2, Length(output));
    CheckEquals([-2, -2, -2, -2], output[0], stol);
    CheckEquals([2, 2, 2, 2], output[1], stol);
  finally
    net.Free;
  end;
end;

procedure TNNMAT4ImporterTests.KnownPool2DOutput;
var net: TNNetChain;
    output: TArray<TArray<Single>>;
begin
  net := ReadNNMAT4Fixture('nn-pool2d.mat');
  try
    net.Execute(TNDAUt.AsArray<Single>([
      [1, 2, 3, 4],
      [5, 6, 7, 8],
      [9, 10, 11, 12],
      [13, 14, 15, 16]
    ]));

    CheckTrue(TNDAUt.TryAsDynArray2D<Single>(net.Output, output));
    CheckEquals([6, 8], output[0], stol);
    CheckEquals([14, 16], output[1], stol);
  finally
    net.Free;
  end;
end;

procedure TNNMAT4ImporterTests.KnownPool1DOutput;
var net: TNNetChain;
    output: TArray<Single>;
begin
  net := ReadNNMAT4Fixture('nn-pool1d.mat');
  try
    net.Execute(TNDAUt.AsArray<Single>([1, 3, 2, 5, 4, 6]));

    CheckTrue(TNDAUt.TryAsDynArray<Single>(net.Output, output));
    CheckEquals([3, 3, 5, 5, 6], output, stol);
  finally
    net.Free;
  end;
end;

{$endregion}

initialization

  RegisterTest(TNNLinLayerTests.Suite);
  RegisterTest(TNNRampLayerTests.Suite);
  RegisterTest(TNNSoftmaxLayerTests.Suite);
  RegisterTest(TNNConvLayerTests.Suite);
  RegisterTest(TNNChainTests.Suite);
  RegisterTest(TNNFlattenLayerTests.Suite);
  RegisterTest(TNNMAT4ImporterTests.Suite);

end.
