(* Export a trained, sequential Wolfram Language net to PaNDA MAT4.

   Usage:
     ExportNNMAT4[trainedNet, "network.mat", {inputWidth}]

    The input shape excludes the batch dimension. Supported layers are
    LinearLayer, ConvolutionLayer, max PoolingLayer, FlattenLayer, Ramp,
    LogisticSigmoid, and SoftmaxLayer. Image NetEncoders and class NetDecoders
    are included in the manifest. Unsupported preprocessing and decoder options
    are rejected.
   LinearLayer weights are written in output_input layout; convolution
   kernels are flattened to a MAT4 matrix and described by kernel_shape.
*)

ClearAll[ExportNNMAT4, nnMat4ActivationName, nnMat4NumericArray, nnMat4Int32Bytes,
  nnMat4PortProperty, nnMat4ImageEncoder, nnMat4ClassDecoder,
  nnMat4Unsigned32, nnMat4SingleParameters];

nnMat4Int32Bytes[value_Integer] := Reverse[IntegerDigits[value, 256, 4]];

nnMat4Unsigned32[bytes_List] := FromDigits[Reverse[bytes], 256];

nnMat4SingleParameters[file_] := Module[
  {bytes, source, entries = {}, offset = 0, header, rows, columns, imag,
   nameLength, payloadOffset, payloadEnd, valueCount, nameBytes, values, valid = True},
  bytes = BinaryReadList[file, "Byte"];
  source = OpenRead[file, BinaryFormat -> True, ByteOrdering -> -1];
  While[offset < Length[bytes] && valid,
    If[offset + 20 > Length[bytes], valid = False; Break[]];
    header = Take[bytes, {offset + 1, offset + 20}];
    rows = nnMat4Unsigned32[header[[5 ;; 8]]];
    columns = nnMat4Unsigned32[header[[9 ;; 12]]];
    imag = nnMat4Unsigned32[header[[13 ;; 16]]];
    nameLength = nnMat4Unsigned32[header[[17 ;; 20]]];
    valueCount = rows * columns;
    payloadOffset = offset + 20 + nameLength;
    payloadEnd = payloadOffset + valueCount * 8;
    If[nnMat4Unsigned32[header[[1 ;; 4]]] =!= 0 || imag =!= 0 ||
       rows <= 0 || columns <= 0 || nameLength <= 0 || payloadEnd > Length[bytes],
      valid = False; Break[]
    ];
    nameBytes = Take[bytes, {offset + 21, offset + 20 + nameLength}];
    SetStreamPosition[source, payloadOffset];
    values = BinaryReadList[source, "Real64", valueCount];
    If[Length[values] =!= valueCount, valid = False; Break[]];
    AppendTo[entries, {Join[nnMat4Int32Bytes[10], Drop[header, 4]], nameBytes, values}];
    offset = payloadEnd
  ];
  Close[source];
  If[valid && offset === Length[bytes], entries, $Failed]
];

nnMat4ActivationName[layer_, function_] := Which[
  MatchQ[layer, Ramp | ElementwiseLayer[Ramp]] || function === Ramp, "relu",
  MatchQ[layer, LogisticSigmoid | ElementwiseLayer[LogisticSigmoid]] || function === LogisticSigmoid, "sigmoid",
  MatchQ[layer, _SoftmaxLayer] || function === SoftmaxLayer, "softmax",
  True, $Failed
];

nnMat4NumericArray[value_, context_] := Module[{array = Normal[value]},
  If[!ArrayQ[array] || !VectorQ[Flatten[array], NumericQ] || !FreeQ[array, _Complex],
    Message[ExportNNMAT4::parameter, context]; Return[$Failed]
  ];
  N[array, MachinePrecision]
];

nnMat4PortProperty[net_, port_, property_] :=
  Quiet@Check[NetExtract[net, {port, property}], $Failed];

nnMat4ImageEncoder[net_] := Module[
  {encoder, imageSize, colorSpace, colorSpaceName, method, methodName,
   resampling, meanImage, varianceImage, interleaving, dataTransposed},
  encoder = Quiet@Check[NetExtract[net, "Input"], None];
  If[Head[encoder] =!= NetEncoder, Return[None]];

  imageSize = nnMat4PortProperty[net, "Input", "ImageSize"];
  If[imageSize === $Failed,
    Message[ExportNNMAT4::encoder]; Return[$Failed]
  ];
  If[IntegerQ[imageSize], imageSize = {imageSize, imageSize}];
  If[!MatchQ[imageSize, {_Integer?Positive, _Integer?Positive}],
    Message[ExportNNMAT4::encoder]; Return[$Failed]
  ];
  colorSpace = nnMat4PortProperty[net, "Input", "ColorSpace"];
  colorSpaceName = Which[
    StringQ[colorSpace], colorSpace,
    Head[colorSpace] === Symbol, SymbolName[colorSpace],
    True, ""
  ];
  colorSpace = Which[
    colorSpaceName === "Grayscale", "grayscale",
    colorSpaceName === "RGB", "rgb",
    True, $Failed
  ];
  method = nnMat4PortProperty[net, "Input", "Method"];
  methodName = Which[
    StringQ[method], method,
    Head[method] === Symbol, SymbolName[method],
    True, ""
  ];
  resampling = nnMat4PortProperty[net, "Input", "Resampling"];
  meanImage = nnMat4PortProperty[net, "Input", "MeanImage"];
  varianceImage = nnMat4PortProperty[net, "Input", "VarianceImage"];
  interleaving = nnMat4PortProperty[net, "Input", "Interleaving"];
  dataTransposed = nnMat4PortProperty[net, "Input", "DataTransposed"];
  If[colorSpace === $Failed || methodName =!= "Stretch" ||
     resampling =!= Automatic || meanImage =!= None || varianceImage =!= None ||
     !MemberQ[{True, False}, interleaving] || !MemberQ[{True, False}, dataTransposed],
    Message[ExportNNMAT4::encoder]; Return[$Failed]
  ];

  <|"type" -> "image", "image_size" -> imageSize,
    "color_space" -> colorSpace, "interleaving" -> interleaving,
    "data_transposed" -> dataTransposed|>
];

nnMat4ClassDecoder[net_] := Module[
  {decoder, labels, inputDepth, multilabel},
  decoder = Quiet@Check[NetExtract[net, "Output"], None];
  If[Head[decoder] =!= NetDecoder, Return[None]];
  inputDepth = nnMat4PortProperty[net, "Output", "InputDepth"];
  multilabel = nnMat4PortProperty[net, "Output", "Multilabel"];
  labels = nnMat4PortProperty[net, "Output", "Labels"];
  If[inputDepth === $Failed || multilabel === $Failed || labels === $Failed ||
     inputDepth =!= 1 || multilabel =!= False,
    Message[ExportNNMAT4::decoder]; Return[$Failed]
  ];
  If[labels =!= None &&
     (!ListQ[labels] || !VectorQ[labels, (StringQ[#] || NumberQ[#]) &]),
    Message[ExportNNMAT4::decoder]; Return[$Failed]
  ];
  <|"type" -> "class"|> ~Join~ If[labels === None, <||>, <|"labels" -> labels|>]
];

ExportNNMAT4::usage = "ExportNNMAT4[net, file, inputShape] exports a sequential neural net, supported image encoder, and class decoder to the PaNDA MAT4 format.";
ExportNNMAT4::net = "The network must be a NetChain containing supported sequential layers.";
ExportNNMAT4::layer = "Layer `1` is unsupported. Use LinearLayer, ConvolutionLayer, max PoolingLayer, FlattenLayer, Ramp, LogisticSigmoid, or SoftmaxLayer.";
ExportNNMAT4::parameter = "Parameter `1` must be a real numeric array.";
ExportNNMAT4::encoder = "Only image NetEncoders with RGB or Grayscale color, stretch resizing, automatic resampling, and no mean/variance images are supported.";
ExportNNMAT4::decoder = "Only single-depth, single-label class NetDecoders with string or numeric labels are supported.";
ExportNNMAT4::shape = "inputShape must be a nonempty list of positive integers.";

ExportNNMAT4[net_NetChain, file_String, inputShape_List] := Module[
  {layers, manifestLayers = {}, matrices = {}, layer, weights, biases,
   kernelName, biasName, activation, i, inputBytes, manifest, rules,
    parameterFile, parameters, prefix, stream, manifestName, kernelShape, kernelMatrix,
  poolSize, poolStride, poolPadding, flattenLevel, inputEncoder,
    outputDecoder},

  If[!VectorQ[inputShape, IntegerQ[#] && # > 0 &],
    Message[ExportNNMAT4::shape]; Return[$Failed]
  ];

  (* NetExtract[net, All] returns the sequential layers for a NetChain. *)
  layers = NetExtract[net, All];
  If[AssociationQ[layers], layers = Values[layers]];
  If[!ListQ[layers] || layers === {}, Message[ExportNNMAT4::net]; Return[$Failed]];

  Do[
    layer = layers[[i]];
    If[MatchQ[layer, _LinearLayer],
      weights = nnMat4NumericArray[NetExtract[net, {i, "Weights"}], "layer " <> ToString[i] <> " weights"];
      If[weights === $Failed || ArrayDepth[weights] =!= 2, Return[$Failed]];
      biases = NetExtract[net, {i, "Biases"}];
      kernelName = "layer" <> ToString[i] <> ".kernel";
      biasName = "";
      If[biases =!= None,
        biases = nnMat4NumericArray[biases, "layer " <> ToString[i] <> " biases"];
        If[biases === $Failed || ArrayDepth[biases] =!= 1, Return[$Failed]];
        biasName = "layer" <> ToString[i] <> ".bias";
        (* Export the vector as a MAT row vector; the Delphi reader maps
           1 x N to its rank-1 array representation. *)
        AppendTo[matrices, biasName -> ArrayReshape[biases, {1, Length[biases]}]]
      ];
      AppendTo[matrices, kernelName -> weights];
      activation = "linear";
      AppendTo[manifestLayers, <|
        "type" -> "dense",
        "weights" -> Join[<|"kernel" -> kernelName|>, If[biasName === "", <||>, <|"bias" -> biasName|>]],
        "kernel_layout" -> "output_input",
        "activation" -> activation
      |>],
    If[MatchQ[layer, _ConvolutionLayer],
      weights = nnMat4NumericArray[NetExtract[net, {i, "Weights"}], "layer " <> ToString[i] <> " weights"];
      kernelShape = Dimensions[weights];
      If[!MemberQ[{3, 4}, Length[kernelShape]],
        Message[ExportNNMAT4::layer, i]; Return[$Failed]
      ];
      kernelName = "layer" <> ToString[i] <> ".kernel";
      kernelMatrix = ArrayReshape[weights, {First[kernelShape], Times @@ Rest[kernelShape]}];
      AppendTo[matrices, kernelName -> kernelMatrix];
      biases = NetExtract[net, {i, "Biases"}];
      biasName = "";
      If[biases =!= None,
        biases = nnMat4NumericArray[biases, "layer " <> ToString[i] <> " biases"];
        If[biases === $Failed || ArrayDepth[biases] =!= 1, Return[$Failed]];
        biasName = "layer" <> ToString[i] <> ".bias";
        AppendTo[matrices, biasName -> ArrayReshape[biases, {1, Length[biases]}]]
      ];
      AppendTo[manifestLayers, <|
        "type" -> "conv",
        "weights" -> Join[<|"kernel" -> kernelName|>, If[biasName === "", <||>, <|"bias" -> biasName|>]],
        "kernel_shape" -> kernelShape
      |>],
      If[MatchQ[layer, _PoolingLayer],
        poolSize = NetExtract[net, {i, "KernelSize"}];
        poolStride = NetExtract[net, {i, "Stride"}];
        poolPadding = NetExtract[net, {i, "PaddingSize"}];
        If[NetExtract[net, {i, "Function"}] =!= Max ||
           NetExtract[net, {i, "Interleaving"}] =!= False ||
           !MemberQ[{1, 2}, Length[poolSize]] ||
           Length[poolStride] =!= Length[poolSize] ||
           !AllTrue[Flatten[poolPadding], # === 0 &],
          Message[ExportNNMAT4::layer, i]; Return[$Failed]
        ];
        AppendTo[manifestLayers, <|"type" -> "maxpool", "pool_size" -> poolSize,
          "stride" -> poolStride|>],
        If[MatchQ[layer, _FlattenLayer],
          flattenLevel = NetExtract[net, {i, "Level"}];
          If[!(IntegerQ[flattenLevel] || flattenLevel === Infinity),
            Message[ExportNNMAT4::layer, i]; Return[$Failed]
          ];
          AppendTo[manifestLayers, <|"type" -> "flatten",
            "level" -> If[flattenLevel === Infinity, "Infinity", flattenLevel]|>],
          activation = nnMat4ActivationName[layer, Quiet[NetExtract[net, {i, "Function"}]]];
          If[activation === $Failed, Message[ExportNNMAT4::layer, i]; Return[$Failed]];
          If[activation === "softmax",
            AppendTo[manifestLayers, <|"type" -> activation,
              "level" -> NetExtract[net, {i, "Level"}]|>],
            AppendTo[manifestLayers, <|"type" -> activation|>]
          ]
        ]
      ]
    ]],
    {i, Length[layers]}
  ];

  inputEncoder = nnMat4ImageEncoder[net];
  outputDecoder = nnMat4ClassDecoder[net];
  If[inputEncoder === $Failed || outputDecoder === $Failed, Return[$Failed]];
  manifest = Join[<|"format" -> "panda.nn.mat4", "version" -> 1,
      "input_shape" -> inputShape, "layers" -> manifestLayers|>,
    If[inputEncoder === None, <||>, <|"input_encoder" -> inputEncoder|>],
    If[outputDecoder === None, <||>, <|"output_decoder" -> outputDecoder|>]
  ];
  inputBytes = ToCharacterCode[ExportString[manifest, "JSON", "Compact" -> True], "UTF8"];
  rules = matrices;
  If[matrices === {},
    parameters = {},
    parameterFile = FileNameJoin[{$TemporaryDirectory, CreateUUID[] <> ".mat"}];
    If[Export[parameterFile, rules, "MAT", "Version" -> 4] === $Failed, Return[$Failed]];
    parameters = nnMat4SingleParameters[parameterFile];
    DeleteFile[parameterFile];
    If[parameters === $Failed, Message[ExportNNMAT4::parameter, "MAT4 parameter matrix"]; Return[$Failed]]
  ];

  (* Prefix the parameter matrices with an explicit MAT4 uint8 manifest. *)
  manifestName = "__panda_nn__";
  prefix = Join[
    nnMat4Int32Bytes[50], nnMat4Int32Bytes[1],
    nnMat4Int32Bytes[Length[inputBytes]], nnMat4Int32Bytes[0],
    nnMat4Int32Bytes[StringLength[manifestName] + 1],
    ToCharacterCode[manifestName, "UTF8"], {0}, inputBytes
  ];
  stream = OpenWrite[file, BinaryFormat -> True, ByteOrdering -> -1];
  BinaryWrite[stream, prefix, "Byte"];
  Do[
    BinaryWrite[stream, parameters[[i, 1]], "Byte"];
    BinaryWrite[stream, parameters[[i, 2]], "Byte"];
    BinaryWrite[stream, parameters[[i, 3]], "Real32"],
    {i, Length[parameters]}
  ];
  Close[stream];
  file
];

ExportNNMAT4[___] := (Message[ExportNNMAT4::net]; $Failed);
