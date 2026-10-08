(* usage: wolframscript -f GenerateNNMAT4TestData.wls *)

Get[FileNameJoin[{DirectoryName[$InputFileName], "..\\ExportNNMAT4.wl"}]];

testDataDirectory = FileNameJoin[{DirectoryName[$InputFileName], "TestData"}];
If[!DirectoryQ[testDataDirectory], CreateDirectory[testDataDirectory]];

linearNet = NetChain[{
  LinearLayer[2, "Weights" -> {{1., 2.}, {-1., 0.5}}, "Biases" -> {0.5, -1.}]
}, "Input" -> 2];

rampNet = NetChain[{
  LinearLayer[2, "Weights" -> {{1., 0.5}, {-1., 2.}}, "Biases" -> {0., -2.}],
  Ramp,
  LinearLayer[2, "Weights" -> {{2., -1.}, {1., 1.}}, "Biases" -> {0.25, -0.5}]
}, "Input" -> 2];

sigmoidNet = NetChain[{
  LinearLayer[2, "Weights" -> {{1., -1.}, {-0.5, 0.5}}, "Biases" -> {0.5, -0.25}],
  LogisticSigmoid
}, "Input" -> 2];

softmaxNet = NetChain[{
  LinearLayer[3, "Weights" -> {{1., 0.}, {0., 1.}, {-1., -1.}}, "Biases" -> {0., 0., 0.}],
  SoftmaxLayer[]
}, "Input" -> 2];

convWeights = ArrayReshape[{1., 0., 0., 1., 0., 1., 1., 0.}, {2, 1, 2, 2}];
convNet = NetChain[{
  ConvolutionLayer["Weights" -> convWeights, "Biases" -> {0., 0.}]
}, "Input" -> {1, 3, 3}];
convInput = {{{1., 2., 3.}, {4., 5., 6.}, {7., 8., 9.}}};

conv1DNet = NetChain[{
  ConvolutionLayer["Weights" -> {{{1., 0., -1.}}, {{-1., 0., 1.}}}, "Biases" -> {0., 0.}]
}, "Input" -> {1, 6}];
conv1DInput = {{1., 2., 3., 4., 5., 6.}};

pool1DNet = NetChain[{
  PoolingLayer[2, 1]
}, "Input" -> {1, 6}];
pool1DInput = {{1., 3., 2., 5., 4., 6.}};

pool2DNet = NetChain[{
  PoolingLayer[{2, 2}, {2, 2}]
}, "Input" -> {1, 4, 4}];
pool2DInput = {{{1., 2., 3., 4.}, {5., 6., 7., 8.},
  {9., 10., 11., 12.}, {13., 14., 15., 16.}}};

softmaxLevel1Net = NetChain[{
  SoftmaxLayer[1]
}, "Input" -> {2, 2}];
softmaxLevel1Input = {{1., 2.}, {3., 4.}};

fixtures = {
  {"nn-linear.mat", linearNet, {1., 2.}},
  {"nn-ramp.mat", rampNet, {2., 1.}},
  {"nn-sigmoid.mat", sigmoidNet, {2., 1.}},
  {"nn-softmax.mat", softmaxNet, {1., 2.}, {2}},
  {"nn-conv2d.mat", convNet, convInput, {3, 3}},
  {"nn-conv1d.mat", conv1DNet, conv1DInput, {6}},
  {"nn-pool1d.mat", pool1DNet, pool1DInput, {6}},
  {"nn-pool2d.mat", pool2DNet, pool2DInput, {4, 4}},
  {"nn-softmax-level1.mat", softmaxLevel1Net, softmaxLevel1Input, {2, 2}}
};

Do[
  {fileName, net, input} = Take[fixture, 3];
  path = FileNameJoin[{testDataDirectory, fileName}];
  inputShape = If[Length[fixture] == 4, fixture[[4]], {2}];
  If[ExportNNMAT4[net, path, inputShape] === $Failed, Exit[1]];
  Print[fileName, " input ", InputForm[input], " output ", InputForm[N[net[input], 12]]];
  , {fixture, fixtures}
];
