unit panda.NN.Tests.PoolingLayer;

interface

uses
    TestFramework
  , panda.Intfs
  , panda.Arrays
  , panda.NN
  , panda.NN.PoolingLayer
  , panda.Tests.NDATestCase
  , System.SysUtils
  , System.Math
  ;

type
  TLowLvlTests = class(TNDATestCase)
  protected const
    stol = 1e-6;
  published
    procedure TestMaxPool1D_10_P2_S2;
    procedure TestMaxPool1D_16_P2_S2;
    procedure TestMaxPool1D_12_P3_S3;
    procedure TestMaxPool2D_4x4_P2_S2;
  end;

  TPoolingLayerTests = class(TNDATestCase)
  protected const
    stol = 1e-6;
  published
    procedure TestMaxPool2D_4x4_P2_S2;
    procedure TestMaxPool2D_4x4_P2_S2_Batch;
  end;

implementation

{$region 'TLowLvlTests'}

procedure TLowLvlTests.TestMaxPool1D_10_P2_S2;
const src: array [0..9] of Single = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
var dst: TArray<Single>;
begin
  SetLength(dst, GetOutputSize(Length(src), 2, 2));
  Init(PSingle(dst), Single.MinValue, Length(dst));

  _MaxPool1D(@src, PSingle(dst), Length(src), 2, 2);

  CheckEquals(1, dst[0], stol);
  CheckEquals(3, dst[1], stol);
  CheckEquals(5, dst[2], stol);
  CheckEquals(7, dst[3], stol);
  CheckEquals(9, dst[4], stol);
end;

procedure TLowLvlTests.TestMaxPool1D_16_P2_S2;
const src: array [0..15] of Single = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
var dst: TArray<Single>;
begin
  SetLength(dst, GetOutputSize(Length(src), 2, 2));
  _MaxPool1D(@src, PSingle(dst), Length(src), 2, 2);

  CheckEquals( 1, dst[0], stol);
  CheckEquals( 3, dst[1], stol);
  CheckEquals( 5, dst[2], stol);
  CheckEquals( 7, dst[3], stol);
  CheckEquals( 9, dst[4], stol);
  CheckEquals(11, dst[5], stol);
  CheckEquals(13, dst[6], stol);
  CheckEquals(15, dst[7], stol);
end;

procedure TLowLvlTests.TestMaxPool1D_12_P3_S3;
const src: array [0..11] of Single = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11);
var dst: TArray<Single>;
begin
  SetLength(dst, GetOutputSize(Length(src), 3, 3));
  _MaxPool1D(@src, PSingle(dst), Length(src), 3, 3);

  CheckEquals( 2, dst[0], stol);
  CheckEquals( 5, dst[1], stol);
  CheckEquals( 8, dst[2], stol);
  CheckEquals(11, dst[3], stol);
end;

procedure TLowLvlTests.TestMaxPool2D_4x4_P2_S2;
const src: array [0..3, 0..3] of Single = (
  ( 0,  1,  2,  3),
  ( 4,  5,  6,  7),
  ( 8,  9, 10, 11),
  (12, 13, 14, 15)
);
var dst: TArray<Single>;
begin
  SetLength(dst, 4);
  Init(PSingle(dst), Single.MinValue, Length(dst));

  _MaxPool2D(@src, PSingle(dst), 4, 4, 16, 8, 2, 2, 2, 2);

  CheckEquals(5,  dst[0], stol);
  CheckEquals(7,  dst[1], stol);
  CheckEquals(13, dst[2], stol);
  CheckEquals(15, dst[3], stol);
end;

{$endregion}

{$region 'TPoolingLayerTests'}

procedure TPoolingLayerTests.TestMaxPool2D_4x4_P2_S2;
var l: TPoolingLayer;
    src: INDArray<Single>;
    m: TArray<TArray<Single>>;
begin
  l := TPoolingLayer.Create;
  try
    l.PoolSize := TArray<NativeInt>.Create(2, 2);
    l.Initialize([4, 4]);

    src := TNDAUt.AsArray<Single>([
      [ 1,  2,  3,  4],
      [ 5,  6,  7,  8],
      [ 9, 10, 11, 12],
      [13, 14, 15, 16]
    ]);

    l.Execute(src);
    CheckEquals([2, 2], l.Output.Shape);
    CheckTrue(TNDAUt.TryAsDynArray2D<Single>(l.Output, m));
    CheckEquals([ 6,  8], m[0]);
    CheckEquals([14, 16], m[1]);
  finally
    l.Free;
  end;
end;

procedure TPoolingLayerTests.TestMaxPool2D_4x4_P2_S2_Batch;
var l: TPoolingLayer;
    src: INDArray<Single>;
    m: TArray<TArray<TArray<Single>>>;
begin
  l := TPoolingLayer.Create;
  try
    l.PoolSize := TArray<NativeInt>.Create(2, 2);
    l.Initialize([2, 4, 4]);

    src := TNDAUt.AsArray<Single>([
      [
        [ 1,  2,  3,  4],
        [ 5,  6,  7,  8],
        [ 9, 10, 11, 12],
        [13, 14, 15, 16]
      ],
      [
        [17, 18, 19, 20],
        [21, 22, 23, 24],
        [25, 26, 27, 28],
        [29, 30, 31, 32]
      ]
    ]);

    l.Execute(src);
    CheckEquals([2, 2, 2], l.Output.Shape);
    CheckTrue(TNDAUt.TryAsDynArray3D<Single>(l.Output, m));

    CheckEquals([ 6,  8], m[0, 0]);
    CheckEquals([14, 16], m[0, 1]);

    CheckEquals([22, 24], m[1, 0]);
    CheckEquals([30, 32], m[1, 1]);
  finally
    l.Free;
  end;
end;

{$endregion}

initialization

  RegisterTest(TLowLvlTests.Suite);
  RegisterTest(TPoolingLayerTests.Suite);

end.
