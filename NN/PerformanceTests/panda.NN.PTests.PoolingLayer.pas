unit panda.NN.PTests.PoolingLayer;

interface

uses
    TestFramework
  , panda.Tests.NDATestCase
  , panda.NN.PoolingLayer
  , panda.DynArrayUtils
  , panda.Nums
  , System.SysUtils
  , System.Math
  ;

type
  TPoolingLayerTests = class(TNDAPerformanceTestCase)
  published
    procedure MaxPoolingLayer1D_S2P2;
    procedure MaxPoolingLayer1D_S3P3;
    procedure MaxPoolingLayer2D_S2P2;

    procedure MaxPoolingLayer1D_S2P2_Pascal;
    procedure MaxPoolingLayer2D_S2P2_Pascal;
  end;

implementation

// suffix _S2P2 means: stride = 2, poolSize = 2
// these specialized function are intended for camparison with general functions

{$EXCESSPRECISION OFF} // to prevent Single -> Double conversion by x64 compiler

procedure _MaxPool1D_S2P2(pSrc, pDst: PSingle; aCount: NativeInt);
var dstCnt: NativeInt;
    pEnd: PByte;
    ma: Single;
begin
  dstCnt := GetOutputSize(aCount, 2, 2);
  pEnd := PByte(pDst) + dstCnt * cF32Sz;

  while PByte(pDst) < pEnd do begin
    ma := pDst^;
    if pSrc^ > ma then
      ma := pSrc^;
    Inc(pSrc);
    if pSrc^ > ma then
      ma := pSrc^;
    Inc(pSrc);
    pDst^ := ma;
    Inc(pDst);
  end;
end;

procedure _MaxPool2D_S2P2(pSrc, pDst: PSingle; aW, aH: NativeInt);
var I, dstW, dstH: NativeInt;
    p: PSingle;
begin
  dstW := GetOutputSize(aW, 2, 2);
  dstH := GetOutputSize(aH, 2, 2);

  for I := 0 to dstH - 1 do begin
    p := pSrc;
    _MaxPool1D_S2P2(p, pDst, aW);
    Inc(p, aW);
    _MaxPool1D_S2P2(p, pDst, aW);

    Inc(pSrc, 2*aW);
    Inc(pDst, dstW);
  end;
end;

procedure TPoolingLayerTests.MaxPoolingLayer1D_S2P2;
var src, dst: TArray<Single>;
const N = 1000000;
begin
  SetLength(src, N);
  dst := TDynAUt.ConstantArray<Single>(Single.MinValue, N div 2);

  SWStart;
  _MaxPool1D(PSingle(src), PSingle(dst), N, 2, 2);
  SWStop;
end;

procedure TPoolingLayerTests.MaxPoolingLayer1D_S3P3;
var src, dst: TArray<Single>;
const N = 1000000;
begin
  SetLength(src, N);
  dst := TDynAUt.ConstantArray<Single>(Single.MinValue, N div 3);

  SWStart;
  _MaxPool1D(PSingle(src), PSingle(dst), N, 3, 3);
  SWStop;
end;

procedure TPoolingLayerTests.MaxPoolingLayer2D_S2P2;
var src, dst: TArray<Single>;
const N = 1000;
begin
  SetLength(src, N*N);
  dst := TDynAUt.ConstantArray<Single>(Single.MinValue, (N*N) div 4);

  SWStart;
  _MaxPool2D(PSingle(src), PSingle(dst), N, N, N*cF32Sz, (N div 2)*cF32Sz, 2, 2, 2, 2);
  SWStop;
end;

procedure TPoolingLayerTests.MaxPoolingLayer1D_S2P2_Pascal;
var src, dst: TArray<Single>;
const N = 1000000;
begin
  SetLength(src, N);
  dst := TDynAUt.ConstantArray<Single>(Single.MinValue, N div 2);

  SWStart;
  _MaxPool1D_S2P2(PSingle(src), PSingle(dst), N);
  SWStop;
end;

procedure TPoolingLayerTests.MaxPoolingLayer2D_S2P2_Pascal;
var src, dst: TArray<Single>;
const N = 1000;
begin
  SetLength(src, N*N);
  dst := TDynAUt.ConstantArray<Single>(Single.MinValue, (N*N) div 4);

  SWStart;
  _MaxPool2D_S2P2(PSingle(src), PSingle(dst), N, N);
  SWStop;
end;


initialization

  RegisterTest(TPoolingLayerTests.Suite);

end.
