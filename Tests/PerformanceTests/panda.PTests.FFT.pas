unit panda.PTests.FFT;

interface

{$ifdef RELEASE}
  {$define OCV}
{$endif}

uses
    TestFramework
  , panda.Tests.NDATestCase
  , panda.Intfs
  , panda.Arrays
  , panda.Nums
  , panda.fft
  , System.Math
  , System.SysUtils
  , VUTS.Common.PFFFT
{$ifdef OCV}
  , ocv.imgproc_c
  , ocv.imgproc.types_c
  , ocv.core_c
  , ocv.core.types_c
{$endif}
  ;

type
  TFFT64Tests = class(TNDAPerformanceTestCase)
  published
    procedure CmplxFFT2_LargeData;
    procedure CmplxFFT2_MediumData;
    procedure CmplxFFT3_LargeData;
    procedure CmplxFFT5_LargeData;
    procedure CmplxFFT23_LargeData;
    procedure RealFFT2_LargeData;
    procedure RealFFT2_LargeData_HalfSpectrum;
    procedure RealFFT2D_1024x1024;
    procedure RealFFT2D_1024x768;
  {$ifdef OCV}
    procedure OcvRealFFT2_LargeData;
    procedure OcvCmplxFFT2_MediumData;
    procedure OcvRealFFT2D_1024x1024;
    procedure OcvRealFFT2D_1024x765;
  {$endif}
  end;

  TFFT32Tests = class(TNDAPerformanceTestCase)
  published
    procedure CmplxFFT1D_LargeData;
    procedure RealFFT2_LargeData_HalfSpectrum;
    procedure RealFFT1D_MediumData;
    procedure RealFFT2D_1024x1024;
    procedure RealFFT2D_1024x768;
    procedure RealIFFT2D_1024_1024;
  {$ifdef OCV}
    procedure OcvCmplxFFT1D_LargeData;
    procedure OcvRealFFT1D_LargeData;
    procedure OcvRealFFT1D_MediumData;
    procedure OcvRealFFT2D_1024x1024;
    procedure OcvRealFFT2D_1024x765;
  {$endif}
    procedure PFFFT_RealFFT_LargeData;
    procedure PFFFT_RealFFT2D_1024x768;
  end;

procedure TransposeBlocked(src, dst: PSingle; H, W: Integer);

implementation

procedure TransposeBlocked(src, dst: PSingle; H, W: Integer);
type
  TSingleArr = array[0..MaxInt div SizeOf(Single) - 1] of Single;
  PSingleArr = ^TSingleArr;
const
  B = 16;
var
  s, d: PSingleArr;
  i, j, bi, bj, iMax, jMax: Integer;
begin
  s := PSingleArr(src);
  d := PSingleArr(dst);
  bi := 0;
  while bi < H do begin
    bj := 0;
    while bj < W do begin
      iMax := bi + B; if iMax > H then iMax := H;
      jMax := bj + B; if jMax > W then jMax := W;
      for i := bi to iMax - 1 do
        for j := bj to jMax - 1 do
          d^[j * H + i] := s^[i * W + j];
      Inc(bj, B);
    end;
    Inc(bi, B);
  end;
end;

{$region 'TFFTTests'}

procedure TFFT64Tests.CmplxFFT2_LargeData;
var data: TArray<TCmplx128>;
    nda, res: INDArray<TCmplx128>;
    I, count: Integer;
    fft: TFFTEvalF64;
const cPwr = 18;
begin
  count := 1 shl cPwr;
  SetLength(data, count);
  for I := 0 to count - 1 do
    data[I] := I mod 100;
  nda := TDynArrWrapper<TCmplx128>.Create(data);
  res := TNDABuffer<TCmplx128>.Create([count]);

  fft := TFFTEvalF64.Create;
  try
    fft.RecursiveMethodThreshold := 2048;
    fft.Init(count);

    SWStart;
    DoTestLoop(procedure begin fft.Execute(nda, res); end, 50);
    SWStop(Format('2^%d samples', [cPwr]));
  finally
    fft.Free;
  end;
end;

procedure TFFT64Tests.CmplxFFT2_MediumData;
var data: TArray<TCmplx128>;
    nda, res: INDArray<TCmplx128>;
    I, count: Integer;
    fft: TFFTEvalF64;
const N = 1000;
begin
  count := 1 shl 10; // 2^10 = 1024
  SetLength(data, count);
  for I := 0 to count - 1 do
    data[I] := I mod 100;
  nda := TDynArrWrapper<TCmplx128>.Create(data);
  res := TNDABuffer<TCmplx128>.Create([count]);

  fft := TFFTEvalF64.Create;
  try
    fft.RecursiveMethodThreshold := 256;
    fft.Init(count);

    SWStart;
    for I := 0 to N do
      fft.Execute(nda, res);
    SWStop('1k data, 1k loop');
  finally
    fft.Free;
  end;
end;

procedure TFFT64Tests.CmplxFFT3_LargeData;
var data: TArray<TCmplx128>;
    nda, res: INDArray<TCmplx128>;
    I, count: Integer;
    fft: TFFTEvalF64;
const cPwr = 11;
begin
  count := Round(IntPower(3, cPwr));
  SetLength(data, count);
  for I := 0 to count - 1 do
    data[I] := I mod 100;
  nda := TDynArrWrapper<TCmplx128>.Create(data);
  res := TNDABuffer<TCmplx128>.Create([count]);

  fft := TFFTEvalF64.Create;
  try
    fft.Init(count);

    SWStart;
    DoTestLoop(procedure begin fft.Execute(nda, res); end, 50);
    SWStop(Format('3^%d samples', [cPwr]));
  finally
    fft.Free;
  end;
end;

procedure TFFT64Tests.CmplxFFT5_LargeData;
var data: TArray<TCmplx128>;
    nda, res: INDArray<TCmplx128>;
    I, count: Integer;
    fft: TFFTEvalF64;
const cPwr = 8;
begin
  count := Round(IntPower(5, cPwr));
  SetLength(data, count);
  for I := 0 to count - 1 do
    data[I] := I mod 100;
  nda := TDynArrWrapper<TCmplx128>.Create(data);
  res := TNDABuffer<TCmplx128>.Create([count]);

  fft := TFFTEvalF64.Create;
  try
    fft.Init(count);

    SWStart;
    DoTestLoop(procedure begin fft.Execute(nda, res); end, 50);
    SWStop(Format('5^%d samples', [cPwr]));
  finally
    fft.Free;
  end;
end;

procedure TFFT64Tests.CmplxFFT23_LargeData;
var data: TArray<TCmplx128>;
    nda, res: INDArray<TCmplx128>;
    I, count: Integer;
    fft: TFFTEvalF64;
const cPwr2 = 12;
      cPwr3 = 4;
begin
  count := (1 shl cPwr2) * Round(IntPower(3, cPwr3));
  SetLength(data, count);
  for I := 0 to count - 1 do
    data[I] := I mod 100;
  nda := TDynArrWrapper<TCmplx128>.Create(data);

  fft := TFFTEvalF64.Create;
  try
    fft.RecursiveMethodThreshold := 2048;
    fft.Init(count);

    SWStart;
    DoTestLoop(procedure begin fft.Execute(nda, res); end, 50);
    SWStop(Format('2^%d*3^%d samples', [cPwr2, cPwr3]));
  finally
    fft.Free;
  end;
end;

procedure TFFT64Tests.RealFFT2_LargeData;
var data: TArray<Double>;
    nda: INDArray<Double>;
    res: INDArray<TCmplx128>;
    I, count: Integer;
    fft: TRealFFTEvalF64;
const cPwr = 18;
begin
  count := 1 shl cPwr;
  SetLength(data, count);
  for I := 0 to count - 1 do
    data[I] := I mod 100;
  nda := TDynArrWrapper<Double>.Create(data);
  res := TNDABuffer<TCmplx128>.Create([count]);

  fft := TRealFFTEvalF64.Create;
  try
    fft.RecursiveMethodThreshold := 2048;
    fft.Init(count, True);

    SWStart;
    DoTestLoop(procedure begin fft.Execute(nda, res); end, 50);
    SWStop(Format('2^%d samples', [cPwr]));
  finally
    fft.Free;
  end;
end;

procedure TFFT64Tests.RealFFT2_LargeData_HalfSpectrum;
var data: TArray<Double>;
    nda: INDArray<Double>;
    res: INDArray<TCmplx128>;
    I, count: Integer;
    fft: TRealFFTEvalF64;
const cPwr = 18;
begin
  count := 1 shl cPwr;
  SetLength(data, count);
  for I := 0 to count - 1 do
    data[I] := I mod 100;
  nda := TDynArrWrapper<Double>.Create(data);
  res := TNDABuffer<TCmplx128>.Create([(count div 2) + 1]);

  fft := TRealFFTEvalF64.Create;
  try
    fft.RecursiveMethodThreshold := 2048;
    fft.Init(count, False);

    SWStart;
    DoTestLoop(procedure begin fft.Execute(nda, res); end, 50);
    SWStop(Format('2^%d samples', [cPwr]));
  finally
    fft.Free;
  end;
end;

procedure TFFT64Tests.RealFFT2D_1024x1024;
var fft: TRealFFTEval2DF64;
    src: INDArray<Double>;
    dst: INDArray<TCmplx128>;
const W = 1024;
      H = 1024;
begin
  src := TNDAUt.Table2D<Double>(
    function (X, Y: NativeInt): Double
    begin
      Result := (X mod 100) + (Y mod 50);
    end,
    0, W - 1, 0, H - 1
  );
  dst := TNDAUt.Empty<TCmplx128>([H, (W div 2) + 1]);

  fft := TRealFFTEval2DF64.Create;
  try
    fft.RecursiveMethodThreshold := 256;
    fft.Init(H, W);
    fft.Execute(src, dst);  // warm-up

    SWStart;
    DoTestLoop(procedure begin fft.Execute(src, dst); end, 50);
    SWStop(Format('%dx%d', [W, H]));
  finally
    fft.Free;
  end;
end;

procedure TFFT64Tests.RealFFT2D_1024x768;
var fft: TRealFFTEval2DF64;
    src: INDArray<Double>;
    dst: INDArray<TCmplx128>;
const W = 1024;
      H = 768;
begin
  src := TNDAUt.Table2D<Double>(
    function (X, Y: NativeInt): Double
    begin
      Result := (X mod 100) + (Y mod 50);
    end,
    0, W - 1, 0, H - 1
  );
  dst := TNDAUt.Empty<TCmplx128>([H, (W div 2) + 1]);

  fft := TRealFFTEval2DF64.Create;
  try
    fft.RecursiveMethodThreshold := 256;
    fft.Init(H, W);
    fft.Execute(src, dst);  // warm-up

    SWStart;
    DoTestLoop(procedure begin fft.Execute(src, dst); end, 50);
    SWStop(Format('%dx%d', [W, H]));
  finally
    fft.Free;
  end;
end;

{$ifdef OCV}
procedure TFFT64Tests.OcvRealFFT2_LargeData;
var src, dst: PCvMat;
    count: Integer;
const cPwr = 18;
begin
  count := 1 shl cPwr;
  src := cvCreateMat(1, count, CV_64FC1);
  dst := cvCreateMat(1, count, CV_64FC2);
  try
    cvDFT(src, dst, CV_DXT_FORWARD); // warm-up

    SWStart;
    DoTestLoop(procedure begin cvDFT(src, dst, CV_DXT_FORWARD); end, 50);
    SWStop(Format('2^%d samples', [cPwr]));
  finally
    cvReleaseMat(src);
    cvReleaseMat(dst);
  end;
end;

procedure TFFT64Tests.OcvCmplxFFT2_MediumData;
var src, dst: PCvMat;
    I: Integer;
const N = 1000;
      count = 1024;
begin
  src := cvCreateMat(1, count, CV_64FC2);
  dst := cvCreateMat(1, count, CV_64FC2);
  try
    cvDFT(src, dst, CV_DXT_FORWARD); // warm-up

    SWStart;
    for I := 0 to N do
      cvDFT(src, dst, CV_DXT_FORWARD);
    SWStop('1k data, 1k loop');
  finally
    cvReleaseMat(src);
    cvReleaseMat(dst);
  end;

  src := cvCreateMat(N, count, CV_32FC2);
  dst := cvCreateMat(N, count, CV_32FC2);
  try
    cvDFT(src, dst, CV_DXT_FORWARD or CV_DXT_ROWS); // warm-up

    SWStart;
    cvDFT(src, dst, CV_DXT_FORWARD or CV_DXT_ROWS);
    SWStop('1k data, 1k rows');
  finally
    cvReleaseMat(src);
    cvReleaseMat(dst);
  end;
end;

procedure TFFT64Tests.OcvRealFFT2D_1024x1024;
var src, dst: PCvMat;
const W = 1024;
      H = 1024;
begin
  src := cvCreateMat(H, W, CV_64FC1);
  dst := cvCreateMat(H, W, CV_64FC2);
  try
    cvDFT(src, dst, CV_DXT_FORWARD); // warm-up

    SWStart;
    DoTestLoop(procedure begin cvDFT(src, dst, CV_DXT_FORWARD); end, 50);
    SWStop(Format('%dx%d', [W, H]));
  finally
    cvReleaseMat(src);
    cvReleaseMat(dst);
  end;
end;

procedure TFFT64Tests.OcvRealFFT2D_1024x765;
var src, dst: PCvMat;
const W = 1024;
      H = 768;
begin
  src := cvCreateMat(H, W, CV_64FC1);
  dst := cvCreateMat(H, W, CV_64FC2);
  try
    cvDFT(src, dst, CV_DXT_FORWARD); // warm-up

    SWStart;
    DoTestLoop(procedure begin cvDFT(src, dst, CV_DXT_FORWARD); end, 50);
    SWStop(Format('%dx%d', [W, H]));
  finally
    cvReleaseMat(src);
    cvReleaseMat(dst);
  end;
end;
{$endif}

{$endregion}

{$region 'TFFT32Tests'}

procedure TFFT32Tests.CmplxFFT1D_LargeData;
var data: TArray<TCmplx64>;
    nda, res: INDArray<TCmplx64>;
    I, count: Integer;
    fft: TFFTEvalF32;
const cPwr = 18;
begin
  count := 1 shl cPwr;
  SetLength(data, count);
  for I := 0 to count - 1 do
    data[I] := I mod 100;
  nda := TDynArrWrapper<TCmplx64>.Create(data);
  res := TNDABuffer<TCmplx64>.Create([count]);

  fft := TFFTEvalF32.Create;
  try
    fft.RecursiveMethodThreshold := 512;//256//2048
    fft.Init(count);

    SWStart;
    DoTestLoop(procedure begin fft.Execute(nda, res); end, 50);
    SWStop(Format('2^%d samples', [cPwr]));
  finally
    fft.Free;
  end;
end;

procedure TFFT32Tests.RealFFT2_LargeData_HalfSpectrum;
var data: TArray<Single>;
    nda: INDArray<Single>;
    res: INDArray<TCmplx64>;
    I, count: Integer;
    fft: TRealFFTEvalF32;
const cPwr = 18;
begin
  count := 1 shl cPwr;
  SetLength(data, count);
  for I := 0 to count - 1 do
    data[I] := I mod 100;
  nda := TDynArrWrapper<Single>.Create(data);
  res := TNDABuffer<TCmplx64>.Create([(count div 2) + 1]);

  fft := TRealFFTEvalF32.Create;
  try
    fft.RecursiveMethodThreshold := 2048;
    fft.Init(count, False);

    SWStart;
    DoTestLoop(procedure begin fft.Execute(nda, res); end, 50);
    SWStop(Format('2^%d samples', [cPwr]));
  finally
    fft.Free;
  end;
end;

procedure TFFT32Tests.RealFFT1D_MediumData;
var data: TArray<Single>;
    nda: INDArray<Single>;
    res: INDArray<TCmplx64>;
    I, count: Integer;
    fft: TRealFFTEvalF32;
const N = 1000;
begin
  count := 1 shl 10; // 2^10 = 1024
  SetLength(data, count);
  for I := 0 to count - 1 do
    data[I] := I mod 100;
  nda := TDynArrWrapper<Single>.Create(data);
  res := TNDABuffer<TCmplx64>.Create([(count div 2) + 1]);

  fft := TRealFFTEvalF32.Create;
  try
    fft.RecursiveMethodThreshold := 256;
    fft.Init(count);

    SWStart;
    for I := 0 to N do
      fft.Execute(nda, res);
    SWStop('1k data, 1k loop');
  finally
    fft.Free;
  end;
end;

procedure TFFT32Tests.RealFFT2D_1024x1024;
var fft: TRealFFTEval2DF32;
    src: INDArray<Single>;
    dst: INDArray<TCmplx64>;
const W = 1024;
      H = 1024;
begin
  src := TNDAUt.Table2D<Single>(
    function (X, Y: NativeInt): Single
    begin
      Result := (X mod 100) + (Y mod 50);
    end,
    0, W - 1, 0, H - 1
  );
  dst := TNDAUt.Empty<TCmplx64>([H, (W div 2) + 1]);

  fft := TRealFFTEval2DF32.Create;
  try
//    fft.SpectrumLayout := slNative;
    fft.RecursiveMethodThreshold := 1024;
    fft.Init(H, W);
    fft.Execute(src, dst);  // warm-up

    SWStart;
    DoTestLoop(procedure begin fft.Execute(src, dst); end, 50);
    SWStop(Format('%dx%d', [W, H]));
  finally
    fft.Free;
  end;
end;

procedure TFFT32Tests.RealFFT2D_1024x768;
var fft: TRealFFTEval2DF32;
    src: INDArray<Single>;
    dst: INDArray<TCmplx64>;
const W = 1024;
      H = 768;
begin
  src := TNDAUt.Table2D<Single>(
    function (X, Y: NativeInt): Single
    begin
      Result := (X mod 100) + (Y mod 50);
    end,
    0, W - 1, 0, H - 1
  );
  dst := TNDAUt.Empty<TCmplx64>([H, (W div 2) + 1]);

  fft := TRealFFTEval2DF32.Create;
  try
    fft.RecursiveMethodThreshold := 1024;
    fft.Init(H, W);
    fft.Execute(src, dst);  // warm-up

    SWStart;
    DoTestLoop(procedure begin fft.Execute(src, dst); end, 50);
    SWStop(Format('%dx%d', [W, H]));
  finally
    fft.Free;
  end;
end;

procedure TFFT32Tests.RealIFFT2D_1024_1024;
var fft: TRealFFTEval2DF32;
    ifft: TRealIFFTEval2DF32;
    src, dst: INDArray<Single>;
    tmp: INDArray<TCmplx64>;
const W = 1024;
      H = 768;
begin
  src := TNDAUt.Table2D<Single>(
    function (X, Y: NativeInt): Single
    begin
      Result := (X mod 100) + (Y mod 50);
    end,
    0, W - 1, 0, H - 1
  );
  dst := TNDAUt.Empty<Single>([H, W]);

  fft := TRealFFTEval2DF32.Create;
  try
    fft.SpectrumLayout := slNative;
    fft.Init(H, W);
    fft.Execute(src, tmp);
  finally
    fft.Free;
  end;

  ifft := TRealiFFTEval2DF32.Create;
  try
    ifft.SpectrumLayout := slNative;
    ifft.Normalize := True;
    ifft.RecursiveMethodThreshold := 1024;
    ifft.Init(H, W);
    ifft.Execute(tmp, dst);

    SWStart;
    DoTestLoop(procedure begin ifft.Execute(tmp, dst); end, 50);
    SWStop(Format('%dx%d', [W, H]));
  finally
    ifft.Free;
  end;
end;


{$ifdef OCV}
procedure TFFT32Tests.OcvCmplxFFT1D_LargeData;
var src, dst: PCvMat;
    count: Integer;
const cPwr = 18;
begin
  count := 1 shl cPwr;
  src := cvCreateMat(1, count, CV_32FC2);
  dst := cvCreateMat(1, count, CV_32FC2);
  try
    cvDFT(src, dst, CV_DXT_FORWARD); // warm-up

    SWStart;
    DoTestLoop(procedure begin cvDFT(src, dst, CV_DXT_FORWARD); end, 50);
    SWStop(Format('2^%d samples', [cPwr]));
  finally
    cvReleaseMat(src);
    cvReleaseMat(dst);
  end;
end;

procedure TFFT32Tests.OcvRealFFT1D_LargeData;
var src, dst: PCvMat;
    count: Integer;
const cPwr = 18;
begin
  count := 1 shl cPwr;
  src := cvCreateMat(1, count, CV_32FC1);
  dst := cvCreateMat(1, count, CV_32FC2);
  try
    cvDFT(src, dst, CV_DXT_FORWARD); // warm-up

    SWStart;
    DoTestLoop(procedure begin cvDFT(src, dst, CV_DXT_FORWARD); end, 50);
    SWStop(Format('2^%d samples', [cPwr]));
  finally
    cvReleaseMat(src);
    cvReleaseMat(dst);
  end;
end;

procedure TFFT32Tests.OcvRealFFT1D_MediumData;
var src, dst: PCvMat;
    I, count: Integer;
const N = 1000;
begin
  count := 1 shl 10;
  src := cvCreateMat(1, count, CV_32FC1);
  dst := cvCreateMat(1, count, CV_32FC2);
  try
    cvDFT(src, dst, CV_DXT_FORWARD); // warm-up

    SWStart;
    for I := 0 to N do
      cvDFT(src, dst, CV_DXT_FORWARD);
    SWStop('1k data, 1k loop');
  finally
    cvReleaseMat(src);
    cvReleaseMat(dst);
  end;

  src := cvCreateMat(N, count, CV_32FC1);
  dst := cvCreateMat(N, count, CV_32FC2);
  try
    cvDFT(src, dst, CV_DXT_FORWARD or CV_DXT_ROWS); // warm-up

    SWStart;
    cvDFT(src, dst, CV_DXT_FORWARD or CV_DXT_ROWS);
    SWStop('1k data, 1k rows');
  finally
    cvReleaseMat(src);
    cvReleaseMat(dst);
  end;
end;

procedure TFFT32Tests.OcvRealFFT2D_1024x1024;
var src, dst: PCvMat;
const W = 1024;
      H = 1024;
begin
  src := cvCreateMat(H, W, CV_32FC1);
  dst := cvCreateMat(H, W, CV_32FC2);
  try
    cvDFT(src, dst, CV_DXT_FORWARD); // warm-up

    SWStart;
    DoTestLoop(procedure begin cvDFT(src, dst, CV_DXT_FORWARD); end, 50);
    SWStop(Format('%dx%d', [W, H]));
  finally
    cvReleaseMat(src);
    cvReleaseMat(dst);
  end;
end;

procedure TFFT32Tests.OcvRealFFT2D_1024x765;
var src, dst: PCvMat;
const W = 1024;
      H = 768;
begin
  src := cvCreateMat(H, W, CV_32FC1);
  dst := cvCreateMat(H, W, CV_32FC2);
  try
    cvDFT(src, dst, CV_DXT_FORWARD); // warm-up

    SWStart;
    DoTestLoop(procedure begin cvDFT(src, dst, CV_DXT_FORWARD); end, 50);
    SWStop(Format('%dx%d', [W, H]));
  finally
    cvReleaseMat(src);
    cvReleaseMat(dst);
  end;
end;
{$endif}

procedure TFFT32Tests.PFFFT_RealFFT_LargeData;
type
  TSingleArr = array[0..MaxInt div SizeOf(Single) - 1] of Single;
  PSingleArr = ^TSingleArr;
const
  cPwr = 18;
var
  setup: PFFFT_Setup;
  inp, outp, work: PSingle;
  arr: PSingleArr;
  i, count: Integer;
begin
  count := 1 shl cPwr;

  setup := pffft_new_setup(count, PFFFT_REAL);
  inp  := pffft_aligned_malloc(count * SizeOf(Single));
  outp := pffft_aligned_malloc(count * SizeOf(Single));
  work := pffft_aligned_malloc(count * SizeOf(Single));
  try
    arr := PSingleArr(inp);
    for i := 0 to count - 1 do
      arr^[i] := i mod 100;

    pffft_transform(setup, inp, outp, work, PFFFT_FORWARD);

    SWStart;
    DoTestLoop(procedure begin
      pffft_transform(setup, inp, outp, work, PFFFT_FORWARD);
    end, 50);
    SWStop(Format('2^%d samples (pffft)', [cPwr]));
  finally
    pffft_aligned_free(inp);
    pffft_aligned_free(outp);
    pffft_aligned_free(work);
    pffft_destroy_setup(setup);
  end;
end;

procedure TFFT32Tests.PFFFT_RealFFT2D_1024x768;
type
  TSingleArr = array[0..MaxInt div SizeOf(Single) - 1] of Single;
  PSingleArr = ^TSingleArr;
const
  W = 1024;
  H = 768;
var
  setupW, setupH: PFFFT_Setup;
  img, rowOut, colBuf, transp, work: PSingle;
  arr: PSingleArr;
  i: Integer;
begin
  setupW := pffft_new_setup(W, PFFFT_REAL);
  setupH := pffft_new_setup(H, PFFFT_REAL);


  img    := pffft_aligned_malloc(W * H * SizeOf(Single));
  rowOut := pffft_aligned_malloc(W * H * SizeOf(Single));
  transp := pffft_aligned_malloc(W * H * SizeOf(Single));
  colBuf := pffft_aligned_malloc(H * SizeOf(Single));
  work   := pffft_aligned_malloc(W * SizeOf(Single));
  try
    arr := PSingleArr(img);
    for i := 0 to W * H - 1 do
      arr^[i] := i mod 100;

    // warm-up
    pffft_transform(setupW, img, rowOut, work, PFFFT_FORWARD);

    SWStart;
    DoTestLoop(procedure
    var rr, cc: Integer;
    begin
      for rr := 0 to H - 1 do
        pffft_transform(setupW,
          PSingle(NativeUInt(img)    + NativeUInt(rr) * W * SizeOf(Single)),
          PSingle(NativeUInt(rowOut) + NativeUInt(rr) * W * SizeOf(Single)),
          work, PFFFT_FORWARD);

      TransposeBlocked(rowOut, transp, H, W);

      for cc := 0 to W - 1 do
        pffft_transform(setupH,
          PSingle(NativeUInt(transp) + NativeUInt(cc) * H * SizeOf(Single)),
          colBuf, work, PFFFT_FORWARD);
    end, 50);
    SWStop(Format('%dx%d (pffft, S TRANSPOZICI)', [W, H]));
  finally
    pffft_aligned_free(img);
    pffft_aligned_free(rowOut);
    pffft_aligned_free(transp);
    pffft_aligned_free(colBuf);
    pffft_aligned_free(work);
    pffft_destroy_setup(setupW);
    pffft_destroy_setup(setupH);
  end;
end;

{$endregion}

initialization

  RegisterTest(TFFT64Tests.Suite);
  RegisterTest(TFFT32Tests.Suite);

end.
