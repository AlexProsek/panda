unit panda.Tests.fft;

interface

uses
    TestFramework
  , Math
  , panda.Intfs
  , panda.Arrays
  , panda.Arithmetic
  , panda.fft
  , panda.Nums
  , panda.NumsMath
  , panda.Tests.NDATestCase
  ;

type
  TFFTLowLvlTests = class(TNDATestCase)
  protected const
    sTol = 1e-4;
    dTol = 1e-12;
  published
    procedure PowerOfTwo_0;
    procedure PowerOfTwo_4;
    procedure PowerOfTwo_12;

    procedure NearestLowerPwrOf2_64;
    procedure NearestLowerPwrOf2_100;

    procedure fft3_C64;
    procedure ifft3_C64;
    procedure fft4_C64;
    procedure ifft4_C64;
    procedure fft5_C64;
    procedure ifft5_C64;

    procedure fft3_C128;
    procedure ifft3_C128;
    procedure fft4_C128;
    procedure ifft4_C128;
    procedure fft5_C128;
    procedure ifft5_C128;
  end;

  TTestFFTEval = class(TFFTEvalF64)
  end;

  TFFT64Tests = class(TNDATestCase)
  protected const
    dtol = 1e-12;
    cWTol = 1e-14;
  protected
    fFFT: TTestFFTEval;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure WTable_3;
    procedure WTable_4;
    procedure WTable_5;
    procedure WTable_6;
    procedure WTable_7;
    procedure WTable_32;

    procedure InvWTable_3;
    procedure InvWTable_4;
    procedure InvWTable_5;
    procedure InvWTable_6;
    procedure InvWTable_7;
    procedure InvWTable_24;
    procedure InvWTable_32;

    procedure FFT_32;   // 2^5
    procedure FFT_8_r2Comb; // radix-2 combination
    procedure FFT_16r;
    procedure FFT_32r;  // r - recursive method
    procedure FFT_3;
    procedure FFT_27;   // 3^3
    procedure FFT_4;
    procedure FFT_16;
    procedure FFT_24;   // 3 x 2^3
    procedure FFT_36;   // 3^2 x 2^2
    procedure FFT_5;
    procedure FFT_25;   // 5^2

    procedure DLW_2;
    procedure DLW_4;
    procedure DLW_32;

    procedure RealFFT_8;
    procedure RealFFT_16;
    procedure RealFFT_20;
    procedure RealFFTHalfSpectrum;

    procedure RealFFT2D_4x5;
    procedure RealFFT2D_4x5_NatSpec; // native spectrum layout without a final transposition

    procedure InvDLW_4;
    procedure InvDLW_32;

    procedure InvFFT_16r;
    procedure InvFFT_3;
    procedure InvFFT_27;
    procedure InvFFT_4;

    procedure InvFFT_5;
    procedure InvFFT_25;   // 5^2

    procedure FwdInvFFT_24;
    procedure FwdInvRealFFT_24;

    procedure FwdInvRealFFT2D_4x5;
    procedure FwdInvRealFFT2D_4x6;
  end;

  TFFT32Tests = class(TNDATestCase)
  protected const
    stol = 1e-4;
    cWTol = 1e-6;
  protected
    fFFT: TFFTevalF32;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure WTable_32;

    procedure FFT_32;   // 2^5
    procedure FFT_8_r2Comb;
    procedure FFT_8_r4Comb;
    procedure FFT_16r;
    procedure FFT_32r;  // r - recursive method
    procedure FFT_3;
    procedure FFT_27;   // 3^3
    procedure FFT_4;
    procedure FFT_16;
    procedure FFT_24;   // 3 x 2^3
    procedure FFT_36;   // 3^2 x 2^2
    procedure FFT_5;
    procedure FFT_25;   // 5^2

    procedure DLW_2;
    procedure DLW_4;
    procedure DLW_8;
    procedure DLW_32;

    procedure RealFFT_8;
    procedure RealFFT_16;
    procedure RealFFT_20;
    procedure RealFFTHalfSpectrum;

    procedure RealFFT2D_4x5;
    procedure RealFFT2D_6x5;
    procedure RealFFT2D_6x8;

    procedure InvDLW_4;
    procedure InvDLW_32;

    procedure InvFFT_16r;
    procedure InvFFT_3;
    procedure InvFFT_27;
    procedure InvFFT_4;

    procedure InvFFT_5;
    procedure InvFFT_25;   // 5^2

    procedure FwdInvFFT_25;
    procedure FwdInvRealFFT_24;

    procedure FwdInvRealFFT2D_4x3;
    procedure FwdInvRealFFT2D_4x5;
    procedure FwdInvRealFFT2D_4x6;
  end;

implementation

function DirectDft(const aArr: TArray<TCmplx128>; aSgn: Integer = 1): TArray<TCmplx128>; overload;
var I, K, N: Integer;
    c: TCmplx128;
    w: TArray<TCmplx128>;
begin
  Assert(Abs(aSgn) = 1);

  N := Length(aArr);
  SetLength(Result, N);
  SetLength(w, N);
  for I := 0 to N - 1 do
    with w[I] do
      SinCos(2*Pi*aSgn*I/N, Im, Re);

  for K := 0 to N - 1 do begin
    c.Init(0, 0);
    for I := 0 to N - 1 do
      c := c + w[K * I mod N] * aArr[I];
    Result[K] := c;
  end;
end;

function DirectDft(const aArr: TArray<TCmplx64>; aSgn: Integer = 1): TArray<TCmplx64>; overload;
var I, K, N: Integer;
    c: TCmplx64;
    w: TArray<TCmplx64>;
begin
  Assert(Abs(aSgn) = 1);

  N := Length(aArr);
  SetLength(Result, N);
  SetLength(w, N);
  for I := 0 to N - 1 do
    with w[I] do
      SinCos(2*Pi*aSgn*I/N, Im, Re);

  for K := 0 to N - 1 do begin
    c.Init(0, 0);
    for I := 0 to N - 1 do
      c := c + w[K * I mod N] * aArr[I];
    Result[K] := c;
  end;
end;

function DirectDft2D(const aArr: TArray<TArray<TCmplx128>>): TArray<TArray<TCmplx128>>; overload;
var I, J, K, L, M, N: Integer;
    c: TCmplx128;
    w: TArray<TArray<TCmplx128>>;
begin
  M := Length(aArr);
  N := Length(aArr[0]);
  SetLength(w, M, N);
  for I := 0 to M - 1 do
    for J := 0 to N - 1 do
      with w[I, J] do
        SinCos(2*Pi*(I/M + J/N), Im, Re);

  SetLength(Result, M, N);
  for I := 0 to M - 1 do
    for J := 0 to N - 1 do begin
      c.Init(0, 0);
      for K := 0 to M - 1 do
        for L := 0 to N - 1 do
          c := c + w[I*K mod M, J*L mod N] * aArr[K, L];
      Result[I, J] := c;
    end;
end;

function DirectDft2D(const aArr: TArray<TArray<TCmplx64>>): TArray<TArray<TCmplx64>>; overload;
var I, J, K, L, M, N: Integer;
    c: TCmplx64;
    w: TArray<TArray<TCmplx64>>;
begin
  M := Length(aArr);
  N := Length(aArr[0]);
  SetLength(w, M, N);
  for I := 0 to M - 1 do
    for J := 0 to N - 1 do
      with w[I, J] do
        SinCos(2*Pi*(I/M + J/N), Im, Re);

  SetLength(Result, M, N);
  for I := 0 to M - 1 do
    for J := 0 to N - 1 do begin
      c.Init(0, 0);
      for K := 0 to M - 1 do
        for L := 0 to N - 1 do
          c := c + w[I*K mod M, J*L mod N] * aArr[K, L];
      Result[I, J] := c;
    end;
end;

function ToCmplx128(const aArr: array of Double): TArray<TCmplx128>; overload;
var I: Integer;
begin
  SetLength(Result, Length(aArr));
  for I := 0 to High(Result) do
    Result[I] := aArr[I];
end;

function ToCmplx64(const aArr: array of Single): TArray<TCmplx64>; overload;
var I: Integer;
begin
  SetLength(Result, Length(aArr));
  for I := 0 to High(Result) do
    Result[I] := aArr[I];
end;

function ToCmplx(const aArr: TArray<TArray<Double>>): TArray<TArray<TCmplx128>>; overload;
var I, J, M, N: Integer;
begin
  M := Length(aArr);
  N := Length(aArr[0]);
  SetLength(Result, M, N);
  for I := 0 to M - 1 do
    for J := 0 to N - 1 do
      Result[I, J] := aArr[I, J];
end;

function ToCmplx64(const aArr: TArray<TArray<Single>>): TArray<TArray<TCmplx64>>; overload;
var I, J, M, N: Integer;
begin
  M := Length(aArr);
  N := Length(aArr[0]);
  SetLength(Result, M, N);
  for I := 0 to M - 1 do
    for J := 0 to N - 1 do
      Result[I, J] := aArr[I, J];
end;

function WTable(N: Integer): TArray<TCmplx128>;
var I: Integer;
begin
  SetLength(Result, N);
  for I := 0 to N - 1 do
    with Result[I] do
      SinCos(2*Pi*I/N, Im, Re);
end;

{$region 'TFFTLowLvlTests'}

procedure TFFTLowLvlTests.PowerOfTwo_0;
begin
  CheckFalse(PowerOfTwoQ(0));
end;

procedure TFFTLowLvlTests.PowerOfTwo_4;
begin
  CheckTrue(PowerOfTwoQ(4));
end;

procedure TFFTLowLvlTests.PowerOfTwo_12;
begin
  CheckFalse(PowerOfTwoQ(12))
end;

procedure TFFTLowLvlTests.NearestLowerPwrOf2_64;
begin
  CheckEquals(64, NearestLowerPowerOfTwo(64));
end;

procedure TFFTLowLvlTests.NearestLowerPwrOf2_100;
begin
  CheckEquals(64, NearestLowerPowerOfTwo(100));
end;

procedure TFFTLowLvlTests.fft3_C64;
var data, exp: TArray<TCmplx64>;
    datav: TVecC64;
    I: Integer;
const N = 3;
begin
  data := TArray<TCmplx64>.Create(Cmplx64(1,2), Cmplx64(2,3), Cmplx64(3,1));
  datav.Init(PByte(data), N);
  exp := DirectDft(data);

  _fft3(datav);

  for I := 0 to N - 1 do
    CheckEquals(exp[I], data[I], sTol);
end;

procedure TFFTLowLvlTests.ifft3_C64;
var data, exp: TArray<TCmplx64>;
    datav: TVecC64;
    I: Integer;
const N = 3;
begin
  data := TArray<TCmplx64>.Create(Cmplx64(1,2), Cmplx64(2,3), Cmplx64(3,1));
  datav.Init(PByte(data), N);
  exp := DirectDft(data, -1);

  _ifft3(datav);

  for I := 0 to N - 1 do
    CheckEquals(exp[I], data[I], sTol);
end;

procedure TFFTLowLvlTests.fft4_C64;
var data, exp: TArray<TCmplx64>;
    datav: TVecC64;
    I: Integer;
const N = 4;
begin
  data := TArray<TCmplx64>.Create(Cmplx64(1,2), Cmplx64(2,3), Cmplx64(3,1), Cmplx64(2,1));
  datav.Init(PByte(data), N);
  exp := DirectDft(data);

  _fft4(datav);

  for I := 0 to N - 1 do
    CheckEquals(exp[I], data[I], sTol);
end;

procedure TFFTLowLvlTests.ifft4_C64;
var data, exp: TArray<TCmplx64>;
    datav: TVecC64;
    I: Integer;
const N = 4;
begin
  data := TArray<TCmplx64>.Create(Cmplx64(1,2), Cmplx64(2,3), Cmplx64(3,1), Cmplx64(2,1));
  datav.Init(PByte(data), N);
  exp := DirectDft(data, -1);

  _ifft4(datav);

  for I := 0 to N - 1 do
    CheckEquals(exp[I], data[I], sTol);
end;

procedure TFFTLowLvlTests.fft5_C64;
var data, exp: TArray<TCmplx64>;
    datav: TVecC64;
    I: Integer;
const N = 5;
begin
  data := TArray<TCmplx64>.Create(
    Cmplx64(1,2), Cmplx64(2,3), Cmplx64(3,1), Cmplx64(2,2), Cmplx64(1, 2)
  );
  datav.Init(PByte(data), N);
  exp := DirectDft(data);

  _fft5(datav);

  for I := 0 to N - 1 do
    CheckEquals(exp[I], data[I], sTol);
end;

procedure TFFTLowLvlTests.ifft5_C64;
var data, exp: TArray<TCmplx64>;
    datav: TVecC64;
    I: Integer;
const N = 5;
begin
  data := TArray<TCmplx64>.Create(
    Cmplx64(1,2), Cmplx64(2,3), Cmplx64(3,1), Cmplx64(2,2), Cmplx64(1, 2)
  );
  datav.Init(PByte(data), N);
  exp := DirectDft(data, -1);

  _ifft5(datav);

  for I := 0 to N - 1 do
    CheckEquals(exp[I], data[I], sTol);
end;

procedure TFFTLowLvlTests.fft3_C128;
var data, exp: TArray<TCmplx128>;
    datav: TVecC128;
    I: Integer;
const N = 3;
begin
  data := TArray<TCmplx128>.Create(Cmplx128(1,2), Cmplx128(2,3), Cmplx128(3,1));
  datav.Init(PByte(data), N);
  exp := DirectDft(data);

  _fft3(datav);

  for I := 0 to N - 1 do
    CheckEquals(exp[I], data[I], dTol);
end;

procedure TFFTLowLvlTests.ifft3_C128;
var data, exp: TArray<TCmplx128>;
    datav: TVecC128;
    I: Integer;
const N = 3;
begin
  data := TArray<TCmplx128>.Create(Cmplx128(1,2), Cmplx128(2,3), Cmplx128(3,1));
  datav.Init(PByte(data), N);
  exp := DirectDft(data, -1);

  _ifft3(datav);

  for I := 0 to N - 1 do
    CheckEquals(exp[I], data[I], dTol);
end;

procedure TFFTLowLvlTests.fft4_C128;
var data, exp: TArray<TCmplx128>;
    datav: TVecC128;
    I: Integer;
const N = 4;
begin
  data := TArray<TCmplx128>.Create(Cmplx128(1,2), Cmplx128(2,3), Cmplx128(3,1), Cmplx128(2,1));
  datav.Init(PByte(data), N);
  exp := DirectDft(data);

  _fft4(datav);

  for I := 0 to N - 1 do
    CheckEquals(exp[I], data[I], dTol);
end;

procedure TFFTLowLvlTests.ifft4_C128;
var data, exp: TArray<TCmplx128>;
    datav: TVecC128;
    I: Integer;
const N = 4;
begin
  data := TArray<TCmplx128>.Create(Cmplx128(1,2), Cmplx128(2,3), Cmplx128(3,1), Cmplx128(2,1));
  datav.Init(PByte(data), N);
  exp := DirectDft(data, -1);

  _ifft4(datav);

  for I := 0 to N - 1 do
    CheckEquals(exp[I], data[I], dTol);
end;

procedure TFFTLowLvlTests.fft5_C128;
var data, exp: TArray<TCmplx128>;
    datav: TVecC128;
    I: Integer;
const N = 5;
begin
  data := TArray<TCmplx128>.Create(
    Cmplx128(1,2), Cmplx128(2,3), Cmplx128(3,1), Cmplx128(2,2), Cmplx128(1, 2)
  );
  datav.Init(PByte(data), N);
  exp := DirectDft(data);

  _fft5(datav);

  for I := 0 to N - 1 do
    CheckEquals(exp[I], data[I], dTol);
end;

procedure TFFTLowLvlTests.ifft5_C128;
var data, exp: TArray<TCmplx128>;
    datav: TVecC128;
    I: Integer;
const N = 5;
begin
  data := TArray<TCmplx128>.Create(
    Cmplx128(1,2), Cmplx128(2,3), Cmplx128(3,1), Cmplx128(2,2), Cmplx128(1, 2)
  );
  datav.Init(PByte(data), N);
  exp := DirectDft(data, -1);

  _ifft5(datav);

  for I := 0 to N - 1 do
    CheckEquals(exp[I], data[I], dTol);
end;

{$endregion}

{$region 'TFFT64Tests'}

procedure GetW(const aW: TArray<TCmplx128>; aSgn: Integer = 1); overload;
var wv: TVecC128;
begin
  wv.Init(PByte(aW), Length(aW));
  EvalTwiddleFactors(wv, aSgn);
end;

procedure TFFT64Tests.WTable_3;
var w, res: TArray<TCmplx128>;
    I: Integer;
const N = 3;
begin
  w := WTable(N);

  SetLength(res, N);
  GetW(res);

  for I := 0 to High(res) do
    CheckEquals(w[I], res[I], cWTol);
end;

procedure TFFT64Tests.WTable_4;
var w, res: TArray<TCmplx128>;
    I: Integer;
const N = 4;
begin
  w := WTable(N);

  SetLength(res, N);
  GetW(res);

  for I := 0 to High(res) do
    CheckEquals(w[I], res[I], cWTol);
end;

procedure TFFT64Tests.WTable_5;
var w, res: TArray<TCmplx128>;
    I: Integer;
const N = 5;
begin
  w := WTable(N);

  SetLength(res, N);
  GetW(res);

  for I := 0 to High(res) do
    CheckEquals(w[I], res[I], cWTol);
end;

procedure TFFT64Tests.WTable_6;
var w, res: TArray<TCmplx128>;
    I: Integer;
const N = 6;
begin
  w := WTable(N);

  SetLength(res, N);
  GetW(res);

  for I := 0 to High(res) do
    CheckEquals(w[I], res[I], cWTol);
end;

procedure TFFT64Tests.WTable_7;
var w, res: TArray<TCmplx128>;
    I: Integer;
const N = 7;
begin
  w := WTable(N);

  SetLength(res, N);
  GetW(res);

  for I := 0 to High(res) do
    CheckEquals(w[I], res[I], cWTol);
end;

procedure TFFT64Tests.WTable_32;
var w, res: TArray<TCmplx128>;
    I: Integer;
const N = 32;
begin
  w := WTable(N);

  SetLength(res, N);
  GetW(res);

  for I := 0 to High(res) do
    CheckEquals(w[I], res[I], cWTol);
end;

procedure TFFT64Tests.InvWTable_3;
var w, res: TArray<TCmplx128>;
    I: Integer;
const N = 3;
begin
  w := WTable(N);

  SetLength(res, N);
  GetW(res, -1);

  for I := 0 to High(res) do
    CheckEquals(w[I].Conjugate, res[I], cWTol);
end;

procedure TFFT64Tests.InvWTable_4;
var w, res: TArray<TCmplx128>;
    I: Integer;
const N = 4;
begin
  w := WTable(N);

  SetLength(res, N);
  GetW(res, -1);

  for I := 0 to High(res) do
    CheckEquals(w[I].Conjugate, res[I], cWTol);
end;

procedure TFFT64Tests.InvWTable_5;
var w, res: TArray<TCmplx128>;
    I: Integer;
const N = 5;
begin
  w := WTable(N);

  SetLength(res, N);
  GetW(res, -1);

  for I := 0 to High(res) do
    CheckEquals(w[I].Conjugate, res[I], cWTol);
end;

procedure TFFT64Tests.InvWTable_6;
var w, res: TArray<TCmplx128>;
    I: Integer;
const N = 6;
begin
  w := WTable(N);

  SetLength(res, N);
  GetW(res, -1);

  for I := 0 to High(res) do
    CheckEquals(w[I].Conjugate, res[I], cWTol);
end;

procedure TFFT64Tests.InvWTable_7;
var w, res: TArray<TCmplx128>;
    I: Integer;
const N = 7;
begin
  w := WTable(N);

  SetLength(res, N);
  GetW(res, -1);

  for I := 0 to High(res) do
    CheckEquals(w[I].Conjugate, res[I], cWTol);
end;

procedure TFFT64Tests.InvWTable_24;
var w, res: TArray<TCmplx128>;
    I: Integer;
const N = 24;
begin
  w := WTable(N);

  SetLength(res, N);
  GetW(res, -1);

  for I := 0 to High(res) do
    CheckEquals(w[I].Conjugate, res[I], cWTol);
end;

procedure TFFT64Tests.InvWTable_32;
var w, res: TArray<TCmplx128>;
    I: Integer;
const N = 32;
begin
  w := WTable(N);

  SetLength(res, N);
  GetW(res, -1);

  for I := 0 to High(res) do
    CheckEquals(w[I].Conjugate, res[I], cWTol);
end;

procedure TFFT64Tests.SetUp;
begin
  inherited;
  fFFT := TTestFFTEval.Create;
end;

procedure TFFT64Tests.TearDown;
begin
  inherited;
  fFFT.Free;
end;

procedure TFFT64Tests.FFT_32;
var data, exp: TArray<TCmplx128>;
    a, res: INDArray<TCmplx128>;
    I: Integer;
begin
  data := ToCmplx128([
    1, 2, 3, 4, 3, 2, 1, 0,
    0, 1, 2, 3, 4, 3, 2, 1,
    0, 0, 1, 2, 3, 4, 3, 2,
    1, 0, 1, 1, 2, 3, 4, 3
  ]);
  exp := DirectDft(data);
  a := TDynArrWrapper<TCmplx128>.Create(data);

  fFFT.Init(Length(data));
  fFFT.Execute(a, res);

  TNDAUt.TryAsDynArray<TCmplx128>(res, data);
  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], dTol);
end;

procedure TFFT64Tests.FFT_8_r2Comb;
var data, exp: TArray<TCmplx128>;
    a, res: INDArray<TCmplx128>;
    I: Integer;
begin
  data := ToCmplx128([1, 2, 3, 4, 3, 2, 1, 0]);
  exp := DirectDft(data);
  a := TDynArrWrapper<TCmplx128>.Create(data);

  fFFT.RecursiveMethodThreshold := 4; // to supress inplace method
  fFFT.Init(Length(data));
  fFFT.Execute(a, res);

  TNDAUt.TryAsDynArray<TCmplx128>(res, data);
  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], dTol);
end;

procedure TFFT64Tests.FFT_16r;
var data, exp: TArray<TCmplx128>;
    a, res: INDArray<TCmplx128>;
    I: Integer;
begin
  data := ToCmplx128([
    1, 2, 3, 4, 3, 2, 1, 0,
    0, 1, 2, 3, 4, 3, 2, 0
  ]);
  exp := DirectDft(data);
  a := TDynArrWrapper<TCmplx128>.Create(data);

  fFFT.RecursiveMethodThreshold := 0; // to supress inplace method
  fFFT.Init(Length(data));
  fFFT.Execute(a, res);

  TNDAUt.TryAsDynArray<TCmplx128>(res, data);
  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], dTol);
end;

procedure TFFT64Tests.FFT_32r;
var data, exp: TArray<TCmplx128>;
    a, res: INDArray<TCmplx128>;
    I: Integer;
begin
  data := ToCmplx128([
    1, 2, 3, 4, 3, 2, 1, 0,
    0, 1, 2, 3, 4, 3, 2, 1,
    0, 0, 1, 2, 3, 4, 3, 2,
    1, 0, 1, 1, 2, 3, 4, 3
  ]);
  exp := DirectDft(data);
  a := TDynArrWrapper<TCmplx128>.Create(data);

  fFFT.RecursiveMethodThreshold := 0; // to supress inplace method
  fFFT.Init(Length(data));
  fFFT.Execute(a, res);

  TNDAUt.TryAsDynArray<TCmplx128>(res, data);
  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], dTol);
end;

procedure TFFT64Tests.FFT_3;
var data, exp: TArray<TCmplx128>;
    a, res: INDArray<TCmplx128>;
    I: Integer;
begin
  data := ToCmplx128([1, 2, 3]);
  exp := DirectDft(data);
  a := TDynArrWrapper<TCmplx128>.Create(data);

  fFFT.Init(Length(data));
  fFFT.Execute(a, res);

  TNDAUt.TryAsDynArray<TCmplx128>(res, data);
  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], dTol);
end;

procedure TFFT64Tests.FFT_27;
var data, exp: TArray<TCmplx128>;
    a, res: INDArray<TCmplx128>;
    I: Integer;
begin
  data := ToCmplx128([
    1, 2, 3, 4, 5, 4, 3, 2, 1, 0,
    1, 2, 3, 4, 5, 4, 3, 2, 1, 0,
    2, 3, 3, 4, 5, 2, 1
  ]);
  exp := DirectDft(data);
  a := TDynArrWrapper<TCmplx128>.Create(data);

  fFFT.Init(Length(data));
  fFFT.Execute(a, res);

  TNDAUt.TryAsDynArray<TCmplx128>(res, data);
  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], dTol);
end;

procedure TFFT64Tests.FFT_4;
 var data, exp: TArray<TCmplx128>;
    a, res: INDArray<TCmplx128>;
    I: Integer;
begin
  data := ToCmplx128([1, 2, 3, 1]);
  exp := DirectDft(data);
  a := TDynArrWrapper<TCmplx128>.Create(data);

  fFFT.RecursiveMethodThreshold := 0; // to supress inplace method
  fFFT.Init(Length(data));
  fFFT.Execute(a, res);

  TNDAUt.TryAsDynArray<TCmplx128>(res, data);
  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], dTol);
end;

procedure TFFT64Tests.FFT_16;
 var data, exp: TArray<TCmplx128>;
    a, res: INDArray<TCmplx128>;
    I: Integer;
begin
  data := ToCmplx128([
    1, 2, 3, 2, 1, 0, 1, 2,
    3, 2, 1, 0, 1, 1, 2, 3
  ]);
  exp := DirectDft(data);
  a := TDynArrWrapper<TCmplx128>.Create(data);

  fFFT.RecursiveMethodThreshold := 0; // to supress inplace method
  fFFT.Init(Length(data));
  fFFT.Execute(a, res);

  TNDAUt.TryAsDynArray<TCmplx128>(res, data);
  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], dTol);
end;

procedure TFFT64Tests.FFT_24;
var data, exp: TArray<TCmplx128>;
    a, res: INDArray<TCmplx128>;
    I: Integer;
begin
  data := ToCmplx128([
    1, 2, 3, 4, 3, 2, 1, 0,
    0, 1, 2, 3, 4, 3, 2, 1,
    0, 0, 1, 3, 3, 4, 4, 3
  ]);
  exp := DirectDft(data);
  a := TDynArrWrapper<TCmplx128>.Create(data);

  fFFT.Init(Length(data));
  fFFT.Execute(a, res);

  TNDAUt.TryAsDynArray<TCmplx128>(res, data);
  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], dTol);
end;

procedure TFFT64Tests.FFT_36;
var data, exp: TArray<TCmplx128>;
    a, res: INDArray<TCmplx128>;
    I: Integer;
begin
  data := ToCmplx128([
    1, 2, 3, 4, 3, 2, 1, 0,
    0, 1, 2, 3, 4, 3, 2, 1,
    0, 0, 1, 2, 3, 4, 3, 2,
    1, 0, 1, 1, 2, 3, 4, 3,
    3, 2, 2, 1
  ]);
  exp := DirectDft(data);
  a := TDynArrWrapper<TCmplx128>.Create(data);

  fFFT.Init(Length(data));
  fFFT.Execute(a, res);

  TNDAUt.TryAsDynArray<TCmplx128>(res, data);
  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], dTol);
end;

procedure TFFT64Tests.FFT_5;
var data, exp: TArray<TCmplx128>;
    a, res: INDArray<TCmplx128>;
    I: Integer;
begin
  data := ToCmplx128([1, 3, 1, 2, 2]);
  exp := DirectDft(data);
  a := TDynArrWrapper<TCmplx128>.Create(data);

  fFFT.Init(Length(data));
  fFFT.Execute(a, res);

  TNDAUt.TryAsDynArray<TCmplx128>(res, data);
  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], dTol);
end;

procedure TFFT64Tests.FFT_25;
var data, exp: TArray<TCmplx128>;
    a, res: INDArray<TCmplx128>;
    I: Integer;
begin
  data := ToCmplx128([
    1, 2, 3, 2, 1, 0, 1, 2, 3, 2,
    1, 0, 1, 2, 3, 2, 1, 0, 1, 2,
    2, 3, 3, 2, 1
  ]);
  exp := DirectDft(data);
  a := TDynArrWrapper<TCmplx128>.Create(data);

  fFFT.Init(Length(data));
  fFFT.Execute(a, res);

  TNDAUt.TryAsDynArray<TCmplx128>(res, data);
  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], dTol);
end;

procedure TFFT64Tests.DLW_2;
var data, exp: TArray<TCmplx128>;
    w: TArray<TCmplx128>;
    datav, wv: TVecC128;
    I: Integer;
begin
  data := ToCmplx128([1, 2]);
  exp := DirectDft(data);
  w := WTable(Length(data));

  datav.Init(@data[0], Length(data));
  wv.Init(@w[0], Length(w));
  _perm(BRPIndices(Length(w)), datav);
  DLW(datav, wv);

  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], dTol);
end;

procedure TFFT64Tests.DLW_4;
var data, exp: TArray<TCmplx128>;
    w: TArray<TCmplx128>;
    datav, wv: TVecC128;
    I: Integer;
begin
  data := ToCmplx128([1, 2, 3, 4]);
  exp := DirectDft(data);
  w := WTable(Length(data));

  datav.Init(@data[0], Length(data));
  wv.Init(@w[0], Length(w));
  _perm(BRPIndices(Length(w)), datav);
  DLW(datav, wv);

  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], dTol);
end;

procedure TFFT64Tests.DLW_32;
var data, exp: TArray<TCmplx128>;
    w: TArray<TCmplx128>;
    datav, wv: TVecC128;
    I: Integer;
begin
  data := ToCmplx128([
    1, 2, 3, 4, 3, 2, 1, 0,
    0, 1, 2, 3, 4, 3, 2, 1,
    0, 0, 1, 2, 3, 4, 3, 2,
    1, 0, 1, 1, 2, 3, 4, 3
  ]);
  exp := DirectDft(data);
  w := WTable(Length(data));

  datav.Init(@data[0], Length(data));
  wv.Init(@w[0], Length(w));
  _perm(BRPIndices(Length(w)), datav);
  DLW(datav, wv);

  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], dTol);
end;

procedure TFFT64Tests.RealFFT_8;
var data: TArray<Double>;
    exp, resArr: TArray<TCmplx128>;
    a: INDArray<Double>;
    res: INDArray<TCmplx128>;
    fft: TRealFFTEvalF64;
    I: Integer;
begin
  data := TArray<Double>.Create(1, 2, 3, 2, 1, 0, 1, 1);
  exp := DirectDft(ToCmplx128(data));
  a := TDynArrWrapper<Double>.Create(data);

  fft := TRealFFTEvalF64.Create;
  try
    fft.Init(Length(data), True);
    fft.Execute(a, res);
  finally
    fft.Free;
  end;

  TNDAUt.TryAsDynArray<TCmplx128>(res, resArr);
  CheckEquals(Length(exp), Length(resArr));
  for I := 0 to High(resArr) do
    CheckEquals(exp[I], resArr[I], dTol);
end;

procedure TFFT64Tests.RealFFT_16;
var data: TArray<Double>;
    exp, resArr: TArray<TCmplx128>;
    a: INDArray<Double>;
    res: INDArray<TCmplx128>;
    fft: TRealFFTEvalF64;
    I: Integer;
begin
  data := TArray<Double>.Create(1, 1, 2, 2, 1, 1, 0, 0, 0, 1, 1, 2, 2, 1, 1, 1);
  exp := DirectDft(ToCmplx128(data));
  a := TDynArrWrapper<Double>.Create(data);

  fft := TRealFFTEvalF64.Create;
  try
    fft.Init(Length(data), True);
    fft.Execute(a, res);
  finally
    fft.Free;
  end;

  TNDAUt.TryAsDynArray<TCmplx128>(res, resArr);
  CheckEquals(Length(exp), Length(resArr));
  for I := 0 to High(resArr) do
    CheckEquals(exp[I], resArr[I], dTol);
end;

procedure TFFT64Tests.RealFFT_20;
var data: TArray<Double>;
    exp, resArr: TArray<TCmplx128>;
    a: INDArray<Double>;
    res: INDArray<TCmplx128>;
    fft: TRealFFTEvalF64;
    I: Integer;
begin
  data := TArray<Double>.Create(
    1, 2, 3, 2, 1, 0, 1, 2, 3, 2,
    1, 0, 1, 2, 2, 2, 1, 0, 1, 1
  );
  exp := DirectDft(ToCmplx128(data));
  a := TDynArrWrapper<Double>.Create(data);

  fft := TRealFFTEvalF64.Create;
  try
    fft.Init(Length(data), True);
    fft.Execute(a, res);
  finally
    fft.Free;
  end;

  TNDAUt.TryAsDynArray<TCmplx128>(res, resArr);
  CheckEquals(Length(exp), Length(resArr));
  for I := 0 to High(resArr) do
    CheckEquals(exp[I], resArr[I], dTol);
end;

procedure TFFT64Tests.RealFFTHalfSpectrum;
var data: TArray<Double>;
    exp, resArr: TArray<TCmplx128>;
    a: INDArray<Double>;
    res: INDArray<TCmplx128>;
    fft: TRealFFTEvalF64;
    I: Integer;
begin
  data := TArray<Double>.Create(
    1, 2, 3, 2, 1, 0, 1, 2, 3, 2,
    1, 0, 1, 2, 2, 2, 1, 0, 1, 1
  );
  exp := DirectDft(ToCmplx128(data));
  a := TDynArrWrapper<Double>.Create(data);

  fft := TRealFFTEvalF64.Create;
  try
    fft.Init(Length(data), False);
    fft.Execute(a, res);
  finally
    fft.Free;
  end;

  TNDAUt.TryAsDynArray<TCmplx128>(res, resArr);
  CheckEquals((Length(exp) div 2) + 1, Length(resArr));
  for I := 0 to High(resArr) do
    CheckEquals(exp[I], resArr[I], dTol);
end;

procedure TFFT64Tests.RealFFT2D_4x5;
var src: INDArray<Double>;
    dst: INDArray<TCmplx128>;
    srcItems: TArray<TArray<Double>>;
    exp, dstItems: TArray<TArray<TCmplx128>>;
    fft: TRealFFTEval2DF64;
    I, J: Integer;
begin
  src := TNDAUt.AsArray<Double>([
     [1, 2, 1, 0, 0],
     [0, 1, 2, 1, 0],
     [0, 0, 1, 2, 1],
     [0, 0, 0, 1, 2]
  ]);
  TNDAUt.TryAsDynArray2D<Double>(src, srcItems);
  exp := directDft2D(ToCmplx(srcItems));

  fft := TRealFFTEval2DF64.Create;
  try
    fft.Init(src.Shape[0], src.Shape[1]);
    fft.Execute(src, dst);
  finally
    fft.Free;
  end;

  CheckEquals(Length(exp), dst.Shape[0]);
  CheckEquals((Length(exp[0]) div 2) + 1, dst.Shape[1]);
  TNDAUt.TryAsDynArray2D<TCmplx128>(dst, dstItems);
  for I := 0 to High(dstItems) do
    for J := 0 to High(dstItems[I]) do
      CheckEquals(exp[I, J], dstItems[I, J], dTol);
end;

procedure TFFT64Tests.RealFFT2D_4x5_NatSpec;
var src: INDArray<Double>;
    dst: INDArray<TCmplx128>;
    srcItems: TArray<TArray<Double>>;
    exp, dstItems: TArray<TArray<TCmplx128>>;
    fft: TRealFFTEval2DF64;
    I, J: Integer;
begin
  src := TNDAUt.AsArray<Double>([
     [1, 2, 1, 0, 0],
     [0, 1, 2, 1, 0],
     [0, 0, 1, 2, 1],
     [0, 0, 0, 1, 2]
  ]);
  TNDAUt.TryAsDynArray2D<Double>(src, srcItems);
  exp := directDft2D(ToCmplx(srcItems));

  fft := TRealFFTEval2DF64.Create;
  try
    fft.SpectrumLayout := slNative;
    fft.Init(src.Shape[0], src.Shape[1]);
    fft.Execute(src, dst);
  finally
    fft.Free;
  end;

  CheckEquals(Length(exp), dst.Shape[1]);
  CheckEquals((Length(exp[0]) div 2) + 1, dst.Shape[0]);
  TNDAUt.TryAsDynArray2D<TCmplx128>(dst, dstItems);
  for I := 0 to High(dstItems) do
    for J := 0 to High(dstItems[I]) do
      CheckEquals(exp[J, I], dstItems[I, J], dTol);
end;

procedure TFFT64Tests.InvDLW_4;
var data, exp: TArray<TCmplx128>;
    w: TArray<TCmplx128>;
    datav, wv: TVecC128;
    N, I: Integer;
begin
  data := TArray<TCmplx128>.Create(Cmplx128(1,2), Cmplx128(3,4), Cmplx128(5,6), Cmplx128(7,8));
  N := Length(data);
  exp := DirectDft(data, -1);
  SetLength(w, N);
  GetW(w, -1);

  datav.Init(@data[0], N);
  wv.Init(@w[0], N);
  _perm(BRPIndices(N), datav);
  DLW(datav, wv);

  for I := 0 to N - 1 do
    CheckEquals(exp[I], data[I], dTol);
end;

procedure TFFT64Tests.InvDLW_32;
var data, exp: TArray<TCmplx128>;
    w: TArray<TCmplx128>;
    datav, wv: TVecC128;
    N, I: Integer;
begin
  data := ToCmplx128([
    1, 2, 3, 4, 3, 2, 1, 0,
    0, 1, 2, 3, 4, 3, 2, 1,
    0, 0, 1, 2, 3, 4, 3, 2,
    1, 0, 1, 1, 2, 3, 4, 3
  ]);
  N := Length(data);
  exp := DirectDft(data, -1);
  SetLength(w, N);
  GetW(w, -1);

  datav.Init(@data[0], N);
  wv.Init(@w[0], N);
  _perm(BRPIndices(N), datav);
  DLW(datav, wv);

  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], dTol);
end;

procedure TFFT64Tests.InvFFT_16r;
var data, exp: TArray<TCmplx128>;
    a, res: INDArray<TCmplx128>;
    I: Integer;
begin
  data := ToCmplx128([
    1, 2, 3, 4, 3, 2, 1, 0,
    0, 1, 2, 3, 4, 3, 2, 0
  ]);
  exp := DirectDft(data, -1);
  a := TDynArrWrapper<TCmplx128>.Create(data);

  fFFT.Direction := fdInverse;
  fFFT.RecursiveMethodThreshold := 0; // to supress inplace method
  fFFT.Init(Length(data));
  fFFT.Execute(a, res);

  TNDAUt.TryAsDynArray<TCmplx128>(res, data);
  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], dTol);
end;

procedure TFFT64Tests.InvFFT_3;
var data, exp: TArray<TCmplx128>;
    a, res: INDArray<TCmplx128>;
    I: Integer;
begin
  data := TArray<TCmplx128>.Create(Cmplx128(1,2), Cmplx128(3,1), Cmplx128(2, 2));
  exp := DirectDft(data, -1);
  a := TDynArrWrapper<TCmplx128>.Create(data);

  fFFT.Direction := fdInverse;
  fFFT.Init(Length(data));
  fFFT.Execute(a, res);

  TNDAUt.TryAsDynArray<TCmplx128>(res, data);
  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], dTol);
end;

procedure TFFT64Tests.InvFFT_27;
var data, exp: TArray<TCmplx128>;
    a, res: INDArray<TCmplx128>;
    I: Integer;
begin
  data := TArray<TCmplx128>.Create(
    Cmplx128(1,2), Cmplx128(2,1), Cmplx128(3,0), Cmplx128(4,1), Cmplx128(5,2),
    Cmplx128(4,2), Cmplx128(3,3), Cmplx128(2,4), Cmplx128(1,5), Cmplx128(0,4),
    Cmplx128(1,3), Cmplx128(2,2), Cmplx128(3,1), Cmplx128(4,0), Cmplx128(5,1),
    Cmplx128(5,2), Cmplx128(4,3), Cmplx128(3,4), Cmplx128(2,5), Cmplx128(1,5),
    Cmplx128(0,3), Cmplx128(1,2), Cmplx128(2,1), Cmplx128(3,0), Cmplx128(4,1),
    Cmplx128(5,2), Cmplx128(5,2)
  );
  exp := DirectDft(data, -1);
  a := TDynArrWrapper<TCmplx128>.Create(data);

  fFFT.Direction := fdInverse;
  fFFT.Init(Length(data));
  fFFT.Execute(a, res);

  TNDAUt.TryAsDynArray<TCmplx128>(res, data);
  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], dTol);
end;

procedure TFFT64Tests.InvFFT_4;
var data, exp: TArray<TCmplx128>;
    a, res: INDArray<TCmplx128>;
    I: Integer;
begin
  data := TArray<TCmplx128>.Create(Cmplx128(1,2), Cmplx128(3,1), Cmplx128(2,1), Cmplx128(1,1));
  exp := DirectDft(data, -1);
  a := TDynArrWrapper<TCmplx128>.Create(data);

  fFFT.RecursiveMethodThreshold := 0;
  fFFT.Direction := fdInverse;
  fFFT.Init(Length(data));
  fFFT.Execute(a, res);

  TNDAUt.TryAsDynArray<TCmplx128>(res, data);
  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], dTol);
end;

procedure TFFT64Tests.InvFFT_5;
var data, exp: TArray<TCmplx128>;
    a, res: INDArray<TCmplx128>;
    I: Integer;
begin
  data := TArray<TCmplx128>.Create(
    Cmplx128(1,2), Cmplx128(3,4), Cmplx128(4,3), Cmplx128(3,2), Cmplx128(2,2)
  );
  exp := DirectDft(data, -1);
  a := TDynArrWrapper<TCmplx128>.Create(data);

  fFFT.Direction := fdInverse;
  fFFT.Init(Length(data));
  fFFT.Execute(a, res);

  TNDAUt.TryAsDynArray<TCmplx128>(res, data);
  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], dTol);
end;

procedure TFFT64Tests.InvFFT_25;
var data, exp: TArray<TCmplx128>;
    a, res: INDArray<TCmplx128>;
    I: Integer;
begin
  data := ToCmplx128([
    1, 2, 3, 2, 1, 0, 1, 2, 3, 2,
    1, 0, 1, 2, 3, 2, 1, 0, 1, 2,
    2, 3, 3, 2, 1
  ]);
  exp := DirectDft(data, -1);
  a := TDynArrWrapper<TCmplx128>.Create(data);

  fFFT.Direction := fdInverse;
  fFFT.Init(Length(data));
  fFFT.Execute(a, res);

  TNDAUt.TryAsDynArray<TCmplx128>(res, data);
  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], dTol);
end;

procedure TFFT64Tests.FwdInvFFT_24;
var data, idata: TArray<TCmplx128>;
    a, res, ires: INDArray<TCmplx128>;
    ifft: TFFTEvalF64;
    I, N: Integer;
begin
  data := ToCmplx128([
    1, 2, 3, 2, 1, 0, 1, 2, 3, 2,
    1, 0, 1, 2, 3, 2, 1, 0, 1, 2,
    2, 3, 3, 2
  ]);
  N := Length(data);
  a := TDynArrWrapper<TCmplx128>.Create(data);

  fFFT.Init(N);
  fFFT.Execute(a, res);

  ifft := TFFTEvalF64.Create;
  try
    ifft.Direction := fdInverse;
    ifft.Init(N);
    ifft.Execute(res, ires);
  finally
    ifft.Free;
  end;

  TNDAUt.TryAsDynArray<TCmplx128>(ires, idata);
  for I := 0 to High(data) do
    CheckEquals(data[I], idata[I]/N, dTol);
end;

procedure TFFT64Tests.FwdInvRealFFT_24;
var data, resData: TArray<Double>;
    a, res: INDArray<Double>;
    tmp: INDArray<TCmplx128>;
    fft: TRealFFTEvalF64;
    ifft: TRealIFFTEvalF64;
    I, N: Integer;
begin
  data := TArray<Double>.Create(
    1, 2, 3, 2, 1, 0, 1, 2, 3, 2,
    1, 0, 1, 2, 3, 2, 1, 0, 1, 2,
    2, 3, 3, 2
  );
  N := Length(data);
  a := TDynArrWrapper<Double>.Create(data);

  fft := TRealFFTEvalF64.Create;
  try
    fft.Init(N);
    fft.Execute(a, tmp);
  finally
    fft.Free;
  end;

  ifft := TRealIFFTEvalF64.Create;
  try
    ifft.Normalize := True;
    ifft.Init(N);
    ifft.Execute(tmp, res);
  finally
    ifft.Free;
  end;

  TNDAUt.TryAsDynArray<Double>(res, resData);
  for I := 0 to High(data) do
    CheckEquals(data[I], resData[I], dTol);
end;

procedure TFFT64Tests.FwdInvRealFFT2D_4x5;
var src, dst: INDArray<Double>;
    tmp: INDArray<TCmplx128>;
    srcItems, dstItems: TArray<TArray<Double>>;
    fft: TRealFFTEval2DF64;
    ifft: TRealIFFTEval2DF64;
    w, h, I, J: Integer;
begin
  src := TNDAUt.AsArray<Double>([
     [1, 2, 1, 0, 0],
     [0, 1, 2, 1, 0],
     [0, 0, 1, 2, 1],
     [0, 0, 0, 1, 2]
  ]);
  TNDAUt.TryAsDynArray2D<Double>(src, srcItems);
  w := src.Shape[1];
  h := src.Shape[0];

  fft := TRealFFTEval2DF64.Create;
  try
    fft.SpectrumLayout := slNative;
    fft.Init(h, w);
    fft.Execute(src, tmp);
  finally
    fft.Free;
  end;

  ifft := TRealIFFTEval2DF64.Create;
  try
    ifft.SpectrumLayout := slNative;
    ifft.Normalize := True;
    ifft.Init(h, w);
    ifft.Execute(tmp, dst);
  finally
    ifft.Free;
  end;

  CheckEquals(h, dst.Shape[0]);
  CheckEquals(w, dst.Shape[1]);
  TNDAUt.TryAsDynArray2D<Double>(dst, dstItems);
  for I := 0 to High(dstItems) do
    for J := 0 to High(dstItems[I]) do
      CheckEquals(srcItems[I, J], dstItems[I, J], dTol);
end;

procedure TFFT64Tests.FwdInvRealFFT2D_4x6;
var src, dst: INDArray<Double>;
    tmp: INDArray<TCmplx128>;
    srcItems, dstItems: TArray<TArray<Double>>;
    fft: TRealFFTEval2DF64;
    ifft: TRealIFFTEval2DF64;
    w, h, I, J: Integer;
begin
  src := TNDAUt.AsArray<Double>([
     [1, 2, 1, 0, 0, 0],
     [0, 1, 2, 1, 0, 0],
     [0, 0, 1, 2, 1, 0],
     [0, 0, 0, 1, 2, 1]
  ]);
  TNDAUt.TryAsDynArray2D<Double>(src, srcItems);
  w := src.Shape[1];
  h := src.Shape[0];

  fft := TRealFFTEval2DF64.Create;
  try
    fft.SpectrumLayout := slNative;
    fft.Init(h, w);
    fft.Execute(src, tmp);
  finally
    fft.Free;
  end;

  ifft := TRealIFFTEval2DF64.Create;
  try
    ifft.SpectrumLayout := slNative;
    ifft.Normalize := True;
    ifft.Init(h, w);
    ifft.Execute(tmp, dst);
  finally
    ifft.Free;
  end;

  CheckEquals(h, dst.Shape[0]);
  CheckEquals(w, dst.Shape[1]);
  TNDAUt.TryAsDynArray2D<Double>(dst, dstItems);
  for I := 0 to High(dstItems) do
    for J := 0 to High(dstItems[I]) do
      CheckEquals(srcItems[I, J], dstItems[I, J], dTol);
end;

{$endregion}

{$region 'TFFT32Tests'}

procedure GetW(const aW: TArray<TCmplx64>; aSgn: Integer = 1); overload;
var wv: TVecC64;
begin
  wv.Init(PByte(aW), Length(aW));
  EvalTwiddleFactors(wv, aSgn);
end;

procedure TFFT32Tests.SetUp;
begin
  inherited;
  fFFT := TFFTevalF32.Create;
end;

procedure TFFT32Tests.TearDown;
begin
  inherited;
  fFFT.Free;
end;

procedure TFFT32Tests.WTable_32;
var w: TArray<TCmplx128>;
    res: TArray<TCmplx64>;
    I: Integer;
const N = 32;
begin
  w := WTable(N);

  SetLength(res, N);
  GetW(res);

  for I := 0 to High(res) do
    CheckEquals(w[I], TCmplx128(res[I]), cWTol);
end;

procedure TFFT32Tests.FFT_32;
var data, exp: TArray<TCmplx64>;
    a, res: INDArray<TCmplx64>;
    I: Integer;
begin
  data := ToCmplx64([
    1, 2, 3, 4, 3, 2, 1, 0,
    0, 1, 2, 3, 4, 3, 2, 1,
    0, 0, 1, 2, 3, 4, 3, 2,
    1, 0, 1, 1, 2, 3, 4, 3
  ]);
  exp := DirectDft(data);
  a := TDynArrWrapper<TCmplx64>.Create(data);

  fFFT.RecursiveMethodThreshold := 16;
  fFFT.Init(Length(data));
  fFFT.Execute(a, res);

  TNDAUt.TryAsDynArray<TCmplx64>(res, data);
  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], sTol);
end;

procedure TFFT32Tests.FFT_8_r2Comb;
var data, exp: TArray<TCmplx64>;
    a, res: INDArray<TCmplx64>;
    I: Integer;
begin
  data := ToCmplx64([1, 2, 3, 4, 3, 2, 1, 0]);
  exp := DirectDft(data);
  a := TDynArrWrapper<TCmplx64>.Create(data);

  fFFT.RecursiveMethodThreshold := 4;
  fFFT.Init(Length(data));
  fFFT.Execute(a, res);

  TNDAUt.TryAsDynArray<TCmplx64>(res, data);
  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], sTol);
end;

procedure TFFT32Tests.FFT_8_r4Comb;
var data, exp: TArray<TCmplx64>;
    a, res: INDArray<TCmplx64>;
    I: Integer;
begin
  data := ToCmplx64([1, 2, 3, 4, 3, 2, 1, 0]);
  exp := DirectDft(data);
  a := TDynArrWrapper<TCmplx64>.Create(data);

  fFFT.RecursiveMethodThreshold := 0; // to supress inplace method
  fFFT.Init(Length(data));
  fFFT.Execute(a, res);

  TNDAUt.TryAsDynArray<TCmplx64>(res, data);
  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], sTol);
end;

procedure TFFT32Tests.FFT_16r;
var data, exp: TArray<TCmplx64>;
    a, res: INDArray<TCmplx64>;
    I: Integer;
begin
  data := ToCmplx64([
    1, 2, 3, 4, 3, 2, 1, 0,
    0, 1, 2, 3, 4, 3, 2, 0
  ]);
  exp := DirectDft(data);
  a := TDynArrWrapper<TCmplx64>.Create(data);

  fFFT.RecursiveMethodThreshold := 0; // to supress inplace method
  fFFT.Init(Length(data));
  fFFT.Execute(a, res);

  TNDAUt.TryAsDynArray<TCmplx64>(res, data);
  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], sTol);
end;

procedure TFFT32Tests.FFT_32r;
var data, exp: TArray<TCmplx64>;
    a, res: INDArray<TCmplx64>;
    I: Integer;
begin
  data := ToCmplx64([
    1, 2, 3, 4, 3, 2, 1, 0,
    0, 1, 2, 3, 4, 3, 2, 1,
    0, 0, 1, 2, 3, 4, 3, 2,
    1, 0, 1, 1, 2, 3, 4, 3
  ]);
  exp := DirectDft(data);
  a := TDynArrWrapper<TCmplx64>.Create(data);

  fFFT.RecursiveMethodThreshold := 0; // to supress inplace method
  fFFT.Init(Length(data));
  fFFT.Execute(a, res);

  TNDAUt.TryAsDynArray<TCmplx64>(res, data);
  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], sTol);
end;

procedure TFFT32Tests.FFT_3;
var data, exp: TArray<TCmplx64>;
    a, res: INDArray<TCmplx64>;
    I: Integer;
begin
  data := TArray<TCmplx64>.Create(Cmplx64(1,2), Cmplx64(3,1), Cmplx64(2, 2));
  exp := DirectDft(data);
  a := TDynArrWrapper<TCmplx64>.Create(data);

  fFFT.Init(Length(data));
  fFFT.Execute(a, res);

  TNDAUt.TryAsDynArray<TCmplx64>(res, data);
  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], sTol);
end;

procedure TFFT32Tests.FFT_27;
var data, exp: TArray<TCmplx64>;
    a, res: INDArray<TCmplx64>;
    I: Integer;
begin
  data := TArray<TCmplx64>.Create(
    Cmplx64(1,2), Cmplx64(2,1), Cmplx64(3,0), Cmplx64(4,1), Cmplx64(5,2),
    Cmplx64(4,2), Cmplx64(3,3), Cmplx64(2,4), Cmplx64(1,5), Cmplx64(0,4),
    Cmplx64(1,3), Cmplx64(2,2), Cmplx64(3,1), Cmplx64(4,0), Cmplx64(5,1),
    Cmplx64(5,2), Cmplx64(4,3), Cmplx64(3,4), Cmplx64(2,5), Cmplx64(1,5),
    Cmplx64(0,3), Cmplx64(1,2), Cmplx64(2,1), Cmplx64(3,0), Cmplx64(4,1),
    Cmplx64(5,2), Cmplx64(5,2)
  );
  exp := DirectDft(data);
  a := TDynArrWrapper<TCmplx64>.Create(data);

  fFFT.Init(Length(data));
  fFFT.Execute(a, res);

  TNDAUt.TryAsDynArray<TCmplx64>(res, data);
  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], sTol);
end;

procedure TFFT32Tests.FFT_4;
var data, exp: TArray<TCmplx64>;
    a, res: INDArray<TCmplx64>;
    I: Integer;
begin
  data := TArray<TCmplx64>.Create(Cmplx64(1,2), Cmplx64(6,5), Cmplx64(3,4), Cmplx64(7,8));
  exp := DirectDft(data);
  a := TDynArrWrapper<TCmplx64>.Create(data);

  fFFT.RecursiveMethodThreshold := 0; // to supress inplace method
  fFFT.Init(Length(data));
  fFFT.Execute(a, res);

  TNDAUt.TryAsDynArray<TCmplx64>(res, data);
  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], sTol);
end;

procedure TFFT32Tests.FFT_16;
 var data, exp: TArray<TCmplx64>;
    a, res: INDArray<TCmplx64>;
    I: Integer;
begin
  data := ToCmplx64([
    1, 2, 3, 2, 1, 0, 1, 2,
    3, 2, 1, 0, 1, 1, 2, 3
  ]);
  exp := DirectDft(data);
  a := TDynArrWrapper<TCmplx64>.Create(data);

  fFFT.Init(Length(data));
  fFFT.Execute(a, res);

  TNDAUt.TryAsDynArray<TCmplx64>(res, data);
  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], sTol);
end;

procedure TFFT32Tests.FFT_24;
var data, exp: TArray<TCmplx64>;
    a, res: INDArray<TCmplx64>;
    I: Integer;
begin
  data := ToCmplx64([
    1, 2, 3, 4, 3, 2, 1, 0,
    0, 1, 2, 3, 4, 3, 2, 1,
    0, 0, 1, 3, 3, 4, 4, 3
  ]);
  exp := DirectDft(data);
  a := TDynArrWrapper<TCmplx64>.Create(data);

  fFFT.Init(Length(data));
  fFFT.Execute(a, res);

  TNDAUt.TryAsDynArray<TCmplx64>(res, data);
  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], sTol);
end;

procedure TFFT32Tests.FFT_36;
var data, exp: TArray<TCmplx64>;
    a, res: INDArray<TCmplx64>;
    I: Integer;
begin
  data := ToCmplx64([
    1, 2, 3, 4, 3, 2, 1, 0,
    0, 1, 2, 3, 4, 3, 2, 1,
    0, 0, 1, 2, 3, 4, 3, 2,
    1, 0, 1, 1, 2, 3, 4, 3,
    3, 2, 2, 1
  ]);
  exp := DirectDft(data);
  a := TDynArrWrapper<TCmplx64>.Create(data);

  fFFT.Init(Length(data));
  fFFT.Execute(a, res);

  TNDAUt.TryAsDynArray<TCmplx64>(res, data);
  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], sTol);
end;

procedure TFFT32Tests.FFT_5;
var data, exp: TArray<TCmplx64>;
    a, res: INDArray<TCmplx64>;
    I: Integer;
begin
  data := TArray<TCmplx64>.Create(
    Cmplx64(1,2), Cmplx64(3,4), Cmplx64(4,3), Cmplx64(3,2), Cmplx64(2,2)
  );
  exp := DirectDft(data);
  a := TDynArrWrapper<TCmplx64>.Create(data);

  fFFT.Init(Length(data));
  fFFT.Execute(a, res);

  TNDAUt.TryAsDynArray<TCmplx64>(res, data);
  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], sTol);
end;

procedure TFFT32Tests.FFT_25;
var data, exp: TArray<TCmplx64>;
    a, res: INDArray<TCmplx64>;
    I: Integer;
begin
  data := ToCmplx64([
    1, 2, 3, 2, 1, 0, 1, 2, 3, 2,
    1, 0, 1, 2, 3, 2, 1, 0, 1, 2,
    2, 3, 3, 2, 1
  ]);
  exp := DirectDft(data);
  a := TDynArrWrapper<TCmplx64>.Create(data);

  fFFT.Init(Length(data));
  fFFT.Execute(a, res);

  TNDAUt.TryAsDynArray<TCmplx64>(res, data);
  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], sTol);
end;

procedure TFFT32Tests.DLW_2;
var data, exp: TArray<TCmplx64>;
    w: TArray<TCmplx64>;
    datav, wv: TVecC64;
    N, I: Integer;
begin
  data := TArray<TCmplx64>.Create(Cmplx64(1, 2), Cmplx64(3, 4));
  N := Length(data);
  exp := DirectDft(data);
  SetLength(w, N);
  GetW(w);

  datav.Init(@data[0], N);
  wv.Init(@w[0], N);
  _perm(BRPIndices(N), datav);
  DLW(datav, wv);

  for I := 0 to N - 1 do
    CheckEquals(exp[I], data[I], sTol);
end;

procedure TFFT32Tests.DLW_4;
var data, exp: TArray<TCmplx64>;
    w: TArray<TCmplx64>;
    datav, wv: TVecC64;
    N, I: Integer;
begin
  data := TArray<TCmplx64>.Create(Cmplx64(1,2), Cmplx64(3,4), Cmplx64(5,6), Cmplx64(7,8));
  N := Length(data);
  exp := DirectDft(data);
  SetLength(w, N);
  GetW(w);

  datav.Init(@data[0], N);
  wv.Init(@w[0], N);
  _perm(BRPIndices(N), datav);
  DLW(datav, wv);

  for I := 0 to N - 1 do
    CheckEquals(exp[I], data[I], sTol);
end;

procedure TFFT32Tests.DLW_8;
var data, exp: TArray<TCmplx64>;
    w: TArray<TCmplx64>;
    datav, wv: TVecC64;
    N, I: Integer;
begin
  data := TArray<TCmplx64>.Create(
    Cmplx64(1,1), Cmplx64(2,2), Cmplx64(1,1), Cmplx64(0,0),
    Cmplx64(0,1), Cmplx64(1,2), Cmplx64(2,1), Cmplx64(1,1)
  );
  N := Length(data);
  exp := DirectDft(data);
  SetLength(w, N);
  GetW(w);

  datav.Init(@data[0], N);
  wv.Init(@w[0], N);
  _perm(BRPIndices(N), datav);
  DLW(datav, wv);

  for I := 0 to N - 1 do
    CheckEquals(exp[I], data[I], sTol);
end;

procedure TFFT32Tests.DLW_32;
var data, exp: TArray<TCmplx64>;
    w: TArray<TCmplx64>;
    datav, wv: TVecC64;
    N, I: Integer;
begin
  data := ToCmplx64([
    1, 2, 3, 4, 3, 2, 1, 0,
    0, 1, 2, 3, 4, 3, 2, 1,
    0, 0, 1, 2, 3, 4, 3, 2,
    1, 0, 1, 1, 2, 3, 4, 3
  ]);
  N := Length(data);
  exp := DirectDft(data);
  SetLength(w, N);
  GetW(w);

  datav.Init(@data[0], N);
  wv.Init(@w[0], N);
  _perm(BRPIndices(N), datav);
  DLW(datav, wv);

  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], sTol);
end;

procedure TFFT32Tests.RealFFT_8;
var data: TArray<Single>;
    exp, resArr: TArray<TCmplx64>;
    a: INDArray<Single>;
    res: INDArray<TCmplx64>;
    fft: TRealFFTEvalF32;
    I: Integer;
begin
  data := TArray<Single>.Create(1, 2, 3, 2, 1, 0, 1, 1);
  exp := DirectDft(ToCmplx64(data));
  a := TDynArrWrapper<Single>.Create(data);

  fft := TRealFFTEvalF32.Create;
  try
    fft.Init(Length(data), True);
    fft.Execute(a, res);
  finally
    fft.Free;
  end;

  TNDAUt.TryAsDynArray<TCmplx64>(res, resArr);
  CheckEquals(Length(exp), Length(resArr));
  for I := 0 to High(resArr) do
    CheckEquals(exp[I], resArr[I], sTol);
end;

procedure TFFT32Tests.RealFFT_16;
var data: TArray<Single>;
    exp, resArr: TArray<TCmplx64>;
    a: INDArray<Single>;
    res: INDArray<TCmplx64>;
    fft: TRealFFTEvalF32;
    I: Integer;
begin
  data := TArray<Single>.Create(1, 1, 2, 2, 1, 1, 0, 0, 0, 1, 1, 2, 2, 1, 1, 1);
  exp := DirectDft(ToCmplx64(data));
  a := TDynArrWrapper<Single>.Create(data);

  fft := TRealFFTEvalF32.Create;
  try
    fft.Init(Length(data), True);
    fft.Execute(a, res);
  finally
    fft.Free;
  end;

  TNDAUt.TryAsDynArray<TCmplx64>(res, resArr);
  CheckEquals(Length(exp), Length(resArr));
  for I := 0 to High(resArr) do
    CheckEquals(exp[I], resArr[I], sTol);
end;

procedure TFFT32Tests.RealFFT_20;
var data: TArray<Single>;
    exp, resArr: TArray<TCmplx64>;
    a: INDArray<Single>;
    res: INDArray<TCmplx64>;
    fft: TRealFFTEvalF32;
    I: Integer;
begin
  data := TArray<Single>.Create(
    1, 2, 3, 2, 1, 0, 1, 2, 3, 2,
    1, 0, 1, 2, 2, 2, 1, 0, 1, 1
  );
  exp := DirectDft(ToCmplx64(data));
  a := TDynArrWrapper<Single>.Create(data);

  fft := TRealFFTEvalF32.Create;
  try
    fft.Init(Length(data), True);
    fft.Execute(a, res);
  finally
    fft.Free;
  end;

  TNDAUt.TryAsDynArray<TCmplx64>(res, resArr);
  CheckEquals(Length(exp), Length(resArr));
  for I := 0 to High(resArr) do
    CheckEquals(exp[I], resArr[I], sTol);
end;

procedure TFFT32Tests.RealFFTHalfSpectrum;
var data: TArray<Single>;
    exp, resArr: TArray<TCmplx64>;
    a: INDArray<Single>;
    res: INDArray<TCmplx64>;
    fft: TRealFFTEvalF32;
    I: Integer;
begin
  data := TArray<Single>.Create(
    1, 2, 3, 2, 1, 0, 1, 2, 3, 2,
    1, 0, 1, 2, 2, 2, 1, 0, 1, 1
  );
  exp := DirectDft(ToCmplx64(data));
  a := TDynArrWrapper<Single>.Create(data);

  fft := TRealFFTEvalF32.Create;
  try
    fft.Init(Length(data), False);
    fft.Execute(a, res);
  finally
    fft.Free;
  end;

  TNDAUt.TryAsDynArray<TCmplx64>(res, resArr);
  CheckEquals((Length(exp) div 2) + 1, Length(resArr));
  for I := 0 to High(resArr) do
    CheckEquals(exp[I], resArr[I], sTol);
end;

procedure TFFT32Tests.RealFFT2D_4x5;
var src: INDArray<Single>;
    dst: INDArray<TCmplx64>;
    srcItems: TArray<TArray<Single>>;
    exp, dstItems: TArray<TArray<TCmplx64>>;
    fft: TRealFFTEval2DF32;
    I, J: Integer;
begin
  src := TNDAUt.AsArray<Single>([
     [1, 2, 1, 0, 0],
     [0, 1, 2, 1, 0],
     [0, 0, 1, 2, 1],
     [0, 0, 0, 1, 2]
  ]);
  TNDAUt.TryAsDynArray2D<Single>(src, srcItems);
  exp := DirectDft2D(ToCmplx64(srcItems));

  fft := TRealFFTEval2DF32.Create;
  try
    fft.Init(src.Shape[0], src.Shape[1]);
    fft.Execute(src, dst);
  finally
    fft.Free;
  end;

  CheckEquals(Length(exp), dst.Shape[0]);
  CheckEquals((Length(exp[0]) div 2) + 1, dst.Shape[1]);
  TNDAUt.TryAsDynArray2D<TCmplx64>(dst, dstItems);
  for I := 0 to High(exp) do
    for J := 0 to High(dstItems[I]) do
      CheckEquals(exp[I, J], dstItems[I, J], sTol);
end;

procedure TFFT32Tests.RealFFT2D_6x5;
var src: INDArray<Single>;
    dst: INDArray<TCmplx64>;
    srcItems: TArray<TArray<Single>>;
    exp, dstItems: TArray<TArray<TCmplx64>>;
    fft: TRealFFTEval2DF32;
    I, J: Integer;
begin
  src := TNDAUt.AsArray<Single>([
     [1, 2, 1, 0, 0],
     [0, 1, 2, 1, 0],
     [0, 0, 1, 2, 1],
     [0, 0, 0, 1, 2],
     [1, 0, 0, 0, 1],
     [2, 1, 0, 0, 1]
  ]);
  TNDAUt.TryAsDynArray2D<Single>(src, srcItems);
  exp := DirectDft2D(ToCmplx64(srcItems));

  fft := TRealFFTEval2DF32.Create;
  try
    fft.Init(src.Shape[0], src.Shape[1]);
    fft.Execute(src, dst);
  finally
    fft.Free;
  end;

  CheckEquals(Length(exp), dst.Shape[0]);
  CheckEquals((Length(exp[0]) div 2) + 1, dst.Shape[1]);
  TNDAUt.TryAsDynArray2D<TCmplx64>(dst, dstItems);
  for I := 0 to High(exp) do
    for J := 0 to High(dstItems[I]) do
      CheckEquals(exp[I, J], dstItems[I, J], sTol);
end;

procedure TFFT32Tests.RealFFT2D_6x8;
var src: INDArray<Single>;
    dst: INDArray<TCmplx64>;
    srcItems: TArray<TArray<Single>>;
    exp, dstItems: TArray<TArray<TCmplx64>>;
    fft: TRealFFTEval2DF32;
    I, J: Integer;
begin
  src := TNDAUt.AsArray<Single>([
     [1, 2, 1, 0, 0, 0, 1, 2],
     [0, 1, 2, 1, 0, 0, 0, 1],
     [0, 0, 1, 2, 1, 0, 0, 0],
     [0, 0, 0, 1, 2, 1, 0, 0],
     [1, 0, 0, 0, 1, 2, 1, 0],
     [2, 1, 0, 0, 1, 1, 2, 1]
  ]);
  TNDAUt.TryAsDynArray2D<Single>(src, srcItems);
  exp := DirectDft2D(ToCmplx64(srcItems));

  fft := TRealFFTEval2DF32.Create;
  try
    fft.Init(src.Shape[0], src.Shape[1]);
    fft.Execute(src, dst);
  finally
    fft.Free;
  end;

  CheckEquals(Length(exp), dst.Shape[0]);
  CheckEquals((Length(exp[0]) div 2) + 1, dst.Shape[1]);
  TNDAUt.TryAsDynArray2D<TCmplx64>(dst, dstItems);
  for I := 0 to High(exp) do
    for J := 0 to High(dstItems[I]) do
      CheckEquals(exp[I, J], dstItems[I, J], sTol);
end;

procedure TFFT32Tests.InvDLW_4;
var data, exp: TArray<TCmplx64>;
    w: TArray<TCmplx64>;
    datav, wv: TVecC64;
    N, I: Integer;
begin
  data := TArray<TCmplx64>.Create(Cmplx64(1,2), Cmplx64(3,4), Cmplx64(5,6), Cmplx64(7,8));
  N := Length(data);
  exp := DirectDft(data, -1);
  SetLength(w, N);
  GetW(w, -1);

  datav.Init(@data[0], N);
  wv.Init(@w[0], N);
  _perm(BRPIndices(N), datav);
  DLW(datav, wv);

  for I := 0 to N - 1 do
    CheckEquals(exp[I], data[I], sTol);
end;

procedure TFFT32Tests.InvDLW_32;
var data, exp: TArray<TCmplx64>;
    w: TArray<TCmplx64>;
    datav, wv: TVecC64;
    N, I: Integer;
begin
  data := ToCmplx64([
    1, 2, 3, 4, 3, 2, 1, 0,
    0, 1, 2, 3, 4, 3, 2, 1,
    0, 0, 1, 2, 3, 4, 3, 2,
    1, 0, 1, 1, 2, 3, 4, 3
  ]);
  N := Length(data);
  exp := DirectDft(data, -1);
  SetLength(w, N);
  GetW(w, -1);

  datav.Init(@data[0], N);
  wv.Init(@w[0], N);
  _perm(BRPIndices(N), datav);
  DLW(datav, wv);

  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], sTol);
end;

procedure TFFT32Tests.InvFFT_16r;
var data, exp: TArray<TCmplx64>;
    a, res: INDArray<TCmplx64>;
    I: Integer;
begin
  data := ToCmplx64([
    1, 2, 3, 4, 3, 2, 1, 0,
    0, 1, 2, 3, 4, 3, 2, 0
  ]);
  exp := DirectDft(data, -1);
  a := TDynArrWrapper<TCmplx64>.Create(data);

  fFFT.Direction := fdInverse;
  fFFT.RecursiveMethodThreshold := 0; // to supress inplace method
  fFFT.Init(Length(data));
  fFFT.Execute(a, res);

  TNDAUt.TryAsDynArray<TCmplx64>(res, data);
  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], sTol);
end;

procedure TFFT32Tests.InvFFT_3;
var data, exp: TArray<TCmplx64>;
    a, res: INDArray<TCmplx64>;
    I: Integer;
begin
  data := TArray<TCmplx64>.Create(Cmplx64(1,2), Cmplx64(3,1), Cmplx64(2, 2));
  exp := DirectDft(data, -1);
  a := TDynArrWrapper<TCmplx64>.Create(data);

  fFFT.Direction := fdInverse;
  fFFT.Init(Length(data));
  fFFT.Execute(a, res);

  TNDAUt.TryAsDynArray<TCmplx64>(res, data);
  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], sTol);
end;

procedure TFFT32Tests.InvFFT_27;
var data, exp: TArray<TCmplx64>;
    a, res: INDArray<TCmplx64>;
    I: Integer;
begin
  data := TArray<TCmplx64>.Create(
    Cmplx64(1,2), Cmplx64(2,1), Cmplx64(3,0), Cmplx64(4,1), Cmplx64(5,2),
    Cmplx64(4,2), Cmplx64(3,3), Cmplx64(2,4), Cmplx64(1,5), Cmplx64(0,4),
    Cmplx64(1,3), Cmplx64(2,2), Cmplx64(3,1), Cmplx64(4,0), Cmplx64(5,1),
    Cmplx64(5,2), Cmplx64(4,3), Cmplx64(3,4), Cmplx64(2,5), Cmplx64(1,5),
    Cmplx64(0,3), Cmplx64(1,2), Cmplx64(2,1), Cmplx64(3,0), Cmplx64(4,1),
    Cmplx64(5,2), Cmplx64(5,2)
  );
  exp := DirectDft(data, -1);
  a := TDynArrWrapper<TCmplx64>.Create(data);

  fFFT.Direction := fdInverse;
  fFFT.Init(Length(data));
  fFFT.Execute(a, res);

  TNDAUt.TryAsDynArray<TCmplx64>(res, data);
  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], sTol);
end;

procedure TFFT32Tests.InvFFT_4;
var data, exp: TArray<TCmplx64>;
    a, res: INDArray<TCmplx64>;
    I: Integer;
begin
  data := TArray<TCmplx64>.Create(Cmplx64(1,2), Cmplx64(3,1), Cmplx64(2,1), Cmplx64(1,1));
  exp := DirectDft(data, -1);
  a := TDynArrWrapper<TCmplx64>.Create(data);

  fFFT.RecursiveMethodThreshold := 0;
  fFFT.Direction := fdInverse;
  fFFT.Init(Length(data));
  fFFT.Execute(a, res);

  TNDAUt.TryAsDynArray<TCmplx64>(res, data);
  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], sTol);
end;

procedure TFFT32Tests.InvFFT_5;
var data, exp: TArray<TCmplx64>;
    a, res: INDArray<TCmplx64>;
    I: Integer;
begin
  data := TArray<TCmplx64>.Create(
    Cmplx64(1,2), Cmplx64(3,4), Cmplx64(4,3), Cmplx64(3,2), Cmplx64(2,2)
  );
  exp := DirectDft(data, -1);
  a := TDynArrWrapper<TCmplx64>.Create(data);

  fFFT.Direction := fdInverse;
  fFFT.Init(Length(data));
  fFFT.Execute(a, res);

  TNDAUt.TryAsDynArray<TCmplx64>(res, data);
  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], sTol);
end;

procedure TFFT32Tests.InvFFT_25;
var data, exp: TArray<TCmplx64>;
    a, res: INDArray<TCmplx64>;
    I: Integer;
begin
  data := ToCmplx64([
    1, 2, 3, 2, 1, 0, 1, 2, 3, 2,
    1, 0, 1, 2, 3, 2, 1, 0, 1, 2,
    2, 3, 3, 2, 1
  ]);
  exp := DirectDft(data, -1);
  a := TDynArrWrapper<TCmplx64>.Create(data);

  fFFT.Direction := fdInverse;
  fFFT.Init(Length(data));
  fFFT.Execute(a, res);

  TNDAUt.TryAsDynArray<TCmplx64>(res, data);
  for I := 0 to High(data) do
    CheckEquals(exp[I], data[I], sTol);
end;

procedure TFFT32Tests.FwdInvFFT_25;
var data, idata: TArray<TCmplx64>;
    a, res, ires: INDArray<TCmplx64>;
    ifft: TFFTEvalF32;
    I, N: Integer;
begin
  data := ToCmplx64([
    1, 2, 3, 2, 1, 0, 1, 2, 3, 2,
    1, 0, 1, 2, 3, 2, 1, 0, 1, 2,
    2, 3, 3, 2, 1
  ]);
  N := Length(data);
  a := TDynArrWrapper<TCmplx64>.Create(data);

  fFFT.Init(N);
  fFFT.Execute(a, res);

  ifft := TFFTEvalF32.Create;
  try
    ifft.Direction := fdInverse;
    ifft.Init(N);
    ifft.Execute(res, ires);
  finally
    ifft.Free;
  end;

  TNDAUt.TryAsDynArray<TCmplx64>(ires, idata);
  for I := 0 to High(data) do
    CheckEquals(data[I], idata[I]/N, sTol);
end;

procedure TFFT32Tests.FwdInvRealFFT_24;
var data, resData: TArray<Single>;
    a, res: INDArray<Single>;
    tmp: INDArray<TCmplx64>;
    fft: TRealFFTEvalF32;
    ifft: TRealIFFTEvalF32;
    I, N: Integer;
begin
  data := TArray<Single>.Create(
    1, 2, 3, 2, 1, 0, 1, 2, 3, 2,
    1, 0, 1, 2, 3, 2, 1, 0, 1, 2,
    2, 3, 3, 2
  );
  N := Length(data);
  a := TDynArrWrapper<Single>.Create(data);

  fft := TRealFFTEvalF32.Create;
  try
    fft.Init(N);
    fft.Execute(a, tmp);
  finally
    fft.Free;
  end;

  ifft := TRealIFFTEvalF32.Create;
  try
    ifft.Normalize := True;
    ifft.Init(N);
    ifft.Execute(tmp, res);
  finally
    ifft.Free;
  end;

  TNDAUt.TryAsDynArray<Single>(res, resData);
  for I := 0 to High(data) do
    CheckEquals(data[I], resData[I], sTol);
end;

procedure TFFT32Tests.FwdInvRealFFT2D_4x3;
var src, dst: INDArray<Single>;
    tmp: INDArray<TCmplx64>;
    srcItems, dstItems: TArray<TArray<Single>>;
    fft: TRealFFTEval2DF32;
    ifft: TRealIFFTEval2DF32;
    w, h, I, J: Integer;
begin
  src := TNDAUt.AsArray<Single>([
     [1, 2, 1],
     [0, 1, 2],
     [0, 0, 1],
     [0, 0, 0]
  ]);
  TNDAUt.TryAsDynArray2D<Single>(src, srcItems);
  w := src.Shape[1];
  h := src.Shape[0];

  fft := TRealFFTEval2DF32.Create;
  try
    fft.SpectrumLayout := slNative;
    fft.Init(h, w);
    fft.Execute(src, tmp);
  finally
    fft.Free;
  end;

  ifft := TRealIFFTEval2DF32.Create;
  try
    ifft.SpectrumLayout := slNative;
    ifft.Normalize := True;
    ifft.Init(h, w);
    ifft.Execute(tmp, dst);
  finally
    ifft.Free;
  end;

  CheckEquals(h, dst.Shape[0]);
  CheckEquals(w, dst.Shape[1]);
  TNDAUt.TryAsDynArray2D<Single>(dst, dstItems);
  for I := 0 to High(dstItems) do
    for J := 0 to High(dstItems[I]) do
      CheckEquals(srcItems[I, J], dstItems[I, J], sTol);
end;

procedure TFFT32Tests.FwdInvRealFFT2D_4x5;
var src, dst: INDArray<Single>;
    tmp: INDArray<TCmplx64>;
    srcItems, dstItems: TArray<TArray<Single>>;
    fft: TRealFFTEval2DF32;
    ifft: TRealIFFTEval2DF32;
    w, h, I, J: Integer;
begin
  src := TNDAUt.AsArray<Single>([
     [1, 2, 1, 0, 0],
     [0, 1, 2, 1, 0],
     [0, 0, 1, 2, 1],
     [0, 0, 0, 1, 2]
  ]);
  TNDAUt.TryAsDynArray2D<Single>(src, srcItems);
  w := src.Shape[1];
  h := src.Shape[0];

  fft := TRealFFTEval2DF32.Create;
  try
    fft.SpectrumLayout := slNative;
    fft.Init(h, w);
    fft.Execute(src, tmp);
  finally
    fft.Free;
  end;

  ifft := TRealIFFTEval2DF32.Create;
  try
    ifft.SpectrumLayout := slNative;
    ifft.Normalize := True;
    ifft.Init(h, w);
    ifft.Execute(tmp, dst);
  finally
    ifft.Free;
  end;

  CheckEquals(h, dst.Shape[0]);
  CheckEquals(w, dst.Shape[1]);
  TNDAUt.TryAsDynArray2D<Single>(dst, dstItems);
  for I := 0 to High(dstItems) do
    for J := 0 to High(dstItems[I]) do
      CheckEquals(srcItems[I, J], dstItems[I, J], sTol);
end;

procedure TFFT32Tests.FwdInvRealFFT2D_4x6;
var src, dst: INDArray<Single>;
    tmp: INDArray<TCmplx64>;
    srcItems, dstItems: TArray<TArray<Single>>;
    fft: TRealFFTEval2DF32;
    ifft: TRealIFFTEval2DF32;
    w, h, I, J: Integer;
begin
  src := TNDAUt.AsArray<Single>([
     [1, 2, 1, 0, 0, 0],
     [0, 1, 2, 1, 0, 0],
     [0, 0, 1, 2, 1, 0],
     [0, 0, 0, 1, 2, 1]
  ]);
  TNDAUt.TryAsDynArray2D<Single>(src, srcItems);
  w := src.Shape[1];
  h := src.Shape[0];

  fft := TRealFFTEval2DF32.Create;
  try
    fft.SpectrumLayout := slNative;
    fft.Init(h, w);
    fft.Execute(src, tmp);
  finally
    fft.Free;
  end;

  ifft := TRealIFFTEval2DF32.Create;
  try
    ifft.SpectrumLayout := slNative;
    ifft.Normalize := True;
    ifft.Init(h, w);
    ifft.Execute(tmp, dst);
  finally
    ifft.Free;
  end;

  CheckEquals(h, dst.Shape[0]);
  CheckEquals(w, dst.Shape[1]);
  TNDAUt.TryAsDynArray2D<Single>(dst, dstItems);
  for I := 0 to High(dstItems) do
    for J := 0 to High(dstItems[I]) do
      CheckEquals(srcItems[I, J], dstItems[I, J], sTol);
end;

{$endregion}

initialization

  RegisterTest(TFFTLowLvlTests.Suite);
  RegisterTest(TFFT64Tests.Suite);
  RegisterTest(TFFT32Tests.Suite);

end.
