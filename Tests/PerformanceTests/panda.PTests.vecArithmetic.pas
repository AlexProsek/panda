unit panda.PTests.vecArithmetic;

interface

uses
    TestFramework
  , panda.Tests.NDATestCase
  , panda.cvArithmetic
  ;

type
  TVecMathPerformance = class(TNDAPerformanceTestCase)
  published
    procedure AddVec_Single;
    procedure AddVec_Double;
    procedure SubVec_Single;
    procedure AddVecInPlace_Single;
    procedure MulVec_Single;
    procedure MulVec_Double;
    procedure MulVec_Cmplx128;
    procedure DivVec_Cmplx128;
    procedure AddScalarToVec_Single;
    procedure AddScalarToVec_Double;
    procedure SubScalarFromVec_Single;
    procedure MulVecByScalar_Single;
    procedure AddVecWithSat_UInt8;
    procedure AddVecWithSat_UInt16;
    procedure AndVec;
    procedure TestMin_2U8;
    procedure TestMin_3U8;
    procedure TestMin_4U8;
    procedure TestMin_NU8;
    procedure TestMin_NU8_N100;
    procedure TestScal_Double;
    procedure TestAXPY;
    procedure TestAXPY_A1;
    procedure TestAXPY_Single;
    procedure TestAXPY_Cmplx;
    procedure TestDot;
    procedure TestDot_Single;
    procedure TestDot_Single_A;
    procedure TestSCopy_Single;
    procedure TestSCopy_Double;
    procedure VecNeg_Single;
    procedure Diff_Single;
    procedure DiffWithStep_Single;
  end;

implementation

{$region 'TVecMathPerformance'}

procedure TVecMathPerformance.AddVec_Single;
var x, res: TArray<Single>;
const N = 1000000;
begin
  SetLength(x, N);
  SetLength(res, N);

  SWStart;
  VecAdd(PSingle(@x[0]), @x[0], @res[0], N);
  SWStop;
end;

procedure TVecMathPerformance.AddVec_Double;
var x, res: TArray<Double>;
const N = 1000000;
begin
  SetLength(x, N);
  SetLength(res, N);

  SWStart;
  VecAdd(PDouble(@x[0]), @x[0], @res[0], N);
  SWStop;
end;

procedure TVecMathPerformance.SubVec_Single;
var x, res: TArray<Single>;
const N = 1000000;
begin
  SetLength(x, N);
  SetLength(res, N);

  SWStart;
  VecSub(PSingle(x), PSingle(x), PSingle(res), N);
  SWStop;
end;

procedure TVecMathPerformance.AddVecInPlace_Single;
var a, b: TArray<Single>;
const N = 10000000;
begin
  SetLength(a, N);
  SetLength(b, N);

  SWStart;
  VecAddInPlace(PSingle(a), PSingle(b), 1, 1, N);
  SWStop;
end;

procedure TVecMathPerformance.MulVec_Single;
var x, res: TArray<Single>;
const N = 1000000;
begin
  SetLength(x, N);
  SetLength(res, N);

  SWStart;
  VecMul(PSingle(@x[0]), @x[0], @res[0], N);
  SWStop;
end;

procedure TVecMathPerformance.MulVec_Double;
var x, res: TArray<Double>;
const N = 1000000;
begin
  SetLength(x, N);
  SetLength(res, N);

  SWStart;
  VecMul(PDouble(@x[0]), @x[0], @res[0], N);
  SWStop;
end;

procedure TVecMathPerformance.MulVec_Cmplx128;
var x, res: TArray<TDblCmplx>;
const N = 1000000;
begin
  SetLength(x, N);
  SetLength(res, N);

  SWStart;
  VecMul(PDblCmplx(@x[0]), @x[0], @res[0], N);
  SWStop;
end;

procedure TVecMathPerformance.DivVec_Cmplx128;
var x, res: TArray<TDblCmplx>;
    I: Integer;
const N = 1000000;
begin
  SetLength(x, N);
  SetLength(res, N);
  for I := 0 to N - 1 do
    x[I].Init(1, 2);

  SWStart;
  VecDiv(PDblCmplx(@x[0]), @x[0], @res[0], N);
  SWStop;
end;

procedure TVecMathPerformance.AddScalarToVec_Single;
var x, res: TArray<Single>;
    y: Single;
const N = 1000000;
begin
  y := 2;
  SetLength(x, N);
  SetLength(res, N);

  SWStart;
  VecAdd(PSingle(@x[0]), y, @res[0], N);
  SWStop;
end;

procedure TVecMathPerformance.AddScalarToVec_Double;
var x, res: TArray<Double>;
    y: Double;
const N = 1000000;
begin
  y := 2;
  SetLength(x, N);
  SetLength(res, N);

  SWStart;
  VecAdd(PDouble(@x[0]), y, @res[0], N);
  SWStop;
end;

procedure TVecMathPerformance.SubScalarFromVec_Single;
var x, res: TArray<Single>;
    y: Single;
const N = 1000000;
begin
  y := 2;
  SetLength(x, N);
  SetLength(res, N);

  SWStart;
  VecSub(PSingle(@x[0]), y, @res[0], N);
  SWStop;
end;

procedure TVecMathPerformance.MulVecByScalar_Single;
var x, res: TArray<Single>;
    y: Single;
const N = 1000000;
begin
  y := 2;
  SetLength(x, N);
  SetLength(res, N);

  SWStart;
  VecMul(PSingle(x), y, PSingle(res), N);
  SWStop;
end;

procedure TVecMathPerformance.AddVecWithSat_UInt8;
var x, res: TArray<UInt8>;
const N = 1000000;
begin
  SetLength(x, N);
  SetLength(res, N);

  SWStart;
  VecAddWithSat(PUInt8(@x[0]), @x[0], @res[0], N);
  SWStop;
end;

procedure TVecMathPerformance.AddVecWithSat_UInt16;
var x, res: TArray<UInt16>;
const N = 1000000;
begin
  SetLength(x, N);
  SetLength(res, N);

  SWStart;
  VecAddWithSat(PUInt16(@x[0]), @x[0], @res[0], N);
  SWStop;
end;

procedure TVecMathPerformance.AndVec;
var x, res: TArray<Byte>;
const N = 10000000;
begin
  SetLength(x, N);
  SetLength(res, N);

  SWStart;
  VecAnd(PByte(x), PByte(x), PByte(res), N);
  SWStop;
end;

procedure TVecMathPerformance.TestMin_2U8;
var a, b, res: UInt8;
    I: Integer;
const N  = 1000000;
begin
  a := 2; b := 3;
  I := N;
  SWStart;
  while I > 0 do begin
    res := Min_2U8(a, b);
    Dec(I);
  end;
  SWStop('without branching');

  I := N;
  SWStart;
  while I > 0 do begin
    if a < b then
      res := a
    else
      res := b;
    Dec(I);
  end;
  SWStop('with branching');
end;

procedure TVecMathPerformance.TestMin_3U8;
var a, b, c, res: UInt8;
    I: Integer;
const N  = 1000000;
begin
  a := 5; b := 10;
  I := N;
  SWStart;
  while I > 0 do begin
    c := I and $F;
    res := Min_3U8(a, b, c);
    Dec(I);
  end;
  SWStop('without branching');

  I := N;
  SWStart;
  while I > 0 do begin
    c := I and $F;
    if a < b then begin
      if c < a then
        res := c
      else
        res := a;
    end else begin
      if c < b then
        res := c
      else
        res := b;
    end;
    Dec(I);
  end;
  SWStop('with branching');
end;

function _Min_4U8(a, b, c, d: UInt8): UInt8; inline;
begin
  Result := a;
  if b < Result then
    Result := b;
  if c < Result then
    Result := c;
  if d < Result then
    Result := d;
end;

procedure TVecMathPerformance.TestMin_4U8;
var a, b, c, d, res: UInt8;
    I: Integer;
const N  = 1000000;
begin
  a := 5; b := 10; d := 1;
  I := N;
  SWStart;
  while I > 0 do begin
    c := I and $F;
    res := Min_4U8(a, b, c, d);
    Dec(I);
  end;
  SWStop('without branching');

  I := N;
  SWStart;
  while I > 0 do begin
    c := I and $F;
    res := _Min_4U8(a, b, c, d);
    Dec(I);
  end;
  SWStop('with branching');
end;

function _Min_NU8(pA: PUInt8; N: Integer): UInt8;
begin
  Result := pA^;
  while N > 1 do begin
    Inc(pA);
    if pA^ < Result then
      Result := pA^;
    Dec(N);
  end;
end;

procedure TVecMathPerformance.TestMin_NU8;
var x: TArray<UInt8>;
    res: UInt8;
    I, M: Integer;
const N  = 1000000;
begin
  x := TArray<UInt8>.Create(5, 4, 3, 2, 1, 2, 3, 4, 5);
  M := Length(x);

  I := N;
  SWStart;
  while I > 0 do begin
    res := Min_NU8(PUInt8(x), M);
    Dec(I);
  end;
  SWStop('without branching');

  I := N;
  SWStart;
  while I > 0 do begin
    res := _Min_NU8(PUInt8(x), M);
    Dec(I);
  end;
  SWStop('with branching');
end;

procedure TVecMathPerformance.TestMin_NU8_N100;
var x: TArray<UInt8>;
    res: UInt8;
    I, M: Integer;
const N  = 1000000;
begin
  M := 100;
  SetLength(x, M);
  for I := 0 to High(x) do begin
    x[I] := M;
    Dec(M);
  end;
  M := Length(x);

  I := N;
  SWStart;
  while I > 0 do begin
    res := Min_NU8(PUInt8(x), M);
    Dec(I);
  end;
  SWStop('without branching');

  I := N;
  SWStart;
  while I > 0 do begin
    res := _Min_NU8(PUInt8(x), M);
    Dec(I);
  end;
  SWStop('with branching');
end;

procedure TVecMathPerformance.TestScal_Double;
var x: TArray<Double>;
const N = 10000000;
begin
  SetLength(x, N);

  SWStart;
  dscal(PDouble(x), N, 2);
  SWStop;
end;

procedure TVecMathPerformance.TestAXPY;
var x, y: TArray<Double>;
const N = 10000000;
begin
  SetLength(x, N);
  SetLength(y, N);

  SWStart;
  axpy(2, PDouble(x), PDouble(y), N);
  SWStop;
end;

procedure TVecMathPerformance.TestAXPY_A1;
var x, y: TArray<Double>;
const N = 10000000;
begin
  SetLength(x, N);
  SetLength(y, N);

  SWStart;
  axpy(1, PDouble(x), PDouble(y), N);
  SWStop;
end;

procedure TVecMathPerformance.TestAXPY_Single;
var x, y: TArray<Double>;
const N = 10000000;
begin
  SetLength(x, N);
  SetLength(y, N);

  SWStart;
  axpy(2, PSingle(x), PSingle(y), N);
  SWStop;
end;

procedure TVecMathPerformance.TestAXPY_Cmplx;
var x, y: TArray<TDblCmplx>;
    a: TDblCmplx;
const N = 1000000;
begin
  SetLength(x, N);
  SetLength(y, N);
  a.Init(1, 1);

  SWStart;
  axpy(a, PDblCmplx(x), PDblCmplx(y), N);
  SWStop;
end;

procedure TVecMathPerformance.TestDot;
var x: TArray<Double>;
    res: Double;
const N = 1000000;
begin
  SetLength(x, N);

  SWStart;
  dot(PDouble(x), PDouble(x), N, res);
  SWStop;
end;

procedure TVecMathPerformance.TestDot_Single;
var x: TArray<Single>;
    pX: PSingle;
    res: Single;
const N = 10000000;
begin
  SetLength(x, N + 4);
  pX := PSingle(NativeUInt(PByte(x) + $F) and (not NativeUInt($F)) + 4);

  SWStart;
  dot(pX, pX, N, res);
  SWStop;
end;

procedure TVecMathPerformance.TestDot_Single_A;
var x: TArray<Single>;
    pX: PSingle;
    res: Single;
const N = 10000000;
begin
  SetLength(x, N + 4);
  pX := PSingle(NativeUInt(PByte(x) + $F) and (not NativeUInt($F)));

  SWStart;
  dot(pX, pX, N, res);
  SWStop('aligned');
end;

procedure TVecMathPerformance.TestSCopy_Single;
var x, y: TArray<Single>;
const N = 10000000;
begin
  SetLength(x, N);
  SetLength(y, N);

  SWStart;
  scopy(PSingle(x), PSingle(y), N, 1, 1);
  SWStop;
end;

procedure TVecMathPerformance.TestSCopy_Double;
var x, y: TArray<Double>;
const N = 10000000;
begin
  SetLength(x, N);
  SetLength(y, N);

  SWStart;
  dcopy(PDouble(x), PDouble(y), N, 1, 1);
  SWStop;
end;

procedure TVecMathPerformance.VecNeg_Single;
var x, y: TArray<Single>;
const N = 10000000;
begin
  SetLength(x, N);
  SetLength(y, N);

  SWStart;
  VecNeg(PSingle(x), PSingle(y), Length(x));
  SWStop;
end;

procedure TVecMathPerformance.Diff_Single;
var x, y: TArray<Single>;
const N = 10000000;
begin
  SetLength(x, N);
  SetLength(y, N);

  SWStart;
  Differences(PSingle(x), PSingle(y), Length(x));
  SWStop;
end;

procedure TVecMathPerformance.DiffWithStep_Single;
var x, y: TArray<Single>;
const N = 10000000;
begin
  SetLength(x, N);
  SetLength(y, N);

  SWStart;
  Differences(PSingle(x), PSingle(y), 2, 2, Length(x));
  SWStop;
end;

{$endregion}


initialization

  RegisterTest(TVecMathPerformance.Suite);

end.
