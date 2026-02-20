unit panda.Tests.cvCvt;

interface

uses
    TestFramework
  , panda.Tests.NDATestCase
  , panda.cvCvt
  , System.SysUtils
  ;

type
  TCvtTests = class(TNDATestCase)
  protected const
    stol = 1e-6;
    dtol = 1e-12;
  published
    procedure TestCvtUI8F32_2;
    procedure TestCvtUI8F32_4;
    procedure TestCvtUI8F32_6;
    procedure TestCvtF32UI8_2;
    procedure TestCvtF32UI8_4;
    procedure TestCvtF32UI8_6;
    procedure TestCvtI32F32_2;
    procedure TestCvtI32F32_4;
    procedure TestCvtI32F32_6;
    procedure TestCvtF32F64_2;
    procedure TestCvtF32F64_4;
    procedure TestCvtF32F64_6;
    procedure TestCvtF64F32_2;
    procedure TestCvtF64F32_4;
    procedure TestCvtF64F32_6;
  end;

  TByteSwappingTests = class(TNDATestCase)
  published
    procedure SwapInPlace16_4;
    procedure SwapInPlace16_5;
    procedure SwapInPlace32_4;
    procedure SwapInPlace64_2;
  end;

implementation

{$region 'TCvtTests'}

procedure TCvtTests.TestCvtUI8F32_2;
var x: TArray<Byte>;
    y: TArray<Single>;
begin
  x := TArray<Byte>.Create(1, 2);
  y := TArray<Single>.Create(0, 0);

  cvt(PByte(x), PSingle(y), Length(x));

  CheckEquals(1, y[0], stol);
  CheckEquals(2, y[1], stol);
end;

procedure TCvtTests.TestCvtUI8F32_4;
var x: TArray<Byte>;
    y: TArray<Single>;
begin
  x := TArray<Byte>.Create(1, 2, 3, 4);
  y := TArray<Single>.Create(0, 0, 0, 0);

  cvt(PByte(x), PSingle(y), Length(x));

  CheckEquals(1, y[0], stol);
  CheckEquals(2, y[1], stol);
  CheckEquals(3, y[2], stol);
  CheckEquals(4, y[3], stol);
end;

procedure TCvtTests.TestCvtUI8F32_6;
var x: TArray<Byte>;
    y: TArray<Single>;
begin
  x := TArray<Byte>.Create(1, 2, 3, 4, 5, 6);
  y := TArray<Single>.Create(0, 0, 0, 0, 0, 0);

  cvt(PByte(x), PSingle(y), Length(x));

  CheckEquals(1, y[0], stol);
  CheckEquals(2, y[1], stol);
  CheckEquals(3, y[2], stol);
  CheckEquals(4, y[3], stol);
  CheckEquals(5, y[4], stol);
  CheckEquals(6, y[5], stol);
end;

procedure TCvtTests.TestCvtF32UI8_2;
var x: TArray<Single>;
    y: TArray<Byte>;
begin
  x := TArray<Single>.Create(1, 2);
  y := TArray<Byte>.Create(0, 0);

  cvt(PSingle(x), PByte(y), Length(x));

  CheckEquals(1, y[0]);
  CheckEquals(2, y[1]);


  x := TArray<Single>.Create(-1, 300);
  y := TArray<Byte>.Create(0, 0);

  cvt(PSingle(x), PByte(y), Length(x));

  CheckEquals(0,   y[0]);
  CheckEquals(255, y[1]);
end;

procedure TCvtTests.TestCvtF32UI8_4;
var x: TArray<Single>;
    y: TArray<Byte>;
begin
  x := TArray<Single>.Create(-1, 2, 300, 4);
  y := TArray<Byte>.Create(0, 0, 0, 0);

  cvt(PSingle(x), PByte(y), Length(x));

  CheckEquals(0,   y[0]);
  CheckEquals(2,   y[1]);
  CheckEquals(255, y[2]);
  CheckEquals(4,   y[3]);
end;

procedure TCvtTests.TestCvtF32UI8_6;
var x: TArray<Single>;
    y: TArray<Byte>;
begin
  x := TArray<Single>.Create(1, -2, 3, 400, 5, -1);
  y := TArray<Byte>.Create(0, 0, 0, 0, 0, 0);

  cvt(PSingle(x), PByte(y), Length(x));

  CheckEquals(1,   y[0]);
  CheckEquals(0,   y[1]);
  CheckEquals(3,   y[2]);
  CheckEquals(255, y[3]);
  CheckEquals(5,   y[4]);
  CheckEquals(0,   y[5]);
end;

procedure TCvtTests.TestCvtI32F32_2;
var x: TArray<Integer>;
    y: TArray<Single>;
begin
  x := TArray<Integer>.Create(1, 2);
  y := TArray<Single>.Create(0, 0);

  cvt(PInteger(x), PSingle(y), Length(x));

  CheckEquals(1, y[0], stol);
  CheckEquals(2, y[1], stol);
end;

procedure TCvtTests.TestCvtI32F32_4;
var x: TArray<Integer>;
    y: TArray<Single>;
begin
  x := TArray<Integer>.Create(1, 2, 3, 4);
  y := TArray<Single>.Create(0, 0, 0, 0);

  cvt(PInteger(x), PSingle(y), Length(x));

  CheckEquals(1, y[0], stol);
  CheckEquals(2, y[1], stol);
  CheckEquals(3, y[2], stol);
  CheckEquals(4, y[3], stol);
end;

procedure TCvtTests.TestCvtI32F32_6;
var x: TArray<Integer>;
    y: TArray<Single>;
begin
  x := TArray<Integer>.Create(1, 2, 3, 4, 5, 6);
  y := TArray<Single>.Create(0, 0, 0, 0, 0, 0);

  cvt(PInteger(x), PSingle(y), Length(x));

  CheckEquals(1, y[0], stol);
  CheckEquals(2, y[1], stol);
  CheckEquals(3, y[2], stol);
  CheckEquals(4, y[3], stol);
  CheckEquals(5, y[4], stol);
  CheckEquals(6, y[5], stol);
end;

procedure TCvtTests.TestCvtF32F64_2;
var x: TArray<Single>;
    y: TArray<Double>;
begin
  x := TArray<Single>.Create(1, 2);
  y := TArray<Double>.Create(0, 0);

  cvt(PSingle(x), PDouble(y), Length(x));

  CheckEquals(1, y[0], stol);
  CheckEquals(2, y[1], stol);
end;

procedure TCvtTests.TestCvtF32F64_4;
var x: TArray<Single>;
    y: TArray<Double>;
begin
  x := TArray<Single>.Create(1, 2, 3, 4);
  y := TArray<Double>.Create(0, 0, 0, 0);

  cvt(PSingle(x), PDouble(y), Length(x));

  CheckEquals(1, y[0], stol);
  CheckEquals(2, y[1], stol);
  CheckEquals(3, y[2], stol);
  CheckEquals(4, y[3], stol);
end;

procedure TCvtTests.TestCvtF32F64_6;
var x: TArray<Single>;
    y: TArray<Double>;
begin
  x := TArray<Single>.Create(1, 2, 3, 4, 5, 6);
  y := TArray<Double>.Create(0, 0, 0, 0, 0, 0);

  cvt(PSingle(x), PDouble(y), Length(x));

  CheckEquals(1, y[0], stol);
  CheckEquals(2, y[1], stol);
  CheckEquals(3, y[2], stol);
  CheckEquals(4, y[3], stol);
  CheckEquals(5, y[4], stol);
  CheckEquals(6, y[5], stol);
end;

procedure TCvtTests.TestCvtF64F32_2;
var x: TArray<Double>;
    y: TArray<Single>;
begin
  x := TArray<Double>.Create(1, 2);
  y := TArray<Single>.Create(0, 0);

  cvt(PDouble(x), PSingle(y), Length(x));

  CheckEquals(1, y[0], stol);
  CheckEquals(2, y[1], stol);
end;

procedure TCvtTests.TestCvtF64F32_4;
var x: TArray<Double>;
    y: TArray<Single>;
begin
  x := TArray<Double>.Create(1, 2, 3, 4);
  y := TArray<Single>.Create(0, 0, 0, 0);

  cvt(PDouble(x), PSingle(y), Length(x));

  CheckEquals(1, y[0], stol);
  CheckEquals(2, y[1], stol);
  CheckEquals(3, y[2], stol);
  CheckEquals(4, y[3], stol);
end;

procedure TCvtTests.TestCvtF64F32_6;
var x: TArray<Double>;
    y: TArray<Single>;
begin
  x := TArray<Double>.Create(1, 2, 3, 4, 5, 6);
  y := TArray<Single>.Create(0, 0, 0, 0, 0, 0);

  cvt(PDouble(x), PSingle(y), Length(x));

  CheckEquals(1, y[0], stol);
  CheckEquals(2, y[1], stol);
  CheckEquals(3, y[2], stol);
  CheckEquals(4, y[3], stol);
  CheckEquals(5, y[4], stol);
  CheckEquals(6, y[5], stol);
end;

{$endregion}

{$region 'TByteSwappingTests'}

procedure TByteSwappingTests.SwapInPlace16_4;
var data: TArray<Word>;
begin
  data := TArray<Word>.Create($0102, $0203, $0304, $0405);
  bswap16(PByte(data), PByte(data), Length(data) * SizeOf(Word));

  CheckEquals($0201, data[0]);
  CheckEquals($0302, data[1]);
  CheckEquals($0403, data[2]);
  CheckEquals($0504, data[3]);
end;

procedure TByteSwappingTests.SwapInPlace16_5;
var data: TArray<Word>;
begin
  data := TArray<Word>.Create($0102, $0203, $0304, $0405, $0506);
  bswap16(PByte(data), PByte(data), Length(data) * SizeOf(Word));

  CheckEquals($0201, data[0]);
  CheckEquals($0302, data[1]);
  CheckEquals($0403, data[2]);
  CheckEquals($0504, data[3]);
  CheckEquals($0605, data[4]);
end;

procedure TByteSwappingTests.SwapInPlace32_4;
var data: TArray<Integer>;
begin
  data := TArray<Integer>.Create($01020304, $02030405, $03040506, $04050607);
  bswap32(PByte(data), PByte(data), Length(data) * SizeOf(Integer));

  CheckEquals($04030201, data[0]);
  CheckEquals($05040302, data[1]);
  CheckEquals($06050403, data[2]);
  CheckEquals($07060504, data[3]);
end;

procedure TByteSwappingTests.SwapInPlace64_2;
var data: TArray<Int64>;
begin
  data := TArray<Int64>.Create($0102030405060708, $0203040506070809);
  bswap64(PByte(data), PByte(data), Length(data) * SizeOf(Int64));

  CheckEquals($0807060504030201, data[0]);
  CheckEquals($0908070605040302, data[1]);
end;

{$endregion}

initialization

  RegisterTest(TCvtTests.Suite);
  RegisterTest(TByteSwappingTests.Suite);

end.
