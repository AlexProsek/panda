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

implementation

{$region 'TCvtTests'}

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

initialization

  RegisterTest(TCvtTests.Suite);

end.
