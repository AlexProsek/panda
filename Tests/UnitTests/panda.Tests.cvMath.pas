unit panda.Tests.cvMath;

interface

uses
    TestFramework
  , panda.Tests.NDATestCase
  , panda.cvMath
  , System.SysUtils
  ;

type
  TTestVecMath = class(TNDATestCase)
  protected const
    stol = 1e-6;
    dtol = 1e-12;
  published
    procedure Total6_Int32;
    procedure Total8_Int32;
    procedure Total10_Int32;
    procedure Total6_Single;
    procedure Total8_Single;
    procedure Total10_Single;
    procedure Total2_Double;
    procedure Total4_Double;
    procedure Total6_Double;

    procedure AbsMax1_Double;
    procedure AbsMax2_Double;
    procedure AbsMax4_Double;
    procedure AbsMax10_Double;

    procedure MinMax3_Double;
    procedure MinMax4_Double;
    procedure MinMax9_Double;
  end;

implementation

{$region 'TTestVecMath'}

procedure TTestVecMath.Total6_Int32;
var x: TArray<Integer>;
    res: Integer;
begin
  x := TArray<Integer>.Create(1, 2, 3, 4, 5, 6);
  res := cvTotal(PInteger(x), Length(x));
  CheckEquals(21, res);
end;

procedure TTestVecMath.Total8_Int32;
var x: TArray<Integer>;
    res: Integer;
begin
  x := TArray<Integer>.Create(1, 2, 3, 4, 5, 6, 7, 8);
  res := cvTotal(PInteger(x), Length(x));
  CheckEquals(36, res);
end;

procedure TTestVecMath.Total10_Int32;
var x: TArray<Integer>;
    res: Integer;
begin
  x := TArray<Integer>.Create(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
  res := cvTotal(PInteger(x), Length(x));
  CheckEquals(55, res);
end;

procedure TTestVecMath.Total6_Single;
var x: TArray<Single>;
    res: Single;
begin
  x := TArray<Single>.Create(1, 2, 3, 4, 5, 6);
  res := cvTotal(PSingle(x), Length(x));
  CheckEquals(21, res);
end;

procedure TTestVecMath.Total8_Single;
var x: TArray<Single>;
    res: Single;
begin
  x := TArray<Single>.Create(1, 2, 3, 4, 5, 6, 7, 8);
  res := cvTotal(PSingle(x), Length(x));
  CheckEquals(36, res);
end;

procedure TTestVecMath.Total10_Single;
var x: TArray<Single>;
    res: Single;
begin
  x := TArray<Single>.Create(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
  res := cvTotal(PSingle(x), Length(x));
  CheckEquals(55, res);
end;

procedure TTestVecMath.Total2_Double;
var x: TArray<Double>;
    res: Double;
begin
  x := TArray<Double>.Create(1, 2);
  res := cvTotal(PDouble(x), Length(x));
  CheckEquals(3, res);
end;

procedure TTestVecMath.Total4_Double;
var x: TArray<Double>;
    res: Double;
begin
  x := TArray<Double>.Create(1, 2, 3, 4);
  res := cvTotal(PDouble(x), Length(x));
  CheckEquals(10, res);
end;

procedure TTestVecMath.Total6_Double;
var x: TArray<Double>;
    res: Double;
begin
  x := TArray<Double>.Create(1, 2, 3, 4, 5, 6);
  res := cvTotal(PDouble(x), Length(x));
  CheckEquals(21, res);
end;

procedure TTestVecMath.AbsMax1_Double;
var x: TArray<Double>;
    res: Double;
begin
  x := TArray<Double>.Create(-1);
  cvAbsMax(PDouble(x), Length(x), res);
  CheckEquals(1, res);
end;

procedure TTestVecMath.AbsMax2_Double;
var x: TArray<Double>;
    res: Double;
begin
  x := TArray<Double>.Create(-1, 2);
  cvAbsMax(PDouble(x), Length(x), res);
  CheckEquals(2, res);
end;

procedure TTestVecMath.AbsMax4_Double;
var x: TArray<Double>;
    res: Double;
begin
  x := TArray<Double>.Create(-1, 2, -4, 3);
  cvAbsMax(PDouble(x), Length(x), res);
  CheckEquals(4, res);
end;

procedure TTestVecMath.AbsMax10_Double;
var x: TArray<Double>;
    res: Double;
begin
  x := TArray<Double>.Create(-1, -5, 6, 4, 3, -7, 4, 5, -3, 1);
  cvAbsMax(PDouble(x), Length(x), res);
  CheckEquals(7, res);

  x := TArray<Double>.Create(-1, -5, 6, 4, 3, -7, 4, 5, -9, 1);
  cvAbsMax(PDouble(x), Length(x), res);
  CheckEquals(9, res);
end;

procedure TTestVecMath.MinMax3_Double;
var x: TArray<Double>;
    mi, ma: Double;
begin
  x := TArray<Double>.Create(1, -2, 3);
  cvMinMax(PDouble(x), Length(x), mi, ma);
  CheckEquals(-2, mi);
  CheckEquals(3, ma);
end;

procedure TTestVecMath.MinMax4_Double;
var x: TArray<Double>;
    mi, ma: Double;
begin
  x := TArray<Double>.Create(1, -2, 3, -4);
  cvMinMax(PDouble(x), Length(x), mi, ma);
  CheckEquals(-4, mi);
  CheckEquals(3, ma);
end;

procedure TTestVecMath.MinMax9_Double;
var x: TArray<Double>;
    mi, ma: Double;
begin
  x := TArray<Double>.Create(1, -2, 3, -3, 2, 5, -6, 1, 4);
  cvMinMax(PDouble(x), Length(x), mi, ma);
  CheckEquals(-6, mi);
  CheckEquals(5, ma);
end;

{$endregion}

initialization

  RegisterTest(TTestVecMath.Suite);

end.
