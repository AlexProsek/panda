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

{$endregion}

initialization

  RegisterTest(TTestVecMath.Suite);

end.
