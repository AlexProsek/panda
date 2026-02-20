unit panda.PTests.cvMath;

interface

uses
    TestFramework
  , panda.Tests.NDATestCase
  , panda.cvMath
  ;

type
  TCVMathTests = class(TNDAPerformanceTestCase)
  published
    procedure Total_Int32;
    procedure Total_Single;
    procedure Total_Double;

    procedure AbsMax_Double;

    procedure MinMax_Double;
  end;

implementation

procedure TCVMathTests.Total_Int32;
var x: TArray<Integer>;
const N = 10000000;
begin
  SetLength(x, N);

  SWStart;
  cvTotal(PInteger(x), Length(x));
  SWStop;
end;


procedure TCVMathTests.Total_Single;
var x: TArray<Single>;
const N = 10000000;
begin
  SetLength(x, N);

  SWStart;
  cvTotal(PSingle(x), Length(x));
  SWStop;
end;

procedure TCVMathTests.Total_Double;
var x: TArray<Double>;
const N = 10000000;
begin
  SetLength(x, N);

  SWStart;
  cvTotal(PDouble(x), Length(x));
  SWStop;
end;

procedure TCVMathTests.AbsMax_Double;
var x: TArray<Double>;
    m: Double;
const N = 10000000;
begin
  SetLength(x, N);

  SWStart;
  cvAbsMax(PDouble(x), Length(x), m);
  SWStop;
end;

procedure TCVMathTests.MinMax_Double;
var x: TArray<Double>;
    mi, ma: Double;
const N = 10000000;
begin
  SetLength(x, N);

  SWStart;
  cvMinMax(PDouble(x), Length(x), mi, ma);
  SWStop;
end;

initialization

  RegisterTest(TCVMathTests.Suite);

end.
