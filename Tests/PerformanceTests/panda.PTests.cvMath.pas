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

initialization

  RegisterTest(TCVMathTests.Suite);

end.
