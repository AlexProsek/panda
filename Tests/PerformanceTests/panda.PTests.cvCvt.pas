unit panda.PTests.cvCvt;

interface

uses
    TestFramework
  , panda.Tests.NDATestCase
  , panda.cvCvt
  ;

type
  TCVCvtTests = class(TNDAPerformanceTestCase)
  published
    procedure Cvt_I32F32;
    procedure Cvt_F32F64;
    procedure Cvt_F64F32;
  end;

implementation

procedure TCVCvtTests.Cvt_I32F32;
var x: TArray<Integer>;
    y: TArray<Single>;
const N = 10000000;
begin
  SetLength(x, N);
  SetLength(y, N);

  SWStart;
  cvt(PInteger(x), PSingle(y), Length(x));
  SWStop;
end;

procedure TCVCvtTests.Cvt_F32F64;
var x: TArray<Single>;
    y: TArray<Double>;
const N = 10000000;
begin
  SetLength(x, N);
  SetLength(y, N);

  SWStart;
  cvt(PSingle(x), PDouble(y), Length(x));
  SWStop;
end;

procedure TCVCvtTests.Cvt_F64F32;
var x: TArray<Double>;
    y: TArray<Single>;
const N = 10000000;
begin
  SetLength(x, N);
  SetLength(y, N);

  SWStart;
  cvt(PDouble(x), PSingle(y), Length(x));
  SWStop;
end;

initialization

  RegisterTest(TCVCvtTests.Suite);

end.
