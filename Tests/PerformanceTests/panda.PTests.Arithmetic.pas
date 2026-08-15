unit panda.PTests.Arithmetic;

interface

uses
    TestFramework
  , panda.Tests.NDATestCase
  , panda.Arithmetic
  , panda.Intfs
  , panda.Arrays
  , panda.Nums
  ;

type
  TTensorArithTests = class(TNDAPerformanceTestCase)
  published
    procedure CmplxDivWithStrides;
  end;

implementation

procedure TTensorArithTests.CmplxDivWithStrides;
var a, b: INDArray<TCmplx128>;
    ta, tb: TTensorC128;
const N = 1000000;
begin
  a := TNDAUt.Full<TCmplx128>([2*N], 1);
  b := TNDAUt.Full<TCmplx128>([2*N], 2);
  ta := a[[NDIAll(2)]];
  tb := b[[NDIAll(2)]];

  SWStart;
  DoTestLoop(procedure begin ta / tb end, 20);
  SWStop;
end;

initialization

  RegisterTest(TTensorArithTests.Suite);

end.
