unit panda.PTests.Poly;

interface

uses
    TestFramework
  , panda.Nums
  , panda.Poly
  , panda.Tests.NDATestCase
  , System.Math
  ;

type
  TPolyEvalTests = class(TNDAPerformanceTestCase)
  published
    procedure PolyEvalOrd6_Double;
    procedure PolyEvalOrd7_Double;
    procedure PolyEvalOrd7_Cmplx128;
  end;

implementation

procedure TPolyEvalTests.PolyEvalOrd6_Double;
var coeffs: TArray<Double>;
    N, cN: Integer;
    x: Double;
begin
  coeffs := TArray<Double>.Create(1, 2, 3, 4, 5, 6);
  cN := Length(coeffs);
  N := 1000000;
  x := 2;

  SWStart;
  while N > 0 do begin
    PolyEval(x, PDouble(coeffs), cN);
    Dec(N);
  end;
  SWStop;
end;

procedure TPolyEvalTests.PolyEvalOrd7_Double;
var coeffs: TArray<Double>;
    N, cN: Integer;
    x: Double;
begin
  coeffs := TArray<Double>.Create(1, 2, 3, 4, 5, 6, 7);
  cN := Length(coeffs);
  N := 1000000;
  x := 2;

  SWStart;
  while N > 0 do begin
    PolyEval(x, PDouble(coeffs), cN);
    Dec(N);
  end;
  SWStop;
end;

procedure TPolyEvalTests.PolyEvalOrd7_Cmplx128;
var coeffs: TArray<TCmplx128>;
    N, cN: Integer;
    x: TCmplx128;
begin
  coeffs := TArray<TCmplx128>.Create(1, 2, 3, 4, 5, 6, 7);
  cN := Length(coeffs);
  N := 1000000;
  x.Init(1, 2);

  SWStart;
  while N > 0 do begin
    PolyEval(x, PCmplx128(coeffs), cN);
    Dec(N);
  end;
  SWStop;
end;

initialization

  RegisterTest(TPolyEvalTests.Suite);

end.
