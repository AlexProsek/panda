unit panda.Tests.Nums;

interface

uses
    TestFramework
  , System.SysUtils
  , panda.Tests.NDATestCase
  , panda.Nums
  ;

type
  TCmplxTests = class(TNDATestCase)
  const
    cTol = 1e-8;
  published
    procedure AddCmplx64;
    procedure NegCmplx64;
    procedure MulCmplx64;
    procedure DivCmplx64;

    procedure DivCmplx128;
  end;

implementation

{$region 'TCmplxTests'}

procedure TCmplxTests.AddCmplx64;
var a, b, c: TCmplx64;
begin
  a.Init(1, 2);
  b.Init(3, 4);
  c := a + b;

  CheckEquals(4, c.Re);
  CheckEquals(6, c.Im);
end;

procedure TCmplxTests.NegCmplx64;
var a: TCmplx64;
begin
  a.Init(1, 2);
  a := -a;
  CheckEquals(-1, a.Re);
  CheckEquals(-2, a.Im);

  a := -a;
  CheckEquals(1, a.Re);
  CheckEquals(2, a.Im);
end;

procedure TCmplxTests.MulCmplx64;
var a, b, c: TCmplx64;
begin
  a.Init(1, 2);
  b.Init(3, 4);
  c := a * b;

  CheckEquals(-5, c.Re);
  CheckEquals(10, c.Im);
end;

procedure TCmplxTests.DivCmplx64;
var a, b, c: TCmplx64;
begin
  a.Init(1, 2);
  b.Init(3, 4);
  c := a / b;

  CheckEquals(0.44, c.Re, cEpsF32);
  CheckEquals(0.08, c.Im, cEpsF32);
end;

procedure TCmplxTests.DivCmplx128;
var a, b, c: TCmplx128;
begin
  a.Init(1, 2);
  b.Init(3, 4);
  c := a / b;

  CheckEquals(0.44, c.Re, cEpsF64);
  CheckEquals(0.08, c.Im, cEpsF64);
end;

{$endregion}

initialization

  RegisterTest(TCmplxTests.Suite)

end.
