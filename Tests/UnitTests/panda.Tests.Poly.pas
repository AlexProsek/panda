unit panda.Tests.Poly;

interface

uses
    TestFramework
  , panda.Nums
  , panda.Poly
  , panda.Tests.NDATestCase
  ;
type
  TPolyEvalTests = class(TNDATestCase)
  published
    procedure EvalOrd0_Double;
    procedure EvalOrd1_Double;
    procedure EvalOrd2_Double;
    procedure EvalOrd3_Double;
    procedure EvalOrd4_Double;
    procedure EvalOrd5_Double;

    procedure EvalOrd0_Cmplx128;
    procedure EvalOrd1_Cmplx128;
    procedure EvalOrd2_Cmplx128;
  end;

  TPolyF64Tests = class(TNDATestCase)
  protected const
    dTol = 1e-14;
  published
    procedure PolyEval;
    procedure PolyAddition;
    procedure PolyAddConst;
    procedure PolySubtraction;
    procedure PolyMultiplication;
    procedure PolyMulByOrd0;
    procedure PolyQuotientRem;
    procedure PolyShiftLeft;
    procedure PolyShiftRight;
    procedure PolySubstConst;
    procedure PolySubst;
    procedure PolyDeriv;
  end;

implementation

{$region 'TPolyEvalTests'}

procedure TPolyEvalTests.EvalOrd0_Double;
var coeffs: TArray<Double>;
    x: Double;
begin
  coeffs := TArray<Double>.Create(1);
  x := 2;
  PolyEval(x, PDouble(coeffs), 1);
  CheckEquals(1, x);
end;

procedure TPolyEvalTests.EvalOrd1_Double;
var coeffs: TArray<Double>;
    x: Double;
begin
  coeffs := TArray<Double>.Create(1, 2);
  x := 2;
  PolyEval(x, PDouble(coeffs), 2);
  CheckEquals(5, x);
end;

procedure TPolyEvalTests.EvalOrd2_Double;
var coeffs: TArray<Double>;
    x: Double;
begin
  coeffs := TArray<Double>.Create(1, 2, 3);
  x := 2;
  PolyEval(x, PDouble(coeffs), 3);
  CheckEquals(17, x);
end;

procedure TPolyEvalTests.EvalOrd3_Double;
var coeffs: TArray<Double>;
    x: Double;
begin
  coeffs := TArray<Double>.Create(1, 2, 3, 4);
  x := 2;
  PolyEval(x, PDouble(coeffs), 4);
  CheckEquals(49, x);
end;

procedure TPolyEvalTests.EvalOrd4_Double;
var coeffs: TArray<Double>;
    x: Double;
begin
  coeffs := TArray<Double>.Create(1, 2, 3, 4, 5);
  x := 2;
  PolyEval(x, PDouble(coeffs), 5);
  CheckEquals(129, x);
end;

procedure TPolyEvalTests.EvalOrd5_Double;
var coeffs: TArray<Double>;
    x: Double;
begin
  coeffs := TArray<Double>.Create(1, 2, 3, 4, 5, 6);
  x := 2;
  PolyEval(x, PDouble(coeffs), 6);
  CheckEquals(321, x);
end;

procedure TPolyEvalTests.EvalOrd0_Cmplx128;
var coeffs: TArray<TCmplx128>;
    x: TCmplx128;
begin
  SetLength(coeffs, 1);
  coeffs[0].Init(1, 2);
  x.Init(3, 4);

  PolyEval(x, PCmplx128(coeffs), 1);
  CheckEquals(1, x.Re);
  CheckEquals(2, x.Im);
end;

procedure TPolyEvalTests.EvalOrd1_Cmplx128;
var coeffs: TArray<TCmplx128>;
    x: TCmplx128;
begin
  SetLength(coeffs, 2);
  coeffs[0].Init(1, 2);
  coeffs[1].Init(3, 4);
  x.Init(1, 2);

  PolyEval(x, PCmplx128(coeffs), 2);
  CheckEquals(-4, x.Re);
  CheckEquals(12, x.Im);
end;

procedure TPolyEvalTests.EvalOrd2_Cmplx128;
var coeffs: TArray<TCmplx128>;
    x: TCmplx128;
begin
  SetLength(coeffs, 3);
  coeffs[0].Init(1, 2);
  coeffs[1].Init(3, 4);
  coeffs[2].Init(5, 6);
  x.Init(1, 2);

  PolyEval(x, PCmplx128(coeffs), 3);
  CheckEquals(-43, x.Re);
  CheckEquals( 14, x.Im);
end;

{$endregion}

{$region 'TPolyF64Tests'}

procedure TPolyF64Tests.PolyEval;
var p: TPolyF64;
begin
  p.Init([1]);
  CheckEquals(1, p[0]);
  CheckEquals(1, p[1]);

  p.Init([1, 2]);
  CheckEquals(1, p[0]);
  CheckEquals(3, p[1]);
end;

procedure TPolyF64Tests.PolyAddition;
var a, b, c: TPolyF64;
begin
  a.Init([1, 2]);
  b.Init([3, 4, 5]);
  c := a + b;
  CheckEquals(2, c.Order);
  CheckEquals([4, 6, 5], c.CoeffList);

  c := b + a;
  CheckEquals(2, c.Order);
  CheckEquals([4, 6, 5], c.CoeffList);

  b.Init([2, -2]);
  c := a + b;
  CheckEquals([3], c.CoeffList);

  b.Init([-1, -2]);
  c := a + b;
  CheckEquals([0], c.CoeffList);
end;

procedure TPolyF64Tests.PolyAddConst;
var x, p: TPolyF64;
begin
  x := TPolyF64.PolyX;
  p := x + 2;
  CheckEquals([2, 1], p.CoeffList);
end;

procedure TPolyF64Tests.PolySubtraction;
var a, b, c: TPolyF64;
begin
  a.Init([1, 2]);
  b.Init([3, 4, 5]);
  c := a - b;
  CheckEquals([-2, -2, -5], c.CoeffList);

  c := b - a;
  CheckEquals([2, 2, 5], c.CoeffList);

  c := b - b;
  CheckEquals([0], c.CoeffList);
end;

procedure TPolyF64Tests.PolyMultiplication;
var a, b, c: TPolyF64;
begin
  a.Init([1, 2]);
  b.Init([3, 4]);
  c := a * b;
  CheckEquals([3, 10, 8], c.CoeffList);

  b.Init([3, 4, 5]);
  c := a * b;
  CheckEquals([3, 10, 13, 10], c.CoeffList);

  c := b * a;
  CheckEquals([3, 10, 13, 10], c.CoeffList);
end;

procedure TPolyF64Tests.PolyMulByOrd0;
var a, b, c: TPolyF64;
begin
  a.Init([1, 2]);
  b.Init([3]);
  c := a * b;
  CheckEquals([3, 6], c.CoeffList);

  c := b * a;
  CheckEquals([3, 6], c.CoeffList);
end;

procedure TPolyF64Tests.PolyQuotientRem;
var a, b, q, r: TPolyF64;
begin
  a.Init([1, 2, 0, 0, 1]); // 1 + 2x + x^4
  b.Init([1, 0, 1]);       // 1 + x^2
  TPolyF64.GetQuotientRemainder(a, b, q, r);

  CheckEquals([-1, 0, 1], q.CoeffList, dTol);
  CheckEquals([2, 2], r.CoeffList, dTol);
end;

procedure TPolyF64Tests.PolyShiftLeft;
var p, q: TPolyF64;
begin
  p.Init([1, 2, 3]);
  q := p.Shift(1);
  CheckEquals([0, 1, 2, 3], q.CoeffList);
end;

procedure TPolyF64Tests.PolyShiftRight;
var p, q: TPolyF64;
begin
  p.Init([1, 2, 3]);
  q := p.Shift(-1);
  CheckEquals([2, 3], q.CoeffList);

  q := p.Shift(-3);
  CheckEquals([0], q.CoeffList);
end;

procedure TPolyF64Tests.PolySubstConst;
var p, q: TPolyF64;
begin
  p.Init([1, 2, 3]);
  q.Init([4]);
  p := p.Substitute(q);
  CheckEquals([57], p.CoeffList);
end;

procedure TPolyF64Tests.PolySubst;
var p, q: TPolyF64;
begin
  p.Init([1, 2, 3]);
  q.Init([4, 5, 6]);
  p := p.Substitute(q);
  CheckEquals([57, 130, 231, 180, 108], p.CoeffList);
end;

procedure TPolyF64Tests.PolyDeriv;
var p: TPolyF64;
begin
  p.Init([1, 2, 3]);
  p := p.Derivative();
  CheckEquals([2, 6], p.CoeffList);
end;

{$endregion}

initialization

  RegisterTest(TPolyEvalTests.Suite);
  RegisterTest(TPolyF64Tests.Suite);

end.
