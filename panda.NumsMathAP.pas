unit panda.NumsMathAP;

interface

uses
    Math
  , SysUtils
  , panda.NumsAP
  , panda.NumsLowLvl
  ;

function EvalPi(aBinDigits: Integer): TReal;
function RealSin(const aA: TReal): TReal;
function RealCos(const aA: TReal): TReal;

function RealPower(const aA, aB: TReal): TReal; overload;
function RealExp(const aA: TReal): TReal;
function RealLn(const aA: TReal): TReal;
function RealLog2(const aA: TReal): TReal;
function RealLog10(const aA: TReal): TReal;

implementation

function EvalPi(aBinDigits: Integer): TReal;
var p, pw, k: Integer;
    a, b, t, half, an, bn, diff: TReal;
begin
  Assert(aBinDigits > 0);

  p := aBinDigits;
  if p <= 53 then begin
    Result.Init(Pi, p);
    Result := Result.BinaryRoundTo(p);
    exit;
  end;

  // One extra limb, same guard as RealExp and RealSqrt.
  pw := p + W_BIT_COUNT;
  half.Init(0.5, pw);
  a := TReal.One(pw);
  b := RealSqrt(half);
  t.Init(0.25, pw);
  k := 0;

  repeat
    an := (a + b) * half;
    bn := RealSqrt(a * b);
    diff := a - an;
    diff := diff * diff;
    diff.Exponent := diff.Exponent + k;
    t := t - diff;
    Inc(k);
    a := an;
    b := bn;
    diff := a - b;
  // Exponent settles at -pw when a and b differ by one unit in the last place.
  until diff.IsZeroQ or (diff.Exponent <= -pw);

  a := a + b;
  t.Exponent := t.Exponent + 2;
  Result := (a * a) / t;
  Result := Result.BinaryRoundTo(p);
end;

function InQuarterPi(const aA: TReal): Boolean;
begin
  if aA.IsZeroQ or (aA.Exponent < -1) then exit(True);
  if aA.Exponent > -1 then exit(False);
  Result := Abs(aA.AsDouble) <= Pi / 4;
end;

function QuadrantIndex(const aN: TInteger): Integer;
var m: TInteger;
begin
  m := aN.Abs mod 4;
  Result := Integer(m);
  if aN.Signum < 0 then
    Result := (4 - Result) and 3;
end;

// aR is in [-Pi/4, Pi/4]. aQuad is n mod 4 for aA = n * (Pi/2) + aR.
procedure ReduceToQuarterPi(const aA: TReal; aPw: Integer;
  out aR: TReal; out aQuad: Integer);
var ea, piBits: Integer;
    x, halfPi, quarter, q, half: TReal;
    n: TInteger;
begin
  if InQuarterPi(aA) then begin
    aR := aA.BinaryRoundTo(aPw);
    aQuad := 0;
    exit;
  end;

  ea := aA.Exponent;
  piBits := aPw + Max(ea + 1, 0);
  halfPi := EvalPi(piBits);
  halfPi.Exponent := halfPi.Exponent - 1;

  x := aA.BinaryRoundTo(piBits);
  q := x / halfPi;
  half.Init(0.5, q.BinDigitCount);
  if q.Signum < 0 then
    n := (q - half).IntegerPart
  else
    n := (q + half).IntegerPart;
  aR := x - n * halfPi;

  quarter := halfPi;
  quarter.Exponent := quarter.Exponent - 1;
  if aR.Abs > quarter then begin
    if aR.Signum < 0 then begin
      aR := aR + halfPi;
      n := n - 1;
    end else begin
      aR := aR - halfPi;
      n := n + 1;
    end;
  end;

  aQuad := QuadrantIndex(n);
  aR := aR.BinaryRoundTo(aPw);
end;

function SinSeries(const aR: TReal; aPw: Integer): TReal;
var y, term: TReal;
    den: TInteger;
    k, lim: Integer;
    kk: Int64;
begin
  if aR.IsZeroQ then exit(TReal.Zero(aPw));

  y := aR * aR;
  Result := aR;
  term := aR;
  lim := aR.Exponent - aPw;
  k := 0;
  repeat
    Inc(k);
    kk := k;
    den := (2 * kk) * (2 * kk + 1);
    term := term * y / den;
    term.Signum := -term.Signum;
    Result := Result + term;
  until term.IsZeroQ or (term.Exponent < lim);
end;

function CosSeries(const aR: TReal; aPw: Integer): TReal;
var y, term: TReal;
    den: TInteger;
    k, lim: Integer;
    kk: Int64;
begin
  y := aR * aR;
  Result := TReal.One(aPw);
  term := Result;
  lim := -aPw;
  k := 0;
  repeat
    Inc(k);
    kk := k;
    den := (2 * kk - 1) * (2 * kk);
    term := term * y / den;
    term.Signum := -term.Signum;
    Result := Result + term;
  until term.IsZeroQ or (term.Exponent < lim);
end;

function QuadrantValue(const aSinVal, aCosVal: TReal; aQuad: Integer;
  aWantSin: Boolean): TReal;
begin
  case aQuad of
    0: if aWantSin then Result := aSinVal else Result := aCosVal;
    1: if aWantSin then Result := aCosVal else Result := -aSinVal;
    2: if aWantSin then Result := -aSinVal else Result := -aCosVal;
  else
    if aWantSin then Result := -aCosVal else Result := aSinVal;
  end;
end;

function RealSin(const aA: TReal): TReal;
var p, pw, quad: Integer;
    r, s, c: TReal;
begin
  p := aA.BinDigitCount;
  if aA.IsZeroQ then exit(TReal.Zero(p));
  if (p <= 53) and InQuarterPi(aA) then begin
    Result.Init(Sin(aA.AsDouble), p);
    Result := Result.BinaryRoundTo(p);
    exit;
  end;

  pw := p + W_BIT_COUNT;
  ReduceToQuarterPi(aA, pw, r, quad);
  s := SinSeries(r, pw);
  c := CosSeries(r, pw);
  Result := QuadrantValue(s, c, quad, True);
  Result := Result.BinaryRoundTo(p);
end;

function RealCos(const aA: TReal): TReal;
var p, pw, quad: Integer;
    r, s, c: TReal;
begin
  p := aA.BinDigitCount;
  if aA.IsZeroQ then exit(TReal.One(p));
  if (p <= 53) and InQuarterPi(aA) then begin
    Result.Init(Cos(aA.AsDouble), p);
    Result := Result.BinaryRoundTo(p);
    exit;
  end;

  pw := p + W_BIT_COUNT;
  ReduceToQuarterPi(aA, pw, r, quad);
  s := SinSeries(r, pw);
  c := CosSeries(r, pw);
  Result := QuadrantValue(s, c, quad, False);
  Result := Result.BinaryRoundTo(p);
end;

function RealPower(const aA, aB: TReal): TReal;
begin
  Result := RealExp(aB * RealLn(aA));
end;

{$ifdef FPC}

function RealPower(const aA: TReal; aB: Integer): TReal;
var ib: TInteger;
begin
  ib.Init(aB);
  Result := RealPower(aA, ib);
end;

{$endif}

function ExpIterCount(aPrecision: Integer): Integer;
var n, lgn2: Integer;
    x0, x1, tmp: Double;
const             // precision: 0,32, 64, 96,128,160,192,224,256,288
  C: array [0..9] of Integer = (0, 8, 15, 20, 25, 30, 35, 39, 43, 47);
  T: Double = -0.5573049591110367;

  function F(const X: Double): Double;
  begin
    Result := Log2(X) - aPrecision / X - T;
  end;

begin
  n := aPrecision shr 5;
  if n < 10 then exit(C[n]);

  n := 32;
  lgn2 := 5;
  while n * (lgn2 - T) < aPrecision do begin
    n := n shl 1;
    Inc(lgn2);
  end;

  x1 := n;
  x0 := n / 2;
  tmp := 1;
  while Abs(tmp) > 0.5 do begin
    tmp := F(x1) * (x1 - x0) / (F(x1) - F(x0));
    x0 := x1;
    x1 := x1 - tmp;
  end;

  Result := Ceil(x1 + 1);
end;

function RealExp(const aA: TReal): TReal;
var J, iterCnt, p, pw, ea: Integer;
    X, u: TReal;
    i: TInteger;
begin
  ea := aA.Exponent;
  p := aA.BinDigitCount;
  pw := p + W_BIT_COUNT;

  u := TReal.One(pw);
  Result := TReal.One(pw);
  X := aA.BinaryRoundTo(pw);
  if ea >= 0 then
    X.Exponent := -2;
  X.Signum := 1;

  iterCnt := ExpIterCount(p + 2);
  for J := iterCnt downto 1 do
    Result := Result * (X / J) + u;

  if ea >= 0 then begin
    i := TInteger.Power(2, ea + 2);
    Result := Result.IntPower(i);
  end;
  if aA.Signum < 1 then
    Result := Result.Reciprocal;
  Result := Result.BinaryRoundTo(p);
end;

function RealLn(const aA: TReal): TReal;
var p, pw, ea: Integer;
    A, t, yExp: TReal;
begin
  p := aA.BinDigitCount;
  pw := p + W_BIT_COUNT;
  ea := aA.Exponent;
  A := aA.BinaryRoundTo(pw);
  if Abs(ea) > 1 then
    A.Exponent := 0;
  Result.Init(Ln(A.AsDouble), pw);
  pw := -p;
  repeat
    yExp := RealExp(Result);
    t := 2 * (A - yExp) / (A + yExp);
    Result := Result + t;
  until (t.Exponent < pw) or t.IsZeroQ;

  if Abs(ea) > 1 then begin
    A.Init(2, Result.BinDigitCount);
    Result := Result + ea * RealLn(A);
  end;
  Result := Result.BinaryRoundTo(p);
end;

function RealLog2(const aA: TReal): TReal;
var b: TReal;
begin
  b.Init(2, aA.BinDigitCount);
  Result := RealLn(aA) / RealLn(b);
end;

function RealLog10(const aA: TReal): TReal;
var b: TReal;
begin
  b.Init(10, aA.BinDigitCount);
  Result := RealLn(aA) / RealLn(b);
end;

end.
