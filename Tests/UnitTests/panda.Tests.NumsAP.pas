unit panda.Tests.NumsAP;

interface

uses
    Math
  , SysUtils
  , TestFramework
  , panda.Tests.NDATestCase
  , panda.NumsLowLvl
  , panda.NumsQP
  , panda.NumsAP
  ;

{$I AsmDefs.inc}

type
  TBigIntegerTests = class(TNDATestCase)
  published
    procedure TestImplicitConvFromInt;
    procedure TestImplicitConvFromInt64;
    procedure TestExplicitConvToInt64;
    procedure TestTopBitPos;
    procedure TestAdd;
    procedure TestSubtract;
    procedure TestMultiply;
    procedure TestLongDivision;
    procedure TestLongDiv2;
    procedure TestShortDivision;
    procedure TestShortMod;
    procedure TestLongMod;
    procedure TestNeg;
    procedure TestNot;
    procedure TestShr;
    procedure TestShl;
    procedure TestLessThan;
    procedure TestGreatherThan;
    procedure TestToHexStr;
    procedure TestStrToInt;
    procedure TestDecParsing;
    procedure TestToDecString;
    procedure TestGCD;
    procedure TestGCD_2;
    procedure TestPower;
    procedure TestPiDigits;
    procedure TestKaratsubaMul;
    procedure TestKaratsubaPowerOfTwo;
    procedure TestKaratsubaMul2;
    procedure TestToDoubleCvt;
    procedure TestFloor;
    procedure TestCeil;
  end;

  TBigRationalTests = class(TNDATestCase)
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRationalConstruction;
    procedure TestRationalFromNotCoprimeIntegers;
    procedure TestMultiply;
    procedure TestAdd;
    procedure TestAdd2;
    procedure TestSubtract;
    procedure TestDivide;
    procedure TestLessThan;
    procedure TestToDoubleCvtWith64BitNumbers;
    procedure TestToDoubleCvtWithLargeNumbers;
    procedure TestRound;
  end;

  TBigRealTests = class(TNDATestCase)
  published
    procedure TestInitFromDouble;
    procedure TestInitFromDigits;
    procedure TestCvtFromBigInt;
    procedure TestInitRational;
    procedure TestAsDouble;
    procedure TestAddPosDbl;
    procedure TestAddNegDbl;
    procedure TestAddPosQuad;
    procedure TestAddPos160;
    procedure TestAddInt;
    procedure TestAddRounding;
    procedure TestSubRounding;
    procedure TestAddNegPosDbl;
    procedure TestSubtractDbl;
    procedure TestMultiplyDbl;
    procedure TestMultiplyByInt;
    procedure TestMulByZero;
    procedure TestDivideDbl;
    procedure TestSchoolDiv;
    procedure TestSchoolDiv128;
    procedure TestSchoolDiv192;
    procedure TestSchoolDiv1024;
    procedure TestNewtonDiv128;
    procedure TestNewtonDiv1024;
    procedure TestNoRoundingAdd128;
    procedure TestNoRoundingMul128;
    procedure TestDivideByInt;
    procedure TestIntDivide_D1;
    procedure TestIntDivide_D2;
    procedure TestNegate;
    procedure TestReciprocal;
    procedure TestReciprocal128;
    procedure TestReciprocal160;
    procedure TestReciprocal1024;
    procedure TestAbs;
    procedure TestBinRoundToWithExtendPrec;
    procedure TestBinRoundTo2;
    procedure TestBinRoundTo32;
    procedure TestBinFloorTo2;
    procedure TestBinFloorTo34;
    procedure TestBinFloorTo;
    procedure TestBinCeilTo2;
    procedure TestBinCeilTo34;
    procedure TestFloor;
    procedure TestCeil;
    procedure TestComparsion;
    procedure TestCmpIntReal;
    procedure TestInt2Real;
    procedure TestIntPower;
    procedure TestSquareRoot;
    procedure TestIntegerPart;
    procedure TestFractionalPart;
    procedure DecimalStrZeroExp;
    procedure DecimalStrNegExp;
    procedure DecimalStrPosExp;
    procedure TestNewtonIntDiv;
  end;

function GetFraction(const aV: TReal): TArray<UInt64>;

implementation

function GetFraction(const aV: TReal): TArray<UInt64>;
{$ifdef Limb64}
begin
   Result := aV.Fraction
end;
{$else}
var limbs: TLimbArray;
    count: Integer;
begin
  limbs := aV.Fraction;
  count := Length(Limbs);
  if count = 0 then exit(nil);

  if (count and 1) <> 0 then begin
    SetLength(Result, (Length(limbs) div 2) + 1);
    Move(limbs[0], (PByte(Result) + 4)^, 4 * Length(limbs));
  end else begin
    SetLength(Result, Length(limbs) div 2);
    Move(limbs[0], Result[0], 4 * Length(limbs));
  end;
 end;
{$endif}

{$region 'TBigIntegerTests'}

procedure TBigIntegerTests.TestTopBitPos;
var iPos: Integer;
    a: TInteger;
begin
  a := $1;
  iPos := a.HighestBitPos();
  CheckEquals(1, iPos);

  a := $2;
  iPos := a.HighestBitPos();
  CheckEquals(2, iPos);

  a := $4;
  iPos := a.HighestBitPos();
  CheckEquals(3, iPos);

  a := $80000000;
  iPos := a.HighestBitPos();
  CheckEquals(32, iPos);

  a := $100000000;
  iPos := a.HighestBitPos();
  CheckEquals(33, iPos);
end;

procedure TBigIntegerTests.TestAdd;
var a, b, c: TInteger;
begin
  a := 1;
  b := 1;
  c := a + b;
  CheckTrue((c.Count = 1) and (c[0] = 2) and (c.Signum = 1), '1 + 1');

  a := -1;
  b := 1;
  c := a + b;
  CheckTrue((c.Count = 1) and (c[0] = 0), '1 + (-1)');

  a := Cardinal($ffffffff);
  b := $1;
  c := a + b;
  CheckEquals($100000000, Int64(c), '$ffffffff + $1');

  a := $1ffffffff;
  b := $1;
  c := a + b;
  CheckEquals($200000000, Int64(c), '$1ffffffff + $1');

  a := -$ffffffff;
  b := -$1;
  c := a + b;
  CheckEquals(-$100000000, Int64(c), '-$ffffffff + (-$1)');
end;

procedure TBigIntegerTests.TestLongDivision;
var a, b, c: TInteger;
begin
{$ifdef Limb64}
  a := TLimbArray.Create($3333333333333333, $2222222222222222, $1111);
  b := TLimbArray.Create($1111111111111111, $3333);
  c := a div b;
  CheckEquals($5555e38e97b46522, UInt64(c));
{$else}
  a := $2222222222; b := $111111111;
  c := a div b;
  CheckEquals($2222222222 div $111111111, UInt64(c));

  a := TLimbArray.Create($33333333, $22222222, $1111);
  b := TLimbArray.Create($11111111, $3333);
  c := a div b;
  CheckEquals($5555e38e, UInt64(c));
{$endif}

  a := TLimbArray.Create(0, 2, 1);
  b := TLimbArray.Create(0, 1);
  c := a div b;
  CheckEquals(2, c[0]);
  CheckEquals(1, c[1]);
end;

procedure TBigIntegerTests.TestLongDiv2;
var a, c: TInteger;
begin
  TInteger.DecimalParse('10507348163952640000', a);
  c := a div a;
  CheckTrue(1 = c);
end;

procedure TBigIntegerTests.TestShortDivision;
var a, b, c: TInteger;
begin
  a := $2222222222;
  b := $11;
  c := a div b;
  CheckEquals($2222222222 div $11, UInt64(c));
end;

procedure TBigIntegerTests.TestShortMod;
var a, b, c: TInteger;
begin
  a := $2222222222;
  b := $11;
  c := a mod b;
  CheckEquals($2222222222 mod $11 {0}, UInt64(c));

  a := $22222222222;
  b := $11;
  c := a mod b;
  CheckEquals($22222222222 mod $11 {2}, UInt64(c));
end;

procedure TBigIntegerTests.TestLongMod;
var a, b, c: TInteger;
begin
{$ifdef Limb64}
  a.Init([$2222222222222222, $22]);
  b.Init([$1111111111111111, $1]);

  c := a mod b;

  CheckEquals(2, Int64(c));


  a.Init([$3333333333333333, $2222222222222222, $11111111]);
  b.Init([$1111111111111111, $33333333]);

  c := a mod b;

  CheckEquals($2f684bda,          c[1]);
  CheckEquals($16c16c16cae75920, c[0]);
{$else}
  a := $2222222222; b := $111111111;
  c := a mod b;
  CheckEquals($2222222222 mod $111111111, Int64(c),
    '$2222222222 mod $111111111');

  a := TLimbArray.Create($33333333, $22222222, $1111);
  b := TLimbArray.Create($11111111, $3333);
  c := a mod b;
  CheckEquals($1e5749f4a8c5, Int64(c),
    '$11112222222233333333 mod $333311111111');
{$endif}
end;

procedure TBigIntegerTests.TestExplicitConvToInt64;
var a: TInteger;
    b: Int64;
begin
{$ifdef Limb64}
  a.Init(TLimbArray.Create($100000000));
{$else}
  a.Init(TLimbArray.Create(0, 1));
{$endif}
  b := Int64(a);
  CheckEquals($100000000, b);
end;

procedure TBigIntegerTests.TestImplicitConvFromInt;
var b: Tinteger;
begin
  b := Integer(1);
  CheckEquals(1, b.Count);
  CheckEquals(1, b[0]);

  b := Integer(-1);
  CheckEquals(1, b.Count);
  CheckEquals(1, b[0]);
  CheckEquals(-1, b.Signum);
end;

procedure TBigIntegerTests.TestImplicitConvFromInt64;
var b: TInteger;
    i: Int64;
begin
  i := 1234;
  b := i;
  CheckEquals(i, Int64(b[0]));

{$ifdef Limb64}
  i := Int64(1) shl 32;
  b := i;
  CheckEquals(1, b.Count);
  CheckEquals($100000000, b[0]);

  i := Int64(-1);
  b := i;
  CheckEquals(-1, b.Signum);
  CheckEquals(1, b.Count);
  CheckEquals(1, b[0]);

  i := -(Int64(1) shl 32);
  b := i;
  CheckEquals(-1, b.Signum);
  CheckEquals(1, b.Count);
  CheckEquals($100000000, b[0]);
{$else}
  i := Int64(1) shl 32;
  b := i;
  CheckTrue((b[0] = 0) and (b[1] = 1));

  i := Int64(-1);
  b := i;
  CheckTrue((b[0] = 1) and (b.Signum = -1));

  i := -(Int64(1) shl 32);
  b := i;
  CheckTrue((b[0] = 0) and (b[1] = 1) and (b.Signum = -1));
{$endif}
end;

procedure TBigIntegerTests.TestLessThan;
var a, b: TInteger;
begin
  a := 1; b := 2;
  CheckEquals(True, a < b, '1 < 2');
  CheckEquals(False, b < a, '1 > 2');

  a := $100000000; b := 1;
  CheckEquals(False, a < b, '$100000000 < $1');
  CheckEquals(True, b < a, '$1 < $100000000');

  a := $222222221; b := $222222222;
  CheckEquals(True, a < b, '$222222221 < $222222222');
  CheckEquals(False, b < a, '$222222222 < $222222221');

  a := $222222222; b := $222222222;
  CheckEquals(False, a < b, '$222222222 < $222222222');

  a := -$222222221; b := -$222222222;
  CheckEquals(False, a < b, '-$222222221 < -$222222222');
  CheckEquals(True, b < a, '-$222222222 < -$222222221');

  a := -$222222222; b := -$222222222;
  CheckEquals(False, a < b, '-$222222222 < -$222222222');
end;

procedure TBigIntegerTests.TestGreatherThan;
var a, b: TInteger;
begin
  a := 1; b := 2;
  CheckEquals(False, a > b, '1 > 2');
  CheckEquals(True, b > a, '1 < 2');

  a := $100000000; b := 1;
  CheckEquals(True, a > b, '$100000000 > $1');
  CheckEquals(False, b > a, '$1 > $100000000');

  a := $222222221; b := $222222222;
  CheckEquals(False, a > b, '$222222221 > $222222222');
  CheckEquals(True, b > a, '$222222222 > $222222221');

  a := $222222222; b := $222222222;
  CheckEquals(False, a > b, '$222222222 > $222222222');

  a := -$222222221; b := -$222222222;
  CheckEquals(True, a > b, '-$222222221 > -$222222222');
  CheckEquals(False, b > a, '-$222222222 > -$222222221');

  a := -$222222222; b := -$222222222;
  CheckEquals(False, a > b, '-$222222222 > -$222222222');
end;

procedure TBigIntegerTests.TestMultiply;
var a, b, c: TInteger;
const //results were taken from Mathematica SW
  res1: array [0..3] of Cardinal = ($90f2a521, $dca5e208, $dc33f6ac, $14b66);
  res2: array [0..3] of Cardinal = ($e5618cf0, $2236d88f, $ad77d742, $121fa00);
begin
  a := 1;
  c := a * a;
  CheckTrue((c.Count = 1) and (c[0] = 1), '1 * 1');

  a := $ffffffff;
  b := $ffffffff;
  c := a * b;
  CheckEquals(UInt64($fffffffe00000001), UInt64(c), '$ffffffff * $ffffffff');

  a := $123456789abcdef;
  c := a * a;
  CheckEqualsMem(@res1[0], c.GetData, Length(res1) * SizeOf(Cardinal));

  b := UInt64($fedcba9876543210);
  c := a * b;
  CheckEqualsMem(@res2[0], c.GetData, Length(res2) * SizeOf(Cardinal));
end;

procedure TBigIntegerTests.TestNeg;
var a, b: TInteger;
begin
  a := $70000000;
  b := -a;
  CheckTrue((b.Signum = -a.Signum));
end;

procedure TBigIntegerTests.TestNot;
var a, b, c: Integer;
begin
  a := -3;
  b :=  1;
  c := a and b;

  c := (-3) and 1;
end;

procedure TBigIntegerTests.TestPower;
var res: TInteger;
begin
  res := TInteger.Power(2, 23);
  CheckEquals(Power(2, 23), UInt64(res), '2^23');
end;

procedure TBigIntegerTests.TestPiDigits;
var I: Integer;
    J: TInteger;
    res: TRational;
    sgn: Integer;
    s: String;
const N = 27;
begin
  J := 0;
  res := 0;
  sgn := 1;
  for I := 0 to N - 1 do begin
    res := res + TRational.Fraction(sgn, 2 * J + 1);
    Inc(J);
    sgn := -sgn;
  end;
  s := res.ToString;
  CheckEquals(s, '4078795356946236329749/5132792460157432044975');
end;

procedure TBigIntegerTests.TestKaratsubaMul;
var a, b, c, d: TInteger;
    km: TKaratsubaMul;
    s: String;
begin
  km.FastMulThreshold := 1;

  TInteger.HexParse('f2e3b19f7c045f15', a);
  TInteger.HexParse('1403f94edb1812b1', b);
  c := km.Eval(a, b);
  s := c.ToHexString;
  CheckEquals('12FD8F15D392A9BAD305C922EEAD3785', s);

  a := $123456789A;
  b := $23456789AB;
  c := km.Eval(a, b);
  s := c.ToDecimalString;
  CheckEquals('11844488030024963913950', s);

  d := km.Eval2(a, b);
  CheckTrue(c = d);

  TInteger.HexParse('100000002000000030000000', a);
  TInteger.HexParse('400000005000000060000000', b);
  c := km.Eval(a, b);
  s := c.ToHexString;
  CheckEquals('40000000D0000001C0000001B0000001200000000000000', s);

  d := km.Eval2(a, b);
  CheckTrue(c = d);

  a := '123456789123456789123456789';
  b := '987654321987654321987654321';
  c := km.Eval(a, b);
  s := c.ToDecimalString;
  CheckEquals('121932631356500531591068431581771069347203169112635269', s);

  d := km.Eval2(a, b);
  CheckTrue(c = d);

  a := '29422574294493211896241916386377884949';
  b := '269815013974968932907980669451170168686';
  c := km.Eval(a, b);
  s := c.ToDecimalString;
  c := a * b;
  s := c.ToDecimalString;

  d := km.Eval2(a, b);
  CheckTrue(c = d);
end;

procedure TBigIntegerTests.TestKaratsubaPowerOfTwo;
var a, c: TInteger;
    km: TKaratsubaMul;
begin
  km.FastMulThreshold := 1;

{$ifdef Limb64}
  a.Init([0, $1]);
  c := km.Eval(a, a);
  CheckEquals(3, c.Count);
  CheckEquals(0, c[0]);
  CheckEquals(0, c[1]);
  CheckEquals(1, c[2]);

  a.Init([0, $10]);
  c := km.Eval(a, a);
  CheckEquals(3, c.Count);
  CheckEquals(0, c[0]);
  CheckEquals(0, c[1]);
  CheckEquals($100, c[2]);

  a.Init([0, $1000000000000000]);
  c := km.Eval(a, a);
  CheckEquals(4, c.Count);
  CheckEquals(0, c[0]);
  CheckEquals(0, c[1]);
  CheckEquals(0, c[2]);
  CheckEquals($100000000000000, c[3]);
{$else}
  a := $100000000;
  c := km.Eval(a, a);
  CheckEquals('10000000000000000', c.ToHexString);

  a := $1000000000;
  c := km.Eval(a, a);
  CheckEquals('1000000000000000000', c.ToHexString);

  TInteger.HexParse('1000000000000000', a);
  c := km.Eval(a, a);
  CheckEquals('1000000000000000000000000000000', c.ToHexString);
{$endif}
end;

procedure TBigIntegerTests.TestKaratsubaMul2;
var a, c: TInteger;
    km: TKaratsubaMul;
begin
  TInteger.DecimalParse('3273344365508751233', a);
  km.FastMulThreshold := 1;
  c := km.Eval(a, a);
  CheckEquals('10714783335207889188713531907079020289', c.ToDecimalString);
end;

procedure TBigIntegerTests.TestShr;
var a, b: TInteger;
begin
  a := $100000000;
  b := a shr 1;
  CheckEquals($100000000 shr 1, Int64(b), '$100000000 shr 1');

  a := $1000000000;
  b := a shr 1;
  CheckEquals($1000000000 shr 1, Int64(b), '$1000000000 shr 1');

  b := a shr 2;
  CheckEquals($1000000000 shr 2, Int64(b), '$1000000000 shr 2');

  b := a shr 5;
  CheckEquals($1000000000 shr 5, Int64(b), '$1000000000 shr 5');

  a := $1;
  b := a shr 2;
  CheckEquals(0, Int64(b), '$1 shr 2');

  a := $100000000;
  b := a shr 32;
  CheckEquals(1, UInt64(b), '$100000000 shr 32');

  a := $1;
  b := a shr 32;
  CheckEquals(0, UInt64(b), '$1 shr 32');

  a := $12345678ABCD;
  b := a shr 8;
  CheckEquals($12345678AB, UInt64(b), '$12345678ABCD shr 8');

  a := $12345678ABCD;
  b := a shr 16;
  CheckEquals($12345678, UInt64(b), '$12345678ABCD shr 32');
end;

procedure TBigIntegerTests.TestShl;
var a, b: TInteger;
begin
  a := $1;
  b := a shl 2;
  CheckEquals($1 shl 2, UInt64(b), '$1 shl 2');

  a := $1;
  b := a shl 32;
  CheckEquals(UInt64($1) shl 32, UInt64(b), '$1 shl 32');

  a := $80000000;
  b := a shl 1;
  CheckEquals(UInt64($80000000) shl 1, UInt64(b), '$80000000 shl 1');

  a := $87654321;
  b := a shl 8;
  CheckEquals(UInt64($87654321) shl 8, UInt64(b), '$87654321 shl 8');
end;

procedure TBigIntegerTests.TestStrToInt;
var i: TInteger;
begin
  if not TInteger.HexParse('123af', i) then
    Fail('TInteger.HexParse failed!');
  CheckEquals($123af, UInt64(i));

  if not TInteger.HexParse('edcba9876543210', i) then
    Fail('TInteger.HexParse failed!');
  CheckEquals($edcba9876543210, UInt64(i));

  if not TInteger.HexParse('123456789abcdefedcba9876543210', i) then
    Fail('TInteger.HexParse failed!');

{$ifdef Limb64}
  CheckEquals(2, i.Count);
  CheckEquals($fedcba9876543210, i[0]);
  CheckEquals($123456789abcde, i[1]);
{$else}
  CheckEquals(4, i.Count);
  CheckEquals($76543210, i[0]);
  CheckEquals($fedcba98, i[1]);
  CheckEquals($789abcde, i[2]);
  CheckEquals($123456, i[3]);
{$endif}
end;

procedure TBigIntegerTests.TestDecParsing;
var i: TInteger;
    j: Integer;
const
{$ifdef Limb64}
  res: array [0..1] of TLimb = ($42b656528f20c28b, $29d);
{$else}
  res: array [0..2] of TLimb = ($8f20c28b, $42b65652, $29d);
{$endif}
begin
  if not TInteger.DecimalParse('12345678909876543210123', i) then
    Fail('It''s not possible to parse decimal string.');

  CheckEquals(1, i.Signum, 'incorrect signum');
  CheckEquals(Length(res), i.Count);
  for j := 0 to High(res) do
    CheckEquals(res[j], i[j], Format('Wrong value at position %d.', [j]));

  if not TInteger.DecimalParse('-12345678909876543210123', i) then
    Fail('It''s not possible to parse decimal string.');

  CheckEquals(-1, i.Signum , 'incorrect signum');
  CheckEquals(Length(res), i.Count);
  for j := 0 to High(res) do
    CheckEquals(res[j], i[j], Format('Wrong value at position %d.', [j]));

  if not TInteger.DecimalParse('+12345678909876543210123', i) then
    Fail('It''s not possible to parse decimal string.');

  CheckEquals(1, i.Signum , 'incorrect signum');
  CheckEquals(Length(res), i.Count);
  for j := 0 to High(res) do
    CheckEquals(res[j], i[j], Format('Wrong value at position %d.', [j]));
end;

procedure TBigIntegerTests.TestSubtract;
var a, b, c: TInteger;
begin
  a := $100000000;
  b := $1;
  c := a - b;
  CheckTrue((c.Count = 1) and (c[0] = $ffffffff) and (c.Signum = 1),
    '$100000000 - $1');

  a := $1;
  b := $100000000;
  c := a - b;
  CheckTrue((c.Count = 1) and (c[0] = $ffffffff) and (c.Signum = -1),
    '$1 - $100000000');


  a := 1;
  b := 2;
  c := a - b;
  CheckTrue((c.Count = 1) and (c[0] = 1) and (c.Signum = -1), '1 - 2');

  a := 1;
  b := -2;
  c := a - b;
  CheckTrue((c.Count = 1) and (c[0] = 3) and (c.Signum = 1), '1 - (-2)');

  a := -1;
  b := 2;
  c := a - b;
  CheckTrue((c.Count = 1) and (c[0] = 3) and (c.Signum = -1), '-1 - 2');

  a := -3;
  b := -4;
  c := a - b;
  CheckTrue((c.Count = 1) and (c[0] = 1) and (c.Signum = 1), '-3-(-4)');
end;

procedure TBigIntegerTests.TestToHexStr;
var a: TInteger;
    s: String;
begin
  a := $12345678;
  s := a.ToHexString;
  CheckEquals('12345678', s);

  a := $123456789abcdef;
  s := a.ToHexString;
  CheckEquals('123456789ABCDEF', s);

  a := TLimbArray.Create($1111, $2222, $3333);
  s := a.ToHexString;
{$ifdef Limb64}
  CheckEquals('333300000000000022220000000000001111', s);
{$else}
  CheckEquals('33330000222200001111', s);
{$endif}

{$ifdef Limb64}
  a := TLimbArray.Create($123456789ABCDEF, $111222);
  s := a.ToHexString;
  CheckEquals('1112220123456789ABCDEF', s);
{$endif}
end;

procedure TBigIntegerTests.TestToDecString;
var a: TInteger;
    s: String;
begin
{$ifdef Limb64}
  a.Init(TLimbArray.Create($42b656528f20c28b, $29d));
  s := a.ToDecimalString;
  CheckEquals('12345678909876543210123', s);
{$else}
  a.Init(TLimbArray.Create($8f20c28b, $42b65652, $29d));
  s := a.ToDecimalString;
  CheckEquals('12345678909876543210123', s);

  a.Init(TLimbArray.Create($a7640000, $de0b6b3));
  s := a.ToDecimalString;
  CheckEquals('1000000000000000000', s);
{$endif}
end;


procedure TBigIntegerTests.TestGCD;
var a, b, c: TInteger;
begin
  a := $1000000000;
  b := $8000000000;
  c := TInteger.GCD(a, b);
  CheckEquals(UInt64(a), UInt64(c));

  a := $123456789012;
  b := $987654321098;
  c := TInteger.GCD(a, b);
  CheckEquals(2, UInt64(c));

  a := $14b66d39fa9c06;
  b := $ad77d2c5f94116;
  c := TInteger.GCD(a, b);
  CheckEquals($333331e, UInt64(c));

  a := 0;
  b := 0;
  c := TInteger.GCD(a, b);
  CheckEquals(0, UInt64(c));

  a := 1;
  b := 0;
  c := TInteger.GCD(a, b);
  CheckEquals(1, UInt64(c));

  a := 0;
  b := 1;
  c := TInteger.GCD(a, b);
  CheckEquals(1, UInt64(c));

  a := -2;
  b := 4;
  c := TInteger.GCD(a, b);
  CheckEquals(2, UInt64(c));

  a := 2;
  b := -4;
  c := TInteger.GCD(a, b);
  CheckEquals(2, UInt64(c));
end;

procedure TBigIntegerTests.TestGCD_2;
var a, b, c: TInteger;
    s: String;
begin
  if not TInteger.DecimalParse('1350851717672992089', a) then
    Fail('Invalid integer string.');
  if not TInteger.DecimalParse('121645100408832000', b) then
    Fail('Invalid integer string.');
  c := TInteger.GCD(a, b);
  s := c.ToDecimalString;
  CheckEquals('6561', s);
end;

procedure TBigIntegerTests.TestToDoubleCvt;
var a: TInteger;
    e, d: Double;
begin
  // I32
  a := 100;
  d := Double(a);
  e := 100;
  CheckEquals(e, d);

  //I64
  a := $1000000000;
  d := Double(a);
  e := 68719476736;
  CheckEquals(e, d);

{$ifdef Limb64}
  a := TLimbArray.Create($200000000, 1);
{$else}
  a := TLimbArray.Create(0, 2, 1);
{$endif}
  d := Double(a);
  e := 1.8446744082299486e19;
  CheckEquals(e, d, Double.Epsilon);

  //negative number
  a := -100;
  d := Double(a);
  e := -100;
  CheckEquals(e, d);
end;

procedure TBigIntegerTests.TestFloor;
var a: TInteger;
begin
  a := TInteger.Floor(300.3);
  CheckEquals('300', a.ToDecimalString);

  a := TInteger.Floor(300000000000.5);
  CheckEquals('300000000000', a.ToDecimalString);

  a := TInteger.Floor(-300.3);
  CheckEquals('-301', a.ToDecimalString);

  a := TInteger.Floor(-300000000000.5);
  CheckEquals('-300000000001', a.ToDecimalString);

  a := TInteger.Floor(-300);
  CheckEquals('-300', a.ToDecimalString);

  a := TInteger.Floor(1e90); // compared to the WL result of the Floor[1.0 10^90] command
  CheckEquals(
    '999999999999999966484112715463900049825186092620125502979674597309179755437379230686511104',
    a.ToDecimalString
  );
end;

procedure TBigIntegerTests.TestCeil;
var a: TInteger;
begin
  a := TInteger.Ceil(300.3);
  CheckEquals('301', a.ToDecimalString);

  a := TInteger.Ceil(300);
  CheckEquals('300', a.ToDecimalString);

  a := TInteger.Ceil(300000000000.5);
  CheckEquals('300000000001', a.ToDecimalString);

  a := TInteger.Ceil(-300.3);
  CheckEquals('-300', a.ToDecimalString);

  a := TInteger.Ceil(-300000000000.5);
  CheckEquals('-300000000000', a.ToDecimalString);
end;

{$endregion}

{$region 'TBigRationalTests'}

procedure TBigRationalTests.SetUp;
begin
  inherited;

end;

procedure TBigRationalTests.TearDown;
begin
  inherited;

end;

procedure TBigRationalTests.TestAdd;
var a, b, res: TRational;
begin
  a.Create(70, 128);
  b.Create(32, 10);
  res := a + b;
  CheckTrue(res.Signum > 0);
  CheckEquals(1199, UInt64(res.Numerator));
  CheckEquals(320, UInt64(res.Denominator));
end;

procedure TBigRationalTests.TestAdd2;
var a, b, res: TRational;
    s: String;
begin
  a.Parse('14348907/262683704098816000000');
  b.Parse('4782969/10507348163952640000');
  res := a + b;
  s := res.ToString;
  CheckEquals('4782969/9381560860672000000', s);

  a.Parse('-101/2');
  b.Parse('-243/2');
  res := a + b;
  s := res.ToString;
  CheckEquals('-172', s);
end;

procedure TBigRationalTests.TestDivide;
var a, b, res: TRational;
begin
  a.Create(70, 128);
  b.Create(32, 10);
  res := a / b;
  CheckTrue(res.Signum > 0);
  CheckEquals(175, UInt64(res.Numerator));
  CheckEquals(1024, UInt64(res.Denominator));
end;

procedure TBigRationalTests.TestLessThan;
var a, b: TRational;
begin
  a.Create(1, 2);
  b.Create(2, 3);
  CheckEquals(True, a < b, '1/2 < 2/3');
end;

procedure TBigRationalTests.TestToDoubleCvtWith64BitNumbers;
var a: TRational;
    e, d: Double;
begin
  a.Create(1, 3);
  d := Double(a);
  e := 1/3;
  CheckEquals(e, d, Double.Epsilon);

  a.Create(1, 3, -1);
  d := Double(a);
  e := -1/3;
  CheckEquals(e, d, Double.Epsilon);

  a.Create(2, 10);
  d := Double(a);
  e := 2/10;
  CheckEquals(e, d, Double.Epsilon);

  a.Create(10, 2);
  d := Double(a);
  e := 10/2;
  CheckEquals(e, d, Double.Epsilon);
end;

procedure TBigRationalTests.TestToDoubleCvtWithLargeNumbers;
var a: TRational;
    e, d: Double;
    num, den: TInteger;
begin
  TInteger.DecimalParse('79228162551157825753847955460', num);  // B^3 + 2 B^2 + 3 B + 4
  TInteger.DecimalParse('79228162532711081671548469248', den);  // B^3 + B^2 + B
  a.Create(num.GetData, den.GetData);
  d := Double(a);
  e := 4294967298/4294967297;
  CheckEquals(e, d, Double.Epsilon);

  // random integers
  TInteger.DecimalParse('3480905512742563769689941670884043915921588', num);
  TInteger.DecimalParse('48676850271698023459800287006966749221176886', den);
  a.Create(num.GetData, den.GetData);
  d := Double(a);
  e := 0.071510492016498693571367467127909;
  CheckEquals(e, d, 1e-15);

  // random integers
  TInteger.DecimalParse('40652306459331425550734587780899415760404153538828713480905512742563769689941670884043915921588', num);
  TInteger.DecimalParse('48676850271698023459800287006966749221176886', den);
  a.Create(num.GetData, den.GetData);
  d := Double(a);
  e := 8.351466093722938621773681692631742e50;
  CheckEquals(e, d, 1e-15);

  a.Create(den.GetData, num.GetData);
  d := Double(a);
  e := 1.197394551780090335932444065748340e-51;
  CheckEquals(e, d, 1e-66);

  TInteger.DecimalParse('815915283247897734345611269596115894272000000000', den);
  a := TRational.Reciprocal(den);
  d := Double(a);
  e := 1.225617439128386E-48;
  CheckEquals(e, d, 1e-63);
end;

procedure TBigRationalTests.TestMultiply;
var a, b, res: TRational;
begin
  a.Create(70, 128);
  b.Create(32, 10);
  res := a * b;
  CheckTrue(res.Signum > 0);
  CheckEquals(7, UInt64(res.Numerator));
  CheckEquals(4, UInt64(res.Denominator));
end;

procedure TBigRationalTests.TestRationalConstruction;
var r: TRational;
begin
  r.Create(32, 128);
  CheckEquals(1, UInt64(r.Numerator));
  CheckEquals(4, UInt64(r.Denominator));

  r.Create($123456789, $123);
  CheckEquals(1628906115, UInt64(r.Numerator));
  CheckEquals(97, UInt64(r.Denominator));
end;

procedure TBigRationalTests.TestRationalFromNotCoprimeIntegers;
var num, denom: TInteger;
    res: TRational;
    s: String;
begin
  if not TInteger.DecimalParse('1350851717672992089', num) then
    Fail('Invalid numerator string.');
  if not TInteger.DecimalParse('121645100408832000', denom) then
    Fail('Invalid denominator string.');

  res.Create(num, denom);
  s := res.ToString;
  CheckEquals('205891132094649/18540634112000', s);
end;

procedure TBigRationalTests.TestSubtract;
var a, b, res: TRational;
begin
  a.Create(70, 128);
  b.Create(32, 10);
  res := a - b;
  CheckTrue(res.Signum < 0);
  CheckEquals(849, UInt64(res.Numerator));
  CheckEquals(320, UInt64(res.Denominator));
end;

procedure TBigRationalTests.TestRound;
var a: TRational;
    res: TInteger;
begin
  // a = (3x2^100 + 1) / 2^100
  a.Parse('3802951800684688204490109616129/2535301200456458802993406410752');
  res := a.Round;
  CheckTrue(res = 2);

  // a = (3x2^100 - 1) / 2^100
  a.Parse('3802951800684688204490109616127/2535301200456458802993406410752');
  res := a.Round;
  CheckTrue(res = 1);

  // a = -(3x2^100 + 1) / 2^100
  a.Parse('-3802951800684688204490109616129/2535301200456458802993406410752');
  res := a.Round;
  CheckTrue(res = -2);

  // a = -(3x2^100 - 1) / 2^100
  a.Parse('-3802951800684688204490109616127/2535301200456458802993406410752');
  res := a.Round;
  CheckTrue(res = -1);
end;

{$endregion}

{$region 'TBigRealTests'}

procedure TBigRealTests.TestInitFromDouble;
var a: TReal;
begin
  a := 1 + 1/4;
  CheckTrue(a.Signum > 0);
  CheckEquals(0, a.Exponent);
{$ifdef Limb64}
  CheckEquals(1, Length(a.Fraction));
  CheckEquals($A000000000000000, a.Fraction[0]);
{$else}
  CheckEquals(2, Length(a.Fraction));
  CheckEquals(0, a.Fraction[0]);
  CheckEquals($A0000000, a.Fraction[1]);
{$endif}

  a := -3/2;
  CheckTrue(a.Signum < 0);
  CheckEquals(0, a.Exponent);
{$ifdef Limb64}
  CheckEquals(1, Length(a.Fraction));
  CheckEquals($C000000000000000, a.Fraction[0]);
{$else}
  CheckEquals(2, Length(a.Fraction));
  CheckEquals(0, a.Fraction[0]);
  CheckEquals($C0000000, a.Fraction[1]);
{$endif}
end;

procedure TBigRealTests.TestInitFromDigits;
var f: TLimbArray;
    a: TReal;
    d: Double;
begin
{$ifdef Limb64}
  f := TLimbArray.Create($8000000000000000);
{$else}
  f := TLimbArray.Create($80000000);
{$endif}
  a.Init(f);
  d := a.AsDouble;
  CheckEquals(1, d);

{$ifdef Limb64}
  f := TLimbArray.Create($4000000000000000);
{$else}
  f := TLimbArray.Create($40000000);
{$endif}
  a.Init(f);
  d := a.AsDouble;
  CheckEquals(1/2, d);
{$ifdef Limb64}
  CheckEquals(1, Length(a.Fraction));
  CheckEquals($8000000000000000, a.Fraction[0]);
{$else}
  CheckEquals(2, Length(a.Fraction));
  CheckEquals($80000000, a.Fraction[1]);
{$endif}
  CheckEquals(-1, a.Exponent);

  f := TLimbArray.Create(2);
  a.Init(f);
{$ifdef Limb64}
  CheckEquals(1, Length(a.Fraction));
  CheckEquals($8000000000000000, a.Fraction[0]);
  CheckEquals(-62, a.Exponent);
{$else}
  CheckEquals(2, Length(a.Fraction));
  CheckEquals(0, a.Fraction[0]);
  CheckEquals($80000000, a.Fraction[1]);
  CheckEquals(-30, a.Exponent);
{$endif}

  f := TLimbArray.Create(2, 0, 0);
  a.Init(f);
  CheckEquals(3, Length(a.Fraction));
{$ifdef Limb64}
  CheckEquals($8000000000000000, a.Fraction[2]);
  CheckEquals(-190, a.Exponent);
{$else}
  CheckEquals($80000000, a.Fraction[2]);
  CheckEquals(-94, a.Exponent);
{$endif}

  f := TLimbArray.Create(0, $80000000, 0);
  a.Init(f);
  CheckEquals(3, Length(a.Fraction));
{$ifdef Limb64}
  CheckEquals($8000000000000000, a.Fraction[2]);
  CheckEquals(-96, a.Exponent);
{$else}
  CheckEquals($80000000, a.Fraction[2]);
  CheckEquals(-32, a.Exponent);
{$endif}
end;

procedure TBigRealTests.TestCvtFromBigInt;
var a: TInteger;
    r: TReal;
begin
  a.Init('123456789');
  r := TReal(a);
  CheckTrue(a = r.IntegerPart);
  CheckTrue(r.FractionalPart.IsZeroQ);
end;

procedure TBigRealTests.TestInitRational;
var a: TReal;
    d: Double;
begin
  a.InitRational(1, 2, 128);
{$ifdef Limb64}
  CheckEquals(2, Length(a.Fraction));
{$else}
  CheckEquals(4, Length(a.Fraction));
{$endif}
  d := a.AsDouble;
  CheckEquals(0.5, d);

  a.InitRational(1, 3, 128);
{$ifdef Limb64}
  CheckEquals(2, Length(a.Fraction));
{$else}
  CheckEquals(4, Length(a.Fraction));
{$endif}
end;

procedure TBigRealTests.TestAsDouble;
var a: TReal;
    d: Double;
begin
  a := 3/2;
  d := a.AsDouble;
  CheckEquals(3/2, d);

  a := 0;
  d := a.AsDouble;
  CheckEquals(0, d);

  a.Init(TLimbArray.Create(W_HI_BIT));
  d := a.AsDouble;
  CheckEquals(1, d);

{$ifdef Limb64}
  a.Init(TLimbArray.Create(W_MASK));
{$else}
  a.Init(TLimbArray.Create(W_MASK, W_MASK));
{$endif}
  d := a.AsDouble;
  CheckEquals(2, d);
end;

procedure TBigRealTests.TestAddPosDbl;
var a, b, c: TReal;
    d: Double;
begin
  a := 1 + 1/2;
  b := 1 + 1/4;
  c := a + b;
  d := c.AsDouble;
  CheckEquals(2.75, d);

  b := 1/4;
  c := a + b;
  d := c.AsDouble;
  CheckEquals(1.75, d);
end;

procedure TBigRealTests.TestAddNegDbl;
var a, b, c: TReal;
    d: Double;
begin
  a := -(1 + 1/2);
  b := -(1 + 1/4);
  c := a + b;
  d := c.AsDouble;
  CheckEquals(-2.75, d);

  b := -1/4;
  c := a + b;
  d := c.AsDouble;
  CheckEquals(-1.75, d);
end;

procedure TBigRealTests.TestAddPosQuad;
var a, b, c: TReal;
    d: Double;
begin
  a := 1 + 1/2;
  b := 1 + 1/4;
  a := a.BinaryRoundTo(128);
  b := b.BinaryRoundTo(128);
  c := a + b;
  d := c.AsDouble;
  CheckEquals(2.75, d);

  a.InitRational(1, 3, 128);
  b.InitRational(1, 5, 128);
  c := a + b;
{$ifdef Limb64}
  CheckEquals(2, Length(c.Fraction));
  CheckEquals($8888888888888889, c.Fraction[0]);
  CheckEquals($8888888888888888, c.Fraction[1]);
{$else}
  CheckEquals(4, Length(c.Fraction));
  CheckEquals($88888889, c.Fraction[0]);
  CheckEquals($88888888, c.Fraction[1]);
  CheckEquals($88888888, c.Fraction[2]);
  CheckEquals($88888888, c.Fraction[3]);
{$endif}
end;

procedure TBigRealTests.TestAddPos160;
var a, b, c: TReal;
    d: Double;
begin
  a := 1 + 1/2;
  b := 1 + 1/4;
  a := a.BinaryRoundTo(160);
  b := b.BinaryRoundTo(160);
  c := a + b;
  d := c.AsDouble;
  CheckEquals(2.75, d);

  a.InitRational(1, 3, 160);
  b.InitRational(1, 5, 160);
  c := a + b;
{$ifdef Limb64}
  CheckEquals(3, Length(c.Fraction));
  CheckEquals(-1, c.Exponent);
  CheckEquals($8888888888888889, c.Fraction[0]);
  CheckEquals($8888888888888888, c.Fraction[1]);
  CheckEquals($8888888888888888, c.Fraction[2]);
{$else}
  CheckEquals(5, Length(c.Fraction));
  CheckEquals(-1, c.Exponent);
  CheckEquals($88888889, c.Fraction[0]);
  CheckEquals($88888888, c.Fraction[1]);
  CheckEquals($88888888, c.Fraction[2]);
  CheckEquals($88888888, c.Fraction[3]);
  CheckEquals($88888888, c.Fraction[4]);
{$endif}
end;

procedure TBigRealTests.TestAddInt;
var a: TReal;
    d: Double;
begin
  a := 3/2;
  a := a + 1000;
  d := a.AsDouble;
  CheckEquals(1001.5, d);
end;

procedure TBigRealTests.TestAddRounding;
var a, b, c: TReal;
    digs: TLimbArray;
begin
{$ifdef Limb64}
   digs := TLimbArray.Create($0000000000000000, $8000000000000000);
  a.Init(digs, -1); // a = 0.5

  digs := TLimbArray.Create($FFFFFFFFFFFFFFFF, $8000000000000000);
  b.Init(digs, -65);
  c := a + b;
  CheckEquals($8000000000000000, c.Fraction[1]);
  CheckEquals($8000000000000001, c.Fraction[0]);

  digs := TLimbArray.Create($8000000000000000, $8000000000000000);
  b.Init(digs, -65);
  c := a + b;
  CheckEquals($8000000000000000, c.Fraction[1]);
  CheckEquals($8000000000000000, c.Fraction[0]);

  digs := TLimbArray.Create($8000000000000000, $8000000000000001);
  b.Init(digs, -65);
  c := a + b;
  CheckEquals($8000000000000000, c.Fraction[1]);
  CheckEquals($8000000000000002, c.Fraction[0]);

  digs := TLimbArray.Create($7000000000000000, $8000000000000000);
  b.Init(digs, -65);
  c := a + b;
  CheckEquals($8000000000000000, c.Fraction[1]);
  CheckEquals($8000000000000000, c.Fraction[0]);
{$else}
  // a variable is set less than 1 due to simple comparsion with
  // the mathematica result
  // Example of the Mathematica code:
  //// a = N[16^^8000000000000000 2^-64, Log10[2] 64]
  //// b = N[16^^80000000ffffffff 2^(-63 - 32), Log10[2] 64]
  //// c = a + b
  //// BaseForm[c, 16]

  digs := TLimbArray.Create($00000000, $80000000);
  a.Init(digs, -1); // a = 0.5

  digs := TLimbArray.Create($FFFFFFFF, $80000000);
  b.Init(digs, -33);
  c := a + b;
  CheckEquals($80000000, c.Fraction[1]);
  CheckEquals($80000001, c.Fraction[0]);

  digs := TLimbArray.Create($80000000, $80000000);
  b.Init(digs, -33);
  c := a + b;
  CheckEquals($80000000, c.Fraction[1]);
  CheckEquals($80000000, c.Fraction[0]);

  digs := TLimbArray.Create($80000000, $80000001);
  b.Init(digs, -33);
  c := a + b;
  CheckEquals($80000000, c.Fraction[1]);
  CheckEquals($80000002, c.Fraction[0]);

  digs := TLimbArray.Create($70000000, $80000000);
  b.Init(digs, -33);
  c := a + b;
  CheckEquals($80000000, c.Fraction[1]);
  CheckEquals($80000000, c.Fraction[0]);
{$endif}
end;

procedure TBigRealTests.TestSubRounding;
var a, b, c: TReal;
    digs: TLimbArray;
begin
{$ifdef Limb64}
  digs := TLimbArray.Create(0, $C000000000000000);
  a.Init(digs, -1); // a = 0.75

  digs := TLimbArray.Create($FFFFFFFFFFFFFFFF, $8000000000000000);
  b.Init(digs, -65);
  c := a - b;
  CheckEquals($BFFFFFFFFFFFFFFF, c.Fraction[1]);
  CheckEquals($7FFFFFFFFFFFFFFF, c.Fraction[0]);

  digs := TLimbArray.Create($8000000000000000, $8000000000000000);
  b.Init(digs, -65);
  c := a - b;
  CheckEquals($BFFFFFFFFFFFFFFF, c.Fraction[1]);
  CheckEquals($8000000000000000, c.Fraction[0]);

  digs := TLimbArray.Create($8000000000000000, $8000000000000001);
  b.Init(digs, -65);
  c := a - b;
  CheckEquals($BFFFFFFFFFFFFFFF, c.Fraction[1]);
  CheckEquals($7FFFFFFFFFFFFFFE, c.Fraction[0]);

  digs := TLimbArray.Create($C000000000000000, $8000000000000000);
  b.Init(digs, -65);
  c := a - b;
  CheckEquals($BFFFFFFFFFFFFFFF, c.Fraction[1]);
  CheckEquals($7FFFFFFFFFFFFFFF, c.Fraction[0]);

  digs := TLimbArray.Create($4000000000000000, $8000000000000000);
  b.Init(digs, -65);
  c := a - b;
  CheckEquals($BFFFFFFFFFFFFFFF, c.Fraction[1]);
  CheckEquals($8000000000000000, c.Fraction[0]);
{$else}
  digs := TLimbArray.Create($00000000, $C0000000);
  a.Init(digs, -1); // a = 0.75

  digs := TLimbArray.Create($FFFFFFFF, $80000000);
  b.Init(digs, -33);
  c := a - b;
  CheckEquals($BFFFFFFF, c.Fraction[1]);
  CheckEquals($7FFFFFFF, c.Fraction[0]);

  digs := TLimbArray.Create($80000000, $80000000);
  b.Init(digs, -33);
  c := a - b;
  CheckEquals($BFFFFFFF, c.Fraction[1]);
  CheckEquals($80000000, c.Fraction[0]);

  digs := TLimbArray.Create($80000000, $80000001);
  b.Init(digs, -33);
  c := a - b;
  CheckEquals($BFFFFFFF, c.Fraction[1]);
  CheckEquals($7FFFFFFE, c.Fraction[0]);

  digs := TLimbArray.Create($C0000000, $80000000);
  b.Init(digs, -33);
  c := a - b;
  CheckEquals($BFFFFFFF, c.Fraction[1]);
  CheckEquals($7FFFFFFF, c.Fraction[0]);

  digs := TLimbArray.Create($40000000, $80000000);
  b.Init(digs, -33);
  c := a - b;
  CheckEquals($BFFFFFFF, c.Fraction[1]);
  CheckEquals($80000000, c.Fraction[0]);
{$endif}
end;

procedure TBigRealTests.TestAddNegPosDbl;
var a, b, c: TReal;
begin
  a := 1 + 1/2;
  b := -(1 + 1/4);
  c := a + b;
  CheckEquals(1/4, c.AsDouble);

  a := 1 + 1/4;
  b := -(1 + 1/2);
  c := a + b;
  CheckEquals(-1/4, c.AsDouble);
end;

procedure TBigRealTests.TestSubtractDbl;
var a, b, c: TReal;
    d: Double;
begin
  a := 3/2;
  b := 1/4;
  c := a - b;
  d := c.AsDouble;
  CheckEquals(5/4, d);

  c := b - a;
  d := c.AsDouble;
  CheckEquals(-5/4, d);
end;

procedure TBigRealTests.TestMultiplyDbl;
var a, b, c: TReal;
    d: Double;
begin
  a := 1/2;
  b := 1/4;
  c := a * b;
  d := c.AsDouble;
  CheckEquals(1/8, d);

  a := 1/8;
  b := 1/4;
  c := a * b;
  d := c.AsDouble;
  CheckEquals(1/32, d);

  a := 1 + 1/2;
  b := 1/4;
  c := a * b;
  d := c.AsDouble;
  CheckEquals(3/8, d);

  a := 1 + 1/2;
  b := 1 + 1/2;
  c := a * b;
  d := c.AsDouble;
  CheckEquals(9/4, d);
end;

procedure TBigRealTests.TestMultiplyByInt;
var a, b: TReal;
    d: Double;
begin
  a := 1/4;
  b := 3 * a;
  d := b.AsDouble;
  CheckEquals(3/4, d);

  b := a * 3;
  d := b.AsDouble;
  CheckEquals(3/4, d);
end;

procedure TBigRealTests.TestMulByZero;
var a, b, c: TReal;
    d: Double;
begin
  a := 1 + 1/2;
  b := 0;
  c := a * b;
  d := c.AsDouble;
  CheckEquals(0, d);
end;

procedure TBigRealTests.TestDivideDbl;
var a, b, c: TReal;
    d: Double;
begin
  a := 1/4;
  b := 1/2;
  c := a / b;
  d := b.AsDouble;
  CheckEquals(1/2, d);
end;

procedure TBigRealTests.TestSchoolDiv;
var a, b, c: TReal;
begin
  a := 1;
  b := 1/2;
  c := TReal.SchoolDiv(a, b);
  CheckEquals(2, c.AsDouble);

  b := 1/4;
  c := TReal.SchoolDiv(a, b);
  CheckEquals(4, c.AsDouble);

  b := 1/8;
  c := TReal.SchoolDiv(a, b);
  CheckEquals(8, c.AsDouble);

  b := 2;
  c := TReal.SchoolDiv(a, b);
  CheckEquals(0.5, c.AsDouble);

  b := 4;
  c := TReal.SchoolDiv(a, b);
  CheckEquals(0.25, c.AsDouble);
end;

procedure TBigRealTests.TestSchoolDiv128;
var a, b, c: TReal;
    f: TArray<UInt64>;
begin
  a.Init(55, 128);
  b.Init(131, 128);

  c := TReal.SchoolDiv(a, b);

  f := GetFraction(c);
  CheckEquals(-2, c.Exponent);
  CheckEquals(2, Length(f));
  CheckEquals($d6f63aa03e88cb3c, f[1]);
  CheckEquals($9484e2afe0bb9a62, f[0]);
end;

procedure TBigRealTests.TestSchoolDiv192;
var a, b, c: TReal;
begin
{$if Limb64}
  a.Init([0, 0, $8000000000000000], 0);
  b.Init([$449E1462529C9CA8, $B2FB1366EAA6A543, $E2463000F855FB08], 1);
{$else}
  a.Init([0, 0, 0, 0, 0, $80000000]);
  b.Init([$529C9CA8, $449E1462, $EAA6A543, $B2FB1366, $F855FB08, $E2463000], 1);
{$endif}

  c := TReal.SchoolDiv(a, b);
  CheckEquals(-2, c.Exponent);
end;

procedure TBigRealTests.TestSchoolDiv1024;
var a, b, c: TReal;
    f: TArray<UInt64>;
begin
  a.Init(55, 1024);
  b.Init(131, 1024);

  c := TReal.SchoolDiv(a, b);

  f := GetFraction(c);
  CheckEquals(-2, c.Exponent);
  CheckEquals(16, Length(f));
  CheckEquals($d6f63aa03e88cb3c, f[15]);
  CheckEquals($9484e2afe0bb9a61, f[14]);
  CheckEquals($b5bd8ea80fa232cf, f[13]);
  CheckEquals($252138abf82ee698, f[12]);
  CheckEquals($6d6f63aa03e88cb3, f[11]);
  CheckEquals($c9484e2afe0bb9a6, f[10]);
  CheckEquals($1b5bd8ea80fa232c, f[9]);
  CheckEquals($f252138abf82ee69, f[8]);
  CheckEquals($86d6f63aa03e88cb, f[7]);
  CheckEquals($3c9484e2afe0bb9a, f[6]);
  CheckEquals($61b5bd8ea80fa232, f[5]);
  CheckEquals($cf252138abf82ee6, f[4]);
  CheckEquals($986d6f63aa03e88c, f[3]);
  CheckEquals($b3c9484e2afe0bb9, f[2]);
  CheckEquals($a61b5bd8ea80fa23, f[1]);
  CheckEquals($2cf252138abf82ee, f[0]);
end;

procedure TBigRealTests.TestNewtonDiv128;
var a, b, c: TReal;
    f: TArray<UInt64>;
begin
  a.Init(1, 128);
  b.Init(1/2, 128);

  c := TReal.NewtonDiv(a, b);
  CheckEquals(2, c.AsDouble);


  a.Init(55, 128);
  b.Init(131, 128);

  c := TReal.NewtonDiv(a, b);

  f := GetFraction(c);
  CheckEquals(-2, c.Exponent);
  CheckEquals(2, Length(f));
  CheckEquals($d6f63aa03e88cb3c, f[1]);
  CheckEquals($9484e2afe0bb9a62, f[0]);
end;

procedure TBigRealTests.TestNewtonDiv1024;
var a, b, c: TReal;
    f: TArray<UInt64>;
begin
  a.Init(55, 1024);
  b.Init(131, 1024);

  c := TReal.NewtonDiv(a, b);

  f := GetFraction(c);
  CheckEquals(-2, c.Exponent);
  CheckEquals(16, Length(f));
  CheckEquals($d6f63aa03e88cb3c, f[15]);
  CheckEquals($9484e2afe0bb9a61, f[14]);
  CheckEquals($b5bd8ea80fa232cf, f[13]);
  CheckEquals($252138abf82ee698, f[12]);
  CheckEquals($6d6f63aa03e88cb3, f[11]);
  CheckEquals($c9484e2afe0bb9a6, f[10]);
  CheckEquals($1b5bd8ea80fa232c, f[9]);
  CheckEquals($f252138abf82ee69, f[8]);
  CheckEquals($86d6f63aa03e88cb, f[7]);
  CheckEquals($3c9484e2afe0bb9a, f[6]);
  CheckEquals($61b5bd8ea80fa232, f[5]);
  CheckEquals($cf252138abf82ee6, f[4]);
  CheckEquals($986d6f63aa03e88c, f[3]);
  CheckEquals($b3c9484e2afe0bb9, f[2]);
  CheckEquals($a61b5bd8ea80fa23, f[1]);
  CheckEquals($2cf252138abf82ee, f[0]);
end;

procedure TBigRealTests.TestNoRoundingAdd128;
var a, b, c: TReal;
begin
  a.Init(3, 256);
  b.Init(5, 256);
  a := a.Reciprocal;
  b := b.Reciprocal;
  c.Init(0, 256);

  TReal._NRAddPos(a, b, c, 2);

end;

procedure TBigRealTests.TestNoRoundingMul128;
var a, b, c: TReal;
begin
  a.Init(1/3, 256);
  b.Init(1/5, 256);
  c.Init(0, 256);

  TReal._NRMul(a, b, c, 2);

end;

procedure TBigRealTests.TestDivideByInt;
var a: TReal;
    d: Double;
begin
  a := 1;
  a := a / 2;
  d := a.AsDouble;
  CheckEquals(1/2, d);

  a := 355;
  a := a / 113;
  d := a.AsDouble;
  CheckEquals(3.141592, d, 1e-6);
end;

procedure TBigRealTests.TestIntDivide_D1;
var a: TReal;
    f: TArray<UInt64>;
    I: Integer;
begin
  a.Init(1, 256);
  a := a.IntDivide(77);

  f := GetFraction(a);
  CheckEquals($d4c77b03531dec0d, f[3]);
  CheckEquals($4c77b03531dec0d4, f[2]);
  CheckEquals($c77b03531dec0d4c, f[1]);
  CheckEquals($77b03531dec0d4c7, f[0]);

  a.Init(131, 512);
  a := a.IntDivide(15);

  f := GetFraction(a);
  CheckEquals($bbbbbbbbbbbbbbbc, f[0]);
  for I := 1 to 6 do
    CheckEquals($bbbbbbbbbbbbbbbb, f[I]);
  CheckEquals($8bbbbbbbbbbbbbbb, f[7]);
end;

procedure TBigRealTests.TestIntDivide_D2;
var a: TReal;
    b: TInteger;
    f: TArray<UInt64>;
begin
  a.Init(1, 256);
{$ifdef Limb64}
  b.Init([$2345678900000000, $1]);
{$else}
  b.Init([$23456789, $1]);
{$endif}

  a := a.IntDivide(b);

{$ifdef Limb64}
  CheckEquals(-65, a.Exponent);
{$else}
  CheckEquals(-33, a.Exponent);
{$endif}
  f := GetFraction(a);
  CheckEquals(4, Length(f));
  CheckEquals($e100000084b70000, f[3]);
  CheckEquals($4e47f1002e2c6f27, f[2]);
  CheckEquals($1b3c359011108397, f[1]);
  CheckEquals($fa10bd9ea47fdfd9, f[0]);
end;

procedure TBigRealTests.TestReciprocal;
var a: TReal;
    d: Double;
begin
  a := 2;
  a := a.Reciprocal;
  d := a.AsDouble;
  CheckEquals(0.5, d);

  a := 6;
  a := a.Reciprocal;
  d := a.AsDouble;
  CheckEquals(0.166666666666667, d, 1e-15);
{$ifdef Limb64}
  CheckEquals($AAAAAAAAAAAAAAAB, a.Fraction[0]);
{$else}
  CheckEquals($AAAAAAAB, a.Fraction[0]);
  CheckEquals($AAAAAAAA, a.Fraction[1]);
{$endif}
end;

procedure TBigRealTests.TestReciprocal128;
var a: TReal;
begin
  a.Init(6, 128);
  a := a.Reciprocal;
{$ifdef Limb64}
  CheckEquals(2, Length(a.Fraction));
  CheckEquals($AAAAAAAAAAAAAAAB, a.Fraction[0]);
  CheckEquals($AAAAAAAAAAAAAAAA, a.Fraction[1]);
{$else}
  CheckEquals(4, Length(a.Fraction));
  CheckEquals($AAAAAAAB, a.Fraction[0]);
  CheckEquals($AAAAAAAA, a.Fraction[1]);
  CheckEquals($AAAAAAAA, a.Fraction[2]);
  CheckEquals($AAAAAAAA, a.Fraction[3]);
{$endif}
end;

procedure TBigRealTests.TestReciprocal160;
var a: TReal;
begin
  a.Init(6, 160);
  a := a.Reciprocal;
{$ifdef Limb64}
  CheckEquals(3, Length(a.Fraction));
  CheckEquals($AAAAAAAAAAAAAAAB, a.Fraction[0]);
  CheckEquals($AAAAAAAAAAAAAAAA, a.Fraction[1]);
  CheckEquals($AAAAAAAAAAAAAAAA, a.Fraction[2]);
{$else}
  CheckEquals(5, Length(a.Fraction));
  CheckEquals($AAAAAAAB, a.Fraction[0]);
  CheckEquals($AAAAAAAA, a.Fraction[1]);
  CheckEquals($AAAAAAAA, a.Fraction[2]);
  CheckEquals($AAAAAAAA, a.Fraction[3]);
  CheckEquals($AAAAAAAA, a.Fraction[4]);
{$endif}

  a.Init(120, 160);
  a := a.Reciprocal;
{$ifdef Limb64}
  CheckEquals(3, Length(a.Fraction));
  CheckEquals($8888888888888889, a.Fraction[0]);
  CheckEquals($8888888888888888, a.Fraction[1]);
  CheckEquals($8888888888888888, a.Fraction[2]);
{$else}
  CheckEquals(5, Length(a.Fraction));
  CheckEquals($88888889, a.Fraction[0]);
  CheckEquals($88888888, a.Fraction[1]);
  CheckEquals($88888888, a.Fraction[2]);
  CheckEquals($88888888, a.Fraction[3]);
  CheckEquals($88888888, a.Fraction[4]);
{$endif}
end;

procedure TBigRealTests.TestReciprocal1024;
var a: TReal;
    I: Integer;
begin
  a.Init(6, 1024);
  a := a.Reciprocal;
{$ifdef Limb64}
  CheckEquals(16, Length(a.Fraction));
  for I := 15 downto 1 do
    CheckEquals($AAAAAAAAAAAAAAAA, a.Fraction[I]);
{$else}
  CheckEquals(32, Length(a.Fraction));
  for I := 31 downto 1 do
    CheckEquals($AAAAAAAA, a.Fraction[I]);
{$endif}
end;

procedure TBigRealTests.TestNegate;
var a: TReal;
begin
  a := 1/2;
  a := -a;
  CheckEquals(-1/2, a.AsDouble);

  a := -a;
  CheckEquals(1/2, a.AsDouble);
end;

procedure TBigRealTests.TestAbs;
var a: TReal;
begin
  a := -1/2;
  CheckEquals(1/2, a.Abs.AsDouble);

  a := 1/2;
  CheckEquals(1/2, a.Abs.AsDouble);
end;

procedure TBigRealTests.TestBinRoundToWithExtendPrec;
var a: TReal;
    d: Double;
begin
  a := 1 + 1/2;
  a := a.BinaryRoundTo(128);
  d := a.AsDouble;
  CheckEquals(1.5, d);
end;

procedure TBigRealTests.TestBinRoundTo2;
var a: TReal;
    d: Double;
begin
  a := 1 + 1/2 + 1/8;
  a := a.BinaryRoundTo(2);
  d := a.AsDouble;
  CheckEquals(1.5, d);

  a := 1 + 1/4;
  a := a.BinaryRoundTo(2);
  d := a.AsDouble;
  CheckEquals(1.0, d);

  a := 1 + 1/4 + 1/8;
  a := a.BinaryRoundTo(2);
  d := a.AsDouble;
  CheckEquals(1.5, d);

  a := 1 + 1/2 + 1/4;
  a := a.BinaryRoundTo(2);
  d := a.AsDouble;
  CheckEquals(2.0, d);

  a := 1 + 1/2 + 1/4 + 1/8;
  a := a.BinaryRoundTo(2);
  d := a.AsDouble;
  CheckEquals(2.0, d);

  a := 1/2 + 1/4;
  a := a.BinaryRoundTo(2);
  d := a.AsDouble;
  CheckEquals(0.75, d);
end;

procedure TBigRealTests.TestBinRoundTo32;
var a: TReal;
    digs: TLimbArray;
begin
{$ifdef Limb64}
  digs := TLimbArray.Create($8000000000000000, $ffffffffffffffff);
  a.Init(digs);
  a := a.BinaryRoundTo(64);
  CheckEquals(1, Length(a.Fraction));
  CheckEquals($8000000000000000, a.Fraction[0]);
  CheckEquals(1, a.Exponent);

  digs := TLimbArray.Create($AAAAAAAAAAAAAAAA, $AAAAAAAAAAAAAAAA);
  a.Init(digs);
  a := a.BinaryRoundTo(64);
  CheckEquals(1, Length(a.Fraction));
  CheckEquals($AAAAAAAAAAAAAAAB, a.Fraction[0]);
  CheckEquals(0, a.Exponent);
{$else}
  digs := TLimbArray.Create($80000000, $ffffffff);
  a.Init(digs);
  a := a.BinaryRoundTo(32);
  CheckEquals(2, Length(a.Fraction));
  CheckEquals(0, a.Fraction[0]);
  CheckEquals($80000000, a.Fraction[1]);
  CheckEquals(1, a.Exponent);

  digs := TLimbArray.Create($AAAAAAAA, $AAAAAAAA);
  a.Init(digs);
  a := a.BinaryRoundTo(32);
  CheckEquals(2, Length(a.Fraction));
  CheckEquals(0, a.Fraction[0]);
  CheckEquals($AAAAAAAB, a.Fraction[1]);
  CheckEquals(0, a.Exponent);
{$endif}
end;

procedure TBigRealTests.TestBinFloorTo2;
var a: TReal;
    d: Double;
begin
  a := 1 + 1/2 + 1/8;
  a := a.BinaryFloorTo(2);
  d := a.AsDouble;
  CheckEquals(1.5, d);

  a := 1 + 1/4;
  a := a.BinaryFloorTo(2);
  d := a.AsDouble;
  CheckEquals(1.0, d);

  a := 1 + 1/4 + 1/8;
  a := a.BinaryFloorTo(2);
  d := a.AsDouble;
  CheckEquals(1.0, d);

  a := 1 + 1/2 + 1/4;
  a := a.BinaryFloorTo(2);
  d := a.AsDouble;
  CheckEquals(1.5, d);

  a := 1 + 1/2 + 1/4 + 1/8;
  a := a.BinaryFloorTo(2);
  d := a.AsDouble;
  CheckEquals(1.5, d);

  a := 1/2 + 1/4;
  a := a.BinaryFloorTo(2);
  d := a.AsDouble;
  CheckEquals(0.75, d);

  a := 1/2 + 1/4 + 1/8;
  a := a.BinaryFloorTo(2);
  d := a.AsDouble;
  CheckEquals(0.75, d);
end;

procedure TBigRealTests.TestBinFloorTo34;
var a: TReal;
    b: Double;
begin
  b := Power(2, -32);

  a := 1 + (1/2 + 1/4) * b;
  a := a.BinaryFloorTo(34);
{$ifdef Limb64}
  CheckEquals(1, Length(a.Fraction));
  CheckEquals($8000000040000000, a.Fraction[0]);
{$else}
  CheckEquals(2, Length(a.Fraction));
  CheckEquals($80000000, a.Fraction[1]);
  CheckEquals($40000000, a.Fraction[0]);
{$endif}
  CheckEquals(0, a.Exponent);

  a := 1 + 1/4 * b;
  a := a.BinaryFloorTo(34);
{$ifdef Limb64}
  CheckEquals(1, Length(a.Fraction));
  CheckEquals($8000000000000000, a.Fraction[0]);
{$else}
  CheckEquals(2, Length(a.Fraction));
  CheckEquals($80000000, a.Fraction[1]);
  CheckEquals($00000000, a.Fraction[0]);
  CheckEquals(0, a.Exponent);
{$endif}
end;

procedure TBigRealTests.TestBinFloorTo;
var a: TReal;
begin
  a := 1 + 3/4 * Power(2, -32);
  a := a.BinaryFloorTo(90);
{$ifdef Limb64}
  CheckEquals(2, Length(a.Fraction));
  CheckEquals($8000000060000000, a.Fraction[1]);
{$else}
  CheckEquals(3, Length(a.Fraction));
  CheckEquals($80000000, a.Fraction[2]);
  CheckEquals($60000000, a.Fraction[1]);
{$endif}
  CheckEquals(0, a.Fraction[0]);
  CheckEquals(0, a.Exponent);
end;

procedure TBigRealTests.TestBinCeilTo2;
var a: TReal;
    d: Double;
begin
  a := 1 + 1/2 + 1/8;
  a := a.BinaryCeilTo(2);
  d := a.AsDouble;
  CheckEquals(2, d);

  a := 1 + 1/4;
  a := a.BinaryCeilTo(2);
  d := a.AsDouble;
  CheckEquals(1.5, d);

  a := 1 + 1/4 + 1/8;
  a := a.BinaryCeilTo(2);
  d := a.AsDouble;
  CheckEquals(1.5, d);

  a := 1 + 1/2 + 1/4;
  a := a.BinaryCeilTo(2);
  d := a.AsDouble;
  CheckEquals(2, d);

  a := 1 + 1/2 + 1/4 + 1/8;
  a := a.BinaryCeilTo(2);
  d := a.AsDouble;
  CheckEquals(2, d);

  a := 1/2 + 1/4;
  a := a.BinaryCeilTo(2);
  d := a.AsDouble;
  CheckEquals(1, d);

  a := 1/2 + 1/4 + 1/8;
  a := a.BinaryCeilTo(2);
  d := a.AsDouble;
  CheckEquals(1, d);
end;

procedure TBigRealTests.TestBinCeilTo34;
var a: TReal;
    b: Double;
begin
  b := Power(2, -32);
{$ifdef Limb64}
  a := 1 + (1/2 + 1/4) * b; // ($1000000060000000)
  a := a.BinaryCeilTo(34);
  CheckEquals(1, Length(a.Fraction));
  CheckEquals($8000000080000000, a.Fraction[0]);
  CheckEquals(0, a.Exponent);

  a := 1 + 1/4 * b;
  a := a.BinaryCeilTo(34);
  CheckEquals(1, Length(a.Fraction));
  CheckEquals($8000000040000000, a.Fraction[0]);
  CheckEquals(0, a.Exponent);

  a := 1 + (1 + 1/2 + 1/4) * b;
  a := a.BinaryCeilTo(34);
  CheckEquals(1, Length(a.Fraction));
  CheckEquals($8000000100000000, a.Fraction[0]);
  CheckEquals(0, a.Exponent);

  a.Init([$FFFFFFFFE0000000]);
  a := a.BinaryCeilTo(34);
  CheckEquals(1, Length(a.Fraction));
  CheckEquals($8000000000000000, a.Fraction[0]);
  CheckEquals(1, a.Exponent);
{$else}
  b := Power(2, -32);

  a := 1 + (1/2 + 1/4) * b; // ($60000000, $10000000)
  a := a.BinaryCeilTo(34);
  CheckEquals(2, Length(a.Fraction));
  CheckEquals($80000000, a.Fraction[1]);
  CheckEquals($80000000, a.Fraction[0]);
  CheckEquals(0, a.Exponent);

  a := 1 + 1/4 * b;
  a := a.BinaryCeilTo(34);
  CheckEquals(2, Length(a.Fraction));
  CheckEquals($80000000, a.Fraction[1]);
  CheckEquals($40000000, a.Fraction[0]);
  CheckEquals(0, a.Exponent);

  a := 1 + (1 + 1/2 + 1/4) * b;
  a := a.BinaryCeilTo(34);
  CheckEquals(2, Length(a.Fraction));
  CheckEquals($80000001, a.Fraction[1]);
  CheckEquals($00000000, a.Fraction[0]);
  CheckEquals(0, a.Exponent);

  a.Init(TLimbArray.Create($E0000000, $FFFFFFFF));
  a := a.BinaryCeilTo(34);
  CheckEquals(2, Length(a.Fraction));
  CheckEquals($80000000, a.Fraction[1]);
  CheckEquals($00000000, a.Fraction[0]);
  CheckEquals(1, a.Exponent);
{$endif}
end;

procedure TBigRealTests.TestFloor;
var a: TReal;
    d: Double;
begin
  a := 3 + 1/4;
  a := RealFloor(a);
  d := a.AsDouble;
  CheckEquals(3, d);

  a := 1 + 1/2;
  a := RealFloor(a);
  d := a.AsDouble;
  CheckEquals(1, d);

  a := 1/2;
  a := RealFloor(a);
  d := a.AsDouble;
  CheckEquals(0, d);

  a := -1/2;
  a := RealFloor(a);
  d := a.AsDouble;
  CheckEquals(-1, d);

  a := -1 - 1/2;
  a := RealFloor(a);
  d := a.AsDouble;
  CheckEquals(-2, d);
end;

procedure TBigRealTests.TestCeil;
var a: TReal;
    d: Double;
begin
  a := 3 + 1/4;
  a := RealCeil(a);
  d := a.AsDouble;
  CheckEquals(4, d);

  a := 1 + 1/2;
  a := RealCeil(a);
  d := a.AsDouble;
  CheckEquals(2, d);

  a := 1/2;
  a := RealCeil(a);
  d := a.AsDouble;
  CheckEquals(1, d);

  a := -1/2;
  a := RealCeil(a);
  d := a.AsDouble;
  CheckEquals(0, d);

  a := -1 - 1/2;
  a := RealCeil(a);
  d := a.AsDouble;
  CheckEquals(-1, d);
end;

procedure TBigRealTests.TestComparsion;
var a, b: TReal;
    res: Integer;
begin
  a := 1/2;
  b := 1;
  res := TReal.Compare(a, b);
  CheckEquals(-1, res);
  res := TReal.Compare(b, a);
  CheckEquals(1, res);
  res := TReal.Compare(a, a);
  CheckEquals(0, res);

  a := 1/2;
  b := 3/2;
  res := TReal.Compare(a, b);
  CheckEquals(-1, res);
  res := TReal.Compare(b, a);
  CheckEquals(1, res);

  a := -1;
  b := -1/2;
  res := TReal.Compare(a, b);
  CheckEquals(-1, res);
  res := TReal.Compare(b, a);
  CheckEquals(1, res);
end;

procedure TBigRealTests.TestCmpIntReal;
var a: TReal;
    b: TInteger;
    res: Integer;
begin
  a := 1e20;
  TInteger.DecimalParse('100000000000000000000', b);
  res := TReal.Compare(a, b);
  CheckEquals(0, res);

  TInteger.DecimalParse('100000000000000000001', b);
  res := TReal.Compare(a, b, True);
  CheckEquals(-1, res);
end;

procedure TBigRealTests.TestInt2Real;
var a: TReal;
    i: TInteger;
    d: Double;
    digs: TLimbArray;
begin
  i := 3;
  a.Init(i);
  d := a.AsDouble;
  CheckEquals(3, d);

{$ifdef Limb64}

{$else}
  i := 3;
  a.Init(i, 32 * 4);
  d := a.AsDouble;
  CheckEquals(3, d);
  CheckEquals(4, Length(a.Fraction));

  i := $300000000;
  a.Init(i);
  CheckEquals(2, Length(a.Fraction));
  CheckEquals(0, a.Fraction[0]);
  CheckEquals($C0000000, a.Fraction[1]);
  d := a.AsDouble;
  CheckEquals($300000000, d);

  digs := TLimbArray.Create(0, 0, 3);
  i.Init(digs);
  a.Init(i);
  CheckEquals(3, Length(a.Fraction));
  CheckEquals($C0000000, a.Fraction[2]);
  CheckEquals(65, a.Exponent);

  a.Init(i, 64);
  CheckEquals(2, Length(a.Fraction));
  CheckEquals($C0000000, a.Fraction[1]);
  CheckEquals(65, a.Exponent);

  digs := TLimbArray.Create(0, $C0000000, 0, $80000000);
  i.Init(digs);
  a.Init(i, 64);
  CheckEquals(2, Length(a.Fraction));
  CheckEquals($80000000, a.Fraction[1]);
  CheckEquals(1, a.Fraction[0]);
  CheckEquals(127, a.Exponent);

  digs := TLimbArray.Create(0, $60000000, 0, $40000000);
  i.Init(digs);
  a.Init(i, 64);
  CheckEquals(2, Length(a.Fraction));
  CheckEquals($80000000, a.Fraction[1]);
  CheckEquals(1, a.Fraction[0]);
  CheckEquals(126, a.Exponent);
{$endif}
end;

procedure TBigRealTests.TestIntPower;
var a: TReal;
    d: Double;
begin
  a := 2;
  a := a.IntPower(0);
  d := a.AsDouble;
  CheckEquals(1, d);

  a := 2;
  a := a.IntPower(1);
  d := a.AsDouble;
  CheckEquals(2, d);

  a := 2;
  a := a.IntPower(3);
  d := a.AsDouble;
  CheckEquals(8, d);

  a := 1/2;
  a := a.IntPower(3);
  d := a.AsDouble;
  CheckEquals(1/8, d);
end;

procedure TBigRealTests.TestSquareRoot;
var a: TReal;
begin
  // BaseForm[N[1/2 Sqrt[2], Log10[2] 128], 16]
  a.Init(2, 128);
  a := RealSqrt(a);
{$ifdef Limb64}
  CheckEquals(0, a.Exponent);
  CheckEquals(2, Length(a.Fraction));
  CheckEquals($b504f333f9de6484, a.Fraction[1]);
  CheckEquals($597d89b3754abe9f, a.Fraction[0]);
{$else}
  CheckEquals(0, a.Exponent);
  CheckEquals(4, Length(a.Fraction));
  CheckEquals($b504f333, a.Fraction[3]);
  CheckEquals($f9de6484, a.Fraction[2]);
  CheckEquals($597d89b3, a.Fraction[1]);
  CheckEquals($754abe9f, a.Fraction[0]);
{$endif}

  a.Init(4, 128);
  a := RealSqrt(a);
{$ifdef Limb64}
  CheckEquals(1, a.Exponent);
  CheckEquals(2, Length(a.Fraction));
  CheckEquals($8000000000000000, a.Fraction[1]);
  CheckEquals(0, a.Fraction[0]);
{$else}
  CheckEquals(1, a.Exponent);
  CheckEquals(4, Length(a.Fraction));
  CheckEquals($80000000, a.Fraction[3]);
  CheckEquals(0, a.Fraction[2]);
  CheckEquals(0, a.Fraction[1]);
  CheckEquals(0, a.Fraction[0]);
{$endif}
end;

procedure TBigRealTests.TestIntegerPart;
var a: TReal;
    i: TInteger;
    digs: TLimbArray;
begin
  a := 1.5;
  i := a.IntegerPart;
  CheckEquals(1, Int64(i));

  a := 2 + 1/2;
  i := a.IntegerPart;
  CheckEquals(2, Int64(i));

  a := -a;
  i := a.IntegerPart;
  CheckEquals(-2, Int64(i));

  a := 0.75;
  i := a.IntegerPart;
  CheckEquals(0, Int64(i));

  a := 0;
  i := a.IntegerPart;
  CheckEquals(0, Int64(i));

{$ifdef Limb64}
  digs := TLimbArray.Create($80000000c0000000);
{$else}
  digs := TLimbArray.Create(0, $c0000000, $80000000);
{$endif}
  a.Init(digs, 31); // a = 2147483648.75
  i := a.IntegerPart;
  CheckEquals(2147483648, Int64(i));

{$ifdef Limb64}
  digs := TLimbArray.Create($40000000c0000000);
{$else}
  digs := TLimbArray.Create(0, $c0000000, $40000000);
{$endif}
  a.Init(digs, 31); // a = 1073741824.75
  i := a.IntegerPart;
  CheckEquals(1073741824, Int64(i));
end;

procedure TBigRealTests.TestFractionalPart;
var a, f: TReal;
    digs: TLimbArray;
begin
  a := 1.5;
  f := a.FractionalPart;
  CheckEquals(0.5, f.AsDouble);

  a := 2 + 1/2;
  f := a.FractionalPart;
  CheckEquals(0.5, f.AsDouble);

  a := -a;
  f := a.FractionalPart;
  CheckEquals(-0.5, f.AsDouble);

  a := 1 + 1/4;
  f := a.FractionalPart;
  CheckEquals(0.25, f.AsDouble);

  a := 0.75;
  f := a.FractionalPart;
  CheckEquals(0.75, f.AsDouble);

  a := 0;
  f := a.FractionalPart;
  CheckEquals(0, f.AsDouble);

  a := 2;
  f := a.FractionalPart;
  CheckEquals(0, f.AsDouble);

{$ifdef Limb64}
  digs := TLimbArray.Create($80000000c0000000);
{$else}
  digs := TLimbArray.Create(0, $c0000000, $80000000);
{$endif}
  a.Init(digs, 31); // a = 2147483648.75
  f := a.FractionalPart;
  CheckEquals(0.75, f.AsDouble);

{$ifdef Limb64}
  digs := TLimbArray.Create($4000000040000000);
{$else}
  digs := TLimbArray.Create(0, $40000000, $40000000);
{$endif}
  a.Init(digs, 31); // a = 1073741824.75
  f := a.FractionalPart;
  CheckEquals(0.25, f.AsDouble);
end;

procedure TBigRealTests.DecimalStrZeroExp;
var a: TReal;
    s: String;
begin
  a := 1.75; // 1 + 1/2 + 1/4
  s := a.ToDecimalString;
  CheckEquals('1.75', s);
end;

procedure TBigRealTests.DecimalStrNegExp;
var a: TReal;
    s: String;
begin
  a := 0.75; // 1/2 + 1/4
  s := a.ToDecimalString;
  CheckEquals('7.5e-1', s);

  a := 1/256;
  s := a.ToDecimalString;
  CheckEquals('3.90625e-3', s);
end;

procedure TBigRealTests.DecimalStrPosExp;
var a: TReal;
    s: String;
begin
  a := 11 + 1/256; // a = 11.0039
  s := a.ToDecimalString;
  CheckEquals('1.100390625e1', s);
end;

procedure TBigRealTests.TestNewtonIntDiv;
var a, b, c: TInteger;
begin
  a.Init('12345678976554321');
  b.Init('23453282930302928');
  c := NewtonIntDiv(a * b, b);
  CheckTrue(a = c);
end;

{$endregion}

initialization

  RegisterTest(TBigIntegerTests.Suite);
  RegisterTest(TBigRationalTests.Suite);
  RegisterTest(TBigRealTests.Suite);

end.
