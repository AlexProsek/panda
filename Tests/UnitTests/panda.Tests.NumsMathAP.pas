unit panda.Tests.NumsMathAP;

interface

uses
    SysUtils
  , TestFramework
  , panda.Tests.NDATestCase
  , panda.NumsAP
  , panda.NumsMathAP
  ;

{$I AsmDefs.inc}

type
  TBigRealMathTests = class(TNDATestCase)
  published
    procedure TestExpDbl;
    procedure TestExp96;
    procedure TestExp128;
    procedure TestExp160;
    procedure TestExp192;
    procedure TestExp256;
    procedure TestExp512;
    procedure TestExp1024;
    procedure TestExp4096;
    procedure TestLn128;
    procedure TestExp;
    procedure TestLn;
    procedure TestLog10;
    procedure TestPiDbl;
    procedure TestPi128;
    procedure TestPi256;
    procedure TestSinCosDbl;
    procedure TestSinCosHalf128;
    procedure TestSinCosTwo128;
    procedure TestSinCosLarge128;
    procedure Exp128_DecimalStr;
    procedure Exp192_DecimalStr;
  end;

implementation

uses
    panda.Tests.NumsAP
  ;

{$region 'TBigRealMathTests'}

procedure TBigRealMathTests.TestExpDbl;
var a: TReal;
    d: Double;
begin
  a := 1;
  a := RealExp(a);
  d := a.AsDouble;
  CheckEquals(Exp(1), d, 1e-14);

  a := -1;
  a := RealExp(a);
  d := a.AsDouble;
  CheckEquals(Exp(-1), d, 1e-15);

  a := 9;
  a := RealExp(a);
  d := a.AsDouble;
  CheckEquals(Exp(9), d, 1e-11);
end;

procedure TBigRealMathTests.TestExp96;
var a: TReal;
begin
  a.Init(1, 96);
  a := RealExp(a);
{$ifdef Limb64}
  a := a.BinaryRoundTo(96);
  CheckEquals(2, Length(a.Fraction));
  CheckEquals(1, a.Exponent);
  CheckEquals($adf85458a2bb4a9a, a.Fraction[1]);
  CheckEquals($afdc562000000000, a.Fraction[0]);
{$else}
  CheckEquals(3, Length(a.Fraction));
  CheckEquals(1, a.Exponent);
  // Result is obtained by Mathematica expression
  // BaseForm[2^-2 N[Exp[1], Log10[2] 96], 16]
  CheckEquals($adf85458, a.Fraction[2]);
  CheckEquals($a2bb4a9a, a.Fraction[1]);
  CheckEquals($afdc5620, a.Fraction[0]);
{$endif}
end;

procedure TBigRealMathTests.TestExp128;
var a: TReal;
begin
  a.Init(1, 128);
  a := RealExp(a);
{$ifdef Limb64}
  CheckEquals(2, Length(a.Fraction));
  CheckEquals(1, a.Exponent);
  CheckEquals($adf85458a2bb4a9a, a.Fraction[1]);
  CheckEquals($afdc5620273d3cf2, a.Fraction[0]);
{$else}
  CheckEquals(4, Length(a.Fraction));
  CheckEquals(1, a.Exponent);
  // Result is obtained by Mathematica expression
  // BaseForm[2^-2 N[Exp[1], Log10[2] 128], 16]
  CheckEquals($adf85458, a.Fraction[3]);
  CheckEquals($a2bb4a9a, a.Fraction[2]);
  CheckEquals($afdc5620, a.Fraction[1]);
  CheckEquals($273d3cf2, a.Fraction[0]);
{$endif}
end;

procedure TBigRealMathTests.TestExp160;
var a: TReal;
begin
  a.Init(1, 160);
  a := RealExp(a);
{$ifdef Limb64}
  a := a.BinaryRoundTo(160);
  CheckEquals(3, Length(a.Fraction));
  CheckEquals(1, a.Exponent);
  CheckEquals($adf85458a2bb4a9a, a.Fraction[2]);
  CheckEquals($afdc5620273d3cf1, a.Fraction[1]);
  CheckEquals($d8b9c58400000000, a.Fraction[0]);
{$else}
  CheckEquals(5, Length(a.Fraction));
  CheckEquals(1, a.Exponent);
  // Result is obtained by Mathematica expression
  // BaseForm[2^-2 N[Exp[1], Log10[2] 160], 16]
  CheckEquals($adf85458, a.Fraction[4]);
  CheckEquals($a2bb4a9a, a.Fraction[3]);
  CheckEquals($afdc5620, a.Fraction[2]);
  CheckEquals($273d3cf1, a.Fraction[1]);
  CheckEquals($d8b9c584, a.Fraction[0]);
{$endif}
end;

procedure TBigRealMathTests.TestExp192;
var a: TReal;
begin
  a.Init(1, 192);
  a := RealExp(a);
{$ifdef Limb64}
  CheckEquals(3, Length(a.Fraction));
  CheckEquals(1, a.Exponent);
  CheckEquals($adf85458a2bb4a9a, a.Fraction[2]);
  CheckEquals($afdc5620273d3cf1, a.Fraction[1]);
  CheckEquals($d8b9c583ce2d3696, a.Fraction[0]);
{$else}
  CheckEquals(6, Length(a.Fraction));
  CheckEquals(1, a.Exponent);
  // Result is obtained by Mathematica expression
  // BaseForm[2^-2 N[Exp[1], Log10[2] 192], 16]
  CheckEquals($adf85458, a.Fraction[5]);
  CheckEquals($a2bb4a9a, a.Fraction[4]);
  CheckEquals($afdc5620, a.Fraction[3]);
  CheckEquals($273d3cf1, a.Fraction[2]);
  CheckEquals($d8b9c583, a.Fraction[1]);
  CheckEquals($ce2d3696, a.Fraction[0]);
{$endif}
end;

procedure TBigRealMathTests.TestExp256;
var a: TReal;
begin
  a.Init(1, 256);
  a := RealExp(a);
{$ifdef Limb64}
  CheckEquals(4, Length(a.Fraction));
  CheckEquals(1, a.Exponent);
  CheckEquals($adf85458a2bb4a9a, a.Fraction[3]);
  CheckEquals($afdc5620273d3cf1, a.Fraction[2]);
  CheckEquals($d8b9c583ce2d3695, a.Fraction[1]);
  CheckEquals($a9e13641146433fc, a.Fraction[0]);
{$else}
  CheckEquals(8, Length(a.Fraction));
  CheckEquals(1, a.Exponent);
  // Result is obtained by Mathematica expression
  // BaseForm[2^-2 N[Exp[1], Log10[2] 256], 16]
  CheckEquals($adf85458, a.Fraction[7]);
  CheckEquals($a2bb4a9a, a.Fraction[6]);
  CheckEquals($afdc5620, a.Fraction[5]);
  CheckEquals($273d3cf1, a.Fraction[4]);
  CheckEquals($d8b9c583, a.Fraction[3]);
  CheckEquals($ce2d3695, a.Fraction[2]);
  CheckEquals($a9e13641, a.Fraction[1]);
  CheckEquals($146433fc, a.Fraction[0]);
{$endif}
end;

procedure TBigRealMathTests.TestExp512;
var a: TReal;
    f: TArray<UInt64>;
begin
  a.Init(1, 512);
  a := RealExp(a);

  f := GetFraction(a);
  CheckEquals(8, Length(f));
  CheckEquals(1, a.Exponent);
  // Result is obtained by Mathematica expression
  // BaseForm[2^-2 N[Exp[1], Log10[2] 512], 16]
  CheckEquals($adf85458a2bb4a9a, f[7]);
  CheckEquals($afdc5620273d3cf1, f[6]);
  CheckEquals($d8b9c583ce2d3695, f[5]);
  CheckEquals($a9e13641146433fb, f[4]);
  CheckEquals($cc939dce249b3ef9, f[3]);
  CheckEquals($7d2fe363630c75d8, f[2]);
  CheckEquals($f681b202aec4617a, f[1]);
  CheckEquals($d3df1ed5d5fd6561, f[0]);
end;

procedure TBigRealMathTests.TestExp1024;
var a: TReal;
    f: TArray<UInt64>;
begin
  a.Init(1, 1024);
  a := RealExp(a);

  f := GetFraction(a);
  CheckEquals(16, Length(f));
  CheckEquals(1, a.Exponent);
  // Result is obtained by Mathematica expression
  // BaseForm[2^-2 N[Exp[1], Log10[2] 1024, 16]
  CheckEquals($adf85458a2bb4a9a, f[15]);
  CheckEquals($afdc5620273d3cf1, f[14]);
  CheckEquals($d8b9c583ce2d3695, f[13]);
  CheckEquals($a9e13641146433fb, f[12]);
  CheckEquals($cc939dce249b3ef9, f[11]);
  CheckEquals($7d2fe363630c75d8, f[10]);
  CheckEquals($f681b202aec4617a, f[9]);
  CheckEquals($d3df1ed5d5fd6561, f[8]);
  CheckEquals($2433f51f5f066ed0, f[7]);
  CheckEquals($856365553ded1af3, f[6]);
  CheckEquals($b557135e7f57c935, f[5]);
  CheckEquals($984f0c70e0e68b77, f[4]);
  CheckEquals($e2a689daf3efe872, f[3]);
  CheckEquals($1df158a136ade735, f[2]);
  CheckEquals($30acca4f483a797a, f[1]);
  CheckEquals($bc0ab182b324fb62, f[0]);
end;

procedure TBigRealMathTests.TestExp4096;
var a: TReal;
    f: TArray<UInt64>;
begin
  a.Init(1, 4096);
  a := RealExp(a);

  f := GetFraction(a);
  CheckEquals(64, Length(f));
  CheckEquals($adf85458a2bb4a9a, f[63]);
  CheckEquals($afdc5620273d3cf1, f[62]);
  CheckEquals($d8b9c583ce2d3695, f[61]);
  CheckEquals($a9e13641146433fb, f[60]);
  CheckEquals($cc939dce249b3ef9, f[59]);
  CheckEquals($7d2fe363630c75d8, f[58]);
  CheckEquals($f681b202aec4617a, f[57]);
  CheckEquals($d3df1ed5d5fd6561, f[56]);
  CheckEquals($2433f51f5f066ed0, f[55]);
  CheckEquals($856365553ded1af3, f[54]);
  CheckEquals($b557135e7f57c935, f[53]);
  CheckEquals($984f0c70e0e68b77, f[52]);
  CheckEquals($e2a689daf3efe872, f[51]);
  CheckEquals($1df158a136ade735, f[50]);
  CheckEquals($30acca4f483a797a, f[49]);
  CheckEquals($bc0ab182b324fb61, f[48]);
  CheckEquals($d108a94bb2c8e3fb, f[47]);
  CheckEquals($b96adab760d7f468, f[46]);
  CheckEquals($1d4f42a3de394df4, f[45]);
  CheckEquals($ae56ede76372bb19, f[44]);
  CheckEquals($0b07a7c8ee0a6d70, f[43]);
  CheckEquals($9e02fce1cdf7e2ec, f[42]);
  CheckEquals($c03404cd28342f61, f[41]);
  CheckEquals($9172fe9ce98583ff, f[40]);
  CheckEquals($8e4f1232eef28183, f[39]);
  CheckEquals($c3fe3b1b4c6fad73, f[38]);
  CheckEquals($3bb5fcbc2ec22005, f[37]);
  CheckEquals($c58ef1837d1683b2, f[36]);
  CheckEquals($c6f34a26c1b2effa, f[35]);
  CheckEquals($886b4238611fcfdc, f[34]);
  CheckEquals($de355b3b6519035b, f[33]);
  CheckEquals($bc34f4def99c0238, f[32]);
  CheckEquals($61b46fc9d6e6c907, f[31]);
  CheckEquals($7ad91d2691f7f7ee, f[30]);
  CheckEquals($598cb0fac186d91c, f[29]);
  CheckEquals($aefe130985139270, f[28]);
  CheckEquals($b4130c93bc437944, f[27]);
  CheckEquals($f4fd4452e2d74dd3, f[26]);
  CheckEquals($64f2e21e71f54bff, f[25]);
  CheckEquals($5cae82ab9c9df69e, f[24]);
  CheckEquals($e86d2bc522363a0d, f[23]);
  CheckEquals($abc521979b0deada, f[22]);
  CheckEquals($1dbf9a42d5c4484e, f[21]);
  CheckEquals($0abcd06bfa53ddef, f[20]);
  CheckEquals($3c1b20ee3fd59d7c, f[19]);
  CheckEquals($25e41d2b669e1ef1, f[18]);
  CheckEquals($6e6f52c3164df4fb, f[17]);
  CheckEquals($7930e9e4e58857b6, f[16]);
  CheckEquals($ac7d5f42d69f6d18, f[15]);
  CheckEquals($7763cf1d55034004, f[14]);
  CheckEquals($87f55ba57e31cc7a, f[13]);
  CheckEquals($7135c886efb4318a, f[12]);
  CheckEquals($ed6a1e012d9e6832, f[11]);
  CheckEquals($a907600a918130c4, f[10]);
  CheckEquals($6dc778f971ad0038, f[9]);
  CheckEquals($092999a333cb8b7a, f[8]);
  CheckEquals($1a1db93d7140003c, f[7]);
  CheckEquals($2a4ecea9f98d0acc, f[6]);
  CheckEquals($0a8291cdcec97dcf, f[5]);
  CheckEquals($8ec9b55a7f88a46b, f[4]);
  CheckEquals($4db5a851f44182e1, f[3]);
  CheckEquals($c68a007e5e0dd902, f[2]);
  CheckEquals($0bfd64b645036c7a, f[1]);
  CheckEquals($4e677d2c38532a3a, f[0]);
end;

procedure TBigRealMathTests.TestLn128;
var a: TReal;
begin
  a.InitRational(3, 2, 128);
  a := RealLn(a);
{$ifdef Limb64}
  CheckEquals(2, Length(a.Fraction));
  CheckEquals(-2, a.Exponent);
  CheckEquals($cf991f65fcc25f95, a.Fraction[1]);
  CheckEquals($b46bb37a02910c0d, a.Fraction[0]);
{$else}
  CheckEquals(4, Length(a.Fraction));
  CheckEquals(-2, a.Exponent);
  // Result is obtained by Mathematica expression
  // BaseForm[2 N[Log[3/2], Log10[2] 128], 16]
  CheckEquals($cf991f65, a.Fraction[3]);
  CheckEquals($fcc25f95, a.Fraction[2]);
  CheckEquals($b46bb37a, a.Fraction[1]);
  CheckEquals($02910c0d, a.Fraction[0]);
{$endif}
end;

procedure TBigRealMathTests.TestExp;
var a: TReal;
    f: TArray<UInt64>;
begin
  a.Init(0.22314355131421, 128); // a := Log(1.25); (* with 53 bits precision *)
  a := RealExp(a);
  CheckEquals(1.25, a.AsDouble, 1e-15);

  a.Init(2, 128);
  a := RealExp(a);
  f := GetFraction(a);

  CheckEquals(2, Length(f));
  CheckEquals($ec7325c6a6ed6e61, f[1]);
  CheckEquals($9d1dd1035b455dd4, f[0]);
end;

procedure TBigRealMathTests.TestLn;
var a: TReal;
begin
  /// BaseForm[N[1/4 Log[10], Log10[2] 128], 16]
  a.Init(10, 128);
  a := RealLn(a);
{$ifdef Limb64}
  CheckEquals(2, Length(a.Fraction));
  CheckEquals(1, a.Exponent);
  CheckEquals($935d8dddaaa8ac16, a.Fraction[1]);
  CheckEquals($ea56d62b82d30a29, a.Fraction[0]);
{$else}
  CheckEquals(4, Length(a.Fraction));
  CheckEquals(1, a.Exponent);
  CheckEquals($935d8ddd, a.Fraction[3]);
  CheckEquals($aaa8ac16, a.Fraction[2]);
  CheckEquals($ea56d62b, a.Fraction[1]);
  CheckEquals($82d30a29, a.Fraction[0]);
{$endif}

  /// BaseForm[N[Log[1/2], Log10[2] 128], 16]
  a.Init(0.5, 128);
  a := RealLn(a);
{$ifdef Limb64}
  CheckEquals(2, Length(a.Fraction));
  CheckEquals($b17217f7d1cf79ab, a.Fraction[1]);
  CheckEquals($c9e3b39803f2f6af, a.Fraction[0]);
{$else}
  CheckEquals(4, Length(a.Fraction));
  CheckEquals($b17217f7, a.Fraction[3]);
  CheckEquals($d1cf79ab, a.Fraction[2]);
  CheckEquals($c9e3b398, a.Fraction[1]);
  CheckEquals($03f2f6af, a.Fraction[0]);
{$endif}
end;

procedure TBigRealMathTests.TestLog10;
var a: TReal;
begin
  /// BaseForm[N[2 Log10[2], Log10[2] 128], 16]
  a.Init(2, 128);
  a := RealLog10(a);
{$ifdef Limb64}
  CheckEquals(2, Length(a.Fraction));
  CheckEquals(-2, a.Exponent);
  CheckEquals($9a209a84fbcff798, a.Fraction[1]);
  CheckEquals($8f8959ac0b7c9178, a.Fraction[0]);
{$else}
  CheckEquals(4, Length(a.Fraction));
  CheckEquals(-2, a.Exponent);
  CheckEquals($9a209a84, a.Fraction[3]);
  CheckEquals($fbcff798, a.Fraction[2]);
  CheckEquals($8f8959ac, a.Fraction[1]);
  CheckEquals($0b7c9178, a.Fraction[0]);
{$endif}

  a.InitRational(1, 256, 128);
  a := RealLog10(a);
{$ifdef Limb64}
  CheckEquals(2, Length(a.Fraction));
  CheckEquals(-1, a.Signum);
  CheckEquals(1, a.Exponent);
  CheckEquals($9a209a84fbcff798, a.Fraction[1]);
  CheckEquals($8f8959ac0b7c9178, a.Fraction[0]);
{$else}
  CheckEquals(4, Length(a.Fraction));
  CheckEquals(-1, a.Signum);
  CheckEquals(1, a.Exponent);
  CheckEquals($9a209a84, a.Fraction[3]);
  CheckEquals($fbcff798, a.Fraction[2]);
  CheckEquals($8f8959ac, a.Fraction[1]);
  CheckEquals($0b7c9178, a.Fraction[0]);
{$endif}
end;

procedure TBigRealMathTests.TestPiDbl;
var a: TReal;
    d: Double;
begin
  a := EvalPi(53);

  d := Pi;
  CheckEquals(d, a.AsDouble, 0);
end;

procedure TBigRealMathTests.TestPi128;
var a: TReal;
begin
  a := EvalPi(128);

{$ifdef Limb64}
  CheckEquals(2, Length(a.Fraction));
  CheckEquals(1, a.Exponent);
  // round(Pi * 2^126)
  CheckEquals($c90fdaa22168c234, a.Fraction[1]);
  CheckEquals($c4c6628b80dc1cd1, a.Fraction[0]);
{$else}
  CheckEquals(4, Length(a.Fraction));
  CheckEquals(1, a.Exponent);
  // round(Pi * 2^126)
  CheckEquals($c90fdaa2, a.Fraction[3]);
  CheckEquals($2168c234, a.Fraction[2]);
  CheckEquals($c4c6628b, a.Fraction[1]);
  CheckEquals($80dc1cd1, a.Fraction[0]);
{$endif}
end;

procedure TBigRealMathTests.TestPi256;
var a: TReal;
begin
  a := EvalPi(256);

{$ifdef Limb64}
  CheckEquals(4, Length(a.Fraction));
  CheckEquals(1, a.Exponent);
  // round(Pi * 2^254)
  CheckEquals($c90fdaa22168c234, a.Fraction[3]);
  CheckEquals($c4c6628b80dc1cd1, a.Fraction[2]);
  CheckEquals($29024e088a67cc74, a.Fraction[1]);
  CheckEquals($020bbea63b139b22, a.Fraction[0]);
{$else}
  CheckEquals(8, Length(a.Fraction));
  CheckEquals(1, a.Exponent);
  // round(Pi * 2^254)
  CheckEquals($c90fdaa2, a.Fraction[7]);
  CheckEquals($2168c234, a.Fraction[6]);
  CheckEquals($c4c6628b, a.Fraction[5]);
  CheckEquals($80dc1cd1, a.Fraction[4]);
  CheckEquals($29024e08, a.Fraction[3]);
  CheckEquals($8a67cc74, a.Fraction[2]);
  CheckEquals($020bbea6, a.Fraction[1]);
  CheckEquals($3b139b22, a.Fraction[0]);
{$endif}
end;

procedure TBigRealMathTests.TestSinCosDbl;
var a, s, c: TReal;
    ds, dc: Double;
begin
  a.Init(0.5, 53);
  s := RealSin(a);
  c := RealCos(a);

  ds := Sin(0.5);
  dc := Cos(0.5);
  CheckEquals(ds, s.AsDouble, 0);
  CheckEquals(dc, c.AsDouble, 0);

  a.Init(2, 53);
  s := RealSin(a);
  c := RealCos(a);

  ds := Sin(2);
  dc := Cos(2);
  CheckEquals(ds, s.AsDouble, 0);
  CheckEquals(dc, c.AsDouble, 0);
end;

procedure TBigRealMathTests.TestSinCosHalf128;
var a, s, c: TReal;
begin
  a.Init(0.5, 128);
  s := RealSin(a);
  c := RealCos(a);

{$ifdef Limb64}
  CheckEquals(2, Length(s.Fraction));
  CheckEquals(-2, s.Exponent);
  CheckEquals(1, s.Signum);
  CheckEquals($f57743a2582f7f43, s.Fraction[1]);
  CheckEquals($b25e1b27ec1bdb33, s.Fraction[0]);
  CheckEquals(2, Length(c.Fraction));
  CheckEquals(-1, c.Exponent);
  CheckEquals(1, c.Signum);
  CheckEquals($e0a94032dbea7ced, c.Fraction[1]);
  CheckEquals($bddd9da2fafad985, c.Fraction[0]);
{$else}
  CheckEquals(4, Length(s.Fraction));
  CheckEquals(-2, s.Exponent);
  CheckEquals(1, s.Signum);
  CheckEquals($f57743a2, s.Fraction[3]);
  CheckEquals($582f7f43, s.Fraction[2]);
  CheckEquals($b25e1b27, s.Fraction[1]);
  CheckEquals($ec1bdb33, s.Fraction[0]);
  CheckEquals(4, Length(c.Fraction));
  CheckEquals(-1, c.Exponent);
  CheckEquals(1, c.Signum);
  CheckEquals($e0a94032, c.Fraction[3]);
  CheckEquals($dbea7ced, c.Fraction[2]);
  CheckEquals($bddd9da2, c.Fraction[1]);
  CheckEquals($fafad985, c.Fraction[0]);
{$endif}
end;

procedure TBigRealMathTests.TestSinCosTwo128;
var a, s, c: TReal;
begin
  a.Init(2, 128);
  s := RealSin(a);
  c := RealCos(a);

{$ifdef Limb64}
  CheckEquals(2, Length(s.Fraction));
  CheckEquals(-1, s.Exponent);
  CheckEquals(1, s.Signum);
  CheckEquals($e8c7b7568da22efd, s.Fraction[1]);
  CheckEquals($5c240c4004e4ddcb, s.Fraction[0]);
  CheckEquals(2, Length(c.Fraction));
  CheckEquals(-2, c.Exponent);
  CheckEquals(-1, c.Signum);
  CheckEquals($d51132ba9b902521, c.Fraction[1]);
  CheckEquals($997c565db9586bb4, c.Fraction[0]);
{$else}
  CheckEquals(4, Length(s.Fraction));
  CheckEquals(-1, s.Exponent);
  CheckEquals(1, s.Signum);
  CheckEquals($e8c7b756, s.Fraction[3]);
  CheckEquals($8da22efd, s.Fraction[2]);
  CheckEquals($5c240c40, s.Fraction[1]);
  CheckEquals($04e4ddcb, s.Fraction[0]);
  CheckEquals(4, Length(c.Fraction));
  CheckEquals(-2, c.Exponent);
  CheckEquals(-1, c.Signum);
  CheckEquals($d51132ba, c.Fraction[3]);
  CheckEquals($9b902521, c.Fraction[2]);
  CheckEquals($997c565d, c.Fraction[1]);
  CheckEquals($b9586bb4, c.Fraction[0]);
{$endif}
end;

procedure TBigRealMathTests.TestSinCosLarge128;
var a, s, c: TReal;
begin
  a.Init(1048576.5, 128);
  s := RealSin(a);
  c := RealCos(a);

{$ifdef Limb64}
  CheckEquals(2, Length(s.Fraction));
  CheckEquals(-1, s.Exponent);
  CheckEquals(1, s.Signum);
  CheckEquals($be15d8ebe0492119, s.Fraction[1]);
  CheckEquals($0e4a55dc75e7cf34, s.Fraction[0]);
  CheckEquals(2, Length(c.Fraction));
  CheckEquals(-1, c.Exponent);
  CheckEquals(1, c.Signum);
  CheckEquals($ab798416c7c52d60, c.Fraction[1]);
  CheckEquals($bfa5ac42decbd9fb, c.Fraction[0]);
{$else}
  CheckEquals(4, Length(s.Fraction));
  CheckEquals(-1, s.Exponent);
  CheckEquals(1, s.Signum);
  CheckEquals($be15d8eb, s.Fraction[3]);
  CheckEquals($e0492119, s.Fraction[2]);
  CheckEquals($0e4a55dc, s.Fraction[1]);
  CheckEquals($75e7cf34, s.Fraction[0]);
  CheckEquals(4, Length(c.Fraction));
  CheckEquals(-1, c.Exponent);
  CheckEquals(1, c.Signum);
  CheckEquals($ab798416, c.Fraction[3]);
  CheckEquals($c7c52d60, c.Fraction[2]);
  CheckEquals($bfa5ac42, c.Fraction[1]);
  CheckEquals($decbd9fb, c.Fraction[0]);
{$endif}
end;

procedure TBigRealMathTests.Exp128_DecimalStr;
var a: TReal;
    s: String;
begin
  a := RealExp(TReal.One(128));
  s := a.ToDecimalString;
  //  N[Exp[1], Floor[Log[10, 2^128 - 1]]]
  CheckEquals('2.7182818284590452353602874713526624978', s);
end;

procedure TBigRealMathTests.Exp192_DecimalStr;
var a: TReal;
    s: String;
begin
  a := RealExp(TReal.One(192));
  s := a.ToDecimalString;
  CheckEquals('2.71828182845904523536028747135266249775724709369995957497', s);
end;

{$endregion}

initialization

  RegisterTest(TBigRealMathTests.Suite);

end.
