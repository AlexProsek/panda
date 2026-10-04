unit panda.Tests.IntFactor;

interface

uses
    TestFramework
  , panda.Intfs
  , panda.Arrays
  , panda.IntFactor
  , panda.NumsQP
  , panda.Tests.NDATestCase
  ;

type
  TIntFactorTests = class(TNDATestCase)
  published
    procedure WheelSmallFactors;
    procedure WheelTwoLargePrimes;
    procedure PrimeQLargeValue;
    procedure UInt128ModulusLargeOperands;
  end;

implementation

{$region 'TIntFactorTests'}

procedure TIntFactorTests.WheelSmallFactors;
var fs: TArray<TFactorUI64>;
begin
  fs := WheelFactorization(8831269065180497);

  CheckEquals(9, Length(fs));

  CheckEquals(13, fs[0].Value);
  CheckEquals(1,  fs[0].Power);

  CheckEquals(23, fs[1].Value);
  CheckEquals(1,  fs[1].Power);

  CheckEquals(31, fs[2].Value);
  CheckEquals(1,  fs[2].Power);

  CheckEquals(37, fs[3].Value);
  CheckEquals(1,  fs[3].Power);

  CheckEquals(41, fs[4].Value);
  CheckEquals(1,  fs[4].Power);

  CheckEquals(47, fs[5].Value);
  CheckEquals(2,  fs[5].Power);

  CheckEquals(59, fs[6].Value);
  CheckEquals(1,  fs[6].Power);

  CheckEquals(61, fs[7].Value);
  CheckEquals(1,  fs[7].Power);

  CheckEquals(79, fs[8].Value);
  CheckEquals(1,  fs[8].Power);
end;

procedure TIntFactorTests.WheelTwoLargePrimes;
var fs: TArray<TFactorUI64>;
begin
  fs := WheelFactorization(16927447722109721827);

  CheckEquals(2, Length(fs));
  CheckEquals(322255481, fs[0].Value);
  CheckEquals(1,  fs[0].Power);
  CheckEquals(52528036667, fs[1].Value);
  CheckEquals(1,  fs[1].Power);
end;

procedure TIntFactorTests.PrimeQLargeValue;
begin
  TLCG64.SetSeed(12345);

  CheckTrue(PrimeQ(52528036667));
end;

procedure TIntFactorTests.UInt128ModulusLargeOperands;
var A, B, M, R: TUInt128;
begin
  A := 1528080477;
  B := 1;
  M := 52528036667;

  R := (A * B) mod M;

  CheckEquals(UInt64(1528080477), UInt64(R));

  A := 26005733175;
  B := 1528080477;
  M := 52528036667;

  R := (A * B) mod M;

  CheckEquals(UInt64(4974558966), UInt64(R));
end;

{$endregion}

initialization

  RegisterTest(TIntFactorTests.Suite);

end.
