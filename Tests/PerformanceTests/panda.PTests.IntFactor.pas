unit panda.PTests.IntFactor;

interface

uses
    TestFramework
  , panda.IntFactor
  , panda.Tests.NDATestCase
  ;

type
  TIntFactorTests = class(TNDAPerformanceTestCase)
  published
    procedure Wheel_Prime10e5;
    procedure Wheel_Prime10e6;
    procedure Wheel_Prime10e7;
    procedure Wheel_Largest32BitPrime;
  end;

implementation

{$region 'TIntFactorTests'}

procedure TIntFactorTests.Wheel_Prime10e5;
var N: UInt64;
begin
  N := UInt64(1299709) * 1299709; // Prime[10^5] - 10^5-th item of a prime number sequence

  SWStart;
  WheelFactorization(N);
  SWStop;
end;

procedure TIntFactorTests.Wheel_Prime10e6;
var N: UInt64;
begin
  N := UInt64(15485863) * 15485863; // Prime[10^6]

  SWStart;
  WheelFactorization(N);
  SWStop;
end;

procedure TIntFactorTests.Wheel_Prime10e7;
var N: UInt64;
begin
  N := UInt64(179424673) * 179424673; // Prime[10^7]

  SWStart;
  WheelFactorization(N);
  SWStop;
end;

procedure TIntFactorTests.Wheel_Largest32BitPrime;
var N: UInt64;
begin
  N := UInt64(4294967197) * 4294967197; // 4294967197 is the largest prime below 2^32

  SWStart;
  WheelFactorization(N);
  SWStop;
end;

{$endregion}

initialization

  RegisterTest(TIntFactorTests.Suite);

end.
