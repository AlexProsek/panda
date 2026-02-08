unit panda.PTests.MPNums;

interface

uses
    TestFramework
  , panda.Tests.NDATestCase
  , panda.MPNums
  ;

type
  TMPNumTests = class(TNDAPerformanceTestCase)
  published
    procedure AddInt128;
    procedure MulInt128;
    procedure ShortDivInt128;
    procedure LongDivInt128;
    procedure LessCmpInt128;
    procedure MulReal128;
  end;

implementation

procedure TMPNumTests.AddInt128;
var a, b, c: TInt128;
    N: Integer;
begin
  N := 1000000;
  a := 123456789;
  b := a;

  SWStart;
  while N > 0 do begin
    c := a + b;
    Dec(N);
  end;
  SWStop;
end;

procedure TMPNumTests.MulInt128;
var a, b, c: TInt128;
    N: Integer;
begin
  N := 1000000;
  a := 123456789;
  b := a;

  SWStart;
  while N > 0 do begin
    c := a * b;
    Dec(N);
  end;
  SWStop;
end;

procedure TMPNumTests.ShortDivInt128;
var a, b, c: TInt128;
    N: Integer;
begin
  N := 1000000;
  a.Init(High(UInt64), 12345);
  b := 12345;

  SWStart;
  while N > 0 do begin
    c := a div b;
    Dec(N);
  end;
  SWStop;
end;

procedure TMPNumTests.LongDivInt128;
var a, b, c: TInt128;
    N: Integer;
begin
  N := 1000000;
  a.Init(0, 1234567898765);
  b.Init(0, 123);

  SWStart;
  while N > 0 do begin
    c := a div b;
    Dec(N);
  end;
  SWStop;
end;

procedure TMPNumTests.LessCmpInt128;
var a, b: TInt128;
    res: Boolean;
    N: Integer;
begin
  N := 1000000;
  a.Init(0, 1);
  b.Init(0, 2);

  SWStart;
  while N > 0 do begin
    res := (a < b);
    Dec(N);
  end;
  SWStop;
end;

procedure TMPNumTests.MulReal128;
var a, b, c: TReal128;
    N: Integer;
begin
  a := cOneF128;
  b := cOneF128;
  N := 1000000;

  SWStart;
  while N > 0 do begin
    c := a * b;
    Dec(N);
  end;
  SWStop;
end;

initialization

  RegisterTest(TMPNumTests.Suite);

end.
