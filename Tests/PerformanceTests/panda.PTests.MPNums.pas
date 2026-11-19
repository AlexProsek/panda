unit panda.PTests.MPNums;

interface

uses
    TestFramework
  , panda.Tests.NDATestCase
  , panda.NumsQP
  , panda.NumsAP
  , panda.NumsLowLvl
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
    procedure DivReal128;
  end;

  TAPRealTests = class(TNDAPerformanceTestCase)
  published
    procedure IntDivide8192;
    procedure SchoolDivVsNewtonDiv;
  end;

  TLowLvlFuncTests = class(TNDAPerformanceTestCase)
  published
    procedure BigIntDec;
  end;

implementation

{$region 'TMPNumTests'}

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

procedure TMPNumTests.DivReal128;
var a, b, c: TReal128;
    N: Integer;
begin
  a.Init(13);
  b.Init(57);
  N := 1000000;

  SWStart;
  while N > 0 do begin
    c := a / b;
    Dec(N);
  end;
  SWStop;
end;

{$endregion}

{$region 'TAPRealTests'}

procedure TAPRealTests.IntDivide8192;
var a: TReal;
    b: TReal;
begin
  a.Init(1, 8192);

  SWStart;
  a := a.IntDivide(77);
  SWStop('1.0/77');


  b.Init(77, 8192);

  SWStart;
  a := a * b.Reciprocal;
  SWStop('1.0*Reciprocal(77)');
end;

procedure TAPRealTests.SchoolDivVsNewtonDiv;
var a, b, c: TReal;
const N = 100;
begin
  a.Init(55, 256);
  b.Init(131, 256);

  SWStart;
  DoTestLoop(procedure begin c := TReal.SchoolDiv(a, b) end, N);
  SWStop('SchoolDiv, 4 64b-limbs');

  SWStart;
  DoTestLoop(procedure begin c := TReal.NewtonDiv(a, b) end, N);
  SWStop('NewtonDiv, 4 64b-limbs');

  a.Init(55, 1024);
  b.Init(131, 1024);

  SWStart;
  DoTestLoop(procedure begin c := TReal.SchoolDiv(a, b) end, N);
  SWStop('SchoolDiv, 16 64b-limbs');

  SWStart;
  DoTestLoop(procedure begin c := TReal.NewtonDiv(a, b) end, N);
  SWStop('NewtonDiv, 16 64b-limbs');


  a.Init(55, 4096);
  b.Init(131, 4096);

  SWStart;
  DoTestLoop(procedure begin c := TReal.SchoolDiv(a, b) end, N);
  SWStop('SchoolDiv, 64 64b-limbs');

  SWStart;
  DoTestLoop(procedure begin c := TReal.NewtonDiv(a, b) end, N);
  SWStop('NewtonDiv, 64 64b-limbs');


  a.Init(55, 512 * 64);
  b.Init(131, 512 * 64);

  SWStart;
  DoTestLoop(procedure begin c := TReal.SchoolDiv(a, b) end, N);
  SWStop('SchoolDiv, 512 64b-limbs');

  SWStart;
  DoTestLoop(procedure begin c := TReal.NewtonDiv(a, b) end, N);
  SWStop('NewtonDiv, 512 64b-limbs');


  a.Init(55, 1024 * 64);
  b.Init(131, 1024 * 64);

  SWStart;
  DoTestLoop(procedure begin c := TReal.SchoolDiv(a, b) end, N);
  SWStop('SchoolDiv, 1024 64b-limbs');

  SWStart;
  DoTestLoop(procedure begin c := TReal.NewtonDiv(a, b) end, N);
  SWStop('NewtonDiv, 1024 64b-limbs');
end;

{$endregion}

{$region 'TLowLvlFuncTests'}

procedure TLowLvlFuncTests.BigIntDec;
var a, b: TLimbArray;
const N = 1000000;
begin
  SetLength(a, N);
  SetLength(b, N);

  SWStart;
  DoTestLoop(procedure begin _Dec(PByte(a), PByte(b), N); end, 40);
  SWStop;
end;

{$endregion}

initialization

  RegisterTest(TMPNumTests.Suite);
  RegisterTest(TAPRealTests.Suite);
  RegisterTest(TLowLvlFuncTests.Suite);

end.
