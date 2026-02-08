unit panda.PTests.Nums;

interface

uses
    TestFramework
  , panda.Tests.NDATestCase
  , panda.Nums
  ;

type
  TNumTests = class(TNDAPerformanceTestCase)
  published
    procedure AddCmplx64;
    procedure MulCmplx64;
    procedure DivCmplx64;

    procedure AddCmplx128;
    procedure DivCmplx128;
  end;

implementation

{$region 'TNumTests'}

procedure TNumTests.AddCmplx64;
var a, b, c: TCmplx64;
    N: Integer;
begin
  a.Init(0, 1);
  b.Init(1, 0);
  N := 1000000;

  SWStart;
  while N > 0 do begin
    c := a + b;
    Dec(N);
  end;
  SWStop;
end;

procedure TNumTests.MulCmplx64;
var a, b, c: TCmplx64;
    N: Integer;
begin
  a.Init(1, 2);
  b.Init(3, 4);
  N := 1000000;

  SWStart;
  while N > 0 do begin
    c := a * b;
    Dec(N);
  end;
  SWStop;
end;

procedure TNumTests.DivCmplx64;
var a, b, c: TCmplx64;
    N: Integer;
begin
  a.Init(1, 2);
  b.Init(3, 4);
  N := 1000000;

  SWStart;
  while N > 0 do begin
    c := a / b;
    Dec(N);
  end;
  SWStop;
end;

procedure TNumTests.AddCmplx128;
var a, b, c: TCmplx128;
    N: Integer;
begin
  a.Init(0, 1);
  b.Init(1, 0);
  N := 1000000;

  SWStart;
  while N > 0 do begin
    c := a + b;
    Dec(N);
  end;
  SWStop;
end;

procedure TNumTests.DivCmplx128;
var a, b, c: TCmplx128;
    N: Integer;
begin
  a.Init(0, 1);
  b.Init(1, 0);
  N := 1000000;

  SWStart;
  while N > 0 do begin
   c := a / b;
   Dec(N);
  end;
  SWStop;
end;

{$endregion}

initialization

  RegisterTest(TNumTests.Suite);

end.
