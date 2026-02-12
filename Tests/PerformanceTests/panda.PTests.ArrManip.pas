unit panda.PTests.ArrManip;

interface

uses
    TestFramework
  , panda.Tests.NDATestCase
  , panda.Intfs
  , panda.Arrays
  , panda.ArrManip
  , pandalib

  , Windows
  , System.SysUtils
  ;

type
  TArrManipTests = class(TNDAPerformanceTestCase)
  published
    procedure Fill2DByScalar;
    procedure Fill2DByVec;
    procedure TransposeMat;
    procedure TransposeT3;
  end;

  TArrManipPascalImplTests = class(TNDAPerformanceTestCase)
  published
    procedure Fill2DByScalar_Pascal;
    procedure Fill2DByVec_Pascal;
    procedure TransposeMat;
    procedure TransposeT3;

    procedure Fill2D_GetMemVsDynArray_Pascal;
  end;

implementation

{$region 'TArrManipTests'}

type
  TTestNDPArray = class(TNDPackedArray<Integer>)
  public
    procedure AfterConstruction; override;
  end;

procedure TTestNDPArray.AfterConstruction;
begin
  inherited;
  fFlags := fFlags or NDAF_WRITEABLE;
end;

procedure TArrManipTests.Fill2DByScalar;
var a, av: INDArray<Integer>;
const N = 1000;
begin
  a := TNDAUt.Full<Integer>([2*N, 2*N], 0); // Initialize array due to cache warm-up
  av := a[[NDIAll(2), NDIAll(2)]];

  SWStart;
  DoTestLoop(procedure begin TNDAUt.Fill<Integer>(av, 2) end, 100);
  SWStop;
end;

procedure TArrManipTests.Fill2DByVec;
var a, b, av: INDArray<Integer>;
const N = 1000;
begin
  a := nda.Full<Integer>([2 * N, 2 * N], 0);
  av := a[[NDIAll(2), NDIAll(2)]];
  b := nda.Full<Integer>([2 * N], 0);
  b := b[[NDIAll(2)]];

  SWStart;
  DoTestLoop(procedure begin TNDAUt.Fill<Integer>(av, b) end, 100);
  SWStop;
end;

procedure TArrManipTests.TransposeMat;
var a, b: INDArray<Integer>;
const N = 1000;
begin
  a := nda.Full<Integer>([N, N], 0);
  b := TNDAMan.Transpose<Integer>(a);

  SWStart;
  DoTestLoop(procedure begin TNDAMan.Transpose<Integer>(a, b, [1, 0]) end, 50);
  SWStop;
end;

procedure TArrManipTests.TransposeT3;
var a, b: INDArray<Integer>;
const N = 100;
begin
  a := nda.Full<Integer>([N, N, N], 0);
  b := TNDAMan.Transpose<Integer>(a);

  SWStart;
  DoTestLoop(procedure begin TNDAMan.Transpose<Integer>(a, b, [2, 1, 0]) end, 50);
  SWStop;
end;

{$endregion}

{$region 'TArrManipPascalImplTests'}

procedure TArrManipPascalImplTests.Fill2DByScalar_Pascal;
var a: TArray<TArray<Integer>>;
const N = 1000;
begin
  SetLength(a, 2*N, 2*N);

  SWStart;

  DoTestLoop(
    procedure
    var I, J: Integer;
    begin
      I := 0;
      while I < 2*N do begin
        J := 0;
        while J < 2*N do begin
          a[I, J] := 2;
          Inc(J, 2);
        end;
        Inc(I, 2);
      end;
    end
  , 100);

  SWStop;
end;

procedure TArrManipPascalImplTests.Fill2DByVec_Pascal;
var a: TArray<TArray<Integer>>;
    b: TArray<Integer>;
const N = 1000;
begin
  SetLength(a, 2*N, 2*N);
  SetLength(b, 2*N);

  SWStart;

  DoTestLoop(
    procedure
    var I, J: Integer;
    begin
      I := 0;
      while I < 2*N do begin
        J := 0;
        while J < 2*N do begin
          a[I, J] := b[J];
          Inc(J, 2);
        end;
        Inc(I, 2);
      end;
    end
  , 100);

  SWStop;
end;

procedure TArrManipPascalImplTests.TransposeMat;
var a, b: TArray<TArray<Integer>>;
const N = 1000;
begin
  SetLength(a, N, N);
  SetLength(b, N, N);

  SWStart;

  DoTestLoop(
    procedure
    var I, J: Integer;
    begin
      for I := 0 to N - 1 do
        for J := 0 to N - 1 do
          b[I, J] := a[J, I];
    end
  , 50);

  SWStop;
end;

procedure TArrManipPascalImplTests.TransposeT3;
var a, b: TArray<TArray<TArray<Integer>>>;
const N = 100;
begin
  SetLength(a, N, N, N);
  SetLength(b, N, N, N);

  SWStart;

  DoTestLoop(
    procedure
    var I, J, K: Integer;
    begin
      for I := 0 to N - 1 do
        for J := 0 to N - 1 do
          for K := 0 to N - 1 do
            b[I, J, K] := a[K, J, I];
    end
  , 50);

  SWStop;
end;


procedure TArrManipPascalImplTests.Fill2D_GetMemVsDynArray_Pascal;
var I, J: Integer;
    items: TArray<Integer>;
    p: PByte;
const N = 1000;
begin
  // GetMem

  GetMem(p, 4*N*N*SizeOf(Integer));

  SWStart;

  I := 0;
  while I < 2*N do begin
    J := 0;
    while J < 2*N do begin
      PInteger(p + (I * 2 * N + J) * SizeOf(Integer))^ := 2;
      Inc(J, 2);
    end;
    Inc(I, 2);
  end;

  SWStop('GetMem');

  FreeMem(p);

  // Dyn array
  SetLength(items, 4*N*N);
  P := PByte(items);

  SWStart;

  I := 0;
  while I < 2*N do begin
    J := 0;
    while J < 2*N do begin
      PInteger(p + (I * 2 * N + J) * SizeOf(Integer))^ := 2;
      Inc(J, 2);
    end;
    Inc(I, 2);
  end;

  SWStop('Dyn array');
end;

{$endregion}

initialization

  RegisterTest(TArrManipTests.Suite);
  RegisterTest(TArrManipPascalImplTests.Suite);

end.
