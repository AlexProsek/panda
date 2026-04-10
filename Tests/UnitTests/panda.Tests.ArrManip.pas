unit panda.Tests.ArrManip;

interface

uses
    TestFramework
  , panda.Intfs
  , panda.Arrays
  , panda.ArrManip
  , panda.Tests.NDATestCase
  ;

type
  TArrManipLowLvlTests = class(TNDATestCase)
  published
    procedure BlockTr4x4_4B;
    procedure BlockTr8x8_4B;
    procedure BlockTr4x6_4B;
    procedure BlockTr6x4_4B;
    procedure BlockTr6x5_4B;
    procedure BlockTr10x10_4B;

    procedure BlockTr8x8_1B;
    procedure BlockTr20x20_1B;

    procedure BlockTr8x8_2B;
    procedure BlockTr20x20_2B;
  end;

  TArrManipTests = class(TNDATestCase)
  published
    procedure SetPartWithCast;

    procedure Flatten2DPacked;
    procedure Flatten2DStridesLvl0;
    procedure Flatten2DStridesLvl1;
    procedure Flatten2DStrides;

    procedure Transpose2D;
    procedure Transpose3D_210;
    procedure Transpose3D_201;
    procedure Transpose3D_102;
    procedure Transpose3D_120;

    procedure Flip1D;
    procedure Flip2D_A0;
    procedure Flip2D_A1;
    procedure Flip2D_A0A1;

    procedure Concat1D;
    procedure Concat2D_A0;
    procedure Concat2D_A1;
    procedure Concat2Dx3_A0;
    procedure Concat2Dx3_A1;
    procedure Concat_SingleArr;

    procedure Append1D;
    procedure Prepend1D;
    procedure Insert1D;
    procedure InsertList1D;
    procedure Insert2D_A0;
    procedure Insert2D_A1;
    procedure InsertList2D_A0;
    procedure InsertList2D_A1;
    procedure Riffle1D;
    procedure Riffle2D_A0;
    procedure Riffle2D_A1;

    procedure Pad1D;
    procedure Pad2D;
    procedure Pad3D;
    procedure Pad2D_DiffPadWidths;
  end;

implementation

uses
    panda.DynArrayUtils
  ;

{$region 'TArrManipLowLvlTests'}

procedure TArrManipLowLvlTests.BlockTr4x4_4B;
var a, b: array [0..3, 0..3] of Single;
    I, J: Integer;
begin
  for I := 0 to 3 do
    for J := 0 to 3 do begin
      a[I, J] := 3*I + J;
      b[J, I] := 0;
    end;

  Tr4x4_4B(@a, @b, 16, 16);

  for I := 0 to 3 do
    for J := 0 to 3 do
      CheckEquals(a[I, J], b[J, I]);
end;

procedure TArrManipLowLvlTests.BlockTr8x8_4B;
var a, b: array [0..7, 0..7] of Single;
    I, J: Integer;
begin
  for I := 0 to 7 do
    for J := 0 to 7 do begin
      a[I, J] := 7*I + J;
      b[J, I] := 0;
    end;

  CTr_4B(@a, @b, 8, 8, 32, 32);

  for I := 0 to 7 do
    for J := 0 to 7 do
      CheckEquals(a[I, J], b[J, I]);
end;

procedure TArrManipLowLvlTests.BlockTr4x6_4B;
var a: array [0..3, 0..5] of Single;
    b: array [0..5, 0..3] of Single;
    I, J: Integer;
begin
  for I := 0 to 3 do
    for J := 0 to 5 do begin
      a[I, J] := 6*I + J;
      b[J, I] := 0;
    end;

  CTr_4B(@a, @b, 4, 6, 24, 16);

  for I := 0 to 3 do
    for J := 0 to 5 do
      CheckEquals(a[I, J], b[J, I]);
end;

procedure TArrManipLowLvlTests.BlockTr6x4_4B;
var a: array [0..5, 0..3] of Single;
    b: array [0..3, 0..5] of Single;
    I, J: Integer;
begin
  for I := 0 to 5 do
    for J := 0 to 3 do begin
      a[I, J] := 4*I + J;
      b[J, I] := 0;
    end;

  CTr_4B(@a, @b, 6, 4, 16, 24);

  for I := 0 to 5 do
    for J := 0 to 3 do
      CheckEquals(a[I, J], b[J, I]);
end;

procedure TArrManipLowLvlTests.BlockTr6x5_4B;
var a: array [0..5, 0..4] of Single;
    b: array [0..4, 0..5] of Single;
    I, J: Integer;
begin
  for I := 0 to 5 do
    for J := 0 to 4 do begin
      a[I, J] := 5*I + J;
      b[J, I] := 0;
    end;

  CTr_4B(@a, @b, 6, 5, 20, 24);

  for I := 0 to 5 do
    for J := 0 to 4 do
      CheckEquals(a[I, J], b[J, I]);
end;

procedure TArrManipLowLvlTests.BlockTr10x10_4B;
var a: array [0..9, 0..9] of Single;
    b: array [0..9, 0..9] of Single;
    I, J: Integer;
begin
  for I := 0 to 9 do
    for J := 0 to 9 do begin
      a[I, J] := 10*I + J;
      b[J, I] := 0;
    end;

  CTr_4B(@a, @b, 10, 10, 40, 40);

  for I := 0 to 9 do
    for J := 0 to 9 do
      CheckEquals(a[I, J], b[J, I]);
end;

procedure TArrManipLowLvlTests.BlockTr8x8_1B;
var a: array [0..7, 0..7] of Byte;
    b: array [0..7, 0..7] of Byte;
    I, J: Integer;
begin
  for I := 0 to 7 do
    for J := 0 to 7 do begin
      a[I, J] := 8*I + J;
      b[I, J] := 0;
    end;

  Tr8x8_1B(@a, @b, 8, 8);

  for I := 0 to 7 do
    for J := 0 to 7 do
      CheckEquals(a[I, J], b[J, I]);
end;

procedure TArrManipLowLvlTests.BlockTr20x20_1B;
var a: array [0..19, 0..19] of Byte;
    b: array [0..19, 0..19] of Byte;
    I, J: Integer;
begin
  for I := 0 to 19 do
    for J := 0 to 19 do begin
      a[I, J] := 10*I + J shr 1;
      b[I, J] := 0;
    end;

  CTr_1B(@a, @b, 20, 20, 20, 20);

  for I := 0 to 19 do
    for J := 0 to 19 do
      CheckEquals(a[I, J], b[J, I]);
end;

procedure TArrManipLowLvlTests.BlockTr8x8_2B;
var a, b: array [0..7, 0..7] of Int16;
    I, J: Integer;
begin
  for I := 0 to 7 do
    for J := 0 to 7 do begin
      a[I, J] := 8*I + J;
      b[I, J] := 0;
    end;

  Tr8x8_2B(@a, @b, 16, 16);

  for I := 0 to 7 do
    for J := 0 to 7 do
      CheckEquals(a[I, J], b[J, I]);
end;

procedure TArrManipLowLvlTests.BlockTr20x20_2B;
var a, b: array [0..19, 0..19] of Word;
    I, J: Integer;
begin
  for I := 0 to 19 do
    for J := 0 to 19 do begin
      a[I, J] := 10*I + J;
      b[I, J] := 0;
    end;

  CTr_2B(@a, @b, 20, 20, 40, 40);

  for I := 0 to 19 do
    for J := 0 to 19 do
      CheckEquals(a[I, J], b[J, I]);
end;

{$endregion}

{$region 'TArrManipTests'}

procedure TArrManipTests.SetPartWithCast;
var a: INDArray<Single>;
    p: INDArray;
    ai: TNDAMatItems<Single>;
const tol = 1e-8;
begin
  a := TNDAUt.AsArray<Single>([1, 2, 3, 4], [2, 2]);
  p := TNDAUt.AsArray<Integer>([5, 6]);
  TNDAMan.SetPart(a, [NDI(0)], p);

  ai := a;
  CheckEquals(5, ai[0, 0], tol);
  CheckEquals(6, ai[0, 1], tol);

  CheckEquals(3, ai[1, 0], tol);
  CheckEquals(4, ai[1, 1], tol);
end;

procedure TArrManipTests.Flatten2DPacked;
var a: INDArray<Integer>;
    arr: TArray<Integer>;
    I: Integer;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3, 4, 5, 6], [2, 3]);
  a := TNDAMan.Flatten<Integer>(a);

  CheckTrue(TNDAUt.TryAsDynArray<Integer>(a, arr));
  CheckEquals(6, Length(arr));
  for I := 0 to High(arr) do
    CheckEquals(I + 1, arr[I]);
end;

procedure TArrManipTests.Flatten2DStridesLvl0;
var a: INDArray<Integer>;
    arr: TArray<Integer>;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3]);
  a := TNDAMan.Flatten<Integer>(a[[NDISpan(0, -1, 2), NDIAll]]);

  CheckTrue(TNDAUt.TryAsDynArray<Integer>(a, arr));
  CheckEquals(6, Length(arr));
  CheckEquals(1, arr[0]);
  CheckEquals(2, arr[1]);
  CheckEquals(3, arr[2]);
  CheckEquals(7, arr[3]);
  CheckEquals(8, arr[4]);
  CheckEquals(9, arr[5]);
end;

procedure TArrManipTests.Flatten2DStridesLvl1;
var a: INDArray<Integer>;
    arr: TArray<Integer>;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3]);
  a := TNDAMan.Flatten<Integer>(a[[NDIAll, NDISpan(0, -1, 2)]]);

  CheckTrue(TNDAUt.TryAsDynArray<Integer>(a, arr));
  CheckEquals(6, Length(arr));
  CheckEquals(1, arr[0]);
  CheckEquals(3, arr[1]);
  CheckEquals(4, arr[2]);
  CheckEquals(6, arr[3]);
  CheckEquals(7, arr[4]);
  CheckEquals(9, arr[5]);
end;

procedure TArrManipTests.Flatten2DStrides;
var a: INDArray<Integer>;
    arr: TArray<Integer>;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3]);
  a := TNDAMan.Flatten<Integer>(a[[NDISpan(0, -1, 2), NDISpan(0, -1, 2)]]);

  CheckTrue(TNDAUt.TryAsDynArray<Integer>(a, arr));
  CheckEquals(4, Length(arr));
  CheckEquals(1, arr[0]);
  CheckEquals(3, arr[1]);
  CheckEquals(7, arr[2]);
  CheckEquals(9, arr[3]);
end;

procedure TArrManipTests.Transpose2D;
var a: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
    I, J: Integer;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3, 4, 5, 6], [2, 3]);
  a := TNDAMan.Transpose<Integer>(a);

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, m));
  CheckEquals(3, Length(m));
  for I := 0 to High(m) do
    CheckEquals(2, Length(m[I]));
  for I := 0 to High(m) do
    for J := 0 to High(m[I]) do
      CheckEquals(1 + I + 3*J, m[I, J]);
end;

procedure TArrManipTests.Transpose3D_210;
var a: INDArray<Integer>;
    ai: TNDAItems<Integer>;
begin
  a := iRng2NDA([2, 3, 2]);
  a := TNDAMan.Transpose<Integer>(a);

  CheckEquals([2, 3, 2], a.Shape);
  ai := a;
  CheckEquals(1,  ai[[0, 0, 0]]);
  CheckEquals(7,  ai[[0, 0, 1]]);
  CheckEquals(3,  ai[[0, 1, 0]]);
  CheckEquals(9,  ai[[0, 1, 1]]);
  CheckEquals(5,  ai[[0, 2, 0]]);
  CheckEquals(11, ai[[0, 2, 1]]);

  CheckEquals(2,  ai[[1, 0, 0]]);
  CheckEquals(8,  ai[[1, 0, 1]]);
  CheckEquals(4,  ai[[1, 1, 0]]);
  CheckEquals(10, ai[[1, 1, 1]]);
  CheckEquals(6,  ai[[1, 2, 0]]);
  CheckEquals(12, ai[[1, 2, 1]]);
end;

procedure TArrManipTests.Transpose3D_201;
var a: INDArray<Integer>;
    ai: TNDAItems<Integer>;
begin
  a := iRng2NDA([2, 3, 2]);
  a := TNDAMan.Transpose<Integer>(a, [2, 0, 1]);

  CheckEquals([2, 2, 3], a.Shape);
  ai := a;
  CheckEquals(1,  ai[[0, 0, 0]]);
  CheckEquals(3,  ai[[0, 0, 1]]);
  CheckEquals(5,  ai[[0, 0, 2]]);
  CheckEquals(7,  ai[[0, 1, 0]]);
  CheckEquals(9,  ai[[0, 1, 1]]);
  CheckEquals(11, ai[[0, 1, 2]]);

  CheckEquals(2,  ai[[1, 0, 0]]);
  CheckEquals(4,  ai[[1, 0, 1]]);
  CheckEquals(6,  ai[[1, 0, 2]]);
  CheckEquals(8,  ai[[1, 1, 0]]);
  CheckEquals(10, ai[[1, 1, 1]]);
  CheckEquals(12, ai[[1, 1, 2]]);
end;

procedure TArrManipTests.Transpose3D_102;
var a: INDArray<Integer>;
    ai: TNDAItems<Integer>;
begin
  a := iRng2NDA([2, 3, 2]);
  a := TNDAMan.Transpose<Integer>(a, [1, 0, 2]);

  CheckEquals([3, 2, 2], a.Shape);
  ai := a;
  CheckEquals(1,  ai[[0, 0, 0]]);
  CheckEquals(2,  ai[[0, 0, 1]]);
  CheckEquals(7,  ai[[0, 1, 0]]);
  CheckEquals(8,  ai[[0, 1, 1]]);

  CheckEquals(3,  ai[[1, 0, 0]]);
  CheckEquals(4,  ai[[1, 0, 1]]);
  CheckEquals(9,  ai[[1, 1, 0]]);
  CheckEquals(10, ai[[1, 1, 1]]);

  CheckEquals(5,  ai[[2, 0, 0]]);
  CheckEquals(6,  ai[[2, 0, 1]]);
  CheckEquals(11, ai[[2, 1, 0]]);
  CheckEquals(12, ai[[2, 1, 1]]);
end;

procedure TArrManipTests.Transpose3D_120;
var a: INDArray<Integer>;
    ai: TNDAItems<Integer>;
begin
  a := iRng2NDA([2, 3, 2]);
  a := TNDAMan.Transpose<Integer>(a, [1, 2, 0]);

  CheckEquals([3, 2, 2], a.Shape);
  ai := a;
  CheckEquals(1,  ai[[0, 0, 0]]);
  CheckEquals(7,  ai[[0, 0, 1]]);
  CheckEquals(2,  ai[[0, 1, 0]]);
  CheckEquals(8,  ai[[0, 1, 1]]);

  CheckEquals(3,  ai[[1, 0, 0]]);
  CheckEquals(9,  ai[[1, 0, 1]]);
  CheckEquals(4,  ai[[1, 1, 0]]);
  CheckEquals(10, ai[[1, 1, 1]]);

  CheckEquals(5,  ai[[2, 0, 0]]);
  CheckEquals(11, ai[[2, 0, 1]]);
  CheckEquals(6,  ai[[2, 1, 0]]);
  CheckEquals(12, ai[[2, 1, 1]]);
end;

procedure TArrManipTests.Flip1D;
var a: INDArray<Integer>;
    v: TArray<Integer>;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3]);
  a := TNDAMan.Flip<Integer>(a);
  CheckTrue(TNDAUt.TryAsDynArray<Integer>(a, v));

  CheckEquals(3, Length(v));
  CheckEquals(3, v[0]);
  CheckEquals(2, v[1]);
  CheckEquals(1, v[2]);
end;

procedure TArrManipTests.Flip2D_A0;
var a: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
    I: Integer;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6], [7, 8, 9]]);
  a := TNDAMan.Flip<Integer>(a, 0);
  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, m));

  CheckEquals(3, Length(m));
  for I := 0 to 2 do
    CheckEquals(3, Length(m[I]));

  CheckEquals([7, 8, 9], m[0]);
  CheckEquals([4, 5, 6], m[1]);
  CheckEquals([1, 2, 3], m[2]);
end;

procedure TArrManipTests.Flip2D_A1;
var a: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
    I: Integer;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6], [7, 8, 9]]);
  a := TNDAMan.Flip<Integer>(a, 1);
  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, m));

  CheckEquals(3, Length(m));
  for I := 0 to 2 do
    CheckEquals(3, Length(m[I]));

  CheckEquals([3, 2, 1], m[0]);
  CheckEquals([6, 5, 4], m[1]);
  CheckEquals([9, 8, 7], m[2]);
end;

procedure TArrManipTests.Flip2D_A0A1;
var a: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
    I: Integer;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6], [7, 8, 9]]);
  a := TNDAMan.Flip<Integer>(a, [0, 1]);
  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, m));

  CheckEquals(3, Length(m));
  for I := 0 to 2 do
    CheckEquals(3, Length(m[I]));

  CheckEquals([9, 8, 7], m[0]);
  CheckEquals([6, 5, 4], m[1]);
  CheckEquals([3, 2, 1], m[2]);
end;

procedure TArrManipTests.Concat1D;
var a, b, c: INDArray<Integer>;
    v: TArray<Integer>;
    I: Integer;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3]);
  b := TNDAUt.AsArray<Integer>([4, 5, 6]);

  c := TNDAMan.Concat<Integer>([a, b]);

  CheckTrue(TNDAUt.TryAsDynArray<Integer>(c, v));
  CheckEquals(6, Length(v));
  for I := 0 to High(v) do
    CheckEquals(I + 1, v[I]);
end;

procedure TArrManipTests.Concat2D_A0;
var a, b, c: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3]]);
  b := TNDAUt.AsArray<Integer>([[4, 5, 6]]);

  c := TNDAMan.Concat<Integer>([a, b]);

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(c, m));
  CheckEquals(2, Length(m));
  CheckEquals([1, 2, 3], m[0]);
  CheckEquals([4, 5, 6], m[1]);
end;

procedure TArrManipTests.Concat2D_A1;
var a, b, c: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6]]);
  b := TNDAUt.AsArray<Integer>([[7], [8]]);

  c := TNDAMan.Concat<Integer>([a, b], 1);

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(c, m));
  CheckEquals(2, Length(m));
  CheckEquals([1, 2, 3, 7], m[0]);
  CheckEquals([4, 5, 6, 8], m[1]);
end;

procedure TArrManipTests.Concat2Dx3_A0;
var a, b, c: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6]]);
  b := TNDAUt.AsArray<Integer>([[7, 8, 9]]);
  c := TNDAUt.AsArray<Integer>([[10, 11, 12]]);

  c := TNDAMan.Concat<Integer>([a, b, c]);

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(c, m));
  CheckEquals(4, Length(m));
  CheckEquals([ 1,  2,  3], m[0]);
  CheckEquals([ 4,  5,  6], m[1]);
  CheckEquals([ 7,  8,  9], m[2]);
  CheckEquals([10, 11, 12], m[3]);
end;

procedure TArrManipTests.Concat2Dx3_A1;
var a, b, c: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6]]);
  b := TNDAUt.AsArray<Integer>([[7], [8]]);
  c := TNDAUt.AsArray<Integer>([[10, 11], [12, 13]]);

  c := TNDAMan.Concat<Integer>([a, b, c], 1);

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(c, m));
  CheckEquals(2, Length(m));
  CheckEquals([1, 2, 3, 7, 10, 11], m[0]);
  CheckEquals([4, 5, 6, 8, 12, 13], m[1]);
end;

procedure TArrManipTests.Concat_SingleArr;
var a, b: INDArray<Integer>;
    v: TArray<Integer>;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3]);

  b := TNDAMan.Concat<Integer>([a]);

  CheckTrue(TNDAUt.TryAsDynArray<Integer>(b, v));
  CheckEquals([1, 2, 3], v);
end;


procedure TArrManipTests.Append1D;
var a: INDArray<Integer>;
    v: TArray<Integer>;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3]);

  a := TNDAMan.Append<Integer>(a, 4);

  CheckTrue(TNDAUt.TryAsDynArray<Integer>(a, v));
  CheckEquals([1, 2, 3, 4], v);
end;

procedure TArrManipTests.Prepend1D;
var a: INDArray<Integer>;
    v: TArray<Integer>;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3]);

  a := TNDAMan.Prepend<Integer>(a, 4);

  CheckTrue(TNDAUt.TryAsDynArray<Integer>(a, v));
  CheckEquals([4, 1, 2, 3], v);
end;

procedure TArrManipTests.Insert1D;
var a, b: INDArray<Integer>;
    v: TArray<Integer>;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3]);
  b := TNDAMan.Insert<Integer>(a, 4, 0);

  CheckTrue(TNDAUt.TryAsDynArray<Integer>(b, v));
  CheckEquals([4, 1, 2, 3], v);

  b := TNDAMan.Insert<Integer>(a, 4, -1);

  CheckTrue(TNDAUt.TryAsDynArray<Integer>(b, v));
  CheckEquals([1, 2, 4, 3], v);

  b := TNDAMan.Insert<Integer>(a, 4, 1);

  CheckTrue(TNDAUt.TryAsDynArray<Integer>(b, v));
  CheckEquals([1, 4, 2, 3], v);
end;

procedure TArrManipTests.InsertList1D;
var a: INDArray<Integer>;
    v: TArray<Integer>;
begin
  a := TNDAUt.AsArray<Integer>([0, 1, 2, 3, 4]);

  a := TNDAMan.Insert<Integer>(a, [5, 6, 7], [-1, 3, 0]);

  CheckTrue(TNDAUt.TryAsDynArray<Integer>(a, v));
  CheckEquals([7, 0, 1, 2, 6, 3, 5, 4], v);
end;

procedure TArrManipTests.Insert2D_A0;
var a, b: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2], [3, 4], [5, 6]]);
  b := TNDAUt.AsArray<Integer>([7, 8]);

  a := TNDAMan.Insert<Integer>(a, b, 1);

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, m));
  CheckEquals(4, Length(m));
  CheckEquals([1, 2], m[0]);
  CheckEquals([7, 8], m[1]);
  CheckEquals([3, 4], m[2]);
  CheckEquals([5, 6], m[3]);
end;

procedure TArrManipTests.Insert2D_A1;
var a, b: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6]]);
  b := TNDAUt.AsArray<Integer>([7, 8]);

  a := TNDAMan.Insert<Integer>(a, b, 1, 1);

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, m));
  CheckEquals(2, Length(m));
  CheckEquals([1, 7, 2, 3], m[0]);
  CheckEquals([4, 8, 5, 6], m[1]);
end;

procedure TArrManipTests.InsertList2D_A0;
var a, b: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2], [3, 4], [5, 6]]);
  b := TNDAUt.AsArray<Integer>([7, 8]);

  a := TNDAMan.Insert<Integer>(a, [b, b, b], [0, 1, -1]);

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, m));
  CheckEquals(6, Length(m));
  CheckEquals([7, 8], m[0]);
  CheckEquals([1, 2], m[1]);
  CheckEquals([7, 8], m[2]);
  CheckEquals([3, 4], m[3]);
  CheckEquals([7, 8], m[4]);
  CheckEquals([5, 6], m[5]);
end;

procedure TArrManipTests.InsertList2D_A1;
var a, b: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6]]);
  b := TNDAUt.AsArray<Integer>([7, 8]);

  a := TNDAMan.Insert<Integer>(a, [b, b, b], [0, 1, -1], 1);

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, m));
  CheckEquals(2, Length(m));
  CheckEquals([7, 1, 7, 2, 7, 3], m[0]);
  CheckEquals([8, 4, 8, 5, 8, 6], m[1]);
end;

procedure TArrManipTests.Riffle1D;
var a: INDArray<Integer>;
    v: TArray<Integer>;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3]);

  a := TNDAMan.Riffle<Integer>(a, 4);

  CheckTrue(TNDAUt.TryAsDynArray<Integer>(a, v));
  CheckEquals([1, 4, 2, 4, 3, 4], v);
end;

procedure TArrManipTests.Riffle2D_A0;
var a, b: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2], [3, 4], [5, 6]]);
  b := TNDAut.AsArray<Integer>([[7, 8]]);

  a := TNDAMan.Riffle<Integer>(a, b);

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, m));
  CheckEquals(6, Length(m));
  CheckEquals([1, 2], m[0]);
  CheckEquals([7, 8], m[1]);
  CheckEquals([3, 4], m[2]);
  CheckEquals([7, 8], m[3]);
  CheckEquals([5, 6], m[4]);
  CheckEquals([7, 8], m[5]);
end;

procedure TArrManipTests.Riffle2D_A1;
var a, b: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6]]);
  b := TNDAUt.AsArray<Integer>([[7, 8]]);

  a := TNDAMan.Riffle<Integer>(a, b, 1);

  CheckTrue(TNDAut.TryAsDynArray2D<Integer>(a, m));
  CheckEquals(2, Length(m));
  CheckEquals([1, 7, 2, 7, 3, 7], m[0]);
  CheckEquals([4, 8, 5, 8, 6, 8], m[1]);
end;

procedure TArrManipTests.Pad1D;
var a, b: INDArray<Integer>;
    v: TArray<Integer>;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3]);

  b := TNDAMan.Pad<Integer>(a, 1);

  CheckTrue(TNDAUt.TryAsDynArray<Integer>(b, v));
  CheckEquals([0, 1, 2, 3, 0], v);
end;

procedure TArrManipTests.Pad2D;
var a, b: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2], [3, 4]]);

  b := TNDAMan.Pad<Integer>(a, 1);

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(b, m));
  CheckEquals(4, Length(m));
  CheckEquals([0, 0, 0, 0], m[0]);
  CheckEquals([0, 1, 2, 0], m[1]);
  CheckEquals([0, 3, 4, 0], m[2]);
  CheckEquals([0, 0, 0, 0], m[3]);
end;

procedure TArrManipTests.Pad3D;
var a, b: INDArray<Integer>;
    m: TArray<TArray<TArray<Integer>>>;
begin
  a := TNDAUt.AsArray<Integer>([[[1]]]);

  b := TNDAMan.Pad<Integer>(a, 1, 5);

  CheckTrue(TNDAUt.TryAsDynArray3D<Integer>(b, m));
  CheckEquals(3, Length(m));

  CheckEquals(3, Length(m[0]));
  CheckEquals([5, 5, 5], m[0, 0]);
  CheckEquals([5, 5, 5], m[0, 1]);
  CheckEquals([5, 5, 5], m[0, 2]);

  CheckEquals(3, Length(m[1]));
  CheckEquals([5, 5, 5], m[1, 0]);
  CheckEquals([5, 1, 5], m[1, 1]);
  CheckEquals([5, 5, 5], m[1, 2]);

  CheckEquals(3, Length(m[2]));
  CheckEquals([5, 5, 5], m[2, 0]);
  CheckEquals([5, 5, 5], m[2, 1]);
  CheckEquals([5, 5, 5], m[2, 2]);
end;


procedure TArrManipTests.Pad2D_DiffPadWidths;
var a, b: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2], [3, 4]]);

  b := TNDAMan.Pad<Integer>(a, [1, 2], 0);

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(b, m));
  CheckEquals(4, Length(m));
  CheckEquals([0, 0, 0, 0, 0, 0], m[0]);
  CheckEquals([0, 0, 1, 2, 0, 0], m[1]);
  CheckEquals([0, 0, 3, 4, 0, 0], m[2]);
  CheckEquals([0, 0, 0, 0, 0, 0], m[3]);
end;

{$endregion}

initialization

  RegisterTest(TArrManipLowLvlTests.Suite);
  RegisterTest(TArrManipTests.Suite);

end.
