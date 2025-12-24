unit panda.Tests.Arrays;

interface

{$ifdef FPC}
  {$mode delphiunicode}{$H+}
{$endif}

uses
{$ifndef FPC}
    TestFramework
{$else}
    fpcunit
  , testutils
  , testregistry
{$endif}
  , SysUtils
  , panda.Intfs
  , panda.Arrays
  , panda.Tests.NDATestCase
  ;

type
  TArrayTests = class(TNDATestCase)
  published
    procedure ArrayConstr1D;
    procedure ArrayConsr2D;

    procedure Reshape2D;

    procedure PermutedArray2D;

    procedure IndexStep1D;
    procedure IndexStep2D;
    procedure IndexStep3D;
    procedure GetItem2D;
    procedure GetItem3D;
    procedure GetItem2Dlvl0;

    procedure GetVecItem;
    procedure GetMatItem;

    procedure SetItem2D;
    procedure SetItem3D;
    procedure SetItem1Dlvl0;
    procedure SetItem2Dlvl0;

    procedure GetVecIdxSet;
    procedure GetMatIdxSetLvl0;
    procedure GetMatIdxSetlvl1;

    procedure GetVecSpanStep;
    procedure GetVecSpanStartOffset;
    procedure GetVecSpanCont;
    procedure GetMatRow;
    procedure GetMatCol;
    procedure GetMatRows;
    procedure GetMatSpanCorners;
    procedure GetMatSpanTopLeftMinor;

    procedure FillVecByValue;
    procedure FillVecRangeByValue;
    procedure FillMatRowsByValue;
    procedure FillMatColsByValue;
    procedure FillMatCornersByValue;
    procedure PartAssignment;

    procedure SetPart2DContLvl0;
    procedure SetPart2DContLvl1;

    procedure SetVecIdxSet;
    procedure SetMatIdxSetLvl0;
    procedure SetMatIdxSetLvl0_SameShapeVal;
    procedure SetMatIdxSetLvl1;

    procedure FillVecIdxSetByValue;

    procedure ReverseVec;
    procedure ReverseMat;

    procedure FlipMatH;
    procedure FlipMatHAssign;

    procedure NestedPart1D;

    procedure GetPackedDataPtr1D;
    procedure GetPackedDataPtr2D;
  end;

  TIndicesTests = class(TNDATestCase)
  published
    procedure NDSpanSizeInc;
    procedure NDSpanSizeDec;
    procedure IntIdxConstr;
    procedure IntIdxCast;

    procedure AllToSpan;

    procedure GetIdxPartShape1D;
    procedure GetSpanPartShape1D;
    procedure GetSetPartShape1D;
    procedure GetIdxPartShape2D;
    procedure GetSpanPartShape2D;
    procedure GetSpanPart;

    procedure IdxSplit;
  end;

  TNDAItTests = class(TNDATestCase)
  published
    procedure PackadArray1D;
    procedure PackedArray2D;
    procedure Array1DWithStrides;
    procedure Array2DWithStrides;
    procedure Array2DLvl0;
    procedure Array2DAPerm;
    procedure Array2DAPermDx;
    procedure Array2DAPermDxDy;

    procedure PackedArray2DStartLvl1;
  end;

  TNDASliceItTests = class(TNDATestCase)
  published
    procedure Array2DRows;
    procedure Array2DCols;
    procedure Array3D_SliceAxes01;
  end;

  TNDASliceItChainTests = class(TNDATestCase)
  published
    procedure SetItArray1D;
    procedure SetItArray2D;

    procedure ItChain3D_IdxSet;
    procedure ItChain3D_SetIdx;
    procedure ItChain3D_SpanSet;
    procedure ItChain3D_SetSpan;
    procedure ItChain3D_IdxSetIdx;

    procedure ItChain2D_SpanSet_Perm;
  end;

  TNDIdxItTests = class(TNDATestCase)
  published
    procedure Array1D;
    procedure Array1DRev;
    procedure Array2D;
    procedure Array2DRevA0;
    procedure Array2DRevA1;
    procedure Array2DAPerm;
  end;

  TNDAUtTests = class(TNDATestCase)
  published
    procedure FullArray2D;
    procedure MakeArray2D;
    procedure MakeArrayFrom2DDynArray;
    procedure TryAsScalarTest;

    procedure Identity2D_Int;

    procedure GetContDim2DLvl0;
    procedure GetContDim2DLvl1;
    procedure GetContDim2DLvl2;
    procedure GetContDim3DLvl0;
    procedure GetContDim3DLvl1;
    procedure GetContDim3DLvl2;
    procedure GetContDim3DLvl3;

    procedure BroadcastLvlArr2DArr1D;
    procedure BreadcastLvlArr2DArr2D;

    procedure Copy2DPacked;
    procedure Copy2DStridesLvl0;
    procedure Copy2DStridesLvl1;
    procedure Copy2DStrides;

    procedure FillVecByConst;
    procedure FillVecByVec;
  end;

  TBinMapTests = class(TNDATestCase)
  published
    procedure TestCompatibleQ;
    procedure TestCompatibleQ_NotComp;
    procedure TestCompatibleQ_4Arr;

    procedure TestContinguousQ_Vec;
    procedure TestContinguousQ_Mat;

    procedure Scalar_Scalar;
    procedure CCArr1D_Scalar;
    procedure Arr1D_Scalar;
    procedure CCLvl1Arr2D_Scalar;
    procedure Arr2D_Scalar;

    procedure CCArr2D_CCArr2D;
    procedure CCArr2D_CCLvl1Arr2D;
    procedure CCArr2D_Arr2D;
    procedure CCLvl1Arr2D_CCLvl1Arr2D;
    procedure Arr2D_Arr2D;
    procedure CCArr2D_CCArr1D;
    procedure Arr1D_ScalarAsArr;

    procedure Scalar_CCArr1D;
    procedure CCArr1D_CCArr2D;
    procedure CCArr1D_CCLvl1Arr2D;
    procedure CCArr1D_Arr2D;
    procedure Arr1D_Arr2D;

    procedure Arr2D_Arr1D;

    procedure Arr1D_Arr2DInv;
  end;

  TNDACvtTests = class(TNDATestCase)
  published
    procedure CvtF32F64;
  end;

implementation

{$region 'TTensorTests'}

procedure TArrayTests.ArrayConstr1D;
var t: INDArray<Integer>;
begin
  t := TNDAUt.AsArray<Integer>([1, 2, 3]);

  CheckEquals(1, t.NDim);
  CheckEquals(3, t.Shape[0]);
end;

procedure TArrayTests.ArrayConsr2D;
var t: INDArray<Integer>;
begin
  t := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6]]);

  CheckEquals(2, t.NDim);
  CheckEquals(2, t.Shape[0]);
  CheckEquals(3, t.Shape[1]);
end;

procedure TArrayTests.Reshape2D;
var t: TNDAItems<Integer>;
begin
  t := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6]]);
  t := t.Base.Reshape([3, 2]);

  CheckEquals(2, t.NDim);
  CheckEquals(3, t.Shape[0]);
  CheckEquals(2, t.Shape[1]);

  CheckEquals(1, t[[0, 0]]);
  CheckEquals(2, t[[0, 1]]);

  CheckEquals(3, t[[1, 0]]);
  CheckEquals(4, t[[1, 1]]);

  CheckEquals(5, t[[2, 0]]);
  CheckEquals(6, t[[2, 1]]);
end;

procedure TArrayTests.PermutedArray2D;
var a, b: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6]]);
  b := TNDArrayWrapper<Integer>.CreatePermuted(a, [1, 0]);

  CheckEquals(3, b.Shape[0]);
  CheckEquals(2, b.Shape[1]);

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(b, m));

  CheckEquals(1, m[0, 0]);
  CheckEquals(4, m[0, 1]);

  CheckEquals(2, m[1, 0]);
  CheckEquals(5, m[1, 1]);

  CheckEquals(3, m[2, 0]);
  CheckEquals(6, m[2, 1]);
end;

procedure TArrayTests.IndexStep1D;
var t: INDArray<Byte>;
begin
  t := TNDAUt.AsArray<Byte>([1, 2, 3]);

  CheckEquals(1, Length(t.Strides));
  CheckEquals(1, t.Strides[0]);
end;

procedure TArrayTests.IndexStep2D;
var t: INDArray<Byte>;
begin
  t := TNDAUt.AsArray<Byte>([1, 2, 3, 4, 5, 6], [2, 3]);

  CheckEquals(2, Length(t.Strides));
  CheckEquals(3, t.Strides[0]);
  CheckEquals(1, t.Strides[1]);
end;

procedure TArrayTests.IndexStep3D;
var t: INDArray<Byte>;
begin
  t := TNDAUt.AsArray<Byte>(
    [1, 2, 3, 4, 5, 6,
     7, 8, 9, 10, 11, 12],
    [2, 2, 3]
  );

  CheckEquals(3, Length(t.Strides));
  CheckEquals(6, t.Strides[0]);
  CheckEquals(3, t.Strides[1]);
  Checkequals(1, t.Strides[2]);
end;

procedure TArrayTests.GetItem2D;
var t: TNDAItems<Integer>;
begin
  t := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6]]);

  CheckEquals(1, t[[0, 0]]);
  CheckEquals(2, t[[0, 1]]);
  CheckEquals(3, t[[0, 2]]);

  CheckEquals(4, t[[1, 0]]);
  CheckEquals(5, t[[1, 1]]);
  CheckEquals(6, t[[1, 2]]);
end;

procedure TArrayTests.GetItem3D;
var t: TNDAItems<Integer>;
begin
  t := TNDAUt.AsArray<Integer>(
    [1, 2, 3, 4, 5, 6,
     7, 8, 9, 10, 11, 12],
    [2, 2, 3]
  );

  CheckEquals(1, t[[0, 0, 0]]);
  CheckEquals(2, t[[0, 0, 1]]);
  CheckEquals(3, t[[0, 0, 2]]);

  CheckEquals(4, t[[0, 1, 0]]);
  CheckEquals(5, t[[0, 1, 1]]);
  CheckEquals(6, t[[0, 1, 2]]);


  CheckEquals(7,  t[[1, 0, 0]]);
  CheckEquals(8,  t[[1, 0, 1]]);
  CheckEquals(9,  t[[1, 0, 2]]);

  CheckEquals(10, t[[1, 1, 0]]);
  CheckEquals(11, t[[1, 1, 1]]);
  CheckEquals(12, t[[1, 1, 2]]);
end;

procedure TArrayTests.GetItem2Dlvl0;
var a, ar: INDArray<Integer>;
    v: TNDAItems<Integer>;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6]]);
  ar := a[[NDI(0)]];

  CheckEquals(1, ar.NDim);
  CheckEquals(3, ar.Shape[0]);

  v := ar;
  CheckEquals(1, v[[0]]);
  CheckEquals(2, v[[1]]);
  CheckEquals(3, v[[2]]);
end;


procedure TArrayTests.GetVecItem;
var a: INDArray<Integer>;
    I: Integer;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3, 4, 5]);
  a := a[[NDI(1)]];

  CheckTrue(TNDAUt.TryAsScalar<Integer>(a, I));
  CheckEquals(2, I);
end;

procedure TArrayTests.GetMatItem;
var a: INDArray<Integer>;
    I: Integer;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3, 4, 5, 6], [2, 3]);
  a := a[[NDI(1), NDI(1)]];

  CheckTrue(TNDAUt.TryAsScalar<Integer>(a, I));
  CheckEquals(5, I);
end;

procedure TArrayTests.SetItem2D;
var t: TNDAItems<Integer>;
begin
  t := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6]]);
  t[[0, 0]] := 3;
  t[[1, 1]] := 8;

  CheckEquals(3, t[[0, 0]]);
  CheckEquals(2, t[[0, 1]]);
  CheckEquals(3, t[[0, 2]]);

  CheckEquals(4, t[[1, 0]]);
  CheckEquals(8, t[[1, 1]]);
  CheckEquals(6, t[[1, 2]]);
end;

procedure TArrayTests.SetItem3D;
var t: TNDAItems<Integer>;
begin
  t := TNDAUt.AsArray<Integer>(
    [1, 2, 3, 4, 5, 6,
     7, 8, 9, 10, 11, 12],
    [2, 2, 3]
  );
  t[[0, 0, 0]] := 9;
  t[[0, 1, 1]] := 8;
  t[[0, 0, 1]] := 7;

  t[[1, 0, 0]] := 5;
  t[[1, 1, 0]] := 4;
  t[[1, 0, 1]] := 3;

  CheckEquals(9, t[[0, 0, 0]]);
  CheckEquals(7, t[[0, 0, 1]]);
  CheckEquals(3, t[[0, 0, 2]]);

  CheckEquals(4, t[[0, 1, 0]]);
  CheckEquals(8, t[[0, 1, 1]]);
  CheckEquals(6, t[[0, 1, 2]]);


  CheckEquals(5,  t[[1, 0, 0]]);
  CheckEquals(3,  t[[1, 0, 1]]);
  CheckEquals(9,  t[[1, 0, 2]]);

  CheckEquals(4 , t[[1, 1, 0]]);
  CheckEquals(11, t[[1, 1, 1]]);
  CheckEquals(12, t[[1, 1, 2]]);
end;

procedure TArrayTests.SetItem1Dlvl0;
var a, b: INDArray<Integer>;
    v: TArray<Integer>;
begin
  a := TNDAUt.AsArray<Integer>([0, 1, 2]);
  b := TNDAUt.Scalar<Integer>(4);
  a[[NDI(1)]] := b;

  CheckTrue(TNDAUt.TryAsDynArray<Integer>(a, v));

  CheckEquals(0, v[0]);
  CheckEquals(4, v[1]);
  CheckEquals(2, v[2]);
end;

procedure TArrayTests.SetItem2Dlvl0;
var a, ar: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3, 4, 5, 6], [2, 3]);
  ar := TNDAUt.AsArray<Integer>([7, 8, 9]);
  a[[NDI(1)]] := ar;

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, m));

  CheckEquals(1, m[0, 0]);
  CheckEquals(2, m[0, 1]);
  CheckEquals(3, m[0, 2]);

  CheckEquals(7, m[1, 0]);
  CheckEquals(8, m[1, 1]);
  CheckEquals(9, m[1, 2]);
end;

procedure TArrayTests.GetVecIdxSet;
var a, b: INDArray<Integer>;
    barr: TArray<Integer>;
begin
  a := TNDAUt.AsArray<Integer>([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]);
  b := a[[NDI([0, 2, 2, 0])]];

  CheckTrue(Assigned(b));
  CheckEquals(1, Length(b.Shape));
  CheckEquals(4, b.Shape[0]);

  CheckTrue(TNDAUt.TryAsDynArray<Integer>(b, barr));
  CheckEquals(0, barr[0]);
  CheckEquals(2, barr[1]);
  CheckEquals(2, barr[2]);
  CheckEquals(0, barr[3]);
end;

procedure TArrayTests.GetMatIdxSetLvl0;
var a, b: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
begin
  a := iRng2NDA([3, 3]);
  b := a[[NDI([0, 2, 1])]];

  CheckTrue(Assigned(b));
  CheckEquals(2, Length(b.Shape));
  CheckEquals(3, b.Shape[0]);
  CheckEquals(3, b.Shape[1]);

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(b, m));
  CheckEquals([1, 2, 3], m[0]);
  CheckEquals([7, 8, 9], m[1]);
  CheckEquals([4, 5, 6], m[2]);
end;

procedure TArrayTests.GetMatIdxSetlvl1;
var a, b: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
begin
  a := iRng2NDA([3, 3]);
  b := a[[NDI([0, 2, 1]), NDI([1, 0, 2])]];

  CheckTrue(Assigned(b));
  CheckEquals(2, Length(b.Shape));
  CheckEquals(3, b.Shape[0]);
  CheckEquals(3, b.Shape[1]);

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(b, m));
  CheckEquals([2, 1, 3], m[0]);
  CheckEquals([8, 7, 9], m[1]);
  CheckEquals([5, 4, 6], m[2]);

  // This result is the same as Mathematica result, but Python returns
  // array([[2, 7, 6]]) for a[[0, 2, 1], [1, 0, 2]] because Python does
  // index pairing ([0, 2, 1], [1, 0, 2] -> (0, 1), (2, 0), (1, 2)).
  // In Python a[np.ix_([0, 2, 1], [1, 0, 2])] works like Mathematica.
end;

procedure TArrayTests.GetVecSpanStep;
var a, b: INDArray<Integer>;
    barr: TArray<Integer>;
begin
  a := TNDAUt.AsArray<Integer>([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]);
  b := a[[NDISpan(0, -1, 2)]];

  CheckTrue(Assigned(b));
  CheckEquals(1, Length(b.Shape));
  CheckEquals(5, b.Shape[0]);
  CheckFalse(CContiguousQ(b));

  CheckTrue(TNDAUt.TryAsDynArray<Integer>(b, barr));
  CheckEquals(0, barr[0]);
  CheckEquals(2, barr[1]);
  CheckEquals(4, barr[2]);
  CheckEquals(6, barr[3]);
  CheckEquals(8, barr[4]);
end;

procedure TArrayTests.GetVecSpanStartOffset;
var a, b: INDArray<Integer>;
    barr: TArray<Integer>;
begin
  a := TNDAUt.AsArray<Integer>([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]);
  b := a[[NDISpan(2, -1, 2)]];

  CheckTrue(Assigned(b));
  CheckEquals(1, Length(b.Shape));
  CheckEquals(4, b.Shape[0]);
  CheckFalse(CContiguousQ(b));

  CheckTrue(TNDAUt.TryAsDynArray<Integer>(b, barr));
  CheckEquals(2, barr[0]);
  CheckEquals(4, barr[1]);
  CheckEquals(6, barr[2]);
  CheckEquals(8, barr[3]);
end;

procedure TArrayTests.GetVecSpanCont;
var a, b: INDArray<Integer>;
    barr: TArray<Integer>;
begin
  a := TNDAUt.AsArray<Integer>([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]);
  b := a[[NDISpan(2, 4)]];

  CheckTrue(Assigned(b));
  CheckEquals(1, Length(b.Shape));
  CheckEquals(3, b.Shape[0]);
  CheckTrue(CContiguousQ(b));

  CheckTrue(TNDAUt.TryAsDynArray<Integer>(b, barr));
  CheckEquals(2, barr[0]);
  CheckEquals(3, barr[1]);
  CheckEquals(4, barr[2]);
end;

procedure TArrayTests.GetMatRow;
var a, b: INDArray<Integer>;
    barr: TArray<Integer>;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6], [7, 8, 9]]);
  b := a[[NDI(1), NDIAll]];

  CheckTrue(Assigned(b));
  CheckEquals(1, Length(b.Shape));
  CheckEquals(3, b.Shape[0]);
  CheckTrue(CContiguousQ(b));

  CheckTrue(TNDAUt.TryAsDynArray<Integer>(b, barr));
  CheckEquals(4, barr[0]);
  CheckEquals(5, barr[1]);
  CheckEquals(6, barr[2]);
end;

procedure TArrayTests.GetMatCol;
var a, b: INDArray<Integer>;
    barr: TArray<Integer>;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6], [7, 8, 9]]);
  b := a[[NDIAll, NDI(1)]];

  CheckTrue(Assigned(b));
  CheckEquals(1, Length(b.Shape));
  CheckEquals(3, b.Shape[0]);
  CheckFalse(CContiguousQ(b));

  CheckTrue(TNDAUt.TryAsDynArray<Integer>(b, barr));
  CheckEquals(2, barr[0]);
  CheckEquals(5, barr[1]);
  CheckEquals(8, barr[2]);
end;

procedure TArrayTests.GetMatRows;
var a, b: INDArray<Integer>;
    bm: TArray<TArray<Integer>>;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6], [7, 8, 9]]);
  b := a[[NDISpan(0, 1), NDIAll]];

  CheckTrue(Assigned(b));
  CheckEquals(2, Length(b.Shape));
  CheckEquals(2, b.Shape[0]);
  CheckEquals(3, b.Shape[1]);
  CheckTrue(CContiguousQ(b));

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(b, bm));
  CheckEquals(1, bm[0, 0]);
  CheckEquals(2, bm[0, 1]);
  CheckEquals(3, bm[0, 2]);
  CheckEquals(4, bm[1, 0]);
  CheckEquals(5, bm[1, 1]);
  CheckEquals(6, bm[1, 2]);
end;

procedure TArrayTests.GetMatSpanCorners;
var a, b: INDArray<Integer>;
    bm: TArray<TArray<Integer>>;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6], [7, 8, 9]]);
  b := a[[NDISpan(0, -1, 2), NDISpan(0, -1, 2)]];

  CheckTrue(Assigned(b));
  CheckEquals(2, Length(b.Shape));
  CheckEquals(2, b.Shape[0]);
  CheckEquals(2, b.Shape[1]);
  CheckFalse(CContiguousQ(b));

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(b, bm));
  CheckEquals(1, bm[0, 0]);
  CheckEquals(3, bm[0, 1]);
  CheckEquals(7, bm[1, 0]);
  CheckEquals(9, bm[1, 1]);
end;

procedure TArrayTests.GetMatSpanTopLeftMinor;
var a, b: INDArray<Integer>;
    bm: TArray<TArray<Integer>>;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6], [7, 8, 9]]);
  b := a[[NDISpan(0, 1), NDISpan(0, 1)]];

  CheckTrue(Assigned(b));
  CheckEquals(2, Length(b.Shape));
  CheckEquals(2, b.Shape[0]);
  CheckEquals(2, b.Shape[1]);
  CheckFalse(CContiguousQ(b));

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(b, bm));
  CheckEquals(1, bm[0, 0]);
  CheckEquals(2, bm[0, 1]);
  CheckEquals(4, bm[1, 0]);
  CheckEquals(5, bm[1, 1]);
end;

procedure TArrayTests.FillVecByValue;
var a: INDArray<Integer>;
    arr: TArray<Integer>;
    I: Integer;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3, 4, 5]);
  a[[NDIAll]] := TNDAUt.Scalar<Integer>(10);

  CheckTrue(TNDAUt.TryAsDynArray<Integer>(a, arr));
  CheckEquals(5, Length(arr));
  for I := 0 to High(arr) do
    CheckEquals(10, arr[I]);
end;

procedure TArrayTests.FillVecRangeByValue;
var a: INDArray<Integer>;
    arr: TArray<Integer>;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3, 4, 5]);
  a[[NDISpan(1, 3)]] := TNDAUt.Scalar<Integer>(0);

  CheckTrue(TNDAUt.TryAsDynArray<Integer>(a, arr));
  CheckEquals(5, Length(arr));
  CheckEquals(1, arr[0]);
  CheckEquals(0, arr[1]);
  CheckEquals(0, arr[2]);
  CheckEquals(0, arr[3]);
  CheckEquals(5, arr[4]);
end;

procedure TArrayTests.FillMatRowsByValue;
var a: INDArray<Integer>;
    am: TArray<TArray<Integer>>;
    I: Integer;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6], [7, 8, 9]]);
  a[[NDISpan(1), NDIAll]] := TNDAUt.Scalar<Integer>(0);

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, am));
  for I := 0 to 2 do
    CheckEquals(I + 1, am[0, I]);
  for I := 0 to 2 do begin
    CheckEquals(0, am[1, I]);
    CheckEquals(0, am[2, I]);
  end;
end;

procedure TArrayTests.FillMatColsByValue;
var a: INDArray<Integer>;
    am: TArray<TArray<Integer>>;
    I: Integer;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6], [7, 8, 9]]);
  a[[NDIAll, NDISpan(1)]] := TNDAUt.Scalar<Integer>(0);

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, am));
  for I := 0 to 2 do
    CheckEquals(3*I + 1, am[I, 0]);
  for I := 0 to 2 do begin
    CheckEquals(0, am[I, 1]);
    CheckEquals(0, am[I, 2]);
  end;
end;

procedure TArrayTests.FillMatCornersByValue;
var a: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
begin
  a := TNDAUt.Full<Integer>([3, 3], 0);
  a[[NDISpan(0, -1, 2), NDISpan(0, -1, 2)]] := TNDAUt.Scalar<Integer>(3);

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, m));

  CheckEquals(3, m[0, 0]);
  CheckEquals(0, m[0, 1]);
  CheckEquals(3, m[0, 2]);

  CheckEquals(0, m[1, 0]);
  CheckEquals(0, m[1, 1]);
  CheckEquals(0, m[1, 2]);

  CheckEquals(3, m[2, 0]);
  CheckEquals(0, m[2, 1]);
  CheckEquals(3, m[2, 2]);
end;

procedure TArrayTests.PartAssignment;
var a, b: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6]]);
  b := TNDAUt.AsArray<Integer>([[7, 8], [9, 10], [11, 12]]);

  a[[NDIAll, NDI(1)]] := b[[NDI(1), NDIAll]];

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, m));

  CheckEquals(2, Length(m));
  CheckEquals(3, Length(m[0]));
  CheckEquals(3, Length(m[1]));

  CheckEquals(1,  m[0, 0]);
  CheckEquals(9,  m[0, 1]);
  CheckEquals(3,  m[0, 2]);

  CheckEquals(4,  m[1, 0]);
  CheckEquals(10, m[1, 1]);
  CheckEquals(6,  m[1, 2]);
end;

procedure TArrayTests.SetPart2DContLvl0;
var a, b: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6]]);
  b := TNDAUt.AsArray<Integer>([[1, 1, 1], [2, 2, 2]]);
  a[[NDIAll, NDIAll]] := b[[NDIAll, NDIAll]];

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, m));

  CheckEquals(2, Length(m));
  CheckEquals(3, Length(m[0]));
  CheckEquals(3, Length(m[1]));

  CheckEquals(1,  m[0, 0]);
  CheckEquals(1,  m[0, 1]);
  CheckEquals(1,  m[0, 2]);

  CheckEquals(2,  m[1, 0]);
  CheckEquals(2,  m[1, 1]);
  CheckEquals(2,  m[1, 2]);
end;

procedure TArrayTests.SetPart2DContLvl1;
var a, b: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6], [7, 8, 9]]);
  b := TNDAUt.AsArray<Integer>([[1, 1, 1], [2, 2, 2]]);
  a[[NDIAll(2), NDIAll]] := b[[NDIAll, NDIAll]];

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, m));

  CheckEquals(3, Length(m));
  CheckEquals(3, Length(m[0]));
  CheckEquals(3, Length(m[1]));
  CheckEquals(3, Length(m[2]));

  CheckEquals(1,  m[0, 0]);
  CheckEquals(1,  m[0, 1]);
  CheckEquals(1,  m[0, 2]);

  CheckEquals(4,  m[1, 0]);
  CheckEquals(5,  m[1, 1]);
  CheckEquals(6,  m[1, 2]);

  CheckEquals(2,  m[2, 0]);
  CheckEquals(2,  m[2, 1]);
  CheckEquals(2,  m[2, 2]);
end;

procedure TArrayTests.SetVecIdxSet;
var a: INDArray<Integer>;
    arr: TArray<Integer>;
begin
  a := TNDAUt.AsArray<Integer>([0, 1, 2, 3, 4]);
  a[[NDI([1, 0, 2])]] := TNDAUt.AsArray<Integer>([3, 4, 5]);

  CheckTrue(TNDAUt.TryAsDynArray<Integer>(a, arr));
  CheckEquals(5, Length(arr));
  CheckEquals(4, arr[0]);
  CheckEquals(3, arr[1]);
  CheckEquals(5, arr[2]);
  CheckEquals(3, arr[3]);
  CheckEquals(4, arr[4]);
end;

procedure TArrayTests.SetMatIdxSetLvl0;
var a: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6], [7, 8, 9]]);
  a[[NDI([0, 2])]] := TNDAUt.AsArray<Integer>([6, 5, 4]);

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, m));
  CheckEquals(3, Length(m));
  CheckEquals([6, 5, 4], m[0]);
  CheckEquals([4, 5, 6], m[1]);
  CheckEquals([6, 5, 4], m[2]);
end;

procedure TArrayTests.SetMatIdxSetLvl0_SameShapeVal;
var a: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6], [7, 8, 9]]);
  a[[NDI([0, 2])]] := TNDAUt.AsArray<Integer>([[3, 2, 1], [9, 8, 7]]);

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, m));
  CheckEquals(3, Length(m));
  CheckEquals([3, 2, 1], m[0]);
  CheckEquals([4, 5, 6], m[1]);
  CheckEquals([9, 8, 7], m[2]);
end;

procedure TArrayTests.SetMatIdxSetLvl1;
var a: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6], [7, 8, 9]]);
  a[[NDIAll, NDI([0, 2])]] := TNDAUt.AsArray<Integer>([6, 5, 4]);

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, m));
  CheckEquals(3, Length(m));
  CheckEquals([6, 2, 6], m[0]);
  CheckEquals([5, 5, 5], m[1]);
  CheckEquals([4, 8, 4], m[2]);
end;

procedure TArrayTests.FillVecIdxSetByValue;
var a: INDArray<Integer>;
    arr: TArray<Integer>;
begin
  a := TNDAUt.AsArray<Integer>([0, 1, 2]);
  a[[NDI([0, 2])]] := TNDAUt.Scalar<Integer>(8);

  CheckTrue(TNDAUt.TryAsDynArray<Integer>(a, arr));
  CheckEquals(3, Length(arr));
  CheckEquals(8, arr[0]);
  CheckEquals(1, arr[1]);
  CheckEquals(8, arr[2]);
end;

procedure TArrayTests.ReverseVec;
var a, b: INDArray<Integer>;
    v: TArray<Integer>;
    I: Integer;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3, 4, 5, 6]);
  b := a[[NDIAll(-1)]];

  CheckTrue(TNDAUt.TryAsDynArray<Integer>(b, v));
  CheckEquals(6, Length(v));
  for I := 0 to High(v) do
    CheckEquals(6 - I, v[I]);
end;

procedure TArrayTests.ReverseMat;
var a, b: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
    I, J: Integer;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6], [7, 8, 9]]);
  b := a[[NDIAll(-1), NDIAll(-1)]];

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(b, m));
  CheckEquals(3, Length(m));
  for I := 0 to High(m) do begin
    CheckEquals(3, Length(m[I]));
    for J := 0 to High(m[I]) do
      CheckEquals(9 - 3 * I - J, m[I, J]);
  end;
end;

procedure TArrayTests.FlipMatH;
var a, b: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
    I, J: Integer;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6], [7, 8, 9]]);
  b := a[[NDIAll(-1), NDIAll]];

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(b, m));
  CheckEquals(3, Length(m));
  for I := 0 to High(m) do begin
    CheckEquals(3, Length(m[I]));
    for J := 0 to High(m[I]) do
      CheckEquals(7 - 3 * I + J, m[I, J]);
  end;
end;

procedure TArrayTests.FlipMatHAssign;
var a, b: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
    I, J: Integer;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6], [7, 8, 9]]);
  b := TNDAUt.Copy<Integer>(a);
  b[[NDIAll, NDIAll]] := a[[NDIAll(-1), NDIAll]];

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(b, m));
  CheckEquals(3, Length(m));
  for I := 0 to High(m) do begin
    CheckEquals(3, Length(m[I]));
    for J := 0 to High(m[I]) do
      CheckEquals(7 - 3 * I + J, m[I, J]);
  end;
end;

procedure TArrayTests.NestedPart1D;
var a, b: INDArray<Integer>;
    I: Integer;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3, 4, 5]);
  b := a[[NDISpan(2)]][[NDIFirst]];

  CheckTrue(TNDAUt.TryAsScalar<Integer>(b, I));
  CheckEquals(3, I);
end;

procedure TArrayTests.GetPackedDataPtr1D;
var a: INDArray<Byte>;
    buff: TBytes;
    pBuff: PByte;
begin
  a := TNDAUt.AsArray<Byte>([1, 2, 3]);
  pBuff := GetPackedDataPtr(a, buff);
  CheckEquals(Pointer(a.Data), Pointer(pBuff));

  pBuff := GetPackedDataPtr(a[[NDIAll(2)]], buff);
  CheckEquals(2, Length(buff));
  CheckEquals(1, buff[0]);
  CheckEquals(3, buff[1]);
  CheckEquals(Pointer(buff), Pointer(pBuff));
end;

procedure TArrayTests.GetPackedDataPtr2D;
var a: INDArray<Byte>;
    buff: TBytes;
    pBuff: PByte;
begin
  a := TNDAUt.AsArray<Byte>([[1, 2, 3], [4, 5, 6]]);
  pBuff := GetPackedDataPtr(a, buff);
  CheckEquals(Pointer(a.Data), Pointer(pBuff));

  pBuff := GetPackedDataPtr(a[[NDIAll, NDIAll(2)]], buff);
  CheckEquals(4, Length(buff));
  CheckEquals(1, buff[0]);
  CheckEquals(3, buff[1]);
  CheckEquals(4, buff[2]);
  CheckEquals(6, buff[3]);
  CheckEquals(Pointer(buff), Pointer(pBuff));
end;

{$endregion}

{$region 'TIndicesTests'}

procedure TIndicesTests.NDSpanSizeInc;
begin
  CheckEquals(10, NDISpanSize(10, NDISpan()));
  CheckEquals(0,  NDISpanSize(10, NDISpan(0, -1, -1)));
  CheckEquals(5,  NDISpanSize(10, NDISpan(0, -1, 2)));
  CheckEquals(4,  NDISpanSize(10, NDISpan(0, -1, 3)));
end;

procedure TIndicesTests.NDSpanSizeDec;
begin
  CheckEquals(10, NDISpanSize(10, NDISpan(-1, 0, -1)));
  CheckEquals(0,  NDISpanSize(10, NDISpan(-1, 0, 1)));
  CheckEquals(5,  NDISpanSize(10, NDISpan(-1, 0, -2)));
  CheckEquals(4,  NDISpanSize(10, NDISpan(-1, 0, -3)));
end;

procedure TIndicesTests.IntIdxConstr;
var idx: INDIntIndex;
begin
  idx := TNDIntIdx.Create(10);
  CheckTrue(nditInt = idx.IndexType);
  CheckEquals(10, idx.Value);
end;

procedure TIndicesTests.IntIdxCast;
var idx: INDIndex;
    iidx: INDIntIndex;
begin
  idx := TNDIntIdx.Create(10);
  CheckTrue(nditInt = idx.IndexType);
  iidx := (idx as INDIntIndex);
  CheckEquals(10, iidx.Value);
end;

procedure TIndicesTests.AllToSpan;
var a: INDArray<Integer>;
    va: TNDAVecItems<Integer>;
    I: Integer;
begin
  a := TNDAUt.AsArray<Integer>([0, 1, 2, 3, 4]);
  va := a[[NDIAllTo(2)]];

  CheckEquals(3, va.Length);
  for I := 0 to va.Length - 1 do
    CheckEquals(I, va[I]);

  va := a[[NDIAllTo(2, -1)]];

  CheckEquals(3, va.Length);
  for I := 0 to va.Length - 1 do
    CheckEquals(4 - I, va[I]);
end;

procedure TIndicesTests.GetIdxPartShape1D;
var a: INDArray<Integer>;
    s: TNDAShape;
begin
  a := TNDAUt.AsArray<Integer>([0, 1, 2, 3, 4]);

  s := GetPartShape(a, [NDI(1)]);
  CheckEquals(0, Length(s));
end;

procedure TIndicesTests.GetSpanPartShape1D;
var a: INDArray<Integer>;
    s: TNDAShape;
begin
  a := TNDAUt.AsArray<Integer>([0, 1, 2, 3, 4]);

  s := GetPartShape(a, [NDIAll(2)]);
  CheckEquals(1, Length(s));
  CheckEquals(3, s[0]);
end;

procedure TIndicesTests.GetSetPartShape1D;
var a: INDArray<Integer>;
    s: TNDAShape;
begin
  a := TNDAUt.AsArray<Integer>([0, 1, 2, 3, 4]);

  s := GetPartShape(a, [NDI([1, 0, 0, 2])]);
  CheckEquals(1, Length(s));
  CheckEquals(4, s[0]);
end;

procedure TIndicesTests.GetIdxPartShape2D;
var a: INDArray<Integer>;
    s: TNDAShape;
begin
  a := iRng2NDA([2, 3]);

  s := GetPartShape(a, [NDI(1), NDI(1)]);
  CheckEquals(0, Length(s));
end;

procedure TIndicesTests.GetSpanPartShape2D;
var a: INDArray<Integer>;
    s: TNDAShape;
begin
  a := iRng2NDA([3, 3]);

  s := GetPartShape(a, [NDIAll(2), NDIAll(2)]);
  CheckEquals(2, Length(s));
  CheckEquals(2, s[0]);
  CheckEquals(2, s[1]);
end;

procedure TIndicesTests.GetSpanPart;
var a: INDArray<Integer>;
    s: TNDAShape;
begin
  a := iRng2NDA([2, 3, 2]);

  s := GetPartShape(a, [NDI([0, 0, 1]), NDIAll(2), NDI(1)]);
  CheckEquals(2, Length(s));
  CheckEquals(3, s[0]);
  CheckEquals(2, s[1]);
end;

procedure TIndicesTests.IdxSplit;
var setIdx, spanIdx: INDIndexSeq;
    axes: TArray<Integer>;
begin
  NDISplit([NDI(1), NDISet([2, 0]), NDI(0)], setIdx, spanIdx, axes);

  CheckEquals(1, Length(setIdx));
  CheckTrue(setIdx[0].IndexType = nditSet);

  CheckEquals(2, Length(spanIdx));
  CheckTrue(spanIdx[0].IndexType = nditInt);
  CheckTrue(spanIdx[1].IndexType = nditInt);

  CheckEquals(3, Length(axes));
  CheckEquals(1, axes[0]);
  CheckEquals(0, axes[1]);
  CheckEquals(2, axes[2]);
end;

{$endregion}

{$region 'TNDAItTests'}

procedure TNDAItTests.PackadArray1D;
var a: INDArray<Integer>;
    it: TNDAIt;
    I: Integer;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3, 4, 5]);

  it := TNDAIt.Create(a);
  try
    I := 1;
    while it.MoveNext do begin
      CheckEquals(I, PInteger(it.Current)^);
      Inc(I);
    end;
    CheckEquals(6, I);
  finally
    it.Free;
  end;
end;

procedure TNDAItTests.PackedArray2D;
var a: INDArray<Integer>;
    it: TNDAIt;
    I: Integer;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6]]);

  it := TNDAIt.Create(a);
  try
    I := 1;
    while it.MoveNext do begin
      CheckEquals(I, PInteger(it.Current)^);
      Inc(I);
    end;
    CheckEquals(7, I);
  finally
    it.Free;
  end;
end;

procedure TNDAItTests.Array1DWithStrides;
var a: INDArray<Integer>;
    it: TNDAIt;
    I: Integer;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3, 4, 5]);
  a := a[[NDISpan(0, -1, 2)]];

  it := TNDAIt.Create(a);
  try
    I := 1;
    while it.MoveNext do begin
      CheckEquals(I, PInteger(it.Current)^);
      Inc(I, 2);
    end;
    CheckEquals(7, I);
  finally
    it.Free;
  end;
end;

procedure TNDAItTests.Array2DWithStrides;
var a: INDArray<Integer>;
    it: TNDAIt;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6], [7, 8, 9]]);
  a := a[[NDISpan(0, -1, 2), NDISpan(0, -1, 2)]];

  it := TNDAIt.Create(a);
  try
    CheckTrue(it.MoveNext);
    CheckEquals(1, PInteger(it.Current)^);
    CheckTrue(it.MoveNext);
    CheckEquals(3, PInteger(it.Current)^);
    CheckTrue(it.MoveNext);
    CheckEquals(7, PInteger(it.Current)^);
    CheckTrue(it.MoveNext);
    CheckEquals(9, PInteger(it.Current)^);
    CheckFalse(it.MoveNext);
  finally
    it.Free;
  end;
end;

procedure TNDAItTests.Array2DLvl0;
var a: INDArray<Integer>;
    it: TNDAIt;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6], [7, 8, 9]]);
  a := a[[NDISpan(0, -1, 2), NDISpan(0, -1, 2)]];

  it := TNDAIt.Create(a, 0);
  try
    CheckTrue(it.MoveNext);
    CheckEquals(1, PInteger(it.Current)^);
    CheckTrue(it.MoveNext);
    CheckEquals(7, PInteger(it.Current)^);
    CheckFalse(it.MoveNext);
  finally
    it.Free;
  end;
end;

procedure TNDAItTests.Array2DAPerm;
var a: INDArray<Integer>;
    it: TNDAIt;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6]]);

  it := TNDAIt.Create(a, TArray<Integer>.Create(1, 0));
  try
    CheckTrue(it.MoveNext);
    CheckEquals(1, PInteger(it.Current)^);
    CheckTrue(it.MoveNext);
    CheckEquals(4, PInteger(it.Current)^);

    CheckTrue(it.MoveNext);
    CheckEquals(2, PInteger(it.Current)^);
    CheckTrue(it.MoveNext);
    CheckEquals(5, PInteger(it.Current)^);

    CheckTrue(it.MoveNext);
    CheckEquals(3, PInteger(it.Current)^);
    CheckTrue(it.MoveNext);
    CheckEquals(6, PInteger(it.Current)^);

    CheckFalse(it.MoveNext);
  finally
    it.Free;
  end;
end;

procedure TNDAItTests.Array2DAPermDx;
var a: INDArray<Integer>;
    it: TNDAIt;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6]]);
  a := a[[NDIAll, NDIAll(2)]];

  it := TNDAIt.Create(a, TArray<Integer>.Create(1, 0));
  try
    CheckTrue(it.MoveNext);
    CheckEquals(1, PInteger(it.Current)^);
    CheckTrue(it.MoveNext);
    CheckEquals(4, PInteger(it.Current)^);

    CheckTrue(it.MoveNext);
    CheckEquals(3, PInteger(it.Current)^);
    CheckTrue(it.MoveNext);
    CheckEquals(6, PInteger(it.Current)^);

    CheckFalse(it.MoveNext);
  finally
    it.Free;
  end;
end;

procedure TNDAItTests.Array2DAPermDxDy;
var a: INDArray<Integer>;
    it: TNDAIt;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6], [7, 8, 9]]);
  a := a[[NDIAll(2), NDIAll(2)]];

  it := TNDAIt.Create(a, TArray<Integer>.Create(1, 0));
  try
    CheckTrue(it.MoveNext);
    CheckEquals(1, PInteger(it.Current)^);
    CheckTrue(it.MoveNext);
    CheckEquals(7, PInteger(it.Current)^);

    CheckTrue(it.MoveNext);
    CheckEquals(3, PInteger(it.Current)^);
    CheckTrue(it.MoveNext);
    CheckEquals(9, PInteger(it.Current)^);

    CheckFalse(it.MoveNext);
  finally
    it.Free;
  end;
end;

procedure TNDAItTests.PackedArray2DStartLvl1;
var a: INDArray<Integer>;
    it: TNDAIt;
    I: Integer;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6]]);

  it := TNDAIt.Create(a, 1, -1);
  try
    I := 1;
    while it.MoveNext do begin
      CheckEquals(I, PInteger(it.Current)^);
      Inc(I);
    end;
    CheckEquals(4, I);
  finally
    it.Free;
  end;
end;

{$endregion}

{$region 'TNDASliceItTests'}

procedure TNDASliceItTests.Array2DRows;
var a: INDArray<Integer>;
    v: TArray<Integer>;
    it: TNDASliceIt;
begin
  a := iRng2NDA([2, 3]);
  it := TNDASliceIt.Create(a, 0);
  try
    CheckTrue(it.MoveNext);
    CheckTrue(TNDAUt.TryAsDynArray<Integer>(it.CurrentSlice, v));
    CheckEquals(3, Length(v));
    CheckEquals(1, v[0]);
    CheckEquals(2, v[1]);
    CheckEquals(3, v[2]);

    CheckTrue(it.MoveNext);
    CheckTrue(TNDAUt.TryAsDynArray<Integer>(it.CurrentSlice, v));
    CheckEquals(3, Length(v));
    CheckEquals(4, v[0]);
    CheckEquals(5, v[1]);
    CheckEquals(6, v[2]);

    CheckFalse(it.MoveNext);
  finally
    it.Free;
  end;
end;

procedure TNDASliceItTests.Array2DCols;
var a: INDArray<Integer>;
    v: TArray<Integer>;
    it: TNDASliceIt;
begin
  a := iRng2NDA([2, 3]);
  it := TNDASliceIt.Create(a, 1, -1);
  try
    CheckTrue(it.MoveNext);
    CheckTrue(TNDAUt.TryAsDynArray<Integer>(it.CurrentSlice, v));
    CheckEquals(2, Length(v));
    CheckEquals(1, v[0]);
    CheckEquals(4, v[1]);

    CheckTrue(it.MoveNext);
    CheckTrue(TNDAUt.TryAsDynArray<Integer>(it.CurrentSlice, v));
    CheckEquals(2, Length(v));
    CheckEquals(2, v[0]);
    CheckEquals(5, v[1]);

    CheckTrue(it.MoveNext);
    CheckTrue(TNDAUt.TryAsDynArray<Integer>(it.CurrentSlice, v));
    CheckEquals(2, Length(v));
    CheckEquals(3, v[0]);
    CheckEquals(6, v[1]);

    CheckFalse(it.MoveNext);
  finally
    it.Free;
  end;
end;

procedure TNDASliceItTests.Array3D_SliceAxes01;
var a: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
    it: TNDASliceIt;
begin
  a := iRng2NDA([2, 3, 2]);
  it := TNDASliceIt.Create(a, 2, -1);
  try
    CheckTrue(it.MoveNext);
    CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(it.CurrentSlice, m));
    CheckEquals([1, 3, 5],  m[0]);
    CheckEquals([7, 9, 11], m[1]);

    CheckTrue(it.MoveNext);
    CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(it.CurrentSlice, m));
    CheckEquals([2, 4, 6],   m[0]);
    CheckEquals([8, 10, 12], m[1]);

    CheckFalse(it.MoveNext);
  finally
    it.Free;
  end;
end;

{$endregion}

{$region 'TNDASliceSetItTests'}

procedure TNDASliceItChainTests.SetItArray1D;
var a: INDArray<Integer>;
    it: TNDASliceSetIt;
    I: Integer;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3, 4, 5]);
  it := TNDASliceSetIt.Create(a, [1, 3]);
  try
    CheckTrue(it.MoveNext);
    CheckTrue(TNDAUt.TryAsScalar<Integer>(it.CurrentSlice, I));
    CheckEquals(2, I);

    CheckTrue(it.MoveNext);
    CheckTrue(TNDAUt.TryAsScalar<Integer>(it.CurrentSlice, I));
    CheckEquals(4, I);

    CheckFalse(it.MoveNext);
  finally
    it.Free;
  end;
end;

procedure TNDASliceItChainTests.SetItArray2D;
var a: INDArray<Integer>;
    it: TNDASliceSetIt;
    v: TArray<Integer>;
begin
  a := iRng2NDA([3, 2]);
  it := TNDASliceSetIt.Create(a, [0, 2, 0]);
  try
    CheckTrue(it.MoveNext);
    CheckTrue(TNDAUt.TryAsDynArray<Integer>(it.CurrentSlice, v));
    CheckEquals(2, Length(v));
    CheckEquals(1, v[0]);
    CheckEquals(2, v[1]);

    CheckTrue(it.MoveNext);
    CheckTrue(TNDAUt.TryAsDynArray<Integer>(it.CurrentSlice, v));
    CheckEquals(2, Length(v));
    CheckEquals(5, v[0]);
    CheckEquals(6, v[1]);

    CheckTrue(it.MoveNext);
    CheckTrue(TNDAUt.TryAsDynArray<Integer>(it.CurrentSlice, v));
    CheckEquals(2, Length(v));
    CheckEquals(1, v[0]);
    CheckEquals(2, v[1]);

    CheckFalse(it.MoveNext);
  finally
    it.Free;
  end;
end;

procedure TNDASliceItChainTests.ItChain3D_IdxSet;
var a: INDArray<Integer>;
    it: TNDASliceItChain;
    v: TArray<Integer>;
    s: TNDAShape;
begin
  a := iRng2NDA([3, 2, 3]);

  it := TNDASliceItChain.Create(a, [NDI(0), NDI([0, 1, 1])]);
  try
    s := it.CurrentSlice.Shape;
    CheckEquals(1, Length(s));
    CheckEquals(3, s[0]);

    CheckTrue(it.MoveNext);
    CheckTrue(TNDAUt.TryAsDynArray<Integer>(it.CurrentSlice, v));
    CheckEquals([1, 2, 3], v);

    CheckTrue(it.MoveNext);
    CheckTrue(TNDAUt.TryAsDynArray<Integer>(it.CurrentSlice, v));
    CheckEquals([4, 5, 6], v);

    CheckTrue(it.MoveNext);
    CheckTrue(TNDAUt.TryAsDynArray<Integer>(it.CurrentSlice, v));
    CheckEquals([4, 5, 6], v);

    CheckFalse(it.MoveNext);
  finally
    it.Free;
  end;
end;

procedure TNDASliceItChainTests.ItChain3D_SetIdx;
var a: INDArray<Integer>;
    it: TNDASliceItChain;
    v: TArray<Integer>;
    s: TNDAShape;
begin
  a := iRng2NDA([3, 2, 3]);

  it := TNDASliceItChain.Create(a, [NDI([0, 1, 1]), NDI(0)]);
  try
    s := it.CurrentSlice.Shape;
    CheckEquals(1, Length(s));
    CheckEquals(3, s[0]);

    CheckTrue(it.MoveNext);
    CheckTrue(TNDAUt.TryAsDynArray<Integer>(it.CurrentSlice, v));
    CheckEquals([1, 2, 3], v);

    CheckTrue(it.MoveNext);
    CheckTrue(TNDAUt.TryAsDynArray<Integer>(it.CurrentSlice, v));
    CheckEquals([7, 8, 9], v);

    CheckTrue(it.MoveNext);
    CheckTrue(TNDAUt.TryAsDynArray<Integer>(it.CurrentSlice, v));
    CheckEquals([7, 8, 9], v);

    CheckFalse(it.MoveNext);
  finally
    it.Free;
  end;
end;

procedure TNDASliceItChainTests.ItChain3D_SpanSet;
var a: INDArray<Integer>;
    it: TNDASliceItChain;
    v: TArray<Integer>;
    s: TNDAShape;
begin
  a := iRng2NDA([3, 2, 3]);

  it := TNDASliceItChain.Create(a, [NDIAll(2), NDI([0, 1, 1])]);
  try
    s := it.CurrentSlice.Shape;
    CheckEquals(1, Length(s));
    CheckEquals(3, s[0]);

    CheckTrue(it.MoveNext);
    CheckTrue(TNDAUt.TryAsDynArray<Integer>(it.CurrentSlice, v));
    CheckEquals([1, 2, 3], v);

    CheckTrue(it.MoveNext);
    CheckTrue(TNDAUt.TryAsDynArray<Integer>(it.CurrentSlice, v));
    CheckEquals([4, 5, 6], v);

    CheckTrue(it.MoveNext);
    CheckTrue(TNDAUt.TryAsDynArray<Integer>(it.CurrentSlice, v));
    CheckEquals([4, 5, 6], v);


    CheckTrue(it.MoveNext);
    CheckTrue(TNDAUt.TryAsDynArray<Integer>(it.CurrentSlice, v));
    CheckEquals([13, 14, 15], v);

    CheckTrue(it.MoveNext);
    CheckTrue(TNDAUt.TryAsDynArray<Integer>(it.CurrentSlice, v));
    CheckEquals([16, 17, 18], v);

    CheckTrue(it.MoveNext);
    CheckTrue(TNDAUt.TryAsDynArray<Integer>(it.CurrentSlice, v));
    CheckEquals([16, 17, 18], v);

    CheckFalse(it.MoveNext);
  finally
    it.Free;
  end;
end;

procedure TNDASliceItChainTests.ItChain3D_SetSpan;
var a: INDArray<Integer>;
    it: TNDASliceItChain;
    v: TArray<Integer>;
    s: TNDAShape;
begin
  a := iRng2NDA([3, 2, 3]);

  it := TNDASliceItChain.Create(a, [NDI([0, 1, 1]), NDIAll(2)]);
  try
    s := it.CurrentSlice.Shape;
    CheckEquals(1, Length(s));
    CheckEquals(3, s[0]);

    CheckTrue(it.MoveNext);
    CheckTrue(TNDAUt.TryAsDynArray<Integer>(it.CurrentSlice, v));
    CheckEquals([1, 2, 3], v);

    CheckTrue(it.MoveNext);
    CheckTrue(TNDAUt.TryAsDynArray<Integer>(it.CurrentSlice, v));
    CheckEquals([7, 8, 9], v);

    CheckTrue(it.MoveNext);
    CheckTrue(TNDAUt.TryAsDynArray<Integer>(it.CurrentSlice, v));
    CheckEquals([7, 8, 9], v);
  finally
    it.Free;
  end;
end;

procedure TNDASliceItChainTests.ItChain3D_IdxSetIdx;
var a: INDArray<Integer>;
    it: TNDASliceItChain;
    v: Integer;
    s: TNDAShape;
begin
  a := iRng2NDA([3, 2, 3]);

  it := TNDASliceItChain.Create(a, [NDI(0), NDI([1, 0]), NDI(1)]);
  try
    s := it.CurrentSlice.Shape;
    CheckEquals(0, Length(s));

    CheckTrue(it.MoveNext);
    CheckTrue(TNDAUt.TryAsScalar<Integer>(it.CurrentSlice, v));
    CheckEquals(5, v);

    CheckTrue(it.MoveNext);
    CheckTrue(TNDAUt.TryAsScalar<Integer>(it.CurrentSlice, v));
    CheckEquals(2, v);

    CheckFalse(it.MoveNext);
  finally
    it.Free;
  end;
end;

procedure TNDASliceItChainTests.ItChain2D_SpanSet_Perm;
var a, b: INDArray<Integer>;
    it: TNDASliceItChain;
    v: TArray<Integer>;
    s: TNDAShape;
begin
  a := iRng2NDA([3, 3]);
  // original problem:
  // a = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
  // a[::2, [0, 1]] -> [[1, 2], [4, 5], [7, 8]]

  b := TNDArrayWrapper<Integer>.CreatePermuted(a, [1, 0]);

  it := TNDASliceItChain.Create(b, [NDI([0, 1])]);
  try
    s := it.CurrentSlice.Shape;
    CheckEquals(1, Length(s));
    CheckEquals(3, s[0]);

    CheckTrue(it.MoveNext);
    CheckTrue(TNDAUt.TryAsDynArray<Integer>(it.CurrentSlice, v));
    CheckEquals([1, 4, 7], v);

    CheckTrue(it.MoveNext);
    CheckTrue(TNDAUt.TryAsDynArray<Integer>(it.CurrentSlice, v));
    CheckEquals([2, 5, 8], v);

    CheckFalse(it.MoveNext);
  finally
    it.Free;
  end;
end;

{$endregion}

{$region 'TNDIdxItTests'}

procedure TNDIdxItTests.Array1D;
var shape, idx: TArray<NativeInt>;
    revAxis: TArray<Boolean>;
    it: TNDIdxIt;
    I: Integer;
begin
  shape := TArray<NativeInt>.Create(4);
  revAxis := TArray<Boolean>.Create(False);

  it := TNDIdxIt.Create(shape, revAxis);
  try
    idx := it.CurrentIndex;
    CheckEquals(1, Length(idx));
    I := 0;
    while it.MoveNext do begin
      CheckEquals(I, idx[0]);
      Inc(I);
    end;
    CheckEquals(4, I);
  finally
    it.Free;
  end;
end;

procedure TNDIdxItTests.Array1DRev;
var shape, idx: TArray<NativeInt>;
    revAxis: TArray<Boolean>;
    it: TNDIdxIt;
    I: Integer;
begin
  shape := TArray<NativeInt>.Create(4);
  revAxis := TArray<Boolean>.Create(True);

  it := TNDIdxIt.Create(shape, revAxis);
  try
    idx := it.CurrentIndex;
    CheckEquals(1, Length(idx));
    I := 3;
    while it.MoveNext do begin
      CheckEquals(I, idx[0]);
      Dec(I);
    end;
    CheckEquals(-1, I);
  finally
    it.Free;
  end;
end;

procedure TNDIdxItTests.Array2D;
var shape, idx: TArray<NativeInt>;
    revAxis: TArray<Boolean>;
    it: TNDIdxIt;
begin
  shape := TArray<NativeInt>.Create(2, 3);
  revAxis := TArray<Boolean>.Create(False, False);

  it := TNDIdxIt.Create(shape, revAxis);
  try
    idx := it.CurrentIndex;
    CheckEquals(2, Length(idx));

    CheckTrue(it.MoveNext);
    CheckEquals([0, 0], idx);
    CheckTrue(it.MoveNext);
    CheckEquals([0, 1], idx);
    CheckTrue(it.MoveNext);
    CheckEquals([0, 2], idx);

    CheckTrue(it.MoveNext);
    CheckEquals([1, 0], idx);
    CheckTrue(it.MoveNext);
    CheckEquals([1, 1], idx);
    CheckTrue(it.MoveNext);
    CheckEquals([1, 2], idx);

    CheckFalse(it.MoveNext);
  finally
    it.Free;
  end;
end;

procedure TNDIdxItTests.Array2DRevA0;
var shape, idx: TArray<NativeInt>;
    revAxis: TArray<Boolean>;
    it: TNDIdxIt;
begin
  shape := TArray<NativeInt>.Create(2, 3);
  revAxis := TArray<Boolean>.Create(True, False);

  it := TNDIdxIt.Create(shape, revAxis);
  try
    idx := it.CurrentIndex;
    CheckEquals(2, Length(idx));

    CheckTrue(it.MoveNext);
    CheckEquals([1, 0], idx);
    CheckTrue(it.MoveNext);
    CheckEquals([1, 1], idx);
    CheckTrue(it.MoveNext);
    CheckEquals([1, 2], idx);

    CheckTrue(it.MoveNext);
    CheckEquals([0, 0], idx);
    CheckTrue(it.MoveNext);
    CheckEquals([0, 1], idx);
    CheckTrue(it.MoveNext);
    CheckEquals([0, 2], idx);

    CheckFalse(it.MoveNext);
  finally
    it.Free;
  end;
end;

procedure TNDIdxItTests.Array2DRevA1;
var shape, idx: TArray<NativeInt>;
    revAxis: TArray<Boolean>;
    it: TNDIdxIt;
begin
  shape := TArray<NativeInt>.Create(2, 3);
  revAxis := TArray<Boolean>.Create(False, True);

  it := TNDIdxIt.Create(shape, revAxis);
  try
    idx := it.CurrentIndex;
    CheckEquals(2, Length(idx));

    CheckTrue(it.MoveNext);
    CheckEquals([0, 2], idx);
    CheckTrue(it.MoveNext);
    CheckEquals([0, 1], idx);
    CheckTrue(it.MoveNext);
    CheckEquals([0, 0], idx);

    CheckTrue(it.MoveNext);
    CheckEquals([1, 2], idx);
    CheckTrue(it.MoveNext);
    CheckEquals([1, 1], idx);
    CheckTrue(it.MoveNext);
    CheckEquals([1, 0], idx);

    CheckFalse(it.MoveNext);
  finally
    it.Free;
  end;
end;

procedure TNDIdxItTests.Array2DAPerm;
var shape, idx: TArray<NativeInt>;
    revAxis: TArray<Boolean>;
    perm: TArray<Integer>;
    it: TNDIdxIt;
begin
  shape := TArray<NativeInt>.Create(2, 3);
  revAxis := TArray<Boolean>.Create(False, False);
  perm := TArray<Integer>.Create(1, 0);

  it := TNDIdxIt.Create(shape, revAxis, perm);
  try
    idx := it.CurrentIndex;
    CheckEquals(2, Length(idx));

    CheckTrue(it.MoveNext);
    CheckEquals([0, 0], idx);
    CheckTrue(it.MoveNext);
    CheckEquals([1, 0], idx);
    CheckTrue(it.MoveNext);
    CheckEquals([2, 0], idx);

    CheckTrue(it.MoveNext);
    CheckEquals([0, 1], idx);
    CheckTrue(it.MoveNext);
    CheckEquals([1, 1], idx);
    CheckTrue(it.MoveNext);
    CheckEquals([2, 1], idx);

    CheckFalse(it.MoveNext);
  finally
    it.Free;
  end;
end;

{$endregion}

{$region 'TNDAUtTests'}

procedure TNDAUtTests.FullArray2D;
var a: INDArray;
    m: TArray<TArray<Integer>>;
    I, J: Integer;
begin
  a := TNDAUt.Full<Integer>([2, 3], 2);

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, m));
  CheckEquals(2, Length(m));
  for I := 0 to High(m) do begin
    CheckEquals(3, Length(m[I]));
    for J := 0 to High(m[I]) do
      CheckEquals(2, m[I, J]);
  end;
end;

procedure TNDAUtTests.MakeArray2D;
var a: INDArray;
    m: TArray<TArray<Integer>>;
    I, J: Integer;
const data: array [0..5] of Integer = (1, 2, 3, 4, 5, 6);
begin
  a := TNDAUt.AsArray<Integer>(data, [2, 3]);

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, m));
  CheckEquals(2, Length(m));
  CheckEquals(3, Length(m[0]));
  CheckEquals(3, Length(m[1]));
  for I := 0 to High(m) do
    for J := 0 to High(m[I]) do
      CheckEquals(3*I + J + 1, m[I, J]);
end;

procedure TNDAUtTests.MakeArrayFrom2DDynArray;
var a: TNDAItems<Integer>;
    I, J: Integer;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6]]);

  CheckEquals(2, a.NDim);
  CheckEquals(2, a.Shape[0]);
  CheckEquals(3, a.Shape[1]);

  for I := 0 to 1 do
    for J := 0 to 2 do
      CheckEquals(I * 3 + J + 1, a[[I, J]]);
end;

procedure TNDAUtTests.TryAsScalarTest;
var s: INDArray;
    val: Integer;
begin
  s := TNDScalar<Integer>.Create(10);
  CheckTrue(TNDAUt.TryAsScalar<Integer>(s, val));
  CheckEquals(10, val);
end;

procedure TNDAUtTests.Identity2D_Int;
var a: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
begin
  a := TNDAUt.Identity<Integer>(3);
  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, m));
  CheckEquals([1, 0, 0], m[0]);
  CheckEquals([0, 1, 0], m[1]);
  CheckEquals([0, 0, 1], m[2]);
end;

procedure TNDAUtTests.GetContDim2DLvl0;
var a: INDArray<Byte>;
    sz: NativeInt;
    lvl: Integer;
begin
  a := TNDAUt.AsArray<Byte>([[1, 2, 3], [4, 5, 6], [7, 8, 9]]);
  lvl := GetCContLvl(a, sz);
  CheckEquals(0, lvl);
  CheckEquals(9, sz);
end;

procedure TNDAUtTests.GetContDim2DLvl1;
var a: INDArray<Byte>;
    sz: NativeInt;
    lvl: Integer;
begin
  a := TNDAUt.AsArray<Byte>([[1, 2, 3], [4, 5, 6], [7, 8, 9]]);
  a := a[[NDIAll, NDISpan(1)]];
  lvl := GetCContLvl(a, sz);
  CheckEquals(1, lvl);
  CheckEquals(2, sz);
end;

procedure TNDAUtTests.GetContDim2DLvl2;
var a: INDArray<Byte>;
    sz: NativeInt;
    lvl: Integer;
begin
  a := TNDAUt.AsArray<Byte>([[1, 2, 3], [4, 5, 6], [7, 8, 9]]);
  a := a[[NDIAll, NDISpan(0, -1, 2)]];
  lvl := GetCContLvl(a, sz);
  CheckEquals(2, lvl);
  CheckEquals(1, sz);
end;

procedure TNDAUtTests.GetContDim3DLvl0;
var a: INDArray<Byte>;
    sz: NativeInt;
    lvl: Integer;
begin
  a := TNDAUt.AsArray<Byte>(
    [1,   2,  3,  4,  5,  6,
     7,   8,  9, 10, 11, 12,
     13, 14, 15, 16, 17, 18],
    [3, 2, 3]
  );
  lvl := GetCContLvl(a, sz);
  CheckEquals(0, lvl);
  CheckEquals(18, sz);

  a := a[[NDISpan(1,2), NDIAll, NDIAll]];
  lvl := GetCContLvl(a, sz);
  CheckEquals(0, lvl);
  CheckEquals(12, sz);
end;

procedure TNDAUtTests.GetContDim3DLvl1;
var a: INDArray<Byte>;
    sz: NativeInt;
    lvl: Integer;
begin
  a := TNDAUt.AsArray<Byte>(
    [1,   2,  3,  4,  5,  6,
     7,   8,  9, 10, 11, 12,
     13, 14, 15, 16, 17, 18],
    [3, 2, 3]
  );
  a := a[[NDIAll, NDISpan(1), NDIAll]];
  lvl := GetCContLvl(a, sz);
  CheckEquals(1, lvl);
  CheckEquals(3, sz);
end;

procedure TNDAUtTests.GEtContDim3DLvl2;
var a: INDArray<Byte>;
    sz: NativeInt;
    lvl: Integer;
begin
  a := TNDAUt.AsArray<Byte>(
    [1, 2, 3, 4, 5, 6,
     7, 8, 9, 10, 11, 12],
    [2, 2, 3]
  );
  a := a[[NDIAll, NDIAll, NDISpan(1)]];
  lvl := GetCContLvl(a, sz);
  CheckEquals(2, lvl);
  CheckEquals(2, sz);
end;

procedure TNDAUtTests.GetContDim3DLvl3;
var a: INDArray<Byte>;
    sz: NativeInt;
    lvl: Integer;
begin
  a := TNDAUt.AsArray<Byte>(
    [1, 2, 3, 4, 5, 6,
     7, 8, 9, 10, 11, 12],
    [2, 2, 3]
  );
  a := a[[NDIAll, NDIAll, NDISpan(0, -1, 2)]];
  lvl := GetCContLvl(a, sz);
  CheckEquals(3, lvl);
  CheckEquals(1, sz);
end;

procedure TNDAUtTests.BroadcastLvlArr2DArr1D;
var a, b: INDArray<Integer>;
    lvl: Integer;
begin
  a := TNDAUt.Full<Integer>([2, 3], 0);
  b := TNDAUt.Full<Integer>([3], 0);
  lvl := BroadcastLvl(a, b);

  CheckEquals(1, lvl);
end;

procedure TNDAUtTests.BreadcastLvlArr2DArr2D;
var a, b: INDArray<Integer>;
    lvl: Integer;
begin
  a := TNDAUt.Full<Integer>([2, 3], 0);
  b := TNDAUt.Full<Integer>([2, 3], 0);
  lvl := BroadcastLvl(a, b);

  CheckEquals(0, lvl);
end;

procedure TNDAUtTests.Copy2DPacked;
var a: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
    I, J: Integer;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6]]);
  a := TNDAUt.Copy<Integer>(a);

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, m));
  CheckEquals(2, Length(m));
  CheckEquals(3, Length(m[0]));
  CheckEquals(3, Length(m[1]));
  for I := 0 to High(m) do
    for J := 0 to High(m[I]) do
      CheckEquals(3*I + J + 1, m[I, J]);
end;

procedure TNDAUtTests.Copy2DStridesLvl0;
var a: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6], [7, 8, 9]]);
  a := TNDAUt.Copy<Integer>(a[[NDISpan(0, -1, 2), NDIAll]]);

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, m));
  CheckEquals(2, Length(m));
  CheckEquals(3, Length(m[0]));
  CheckEquals(3, Length(m[1]));

  CheckEquals(1, m[0, 0]);
  CheckEquals(2, m[0, 1]);
  CheckEquals(3, m[0, 2]);

  CheckEquals(7, m[1, 0]);
  CheckEquals(8, m[1, 1]);
  CheckEquals(9, m[1, 2]);
end;

procedure TNDAUtTests.Copy2DStridesLvl1;
var a: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
    I: Integer;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6], [7, 8, 9]]);
  a := TNDAUt.Copy<Integer>(a[[NDIAll, NDISpan(0, -1, 2)]]);

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, m));
  CheckEquals(3, Length(m));
  for I := 0 to High(m) do
    CheckEquals(2, Length(m[I]));

  CheckEquals(1, m[0, 0]);
  CheckEquals(3, m[0, 1]);

  CheckEquals(4, m[1, 0]);
  CheckEquals(6, m[1, 1]);

  CheckEquals(7, m[2, 0]);
  CheckEquals(9, m[2, 1]);
end;

procedure TNDAUtTests.Copy2DStrides;
var a: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6], [7, 8, 9]]);
  a := TNDAUt.Copy<Integer>(a[[NDISpan(0, -1, 2), NDISpan(0, -1, 2)]]);

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, m));
  CheckEquals(2, Length(m));
  CheckEquals(2, Length(m[0]));
  CheckEquals(2, Length(m[1]));

  CheckEquals(1, m[0, 0]);
  CheckEquals(3, m[0, 1]);

  CheckEquals(7, m[1, 0]);
  CheckEquals(9, m[1, 1]);
end;

procedure TNDAUtTests.FillVecByConst;
var a: INDArray<Integer>;
    v: TArray<Integer>;
begin
  a := TNDAUt.AsArray<Integer>([0, 0, 0]);
  TNDAUt.Fill<Integer>(a, 1);

  CheckTrue(TNDAUt.TryAsDynArray<Integer>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(1, v[0]);
  CheckEquals(1, v[1]);
  CheckEquals(1, v[2]);
end;

procedure TNDAUtTests.FillVecByVec;
var a, b: INDArray<Integer>;
    v: TArray<Integer>;
begin
  a := TNDAUt.AsArray<Integer>([0, 0, 0]);
  b := TNDAUt.AsArray<Integer>([1, 2, 3]);
  TNDAUt.Fill<Integer>(a, b);

  CheckTrue(TNDAUt.TryAsDynArray<Integer>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(1, v[0]);
  CheckEquals(2, v[1]);
  CheckEquals(3, v[2]);
end;

{$endregion}

{$region 'TBinMapTests'}

type
  TNDAUt = class(panda.Arrays.TNDAUt)
  end;

// A <- Fb(A, B) = 2*A - B
procedure Fb(N: NativeInt; A: PByte; IncA: NativeInt; B: PByte; IncB: NativeInt);
var pEnd: PByte;
    s: Integer;
begin
  if IncB = 0 then begin
    s := PInteger(B)^;
    pEnd := A + N * IncA;
    while A < pEnd do begin
      PInteger(A)^  := 2*PInteger(A)^ - s;
      Inc(A, IncA);
    end;

    exit;
  end;

  pEnd := A + N * IncA;
  while A < pEnd do begin
    PInteger(A)^ := 2*PInteger(A)^ - PInteger(B)^;
    Inc(A, IncA);
    Inc(B, IncB);
  end;
end;

// B <- Fa(A, B) = 2*A - B
procedure Fa(N: NativeInt; A: PByte; IncA: NativeInt; B: PByte; IncB: NativeInt);
var pEnd: PByte;
    s: Integer;
begin
  if IncA = 0 then begin
    s := PInteger(A)^;
    pEnd := B + N * IncB;
    while B < pEnd do begin
      PInteger(B)^  := 2*s - PInteger(B)^;
      Inc(B, IncB);
    end;

    exit;
  end;

  pEnd := A + N * IncA;
  while A < pEnd do begin
    PInteger(B)^ := 2*PInteger(A)^ - PInteger(B)^;
    Inc(A, IncA);
    Inc(B, IncB);
  end;
end;

procedure TBinMapTests.TestCompatibleQ;
var s0, s1, s: TNDAShape;
begin
  s0 := TNDAShape.Create(8, 1, 6, 1);
  s1 := TNDAShape.Create(   7, 1, 5);

  CheckTrue(CompatibleQ([s0, s1], s));

  CheckEquals(4, Length(s));
  CheckEquals([8, 7, 6, 5], s);
end;

procedure TBinMapTests.TestCompatibleQ_NotComp;
var s0, s1, s: TNDAShape;
begin
  s0 := TNDAShape.Create(2, 3, 2);
  s1 := TNDAShape.Create(2, 2, 1);

  CheckFalse(CompatibleQ([s0, s1], s));
end;

procedure TBinMapTests.TestCompatibleQ_4Arr;
var s0, s1, s2, s3, s: TNDAShape;
begin
  s0 := TNDAShape.Create(   6, 7);
  s1 := TNDAShape.Create(5, 6, 1);
  s2 := TNDAShape.Create(      7);
  s3 := TNDAShape.Create(5, 1, 7);

  CheckTrue(CompatibleQ([s0, s1, s2, s3], s));

  CheckEquals(3, Length(s));
  CheckEquals([5, 6, 7], s);
end;

procedure TBinMapTests.TestContinguousQ_Vec;
var s, str: TNDAShape;
begin
  s := TNDAShape.Create(3);
  str := TNDAShape.Create(8);
  CheckTrue(ContiguousQ(8, s, str));
  CheckFalse(ContiguousQ(4, s, str));
end;

procedure TBinMapTests.TestContinguousQ_Mat;
var s, str: TNDAShape;
begin
  s := TNDAShape.Create(2, 3);
  str := TNDAShape.Create(24, 8);
  CheckTrue(ContiguousQ(8, s, str));
  CheckFalse(ContiguousQ(4, s, str));

  str := TNDAShape.Create(30, 8);
  CheckFalse(ContiguousQ(8, s, str));
end;

procedure TBinMapTests.Scalar_Scalar;
var a: INDArray<Integer>;
    s: Integer;
begin
  a := TNDAUt.Scalar<Integer>(1);
  TNDAUt.MapR<Integer>(a, 2, Fb);

  CheckTrue(TNDAUt.TryAsScalar<Integer>(a, s));
  CheckEquals(0, s);
end;

procedure TBinMapTests.CCArr1D_Scalar;
var a: INDArray<Integer>;
    v: TArray<Integer>;
    I: Integer;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3]);
  TNDAUt.MapR<Integer>(a, 2, Fb);

  CheckTrue(TNDAUt.TryAsDynArray<Integer>(a, v));
  CheckEquals(3, Length(v));
  for I := 0 to High(v) do
    CheckEquals(2 * (I + 1) - 2, v[I]);
end;

procedure TBinMapTests.Arr1D_Scalar;
var a: INDArray<Integer>;
    v: TArray<Integer>;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3, 4, 5]);
  TNDAUt.MapR<Integer>(a[[NDIAll(2)]], 2, Fb);

  CheckTrue(TNDAUt.TryAsDynArray<Integer>(a, v));
  CheckEquals(5, Length(v));
  CheckEquals(0, v[0]);
  CheckEquals(2, v[1]);
  CheckEquals(4, v[2]);
  CheckEquals(4, v[3]);
  CheckEquals(8, v[4]);
end;

procedure TBinMapTests.CCLvl1Arr2D_Scalar;
var a: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
    I: Integer;
begin
  a := iRng2NDA([3, 3]);
  TNDAUt.MapR<Integer>(a[[NDIAll(2)]], 2, Fb);

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, m));
  CheckEquals(3, Length(m));
  CheckEquals(3, Length(m[0]));
  for I := 0 to High(m[0]) do
    CheckEquals(2 * (I + 1) - 2, m[0, I]);

  CheckEquals(3, Length(m[1]));
  for I := 0 to High(m[1]) do
    CheckEquals(I + 4, m[1, I]);

  CheckEquals(3, Length(m[2]));
  for I := 0 to High(m[2]) do
    CheckEquals(2 * (I + 7) - 2, m[2, I]);
end;

procedure TBinMapTests.Arr2D_Scalar;
var a: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
    I: Integer;
begin
  a := iRng2NDA([3, 3]);
  TNDAUt.MapR<Integer>(a[[NDIAll(2), NDIAll(2)]], 2, Fb);

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, m));
  CheckEquals(3, Length(m));
  for I := 0 to High(m) do
    CheckEquals(3, Length(m[I]));

  CheckEquals(0,  m[0, 0]);
  CheckEquals(2,  m[0, 1]);
  CheckEquals(4,  m[0, 2]);

  CheckEquals(4,  m[1, 0]);
  CheckEquals(5,  m[1, 1]);
  CheckEquals(6,  m[1, 2]);

  CheckEquals(12, m[2, 0]);
  CheckEquals(8,  m[2, 1]);
  CheckEquals(16, m[2, 2]);
end;

procedure TBinMapTests.CCArr2D_CCArr2D;
var a, b: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
begin
  a := TNDAUt.AsArray<Integer>([[3, 2, 1], [6, 5, 4]]);
  b := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6]]);
  TNDAUt.MapR(a, b, Fb);

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, m));
  CheckEquals(2, Length(m));
  CheckEquals(3, Length(m[0]));
  CheckEquals(3, Length(m[1]));

  CheckEquals(5,  m[0, 0]);
  CheckEquals(2,  m[0, 1]);
  CheckEquals(-1, m[0, 2]);

  CheckEquals(8,  m[1, 0]);
  CheckEquals(5,  m[1, 1]);
  CheckEquals(2,  m[1, 2]);
end;

procedure TBinMapTests.CCArr2D_CCLvl1Arr2D;
var a, b: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
begin
  a := TNDAUt.AsArray<Integer>([[3, 2, 1], [6, 5, 4]]);
  b := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6], [7, 8, 9]]);
  TNDAUt.MapR(a, b[[NDIAll(2)]], Fb);

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, m));
  CheckEquals(2, Length(m));
  CheckEquals(3, Length(m[0]));
  CheckEquals(3, Length(m[1]));

  CheckEquals(5,  m[0, 0]);
  CheckEquals(2,  m[0, 1]);
  CheckEquals(-1, m[0, 2]);

  CheckEquals(5,  m[1, 0]);
  CheckEquals(2,  m[1, 1]);
  CheckEquals(-1, m[1, 2]);
end;

procedure TBinMapTests.CCArr2D_Arr2D;
var a, b: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
begin
  a := TNDAUt.AsArray<Integer>([[2, 1], [4, 3]]);
  b := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6]]);
  TNDAUt.MapR(a, b[[NDIAll, NDIAll(2)]], Fb);

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, m));
  CheckEquals(2, Length(m));
  CheckEquals(2, Length(m[0]));
  CheckEquals(2, Length(m[1]));

  CheckEquals(3,  m[0, 0]);
  CheckEquals(-1, m[0, 1]);

  CheckEquals(4,  m[1, 0]);
  CheckEquals(0,  m[1, 1]);
end;

procedure TBinMapTests.CCLvl1Arr2D_CCLvl1Arr2D;
var a, b: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
begin
  a := TNDAUt.AsArray<Integer>([[3, 2, 1], [6, 5, 4], [9, 8, 7]]);
  a := a[[NDIAll(2)]];
  b := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6], [7, 8, 9]]);
  b := b[[NDIAll(2)]];
  TNDAUt.MapR(a, b, Fb);

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, m));
  CheckEquals(2, Length(m));
  CheckEquals(3, Length(m[0]));
  CheckEquals(3, Length(m[1]));

  CheckEquals(5,  m[0, 0]);
  CheckEquals(2,  m[0, 1]);
  CheckEquals(-1, m[0, 2]);

  CheckEquals(11, m[1, 0]);
  CheckEquals(8,  m[1, 1]);
  CheckEquals(5,  m[1, 2]);
end;

procedure TBinMapTests.Arr2D_Arr2D;
var a, b: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
begin
  a := TNDAUt.AsArray<Integer>([[3, 2, 1], [6, 5, 4], [9, 8, 7]]);
  a := a[[NDIAll(2), NDIAll(2)]];
  b := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6], [7, 8, 9]]);
  b := b[[NDIAll(2), NDIAll(2)]];
  TNDAUt.MapR(a, b, Fb);

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, m));
  CheckEquals(2, Length(m));
  CheckEquals(2, Length(m[0]));
  CheckEquals(2, Length(m[1]));

  CheckEquals(5,  m[0, 0]);
  CheckEquals(-1, m[0, 1]);

  CheckEquals(11, m[1, 0]);
  CheckEquals(5,  m[1, 1]);
end;

procedure TBinMapTests.Arr1D_ScalarAsArr;
var a, b: INDArray<Integer>;
    v: TArray<Integer>;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3]);
  b := TNDAUt.Scalar<Integer>(4);
  TNDAUt.MapR(a, b, Fb);

  CheckTrue(TNDAUt.TryAsDynArray<Integer>(a, v));
  CheckEquals(3, Length(v));

  CheckEquals(-2, v[0]);
  CheckEquals(0,  v[1]);
  CheckEquals(2,  v[2]);
end;

procedure TBinMapTests.CCArr2D_CCArr1D;
var a, b: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
begin
  a := TNDAUt.AsArray<Integer>([[3, 2, 1], [6, 5, 4]]);
  b := TNDAUt.AsArray<Integer>([1, 2, 3]);
  TNDAUt.MapR(a, b, Fb);

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, m));
  CheckEquals(2, Length(m));
  CheckEquals(3, Length(m[0]));
  CheckEquals(3, Length(m[1]));

  CheckEquals(5,  m[0, 0]);
  CheckEquals(2,  m[0, 1]);
  CheckEquals(-1, m[0, 2]);

  CheckEquals(11, m[1, 0]);
  CheckEquals(8,  m[1, 1]);
  CheckEquals(5,  m[1, 2]);
end;

procedure TBinMapTests.Scalar_CCArr1D;
var a: INDArray<Integer>;
    v: TArray<Integer>;
    I: Integer;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3]);
  TNDAUt.MapL<Integer>(2, a, Fa);

  CheckTrue(TNDAUt.TryAsDynArray<Integer>(a, v));
  CheckEquals(3, Length(v));
  for I := 0 to High(v) do
    CheckEquals(2 * 2 - (I + 1), v[I]);
end;

procedure TBinMapTests.CCArr1D_CCArr2D;
var a, b: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
begin
  a := TNDAUt.AsArray<Integer>([3, 2, 1]);
  b := iRng2NDA([2, 3]);
  TNDAUt.MapL(a, b, Fa);

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(b, m));
  CheckEquals(2, Length(m));
  CheckEquals(3, Length(m[0]));
  CheckEquals(3, Length(m[1]));

  CheckEquals(5,  m[0, 0]);
  CheckEquals(2,  m[0, 1]);
  CheckEquals(-1, m[0, 2]);

  CheckEquals(2,  m[1, 0]);
  CheckEquals(-1, m[1, 1]);
  CheckEquals(-4, m[1, 2]);
end;

procedure TBinMapTests.CCArr1D_CCLvl1Arr2D;
var a, b: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
    I: Integer;
begin
  a := TNDAUt.AsArray<Integer>([3, 2, 1]);
  b := iRng2NDA([3, 3]);
  TNDAUt.MapL(a, b[[NDIAll(2), NDIAll]], Fa);

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(b, m));
  CheckEquals(3, Length(m));
  for I := 0 to High(m) do
    CheckEquals(3, Length(m[I]));

  CheckEquals(5,  m[0, 0]);
  CheckEquals(2,  m[0, 1]);
  CheckEquals(-1, m[0, 2]);

  CheckEquals(4,  m[1, 0]);
  CheckEquals(5,  m[1, 1]);
  CheckEquals(6,  m[1, 2]);

  CheckEquals(-1, m[2, 0]);
  CheckEquals(-4, m[2, 1]);
  CheckEquals(-7, m[2, 2]);
end;

procedure TBinMapTests.CCArr1D_Arr2D;
var a, b: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
    I: Integer;
begin
  a := TNDAUt.AsArray<Integer>([3, 1]);
  b := iRng2NDA([3, 3]);
  TNDAUt.MapL(a, b[[NDIAll(2), NDIAll(2)]], Fa);

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(b, m));
  CheckEquals(3, Length(m));
  for I := 0 to High(m) do
    CheckEquals(3, Length(m[I]));

  CheckEquals(5,  m[0, 0]);
  CheckEquals(2,  m[0, 1]);
  CheckEquals(-1, m[0, 2]);

  CheckEquals(4,  m[1, 0]);
  CheckEquals(5,  m[1, 1]);
  CheckEquals(6,  m[1, 2]);

  CheckEquals(-1, m[2, 0]);
  CheckEquals(8,  m[2, 1]);
  CheckEquals(-7, m[2, 2]);
end;

procedure TBinMapTests.Arr1D_Arr2D;
var a, b: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
    I: Integer;
begin
  a := TNDAUt.AsArray<Integer>([3, 2, 1]);
  b := iRng2NDA([3, 3]);
  TNDAUt.MapL(a[[NDIAll(2)]], b[[NDIAll(2), NDIAll(2)]], Fa);

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(b, m));
  CheckEquals(3, Length(m));
  for I := 0 to High(m) do
    CheckEquals(3, Length(m[I]));

  CheckEquals(5,  m[0, 0]);
  CheckEquals(2,  m[0, 1]);
  CheckEquals(-1, m[0, 2]);

  CheckEquals(4,  m[1, 0]);
  CheckEquals(5,  m[1, 1]);
  CheckEquals(6,  m[1, 2]);

  CheckEquals(-1, m[2, 0]);
  CheckEquals(8,  m[2, 1]);
  CheckEquals(-7, m[2, 2]);
end;

procedure TBinMapTests.Arr2D_Arr1D;
var a, b: INDArray<Integer>;
    m: TArray<TArray<Integer>>;
    I: Integer;
begin
  a := iRng2NDA([3, 3]);
  b := TNDAUt.AsArray<Integer>([3, 2, 1]);
  TNDAUt.MapR(a[[NDIAll(2), NDIAll(2)]], b[[NDIAll(2)]], Fb);

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, m));
  CheckEquals(3, Length(m));
  for I := 0 to High(m) do
    CheckEquals(3, Length(m[I]));

  CheckEquals(-1, m[0, 0]);
  CheckEquals(2,  m[0, 1]);
  CheckEquals(5,  m[0, 2]);

  CheckEquals(4,  m[1, 0]);
  CheckEquals(5,  m[1, 1]);
  CheckEquals(6,  m[1, 2]);

  CheckEquals(11, m[2, 0]);
  CheckEquals(8,  m[2, 1]);
  CheckEquals(17, m[2, 2]);
end;

procedure TBinMapTests.Arr1D_Arr2DInv;
var a, b: INDArray<Integer>;
begin
  a := TNDAUt.Full<Integer>([3], 0);
  b := iRng2NDA([3, 2]);

  ExpectedException := ENDAMapError;
  TNDAUt.MapL(a, b, Fa);
end;

{$endregion}

{$region 'TNDACvtTests'}

procedure TNDACvtTests.CvtF32F64;
var a: INDArray<Single>;
    b: INDArray<Double>;
    bArr: TArray<Double>;
begin
  a := TNDAUt.AsArray<Single>([1, 2, 3]);
  b := TNDAUt.AsType<Double>(a);
  CheckTrue(TNDAUt.TryAsDynArray<Double>(b, bArr));

  CheckEquals(3, Length(bArr));
  CheckEquals(1, bArr[0]);
  CheckEquals(2, bArr[1]);
  CheckEquals(3, bArr[2]);
end;

{$endregion}

initialization

  RegisterTest(TArrayTests.Suite);
  RegisterTest(TIndicesTests.Suite);
  RegisterTest(TNDAItTests.Suite);
  RegisterTest(TNDASliceItTests.Suite);
  RegisterTest(TNDASliceItChainTests.Suite);
  RegisterTest(TNDIdxItTests.Suite);
  RegisterTest(TNDAUtTests.Suite);
  RegisterTest(TBinMapTests.Suite);
  RegisterTest(TNDACvtTests.Suite);

end.
