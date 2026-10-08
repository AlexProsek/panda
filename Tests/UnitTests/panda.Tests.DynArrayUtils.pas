unit panda.Tests.DynArrayUtils;

interface

uses
    TestFramework
  , panda.DynArrayUtils
  , panda.Tests.NDATestCase
  ;

type
  TMergeSortTests = class(TNDATestCase)
  published
    procedure Sort8;
    procedure Sort10;
    procedure Sort11;
    procedure StabilityTest;
  end;

  TDynArrUtilTests = class(TNDATestCase)
  private
    procedure CheckNInts(const aExpected: array of Integer; const aValue: TArray<NativeInt>);
    procedure CheckIntHeap(const aData: TArray<Integer>; aCount: Integer; aMinHeap: Boolean);
    procedure CheckSortedPrefix(const aData: TArray<Integer>; aCount: Integer;
      const aExpected: array of Integer);
  published
    procedure Concat;
    procedure Clone;
    procedure CopyData;
    procedure CopySameArray;
    procedure CopyOutOfRange;
    procedure SearchPos;
    procedure MergeSort;
    procedure SearchPosSuccessively;
    procedure Flatten;
    procedure Partition;
    procedure PartitionManaged;
    procedure Reverse;
    procedure ReverseInPlace;
    procedure SwapItems;
    procedure SwapRows;
    procedure Table;
    procedure ElementsCount;
    procedure SubArray;
    procedure Permute;
    procedure InversePermutation;
    procedure Rotate;
    procedure RotateRightManaged;
    procedure RotateLeftManaged;
    procedure MakeHeap;
    procedure HeapSort;
    procedure HeapPop;
    procedure HeapPush;
    procedure HeapPushFull;
    procedure HeapRemove;
    procedure Take;
    procedure Drop;
    procedure InsertItem;
    procedure InsertList;
    procedure Select;
    procedure Position;
    procedure FirstPosition;
    procedure SortPerm;
    procedure MakeIndexedValues;
    procedure SortIndexedValuesByValue;
    procedure SortIndexedValuesByIndex;
    procedure MergeSortIndexedValuesByIndex;
    procedure Map;
    procedure Append;
    procedure Prepend;
    procedure AppendList;
    procedure PrependList;
    procedure MemberQ;
    procedure FindMin;
    procedure FindMax;
    procedure FindMinMax;
    procedure FindMinMax_MaxAtFirst;
    procedure FindFirstPos;
    procedure Where;
    procedure Median;
    procedure GatherBy;
    procedure SplitBy;
    procedure MovingMap;
    procedure MovingMapTooWide;
    procedure ToMListString;
    procedure Riffle;
    procedure RiffleDifferentLengths;
    procedure ToArray;
    procedure ConstantArray;
    procedure ItemCount;
    procedure MaxLength;
    procedure PositiveQ;
    procedure Int2NInt;
    procedure Range;
    procedure CaseConvert;
    procedure MatrixShape;
    procedure Triangularize;
    procedure SetDiagonal;
    procedure Transpose;
    procedure Augment;
  end;

implementation

uses
    SysUtils
  , Math
  , IOUtils
  , Generics.Defaults
  , Generics.Collections
  ;


{$region 'TMergeSortTests'}

procedure TMergeSortTests.Sort8;
var arr, res: TArray<Integer>;
    me: TMergeSort<Integer>;
begin
  arr := TArray<Integer>.Create(8, 7, 6, 5, 4, 3, 2, 1);

  me.Init;
  res := me.Sort(arr);

  CheckEquals([1, 2, 3, 4, 5, 6, 7, 8], res);
end;

procedure TMergeSortTests.Sort10;
var arr, res: TArray<Integer>;
    me: TMergeSort<Integer>;
begin
  arr := TArray<Integer>.Create(10, 9, 8, 7, 6, 5, 4, 3, 2, 1);

  me.Init;
  res := me.Sort(arr);

  CheckEquals([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], res);
end;

procedure TMergeSortTests.Sort11;
var arr, res: TArray<Integer>;
    me: TMergeSort<Integer>;
begin
  arr := TArray<Integer>.Create(11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1);

  me.Init;
  res := me.Sort(arr);

  CheckEquals([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11], res);
end;

procedure TMergeSortTests.StabilityTest;
var me: TMergeSort<TIndexedValue<Integer>>;
    data, res: TArray<TIndexedValue<Integer>>;
begin
  me.Init(TIndexedValueIndexComparer<Integer>.Create);

  SetLength(data, 5);
  data[0].Init(3, 1);
  data[1].Init(2, 3);
  data[2].Init(1, 1);
  data[3].Init(2, 2);
  data[4].Init(2, 1);

  res := me.Sort(data);

  CheckEquals(1, res[0].Idx);
  CheckEquals(1, res[0].Value);

  CheckEquals(2, res[1].Idx);
  CheckEquals(3, res[1].Value);

  CheckEquals(2, res[2].Idx);
  CheckEquals(2, res[2].Value);

  CheckEquals(2, res[3].Idx);
  CheckEquals(1, res[3].Value);

  CheckEquals(3, res[4].Idx);
  CheckEquals(1, res[4].Value);
end;

{$endregion}

{$region 'TDynArrUtilTests'}

procedure TDynArrUtilTests.CheckNInts(const aExpected: array of Integer; const aValue: TArray<NativeInt>);
var I: Integer;
begin
  CheckEquals(Length(aExpected), Length(aValue));
  for I := 0 to High(aExpected) do
    CheckEquals(aExpected[I], aValue[I]);
end;

procedure TDynArrUtilTests.CheckIntHeap(const aData: TArray<Integer>; aCount: Integer; aMinHeap: Boolean);
var I, left, right: Integer;
begin
  for I := 0 to (aCount div 2) - 1 do begin
    left := 2 * I + 1;
    right := left + 1;
    if aMinHeap then begin
      if left < aCount then
        CheckTrue(aData[I] <= aData[left]);
      if right < aCount then
        CheckTrue(aData[I] <= aData[right]);
    end else begin
      if left < aCount then
        CheckTrue(aData[I] >= aData[left]);
      if right < aCount then
        CheckTrue(aData[I] >= aData[right]);
    end;
  end;
end;

procedure TDynArrUtilTests.CheckSortedPrefix(const aData: TArray<Integer>; aCount: Integer;
  const aExpected: array of Integer);
var tmp: TArray<Integer>;
begin
  tmp := System.Copy(aData, 0, aCount);
  TArray.Sort<Integer>(tmp);
  CheckEquals(aExpected, tmp);
end;

procedure TDynArrUtilTests.Concat;
var a, b, res: TArray<Integer>;
begin
  a := TArray<Integer>.Create(1, 2);
  b := TArray<Integer>.Create(3, 4);

  res := TDynAUt.Concat<Integer>(a, b);

  CheckEquals([1, 2, 3, 4], res);

  a := nil;
  b := TArray<Integer>.Create(5);

  res := TDynAUt.Concat<Integer>(a, b);

  CheckEquals([5], res);

  res := TDynAUt.Concat<Integer>([
    TArray<Integer>.Create(1, 2),
    nil,
    TArray<Integer>.Create(3),
    TArray<Integer>.Create(4, 5)
  ]);

  CheckEquals([1, 2, 3, 4, 5], res);
end;

procedure TDynArrUtilTests.Clone;
var a, b: TArray<Integer>;
    s, s2: TArray<String>;
    a2, b2: TArray<TArray<Integer>>;
    a3, b3: TArray<TArray<TArray<Integer>>>;
begin
  a := TArray<Integer>.Create(1, 2, 3);

  b := TDynAUt.Clone<Integer>(a);
  b[0] := 9;

  CheckEquals([1, 2, 3], a);
  CheckEquals([9, 2, 3], b);

  s := TArray<String>.Create('a', 'b');

  s2 := TDynAUt.Clone<String>(s);
  s2[0] := 'z';

  CheckEquals('a', s[0]);
  CheckEquals('z', s2[0]);

  a2 := TArray<TArray<Integer>>.Create(
    TArray<Integer>.Create(1, 2),
    TArray<Integer>.Create(3, 4)
  );

  b2 := TDynAUt.Clone<Integer>(a2);
  b2[0, 0] := 9;

  CheckEquals([1, 2], a2[0]);
  CheckEquals([9, 2], b2[0]);
  CheckEquals([3, 4], b2[1]);

  SetLength(a3, 1, 1, 1);
  a3[0, 0, 0] := 5;

  b3 := TDynAUt.Clone<Integer>(a3);
  b3[0, 0, 0] := 8;

  CheckEquals(5, a3[0, 0, 0]);
  CheckEquals(8, b3[0, 0, 0]);

  a := nil;

  b := TDynAUt.Clone<Integer>(a);

  CheckEquals(0, Length(b));
end;

procedure TDynArrUtilTests.CopyData;
var src, dst: TArray<Integer>;
    sSrc, sDst: TArray<String>;
begin
  src := TArray<Integer>.Create(1, 2, 3, 4, 5);
  SetLength(dst, 5);

  TDynAUt.Copy<Integer>(src, dst, 1, 2, 3);

  CheckEquals([0, 0, 2, 3, 4], dst);

  dst := TArray<Integer>.Create(9, 9, 9);

  TDynAUt.Copy<Integer>(src, dst, -1, -1, 0);

  CheckEquals([9, 9, 9], dst);

  sSrc := TArray<String>.Create('aa', 'bb', 'cc');
  SetLength(sDst, 3);

  TDynAUt.Copy<String>(sSrc, sDst, 0, 1, 2);

  CheckEquals('', sDst[0]);
  CheckEquals('aa', sDst[1]);
  CheckEquals('bb', sDst[2]);
end;

procedure TDynArrUtilTests.CopySameArray;
var data: TArray<Integer>;
begin
  ExpectedException := EArgumentException;
  data := TArray<Integer>.Create(1, 2, 3);

  TDynAUt.Copy<Integer>(data, data, 0, 0, 2);
end;

procedure TDynArrUtilTests.CopyOutOfRange;
var src, dst: TArray<Integer>;
begin
  ExpectedException := EArgumentOutOfRangeException;
  src := TArray<Integer>.Create(1, 2, 3);
  SetLength(dst, 3);

  TDynAUt.Copy<Integer>(src, dst, 0, 0, 4);
end;

procedure TDynArrUtilTests.SearchPos;
var pos: NativeInt;
    ok: Boolean;
begin
  ok := TDynAUt.SearchPos<Integer>([0, 10, 20, 30, 40], 25, pos);

  CheckTrue(ok);
  CheckEquals(2, pos);

  ok := TDynAUt.SearchPos<Integer>([0, 10, 20, 30, 40], 20, pos);

  CheckTrue(ok);
  CheckEquals(2, pos);

  ok := TDynAUt.SearchPos<Integer>([0, 10, 20, 30, 40], 5, pos);

  CheckTrue(ok);
  CheckEquals(0, pos);

  ok := TDynAUt.SearchPos<Integer>([0, 10, 20, 30, 40], 25, pos, nil, 2);

  CheckTrue(ok);
  CheckEquals(2, pos);

  ok := TDynAUt.SearchPos<Integer>([0, 10, 20, 30, 40], 15, pos, nil, 3);

  CheckFalse(ok);

  ok := TDynAUt.SearchPos<Integer>([0, 10, 20, 30, 40], 0, pos);

  CheckFalse(ok);

  ok := TDynAUt.SearchPos<Integer>([0, 10, 20, 30, 40], 40, pos);

  CheckFalse(ok);

  ok := TDynAUt.SearchPos<Integer>([7], 7, pos);

  CheckTrue(ok);
  CheckEquals(0, pos);

  ok := TDynAUt.SearchPos<Integer>([7], 8, pos);

  CheckFalse(ok);

  ok := TDynAUt.SearchPos<Integer>([], 1, pos);

  CheckFalse(ok);

  ok := TDynAUt.SearchPos<Double>([0, 1, 2, 3, 4, 5], 4.55, pos);

  CheckTrue(ok);
  CheckEquals(4, pos);
end;

procedure TDynArrUtilTests.MergeSort;
var data: TArray<Integer>;
begin
  data := TArray<Integer>.Create(3, 1, 2);

  TDynAUt.MergeSort<Integer>(data);

  CheckEquals([1, 2, 3], data);

  data := TArray<Integer>.Create(1, 3, 2);

  TDynAUt.MergeSort<Integer>(data, TComparer<Integer>.Construct(
    function (const aL, aR: Integer): Integer
    begin
      Result := aR - aL;
    end
  ));

  CheckEquals([3, 2, 1], data);

  data := TArray<Integer>.Create(7);

  TDynAUt.MergeSort<Integer>(data);

  CheckEquals([7], data);

  data := nil;

  TDynAUt.MergeSort<Integer>(data);

  CheckEquals(0, Length(data));
end;

procedure TDynArrUtilTests.SearchPosSuccessively;
var pos: NativeInt;
    ok: Boolean;
    cmp: IComparer<Integer>;
begin
  cmp := TComparer<Integer>.Default;

  ok := TDynAUt.SearchPosSuccessively<Integer>([0, 10, 20, 30, 40], cmp, 25, pos);

  CheckTrue(ok);
  CheckEquals(2, pos);

  ok := TDynAUt.SearchPosSuccessively<Integer>([0, 10, 20, 30, 40], cmp, 0, pos);

  CheckTrue(ok);
  CheckEquals(0, pos);

  ok := TDynAUt.SearchPosSuccessively<Integer>([0, 10, 20, 30, 40], cmp, 40, pos);

  CheckFalse(ok);

  ok := TDynAUt.SearchPosSuccessively<Integer>([0, 10, 20, 30, 40], cmp, 15, pos, 0, 2);

  CheckTrue(ok);
  CheckEquals(1, pos);

  ok := TDynAUt.SearchPosSuccessively<Integer>([0, 10, 20, 30, 40], cmp, 25, pos, 0, 2);

  CheckFalse(ok);

  ok := TDynAUt.SearchPosSuccessively<Integer>([], cmp, 1, pos);

  CheckFalse(ok);
end;

procedure TDynArrUtilTests.Flatten;
var flat: TArray<Integer>;
    sflat: TArray<String>;
begin
  flat := TDynAUt.Flatten<Integer>(TArray<TArray<Integer>>.Create(
    TArray<Integer>.Create(1, 2),
    TArray<Integer>.Create(3),
    TArray<Integer>.Create(4, 5, 6)
  ));

  CheckEquals([1, 2, 3, 4, 5, 6], flat);

  sflat := TDynAUt.Flatten<String>(TArray<TArray<String>>.Create(
    TArray<String>.Create('a', 'b'),
    nil,
    TArray<String>.Create('c')
  ));

  CheckEquals(['a', 'b', 'c'], sflat);

  flat := TDynAUt.Flatten<Integer>(nil);

  CheckEquals(0, Length(flat));
end;

procedure TDynArrUtilTests.Partition;
var rows: TArray<TArray<Integer>>;
begin
  rows := TDynAUt.Partition<Integer>(TArray<Integer>.Create(1, 2, 3, 4, 5, 6), 3);

  CheckEquals(2, Length(rows));
  CheckEquals([1, 2, 3], rows[0]);
  CheckEquals([4, 5, 6], rows[1]);

  rows := TDynAUt.Partition<Integer>(TArray<Integer>.Create(1, 2, 3, 4, 5), 2);

  CheckEquals(3, Length(rows));
  CheckEquals([1, 2], rows[0]);
  CheckEquals([3, 4], rows[1]);
  CheckEquals([5], rows[2]);

  rows := TDynAUt.Partition<Integer>(nil, 2);

  CheckEquals(0, Length(rows));
end;

procedure TDynArrUtilTests.PartitionManaged;
var rows: TArray<TArray<String>>;
begin
  rows := TDynAUt.Partition<String>(TArray<String>.Create('a', 'b', 'c', 'd', 'e'), 2);

  CheckEquals(3, Length(rows));
  CheckEquals(['a', 'b'], rows[0]);
  CheckEquals(['c', 'd'], rows[1]);
  CheckEquals(['e'], rows[2]);

  rows := TDynAUt.Partition<String>(TArray<String>.Create('a', 'b', 'c', 'd'), 3, 1);

  CheckEquals(2, Length(rows));
  CheckEquals(['a', 'b', 'c'], rows[0]);
  CheckEquals(['c', 'd'], rows[1]);
end;

procedure TDynArrUtilTests.Reverse;
var src, res: TArray<Integer>;
    sres: TArray<String>;
begin
  src := TArray<Integer>.Create(1, 2, 3, 4);

  res := TDynAUt.Reverse<Integer>(src);

  CheckEquals([4, 3, 2, 1], res);
  CheckEquals([1, 2, 3, 4], src);

  sres := TDynAUt.Reverse<String>(TArray<String>.Create('a', 'b', 'c'));

  CheckEquals(['c', 'b', 'a'], sres);

  res := TDynAUt.Reverse<Integer>(nil);

  CheckEquals(0, Length(res));
end;

procedure TDynArrUtilTests.ReverseInPlace;
var data: TArray<Integer>;
begin
  data := TArray<Integer>.Create(1, 2, 3, 4);

  TDynAUt.ReverseInPlace<Integer>(data);

  CheckEquals([4, 3, 2, 1], data);

  data := TArray<Integer>.Create(1, 2, 3);

  TDynAUt.ReverseInPlace<Integer>(data);

  CheckEquals([3, 2, 1], data);
end;

procedure TDynArrUtilTests.SwapItems;
var data: TArray<Integer>;
begin
  data := TArray<Integer>.Create(1, 2, 3);

  TDynAUt.SwapItems<Integer>(data, 0, 2);

  CheckEquals([3, 2, 1], data);
end;

procedure TDynArrUtilTests.SwapRows;
var rows: TArray<TArray<Integer>>;
begin
  rows := TArray<TArray<Integer>>.Create(
    TArray<Integer>.Create(1, 2),
    TArray<Integer>.Create(3),
    TArray<Integer>.Create(4, 5, 6)
  );

  TDynAUt.SwapRows<Integer>(rows, 0, 1);

  CheckEquals([3], rows[0]);
  CheckEquals([1, 2], rows[1]);
  CheckEquals([4, 5, 6], rows[2]);
end;

procedure TDynArrUtilTests.Table;
var res: TArray<Integer>;
    res2: TArray<TArray<Integer>>;
begin
  res := TDynAUt.Table<Integer>(
    function (const aX: Double): Integer
    begin
      Result := Round(aX);
    end,
    1, 4, 1
  );

  CheckEquals([1, 2, 3, 4], res);

  res := TDynAUt.Table<Integer>(
    function (const aX: Double): Integer
    begin
      Result := Round(aX);
    end,
    4, 1, -1
  );

  CheckEquals([4, 3, 2, 1], res);

  res := TDynAUt.Table<Integer>(
    function (const aX: Double): Integer
    begin
      Result := Round(aX);
    end,
    4, 1, 1
  );

  CheckEquals(0, Length(res));

  res2 := TDynAUt.Table<Integer>(
    function (const aX, aY: Double): Integer
    begin
      Result := Round(10 * aX + aY);
    end,
    0, 1, 1,
    0, 2, 1
  );

  CheckEquals(2, Length(res2));
  CheckEquals([0, 1, 2], res2[0]);
  CheckEquals([10, 11, 12], res2[1]);
end;

procedure TDynArrUtilTests.ElementsCount;
var data: TArray<TArray<Integer>>;
begin
  data := TArray<TArray<Integer>>.Create(
    TArray<Integer>.Create(1, 2),
    nil,
    TArray<Integer>.Create(3)
  );

  CheckEquals(3, TDynAUt.ElementsCount<Integer>(data));

  CheckEquals(0, TDynAUt.ElementsCount<Integer>(nil));
end;

procedure TDynArrUtilTests.SubArray;
var src: TArray<TArray<Integer>>;
    res2: TArray<TArray<Integer>>;
    res: TArray<Integer>;
begin
  src := TArray<TArray<Integer>>.Create(
    TArray<Integer>.Create(1, 2, 3),
    TArray<Integer>.Create(4, 5, 6),
    TArray<Integer>.Create(7, 8, 9)
  );

  res2 := TDynAUt.SubArray<Integer>(src, 1, 1, 2, 2);

  CheckEquals(2, Length(res2));
  CheckEquals([5, 6], res2[0]);
  CheckEquals([8, 9], res2[1]);

  res := TDynAUt.SubArray<Integer>(TArray<Integer>.Create(10, 20, 30, 40), 1, 2);

  CheckEquals([20, 30], res);

  res := TDynAUt.SubArray<Integer>(TArray<Integer>.Create(10, 20, 30), 1, 1);

  CheckEquals([20], res);
end;

procedure TDynArrUtilTests.Permute;
var res: TArray<Integer>;
    rows: TArray<TArray<Integer>>;
begin
  res := TDynAUt.Permute<Integer>(TArray<Integer>.Create(10, 20, 30), [2, 0, 1]);

  CheckEquals([30, 10, 20], res);

  rows := TArray<TArray<Integer>>.Create(
    TArray<Integer>.Create(1, 2),
    TArray<Integer>.Create(3, 4),
    TArray<Integer>.Create(5, 6)
  );

  rows := TDynAUt.Permute<Integer>(rows, [2, 0, 1]);

  CheckEquals([5, 6], rows[0]);
  CheckEquals([1, 2], rows[1]);
  CheckEquals([3, 4], rows[2]);
end;

procedure TDynArrUtilTests.InversePermutation;
var perm, inv: TArray<NativeInt>;
begin
  perm := TArray<NativeInt>.Create(2, 0, 1);

  inv := TDynAUt.InversePermutation(perm);

  CheckNInts([1, 2, 0], inv);
  CheckEquals(0, perm[inv[0]]);
  CheckEquals(1, perm[inv[1]]);
  CheckEquals(2, perm[inv[2]]);

  CheckNInts([2, 0, 1], TDynAUt.InversePermutation(inv));
end;

procedure TDynArrUtilTests.Rotate;
var src, res: TArray<Integer>;
begin
  src := TArray<Integer>.Create(1, 2, 3, 4);

  res := TDynAUt.RotateRight<Integer>(src, 1);

  CheckEquals([4, 1, 2, 3], res);
  CheckEquals([1, 2, 3, 4], src);

  res := TDynAUt.RotateLeft<Integer>(src, 1);

  CheckEquals([2, 3, 4, 1], res);

  res := TDynAUt.RotateRight<Integer>(src, 6);

  CheckEquals([3, 4, 1, 2], res);

  res := TDynAUt.RotateRight<Integer>(src, 0);

  CheckEquals([1, 2, 3, 4], res);

  res := TDynAUt.RotateLeft<Integer>(src, 4);

  CheckEquals([1, 2, 3, 4], res);

  res := TDynAUt.RotateRight<Integer>(nil, 1);

  CheckEquals(0, Length(res));
end;

procedure TDynArrUtilTests.RotateRightManaged;
var src, res: TArray<String>;
begin
  src := TArray<String>.Create('a', 'b', 'c', 'd');

  res := TDynAUt.RotateRight<String>(src, 1);

  CheckEquals('d', res[0]);
  CheckEquals('a', res[1]);
  CheckEquals('b', res[2]);
  CheckEquals('c', res[3]);

  res := TDynAUt.RotateRight<String>(src, -1);
  CheckEquals('b', res[0]);
  CheckEquals('c', res[1]);
  CheckEquals('d', res[2]);
  CheckEquals('a', res[3]);

  res := TDynAUt.RotateRight<String>(src, 4);
  CheckEquals('a', res[0]);
  CheckEquals('b', res[1]);
  CheckEquals('c', res[2]);
  CheckEquals('d', res[3]);
  CheckEquals(0, Length(TDynAUt.RotateRight<String>(nil, 1)));
end;

procedure TDynArrUtilTests.RotateLeftManaged;
var src, res: TArray<String>;
begin
  src := TArray<String>.Create('a', 'b', 'c', 'd');

  res := TDynAUt.RotateLeft<String>(src, 1);

  CheckEquals('b', res[0]);
  CheckEquals('c', res[1]);
  CheckEquals('d', res[2]);
  CheckEquals('a', res[3]);

  res := TDynAUt.RotateLeft<String>(src, -1);
  CheckEquals('d', res[0]);
  CheckEquals('a', res[1]);
  CheckEquals('b', res[2]);
  CheckEquals('c', res[3]);

  res := TDynAUt.RotateLeft<String>(src, 4);
  CheckEquals('a', res[0]);
  CheckEquals('b', res[1]);
  CheckEquals('c', res[2]);
  CheckEquals('d', res[3]);
  CheckEquals(0, Length(TDynAUt.RotateLeft<String>(nil, 1)));
end;

procedure TDynArrUtilTests.MakeHeap;
var data: TArray<Integer>;
begin
  data := TArray<Integer>.Create(4, 1, 3, 2, 8, 0, 5);

  TDynAUt.MakeHeap<Integer>(data);

  CheckEquals(0, data[0]);
  CheckIntHeap(data, Length(data), True);

  data := TArray<Integer>.Create(4, 1, 3, 9);

  TDynAUt.MakeHeap<Integer>(data, nil, 3);

  CheckEquals(1, data[0]);
  CheckEquals(9, data[3]);
  CheckIntHeap(data, 3, True);

  data := TArray<Integer>.Create(4, 1, 3, 2);

  TDynAUt.MakeHeap<Integer>(data, nil, -1, False);

  CheckEquals(4, data[0]);
  CheckIntHeap(data, Length(data), False);

  data := nil;

  TDynAUt.MakeHeap<Integer>(data);

  CheckEquals(0, Length(data));
end;

procedure TDynArrUtilTests.HeapSort;
var data: TArray<Integer>;
begin
  data := TArray<Integer>.Create(4, 1, 3, 2, 2);

  TDynAUt.HeapSort<Integer>(data);

  CheckEquals([1, 2, 2, 3, 4], data);

  data := TArray<Integer>.Create(4, 1, 3, 2);

  TDynAUt.HeapSort<Integer>(data, nil, False);

  CheckEquals([4, 3, 2, 1], data);

  data := TArray<Integer>.Create(2, 1);

  TDynAUt.HeapSort<Integer>(data);

  CheckEquals([1, 2], data);

  data := nil;

  TDynAUt.HeapSort<Integer>(data);

  CheckEquals(0, Length(data));
end;

procedure TDynArrUtilTests.HeapPop;
var data: TArray<Integer>;
    popped: Integer;
begin
  data := TArray<Integer>.Create(4, 1, 3, 2);
  TDynAUt.MakeHeap<Integer>(data);

  popped := TDynAUt.HeapPop<Integer>(data);

  CheckEquals(1, popped);
  CheckEquals(2, data[0]);
  CheckEquals(0, data[3]);
  CheckIntHeap(data, 3, True);
  CheckSortedPrefix(data, 3, [2, 3, 4]);

  data := TArray<Integer>.Create(4, 1, 3, 2);
  TDynAUt.MakeHeap<Integer>(data, nil, -1, False);

  popped := TDynAUt.HeapPop<Integer>(data, nil, -1, False);

  CheckEquals(4, popped);
  CheckEquals(3, data[0]);
  CheckEquals(0, data[3]);
  CheckIntHeap(data, 3, False);
  CheckSortedPrefix(data, 3, [1, 2, 3]);
end;

procedure TDynArrUtilTests.HeapPush;
var data: TArray<Integer>;
begin
  data := TArray<Integer>.Create(4, 1, 3, 99);
  TDynAUt.MakeHeap<Integer>(data, nil, 3);

  TDynAUt.HeapPush<Integer>(data, 0, nil, 3);

  CheckEquals(0, data[0]);
  CheckIntHeap(data, 4, True);
  CheckSortedPrefix(data, 4, [0, 1, 3, 4]);

  data := TArray<Integer>.Create(1, 4, 2, 0);
  TDynAUt.MakeHeap<Integer>(data, nil, 3, False);

  TDynAUt.HeapPush<Integer>(data, 9, nil, 3, False);

  CheckEquals(9, data[0]);
  CheckIntHeap(data, 4, False);
  CheckSortedPrefix(data, 4, [1, 2, 4, 9]);
end;

procedure TDynArrUtilTests.HeapPushFull;
var data: TArray<Integer>;
begin
  ExpectedException := EArgumentOutOfRangeException;
  data := TArray<Integer>.Create(1, 2);

  TDynAUt.HeapPush<Integer>(data, 0, nil, 2);
end;

procedure TDynArrUtilTests.HeapRemove;
var data: TArray<Integer>;
begin
  data := TArray<Integer>.Create(1, 2, 3);
  TDynAUt.MakeHeap<Integer>(data);

  CheckFalse(TDynAUt.HeapRemove<Integer>(data, 9));

  CheckEquals([1, 2, 3], data);

  CheckTrue(TDynAUt.HeapRemove<Integer>(data, 3));

  CheckEquals([1, 2, 0], data);

  data := TArray<Integer>.Create(4, 1, 3, 2);
  TDynAUt.MakeHeap<Integer>(data);

  CheckTrue(TDynAUt.HeapRemove<Integer>(data, 3));

  CheckEquals(0, data[3]);
  CheckIntHeap(data, 3, True);
  CheckSortedPrefix(data, 3, [1, 2, 4]);
end;

procedure TDynArrUtilTests.Take;
var data, res: TArray<Integer>;
    idxs: TIndices;
begin
  data := TArray<Integer>.Create(10, 20, 30, 40, 50);
  SetLength(idxs, 2);
  idxs[0] := 2;
  idxs[1] := 0;

  res := TDynAUt.Take<Integer>(data, idxs);

  CheckEquals([30, 10], res);

  res := TDynAUt.Take<Integer>(data, 2);

  CheckEquals([10, 20], res);

  res := TDynAUt.Take<Integer>(data, -2);

  CheckEquals([40, 50], res);

  res := TDynAUt.Take<Integer>(data, 0);

  CheckEquals(0, Length(res));

  res := TDynAUt.Take<Integer>(data, 5);

  CheckEquals([10, 20, 30, 40, 50], res);
end;

procedure TDynArrUtilTests.Drop;
var data, res: TArray<Integer>;
    idxs: TIndices;
begin
  data := TArray<Integer>.Create(10, 20, 30, 40, 50);
  SetLength(idxs, 2);
  idxs[0] := 1;
  idxs[1] := 3;

  res := TDynAUt.Drop<Integer>(data, idxs);

  CheckEquals([10, 30, 50], res);

  res := TDynAUt.Drop<Integer>(data, 2);

  CheckEquals([30, 40, 50], res);

  res := TDynAUt.Drop<Integer>(data, -2);

  CheckEquals([10, 20, 30], res);

  res := TDynAUt.Drop<Integer>(data, 0);

  CheckEquals([10, 20, 30, 40, 50], res);

  res := TDynAUt.Drop<Integer>(data, 5);

  CheckEquals(0, Length(res));

  res := TDynAUt.Drop<Integer>(data, -5);

  CheckEquals(0, Length(res));

  SetLength(idxs, 3);
  idxs[0] := -1;
  idxs[1] := 1;
  idxs[2] := 9;

  res := TDynAUt.Drop<Integer>(TArray<Integer>.Create(10, 20, 30), idxs);

  CheckEquals([10, 30], res);
end;

procedure TDynArrUtilTests.InsertItem;
var res: TArray<Integer>;
begin
  res := TDynAUt.Insert<Integer>(TArray<Integer>.Create(1, 2, 4), 3, 2);

  CheckEquals([1, 2, 3, 4], res);

  res := TDynAUt.Insert<Integer>(TArray<Integer>.Create(2, 3), 1, 0);

  CheckEquals([1, 2, 3], res);

  res := TDynAUt.Insert<Integer>(TArray<Integer>.Create(1, 2), 3, 2);

  CheckEquals([1, 2, 3], res);

  res := TDynAUt.Insert<Integer>(nil, 1, 0);

  CheckEquals([1], res);
end;

procedure TDynArrUtilTests.InsertList;
var data, res: TArray<Integer>;
begin
  data := TArray<Integer>.Create(10, 20, 30);

  res := TDynAUt.InsertList<Integer>(data, [7, 8], [1, 2]);

  CheckEquals([10, 7, 20, 8, 30], res);

  res := TDynAUt.InsertList<Integer>(data, [7, 8], [0, 0]);

  CheckEquals([7, 8, 10, 20, 30], res);

  res := TDynAUt.InsertList<Integer>(data, [9], [3]);

  CheckEquals([10, 20, 30, 9], res);

  res := TDynAUt.InsertList<Integer>(data, [], []);

  CheckEquals([10, 20, 30], res);
end;

procedure TDynArrUtilTests.Select;
var data, res: TArray<Integer>;
begin
  data := TArray<Integer>.Create(1, 2, 3, 4, 5);

  res := TDynAUt.Select<Integer>(
    data,
    function (const aValue: Integer): Boolean
    begin
      Result := not Odd(aValue);
    end
  );

  CheckEquals([2, 4], res);

  res := TDynAUt.Select<Integer>(
    TArray<Integer>.Create(1, 3, 5),
    function (const aValue: Integer): Boolean
    begin
      Result := not Odd(aValue);
    end
  );

  CheckEquals(0, Length(res));
end;

procedure TDynArrUtilTests.Position;
var pos: TArray<NativeInt>;
begin
  pos := TDynAUt.Position<Integer>(
    TArray<Integer>.Create(1, 2, 3, 4, 5),
    function (const aValue: Integer): Boolean
    begin
      Result := not Odd(aValue);
    end
  );

  CheckNInts([1, 3], pos);

  pos := TDynAUt.Position<Integer>(
    TArray<Integer>.Create(1, 3),
    function (const aValue: Integer): Boolean
    begin
      Result := not Odd(aValue);
    end
  );

  CheckEquals(0, Length(pos));
end;

procedure TDynArrUtilTests.FirstPosition;
var data: TArray<Integer>;
begin
  data := TArray<Integer>.Create(4, 1, 4, 2);

  CheckEquals(1, TDynAUt.FirstPosition<Integer>(data, 1));
  CheckEquals(0, TDynAUt.FirstPosition<Integer>(data, 4));
  CheckEquals(-1, TDynAUt.FirstPosition<Integer>(data, 9));
  CheckEquals(-1, TDynAUt.FirstPosition<Integer>(nil, 1));
end;

procedure TDynArrUtilTests.SortPerm;
var perm: TArray<NativeInt>;
    cmp: TComparison<Integer>;
begin
  perm := TDynAUt.SortPerm<Integer>(TArray<Integer>.Create(3, 1, 2, 1));

  CheckNInts([1, 3, 2, 0], perm);

  cmp := function (const aL, aR: Integer): Integer
  begin
    Result := aR - aL;
  end;

  perm := TDynAUt.SortPerm<Integer>(TArray<Integer>.Create(3, 1, 2), cmp);

  CheckNInts([0, 2, 1], perm);
end;

procedure TDynArrUtilTests.MakeIndexedValues;
var vals: TArray<TIndexedValue<Integer>>;
begin
  vals := TDynAUt.MakeIndexedValues<Integer>([1, 3, 5], [10, 30, 50]);

  CheckEquals(3, Length(vals));
  CheckEquals(1, vals[0].Idx);
  CheckEquals(10, vals[0].Value);
  CheckEquals(3, vals[1].Idx);
  CheckEquals(30, vals[1].Value);
  CheckEquals(5, vals[2].Idx);
  CheckEquals(50, vals[2].Value);
end;

procedure TDynArrUtilTests.SortIndexedValuesByValue;
var vals: TArray<TIndexedValue<Single>>;
begin
  vals := TDynAUt.MakeIndexedValues<Single>([1, 3, 5], [3.0, 2.0, 1.0]);

  TDynAUt.SortIndexedValues<Single>(vals);

  CheckEquals(5, vals[0].Idx);
  CheckEquals(3, vals[1].Idx);
  CheckEquals(1, vals[2].Idx);

  vals := TDynAUt.MakeIndexedValues<Single>([1, 3, 5], [1.0, 2.0, 3.0]);

  TDynAUt.SortIndexedValues<Single>(vals, TComparer<Single>.Construct(
    function (const aL, aR: Single): Integer
    begin
      if aL < aR then
        Result := 1
      else if aL > aR then
        Result := -1
      else
        Result := 0;
    end
  ));

  CheckEquals(5, vals[0].Idx);
  CheckEquals(3, vals[1].Idx);
  CheckEquals(1, vals[2].Idx);
end;

procedure TDynArrUtilTests.SortIndexedValuesByIndex;
var vals: TArray<TIndexedValue<Integer>>;
begin
  vals := TDynAUt.MakeIndexedValues<Integer>([3, 1, 2], [30, 10, 20]);

  TDynAUt.SortIndexedValuesByIndex<Integer>(vals);

  CheckEquals(1, vals[0].Idx);
  CheckEquals(10, vals[0].Value);
  CheckEquals(2, vals[1].Idx);
  CheckEquals(20, vals[1].Value);
  CheckEquals(3, vals[2].Idx);
  CheckEquals(30, vals[2].Value);
end;

procedure TDynArrUtilTests.MergeSortIndexedValuesByIndex;
var vals: TArray<TIndexedValue<Integer>>;
begin
  SetLength(vals, 4);
  vals[0].Init(2, 10);
  vals[1].Init(1, 20);
  vals[2].Init(2, 30);
  vals[3].Init(0, 40);

  TDynAUt.MergeSortIndexedValuesByIndex<Integer>(vals);

  CheckEquals(0, vals[0].Idx);
  CheckEquals(40, vals[0].Value);
  CheckEquals(1, vals[1].Idx);
  CheckEquals(20, vals[1].Value);
  CheckEquals(2, vals[2].Idx);
  CheckEquals(10, vals[2].Value);
  CheckEquals(2, vals[3].Idx);
  CheckEquals(30, vals[3].Value);
end;

procedure TDynArrUtilTests.Map;
var src, res: TArray<Integer>;
    sres: TArray<String>;
    src2: TArray<TArray<Integer>>;
    res2: TArray<TArray<Integer>>;
    sres2: TArray<TArray<String>>;
    dbl: TFnc<Integer, Integer>;
    inc1: TFnc<Integer, Integer>;
    addIdx: TFnc<Integer, Integer, Integer>;
    toStr: TFnc<Integer, String>;
begin
  src := TArray<Integer>.Create(1, 2, 3);
  dbl := function (const aValue: Integer): Integer
  begin
    Result := aValue * 2;
  end;
  inc1 := function (const aValue: Integer): Integer
  begin
    Result := aValue + 1;
  end;
  addIdx := function (const aValue: Integer; const aIdx: Integer): Integer
  begin
    Result := aValue + aIdx;
  end;
  toStr := function (const aValue: Integer): String
  begin
    Result := IntToStr(aValue);
  end;

  res := TDynAUt.Map<Integer, Integer>(src, dbl);

  CheckEquals([2, 4, 6], res);

  res := TDynAUt.Map<Integer>(src, inc1);

  CheckEquals([2, 3, 4], res);

  res := TDynAUt.MapIndexed<Integer, Integer>(src, addIdx);

  CheckEquals([1, 3, 5], res);

  src2 := TArray<TArray<Integer>>.Create(
    TArray<Integer>.Create(1, 2),
    TArray<Integer>.Create(3, 4)
  );

  res2 := TDynAUt.Map2D<Integer>(src2, inc1);

  CheckEquals([2, 3], res2[0]);
  CheckEquals([4, 5], res2[1]);

  sres2 := TDynAUt.Map2D<Integer, String>(src2, toStr);

  CheckEquals(['1', '2'], sres2[0]);
  CheckEquals(['3', '4'], sres2[1]);

  sres := TDynAUt.Map<Integer, String>(nil, toStr);

  CheckEquals(0, Length(sres));
end;

procedure TDynArrUtilTests.Append;
var res: TArray<Integer>;
begin
  res := TDynAUt.Append<Integer>(TArray<Integer>.Create(1, 2), 3);

  CheckEquals([1, 2, 3], res);

  res := TDynAUt.Append<Integer>(nil, 1);

  CheckEquals([1], res);
end;

procedure TDynArrUtilTests.Prepend;
var res: TArray<Integer>;
begin
  res := TDynAUt.Prepend<Integer>(TArray<Integer>.Create(2, 3), 1);

  CheckEquals([1, 2, 3], res);

  res := TDynAUt.Prepend<Integer>(nil, 1);

  CheckEquals([1], res);
end;

procedure TDynArrUtilTests.AppendList;
var res: TArray<Integer>;
begin
  res := TDynAUt.AppendList<Integer>(TArray<Integer>.Create(1, 2), [3, 4]);

  CheckEquals([1, 2, 3, 4], res);

  res := TDynAUt.AppendList<Integer>(TArray<Integer>.Create(1, 2), []);

  CheckEquals([1, 2], res);
end;

procedure TDynArrUtilTests.PrependList;
var res: TArray<Integer>;
begin
  res := TDynAUt.PrependList<Integer>(TArray<Integer>.Create(1, 2), [3, 4]);

  CheckEquals([3, 4, 1, 2], res);

  res := TDynAUt.PrependList<Integer>(nil, [1, 2]);

  CheckEquals([1, 2], res);
end;

procedure TDynArrUtilTests.MemberQ;
var data: TArray<Integer>;
begin
  data := TArray<Integer>.Create(1, 2, 3);

  CheckTrue(TDynAUt.MemberQ<Integer>(data, 2));

  CheckFalse(TDynAUt.MemberQ<Integer>(data, 4));

  data := nil;

  CheckFalse(TDynAUt.MemberQ<Integer>(data, 1));
end;

procedure TDynArrUtilTests.FindMin;
var data: TArray<Integer>;
begin
  data := TArray<Integer>.Create(4, 1, 7, 2);

  CheckEquals(1, TDynAUt.FindMin<Integer>(data));
  CheckEquals(1, TDynAUt.FindMinPos<Integer>(data));

  data := TArray<Integer>.Create(5, 1, 5, 1);

  CheckEquals(1, TDynAUt.FindMin<Integer>(data));
  CheckEquals(1, TDynAUt.FindMinPos<Integer>(data));

  data := TArray<Integer>.Create(4);

  CheckEquals(4, TDynAUt.FindMin<Integer>(data));
  CheckEquals(0, TDynAUt.FindMinPos<Integer>(data));
end;

procedure TDynArrUtilTests.FindMax;
var data: TArray<Integer>;
begin
  data := TArray<Integer>.Create(4, 1, 7, 2);

  CheckEquals(7, TDynAUt.FindMax<Integer>(data));
  CheckEquals(2, TDynAUt.FindMaxPos<Integer>(data));

  data := TArray<Integer>.Create(5, 1, 5, 1);

  CheckEquals(5, TDynAUt.FindMax<Integer>(data));
  CheckEquals(0, TDynAUt.FindMaxPos<Integer>(data));
end;

procedure TDynArrUtilTests.FindMinMax;
var data: TArray<Integer>;
    mn, mx: Integer;
begin
  data := TArray<Integer>.Create(4, 1, 7, 2);

  TDynAUt.FindMinMax<Integer>(data, mn, mx);

  CheckEquals(1, mn);
  CheckEquals(7, mx);
end;

procedure TDynArrUtilTests.FindMinMax_MaxAtFirst;
var data: TArray<Integer>;
    mn, mx: Integer;
begin
  data := TArray<Integer>.Create(5, 1, 3);

  TDynAUt.FindMinMax<Integer>(data, mn, mx);

  CheckEquals(1, mn);
  CheckEquals(5, mx);
end;

procedure TDynArrUtilTests.FindFirstPos;
var data: TArray<Integer>;
begin
  data := TArray<Integer>.Create(1, 3, 4, 6);

  CheckEquals(2, TDynAUt.FindFirstPos<Integer>(
    data,
    function (const aValue: Integer): Boolean
    begin
      Result := not Odd(aValue);
    end
  ));

  CheckEquals(-1, TDynAUt.FindFirstPos<Integer>(
    TArray<Integer>.Create(1, 3),
    function (const aValue: Integer): Boolean
    begin
      Result := not Odd(aValue);
    end
  ));

  CheckEquals(2, TDynAUt.FindFirstPos<Integer>(data, 4));
  CheckEquals(-1, TDynAUt.FindFirstPos<Integer>(data, 9));
end;

procedure TDynArrUtilTests.Where;
var idxs: TArray<Integer>;
    idxs2: TArray<TArrayIndex2D>;
    data: TArray<TArray<Integer>>;
    even: TPredicateFunc<Integer>;
    always: TPredicateFunc<Integer>;
begin
  even := function (const aValue: Integer): Boolean
  begin
    Result := not Odd(aValue);
  end;
  always := function (const aValue: Integer): Boolean
  begin
    Result := True;
  end;

  idxs := TDynAUt.Where<Integer>(TArray<Integer>.Create(1, 2, 3, 4, 5), even);

  CheckEquals([1, 3], idxs);

  data := TArray<TArray<Integer>>.Create(
    TArray<Integer>.Create(1, 2, 3),
    TArray<Integer>.Create(4, 5),
    TArray<Integer>.Create(6, 7, 8, 9)
  );

  idxs2 := TDynAUt.Where<Integer>(data, even);

  CheckEquals(4, Length(idxs2));
  CheckEquals(0, idxs2[0].Row);
  CheckEquals(1, idxs2[0].Col);
  CheckEquals(1, idxs2[1].Row);
  CheckEquals(0, idxs2[1].Col);
  CheckEquals(2, idxs2[2].Row);
  CheckEquals(0, idxs2[2].Col);
  CheckEquals(2, idxs2[3].Row);
  CheckEquals(2, idxs2[3].Col);

  data := nil;

  idxs2 := TDynAUt.Where<Integer>(data, always);

  CheckEquals(0, Length(idxs2));
end;

procedure TDynArrUtilTests.Median;
var data: TArray<Integer>;
begin
  data := TArray<Integer>.Create(3, 1, 2);

  CheckEquals(2, TDynAUt.Median<Integer>(data));
  CheckEquals([3, 1, 2], data);

  CheckEquals(3, TDynAUt.Median<Integer>(TArray<Integer>.Create(1, 3, 2, 4)));
  CheckEquals(8, TDynAUt.Median<Integer>(TArray<Integer>.Create(8)));
end;

procedure TDynArrUtilTests.GatherBy;
var groups: TArray<TArray<Integer>>;
    data: TArray<Integer>;
    absKey: TFnc<Integer, Integer>;
begin
  absKey := function (const aValue: Integer): Integer
  begin
    Result := Abs(aValue);
  end;

  data := TArray<Integer>.Create(1, 3, 2, 1, 2);

  groups := TDynAUt.GatherBy<Integer>(data);

  CheckEquals(3, Length(groups));
  CheckEquals(2, Length(groups[0]));
  CheckEquals(1, groups[0, 0]);
  CheckEquals(1, groups[0, 1]);
  CheckEquals(2, Length(groups[1]));
  CheckEquals(2, groups[1, 0]);
  CheckEquals(2, groups[1, 1]);
  CheckEquals(1, Length(groups[2]));
  CheckEquals(3, groups[2, 0]);

  groups := TDynAUt.GatherBy<Integer, Integer>(
    TArray<Integer>.Create(-1, 2, -3, 2),
    absKey
  );

  CheckEquals(3, Length(groups));
  CheckEquals(1, Length(groups[0]));
  CheckEquals(-1, groups[0, 0]);
  CheckEquals(2, Length(groups[1]));
  CheckEquals(2, groups[1, 0]);
  CheckEquals(2, groups[1, 1]);
  CheckEquals(1, Length(groups[2]));
  CheckEquals(-3, groups[2, 0]);

  data := nil;

  groups := TDynAUt.GatherBy<Integer>(data);

  CheckEquals(0, Length(groups));
end;

procedure TDynArrUtilTests.SplitBy;
var parts: TArray<TArray<Integer>>;
begin
  parts := TDynAUt.SplitBy<Integer>(TArray<Integer>.Create(1, 1, 2, 2, 2, 1));

  CheckEquals(3, Length(parts));
  CheckEquals([1, 1], parts[0]);
  CheckEquals([2, 2, 2], parts[1]);
  CheckEquals([1], parts[2]);

  parts := TDynAUt.SplitBy<Integer>(TArray<Integer>.Create(4, 4));

  CheckEquals(1, Length(parts));
  CheckEquals([4, 4], parts[0]);

  parts := TDynAUt.SplitBy<Integer>(nil);

  CheckEquals(0, Length(parts));
end;

procedure TDynArrUtilTests.MovingMap;
var res: TArray<Integer>;
begin
  res := TDynAUt.MovingMap<Integer, Integer>(
    TArray<Integer>.Create(1, 2, 3, 4),
    function (const aWin: TArray<Integer>): Integer
    var I, s: Integer;
    begin
      s := 0;
      for I := 0 to High(aWin) do
        s := s + aWin[I];
      Result := s;
    end,
    2
  );

  CheckEquals([3, 5, 7], res);

  res := TDynAUt.MovingMap<Integer, Integer>(
    TArray<Integer>.Create(1, 2, 3, 4),
    function (const aWin: TArray<Integer>): Integer
    var I, s: Integer;
    begin
      s := 0;
      for I := 0 to High(aWin) do
        s := s + aWin[I];
      Result := s;
    end,
    4
  );

  CheckEquals([10], res);
end;

procedure TDynArrUtilTests.MovingMapTooWide;
begin
  ExpectedException := EInvalidArgument;

  TDynAUt.MovingMap<Integer, Integer>(
    TArray<Integer>.Create(1, 2, 3),
    function (const aWin: TArray<Integer>): Integer
    begin
      Result := aWin[0];
    end,
    5
  );
end;

procedure TDynArrUtilTests.ToMListString;
var s: String;
    m: TArray<TArray<Double>>;
begin
  s := TDynAUt.ToMListString<Double>(TArray<Double>.Create(1.5, 2.25));

  CheckEquals('{1.5, 2.25}', s);

  m := TArray<TArray<Double>>.Create(
    TArray<Double>.Create(1.5, 2.25),
    TArray<Double>.Create(3.5, 4.5)
  );

  s := TDynAUt.ToMListString<Double>(m);

  CheckEquals('{{1.5,2.25},{3.5,4.5}}', s);
end;

procedure TDynArrUtilTests.Riffle;
var res: TArray<Integer>;
begin
  res := TDynAUt.Riffle<Integer>(
    TArray<Integer>.Create(1, 2, 3),
    TArray<Integer>.Create(4, 5, 6)
  );

  CheckEquals([1, 4, 2, 5, 3, 6], res);
end;

procedure TDynArrUtilTests.RiffleDifferentLengths;
var res: TArray<Integer>;
begin
  res := TDynAUt.Riffle<Integer>(
    TArray<Integer>.Create(1, 2, 3, 4),
    TArray<Integer>.Create(8, 9)
  );

  CheckEquals([1, 8, 2, 9, 3, 8, 4], res);

  res := TDynAUt.Riffle<Integer>(
    TArray<Integer>.Create(1, 2),
    TArray<Integer>.Create(8, 9, 10)
  );

  CheckEquals([1, 8, 2], res);

  res := TDynAUt.Riffle<Integer>(TArray<Integer>.Create(1, 2), nil);

  CheckEquals([1, 2], res);
end;

procedure TDynArrUtilTests.ToArray;
var res: TArray<Integer>;
begin
  res := TDynAUt.ToArray<Integer>([1, 2, 3]);

  CheckEquals([1, 2, 3], res);
  CheckEquals(0, Length(TDynAUt.ToArray<Integer>([])));
end;

procedure TDynArrUtilTests.ConstantArray;
var res: TArray<Integer>;
    sres: TArray<String>;
begin
  res := TDynAUt.ConstantArray<Integer>(7, 3);

  CheckEquals([7, 7, 7], res);

  sres := TDynAUt.ConstantArray<String>('a', 2);

  CheckEquals(['a', 'a'], sres);
end;

procedure TDynArrUtilTests.ItemCount;
var data: TArray<TArray<Integer>>;
begin
  data := TArray<TArray<Integer>>.Create(
    TArray<Integer>.Create(1, 2),
    nil,
    TArray<Integer>.Create(3)
  );

  CheckEquals(3, TDynAUt.ItemCount<Integer>(data));
  CheckEquals(0, TDynAUt.ItemCount<Integer>(nil));
end;

procedure TDynArrUtilTests.MaxLength;
var data: TArray<TArray<Integer>>;
begin
  data := TArray<TArray<Integer>>.Create(
    nil,
    TArray<Integer>.Create(1, 2, 3),
    TArray<Integer>.Create(1, 2)
  );

  CheckEquals(3, TDynAUt.MaxLength<Integer>(data));
  CheckEquals(0, TDynAUt.MaxLength<Integer>(nil));
end;

procedure TDynArrUtilTests.PositiveQ;
var empty: TArray<Integer>;
begin
  CheckTrue(TDynAUt.PositiveQ([1, 2, 3]));

  CheckFalse(TDynAUt.PositiveQ([1, 0, 3]));

  CheckFalse(TDynAUt.PositiveQ([-1]));

  CheckTrue(TDynAUt.PositiveQ(empty));
end;

procedure TDynArrUtilTests.Int2NInt;
begin
  CheckNInts([1, -2, 3], TDynAUt.Int2NInt([1, -2, 3]));
  CheckEquals(0, Length(TDynAUt.Int2NInt([])));
end;

procedure TDynArrUtilTests.Range;
begin
  CheckNInts([1, 2, 3, 4, 5], TDynAUt.Range(NativeInt(1), NativeInt(5)));
  CheckNInts([1, 3, 5], TDynAUt.Range(NativeInt(1), NativeInt(5), NativeInt(2)));
  CheckNInts([5, 4, 3, 2, 1], TDynAUt.Range(NativeInt(5), NativeInt(1), NativeInt(-1)));
  CheckNInts([5, 3, 1], TDynAUt.Range(NativeInt(5), NativeInt(1), NativeInt(-2)));
  CheckEquals(0, Length(TDynAUt.Range(NativeInt(5), NativeInt(1))));
  CheckNInts([0], TDynAUt.Range(NativeInt(0), NativeInt(0)));

  CheckEquals([1, 2, 3, 4, 5], TDynAUt.Range_I32(1, 5));
  CheckEquals([1, 3, 5], TDynAUt.Range_I32(1, 5, 2));
  CheckEquals([5, 4, 3, 2, 1], TDynAUt.Range_I32(5, 1, -1));
  CheckEquals(0, Length(TDynAUt.Range_I32(5, 1)));

  CheckEquals([1, 2, 3], TDynAUt.Range(1.0, 3.0));
  CheckEquals([0, 0.5, 1, 1.5, 2], TDynAUt.Range(0.0, 2.0, 0.5));
  CheckEquals([2, 1, 0], TDynAUt.Range(2.0, 0.0, -1.0));
  CheckEquals(0, Length(TDynAUt.Range(2.0, 0.0, 1.0)));

  CheckEquals([0, 0.5, 1], TDynAUt.Range_F32(0, 1, 0.5));
  CheckEquals([3, 2, 1], TDynAUt.Range_F32(3, 1, -1));
  CheckEquals(0, Length(TDynAUt.Range_F32(2, 0, 1)));
end;

procedure TDynArrUtilTests.CaseConvert;
begin
  CheckEquals(['ABC', 'X'], TDynAUt.ToUpper(['AbC', 'x']));

  CheckEquals(['abc', 'x'], TDynAUt.ToLower(['AbC', 'X']));
end;

procedure TDynArrUtilTests.MatrixShape;
var a: TArray<TArray<Integer>>;
begin
  a := TArray<TArray<Integer>>.Create(
    TArray<Integer>.Create(1, 2),
    TArray<Integer>.Create(3, 4)
  );

  CheckTrue(TDynAUt.MatrixQ<Integer>(a));
  CheckTrue(TDynAUt.SquareMatrixQ<Integer>(a));

  a := TArray<TArray<Integer>>.Create(
    TArray<Integer>.Create(1, 2, 3),
    TArray<Integer>.Create(4, 5, 6)
  );

  CheckTrue(TDynAUt.MatrixQ<Integer>(a));
  CheckFalse(TDynAUt.SquareMatrixQ<Integer>(a));

  a := TArray<TArray<Integer>>.Create(
    TArray<Integer>.Create(1, 2),
    TArray<Integer>.Create(3)
  );

  CheckFalse(TDynAUt.MatrixQ<Integer>(a));
  CheckFalse(TDynAUt.SquareMatrixQ<Integer>(a));

  a := nil;

  CheckFalse(TDynAUt.MatrixQ<Integer>(a));
  CheckFalse(TDynAUt.SquareMatrixQ<Integer>(a));
end;

procedure TDynArrUtilTests.Triangularize;
var a, low, up: TArray<TArray<Integer>>;
begin
  a := TArray<TArray<Integer>>.Create(
    TArray<Integer>.Create(1, 2, 3),
    TArray<Integer>.Create(4, 5, 6),
    TArray<Integer>.Create(7, 8, 9)
  );

  low := TDynAUt.LowerTriangularize<Integer>(a);

  CheckEquals([1, 0, 0], low[0]);
  CheckEquals([4, 5, 0], low[1]);
  CheckEquals([7, 8, 9], low[2]);

  low := TDynAUt.LowerTriangularize<Integer>(a, -1);

  CheckEquals([0, 0, 0], low[0]);
  CheckEquals([4, 0, 0], low[1]);
  CheckEquals([7, 8, 0], low[2]);

  up := TDynAUt.UpperTriangularize<Integer>(a);

  CheckEquals([1, 2, 3], up[0]);
  CheckEquals([0, 5, 6], up[1]);
  CheckEquals([0, 0, 9], up[2]);

  up := TDynAUt.UpperTriangularize<Integer>(a, 1);

  CheckEquals([0, 2, 3], up[0]);
  CheckEquals([0, 0, 6], up[1]);
  CheckEquals([0, 0, 0], up[2]);
end;

procedure TDynArrUtilTests.SetDiagonal;
var a: TArray<TArray<Integer>>;
begin
  a := TArray<TArray<Integer>>.Create(
    TArray<Integer>.Create(1, 2, 3),
    TArray<Integer>.Create(4, 5, 6),
    TArray<Integer>.Create(7, 8, 9)
  );

  TDynAUt.SetDiagonal<Integer>(a, 0);

  CheckEquals([0, 2, 3], a[0]);
  CheckEquals([4, 0, 6], a[1]);
  CheckEquals([7, 8, 0], a[2]);

  a := TArray<TArray<Integer>>.Create(
    TArray<Integer>.Create(1, 2, 3),
    TArray<Integer>.Create(4, 5, 6),
    TArray<Integer>.Create(7, 8, 9)
  );

  TDynAUt.SetDiagonal<Integer>(a, TArray<Integer>.Create(9, 8));

  CheckEquals([9, 2, 3], a[0]);
  CheckEquals([4, 8, 6], a[1]);
  CheckEquals([7, 8, 9], a[2]);
end;

procedure TDynArrUtilTests.Transpose;
var a, t: TArray<TArray<Integer>>;
begin
  a := TArray<TArray<Integer>>.Create(
    TArray<Integer>.Create(1, 2, 3),
    TArray<Integer>.Create(4, 5, 6)
  );

  t := TDynAUt.Transpose<Integer>(a);

  CheckEquals(3, Length(t));
  CheckEquals([1, 4], t[0]);
  CheckEquals([2, 5], t[1]);
  CheckEquals([3, 6], t[2]);
end;

procedure TDynArrUtilTests.Augment;
var a, b, c: TArray<TArray<Integer>>;
begin
  a := TArray<TArray<Integer>>.Create(
    TArray<Integer>.Create(1, 2),
    TArray<Integer>.Create(3, 4)
  );
  b := TArray<TArray<Integer>>.Create(
    TArray<Integer>.Create(5),
    TArray<Integer>.Create(6)
  );

  c := TDynAUt.Augment<Integer>(a, b);

  CheckEquals([1, 2, 5], c[0]);
  CheckEquals([3, 4, 6], c[1]);
end;

{$endregion}

initialization

  RegisterTest(TMergeSortTests.Suite);
  RegisterTest(TDynArrUtilTests.Suite);

end.
