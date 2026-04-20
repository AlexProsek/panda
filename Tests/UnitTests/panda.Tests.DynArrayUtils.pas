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
  published
    procedure SortIndexedValuesByValue;
  end;

implementation


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

procedure TDynArrUtilTests.SortIndexedValuesByValue;
var vals: TArray<TIndexedValue<Single>>;
begin
  vals := TDynAUt.MakeIndexedValues<Single>([1, 3, 5], [3.0, 2.0, 1.0]);

  TDynAUt.SortIndexedValues<Single>(vals);

  CheckEquals(5, vals[0].Idx);
  CheckEquals(3, vals[1].Idx);
  CheckEquals(1, vals[2].Idx);
end;

{$endregion}

initialization

  RegisterTest(TMergeSortTests.Suite);
  RegisterTest(TDynArrUtilTests.Suite);

end.
