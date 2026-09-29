unit panda.Filters.Tests.OrdStatFilters;

interface

uses
    TestFramework
  , panda.Intfs
  , panda.Arrays
  , panda.ArrManip
  , panda.DynArrayUtils
  , panda.Filters.OrderStatFilters
  , panda.Tests.NDATestCase
  , System.Generics.Collections
  , System.Math
  ;

type
  TMinFilter1DUI8Tests = class(TNDATestCase)
  protected
    fFilter: TMinFilter1DUI8;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure MinFilter_N5_R1;
    procedure MinFilter_N10_R2;
  end;

  TMinFilter2DUI8Tests = class(TNDATestCase)
  protected
    fFilter: TBoxMinFilter2DUI8;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure MinFilter_N5x5_R1;
    procedure MinFilter_N5x5_R1_HGW;
  end;

  TMaxFilter2DUI8Tests = class(TNDATestCase)
  protected
    fFilter: TBoxMaxFilter2DUI8;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure MaxFilter_N5x5_R1;
    procedure MaxFilter_N5x5_R1_HGW;
  end;

  TMedianFilter1DUI8Tests = class(TNDATestCase)
  protected
    fFilter: TMedianFilter1DUI8;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure MedianFilter_N5_R1;
    procedure MedianFilter_N10_R2;
  end;

  TMedianFilter1DF64Tests = class(TNDATestCase)
  protected const
    cTol = 1e-10;
  protected
    fFilter: TMedianFilter1DF64;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure MedianFilter_N5_R1;
    procedure MedianFilter_N10_R2;
  end;

  TMedianFilter2DUI8Tests = class(TNDATestCase)
  protected const
    cTol = 1e-10;
  protected
    fFilter: TMedianFilter2DUI8;
    function RefMedianFilter(const aArr: INDArray<Byte>;
      aRx, aRy: Integer): TArray<TArray<Byte>>;
    function CreateTestMat(aRowCount, aColCount: Integer): INDArray<Byte>;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure MedianFilter_N5x6_R1;
    procedure MedianFilter_N7x8_R2;
    procedure MedianFilter_N7x8_R1_R2;
    procedure MedianFilter_N30x25_R1;
    procedure MedianFilter_N30x25_R5;
  end;

  TLocEqFilter2DUI8Tests = class(TNDATestCase)
  protected
    fFilter: TLocEqFilter2DUI8;
    function RefLocEqFilter(const aArr: INDArray<Byte>;
      aRx, aRy: Integer): TArray<TArray<Byte>>;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure LocEqFilter_N5x6_R1;
    procedure LocEqFilter_N7x8_R2;
    procedure LocEqFilter_N7x8_R1_R2;
    procedure LocEqFilter_N30x25_R1;
    procedure LocEqFilter_N30x25_R5;
  end;

implementation

{$region 'TMinFilter1DUI8Tests'}

procedure TMinFilter1DUI8Tests.SetUp;
begin
  inherited;
  fFilter := TMinFilter1DUI8.Create;
end;

procedure TMinFilter1DUI8Tests.TearDown;
begin
  inherited;
  fFilter.Free;
end;

procedure TMinFilter1DUI8Tests.MinFilter_N5_R1;
var x, res: TArray<Byte>;
begin
  x := TArray<Byte>.Create(5, 4, 3, 2, 5);
  SetLength(res, Length(x));

  fFilter.Radius := 1;
  fFilter.Execute(PByte(x), PByte(res), Length(x));

  CheckEquals([4, 3, 2, 2, 2], res);
end;

procedure TMinFilter1DUI8Tests.MinFilter_N10_R2;
var x, res: TArray<Byte>;
begin
  x := TArray<Byte>.Create(5, 4, 3, 2, 5, 6, 5, 4, 3, 1);
  SetLength(res, Length(x));

  fFilter.Radius := 2;
  fFilter.Execute(PByte(x), PByte(res), Length(x));

  CheckEquals([3, 2, 2, 2, 2, 2, 3, 1, 1, 1], res);
end;

{$endregion}

{$region 'TMinFilter2DUI8Tests'}

procedure TMinFilter2DUI8Tests.SetUp;
begin
  inherited;
  fFilter := TBoxMinFilter2DUI8.Create;
end;

procedure TMinFilter2DUI8Tests.TearDown;
begin
  inherited;
  fFilter.Free;
end;

procedure TMinFilter2DUI8Tests.MinFilter_N5x5_R1;
var arr: TArray<TArray<Byte>>;
    src, dst: INDArray<Byte>;
    I, J: Integer;
begin
  SetLength(arr, 5, 5);
  for I := 0 to 4 do
    for J := 0 to 4 do
      arr[I, J] := 5*I + J;
  src := TNDAUt.AsArray<Byte>(arr);
  dst := TNDAUt.Empty<Byte>([5, 5]);

  fFilter.HRadius := 1;
  fFilter.VRadius := 1;
  fFilter.Execute(src.Data, dst.Data, src.Strides[0], dst.Strides[0], 5, 5);

  CheckTrue(TNDAUt.TryAsDynArray2D<Byte>(dst, arr));

  CheckEquals([ 0,  0,  1,  2,  3], arr[0]);
  CheckEquals([ 0,  0,  1,  2,  3], arr[1]);
  CheckEquals([ 5,  5,  6,  7,  8], arr[2]);
  CheckEquals([10, 10, 11, 12, 13], arr[3]);
  CheckEquals([15, 15, 16, 17, 18], arr[4]);
end;

procedure TMinFilter2DUI8Tests.MinFilter_N5x5_R1_HGW;
var arr: TArray<TArray<Byte>>;
    src, dst: INDArray<Byte>;
    I, J: Integer;
begin
  SetLength(arr, 5, 5);
  for I := 0 to 4 do
    for J := 0 to 4 do
      arr[I, J] := 5*I + J;
  src := TNDAUt.AsArray<Byte>(arr);
  dst := TNDAUt.Empty<Byte>([5, 5]);

  fFilter.HRadius := 1;
  fFilter.VRadius := 1;
  fFilter.UseHGWMethod := True;
  fFilter.Execute(src.Data, dst.Data, src.Strides[0], dst.Strides[0], 5, 5);

  CheckTrue(TNDAUt.TryAsDynArray2D<Byte>(dst, arr));
  CheckEquals([ 0,  0,  1,  2,  3], arr[0]);
  CheckEquals([ 0,  0,  1,  2,  3], arr[1]);
  CheckEquals([ 5,  5,  6,  7,  8], arr[2]);
  CheckEquals([10, 10, 11, 12, 13], arr[3]);
  CheckEquals([15, 15, 16, 17, 18], arr[4]);
end;

{$endregion}

{$region 'TMaxFilter2DUI8Tests'}

procedure TMaxFilter2DUI8Tests.SetUp;
begin
  inherited;
  fFilter := TBoxMaxFilter2DUI8.Create;
end;

procedure TMaxFilter2DUI8Tests.TearDown;
begin
  inherited;
  fFilter.Free;
end;

procedure TMaxFilter2DUI8Tests.MaxFilter_N5x5_R1;
var arr: TArray<TArray<Byte>>;
    src, dst: INDArray<Byte>;
    I, J: Integer;
begin
  SetLength(arr, 5, 5);
  for I := 0 to 4 do
    for J := 0 to 4 do
      arr[I, J] := 5*I + J;
  src := TNDAUt.AsArray<Byte>(arr);
  dst := TNDAUt.Empty<Byte>([5, 5]);

  fFilter.HRadius := 1;
  fFilter.VRadius := 1;
  fFilter.Execute(src.Data, dst.Data, src.Strides[0], dst.Strides[0], 5, 5);

  CheckTrue(TNDAUt.TryAsDynArray2D<Byte>(dst, arr));
  CheckEquals([ 6,  7,  8,  9,  9], arr[0]);
  CheckEquals([11, 12, 13, 14, 14], arr[1]);
  CheckEquals([16, 17, 18, 19, 19], arr[2]);
  CheckEquals([21, 22, 23, 24, 24], arr[3]);
  CheckEquals([21, 22, 23, 24, 24], arr[4]);
end;

procedure TMaxFilter2DUI8Tests.MaxFilter_N5x5_R1_HGW;
var arr: TArray<TArray<Byte>>;
    src, dst: INDArray<Byte>;
    I, J: Integer;
begin
  SetLength(arr, 5, 5);
  for I := 0 to 4 do
    for J := 0 to 4 do
      arr[I, J] := 5*I + J;
  src := TNDAUt.AsArray<Byte>(arr);
  dst := TNDAUt.Empty<Byte>([5, 5]);

  fFilter.HRadius := 1;
  fFilter.VRadius := 1;
  fFilter.UseHGWMethod := True;
  fFilter.Execute(src.Data, dst.Data, src.Strides[0], dst.Strides[0], 5, 5);

  CheckTrue(TNDAUt.TryAsDynArray2D<Byte>(dst, arr));
  CheckEquals([ 6,  7,  8,  9,  9], arr[0]);
  CheckEquals([11, 12, 13, 14, 14], arr[1]);
  CheckEquals([16, 17, 18, 19, 19], arr[2]);
  CheckEquals([21, 22, 23, 24, 24], arr[3]);
  CheckEquals([21, 22, 23, 24, 24], arr[4]);
end;

{$endregion}

{$region 'TMedianFilter1DUI8Tests'}

procedure TMedianFilter1DUI8Tests.SetUp;
begin
  inherited;
  fFilter := TMedianFilter1DUI8.Create;
end;

procedure TMedianFilter1DUI8Tests.TearDown;
begin
  fFilter.Free;
  inherited;
end;

procedure TMedianFilter1DUI8Tests.MedianFilter_N5_R1;
var x, y: TArray<Byte>;
begin
  x := TArray<Byte>.Create(1, 2, 3, 2, 1);
  SetLength(y, Length(x));

  fFilter.Radius := 1;
  fFilter.Execute(PByte(x), PByte(y), Length(x));

  CheckEquals([2, 2, 2, 2, 2], y);
end;

procedure TMedianFilter1DUI8Tests.MedianFilter_N10_R2;
var x, y: TArray<Byte>;
begin
  x := TArray<Byte>.Create(1, 2, 3, 4, 3, 2, 1, 4, 5, 6);
  SetLength(y, Length(x));

  fFilter.Radius := 2;
  fFilter.Execute(PByte(x), PByte(y), Length(x));

  CheckEquals([2, 3, 3, 3, 3, 3, 3, 4, 5, 5], y);
end;

{$endregion}

{$region 'TMedianFilter1DF64Tests'}

procedure TMedianFilter1DF64Tests.SetUp;
begin
  inherited;
  fFilter := TMedianFilter1DF64.Create;
end;

procedure TMedianFilter1DF64Tests.TearDown;
begin
  fFilter.Free;
  inherited;
end;

procedure TMedianFilter1DF64Tests.MedianFilter_N5_R1;
var x, y: TArray<Double>;
begin
  x := TArray<Double>.Create(1, 2, 3, 2, 1);
  SetLength(y, Length(x));

  fFilter.Radius := 1;
  fFilter.Execute(PByte(x), PByte(y), Length(x));

  CheckEquals([1.5, 2, 2, 2, 1.5], y, cTol);
end;

procedure TMedianFilter1DF64Tests.MedianFilter_N10_R2;
var x, y: TArray<Double>;
begin
  x := TArray<Double>.Create(1, 2, 3, 4, 3, 2, 1, 4, 5, 6);
  SetLength(y, Length(x));

  fFilter.Radius := 2;
  fFilter.Execute(PByte(x), PByte(y), Length(x));

  CheckEquals([2, 5/2, 3, 3, 3, 3, 3, 4, 9/2, 5], y, cTol);
end;

{$endregion}

{$region 'TMedianFilter2DUI8Tests'}

procedure TMedianFilter2DUI8Tests.SetUp;
begin
  inherited;
  fFilter := TMedianFilter2DUI8.Create;
end;

procedure TMedianFilter2DUI8Tests.TearDown;
begin
  fFilter.Free;
  inherited;
end;

function TMedianFilter2DUI8Tests.RefMedianFilter(const aArr: INDArray<Byte>; aRx, aRy: Integer): TArray<TArray<Byte>>;
var w, h, I, J: Integer;
    k: INDArray<Byte>;
    kItems: TArray<Byte>;
begin
  w := aArr.Shape[1];
  h := aArr.Shape[0];
  SetLength(Result, h, w);

  for I := 0 to h - 1 do
    for J := 0 to w - 1 do begin
      k := aArr[[NDISpan(Max(0, I-aRy), Min(h-1, I+aRy)), NDISpan(Max(0, J-aRx), Min(w-1, J+aRx))]];
      TNDAUt.TryAsDynArray<Byte>(TNDAMan.Flatten<Byte>(k), kItems);
      TArray.Sort<Byte>(kItems);
      Result[I, J] := kItems[Length(kItems) div 2];
    end;
end;

function TMedianFilter2DUI8Tests.CreateTestMat(aRowCount, aColCount: Integer): INDArray<Byte>;
begin
  Result := TNDAUt.Table2D<Byte>(
    function (R, C: NativeInt): Byte
    begin
      Result := Byte(R * aColCount + C);
    end,
    0, aColCount - 1, 0, aRowCount - 1
  );
end;

procedure TMedianFilter2DUI8Tests.MedianFilter_N5x6_R1;
var arr, res: INDArray<Byte>;
    expected, resMat: TArray<TArray<Byte>>;
    I, J: Integer;
const
  h = 5;
  w = 6;
  R = 1;
begin
  arr := CreateTestMat(h, w);
  expected := RefMedianFilter(arr, R, R);
  res := TNDAUt.Empty<Byte>(arr.Shape);

  fFilter.HRadius := R;
  fFilter.VRadius := R;
  fFilter.Execute(arr.Data, res.Data, arr.Strides[0], res.Strides[0], w, h);

  TNDAUt.TryAsDynArray2D<Byte>(res, resMat);
  for I := 0 to h - 1 do
    for J := 0 to w - 1 do
      CheckEquals(expected[I, J], resMat[I, J]);
end;

procedure TMedianFilter2DUI8Tests.MedianFilter_N7x8_R2;
var arr, res: INDArray<Byte>;
    expected, resMat: TArray<TArray<Byte>>;
    I, J: Integer;
const
  h = 7;
  w = 8;
  R = 2;
begin
  arr := CreateTestMat(h, w);
  expected := RefMedianFilter(arr, R, R);
  res := TNDAUt.Empty<Byte>(arr.Shape);

  fFilter.HRadius := R;
  fFilter.VRadius := R;
  fFilter.Execute(arr.Data, res.Data, arr.Strides[0], res.Strides[0], w, h);

  TNDAUt.TryAsDynArray2D<Byte>(res, resMat);
  for I := 0 to h - 1 do
    for J := 0 to w - 1 do
      CheckEquals(expected[I, J], resMat[I, J]);
end;

procedure TMedianFilter2DUI8Tests.MedianFilter_N7x8_R1_R2;
var arr, res: INDArray<Byte>;
    expected, resMat: TArray<TArray<Byte>>;
    I, J: Integer;
const
  h = 7;
  w = 8;
begin
  arr := CreateTestMat(h, w);
  expected := RefMedianFilter(arr, 1, 2);
  res := TNDAUt.Empty<Byte>(arr.Shape);

  fFilter.HRadius := 1;
  fFilter.VRadius := 2;
  fFilter.Execute(arr.Data, res.Data, arr.Strides[0], res.Strides[0], w, h);

  TNDAUt.TryAsDynArray2D<Byte>(res, resMat);
  for I := 0 to h - 1 do
    for J := 0 to w - 1 do
      CheckEquals(expected[I, J], resMat[I, J]);
end;

procedure TMedianFilter2DUI8Tests.MedianFilter_N30x25_R1;
var arr, res: INDArray<Byte>;
    expected, resMat: TArray<TArray<Byte>>;
    I, J: Integer;
const
  h = 30;
  w = 25;
  R = 1;
begin
  arr := CreateTestMat(h, w);
  expected := RefMedianFilter(arr, R, R);
  res := TNDAUt.Empty<Byte>(arr.Shape);

  fFilter.HRadius := R;
  fFilter.VRadius := R;
  fFilter.Execute(arr.Data, res.Data, arr.Strides[0], res.Strides[0], w, h);

  TNDAUt.TryAsDynArray2D<Byte>(res, resMat);
  for I := 0 to h - 1 do
    for J := 0 to w - 1 do
      CheckEquals(expected[I, J], resMat[I, J]);
end;

procedure TMedianFilter2DUI8Tests.MedianFilter_N30x25_R5;
var arr, res: INDArray<Byte>;
    expected, resMat: TArray<TArray<Byte>>;
    I, J: Integer;
const
  h = 30;
  w = 25;
  R = 5;
begin
  arr := CreateTestMat(h, w);
  expected := RefMedianFilter(arr, R, R);
  res := TNDAUt.Empty<Byte>(arr.Shape);

  fFilter.HRadius := R;
  fFilter.VRadius := R;
  fFilter.Execute(arr.Data, res.Data, arr.Strides[0], res.Strides[0], w, h);

  TNDAUt.TryAsDynArray2D<Byte>(res, resMat);
  for I := 0 to h - 1 do
    for J := 0 to w - 1 do
      CheckEquals(expected[I, J], resMat[I, J]);
end;

{$endregion}

{$region 'TLocEqFilter2DUI8Tests'}

procedure TLocEqFilter2DUI8Tests.SetUp;
begin
  inherited;
  fFilter := TLocEqFilter2DUI8.Create;
end;

procedure TLocEqFilter2DUI8Tests.TearDown;
begin
  fFilter.Free;
  inherited;
end;

function TLocEqFilter2DUI8Tests.RefLocEqFilter(const aArr: INDArray<Byte>;
  aRx, aRy: Integer): TArray<TArray<Byte>>;
var w, h, I, J, R, C, Count, CDF: Integer;
    mat: TArray<TArray<Byte>>;
    Center: Byte;
begin
  w := aArr.Shape[1];
  h := aArr.Shape[0];
  TNDAUt.TryAsDynArray2D<Byte>(aArr, mat);
  SetLength(Result, h, w);

  for I := 0 to h - 1 do
    for J := 0 to w - 1 do begin
      Center := mat[I, J];
      Count := 0;
      CDF := 0;
      for R := Max(0, I-aRy) to Min(h-1, I+aRy) do
        for C := Max(0, J-aRx) to Min(w-1, J+aRx) do begin
          Inc(Count);
          if mat[R, C] <= Center then
            Inc(CDF);
        end;
      Result[I, J] := Round(255 * CDF / Count);
    end;
end;

procedure TLocEqFilter2DUI8Tests.LocEqFilter_N5x6_R1;
var arr, res: INDArray<Byte>;
    expected, resMat: TArray<TArray<Byte>>;
    I, J: Integer;
const
  h = 5;
  w = 6;
begin
  arr := TNDAUt.Table2D<Byte>(
    function (R, C: NativeInt): Byte
    begin
      Result := Byte((R * 43 + C * 29) mod 256);
    end,
    0, w - 1, 0, h - 1
  );
  expected := RefLocEqFilter(arr, 1, 1);
  res := TNDAUt.Empty<Byte>(arr.Shape);

  fFilter.HRadius := 1;
  fFilter.VRadius := 1;
  fFilter.Execute(arr.Data, res.Data, arr.Strides[0], res.Strides[0], w, h);

  TNDAUt.TryAsDynArray2D<Byte>(res, resMat);
  for I := 0 to h - 1 do
    for J := 0 to w - 1 do
      CheckEquals(expected[I, J], resMat[I, J]);
end;

procedure TLocEqFilter2DUI8Tests.LocEqFilter_N7x8_R2;
var arr, res: INDArray<Byte>;
    expected, resMat: TArray<TArray<Byte>>;
    I, J: Integer;
const
  h = 7;
  w = 8;
begin
  arr := TNDAUt.Table2D<Byte>(
    function (R, C: NativeInt): Byte
    begin
      Result := Byte((R * 61 + C * 37 + R * C * 11) mod 256);
    end,
    0, w - 1, 0, h - 1
  );
  expected := RefLocEqFilter(arr, 2, 2);
  res := TNDAUt.Empty<Byte>(arr.Shape);

  fFilter.HRadius := 2;
  fFilter.VRadius := 2;
  fFilter.Execute(arr.Data, res.Data, arr.Strides[0], res.Strides[0], w, h);

  TNDAUt.TryAsDynArray2D<Byte>(res, resMat);
  for I := 0 to h - 1 do
    for J := 0 to w - 1 do
      CheckEquals(expected[I, J], resMat[I, J]);
end;

procedure TLocEqFilter2DUI8Tests.LocEqFilter_N7x8_R1_R2;
var arr, res: INDArray<Byte>;
    expected, resMat: TArray<TArray<Byte>>;
    I, J: Integer;
const
  h = 7;
  w = 8;
begin
  arr := TNDAUt.Table2D<Byte>(
    function (R, C: NativeInt): Byte
    begin
      Result := Byte((R * 61 + C * 37 + R * C * 11) mod 256);
    end,
    0, w - 1, 0, h - 1
  );
  expected := RefLocEqFilter(arr, 1, 2);
  res := TNDAUt.Empty<Byte>(arr.Shape);

  fFilter.HRadius := 1;
  fFilter.VRadius := 2;
  fFilter.Execute(arr.Data, res.Data, arr.Strides[0], res.Strides[0], w, h);

  TNDAUt.TryAsDynArray2D<Byte>(res, resMat);
  for I := 0 to h - 1 do
    for J := 0 to w - 1 do
      CheckEquals(expected[I, J], resMat[I, J]);
end;

procedure TLocEqFilter2DUI8Tests.LocEqFilter_N30x25_R1;
var arr, res: INDArray<Byte>;
    expected, resMat: TArray<TArray<Byte>>;
    I, J: Integer;
const
  h = 30;
  w = 25;
begin
  arr := TNDAUt.Table2D<Byte>(
    function (R, C: NativeInt): Byte
    begin
      Result := Byte((R * 61 + C * 37 + R * C * 11) mod 256);
    end,
    0, w - 1, 0, h - 1
  );
  expected := RefLocEqFilter(arr, 1, 1);
  res := TNDAUt.Empty<Byte>(arr.Shape);

  fFilter.HRadius := 1;
  fFilter.VRadius := 1;
  fFilter.Execute(arr.Data, res.Data, arr.Strides[0], res.Strides[0], w, h);

  TNDAUt.TryAsDynArray2D<Byte>(res, resMat);
  for I := 0 to h - 1 do
    for J := 0 to w - 1 do
      CheckEquals(expected[I, J], resMat[I, J]);
end;

procedure TLocEqFilter2DUI8Tests.LocEqFilter_N30x25_R5;
var arr, res: INDArray<Byte>;
    expected, resMat: TArray<TArray<Byte>>;
    I, J: Integer;
const
  h = 30;
  w = 25;
begin
  arr := TNDAUt.Table2D<Byte>(
    function (R, C: NativeInt): Byte
    begin
      Result := Byte((R * 61 + C * 37 + R * C * 11) mod 256);
    end,
    0, w - 1, 0, h - 1
  );
  expected := RefLocEqFilter(arr, 5, 5);
  res := TNDAUt.Empty<Byte>(arr.Shape);

  fFilter.HRadius := 5;
  fFilter.VRadius := 5;
  fFilter.Execute(arr.Data, res.Data, arr.Strides[0], res.Strides[0], w, h);

  TNDAUt.TryAsDynArray2D<Byte>(res, resMat);
  for I := 0 to h - 1 do
    for J := 0 to w - 1 do
      CheckEquals(expected[I, J], resMat[I, J]);
end;

{$endregion}

initialization

  RegisterTest(TMinFilter1DUI8Tests.Suite);
  RegisterTest(TMinFilter2DUI8Tests.Suite);
  RegisterTest(TMaxFilter2DUI8Tests.Suite);
  RegisterTest(TMedianFilter1DUI8Tests.Suite);
  RegisterTest(TMedianFilter1DF64Tests.Suite);
  RegisterTest(TMedianFilter2DUI8Tests.Suite);
  RegisterTest(TLocEqFilter2DUI8Tests.Suite);

end.
