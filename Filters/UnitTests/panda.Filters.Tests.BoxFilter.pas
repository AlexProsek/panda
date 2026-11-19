unit panda.Filters.Tests.BoxFilter;

interface

uses
    TestFramework
  , panda.Intfs
  , panda.Arrays
  , panda.DynArrayUtils
  , panda.Filters.BoxFilter
  , panda.Tests.NDATestCase
  ;

type
  TBoxFilter1DF32Tests = class(TNDATestCase)
  protected const
    sTol = 1e-5;
  protected
    fFilter: TBoxFilter1DF32;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure BoxFilter_N5_R1;
    procedure BoxFilter_N10_R2;

    procedure MovingSum_N10_K3;
    procedure MovingAvg_N10_K3;
  end;

  TIntegralTests = class(TNDATestCase)
  protected const
    sTol = 1e-5;
  published
    procedure CArr1D;
    procedure SArr1D;
    procedure CArr2D;
    procedure CArr3D;
  end;

implementation

{$region 'TBoxFilter1DF32Tests'}

procedure TBoxFilter1DF32Tests.SetUp;
begin
  inherited;
  fFilter := TBoxFilter1DF32.Create;
end;

procedure TBoxFilter1DF32Tests.TearDown;
begin
  fFilter.Free;
  inherited;
end;

procedure TBoxFilter1DF32Tests.BoxFilter_N5_R1;
var x, res: TArray<Single>;
begin
  x := TArray<Single>.Create(1, 2, 3, 2, 1);
  SetLength(res, Length(x));

  fFilter.Radius := 1;
  fFilter.Execute(PByte(x), PByte(res), Length(x));

  CheckEquals([3/2, 2, 7/3, 2, 3/2], res, sTol);
end;

procedure TBoxFilter1DF32Tests.BoxFilter_N10_R2;
var x, res: TArray<Single>;
begin
  x := TArray<Single>.Create(1, 2, 3, 4, 5, 5, 4, 3, 2, 1);
  SetLength(res, Length(x));

  fFilter.Radius := 2;
  fFilter.Execute(PByte(x), PByte(res), Length(x));

  CheckEquals([2, 5/2, 3, 19/5, 21/5, 21/5, 19/5, 3, 5/2, 2], res, sTol);
end;

procedure TBoxFilter1DF32Tests.MovingSum_N10_K3;
var x, res: TArray<Single>;
begin
  x := TArray<Single>.Create(1, 2, 3, 4, 5, 5, 4, 3, 2, 1);
  SetLength(res, Length(x) - 2);

  MovingSum_F32(PByte(x), PByte(res), 3, Length(x));

  CheckEquals(x[0] + x[1] + x[2], res[0], sTol);
  CheckEquals(x[1] + x[2] + x[3], res[1], sTol);
  CheckEquals(x[2] + x[3] + x[4], res[2], sTol);
  CheckEquals(x[3] + x[4] + x[5], res[3], sTol);
  CheckEquals(x[4] + x[5] + x[6], res[4], sTol);
  CheckEquals(x[5] + x[6] + x[7], res[5], sTol);
  CheckEquals(x[6] + x[7] + x[8], res[6], sTol);
  CheckEquals(x[7] + x[8] + x[9], res[7], sTol);
end;

procedure TBoxFilter1DF32Tests.MovingAvg_N10_K3;
var x, res: TArray<Single>;
begin
  x := TArray<Single>.Create(1, 2, 3, 4, 5, 5, 4, 3, 2, 1);
  SetLength(res, Length(x) - 2);

  MovingAvg_F32(PByte(x), PByte(res), 3, Length(x));

  CheckEquals((x[0] + x[1] + x[2])/3, res[0], sTol);
  CheckEquals((x[1] + x[2] + x[3])/3, res[1], sTol);
  CheckEquals((x[2] + x[3] + x[4])/3, res[2], sTol);
  CheckEquals((x[3] + x[4] + x[5])/3, res[3], sTol);
  CheckEquals((x[4] + x[5] + x[6])/3, res[4], sTol);
  CheckEquals((x[5] + x[6] + x[7])/3, res[5], sTol);
  CheckEquals((x[6] + x[7] + x[8])/3, res[6], sTol);
  CheckEquals((x[7] + x[8] + x[9])/3, res[7], sTol);
end;

{$endregion}

{$region 'TIntegralTests'}

procedure TIntegralTests.CArr1D;
var a, acc: INDArray<Single>;
    v: TArray<Single>;
begin
  a := TNDAUt.AsArray<Single>([1, 2, 3, 4, 5]);

  Integral(a, acc);

  CheckTrue(TNDAUt.TryAsDynArray<Single>(acc, v));
  CheckEquals([1, 3, 6, 10, 15], v, sTol);
end;

procedure TIntegralTests.SArr1D;
var a, sa, acc: INDArray<Single>;
    v: TArray<Single>;
begin
  a := TNDAUt.AsArray<Single>([1, 2, 3, 4, 5]);
  sa := a[[NDIAll(2)]];

  Integral(sa, acc);

  CheckTrue(TNDAUt.TryAsDynArray<Single>(acc, v));
  CheckEquals([1, 4, 9], v, sTol);
end;

procedure TIntegralTests.CArr2D;
var a, acc: INDArray<Single>;
    m: TArray<TArray<Single>>;
begin
  a := TNDAUt.AsArray<Single>([
    [ 1,  2,  3,  4,  5],
    [ 6,  7,  8,  9, 10],
    [11, 12, 13, 14, 15],
    [16, 17, 18, 19, 20]
  ]);

  Integral(a, acc);

  CheckTrue(TNDAUt.TryAsDynArray2D<Single>(acc, m));
  CheckEquals([ 1,  3,   6,  10,  15], m[0], sTol);
  CheckEquals([ 7, 16,  27,  40,  55], m[1], sTol);
  CheckEquals([18, 39,  63,  90, 120], m[2], sTol);
  CheckEquals([34, 72, 114, 160, 210], m[3], sTol);
end;

procedure TIntegralTests.CArr3D;
var a, acc: INDArray<Single>;
    t: TArray<TArray<TArray<Single>>>;
begin
  a := TNDAUt.AsArray<Single>([
    [[1, 2, 3], [4, 5, 6]],
    [[7, 8, 9], [10, 11, 12]]
  ]);

  Integral(a, acc);

  CheckTrue(TNDAUt.TryAsDynArray3D<Single>(acc, t));
  CheckEquals([ 1,  3,  6], t[0, 0], sTol);
  CheckEquals([ 5, 12, 21], t[0, 1], sTol);
  CheckEquals([ 8, 18, 30], t[1, 0], sTol);
  CheckEquals([22, 48, 78], t[1, 1], sTol);
end;

{$endregion}

initialization

  RegisterTest(TBoxFilter1DF32Tests.Suite);
  RegisterTest(TIntegralTests.Suite);

end.
