unit panda.Filters.Tests.OrdStatFilters;

interface

uses
    TestFramework
  , panda.Intfs
  , panda.Arrays
  , panda.DynArrayUtils
  , panda.Filters.OrderStatFilters
  , panda.Tests.NDATestCase
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

initialization

  RegisterTest(TMinFilter1DUI8Tests.Suite);
  RegisterTest(TMinFilter2DUI8Tests.Suite);
  RegisterTest(TMedianFilter1DUI8Tests.Suite);
  RegisterTest(TMedianFilter1DF64Tests.Suite);

end.
