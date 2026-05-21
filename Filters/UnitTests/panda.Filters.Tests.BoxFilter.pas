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

initialization

  RegisterTest(TBoxFilter1DF32Tests.Suite);

end.
