unit panda.Tests.cvCmp;

interface

uses
    TestFramework
  , panda.Tests.NDATestCase
  , panda.cvCmp
  , System.SysUtils
  ;

type
  TVecCmpTests = class(TNDATestCase)
  protected const
    sTol = 1e-6;
  published
    procedure LessThan_VS_I32_3;
    procedure LessThan_VS_I32_8;
    procedure LessThan_VV_I32_3;
    procedure LessThan_VV_I32_8;

    procedure LessThanOrEqual_VS_I32_3;
    procedure LessThanOrEqual_VS_I32_8;
    procedure LessThanOrEqual_VV_I32_3;
    procedure LessThanOrEqual_VV_I32_8;

    procedure GreaterThan_VS_I32_3;
    procedure GreaterThan_VS_I32_8;
    procedure GreaterThan_VV_I32_3;
    procedure GreaterThan_VV_I32_8;

    procedure GreaterThanOrEqual_VS_I32_3;
    procedure GreaterThanOrEqual_VS_I32_8;
    procedure GreaterThanOrEqual_VV_I32_3;
    procedure GreaterThanOrEqual_VV_I32_8;

    procedure Threshold_UI8_16;
    procedure Threshold_UI8_16_Up127;
    procedure Threshold_UI8_20;

    procedure ThresholdInv_UI8_16;
    procedure ThresholdInv_UI8_16_Up127;
    procedure ThresholdInv_UI8_20;

    procedure Binarize_UI8_16;
    procedure Binarize_UI8_16_Up127;
    procedure Binarize_UI8_20;

    procedure BinarizeInv_UI8_16;
    procedure BinarizeInv_UI8_16_Up127;
    procedure BinarizeInv_UI8_20;

    procedure BinarizeVT_F32_16;
    procedure BinarizeVT_F32_20;
  end;

implementation

{$region 'TVecCmpTests'}

procedure TVecCmpTests.LessThan_VS_I32_3;
var x: TArray<Integer>;
    res: TArray<Boolean>;
begin
  x := TArray<Integer>.Create(1, 2, 3);
  SetLength(res, Length(x));

  CmpLT(PInteger(x), 3, PBoolean(res), Length(x));

  CheckEquals([True, True, False], res);
end;

procedure TVecCmpTests.LessThan_VS_I32_8;
var x: TArray<Integer>;
    res: TArray<Boolean>;
begin
  x := TArray<Integer>.Create(1, 2, 3, 2, 3, 1, 2, 3);
  SetLength(res, Length(x));

  CmpLT(PInteger(x), 3, PBoolean(res), Length(x));

  CheckEquals([True, True, False, True, False, True, True, False], res);
end;

procedure TVecCmpTests.LessThan_VV_I32_3;
var x, y: TArray<Integer>;
    res: TArray<Boolean>;
    I: Integer;
begin
  x := TArray<Integer>.Create(1, 2, 3);
  y := TArray<Integer>.Create(3, 2, 1);
  SetLength(res, Length(x));

  CmpLT(PInteger(x), PInteger(y), PBoolean(res), Length(x));

  for I := 0 to High(res) do
    CheckEquals(x[I] < y[I], res[I]);
end;

procedure TVecCmpTests.LessThan_VV_I32_8;
var x, y: TArray<Integer>;
    res: TArray<Boolean>;
    I: Integer;
begin
  x := TArray<Integer>.Create(1, 2, 3, 2, 3, 2, 1, 2);
  y := TArray<Integer>.Create(3, 2, 1, 3, 2, 1, 2, 3);
  SetLength(res, Length(x));

  CmpLT(PInteger(x), PInteger(y), PBoolean(res), Length(x));

  for I := 0 to High(res) do
    CheckEquals(x[I] < y[I], res[I]);
end;

procedure TVecCmpTests.LessThanOrEqual_VS_I32_3;
var x: TArray<Integer>;
    res: TArray<Boolean>;
begin
  x := TArray<Integer>.Create(1, 2, 3);
  SetLength(res, Length(x));

  CmpLTE(PInteger(x), 2, PBoolean(res), Length(x));

  CheckEquals([True, True, False], res);
end;

procedure TVecCmpTests.LessThanOrEqual_VS_I32_8;
var x: TArray<Integer>;
    res: TArray<Boolean>;
begin
  x := TArray<Integer>.Create(1, 2, 3, 2, 3, 1, 2, 3);
  SetLength(res, Length(x));

  CmpLTE(PInteger(x), 2, PBoolean(res), Length(x));

  CheckEquals([True, True, False, True, False, True, True, False], res);
end;

procedure TVecCmpTests.LessThanOrEqual_VV_I32_3;
var x, y: TArray<Integer>;
    res: TArray<Boolean>;
    I: Integer;
begin
  x := TArray<Integer>.Create(1, 2, 3);
  y := TArray<Integer>.Create(3, 2, 1);
  SetLength(res, Length(x));

  CmpLTE(PInteger(x), PInteger(y), PBoolean(res), Length(x));

  for I := 0 to High(res) do
    CheckEquals(x[I] <= y[I], res[I]);
end;

procedure TVecCmpTests.LessThanOrEqual_VV_I32_8;
var x, y: TArray<Integer>;
    res: TArray<Boolean>;
    I: Integer;
begin
  x := TArray<Integer>.Create(1, 2, 3, 2, 3, 2, 1, 2);
  y := TArray<Integer>.Create(3, 2, 1, 3, 2, 1, 2, 3);
  SetLength(res, Length(x));

  CmpLTE(PInteger(x), PInteger(y), PBoolean(res), Length(x));

  for I := 0 to High(res) do
    CheckEquals(x[I] <= y[I], res[I]);
end;

procedure TVecCmpTests.GreaterThan_VS_I32_3;
var x: TArray<Integer>;
    res: TArray<Boolean>;
    I: Integer;
begin
  x := TArray<Integer>.Create(1, 2, 3);
  SetLength(res, Length(x));

  CmpGT(PInteger(x), 1, PBoolean(res), Length(x));

  for I := 0 to High(x) do
    CheckEquals(x[I] > 1, res[I]);
end;

procedure TVecCmpTests.GreaterThan_VS_I32_8;
var x: TArray<Integer>;
    res: TArray<Boolean>;
    I: Integer;
begin
  x := TArray<Integer>.Create(1, 2, 3, 2, 3, 1, 2, 3);
  SetLength(res, Length(x));

  CmpGT(PInteger(x), 1, PBoolean(res), Length(x));

  for I := 0 to High(x) do
    CheckEquals(x[I] > 1, res[I]);
end;

procedure TVecCmpTests.GreaterThan_VV_I32_3;
var x, y: TArray<Integer>;
    res: TArray<Boolean>;
    I: Integer;
begin
  x := TArray<Integer>.Create(1, 2, 3);
  y := TArray<Integer>.Create(3, 2, 1);
  SetLength(res, Length(x));

  CmpGT(PInteger(x), PInteger(y), PBoolean(res), Length(x));

  for I := 0 to High(res) do
    CheckEquals(x[I] > y[I], res[I]);
end;

procedure TVecCmpTests.GreaterThan_VV_I32_8;
var x, y: TArray<Integer>;
    res: TArray<Boolean>;
    I: Integer;
begin
  x := TArray<Integer>.Create(1, 2, 3, 2, 3, 2, 1, 2);
  y := TArray<Integer>.Create(3, 2, 1, 3, 2, 1, 2, 3);
  SetLength(res, Length(x));

  CmpGT(PInteger(x), PInteger(y), PBoolean(res), Length(x));

  for I := 0 to High(res) do
    CheckEquals(x[I] > y[I], res[I]);
end;

procedure TVecCmpTests.GreaterThanOrEqual_VS_I32_3;
var x: TArray<Integer>;
    res: TArray<Boolean>;
    I: Integer;
begin
  x := TArray<Integer>.Create(1, 2, 3);
  SetLength(res, Length(x));

  CmpGTE(PInteger(x), 2, PBoolean(res), Length(x));

  for I := 0 to High(x) do
    CheckEquals(x[I] >= 2, res[I]);
end;

procedure TVecCmpTests.GreaterThanOrEqual_VS_I32_8;
var x: TArray<Integer>;
    res: TArray<Boolean>;
    I: Integer;
begin
  x := TArray<Integer>.Create(1, 2, 3, 2, 3, 1, 2, 3);
  SetLength(res, Length(x));

  CmpGTE(PInteger(x), 2, PBoolean(res), Length(x));

  for I := 0 to High(x) do
    CheckEquals(x[I] >= 2, res[I]);
end;

procedure TVecCmpTests.GreaterThanOrEqual_VV_I32_3;
var x, y: TArray<Integer>;
    res: TArray<Boolean>;
    I: Integer;
begin
  x := TArray<Integer>.Create(1, 2, 3);
  y := TArray<Integer>.Create(3, 2, 1);
  SetLength(res, Length(x));

  CmpGTE(PInteger(x), PInteger(y), PBoolean(res), Length(x));

  for I := 0 to High(res) do
    CheckEquals(x[I] >= y[I], res[I]);
end;

procedure TVecCmpTests.GreaterThanOrEqual_VV_I32_8;
var x, y: TArray<Integer>;
    res: TArray<Boolean>;
    I: Integer;
begin
  x := TArray<Integer>.Create(1, 2, 3, 2, 3, 2, 1, 2);
  y := TArray<Integer>.Create(3, 2, 1, 3, 2, 1, 2, 3);
  SetLength(res, Length(x));

  CmpGTE(PInteger(x), PInteger(y), PBoolean(res), Length(x));

  for I := 0 to High(res) do
    CheckEquals(x[I] >= y[I], res[I]);
end;

procedure TVecCmpTests.Threshold_UI8_16;
var x, res: TArray<Byte>;
    I: Integer;
begin
  x := TArray<Byte>.Create(1, 2, 3, 4, 5, 5, 4, 3, 2, 1, 1, 2, 3, 4, 5, 6);
  SetLength(res, Length(x));

  VecThreshold(PByte(x), PByte(res), 3, Length(x));

  for I := 0 to High(res) do
    if x[I] > 3 then
      CheckEquals(x[I], res[I])
    else
      CheckEquals(0, res[I]);
end;

procedure TVecCmpTests.Threshold_UI8_16_Up127;
var x, res: TArray<Byte>;
    I: Integer;
begin
  x := TArray<Byte>.Create(
    128, 129, 130, 131, 132, 132, 131, 130,
    129, 128, 128, 129, 130, 131, 132, 133);
  SetLength(res, Length(x));

  VecThreshold(PByte(x), PByte(res), 130, Length(x));

  for I := 0 to High(res) do
    if x[I] > 130 then
      CheckEquals(x[I], res[I])
    else
      CheckEquals(0, res[I]);
end;

procedure TVecCmpTests.Threshold_UI8_20;
var x, res: TArray<Byte>;
    I: Integer;
begin
  x := TArray<Byte>.Create(1, 2, 3, 4, 5, 5, 4, 3, 2, 1, 1, 2, 3, 4, 5, 5, 4, 3, 2, 1);
  SetLength(res, Length(x));

  VecThreshold(PByte(x), PByte(res), 3, Length(x));

  for I := 0 to High(res) do
    if x[I] > 3 then
      CheckEquals(x[I], res[I])
    else
      CheckEquals(0, res[I]);
end;

procedure TVecCmpTests.ThresholdInv_UI8_16;
var x, res: TArray<Byte>;
    I: Integer;
begin
  x := TArray<Byte>.Create(1, 2, 3, 4, 5, 5, 4, 3, 2, 1, 1, 2, 3, 4, 5, 6);
  SetLength(res, Length(x));

  VecThresholdInv(PByte(x), PByte(res), 3, Length(x));

  for I := 0 to High(res) do
    if x[I] > 3 then
      CheckEquals(0, res[I])
    else
      CheckEquals(x[I], res[I]);
end;

procedure TVecCmpTests.ThresholdInv_UI8_16_Up127;
var x, res: TArray<Byte>;
    I: Integer;
begin
  x := TArray<Byte>.Create(
    128, 129, 130, 131, 132, 132, 131, 130,
    129, 128, 128, 129, 130, 131, 132, 133);
  SetLength(res, Length(x));

  VecThresholdInv(PByte(x), PByte(res), 130, Length(x));

  for I := 0 to High(res) do
    if x[I] > 130 then
      CheckEquals(0, res[I])
    else
      CheckEquals(x[I], res[I]);
end;

procedure TVecCmpTests.ThresholdInv_UI8_20;
var x, res: TArray<Byte>;
    I: Integer;
begin
  x := TArray<Byte>.Create(1, 2, 3, 4, 5, 5, 4, 3, 2, 1, 1, 2, 3, 4, 5, 5, 4, 3, 2, 1);
  SetLength(res, Length(x));

  VecThresholdInv(PByte(x), PByte(res), 3, Length(x));

  for I := 0 to High(res) do
    if x[I] > 3 then
      CheckEquals(0, res[I])
    else
      CheckEquals(x[I], res[I]);
end;

procedure TVecCmpTests.Binarize_UI8_16;
var x, res: TArray<Byte>;
    I: Integer;
begin
  x := TArray<Byte>.Create(1, 2, 3, 4, 5, 5, 4, 3, 2, 1, 1, 2, 3, 4, 5, 6);
  SetLength(res, Length(x));

  VecBinarize(PByte(x), PByte(res), 3, Length(x));

  for I := 0 to High(res) do
    if x[I] > 3 then
      CheckEquals(255, res[I])
    else
      CheckEquals(0, res[I]);
end;

procedure TVecCmpTests.Binarize_UI8_16_Up127;
var x, res: TArray<Byte>;
    I: Integer;
begin
  x := TArray<Byte>.Create(
    128, 129, 130, 131, 132, 132, 131, 130,
    129, 128, 128, 129, 130, 131, 132, 133);
  SetLength(res, Length(x));

  VecBinarize(PByte(x), PByte(res), 130, Length(x));

  for I := 0 to High(res) do
    if x[I] > 130 then
      CheckEquals(255, res[I])
    else
      CheckEquals(0, res[I]);
end;

procedure TVecCmpTests.Binarize_UI8_20;
var x, res: TArray<Byte>;
    I: Integer;
begin
  x := TArray<Byte>.Create(1, 2, 3, 4, 5, 5, 4, 3, 2, 1, 1, 2, 3, 4, 5, 5, 4, 3, 2, 1);
  SetLength(res, Length(x));

  VecBinarize(PByte(x), PByte(res), 3, Length(x));

  for I := 0 to High(res) do
    if x[I] > 3 then
      CheckEquals(255, res[I])
    else
      CheckEquals(0, res[I]);
end;

procedure TVecCmpTests.BinarizeInv_UI8_16;
var x, res: TArray<Byte>;
    I: Integer;
begin
  x := TArray<Byte>.Create(1, 2, 3, 4, 5, 5, 4, 3, 2, 1, 1, 2, 3, 4, 5, 6);
  SetLength(res, Length(x));

  VecBinarizeInv(PByte(x), PByte(res), 3, Length(x));

  for I := 0 to High(res) do
    if x[I] > 3 then
      CheckEquals(0, res[I])
    else
      CheckEquals(255, res[I]);
end;

procedure TVecCmpTests.BinarizeInv_UI8_16_Up127;
var x, res: TArray<Byte>;
    I: Integer;
begin
  x := TArray<Byte>.Create(
    128, 129, 130, 131, 132, 132, 131, 130,
    129, 128, 128, 129, 130, 131, 132, 133);
  SetLength(res, Length(x));

  VecBinarizeInv(PByte(x), PByte(res), 130, Length(x));

  for I := 0 to High(res) do
    if x[I] > 130 then
      CheckEquals(0, res[I])
    else
      CheckEquals(255, res[I]);
end;

procedure TVecCmpTests.BinarizeInv_UI8_20;
var x, res: TArray<Byte>;
    I: Integer;
begin
  x := TArray<Byte>.Create(1, 2, 3, 4, 5, 5, 4, 3, 2, 1, 1, 2, 3, 4, 5, 5, 4, 3, 2, 1);
  SetLength(res, Length(x));

  VecBinarizeInv(PByte(x), PByte(res), 3, Length(x));

  for I := 0 to High(res) do
    if x[I] > 3 then
      CheckEquals(0, res[I])
    else
      CheckEquals(255, res[I]);
end;

procedure TVecCmpTests.BinarizeVT_F32_16;
var x, t, res: TArray<Single>;
    I: Integer;
begin
  x := TArray<Single>.Create(1, 2, 3, 4, 5, 5, 4, 3, 2, 1, 1, 2, 3, 4, 5, 5);
  t := TArray<Single>.Create(2, 1, 3, 5, 4, 3, 5, 1, 3, 2, 0, 3, 4, 5, 2, 6);
  SetLength(res, Length(x));

  VecBinarize(PSingle(x), PSingle(res), PSingle(t), Length(x));

  for I := 0 to High(res) do
    if x[I] > t[I] then
      CheckEquals(1, res[I], sTol)
    else
      CheckEquals(0, res[I], sTol);
end;

procedure TVecCmpTests.BinarizeVT_F32_20;
var x, t, res: TArray<Single>;
    I: Integer;
begin
  x := TArray<Single>.Create(1, 2, 3, 4, 5, 5, 4, 3, 2, 1, 1, 2, 3, 4, 5, 5, 4, 3, 2, 1);
  t := TArray<Single>.Create(2, 1, 3, 5, 4, 3, 5, 1, 3, 2, 0, 3, 4, 5, 2, 6, 4, 1, 2, 3);
  SetLength(res, Length(x));

  VecBinarize(PSingle(x), PSingle(res), PSingle(t), Length(x));

  for I := 0 to High(res) do
    if x[I] > t[I] then
      CheckEquals(1, res[I], sTol)
    else
      CheckEquals(0, res[I], sTol);
end;

{$endregion}

initialization

  RegisterTest(TVecCmpTests.Suite);

end.
