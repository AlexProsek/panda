unit panda.Tests.ArrCmp;

interface

uses
    TestFramework
  , panda.Intfs
  , panda.Arrays
  , panda.ArrCmp
  , panda.Arithmetic
  , panda.Tests.NDATestCase
  ;

type
  TArrCmpTests = class(TNDATestCase)
  protected const
    stol = 1e-6;
  published
    procedure MaxSV_1D_Cont_F32;
    procedure MaxSV_1D_F32;
    procedure MaxVV_1D_Cont_F32;
    procedure MaxVV_1D_F32;
  end;

  TTensorCmpTests = class(TNDATestCase)
  published
    procedure EqualOp_I32_VV;
    procedure NotEqualOp_I32_VV;
    procedure LessThanOp_I32_VV;
    procedure LessThanOrEqualOp_I32_VV;
    procedure GreaterThanOp_I32_VV;
    procedure GreaterThanOrEqualOp_I32_VV;

    procedure GreaterThanOp_I32_VS;
  end;

  TTensorMaskingTests = class(TNDATestCase)
  published
    procedure Masking1D_I32;
    procedure Masking2D_I32;
  end;

implementation

{$region 'TArrCmpTests'}

procedure TArrCmpTests.MaxSV_1D_Cont_F32;
var x, res: INDArray<Single>;
    v: TArray<Single>;
begin
  x := TNDAUt.AsArray<Single>([1, 2, 3]);
  res := TNDAUt.Empty<Single>([3]);

  ndaMax(2.5, x, res);

  CheckTrue(TNDAUt.TryAsDynArray<Single>(res, v));
  CheckEquals([2.5, 2.5, 3], v, stol);
end;

procedure TArrCmpTests.MaxSV_1D_F32;
var x, res, resView: INDArray<Single>;
    v: TArray<Single>;
begin
  x := TNDAUt.AsArray<Single>([1, 2, 3, 4, 5]);
  res := TNDAUt.Full<Single>([5], 0);
  resView := res[[NDIAll(2)]];

  ndaMax(2.5, x[[NDIAll(2)]], resView);

  CheckTrue(TNDAUt.TryAsDynArray<Single>(res, v));
  CheckEquals([2.5, 0, 3, 0, 5], v, stol);
end;

procedure TArrCmpTests.MaxVV_1D_Cont_F32;
var x, y, res: INDArray<Single>;
    v: TArray<Single>;
begin
  x := TNDAUt.AsArray<Single>([1, 2, 3]);
  y := TNDAUt.AsArray<Single>([3, 2, 1]);

  ndaMax(x, y, res);

  CheckTrue(TNDAUt.TryAsDynArray<Single>(res, v));
  CheckEquals([3, 2, 3], v, stol);
end;

procedure TArrCmpTests.MaxVV_1D_F32;
var x, y, res, resView: INDArray<Single>;
    v: TArray<Single>;
begin
  x := TNDAUt.AsArray<Single>([1, 2, 3, 4, 5]);
  y := TNDAUt.AsArray<Single>([5, 4, 3, 2, 1]);
  res := TNDAUt.Full<Single>([5], 0);
  resView := res[[NDIAll(2)]];

  ndaMax(x[[NDIAll(2)]], y[[NDIAll(2)]], resView);

  CheckTrue(TNDAUt.TryAsDynArray<Single>(res, v));
  CheckEquals([5, 0, 3, 0, 5], v);
end;

{$endregion}

{$region 'TTensorCmpTests'}

procedure TTensorCmpTests.EqualOp_I32_VV;
var x, y: TTensorI32;
    res: TTensorBool;
    v: TArray<Boolean>;
begin
  x := TNDAUt.AsArray<Integer>([1, 2, 3]);
  y := TNDAUt.AsArray<Integer>([3, 2, 1]);

  res := (x = y);
  CheckTrue(TNDAUt.TryAsDynArray<Boolean>(res, v));
  CheckEquals([False, True, False], v);
end;

procedure TTensorCmpTests.NotEqualOp_I32_VV;
var x, y: TTensorI32;
    res: TTensorBool;
    v: TArray<Boolean>;
begin
  x := TNDAUt.AsArray<Integer>([1, 2, 3]);
  y := TNDAUt.AsArray<Integer>([3, 2, 1]);

  res := (x <> y);
  CheckTrue(TNDAUt.TryAsDynArray<Boolean>(res, v));
  CheckEquals([True, False, True], v);
end;

procedure TTensorCmpTests.LessThanOp_I32_VV;
var x, y: TTensorI32;
    res: TTensorBool;
    v: TArray<Boolean>;
begin
  x := TNDAUt.AsArray<Integer>([1, 2, 3]);
  y := TNDAUt.AsArray<Integer>([3, 2, 1]);

  res := (x < y);
  CheckTrue(TNDAUt.TryAsDynArray<Boolean>(res, v));
  CheckEquals([True, False, False], v);
end;

procedure TTensorCmpTests.LessThanOrEqualOp_I32_VV;
var x, y: TTensorI32;
    res: TTensorBool;
    v: TArray<Boolean>;
begin
  x := TNDAUt.AsArray<Integer>([1, 2, 3]);
  y := TNDAUt.AsArray<Integer>([3, 2, 1]);

  res := (x <= y);
  CheckTrue(TNDAUt.TryAsDynArray<Boolean>(res, v));
  CheckEquals([True, True, False], v);
end;

procedure TTensorCmpTests.GreaterThanOp_I32_VV;
var x, y: TTensorI32;
    res: TTensorBool;
    v: TArray<Boolean>;
begin
  x := TNDAUt.AsArray<Integer>([1, 2, 3]);
  y := TNDAUt.AsArray<Integer>([3, 2, 1]);

  res := (x > y);
  CheckTrue(TNDAUt.TryAsDynArray<Boolean>(res, v));
  CheckEquals([False, False, True], v);
end;

procedure TTensorCmpTests.GreaterThanOrEqualOp_I32_VV;
var x, y: TTensorI32;
    res: TTensorBool;
    v: TArray<Boolean>;
begin
  x := TNDAUt.AsArray<Integer>([1, 2, 3]);
  y := TNDAUt.AsArray<Integer>([3, 2, 1]);

  res := (x >= y);
  CheckTrue(TNDAUt.TryAsDynArray<Boolean>(res, v));
  CheckEquals([False, True, True], v);
end;

procedure TTensorCmpTests.GreaterThanOp_I32_VS;
var x: TTensorI32;
    res: TTensorBool;
    v: TArray<Boolean>;
begin
  x := TNDAUt.AsArray<Integer>([1, 2, 3]);

  res := (x > 2);
  CheckTrue(TNDAUt.TryAsDynArray<Boolean>(res, v));
  CheckEquals([False, False, True], v);
end;

{$endregion}

{$region 'TTensorMaskingTests'}

procedure TTensorMaskingTests.Masking1D_I32;
var t: TTensorI32;
    v: TArray<Integer>;
begin
  t := TNDAUt.AsArray<Integer>([1, 2, 3, 4, 5, 4, 3, 2, 1]);

  t.MaskBy(t > 2);
  CheckTrue(TNDAUt.TryAsDynArray<Integer>(t, v));
  CheckEquals([0, 0, 3, 4, 5, 4, 3, 0, 0], v);
end;

procedure TTensorMaskingTests.Masking2D_I32;
var t: TTensorI32;
    m: TArray<TArray<Integer>>;
begin
  t := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6], [7, 8, 9]]);

  t.MaskBy((t > 2) and (t < 8));
  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(t, m));
  CheckEquals([0, 0, 3], m[0]);
  CheckEquals([4, 5, 6], m[1]);
  CheckEquals([7, 0, 0], m[2]);
end;

{$endregion}

initialization

  RegisterTest(TArrCmpTests.Suite);
  RegisterTest(TTensorCmpTests.Suite);
  RegisterTest(TTensorMaskingTests.Suite);

end.
