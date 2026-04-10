unit panda.Tests.ArrCmp;

interface

uses
    TestFramework
  , panda.Intfs
  , panda.Arrays
  , panda.ArrCmp
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

initialization

  RegisterTest(TArrCmpTests.Suite);

end.
