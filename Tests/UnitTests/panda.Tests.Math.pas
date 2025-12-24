unit panda.Tests.Math;

interface

uses
    TestFramework
  , panda.Intfs
  , panda.Arrays
  , panda.Math
  , panda.Arithmetic
  , panda.Tests.NDATestCase
  , pandalib
  ;

type
  TMathTests = class(TNDATestCase)
  protected const
    tol = 1e-5;
  published
    procedure Range1DInt;
    procedure Range1DSingle;

    procedure Total1D;
    procedure Total2D;
    procedure Total2DLvl1;
    procedure Total3D;
    procedure Total3DLvl1;
    procedure Total3DLvl2;

    procedure Softmax1D;
    procedure Softmax2D;

    procedure Dot_VxV;
    procedure Dot_MxV;
    procedure Dot_T3xV;
    procedure Dot_VxM;
    procedure Dot_VxT3;
    procedure Dot_MxM;
    procedure Dot_T3xM;
    procedure Dot_MxT3;
    procedure Dot_T3xT3;

    procedure Outer_VxV;
    procedure Outer_VmxVn;
    procedure Outer_MxV;
    procedure Outer_MxM;
  end;

implementation

{$region 'TMathTests'}

procedure TMathTests.Range1DInt;
var x: TNDAItems<Integer>;
begin
  x := ndaRange(3);

  CheckEquals(1, x.NDim);
  CheckEquals(3, x.Shape[0]);
  CheckEquals(0, x[[0]]);
  CheckEquals(1, x[[1]]);
  CheckEquals(2, x[[2]]);
end;

procedure TMathTests.Range1DSingle;
var x: TNDAItems<Single>;
begin
  x := ndaRange(Single(3.0));

  CheckEquals(1, x.NDim);
  CheckEquals(3, x.Shape[0]);
  CheckEquals(0, x[[0]], tol);
  CheckEquals(1, x[[1]], tol);
  CheckEquals(2, x[[2]], tol);
end;

procedure TMathTests.Total1D;
var x: INDArray<Single>;
    s: Single;
begin
  x := nda.AsArray<Single>([1, 2, 3]);
  x := ndaTotal(x);
  CheckTrue(nda.TryAsScalar<Single>(x, s));
  CheckEquals(6, s, tol);
end;

procedure TMathTests.Total2D;
var x: TNDAItems<Single>;
begin
  x := nda.AsArray<Single>([1, 2, 3, 4, 5, 6], [2, 3]);
  x := ndaTotal(x);

  CheckEquals(1, x.NDim);
  CheckEquals(3, x.Shape[0]);
  CheckEquals(5, x[[0]], tol);
  CheckEquals(7, x[[1]], tol);
  CheckEquals(9, x[[2]], tol);
end;

procedure TMathTests.Total2DLvl1;
var x: TNDAItems<Single>;
begin
  x := nda.AsArray<Single>([1, 2, 3, 4, 5, 6], [2, 3]);
  x := ndaTotalAtLvl(x, 1);

  CheckEquals(1, x.NDim);
  CheckEquals(2, x.Shape[0]);
  CheckEquals(6,  x[[0]], tol);
  CheckEquals(15, x[[1]], tol);
end;

procedure TMathTests.Total3D;
var x: TNDAItems<Single>;
begin
  x := sRng2NDA([2, 3, 2]);
  x := ndaTotal(x);

  CheckEquals(2, x.NDim);
  CheckEquals(3, x.Shape[0]);
  CheckEquals(2, x.Shape[1]);

  CheckEquals(8,  x[[0, 0]], tol);
  CheckEquals(10, x[[0, 1]], tol);

  CheckEquals(12, x[[1, 0]], tol);
  CheckEquals(14, x[[1, 1]], tol);

  CheckEquals(16, x[[2, 0]], tol);
  CheckEquals(18, x[[2, 1]], tol);
end;

procedure TMathTests.Total3DLvl1;
var x: TNDAItems<Single>;
begin
  x := sRng2NDA([2, 3, 2]);
  x := ndaTotalAtLvl(x, 1);

  CheckEquals(2, x.NDim);
  CheckEquals(2, x.Shape[0]);
  CheckEquals(2, x.Shape[1]);

  CheckEquals(9,  x[[0, 0]], tol);
  CheckEquals(12, x[[0, 1]], tol);

  CheckEquals(27, x[[1, 0]], tol);
  CheckEquals(30, x[[1, 1]], tol);
end;

procedure TMathTests.Total3DLvl2;
var x: TNDAItems<Single>;
begin
  x := sRng2NDA([2, 3, 2]);
  x := ndaTotalAtLvl(x, 2);

  CheckEquals(2, x.NDim);
  CheckEquals(2, x.Shape[0]);
  CheckEquals(3, x.Shape[1]);

  CheckEquals(3,  x[[0, 0]], tol);
  CheckEquals(7,  x[[0, 1]], tol);
  CheckEquals(11, x[[0, 2]], tol);

  CheckEquals(15, x[[1, 0]], tol);
  CheckEquals(19, x[[1, 1]], tol);
  CheckEquals(23, x[[1, 2]], tol);
end;

function softmax(const aX: INDArray<Single>): INDArray<Single>;
var x, tmp: TTensorF32;
begin
  with nda do begin
    x := Exp(aX);
    tmp := TotalAtLvl(x, -1);
    Result := Transpose<Single>(Transpose<Single>(x) / tmp);
  end;
end;

procedure TMathTests.Softmax1D;
var x: TNDAItems<Single>;
begin
  x := softmax(nda.AsArray<Single>([1, 2, 3]));

  CheckEquals(1, x.NDim);
  CheckEquals(3, x.Shape[0]);
  CheckEquals(0.090030,  x[[0]], tol);
  CheckEquals(0.244728,  x[[1]], tol);
  CheckEquals(0.665241,  x[[2]], tol);
end;

procedure TMathTests.Softmax2D;
var x: TNDAItems<Single>;
    m: TArray<TArray<Single>>;
begin
  x := softmax(nda.AsArray<Single>([[1, 0, 1], [0, 0, 1]]));

  CheckTrue(nda.TryAsDynArray2D<Single>(x, m));
  CheckEquals(2, Length(m));
  CheckEquals(3, Length(m[0]));
  CheckEquals(3, Length(m[1]));

  CheckEquals(0.422319, m[0, 0], tol);
  CheckEquals(0.155362, m[0, 1], tol);
  CheckEquals(0.422319, m[0, 2], tol);

  CheckEquals(0.211942, m[1, 0], tol);
  CheckEquals(0.211942, m[1, 1], tol);
  CheckEquals(0.576117, m[1, 2], tol);
end;

procedure TMathTests.Dot_VxV;
var x, y, res: INDArray<Single>;
    d: Single;
begin
  x := nda.AsArray<Single>([1, 2, 3]);
  y := nda.AsArray<Single>([3, 2, 1]);
  res := ndaDot(x, y);

  CheckTrue(nda.TryAsScalar<Single>(res, d));
  CheckEquals(10, d, tol);
end;

procedure TMathTests.Dot_MxV;
var m, x: INDArray<Single>;
    y: TNDAItems<Single>;
begin
  m := nda.AsArray<Single>([1, 2, 3, 4, 5, 6], [2, 3]);
  x := nda.AsArray<Single>([7, 8, 9]);
  y := ndaDot(m, x);

  CheckEquals(1, y.NDim);
  CheckEquals(2, y.Shape[0]);

  CheckEquals(50,  y[[0]], tol);
  CheckEquals(122, y[[1]], tol);
end;

procedure TMathTests.Dot_T3xV;
var x, t: INDArray<Single>;
    y: TNDAItems<Single>;
begin
  t := sRng2NDA([2, 2, 3]);
  x := nda.AsArray<Single>([1, 2, 3]);
  y := ndaDot(t, x);

  CheckEquals(2, y.NDim);
  CheckEquals(2, y.Shape[0]);
  CheckEquals(2, y.Shape[1]);

  CheckEquals(14, y[[0, 0]], tol);
  CheckEquals(32, y[[0, 1]], tol);

  CheckEquals(50, y[[1, 0]], tol);
  CheckEquals(68, y[[1, 1]], tol);
end;

procedure TMathTests.Dot_VxM;
var m, x: INDArray<Single>;
    y: TNDAItems<Single>;
begin
  m := nda.AsArray<Single>([1, 2, 3, 4, 5, 6], [2, 3]);
  x := nda.AsArray<Single>([7, 8]);
  y := ndaDot(x, m);

  CheckEquals(1, y.NDim);
  CheckEquals(3, y.Shape[0]);

  CheckEquals(39, y[[0]], tol);
  CheckEquals(54, y[[1]], tol);
  CheckEquals(69, y[[2]], tol);
end;

procedure TMathTests.Dot_VxT3;
var x, t: INDArray<Single>;
    y: TNDAItems<Single>;
begin
  t := sRng2NDA([2, 2, 3]);
  x := nda.AsArray<Single>([1, 2]);
  y := ndaDot(x, t);

  CheckEquals(2, y.NDim);
  CheckEquals(2, y.Shape[0]);
  CheckEquals(3, y.Shape[1]);

  CheckEquals(15, y[[0, 0]], tol);
  CheckEquals(18, y[[0, 1]], tol);
  CheckEquals(21, y[[0, 2]], tol);

  CheckEquals(24, y[[1, 0]], tol);
  CheckEquals(27, y[[1, 1]], tol);
  CheckEquals(30, y[[1, 2]], tol);
end;

procedure TMathTests.Dot_MxM;
var m1, m2: INDArray<Single>;
    m: TNDAItems<Single>;
begin
  m1 := nda.AsArray<Single>([[1, 2, 3], [4, 5, 6]]);
  m2 := nda.AsArray<Single>([[6, 5], [4, 3], [2, 1]]);
  m := ndaDot(m1, m2);

  CheckEquals(2, m.NDim);
  CheckEquals(2, m.Shape[0]);
  CheckEquals(2, m.Shape[1]);

  CheckEquals(20, m[[0, 0]], tol);
  CheckEquals(14, m[[0, 1]], tol);

  CheckEquals(56, m[[1, 0]], tol);
  CheckEquals(41, m[[1, 1]], tol);
end;

procedure TMathTests.Dot_T3xM;
var m1, m2: INDArray<Single>;
    m: TNDAItems<Single>;
begin
// ArrayReshape[Range[2 3 2], {2, 3, 2}] . {{1, 2}, {3, 4}}
  m1 := sRng2NDA([2, 3, 2]);
  m2 := nda.AsArray<Single>([[1, 2], [3, 4]]);
  m := NDADot(m1, m2);

  CheckEquals(3, m.NDim);
  CheckEquals(2, m.Shape[0]);
  CheckEquals(3, m.Shape[1]);
  CheckEquals(2, m.Shape[2]);

  CheckEquals(7 , m[[0, 0, 0]], tol);
  CheckEquals(10, m[[0, 0, 1]], tol);

  CheckEquals(15, m[[0, 1, 0]], tol);
  CheckEquals(22, m[[0, 1, 1]], tol);

  CheckEquals(23, m[[0, 2, 0]], tol);
  CheckEquals(34, m[[0, 2, 1]], tol);


  CheckEquals(31, m[[1, 0, 0]], tol);
  CheckEquals(46, m[[1, 0, 1]], tol);

  CheckEquals(39, m[[1, 1, 0]], tol);
  CheckEquals(58, m[[1, 1, 1]], tol);

  CheckEquals(47, m[[1, 2, 0]], tol);
  CheckEquals(70, m[[1, 2, 1]], tol);
end;

procedure TMathTests.Dot_MxT3;
var m1, m2: INDArray<Single>;
    m: TNDAItems<Single>;
begin
// {{1, 2}, {3, 4}} . ArrayReshape[Range[2 3 2], {2, 3, 2}]
  m1 := nda.AsArray<Single>([[1, 2], [3, 4]]);
  m2 := sRng2NDA([2, 3, 2]);
  m := ndaDot(m1, m2);

  CheckEquals(3, m.NDim);
  CheckEquals(2, m.Shape[0]);
  CheckEquals(3, m.Shape[1]);
  CheckEquals(2, m.Shape[2]);

  CheckEquals(15, m[[0, 0, 0]], tol);
  CheckEquals(18, m[[0, 0, 1]], tol);

  CheckEquals(21, m[[0, 1, 0]], tol);
  CheckEquals(24, m[[0, 1, 1]], tol);

  CheckEquals(27, m[[0, 2, 0]], tol);
  CheckEquals(30, m[[0, 2, 1]], tol);


  CheckEquals(31, m[[1, 0, 0]], tol);
  CheckEquals(38, m[[1, 0, 1]], tol);

  CheckEquals(45, m[[1, 1, 0]], tol);
  CheckEquals(52, m[[1, 1, 1]], tol);

  CheckEquals(59, m[[1, 2, 0]], tol);
  CheckEquals(66, m[[1, 2, 1]], tol);
end;

procedure TMathTests.Dot_T3xT3;
var m, expected, m1: INDArray<Single>;
begin
  m1 := sRng2NDA([2, 3, 2]);
  m := ndaDot(m1, m1);

  CheckEquals([2, 3, 3, 2], m.Shape);

  expected := TNDAUt.AsArray<Single>([
    15, 18, 21, 24, 27, 30, 31, 38, 45, 52, 59, 66, 47, 58, 69, 80, 91,
    102, 63, 78, 93, 108, 123, 138, 79, 98, 117, 136, 155, 174, 95, 118,
    141, 164, 187, 210], [2, 3, 3, 2]
  );

  CheckTrue(ndaAllClose(expected, m, tol));
end;

procedure TMathTests.Outer_VxV;
var x, y, res: INDArray<Single>;
    m: TArray<TArray<Single>>;
begin
  x := nda.AsArray<Single>([1, 2]);
  y := nda.AsArray<Single>([3, 4]);
  res := ndaOuter(x, y);

  CheckTrue(TNDAUt.TryAsDynArray2D<Single>(res, m));
  CheckEquals(2, Length(m));

  CheckEquals(2, Length(m[0]));
  CheckEquals(3, m[0, 0], tol);
  CheckEquals(4, m[0, 1], tol);

  CheckEquals(2, Length(m[1]));
  CheckEquals(6, m[1, 0], tol);
  CheckEquals(8, m[1, 1], tol);
end;

procedure TMathTests.Outer_VmxVn;
var x, y, res: INDArray<Single>;
    m: TArray<TArray<Single>>;
begin
  x := nda.AsArray<Single>([1, 2]);
  y := nda.AsArray<Single>([3, 4, 5]);
  res := ndaOuter(x, y);

  CheckTrue(TNDAUt.TryAsDynArray2D<Single>(res, m));
  CheckEquals(2, Length(m));

  CheckEquals(3,  Length(m[0]));
  CheckEquals(3,  m[0, 0], tol);
  CheckEquals(4,  m[0, 1], tol);
  CheckEquals(5,  m[0, 2], tol);

  CheckEquals(3,  Length(m[1]));
  CheckEquals(6,  m[1, 0], tol);
  CheckEquals(8,  m[1, 1], tol);
  CheckEquals(10, m[1, 2], tol);
end;

procedure TMathTests.Outer_MxV;
var x, y: INDArray<Single>;
    m: TNDAItems<Single>;
begin
  x := nda.AsArray<Single>([[1, 2, 3], [4, 5, 6]]);
  y := nda.AsArray<Single>([7, 8]);
  m := ndaOuter(x, y);

  CheckEquals(3, m.NDim);
  CheckEquals(2, m.Shape[0]);
  CheckEquals(3, m.Shape[1]);
  CheckEquals(2, m.Shape[2]);

  CheckEquals(m[[0, 0, 0]], 7,  tol);
  CheckEquals(m[[0, 0, 1]], 8,  tol);
  CheckEquals(m[[0, 1, 0]], 14, tol);
  CheckEquals(m[[0, 1, 1]], 16, tol);
  CheckEquals(m[[0, 2, 0]], 21, tol);
  CheckEquals(m[[0, 2, 1]], 24, tol);

  CheckEquals(m[[1, 0, 0]], 28, tol);
  CheckEquals(m[[1, 0, 1]], 32, tol);
  CheckEquals(m[[1, 1, 0]], 35, tol);
  CheckEquals(m[[1, 1, 1]], 40, tol);
  CheckEquals(m[[1, 2, 0]], 42, tol);
  CheckEquals(m[[1, 2, 1]], 48, tol);
end;

procedure TMathTests.Outer_MxM;
var x, y, m, expected: INDArray<Single>;
begin
  x := nda.AsArray<Single>([[1, 2], [3, 4]]);
  y := nda.AsArray<Single>([[5, 6, 7], [8, 9, 10]]);
  m := ndaOuter(x, y);

  CheckEquals(4, m.NDim);
  CheckEquals(2, m.Shape[0]);
  CheckEquals(2, m.Shape[1]);
  CheckEquals(2, m.Shape[2]);
  CheckEquals(3, m.Shape[3]);

  expected := TNDAUt.AsArray<Single>([
    5, 6, 7, 8, 9, 10,
    10, 12, 14, 16, 18, 20,
    15, 18, 21, 24, 27, 30,
    20, 24, 28, 32, 36, 40],
    [2, 2, 2, 3]
  );

  CheckTrue(ndaAllClose(expected, m, tol));
end;

{$endregion}

initialization

  RegisterTest(TMathTests.Suite);

end.
