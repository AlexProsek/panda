unit panda.Tests.Arithmetic;

interface

uses
    TestFramework
  , panda.Tests.NDATestCase
  , panda.Intfs
  , panda.Arrays
  , panda.Arithmetic
  , pandalib
  ;

type
  TTensorF64Tests = class(TNDATestCase)
  protected const
    tol = 1e-12;
  published
    // S - scalar, V - vecotr, M - matrix
    procedure Add_VS;
    procedure Add_SV;
    procedure Add_VV;
    procedure Add_MV;
    procedure Add_VM;
    procedure Subtract_VS;
    procedure Subtract_SV;
    procedure Subtract_VV;
    procedure Subtract_MV;
    procedure Subtract_VM;
    procedure Multiply_VS;
    procedure Multiply_SV;
    procedure Multiply_VV;
    procedure Multiply_MV;
    procedure Multiply_VM;
    procedure Divide_VS;
    procedure Divide_SV;
    procedure Divide_VV;
    procedure Divide_MV;
    procedure Divide_VM;
  end;

  TTensorI32Tests = class(TNDATestCase)
  published
    procedure Add_SS;
    procedure Add_VS;
    procedure Add_SV;
    procedure Add_VV;
    procedure Add_MV;
    procedure Add_VM;
    procedure Subtract_VS;
    procedure Subtract_SV;
    procedure Subtract_VV;
    procedure Subtract_MV;
    procedure Subtract_VM;
    procedure Multiply_VS;
    procedure Multiply_SV;
    procedure Multiply_VV;
    procedure Multiply_MV;
    procedure Multiply_VM;
  end;

  TTensorI64Tests = class(TNDATestCase)
  published
    procedure Add_VS;
    procedure Add_SV;
    procedure Add_VV;
    procedure Add_MV;
    procedure Add_VM;
    procedure Subtract_VS;
    procedure Subtract_SV;
    procedure Subtract_VV;
    procedure Subtract_MV;
    procedure Subtract_VM;
    procedure Multiply_VS;
    procedure Multiply_SV;
    procedure Multiply_VV;
    procedure Multiply_MV;
    procedure Multiply_VM;
  end;

  TTensorF32Tests = class(TNDATestCase)
  protected const
    tol = 1e-5;
  published
    procedure Add_VS;
    procedure Add_SV;
    procedure Add_VV;
    procedure Add_MV;
    procedure Add_VM;
    procedure Subtract_VS;
    procedure Subtract_SV;
    procedure Subtract_VV;
    procedure Subtract_MV;
    procedure Subtract_VM;
    procedure Multiply_VS;
    procedure Multiply_SV;
    procedure Multiply_VV;
    procedure Multiply_MV;
    procedure Multiply_VM;
    procedure Divide_VS;
    procedure Divide_SV;
    procedure Divide_VV;
    procedure Divide_MV;
    procedure Divide_VM;
    procedure SubFrom_VS;
    procedure SubFrom_VV;
    procedure SubFrom_MV;

    procedure CvtInt32ToF32;
  end;

  TArithmeticTests = class(TNDATestCase)
  protected const
    tol = 1e-5;
  published
    procedure AllClose1D;
    procedure AllClose2D;
    procedure AllClose1Ds;
    procedure AllClose2Ds;
    procedure AllClose2Ds_HGaps;
    procedure AllClose3Ds_HGaps;
  end;

  TBoolArithmeticTests = class(TNDATestCase)
  published
    procedure BoolAnd1D;
    procedure BoolOr1D;
    procedure BoolXor1D;
    procedure BoolNot1D;
  end;

implementation

{$region 'TTensorF64Tests'}

procedure TTensorF64Tests.Add_VS;
var a: TTensorF64;
    v: TArray<Double>;
begin
  a := TNDAUt.AsArray<Double>([1, 2, 3]);
  a := a + 2;

  CheckTrue(TNDAUt.TryAsDynArray<Double>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(3, v[0], tol);
  CheckEquals(4, v[1], tol);
  CheckEquals(5, v[2], tol);
end;

procedure TTensorF64Tests.Add_SV;
var a: TTensorF64;
    v: TArray<Double>;
begin
  a := TNDAUt.AsArray<Double>([1, 2, 3]);
  a := 2 + a;

  CheckTrue(TNDAUt.TryAsDynArray<Double>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(3, v[0], tol);
  CheckEquals(4, v[1], tol);
  CheckEquals(5, v[2], tol);
end;

procedure TTensorF64Tests.Add_VV;
var a: TTensorF64;
    v: TArray<Double>;
begin
  a := TNDAUt.AsArray<Double>([1, 2, 3]);
  a := a + a;

  CheckTrue(TNDAUt.TryAsDynArray<Double>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(2, v[0], tol);
  CheckEquals(4, v[1], tol);
  CheckEquals(6, v[2], tol);
end;

procedure TTensorF64Tests.Add_MV;
var a, b: TTensorF64;
    m: TArray<TArray<Double>>;
    v: TArray<Double>;
begin
  a := TNDAUt.AsArray<Double>([[1, 2, 3], [4, 5, 6], [7, 8, 9]]);
  b := TNDAUt.AsArray<Double>([1, 2, 3]);
  a := a + b;

  CheckTrue(TNDAUt.TryAsDynArray2D<Double>(a, m));
  CheckEquals(3, Length(m));
  CheckEquals([2,  4,  6], m[0], tol);
  CheckEquals([5,  7,  9], m[1], tol);
  CheckEquals([8, 10, 12], m[2], tol);
end;

procedure TTensorF64Tests.Add_VM;
var a, b: TTensorF64;
    m: TArray<TArray<Double>>;
begin
  a := TNDAUt.AsArray<Double>([1, 2, 3]);
  b := TNDAUt.AsArray<Double>([[1, 2, 3], [4, 5, 6], [7, 8, 9]]);
  a := a + b;

  CheckTrue(TNDAUt.TryAsDynArray2D<Double>(a, m));
  CheckEquals(3, Length(m));
  CheckEquals([2,  4,  6], m[0], tol);
  CheckEquals([5,  7,  9], m[1], tol);
  CheckEquals([8, 10, 12], m[2], tol);
end;

procedure TTensorF64Tests.Subtract_VS;
var a: TTensorF64;
    v: TArray<Double>;
begin
  a := TNDAUt.AsArray<Double>([2, 4, 6]);
  a := a - 1;

  CheckTrue(TNDAUt.TryAsDynArray<Double>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(1, v[0], tol);
  CheckEquals(3, v[1], tol);
  CheckEquals(5, v[2], tol);
end;

procedure TTensorF64Tests.Subtract_SV;
var a: TTensorF64;
    v: TArray<Double>;
begin
  a := TNDAUt.AsArray<Double>([2, 4, 6]);
  a := 7 - a;

  CheckTrue(TNDAUt.TryAsDynArray<Double>(a, v));

  CheckEquals(3, Length(v));
  CheckEquals(5, v[0], tol);
  CheckEquals(3, v[1], tol);
  CheckEquals(1, v[2], tol);
end;

procedure TTensorF64Tests.Subtract_VV;
var a, b: TTensorF64;
    v: TArray<Double>;
begin
  a := TNDAUt.AsArray<Double>([5, 7, 9]);
  b := TNDAUt.AsArray<Double>([1, 2, 3]);
  a := a - b;

  CheckTrue(TNDAUt.TryAsDynArray<Double>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(4, v[0], tol);
  CheckEquals(5, v[1], tol);
  CheckEquals(6, v[2], tol);
end;

procedure TTensorF64Tests.Subtract_MV;
var a, b: TTensorF64;
    m: TArray<TArray<Double>>;
begin
  a := TNDAUt.AsArray<Double>([[12, 24, 36], [48, 60, 72]]);
  b := TNDAUt.AsArray<Double>([2, 3, 4]);
  a := a - b;

  CheckTrue(TNDAUt.TryAsDynArray2D<Double>(a, m));
  CheckEquals(2, Length(m));
  CheckEquals([10, 21, 32], m[0], tol);
  CheckEquals([46, 57, 68], m[1], tol);
end;

procedure TTensorF64Tests.Subtract_VM;
var a, b: TTensorF64;
    m: TArray<TArray<Double>>;
begin
  a := TNDAUt.AsArray<Double>([2, 3, 4]);
  b := TNDAUt.AsArray<Double>([[12, 24, 36], [48, 60, 72]]);
  a := a - b;

  CheckTrue(TNDAUt.TryAsDynArray2D<Double>(a, m));
  CheckEquals(2, Length(m));
  CheckEquals([-10, -21, -32], m[0], tol);
  CheckEquals([-46, -57, -68], m[1], tol);
end;

procedure TTensorF64Tests.Multiply_VS;
var a: TTensorF64;
    v: TArray<Double>;
begin
  a := TNDAUt.AsArray<Double>([1, 2, 3]);
  a := a * 2;

  CheckTrue(TNDAUt.TryAsDynArray<Double>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(2, v[0], tol);
  CheckEquals(4, v[1], tol);
  CheckEquals(6, v[2], tol);
end;

procedure TTensorF64Tests.Multiply_SV;
var a: TTensorF64;
    v: TArray<Double>;
begin
  a := TNDAUt.AsArray<Double>([1, 2, 3]);
  a := 2 * a;

  CheckTrue(TNDAUt.TryAsDynArray<Double>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(2, v[0], tol);
  CheckEquals(4, v[1], tol);
  CheckEquals(6, v[2], tol);
end;

procedure TTensorF64Tests.Multiply_VV;
var a, b: TTensorF64;
    v: TArray<Double>;
begin
  a := TNDAUt.AsArray<Double>([1, 2, 3]);
  b := TNDAUt.AsArray<Double>([4, 5, 6]);
  a := a * b;

  CheckTrue(TNDAUt.TryAsDynArray<Double>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(4, v[0], tol);
  CheckEquals(10, v[1], tol);
  CheckEquals(18, v[2], tol);
end;

procedure TTensorF64Tests.Multiply_MV;
var a, b: TTensorF64;
    m: TArray<TArray<Double>>;
begin
  a := TNDAUt.AsArray<Double>([[12, 24, 36], [48, 60, 72]]);
  b := TNDAUt.AsArray<Double>([2, 3, 4]);
  a := a * b;

  CheckTrue(TNDAUt.TryAsDynArray2D<Double>(a, m));
  CheckEquals(2, Length(m));
  CheckEquals([24, 72, 144], m[0], tol);
  CheckEquals([96, 180, 288], m[1], tol);
end;

procedure TTensorF64Tests.Multiply_VM;
var a, b: TTensorF64;
    m: TArray<TArray<Double>>;
begin
  a := TNDAUt.AsArray<Double>([2, 3, 4]);
  b := TNDAUt.AsArray<Double>([[12, 24, 36], [48, 60, 72]]);
  a := a * b;

  CheckTrue(TNDAUt.TryAsDynArray2D<Double>(a, m));
  CheckEquals(2, Length(m));
  CheckEquals([24, 72, 144], m[0], tol);
  CheckEquals([96, 180, 288], m[1], tol);
end;

procedure TTensorF64Tests.Divide_VS;
var a: TTensorF64;
    v: TArray<Double>;
begin
  a := TNDAUt.AsArray<Double>([2, 4, 6]);
  a := a / 2;

  CheckTrue(TNDAUt.TryAsDynArray<Double>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(1, v[0], tol);
  CheckEquals(2, v[1], tol);
  CheckEquals(3, v[2], tol);
end;

procedure TTensorF64Tests.Divide_SV;
var a: TTensorF64;
    v: TArray<Double>;
begin
  a := TNDAUt.AsArray<Double>([1, 2, 4]);
  a := 8 / a;

  CheckTrue(TNDAUt.TryAsDynArray<Double>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(8, v[0], tol);
  CheckEquals(4, v[1], tol);
  CheckEquals(2, v[2], tol);
end;

procedure TTensorF64Tests.Divide_VV;
var a, b: TTensorF64;
    v: TArray<Double>;
begin
  a := TNDAUt.AsArray<Double>([2, 6, 12]);
  b := TNDAUt.AsArray<Double>([2, 3, 4]);
  a := a / b;

  CheckTrue(TNDAUt.TryAsDynArray<Double>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(1, v[0], tol);
  CheckEquals(2, v[1], tol);
  CheckEquals(3, v[2], tol);
end;

procedure TTensorF64Tests.Divide_MV;
var a, b: TTensorF64;
    m: TArray<TArray<Double>>;
begin
  a := TNDAUt.AsArray<Double>([[12, 24, 36], [48, 60, 72]]);
  b := TNDAUt.AsArray<Double>([2, 3, 4]);
  a := a / b;

  CheckTrue(TNDAUt.TryAsDynArray2D<Double>(a, m));
  CheckEquals(2, Length(m));
  CheckEquals([6, 8, 9], m[0], tol);
  CheckEquals([24, 20, 18], m[1], tol);
end;

procedure TTensorF64Tests.Divide_VM;
var a, b: TTensorF64;
    m: TArray<TArray<Double>>;
begin
  a := TNDAUt.AsArray<Double>([2, 3, 4]);
  b := TNDAUt.AsArray<Double>([[12, 24, 36], [48, 60, 72]]);
  a := a / b;

  CheckTrue(TNDAUt.TryAsDynArray2D<Double>(a, m));
  CheckEquals(2, Length(m));
  CheckEquals([1/6, 1/8, 1/9], m[0], tol);
  CheckEquals([1/24, 1/20, 1/18], m[1], tol);
end;

{$endregion}

{$region 'TTensorI32Tests'}

procedure TTensorI32Tests.Add_SS;
var a: TTensorI32;
    res: Integer;
begin
  a := nda.Scalar<Integer>(5);
  a := a + a;

  CheckTrue(nda.TryAsScalar<Integer>(a, res));
  CheckEquals(10, res);
end;

procedure TTensorI32Tests.Add_VS;
var a: TTensorI32;
    v: TArray<Integer>;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3]);
  a := a + 2;

  CheckTrue(TNDAUt.TryAsDynArray<Integer>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(3, v[0]);
  CheckEquals(4, v[1]);
  CheckEquals(5, v[2]);
end;

procedure TTensorI32Tests.Add_SV;
var a: TTensorI32;
    v: TArray<Integer>;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3]);
  a := 2 + a;

  CheckTrue(TNDAUt.TryAsDynArray<Integer>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(3, v[0]);
  CheckEquals(4, v[1]);
  CheckEquals(5, v[2]);
end;

procedure TTensorI32Tests.Add_VV;
var a: TTensorI32;
    b: TTensorI32;
    v: TArray<Integer>;
begin
  a := TNDAUt.AsArray<Integer>([2, 4, 6]);
  b := TNDAUt.AsArray<Integer>([4, 5, 6]);
  a := a + b;

  CheckTrue(TNDAUt.TryAsDynArray<Integer>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(6, v[0]);
  CheckEquals(9, v[1]);
  CheckEquals(12, v[2]);
end;

procedure TTensorI32Tests.Add_MV;
var a: TTensorI32;
    b: TTensorI32;
    m: TArray<TArray<Integer>>;
begin
  a := TNDAUt.AsArray<Integer>([[12, 24, 36], [48, 60, 72]]);
  b := TNDAUt.AsArray<Integer>([2, 3, 4]);
  a := a + b;

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, m));
  CheckEquals(2, Length(m));
  CheckEquals([14, 27, 40], m[0]);
  CheckEquals([50, 63, 76], m[1]);
end;

procedure TTensorI32Tests.Add_VM;
var a: TTensorI32;
    b: TTensorI32;
    m: TArray<TArray<Integer>>;
begin
  a := TNDAUt.AsArray<Integer>([2, 3, 4]);
  b := TNDAUt.AsArray<Integer>([[12, 24, 36], [48, 60, 72]]);
  a := a + b;

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, m));
  CheckEquals(2, Length(m));
  CheckEquals([14, 27, 40], m[0]);
  CheckEquals([50, 63, 76], m[1]);
end;

procedure TTensorI32Tests.Subtract_VS;
var a: TTensorI32;
    v: TArray<Integer>;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3]);
  a := a - 2;

  CheckTrue(TNDAUt.TryAsDynArray<Integer>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(-1, v[0]);
  CheckEquals(0, v[1]);
  CheckEquals(1, v[2]);
end;

procedure TTensorI32Tests.Subtract_SV;
var a: TTensorI32;
    v: TArray<Integer>;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3]);
  a := 2 - a;

  CheckTrue(TNDAUt.TryAsDynArray<Integer>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(1, v[0]);
  CheckEquals(0, v[1]);
  CheckEquals(-1, v[2]);
end;

procedure TTensorI32Tests.Subtract_VV;
var a: TTensorI32;
    b: TTensorI32;
    v: TArray<Integer>;
begin
  a := TNDAUt.AsArray<Integer>([2, 4, 6]);
  b := TNDAUt.AsArray<Integer>([4, 5, 6]);
  a := a - b;

  CheckTrue(TNDAUt.TryAsDynArray<Integer>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(-2, v[0]);
  CheckEquals(-1, v[1]);
  CheckEquals(0, v[2]);
end;

procedure TTensorI32Tests.Subtract_MV;
var a: TTensorI32;
    b: TTensorI32;
    m: TArray<TArray<Integer>>;
begin
  a := TNDAUt.AsArray<Integer>([[12, 24, 36], [48, 60, 72]]);
  b := TNDAUt.AsArray<Integer>([2, 3, 4]);
  a := a - b;

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, m));
  CheckEquals(2, Length(m));
  CheckEquals([10, 21, 32], m[0]);
  CheckEquals([46, 57, 68], m[1]);
end;

procedure TTensorI32Tests.Subtract_VM;
var a: TTensorI32;
    b: TTensorI32;
    m: TArray<TArray<Integer>>;
begin
  a := TNDAUt.AsArray<Integer>([2, 3, 4]);
  b := TNDAUt.AsArray<Integer>([[12, 24, 36], [48, 60, 72]]);
  a := a - b;

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, m));
  CheckEquals(2, Length(m));
  CheckEquals([-10, -21, -32], m[0]);
  CheckEquals([-46, -57, -68], m[1]);
end;

procedure TTensorI32Tests.Multiply_VS;
var a: TTensorI32;
    v: TArray<Integer>;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3]);
  a := a * 2;

  CheckTrue(TNDAUt.TryAsDynArray<Integer>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(2, v[0]);
  CheckEquals(4, v[1]);
  CheckEquals(6, v[2]);
end;

procedure TTensorI32Tests.Multiply_SV;
var a: TTensorI32;
    v: TArray<Integer>;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3]);
  a := 2 * a;

  CheckTrue(TNDAUt.TryAsDynArray<Integer>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(2, v[0]);
  CheckEquals(4, v[1]);
  CheckEquals(6, v[2]);
end;

procedure TTensorI32Tests.Multiply_VV;
var a: TTensorI32;
    b: TTensorI32;
    v: TArray<Integer>;
begin
  a := TNDAUt.AsArray<Integer>([2, 4, 6]);
  b := TNDAUt.AsArray<Integer>([4, 5, 6]);
  a := a * b;

  CheckTrue(TNDAUt.TryAsDynArray<Integer>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(8, v[0]);
  CheckEquals(20, v[1]);
  CheckEquals(36, v[2]);
end;

procedure TTensorI32Tests.Multiply_MV;
var a: TTensorI32;
    b: TTensorI32;
    m: TArray<TArray<Integer>>;
begin
  a := TNDAUt.AsArray<Integer>([[12, 24, 36], [48, 60, 72]]);
  b := TNDAUt.AsArray<Integer>([2, 3, 4]);
  a := a * b;

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, m));
  CheckEquals(2, Length(m));
  CheckEquals([24, 72, 144], m[0]);
  CheckEquals([96, 180, 288], m[1]);
end;

procedure TTensorI32Tests.Multiply_VM;
var a: TTensorI32;
    b: TTensorI32;
    m: TArray<TArray<Integer>>;
begin
  a := TNDAUt.AsArray<Integer>([2, 3, 4]);
  b := TNDAUt.AsArray<Integer>([[12, 24, 36], [48, 60, 72]]);
  a := a * b;

  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, m));
  CheckEquals(2, Length(m));
  CheckEquals([24, 72, 144], m[0]);
  CheckEquals([96, 180, 288], m[1]);
end;

{$endregion}

{$region 'TTensorI64Tests'}

procedure TTensorI64Tests.Add_VS;
var a: TTensorI64;
    v: TArray<Int64>;
begin
  a := TNDAUt.AsArray<Int64>([1, 2, 3]);
  a := a + 2;

  CheckTrue(TNDAUt.TryAsDynArray<Int64>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(3, v[0]);
  CheckEquals(4, v[1]);
  CheckEquals(5, v[2]);
end;

procedure TTensorI64Tests.Add_SV;
var a: TTensorI64;
    v: TArray<Int64>;
begin
  a := TNDAUt.AsArray<Int64>([1, 2, 3]);
  a := 2 + a;

  CheckTrue(TNDAUt.TryAsDynArray<Int64>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(3, v[0]);
  CheckEquals(4, v[1]);
  CheckEquals(5, v[2]);
end;

procedure TTensorI64Tests.Add_VV;
var a: TTensorI64;
    b: TTensorI64;
    v: TArray<Int64>;
begin
  a := TNDAUt.AsArray<Int64>([2, 4, 6]);
  b := TNDAUt.AsArray<Int64>([4, 5, 6]);
  a := a + b;

  CheckTrue(TNDAUt.TryAsDynArray<Int64>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(6, v[0]);
  CheckEquals(9, v[1]);
  CheckEquals(12, v[2]);
end;

procedure TTensorI64Tests.Add_MV;
var a: TTensorI64;
    b: TTensorI64;
    m: TArray<TArray<Int64>>;
begin
  a := TNDAUt.AsArray<Int64>([[12, 24, 36], [48, 60, 72]]);
  b := TNDAUt.AsArray<Int64>([2, 3, 4]);
  a := a + b;

  CheckTrue(TNDAUt.TryAsDynArray2D<Int64>(a, m));
  CheckEquals(2, Length(m));
  CheckEquals([14, 27, 40], m[0]);
  CheckEquals([50, 63, 76], m[1]);
end;

procedure TTensorI64Tests.Add_VM;
var a: TTensorI64;
    b: TTensorI64;
    m: TArray<TArray<Int64>>;
begin
  a := TNDAUt.AsArray<Int64>([2, 3, 4]);
  b := TNDAUt.AsArray<Int64>([[12, 24, 36], [48, 60, 72]]);
  a := a + b;

  CheckTrue(TNDAUt.TryAsDynArray2D<Int64>(a, m));
  CheckEquals(2, Length(m));
  CheckEquals([14, 27, 40], m[0]);
  CheckEquals([50, 63, 76], m[1]);
end;

procedure TTensorI64Tests.Subtract_VS;
var a: TTensorI64;
    v: TArray<Int64>;
begin
  a := TNDAUt.AsArray<Int64>([1, 2, 3]);
  a := a - 2;

  CheckTrue(TNDAUt.TryAsDynArray<Int64>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(-1, v[0]);
  CheckEquals(0, v[1]);
  CheckEquals(1, v[2]);
end;

procedure TTensorI64Tests.Subtract_SV;
var a: TTensorI64;
    v: TArray<Int64>;
begin
  a := TNDAUt.AsArray<Int64>([1, 2, 3]);
  a := 2 - a;

  CheckTrue(TNDAUt.TryAsDynArray<Int64>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(1, v[0]);
  CheckEquals(0, v[1]);
  CheckEquals(-1, v[2]);
end;

procedure TTensorI64Tests.Subtract_VV;
var a: TTensorI64;
    b: TTensorI64;
    v: TArray<Int64>;
begin
  a := TNDAUt.AsArray<Int64>([2, 4, 6]);
  b := TNDAUt.AsArray<Int64>([4, 5, 6]);
  a := a - b;

  CheckTrue(TNDAUt.TryAsDynArray<Int64>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(-2, v[0]);
  CheckEquals(-1, v[1]);
  CheckEquals(0, v[2]);
end;

procedure TTensorI64Tests.Subtract_MV;
var a: TTensorI64;
    b: TTensorI64;
    m: TArray<TArray<Int64>>;
begin
  a := TNDAUt.AsArray<Int64>([[12, 24, 36], [48, 60, 72]]);
  b := TNDAUt.AsArray<Int64>([2, 3, 4]);
  a := a - b;

  CheckTrue(TNDAUt.TryAsDynArray2D<Int64>(a, m));
  CheckEquals(2, Length(m));
  CheckEquals([10, 21, 32], m[0]);
  CheckEquals([46, 57, 68], m[1]);
end;

procedure TTensorI64Tests.Subtract_VM;
var a: TTensorI64;
    b: TTensorI64;
    m: TArray<TArray<Int64>>;
begin
  a := TNDAUt.AsArray<Int64>([2, 3, 4]);
  b := TNDAUt.AsArray<Int64>([[12, 24, 36], [48, 60, 72]]);
  a := a - b;

  CheckTrue(TNDAUt.TryAsDynArray2D<Int64>(a, m));
  CheckEquals(2, Length(m));
  CheckEquals([-10, -21, -32], m[0]);
  CheckEquals([-46, -57, -68], m[1]);
end;

procedure TTensorI64Tests.Multiply_VS;
var a: TTensorI64;
    v: TArray<Int64>;
begin
  a := TNDAUt.AsArray<Int64>([1, 2, 3]);
  a := a * 2;

  CheckTrue(TNDAUt.TryAsDynArray<Int64>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(2, v[0]);
  CheckEquals(4, v[1]);
  CheckEquals(6, v[2]);
end;

procedure TTensorI64Tests.Multiply_SV;
var a: TTensorI64;
    v: TArray<Int64>;
begin
  a := TNDAUt.AsArray<Int64>([1, 2, 3]);
  a := 2 * a;

  CheckTrue(TNDAUt.TryAsDynArray<Int64>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(2, v[0]);
  CheckEquals(4, v[1]);
  CheckEquals(6, v[2]);
end;

procedure TTensorI64Tests.Multiply_VV;
var a: TTensorI64;
    b: TTensorI64;
    v: TArray<Int64>;
begin
  a := TNDAUt.AsArray<Int64>([2, 4, 6]);
  b := TNDAUt.AsArray<Int64>([4, 5, 6]);
  a := a * b;

  CheckTrue(TNDAUt.TryAsDynArray<Int64>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(8, v[0]);
  CheckEquals(20, v[1]);
  CheckEquals(36, v[2]);
end;

procedure TTensorI64Tests.Multiply_MV;
var a: TTensorI64;
    b: TTensorI64;
    m: TArray<TArray<Int64>>;
begin
  a := TNDAUt.AsArray<Int64>([[12, 24, 36], [48, 60, 72]]);
  b := TNDAUt.AsArray<Int64>([2, 3, 4]);
  a := a * b;

  CheckTrue(TNDAUt.TryAsDynArray2D<Int64>(a, m));
  CheckEquals(2, Length(m));
  CheckEquals([24, 72, 144], m[0]);
  CheckEquals([96, 180, 288], m[1]);
end;

procedure TTensorI64Tests.Multiply_VM;
var a: TTensorI64;
    b: TTensorI64;
    m: TArray<TArray<Int64>>;
begin
  a := TNDAUt.AsArray<Int64>([2, 3, 4]);
  b := TNDAUt.AsArray<Int64>([[12, 24, 36], [48, 60, 72]]);
  a := a * b;

  CheckTrue(TNDAUt.TryAsDynArray2D<Int64>(a, m));
  CheckEquals(2, Length(m));
  CheckEquals([24, 72, 144], m[0]);
  CheckEquals([96, 180, 288], m[1]);
end;

{$endregion}

{$region 'TTensorF32Tests'}

procedure TTensorF32Tests.Add_VS;
var a: TTensorF32;
    v: TArray<Single>;
begin
  a := TNDAUt.AsArray<Single>([1, 2, 3]);
  a := a + 2;

  CheckTrue(TNDAUt.TryAsDynArray<Single>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(3, v[0], tol);
  CheckEquals(4, v[1], tol);
  CheckEquals(5, v[2], tol);
end;

procedure TTensorF32Tests.Add_SV;
var a: TTensorF32;
    v: TArray<Single>;
begin
  a := TNDAUt.AsArray<Single>([1, 2, 3]);
  a := 2 + a;

  CheckTrue(TNDAUt.TryAsDynArray<Single>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(3, v[0], tol);
  CheckEquals(4, v[1], tol);
  CheckEquals(5, v[2], tol);
end;

procedure TTensorF32Tests.Add_VV;
var a: TTensorF32;
    b: TTensorF32;
    v: TArray<Single>;
begin
  a := TNDAUt.AsArray<Single>([2, 4, 6]);
  b := TNDAUt.AsArray<Single>([4, 5, 6]);
  a := a + b;

  CheckTrue(TNDAUt.TryAsDynArray<Single>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(6, v[0], tol);
  CheckEquals(9, v[1], tol);
  CheckEquals(12, v[2], tol);
end;

procedure TTensorF32Tests.Add_MV;
var a: TTensorF32;
    b: TTensorF32;
    m: TArray<TArray<Single>>;
begin
  a := TNDAUt.AsArray<Single>([[12, 24, 36], [48, 60, 72]]);
  b := TNDAUt.AsArray<Single>([2, 3, 4]);
  a := a + b;

  CheckTrue(TNDAUt.TryAsDynArray2D<Single>(a, m));
  CheckEquals(2, Length(m));
  CheckEquals([14, 27, 40], m[0], tol);
  CheckEquals([50, 63, 76], m[1], tol);
end;

procedure TTensorF32Tests.Add_VM;
var a: TTensorF32;
    b: TTensorF32;
    m: TArray<TArray<Single>>;
begin
  a := TNDAUt.AsArray<Single>([2, 3, 4]);
  b := TNDAUt.AsArray<Single>([[12, 24, 36], [48, 60, 72]]);
  a := a + b;

  CheckTrue(TNDAUt.TryAsDynArray2D<Single>(a, m));
  CheckEquals(2, Length(m));
  CheckEquals([14, 27, 40], m[0], tol);
  CheckEquals([50, 63, 76], m[1], tol);
end;

procedure TTensorF32Tests.Subtract_VS;
var a: TTensorF32;
    v: TArray<Single>;
begin
  a := TNDAUt.AsArray<Single>([1, 2, 3]);
  a := a - 2;

  CheckTrue(TNDAUt.TryAsDynArray<Single>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(-1, v[0], tol);
  CheckEquals(0, v[1], tol);
  CheckEquals(1, v[2], tol);
end;

procedure TTensorF32Tests.Subtract_SV;
var a: TTensorF32;
    v: TArray<Single>;
begin
  a := TNDAUt.AsArray<Single>([1, 2, 3]);
  a := 2 - a;

  CheckTrue(TNDAUt.TryAsDynArray<Single>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(1, v[0], tol);
  CheckEquals(0, v[1], tol);
  CheckEquals(-1, v[2], tol);
end;

procedure TTensorF32Tests.Subtract_VV;
var a: TTensorF32;
    b: TTensorF32;
    v: TArray<Single>;
begin
  a := TNDAUt.AsArray<Single>([2, 4, 6]);
  b := TNDAUt.AsArray<Single>([4, 5, 6]);
  a := a - b;

  CheckTrue(TNDAUt.TryAsDynArray<Single>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(-2, v[0], tol);
  CheckEquals(-1, v[1], tol);
  CheckEquals(0, v[2], tol);
end;

procedure TTensorF32Tests.Subtract_MV;
var a: TTensorF32;
    b: TTensorF32;
    m: TArray<TArray<Single>>;
begin
  a := TNDAUt.AsArray<Single>([[12, 24, 36], [48, 60, 72]]);
  b := TNDAUt.AsArray<Single>([2, 3, 4]);
  a := a - b;

  CheckTrue(TNDAUt.TryAsDynArray2D<Single>(a, m));
  CheckEquals(2, Length(m));
  CheckEquals([10, 21, 32], m[0], tol);
  CheckEquals([46, 57, 68], m[1], tol);
end;

procedure TTensorF32Tests.Subtract_VM;
var a: TTensorF32;
    b: TTensorF32;
    m: TArray<TArray<Single>>;
begin
  a := TNDAUt.AsArray<Single>([2, 3, 4]);
  b := TNDAUt.AsArray<Single>([[12, 24, 36], [48, 60, 72]]);
  a := a - b;

  CheckTrue(TNDAUt.TryAsDynArray2D<Single>(a, m));
  CheckEquals(2, Length(m));
  CheckEquals([-10, -21, -32], m[0], tol);
  CheckEquals([-46, -57, -68], m[1], tol);
end;

procedure TTensorF32Tests.Multiply_VS;
var a: TTensorF32;
    v: TArray<Single>;
begin
  a := TNDAUt.AsArray<Single>([1, 2, 3]);
  a := a * 2;

  CheckTrue(TNDAUt.TryAsDynArray<Single>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(2, v[0], tol);
  CheckEquals(4, v[1], tol);
  CheckEquals(6, v[2], tol);
end;

procedure TTensorF32Tests.Multiply_SV;
var a: TTensorF32;
    v: TArray<Single>;
begin
  a := TNDAUt.AsArray<Single>([1, 2, 3]);
  a := 2 * a;

  CheckTrue(TNDAUt.TryAsDynArray<Single>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(2, v[0], tol);
  CheckEquals(4, v[1], tol);
  CheckEquals(6, v[2], tol);
end;

procedure TTensorF32Tests.Multiply_VV;
var a: TTensorF32;
    b: TTensorF32;
    v: TArray<Single>;
begin
  a := TNDAUt.AsArray<Single>([2, 4, 6]);
  b := TNDAUt.AsArray<Single>([4, 5, 6]);
  a := a * b;

  CheckTrue(TNDAUt.TryAsDynArray<Single>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(8, v[0], tol);
  CheckEquals(20, v[1], tol);
  CheckEquals(36, v[2], tol);
end;

procedure TTensorF32Tests.Multiply_MV;
var a: TTensorF32;
    b: TTensorF32;
    m: TArray<TArray<Single>>;
begin
  a := TNDAUt.AsArray<Single>([[12, 24, 36], [48, 60, 72]]);
  b := TNDAUt.AsArray<Single>([2, 3, 4]);
  a := a * b;

  CheckTrue(TNDAUt.TryAsDynArray2D<Single>(a, m));
  CheckEquals(2, Length(m));
  CheckEquals([24, 72, 144], m[0], tol);
  CheckEquals([96, 180, 288], m[1], tol);
end;

procedure TTensorF32Tests.Multiply_VM;
var a: TTensorF32;
    b: TTensorF32;
    m: TArray<TArray<Single>>;
begin
  a := TNDAUt.AsArray<Single>([2, 3, 4]);
  b := TNDAUt.AsArray<Single>([[12, 24, 36], [48, 60, 72]]);
  a := a * b;

  CheckTrue(TNDAUt.TryAsDynArray2D<Single>(a, m));
  CheckEquals(2, Length(m));
  CheckEquals([24, 72, 144], m[0], tol);
  CheckEquals([96, 180, 288], m[1], tol);
end;

procedure TTensorF32Tests.Divide_VS;
var a: TTensorF32;
    v: TArray<Single>;
begin
  a := TNDAUt.AsArray<Single>([1, 2, 3]);
  a := a / 2;

  CheckTrue(TNDAUt.TryAsDynArray<Single>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(0.5, v[0], tol);
  CheckEquals(1, v[1], tol);
  CheckEquals(1.5, v[2], tol);
end;

procedure TTensorF32Tests.Divide_SV;
var a: TTensorF32;
    v: TArray<Single>;
begin
  a := TNDAUt.AsArray<Single>([1, 2, 3]);
  a := 2 / a;

  CheckTrue(TNDAUt.TryAsDynArray<Single>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(2, v[0], tol);
  CheckEquals(1, v[1], tol);
  CheckEquals(0.6666666666666666, v[2], tol);
end;

procedure TTensorF32Tests.Divide_VV;
var a: TTensorF32;
    b: TTensorF32;
    v: TArray<Single>;
begin
  a := TNDAUt.AsArray<Single>([2, 4, 6]);
  b := TNDAUt.AsArray<Single>([4, 5, 6]);
  a := a / b;

  CheckTrue(TNDAUt.TryAsDynArray<Single>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(0.5, v[0], tol);
  CheckEquals(0.8, v[1], tol);
  CheckEquals(1, v[2], tol);
end;

procedure TTensorF32Tests.Divide_MV;
var a: TTensorF32;
    b: TTensorF32;
    m: TArray<TArray<Single>>;
begin
  a := TNDAUt.AsArray<Single>([[12, 24, 36], [48, 60, 72]]);
  b := TNDAUt.AsArray<Single>([2, 3, 4]);
  a := a / b;

  CheckTrue(TNDAUt.TryAsDynArray2D<Single>(a, m));
  CheckEquals(2, Length(m));
  CheckEquals([6, 8, 9], m[0], tol);
  CheckEquals([24, 20, 18], m[1], tol);
end;

procedure TTensorF32Tests.Divide_VM;
var a: TTensorF32;
    b: TTensorF32;
    m: TArray<TArray<Single>>;
begin
  a := TNDAUt.AsArray<Single>([2, 3, 4]);
  b := TNDAUt.AsArray<Single>([[12, 24, 36], [48, 60, 72]]);
  a := a / b;

  CheckTrue(TNDAUt.TryAsDynArray2D<Single>(a, m));
  CheckEquals(2, Length(m));
  CheckEquals([0.16666666666666666, 0.125, 0.1111111111111111], m[0], tol);
  CheckEquals([0.041666666666666664, 0.05, 0.05555555555555555], m[1], tol);
end;

procedure TTensorF32Tests.SubFrom_VS;
var a: TTensorF32;
    res: TArray<Single>;
    I: Integer;
begin
  a := nda.AsArray<Single>([1, 2, 3]);
  a.SubtractFrom(1);

  CheckTrue(nda.TryAsDynArray<Single>(a, res));
  CheckEquals(3, Length(res));
  for I := 0 to High(res) do
    CheckEquals(I, res[I], tol);
end;

procedure TTensorF32Tests.SubFrom_VV;
var a: TTensorF32;
    res: TArray<Single>;
    I: Integer;
begin
  a := nda.AsArray<Single>([1, 2, 3]);
  a.SubtractFrom(a/2);

  CheckTrue(nda.TryAsDynArray<Single>(a, res));
  CheckEquals(3, Length(res));
  for I := 0 to High(res) do
    CheckEquals((I + 1)/2, res[I], tol);
end;

procedure TTensorF32Tests.SubFrom_MV;
var a, b: TTensorF32;
    res: TArray<TArray<Single>>;
    I, J: Integer;
begin
  a := nda.AsArray<Single>([[1, 2, 3], [4, 5, 6]]);
  b := nda.AsArray<Single>([0, 1, 2]);
  a.SubtractFrom(b);

  CheckTrue(nda.TryAsDynArray2D<Single>(a, res));
  CheckEquals(2, Length(res));
  CheckEquals(3, Length(res[0]));
  CheckEquals(3, Length(res[1]));
  for I := 0 to High(res) do
    for J := 0 to High(res[I]) do
      CheckEquals(1 + 3*I, res[I, J], tol);
end;

procedure TTensorF32Tests.CvtInt32ToF32;
var a: TTensorI32;
    b: TTensorF32;
    res: TArray<Single>;
    I: Integer;
begin
  a := nda.AsArray<Integer>([1, 2, 3, 4, 5]);
  b := a;

  CheckTrue(nda.TryAsDynArray<Single>(b, res));
  for I := 0 to High(res) do
    CheckEquals(I + 1, res[I], tol);
end;

{$endregion}

{$region 'TArithmeticTests'}

procedure TArithmeticTests.AllClose1D;
var a, b: INDArray<Single>;
begin
  a := nda.AsArray<Single>([1, 2, 3]);
  b := nda.AsArray<Single>([1, 2, 3.1]);

  CheckTrue(ndaAllClose(a, b, 0.15));
  CheckFalse(ndaAllClose(a, b, 0.05));
end;

procedure TArithmeticTests.AllClose2D;
var a, b: INDArray<Single>;
begin
  a := nda.AsArray<Single>([[1, 2, 3], [4, 5, 6]]);
  b := nda.AsArray<Single>([[1, 2, 3.1], [4, 5, 6]]);

  CheckTrue(ndaAllClose(a, b, 0.15));
  CheckFalse(ndaAllClose(a, b, 0.05));
end;

procedure TArithmeticTests.AllClose1Ds;
var a: INDArray<Single>;
begin
  a := nda.AsArray<Single>([1.1, 0.9, 1]);

  CheckTrue(ndaAllClose(a, 1, 0.15));
  CheckFalse(ndaAllClose(a, 1, 0.05));
end;

procedure TArithmeticTests.AllClose2Ds;
var a: INDArray<Single>;
begin
  a := nda.AsArray<Single>([[1.1, 0.9, 1], [0.9, 1, 1.1], [1, 1.1, 0.9]]);

  CheckTrue(ndaAllClose(a, 1, 0.15));
  CheckFalse(ndaAllClose(a, 1, 0.05));
end;

procedure TArithmeticTests.AllClose2Ds_HGaps;
var a, av: INDArray<Single>;
begin
  a := nda.AsArray<Single>([[1.1, 0.9, 1], [0.9, 1, 1.1], [1, 1.1, 0.9]]);
  av := a[[NDIAll, NDIAll(2)]];

  CheckTrue(ndaAllClose(av, 1, 0.15));
  CheckFalse(ndaAllClose(av, 1, 0.05));
end;

procedure TArithmeticTests.AllClose3Ds_HGaps;
var a, av: INDArray<Single>;
begin
  a := nda.Range(1, 1.1, 0.1/(3*3*3)).Reshape([3, 3, 3]);
  av := a[[NDIAll, NDIAll, NDIAll(2)]];

  CheckTrue(ndaAllClose(av, 1, 0.15));
  CheckFalse(ndaAllClose(av, 1, 0.05));
end;

{$endregion}

{$region 'TBoolArithmeticTests'}

procedure TBoolArithmeticTests.BoolAnd1D;
var a, b: TTensorBool;
    v: TArray<Boolean>;
begin
  a := TNDAUt.AsArray<Boolean>([True, True, False, False]);
  b := TNDAUt.AsArray<Boolean>([True, False, True, False]);

  a := a and b;

  CheckTrue(TNDAUt.TryAsDynArray<Boolean>(a, v));
  CheckEquals([True, False, False, False], v);
end;

procedure TBoolArithmeticTests.BoolOr1D;
var a, b: TTensorBool;
    v: TArray<Boolean>;
begin
  a := TNDAUt.AsArray<Boolean>([True, True, False, False]);
  b := TNDAUt.AsArray<Boolean>([True, False, True, False]);

  a := a or b;

  CheckTrue(TNDAUt.TryAsDynArray<Boolean>(a, v));
  CheckEquals([True, True, True, False], v);
end;

procedure TBoolArithmeticTests.BoolXor1D;
var a, b: TTensorBool;
    v: TArray<Boolean>;
begin
  a := TNDAUt.AsArray<Boolean>([True, True, False, False]);
  b := TNDAUt.AsArray<Boolean>([True, False, True, False]);

  a := a xor b;

  CheckTrue(TNDAUt.TryAsDynArray<Boolean>(a, v));
  CheckEquals([False, True, True, False], v);
end;

procedure TBoolArithmeticTests.BoolNot1D;
var a: TTensorBool;
    v: TArray<Boolean>;
begin
  a := TNDAUt.AsArray<Boolean>([True, False, True, False]);

  a := not a;

  CheckTrue(TNDAUt.TryAsDynArray<Boolean>(a, v));
  CheckEquals([False, True, False, True], v);
end;


{$endregion}

initialization

  RegisterTest(TTensorF64Tests.Suite);
  RegisterTest(TTensorI32Tests.Suite);
  RegisterTest(TTensorI64Tests.Suite);
  RegisterTest(TTensorF32Tests.Suite);
  RegisterTest(TArithmeticTests.Suite);
  RegisterTest(TBoolArithmeticTests.Suite);

end.
