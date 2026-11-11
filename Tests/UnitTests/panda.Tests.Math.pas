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
  protected
    procedure CheckMatMul(aM, aK, aN: Integer);
    procedure CheckMatMulSingle(aM, aK, aN: Integer);
    procedure CheckMatMulParallel(aM, aK, aN, aMaxWorkers: Integer);
    procedure CheckMatMulParallelSingle(aM, aK, aN, aMaxWorkers: Integer);
    procedure CheckMatMulNative(aM, aK, aN: Integer; aTiled: Boolean);
    procedure CheckGramParallel(aH, aW, aMaxWorkers: Integer);
    procedure CheckGramParallelSingle(aH, aW, aMaxWorkers: Integer);
    procedure CheckGramNative(aWidth: Integer; aTiled, aPartial: Boolean);
  {$ifdef BLAS}
    procedure CheckGramSingleBLAS(aReverseWorkspace, aReverseResult, aParallel: Boolean);
    procedure CheckMatMulSingleBLAS(aReverseA, aReverseWorkspace, aReverseResult, aParallel: Boolean);
  {$endif}
  published
    procedure Range1DInt;
    procedure Range1DSingle;

    procedure Total1D;
    procedure Total2D;
    procedure Total2DLvl1;
    procedure Total3D;
    procedure Total3DLvl1;
    procedure Total3DLvl2;
    procedure TotalAll2D;
    procedure TotalAll3D;

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

    procedure GramNative_Dot_EmptyRange;
    procedure GramNative_Dot2x2_EmptyRange;
    procedure GramNative_Dot_OddWidth;
    procedure GramNative_Dot_EvenWidth;
    procedure GramNative_Dot_SingleColumn;
    procedure GramNative_Dot_PartialRanges;
    procedure GramNative_Dot2x2_OddWidth;
    procedure GramNative_Dot2x2_EvenWidth;
    procedure GramNative_Dot2x2_SingleColumn;
    procedure GramNative_Dot2x2_DiagonalTile;
    procedure GramNative_Dot2x2_PartialRanges;

    procedure Gram_5x4;
    procedure Gram_5x5;
    procedure Gram_1x1;
    procedure Gram_StridedInput;
    procedure Gram_PaddedBuffers;
    procedure Gram_ReversedWorkspace;
    procedure Gram_ReversedResult;
    procedure Gram_ReusedBuffers;

    procedure GramParallel_Workers1;
    procedure GramParallel_Workers2;
    procedure GramParallel_Workers4;
    procedure GramParallel_AutomaticWorkers;
    procedure GramParallel_1x1;
    procedure GramParallel_SingleColumn;
    procedure GramParallel_SingleTile;
    procedure GramParallel_OddWidth;
    procedure GramParallel_EvenWidth;
    procedure GramParallel_UnevenTiles;
    procedure GramParallel_BoundaryInsideRow;
    procedure GramParallel_ExcessWorkers;
    procedure GramParallel_SingleInputRow;
    procedure GramParallel_LargerProduct;
    procedure GramParallel_FixedValues;
    procedure GramParallel_AllocatedResult;
    procedure GramParallel_ReusedResult;
    procedure GramParallel_ReusedBuffers;
    procedure GramParallel_StridedInput;
    procedure GramParallel_TransposedInput;
    procedure GramParallel_ReversedInput;
    procedure GramParallel_ReversedRows;
    procedure GramParallel_PaddedBuffers;
    procedure GramParallel_ReversedBuffers;
    procedure GramParallel_NegativeWorkers;

    procedure GramSingle_NilInput;
    procedure GramSingle_VectorInput;
    procedure GramSingle_TensorInput;
  {$ifdef BLAS}
    procedure GramSingle_BLAS_ContiguousBuffers;
    procedure GramSingle_BLAS_ReversedWorkspace;
    procedure GramSingle_BLAS_ReversedResult;
    procedure GramParallelSingle_BLAS_UsesNative;
  {$endif}
    procedure GramSingle_5x4;
    procedure GramSingle_5x5;
    procedure GramSingle_1x1;
    procedure GramSingle_StridedInput;
    procedure GramSingle_PaddedBuffers;
    procedure GramSingle_ReversedWorkspace;
    procedure GramSingle_ReversedResult;
    procedure GramSingle_ReusedBuffers;

    procedure GramParallelSingle_Workers1;
    procedure GramParallelSingle_Workers2;
    procedure GramParallelSingle_Workers4;
    procedure GramParallelSingle_AutomaticWorkers;
    procedure GramParallelSingle_1x1;
    procedure GramParallelSingle_SingleColumn;
    procedure GramParallelSingle_TwoColumns;
    procedure GramParallelSingle_OddWidth;
    procedure GramParallelSingle_EvenWidth;
    procedure GramParallelSingle_UnevenTiles;
    procedure GramParallelSingle_BoundaryInsideRow;
    procedure GramParallelSingle_ExcessWorkers;
    procedure GramParallelSingle_SingleInputRow;
    procedure GramParallelSingle_LargerProduct;
    procedure GramParallelSingle_FixedValues;
    procedure GramParallelSingle_AllocatedResult;
    procedure GramParallelSingle_ReusedResult;
    procedure GramParallelSingle_ReusedBuffers;
    procedure GramParallelSingle_StridedInput;
    procedure GramParallelSingle_TransposedInput;
    procedure GramParallelSingle_ReversedInput;
    procedure GramParallelSingle_ReversedRows;
    procedure GramParallelSingle_PaddedBuffers;
    procedure GramParallelSingle_ReversedBuffers;
    procedure MatMulNative_Dot_EvenRowsEvenCols;
    procedure MatMulNative_Dot_EvenRowsOddCols;
    procedure MatMulNative_Dot_OddRowsEvenCols;
    procedure MatMulNative_Dot_OddRowsOddCols;
    procedure MatMulNative_Dot_SingleRow;
    procedure MatMulNative_Dot_SingleColumn;
    procedure MatMulNative_Dot_InnerDimension1;
    procedure MatMulNative_Dot_1x1;
    procedure MatMulNative_Dot2x2_EvenRowsEvenCols;
    procedure MatMulNative_Dot2x2_EvenRowsOddCols;
    procedure MatMulNative_Dot2x2_OddRowsEvenCols;
    procedure MatMulNative_Dot2x2_OddRowsOddCols;
    procedure MatMulNative_Dot2x2_SingleRow;
    procedure MatMulNative_Dot2x2_SingleColumn;
    procedure MatMulNative_Dot2x2_InnerDimension1;
    procedure MatMulNative_Dot2x2_1x1;

    procedure MatMul_EvenRowsEvenCols;
    procedure MatMul_EvenRowsOddCols;
    procedure MatMul_OddRowsEvenCols;
    procedure MatMul_OddRowsOddCols;
    procedure MatMul_1x1;
    procedure MatMul_SingleRow;
    procedure MatMul_SingleColumn;
    procedure MatMul_InnerDimension1;
    procedure MatMul_FixedValues;
    procedure MatMul_AllocatedResult;
    procedure MatMul_ReusedResult;
    procedure MatMul_ReusedBuffers;
    procedure MatMul_StridedInputs;
    procedure MatMul_TransposedInputs;
    procedure MatMul_ReversedInputs;
    procedure MatMul_ReversedRows;
    procedure MatMul_PaddedBuffers;
    procedure MatMul_ReversedWorkspace;
    procedure MatMul_ReversedResult;
    procedure MatMul_NilA;
    procedure MatMul_NilB;
    procedure MatMul_VectorA;
    procedure MatMul_VectorB;
    procedure MatMul_TensorA;
    procedure MatMul_TensorB;
    procedure MatMul_IncompatibleDimensions;

    procedure MatMulParallel_Workers1;
    procedure MatMulParallel_Workers2;
    procedure MatMulParallel_Workers4;
    procedure MatMulParallel_AutomaticWorkers;
    procedure MatMulParallel_EvenRows;
    procedure MatMulParallel_LargerProduct;
    procedure MatMulParallel_ExcessWorkers;
    procedure MatMulParallel_SingleRow;
    procedure MatMulParallel_SingleColumn;
    procedure MatMulParallel_InnerDimension1;
    procedure MatMulParallel_ReusedResult;
    procedure MatMulParallel_ReusedBuffers;
    procedure MatMulParallel_StridedInputs;
    procedure MatMulParallel_TransposedInputs;
    procedure MatMulParallel_ReversedInputs;
    procedure MatMulParallel_PaddedBuffers;
    procedure MatMulParallel_ReversedBuffers;
    procedure MatMulParallel_NegativeWorkers;
    procedure MatMulParallel_IncompatibleDimensions;
  {$ifdef BLAS}
    procedure MatMulSingle_BLAS_ContiguousBuffers;
    procedure MatMulSingle_BLAS_ReversedA;
    procedure MatMulSingle_BLAS_ReversedWorkspace;
    procedure MatMulSingle_BLAS_ReversedResult;
    procedure MatMulParallelSingle_BLAS_UsesNative;
  {$endif}
    procedure MatMulSingle_EvenRowsEvenCols;
    procedure MatMulSingle_EvenRowsOddCols;
    procedure MatMulSingle_OddRowsEvenCols;
    procedure MatMulSingle_OddRowsOddCols;
    procedure MatMulSingle_1x1;
    procedure MatMulSingle_SingleRow;
    procedure MatMulSingle_SingleColumn;
    procedure MatMulSingle_InnerDimension1;
    procedure MatMulSingle_FixedValues;
    procedure MatMulSingle_AllocatedResult;
    procedure MatMulSingle_ReusedResult;
    procedure MatMulSingle_ReusedBuffers;
    procedure MatMulSingle_StridedInputs;
    procedure MatMulSingle_TransposedInputs;
    procedure MatMulSingle_ReversedInputs;
    procedure MatMulSingle_ReversedRows;
    procedure MatMulSingle_PaddedBuffers;
    procedure MatMulSingle_ReversedWorkspace;
    procedure MatMulSingle_ReversedResult;

    procedure MatMulParallelSingle_Workers1;
    procedure MatMulParallelSingle_Workers2;
    procedure MatMulParallelSingle_Workers4;
    procedure MatMulParallelSingle_AutomaticWorkers;
    procedure MatMulParallelSingle_EvenRows;
    procedure MatMulParallelSingle_LargerProduct;
    procedure MatMulParallelSingle_ExcessWorkers;
    procedure MatMulParallelSingle_SingleRow;
    procedure MatMulParallelSingle_SingleColumn;
    procedure MatMulParallelSingle_InnerDimension1;
    procedure MatMulParallelSingle_ReusedResult;
    procedure MatMulParallelSingle_ReusedBuffers;
    procedure MatMulParallelSingle_StridedInputs;
    procedure MatMulParallelSingle_TransposedInputs;
    procedure MatMulParallelSingle_ReversedInputs;
    procedure MatMulParallelSingle_PaddedBuffers;
    procedure MatMulParallelSingle_ReversedBuffers;
    procedure MatMulParallelSingle_IncompatibleDimensions;
  end;

implementation

uses
    panda.ArrManip
  , panda.cvArithmetic
  , System.SysUtils
{$ifdef BLAS}
  , LibCBLAS
{$endif}
  ;

type
  TMathAccess = class(TNDAMath);

{$ifdef BLAS}
  TGramSSyrkProc = procedure (aOrder: Tcblas_ORDER; aUplo: Tcblas_UPLO;
    aTrans: Tcblas_TRANSPOSE; aN, aK: LongInt; aAlpha: Single; aA: PSingle;
    aLda: LongInt; aBeta: Single; aC: PSingle; aLdc: LongInt); cdecl;
  TSGemmProc = procedure (aOrder: Tcblas_ORDER; aTransA, aTransB: Tcblas_TRANSPOSE;
    aM, aN, aK: LongInt; aAlpha: Single; aA: PSingle; aLda: LongInt;
    aB: PSingle; aLdb: LongInt; aBeta: Single; aC: PSingle; aLdc: LongInt); cdecl;

var savedSSyrk: TGramSSyrkProc;
    ssyrkCalls: Integer;
    savedSGemm: TSGemmProc;
    sgemmCalls: Integer;

procedure CountedSGemm(aOrder: Tcblas_ORDER; aTransA, aTransB: Tcblas_TRANSPOSE;
  aM, aN, aK: LongInt; aAlpha: Single; aA: PSingle; aLda: LongInt;
  aB: PSingle; aLdb: LongInt; aBeta: Single; aC: PSingle; aLdc: LongInt); cdecl;
begin
  Inc(sgemmCalls);
  savedSGemm(aOrder, aTransA, aTransB, aM, aN, aK, aAlpha, aA, aLda,
    aB, aLdb, aBeta, aC, aLdc
  );
end;

procedure CountedSSyrk(aOrder: Tcblas_ORDER; aUplo: Tcblas_UPLO;
  aTrans: Tcblas_TRANSPOSE; aN, aK: LongInt; aAlpha: Single; aA: PSingle;
  aLda: LongInt; aBeta: Single; aC: PSingle; aLdc: LongInt); cdecl;
begin
  Inc(ssyrkCalls);
  savedSSyrk(aOrder, aUplo, aTrans, aN, aK, aAlpha, aA, aLda, aBeta, aC, aLdc);
end;
{$endif}

procedure TestDotF64(pX, pY: TNDA<Double>.PT; aCount: NativeInt; out aRes: Double);
begin
  dot(PDouble(pX), PDouble(pY), aCount, aRes);
end;

procedure TestDot2x2F64(pR00, pR01, pR10, pR11: TNDA<Double>.PT;
  aCount: NativeInt; out D00, D01, D10, D11: Double);
begin
  dot2x2(PDouble(pR00), PDouble(pR01), PDouble(pR10), PDouble(pR11),
    aCount, D00, D01, D10, D11
  );
end;

{$region 'TMathTests'}

procedure TMathTests.CheckGramNative(aWidth: Integer; aTiled, aPartial: Boolean);
var at, g: INDArray<Double>;
    src, dst: TNDAItems<Double>;
    I, J, tileSize, tileRows, total, first, count, split: Integer;

  procedure Execute(aFirst, aCount: Integer);
  begin
    if aTiled then
      TMathAccess.GramNative<Double>(at, g, aFirst, aCount, TestDotF64, TestDot2x2F64)
    else
      TMathAccess.GramNative<Double>(at, g, aFirst, aCount, TestDotF64);
  end;

  procedure CheckResult(aFirst, aCount: Integer);
  var R, C, K, tileRow, tileCol, tileIndex: Integer;
      expected: Double;
  begin
    for R := 0 to aWidth - 1 do
      for C := 0 to aWidth - 1 do begin
        if R < C then begin
          tileRow := R div tileSize;
          tileCol := C div tileSize;
        end else begin
          tileRow := C div tileSize;
          tileCol := R div tileSize;
        end;
        tileIndex := 0;
        for K := 0 to tileRow - 1 do
          Inc(tileIndex, tileRows - K);
        Inc(tileIndex, tileCol - tileRow);
        expected := -999;
        if (tileIndex >= aFirst) and (tileIndex < aFirst + aCount) then begin
          expected := 0;
          for K := 0 to 6 do
            expected := expected + src[[R, K]]*src[[C, K]];
        end;
        CheckEquals(expected, dst[[R, C]], 0);
      end;
  end;

begin
  at := nda.Empty<Double>([aWidth, 7]);
  src := at;
  for I := 0 to aWidth - 1 do
    for J := 0 to 6 do
      src[[I, J]] := (I*7 + J*3) mod 13 - 6;
  g := nda.Full<Double>([aWidth, aWidth], -999);
  dst := g;
  tileSize := 1;
  if aTiled then
    tileSize := 2;
  tileRows := (aWidth + tileSize - 1) div tileSize;
  total := tileRows*(tileRows + 1) div 2;

  if aPartial then begin
    first := 1;
    count := total - 2;
    split := count div 2;

    Execute(first, split);

    CheckResult(first, split);

    Execute(first + split, count - split);

    CheckResult(first, count);
  end else begin
    Execute(0, total);

    CheckResult(0, total);
  end;
end;

procedure TMathTests.GramNative_Dot_OddWidth;
begin
  CheckGramNative(5, False, False);
end;

procedure TMathTests.GramNative_Dot_EvenWidth;
begin
  CheckGramNative(6, False, False);
end;

procedure TMathTests.GramNative_Dot_SingleColumn;
begin
  CheckGramNative(1, False, False);
end;

procedure TMathTests.GramNative_Dot_PartialRanges;
begin
  CheckGramNative(5, False, True);
end;

procedure TMathTests.GramNative_Dot2x2_OddWidth;
begin
  CheckGramNative(5, True, False);
end;

procedure TMathTests.GramNative_Dot2x2_EvenWidth;
begin
  CheckGramNative(6, True, False);
end;

procedure TMathTests.GramNative_Dot2x2_SingleColumn;
begin
  CheckGramNative(1, True, False);
end;

procedure TMathTests.GramNative_Dot2x2_DiagonalTile;
begin
  CheckGramNative(2, True, False);
end;

procedure TMathTests.GramNative_Dot2x2_PartialRanges;
begin
  CheckGramNative(7, True, True);
end;

procedure TMathTests.GramNative_Dot_EmptyRange;
var at, g: INDArray<Double>;
begin
  at := nda.Full<Double>([3, 2], 1);
  g := nda.Full<Double>([3, 3], -999);

  TMathAccess.GramNative<Double>(at, g, 6, 0, TestDotF64);

  CheckTrue(ndaAllClose(nda.Full<Double>([3, 3], -999), g, 0));
end;

procedure TMathTests.GramNative_Dot2x2_EmptyRange;
var at, g: INDArray<Double>;
begin
  at := nda.Full<Double>([3, 2], 1);
  g := nda.Full<Double>([3, 3], -999);

  TMathAccess.GramNative<Double>(at, g, 3, 0, TestDotF64, TestDot2x2F64);

  CheckTrue(ndaAllClose(nda.Full<Double>([3, 3], -999), g, 0));
end;

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
  x := nda.AsArray<Single>([[1, 2, 3], [4, 5, 6]]);
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
  x := nda.AsArray<Single>([[1, 2, 3], [4, 5, 6]]);
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

procedure TMathTests.TotalAll2D;
var x: INDArray<Single>;
begin
  x := TNDAUt.AsArray<Single>([[1, 2, 3], [4, 5, 6]]);
  x := ndaTotalToLvl(x, 1);
  CheckTrue(ScalarQ(x));
  CheckEquals(21, PSingle(x.Data)^, tol);
end;

procedure TMathTests.TotalAll3D;
var x: INDArray<Single>;
begin
  x := TNDAUt.AsArray<Single>([[[1, 2, 3], [4, 5, 6]], [[7, 8, 9], [10, 11, 12]]]);
  x := ndaTotalToLvl(x, 2);
  CheckTrue(ScalarQ(x));
  CheckEquals(78, PSingle(x.Data)^, tol);
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

procedure TMathTests.Gram_5x4;
var x, g: INDArray<Double>;
    m, expected: TArray<TArray<Double>>;
    I: Integer;
begin
  x := nda.AsArray<Double>([
    [ 1,  2,  3,  4],
    [ 5,  6,  7,  8],
    [ 9, 10, 11, 12],
    [13, 14, 15, 16],
    [17, 18, 19, 20]
  ]);
  TNDAUt.TryAsDynArray2D<Double>(ndaDot(TNDAMan.Transpose<Double>(x),x), expected);

  g := ndaGram(x);

  CheckTrue(TNDAUt.TryAsDynArray2D<Double>(g, m));
  CheckEquals(Length(expected), Length(m));
  for I := 0 to High(expected) do
    CheckEquals(expected[I], m[I], tol);
end;

procedure TMathTests.Gram_5x5;
var x, g: INDArray<Double>;
    m, expected: TArray<TArray<Double>>;
    I: Integer;
begin
  x := nda.AsArray<Double>([
    [ 1,  2,  3,  4,  5],
    [ 6,  7,  8,  9, 10],
    [11, 12, 13, 14, 15],
    [16, 17, 18, 19, 20],
    [21, 22, 23, 24, 25]
  ]);
  TNDAUt.TryAsDynArray2D<Double>(ndaDot(TNDAMan.Transpose<Double>(x),x), expected);

  g := ndaGram(x);

  CheckTrue(TNDAUt.TryAsDynArray2D<Double>(g, m));
  CheckEquals(Length(expected), Length(m));
  for I := 0 to High(expected) do
    CheckEquals(expected[I], m[I], tol);
end;

procedure TMathTests.Gram_1x1;
var x, g: INDArray<Double>;
    m: TNDAItems<Double>;
begin
  x := nda.AsArray<Double>([[-3]]);

  g := ndaGram(x);

  m := g;
  CheckEquals([1, 1], g.Shape);
  CheckEquals(9, m[[0, 0]], tol);
end;

procedure TMathTests.Gram_StridedInput;
var x, g, expected: INDArray<Double>;
begin
  x := nda.AsArray<Double>([[1, 99, -2, 99, 3, 99], [4, 99, 5, 99, -6, 99]]);
  x := x[[NDIAll, NDIAll(2)]];
  expected := nda.AsArray<Double>([[17, 18, -21], [18, 29, -36], [-21, -36, 45]]);

  ndaGram(x, g);

  CheckTrue(ndaAllClose(expected, g, tol));
end;

procedure TMathTests.Gram_PaddedBuffers;
var x, g, at, gStorage, atStorage, expected: INDArray<Double>;
    values, workspace: TNDAItems<Double>;
    I: Integer;
begin
  x := nda.AsArray<Double>([[1, -2, 3], [4, 5, -6]]);
  gStorage := nda.Full<Double>([3, 5], -99);
  atStorage := nda.Full<Double>([3, 4], -99);
  g := gStorage[[NDIAll, NDIAllTo(2)]];
  at := atStorage[[NDIAll, NDIAllTo(1)]];
  expected := nda.AsArray<Double>([[17, 18, -21], [18, 29, -36], [-21, -36, 45]]);

  ndaGram(x, g, at);

  CheckTrue(ndaAllClose(expected, g, tol));
  CheckTrue(ndaAllClose(nda.AsArray<Double>([[1, 4], [-2, 5], [3, -6]]), at, tol));
  values := gStorage;
  workspace := atStorage;
  for I := 0 to 2 do begin
    CheckEquals(-99, values[[I, 3]], tol);
    CheckEquals(-99, values[[I, 4]], tol);
    CheckEquals(-99, workspace[[I, 2]], tol);
    CheckEquals(-99, workspace[[I, 3]], tol);
  end;
end;

procedure TMathTests.Gram_ReversedWorkspace;
var x, g, at, expected: INDArray<Double>;
begin
  x := nda.AsArray<Double>([[1, -2, 3], [4, 5, -6]]);
  at := nda.Empty<Double>([3, 2]);
  at := at[[NDIAll(-1), NDIAll]];
  expected := nda.AsArray<Double>([[17, 18, -21], [18, 29, -36], [-21, -36, 45]]);

  ndaGram(x, g, at);

  CheckTrue(ndaAllClose(expected, g, tol));
  CheckTrue(ndaAllClose(nda.AsArray<Double>([[1, 4], [-2, 5], [3, -6]]), at, tol));
end;

procedure TMathTests.Gram_ReversedResult;
var x, g, at, expected: INDArray<Double>;
begin
  x := nda.AsArray<Double>([[1, -2, 3], [4, 5, -6]]);
  g := nda.Empty<Double>([3, 3]);
  g := g[[NDIAll(-1), NDIAll]];
  expected := nda.AsArray<Double>([[17, 18, -21], [18, 29, -36], [-21, -36, 45]]);

  ndaGram(x, g, at);

  CheckTrue(ndaAllClose(expected, g, tol));
end;

procedure TMathTests.Gram_ReusedBuffers;
var x, g, at: INDArray<Double>;
    pG, pAt: PByte;
begin
  x := nda.AsArray<Double>([[1, 2], [3, 4]]);
  ndaGram(x, g, at);
  pG := g.Data;
  pAt := at.Data;
  x.SetItem([0, 0], 2);

  ndaGram(x, g, at);

  CheckTrue(g.Data = pG);
  CheckTrue(at.Data = pAt);
  CheckTrue(ndaAllClose(nda.AsArray<Double>([[13, 16], [16, 20]]), g, tol));
  CheckTrue(ndaAllClose(nda.AsArray<Double>([[2, 3], [2, 4]]), at, tol));
end;

{$endregion}

{$region 'ndaMatMul'}

procedure TMathTests.CheckMatMulNative(aM, aK, aN: Integer; aTiled: Boolean);
var a, bt, c, storage: INDArray<Double>;
    av, bv, cv, allValues: TNDAItems<Double>;
    I, J, K: Integer;
    expected: Double;
begin
  a := nda.Empty<Double>([aM, aK]);
  bt := nda.Empty<Double>([aN, aK]);
  av := a;
  bv := bt;
  for I := 0 to aM - 1 do
    for K := 0 to aK - 1 do
      av[[I, K]] := (I*7 + K*3) mod 13 - 6;
  for J := 0 to aN - 1 do
    for K := 0 to aK - 1 do
      bv[[J, K]] := (J*5 + K*11) mod 17 - 8;
  storage := nda.Full<Double>([aM, aN + 1], -999);
  c := storage[[NDIAll, NDIAllTo(aN - 1)]];

  if aTiled then
    TMathAccess.MatMulNative<Double>(a, bt, c, TestDotF64, TestDot2x2F64)
  else
    TMathAccess.MatMulNative<Double>(a, bt, c, TestDotF64);

  cv := c;
  for I := 0 to aM - 1 do
    for J := 0 to aN - 1 do begin
      expected := 0;
      for K := 0 to aK - 1 do
        expected := expected + av[[I, K]]*bv[[J, K]];
      CheckEquals(expected, cv[[I, J]], 0);
    end;
  allValues := storage;
  for I := 0 to aM - 1 do
    CheckEquals(-999, allValues[[I, aN]], 0);
end;

procedure TMathTests.MatMulNative_Dot_EvenRowsEvenCols;
begin
  CheckMatMulNative(4, 7, 6, False);
end;

procedure TMathTests.MatMulNative_Dot_EvenRowsOddCols;
begin
  CheckMatMulNative(4, 7, 5, False);
end;

procedure TMathTests.MatMulNative_Dot_OddRowsEvenCols;
begin
  CheckMatMulNative(3, 7, 6, False);
end;

procedure TMathTests.MatMulNative_Dot_OddRowsOddCols;
begin
  CheckMatMulNative(3, 7, 5, False);
end;

procedure TMathTests.MatMulNative_Dot_SingleRow;
begin
  CheckMatMulNative(1, 7, 5, False);
end;

procedure TMathTests.MatMulNative_Dot_SingleColumn;
begin
  CheckMatMulNative(3, 7, 1, False);
end;

procedure TMathTests.MatMulNative_Dot_InnerDimension1;
begin
  CheckMatMulNative(3, 1, 5, False);
end;

procedure TMathTests.MatMulNative_Dot_1x1;
begin
  CheckMatMulNative(1, 1, 1, False);
end;

procedure TMathTests.MatMulNative_Dot2x2_EvenRowsEvenCols;
begin
  CheckMatMulNative(4, 7, 6, True);
end;

procedure TMathTests.MatMulNative_Dot2x2_EvenRowsOddCols;
begin
  CheckMatMulNative(4, 7, 5, True);
end;

procedure TMathTests.MatMulNative_Dot2x2_OddRowsEvenCols;
begin
  CheckMatMulNative(3, 7, 6, True);
end;

procedure TMathTests.MatMulNative_Dot2x2_OddRowsOddCols;
begin
  CheckMatMulNative(3, 7, 5, True);
end;

procedure TMathTests.MatMulNative_Dot2x2_SingleRow;
begin
  CheckMatMulNative(1, 7, 5, True);
end;

procedure TMathTests.MatMulNative_Dot2x2_SingleColumn;
begin
  CheckMatMulNative(3, 7, 1, True);
end;

procedure TMathTests.MatMulNative_Dot2x2_InnerDimension1;
begin
  CheckMatMulNative(3, 1, 5, True);
end;

procedure TMathTests.MatMulNative_Dot2x2_1x1;
begin
  CheckMatMulNative(1, 1, 1, True);
end;

procedure TMathTests.CheckMatMul(aM, aK, aN: Integer);
var a, b, c, expected: INDArray<Double>;
    I, J: Integer;
begin
  a := nda.Empty<Double>([aM, aK]);
  b := nda.Empty<Double>([aK, aN]);
  for I := 0 to aM - 1 do
    for J := 0 to aK - 1 do
      a.SetItem([I, J], ((I*7 + J*3) mod 13 - 6) / 4);
  for I := 0 to aK - 1 do
    for J := 0 to aN - 1 do
      b.SetItem([I, J], ((I*5 + J*11) mod 17 - 8) / 8);
  expected := ndaDot(a, b);

  c := ndaMatMul(a, b);

  CheckEquals([aM, aN], c.Shape);
  CheckTrue(ndaAllClose(expected, c, tol));
end;

procedure TMathTests.MatMul_EvenRowsEvenCols;
begin
  CheckMatMul(4, 8, 6);
end;

procedure TMathTests.MatMul_EvenRowsOddCols;
begin
  CheckMatMul(4, 7, 3);
end;

procedure TMathTests.MatMul_OddRowsEvenCols;
begin
  CheckMatMul(3, 4, 6);
end;

procedure TMathTests.MatMul_OddRowsOddCols;
begin
  CheckMatMul(3, 5, 5);
end;

procedure TMathTests.MatMul_1x1;
var a, b, c: INDArray<Double>;
begin
  a := nda.AsArray<Double>([[-3]]);
  b := nda.AsArray<Double>([[2]]);

  c := ndaMatMul(a, b);

  CheckEquals([1, 1], c.Shape);
  CheckTrue(ndaAllClose(nda.AsArray<Double>([[-6]]), c, tol));
end;

procedure TMathTests.MatMul_SingleRow;
begin
  CheckMatMul(1, 3, 5);
end;

procedure TMathTests.MatMul_SingleColumn;
begin
  CheckMatMul(5, 3, 1);
end;

procedure TMathTests.MatMul_InnerDimension1;
begin
  CheckMatMul(3, 1, 5);
end;

procedure TMathTests.MatMul_FixedValues;
var a, b, c, expected: INDArray<Double>;
begin
  a := nda.AsArray<Double>([[1, -2, 3], [4, 5, -6]]);
  b := nda.AsArray<Double>([[2, -1], [1, 4], [-3, 2]]);
  expected := nda.AsArray<Double>([[-9, -3], [31, 4]]);

  c := ndaMatMul(a, b);

  CheckEquals([2, 2], c.Shape);
  CheckTrue(ndaAllClose(expected, c, tol));
end;

procedure TMathTests.MatMul_AllocatedResult;
var a, b, c: INDArray<Double>;
begin
  a := nda.AsArray<Double>([[1, -2, 3], [4, 5, -6]]);
  b := nda.AsArray<Double>([[2, -1], [1, 4], [-3, 2]]);

  ndaMatMul(a, b, c);

  CheckEquals([2, 2], c.Shape);
  CheckTrue(ndaAllClose(nda.AsArray<Double>([[-9, -3], [31, 4]]), c, tol));
end;

procedure TMathTests.MatMul_ReusedResult;
var a, b, c: INDArray<Double>;
    pC: PByte;
begin
  a := nda.AsArray<Double>([[1, -2, 3], [4, 5, -6]]);
  b := nda.AsArray<Double>([[2, -1], [1, 4], [-3, 2]]);
  c := nda.Full<Double>([2, 2], -99);
  pC := c.Data;

  ndaMatMul(a, b, c);

  CheckTrue(c.Data = pC);
  CheckTrue(ndaAllClose(nda.AsArray<Double>([[-9, -3], [31, 4]]), c, tol));
end;

procedure TMathTests.MatMul_ReusedBuffers;
var a, b, c, bt: INDArray<Double>;
    pC, pBt: PByte;
begin
  a := nda.AsArray<Double>([[1, -2, 3], [4, 5, -6]]);
  b := nda.AsArray<Double>([[2, -1], [1, 4], [-3, 2]]);
  ndaMatMul(a, b, c, bt);

  CheckEquals([2, 2], c.Shape);
  CheckEquals([2, 3], bt.Shape);
  CheckTrue(ndaAllClose(nda.AsArray<Double>([[-9, -3], [31, 4]]), c, tol));
  CheckTrue(ndaAllClose(TNDAMan.Transpose<Double>(b), bt, tol));

  pC := c.Data;
  pBt := bt.Data;
  a.SetItem([0, 0], 2);
  b.SetItem([0, 1], 3);

  ndaMatMul(a, b, c, bt);

  CheckTrue(c.Data = pC);
  CheckTrue(bt.Data = pBt);
  CheckTrue(ndaAllClose(nda.AsArray<Double>([[-7, 4], [31, 20]]), c, tol));
  CheckTrue(ndaAllClose(nda.AsArray<Double>([[2, 1, -3], [3, 4, 2]]), bt, tol));
end;

procedure TMathTests.MatMul_StridedInputs;
var a, b, c, bt: INDArray<Double>;
begin
  a := nda.AsArray<Double>([[1, 99, -2, 99, 3, 99], [4, 99, 5, 99, -6, 99]]);
  b := nda.AsArray<Double>([
    [2, 99, -1, 99], [99, 99, 99, 99],
    [1, 99, 4, 99], [99, 99, 99, 99],
    [-3, 99, 2, 99], [99, 99, 99, 99]
  ]);
  a := a[[NDIAll, NDIAll(2)]];
  b := b[[NDIAll(2), NDIAll(2)]];

  ndaMatMul(a, b, c, bt);

  CheckTrue(ndaAllClose(nda.AsArray<Double>([[-9, -3], [31, 4]]), c, tol));
  CheckTrue(ndaAllClose(nda.AsArray<Double>([[2, 1, -3], [-1, 4, 2]]), bt, tol));
  CheckTrue(ndaAllClose(nda.AsArray<Double>([[1, -2, 3], [4, 5, -6]]), a, tol));
end;

procedure TMathTests.MatMul_TransposedInputs;
var a, b, c: INDArray<Double>;
begin
  a := nda.AsArray<Double>([[1, 4], [-2, 5], [3, -6]]);
  b := nda.AsArray<Double>([[2, 1, -3], [-1, 4, 2]]);
  a := TNDAMan.TransposedView<Double>(a);
  b := TNDAMan.TransposedView<Double>(b);

  c := ndaMatMul(a, b);

  CheckTrue(ndaAllClose(nda.AsArray<Double>([[-9, -3], [31, 4]]), c, tol));
end;

procedure TMathTests.MatMul_ReversedInputs;
var a, b, c: INDArray<Double>;
begin
  a := nda.AsArray<Double>([[1, -2, 3], [4, 5, -6]]);
  b := nda.AsArray<Double>([[2, -1], [1, 4], [-3, 2]]);
  a := a[[NDIAll(-1), NDIAll(-1)]];
  b := b[[NDIAll(-1), NDIAll(-1)]];

  c := ndaMatMul(a, b);

  CheckTrue(ndaAllClose(nda.AsArray<Double>([[4, 31], [-3, -9]]), c, tol));
end;

procedure TMathTests.MatMul_ReversedRows;
var a, b, c: INDArray<Double>;
begin
  a := nda.AsArray<Double>([[1, -2, 3], [4, 5, -6]]);
  b := nda.AsArray<Double>([[2, -1], [1, 4], [-3, 2]]);
  a := a[[NDIAll(-1), NDIAll]];

  c := ndaMatMul(a, b);

  CheckTrue(ndaAllClose(nda.AsArray<Double>([[31, 4], [-9, -3]]), c, tol));
end;

procedure TMathTests.MatMul_PaddedBuffers;
var a, b, c, bt, cStorage, btStorage: INDArray<Double>;
    values, workspace: TNDAItems<Double>;
    pC, pBt: PByte;
    I: Integer;
begin
  a := nda.AsArray<Double>([[1, -2, 99], [4, 5, 99], [-3, 6, 99]]);
  a := a[[NDIAll, NDIAllTo(1)]];
  b := nda.AsArray<Double>([[2, -1, 3], [1, 4, -2]]);
  cStorage := nda.Full<Double>([3, 5], -99);
  btStorage := nda.Full<Double>([3, 4], -99);
  c := cStorage[[NDIAll, NDIAllTo(2)]];
  bt := btStorage[[NDIAll, NDIAllTo(1)]];
  pC := c.Data;
  pBt := bt.Data;

  ndaMatMul(a, b, c, bt);

  CheckTrue(c.Data = pC);
  CheckTrue(bt.Data = pBt);
  CheckTrue(ndaAllClose(nda.AsArray<Double>([[0, -9, 7], [13, 16, 2], [0, 27, -21]]), c, tol));
  CheckTrue(ndaAllClose(TNDAMan.Transpose<Double>(b), bt, tol));
  values := cStorage;
  workspace := btStorage;
  for I := 0 to 2 do begin
    CheckEquals(-99, values[[I, 3]], tol);
    CheckEquals(-99, values[[I, 4]], tol);
    CheckEquals(-99, workspace[[I, 2]], tol);
    CheckEquals(-99, workspace[[I, 3]], tol);
  end;
end;

procedure TMathTests.MatMul_ReversedWorkspace;
var a, b, c, bt: INDArray<Double>;
    pBt: PByte;
begin
  a := nda.AsArray<Double>([[1, -2], [4, 5], [-3, 6]]);
  b := nda.AsArray<Double>([[2, -1, 3], [1, 4, -2]]);
  bt := nda.Empty<Double>([3, 2]);
  bt := bt[[NDIAll(-1), NDIAll]];
  pBt := bt.Data;

  ndaMatMul(a, b, c, bt);

  CheckTrue(bt.Data = pBt);
  CheckTrue(ndaAllClose(nda.AsArray<Double>([[0, -9, 7], [13, 16, 2], [0, 27, -21]]), c, tol));
  CheckTrue(ndaAllClose(TNDAMan.Transpose<Double>(b), bt, tol));
end;

procedure TMathTests.MatMul_ReversedResult;
var a, b, c, bt: INDArray<Double>;
    pC: PByte;
begin
  a := nda.AsArray<Double>([[1, -2], [4, 5], [-3, 6]]);
  b := nda.AsArray<Double>([[2, -1, 3], [1, 4, -2]]);
  c := nda.Empty<Double>([3, 3]);
  c := c[[NDIAll(-1), NDIAll]];
  pC := c.Data;

  ndaMatMul(a, b, c, bt);

  CheckTrue(c.Data = pC);
  CheckTrue(ndaAllClose(nda.AsArray<Double>([[0, -9, 7], [13, 16, 2], [0, 27, -21]]), c, tol));
end;

procedure TMathTests.MatMul_NilA;
var a, b, c, bt: INDArray<Double>;
begin
  b := nda.Empty<Double>([2, 3]);

  try
    ndaMatMul(a, b, c, bt);
    Fail('Expected ENDAShapeError.');
  except
    on E: ENDAShapeError do begin
      CheckFalse(Assigned(c));
      CheckFalse(Assigned(bt));
    end;
  end;
end;

procedure TMathTests.MatMul_NilB;
var a, b, c, bt: INDArray<Double>;
begin
  a := nda.Empty<Double>([2, 3]);

  try
    ndaMatMul(a, b, c, bt);
    Fail('Expected ENDAShapeError.');
  except
    on E: ENDAShapeError do begin
      CheckFalse(Assigned(c));
      CheckFalse(Assigned(bt));
    end;
  end;
end;

procedure TMathTests.MatMul_VectorA;
var a, b, c: INDArray<Double>;
begin
  a := nda.Empty<Double>([2]);
  b := nda.Empty<Double>([2, 3]);
  ExpectedException := ENDAShapeError;

  c := ndaMatMul(a, b);
end;

procedure TMathTests.MatMul_VectorB;
var a, b, c: INDArray<Double>;
begin
  a := nda.Empty<Double>([3, 2]);
  b := nda.Empty<Double>([2]);
  ExpectedException := ENDAShapeError;

  c := ndaMatMul(a, b);
end;

procedure TMathTests.MatMul_TensorA;
var a, b, c: INDArray<Double>;
begin
  a := nda.Empty<Double>([2, 3, 4]);
  b := nda.Empty<Double>([4, 2]);
  ExpectedException := ENDAShapeError;

  c := ndaMatMul(a, b);
end;

procedure TMathTests.MatMul_TensorB;
var a, b, c: INDArray<Double>;
begin
  a := nda.Empty<Double>([2, 3]);
  b := nda.Empty<Double>([3, 4, 2]);
  ExpectedException := ENDAShapeError;

  c := ndaMatMul(a, b);
end;

procedure TMathTests.MatMul_IncompatibleDimensions;
var a, b, c, bt: INDArray<Double>;
begin
  a := nda.Empty<Double>([2, 3]);
  b := nda.Empty<Double>([4, 2]);

  try
    ndaMatMul(a, b, c, bt);
    Fail('Expected ENDAShapeError.');
  except
    on E: ENDAShapeError do begin
      CheckFalse(Assigned(c));
      CheckFalse(Assigned(bt));
    end;
  end;
end;


{$endregion}

{$region 'ndaMatMulParallel'}

procedure TMathTests.CheckMatMulParallel(aM, aK, aN, aMaxWorkers: Integer);
var a, b, c, serial, expected: INDArray<Double>;
    I, J: Integer;
begin
  a := nda.Empty<Double>([aM, aK]);
  b := nda.Empty<Double>([aK, aN]);
  for I := 0 to aM - 1 do
    for J := 0 to aK - 1 do
      a.SetItem([I, J], ((I*7 + J*3) mod 13 - 6) / 4);
  for I := 0 to aK - 1 do
    for J := 0 to aN - 1 do
      b.SetItem([I, J], ((I*5 + J*11) mod 17 - 8) / 8);
  expected := ndaDot(a, b);
  serial := ndaMatMulParallel(a, b, 1);

  c := ndaMatMulParallel(a, b, aMaxWorkers);

  CheckEquals([aM, aN], c.Shape);
  CheckTrue(ndaAllClose(expected, c, tol));
  CheckTrue(ndaAllClose(serial, c, tol));
end;

procedure TMathTests.MatMulParallel_Workers1;
begin
  CheckMatMulParallel(11, 7, 5, 1);
end;

procedure TMathTests.MatMulParallel_Workers2;
begin
  CheckMatMulParallel(11, 7, 5, 2);
end;

procedure TMathTests.MatMulParallel_Workers4;
begin
  CheckMatMulParallel(11, 7, 5, 4);
end;

procedure TMathTests.MatMulParallel_AutomaticWorkers;
begin
  CheckMatMulParallel(11, 7, 5, 0);
end;

procedure TMathTests.MatMulParallel_EvenRows;
begin
  CheckMatMulParallel(14, 8, 6, 4);
end;

procedure TMathTests.MatMulParallel_LargerProduct;
begin
  CheckMatMulParallel(257, 129, 65, 4);
end;

procedure TMathTests.MatMulParallel_ExcessWorkers;
begin
  CheckMatMulParallel(3, 3, 4, 100);
end;

procedure TMathTests.MatMulParallel_SingleRow;
begin
  CheckMatMulParallel(1, 3, 5, 4);
end;

procedure TMathTests.MatMulParallel_SingleColumn;
begin
  CheckMatMulParallel(9, 3, 1, 4);
end;

procedure TMathTests.MatMulParallel_InnerDimension1;
begin
  CheckMatMulParallel(9, 1, 7, 4);
end;

procedure TMathTests.MatMulParallel_ReusedResult;
var a, b, c: INDArray<Double>;
    pC: PByte;
begin
  a := nda.AsArray<Double>([[1, -2], [4, 5], [-3, 6]]);
  b := nda.AsArray<Double>([[2, -1, 3], [1, 4, -2]]);
  c := nda.Full<Double>([3, 3], -99);
  pC := c.Data;

  ndaMatMulParallel(a, b, c, 2);

  CheckTrue(c.Data = pC);
  CheckTrue(ndaAllClose(nda.AsArray<Double>([[0, -9, 7], [13, 16, 2], [0, 27, -21]]), c, tol));
end;

procedure TMathTests.MatMulParallel_ReusedBuffers;
var a, b, c, bt, expected: INDArray<Double>;
    pC, pBt: PByte;
    I: Integer;
begin
  a := nda.AsArray<Double>([[1, -2], [4, 5], [-3, 6]]);
  b := nda.AsArray<Double>([[2, -1, 3], [1, 4, -2]]);
  ndaMatMulParallel(a, b, c, bt);
  pC := c.Data;
  pBt := bt.Data;

  CheckEquals([3, 3], c.Shape);
  CheckEquals([3, 2], bt.Shape);
  CheckTrue(ndaAllClose(ndaDot(a, b), c, tol));
  CheckTrue(ndaAllClose(TNDAMan.Transpose<Double>(b), bt, tol));

  for I := 1 to 10 do begin
    a.SetItem([0, 0], I);
    b.SetItem([0, 1], -I);
    expected := ndaDot(a, b);

    ndaMatMulParallel(a, b, c, bt, 4);

    CheckTrue(c.Data = pC);
    CheckTrue(bt.Data = pBt);
    CheckTrue(ndaAllClose(expected, c, tol));
    CheckTrue(ndaAllClose(TNDAMan.Transpose<Double>(b), bt, tol));
  end;
end;

procedure TMathTests.MatMulParallel_StridedInputs;
var a, b, c, bt, expected, originalA, originalB: INDArray<Double>;
begin
  a := nda.AsArray<Double>([[1, 99, -2, 99], [4, 99, 5, 99], [-3, 99, 6, 99]]);
  b := nda.AsArray<Double>([[2, 99, -1, 99, 3, 99], [1, 99, 4, 99, -2, 99]]);
  a := a[[NDIAll, NDIAll(2)]];
  b := b[[NDIAll, NDIAll(2)]];
  originalA := TNDAUt.Copy<Double>(a);
  originalB := TNDAUt.Copy<Double>(b);
  expected := ndaDot(originalA, originalB);

  ndaMatMulParallel(a, b, c, bt, 2);

  CheckTrue(ndaAllClose(expected, c, tol));
  CheckTrue(ndaAllClose(TNDAMan.Transpose<Double>(originalB), bt, tol));
  CheckTrue(ndaAllClose(originalA, a, tol));
  CheckTrue(ndaAllClose(originalB, b, tol));
end;

procedure TMathTests.MatMulParallel_TransposedInputs;
var a, b, c: INDArray<Double>;
begin
  a := nda.AsArray<Double>([[1, 4, -3], [-2, 5, 6]]);
  b := nda.AsArray<Double>([[2, 1], [-1, 4], [3, -2]]);
  a := TNDAMan.TransposedView<Double>(a);
  b := TNDAMan.TransposedView<Double>(b);

  c := ndaMatMulParallel(a, b, 2);

  CheckTrue(ndaAllClose(nda.AsArray<Double>([[0, -9, 7], [13, 16, 2], [0, 27, -21]]), c, tol));
end;

procedure TMathTests.MatMulParallel_ReversedInputs;
var a, b, c, expected: INDArray<Double>;
begin
  a := nda.AsArray<Double>([[1, -2], [4, 5], [-3, 6]]);
  b := nda.AsArray<Double>([[2, -1, 3], [1, 4, -2]]);
  a := a[[NDIAll(-1), NDIAll(-1)]];
  b := b[[NDIAll(-1), NDIAll(-1)]];
  expected := nda.AsArray<Double>([[-21, 27, 0], [2, 16, 13], [7, -9, 0]]);

  c := ndaMatMulParallel(a, b, 2);

  CheckTrue(ndaAllClose(expected, c, tol));
end;

procedure TMathTests.MatMulParallel_PaddedBuffers;
var a, b, c, bt, cStorage, btStorage: INDArray<Double>;
    values, workspace: TNDAItems<Double>;
    I: Integer;
begin
  a := nda.AsArray<Double>([[1, -2, 99], [4, 5, 99], [-3, 6, 99]]);
  a := a[[NDIAll, NDIAllTo(1)]];
  b := nda.AsArray<Double>([[2, -1, 3], [1, 4, -2]]);
  cStorage := nda.Full<Double>([3, 5], -99);
  btStorage := nda.Full<Double>([3, 4], -99);
  c := cStorage[[NDIAll, NDIAllTo(2)]];
  bt := btStorage[[NDIAll, NDIAllTo(1)]];

  ndaMatMulParallel(a, b, c, bt, 2);

  CheckTrue(ndaAllClose(nda.AsArray<Double>([[0, -9, 7], [13, 16, 2], [0, 27, -21]]), c, tol));
  CheckTrue(ndaAllClose(TNDAMan.Transpose<Double>(b), bt, tol));
  values := cStorage;
  workspace := btStorage;
  for I := 0 to 2 do begin
    CheckEquals(-99, values[[I, 3]], tol);
    CheckEquals(-99, values[[I, 4]], tol);
    CheckEquals(-99, workspace[[I, 2]], tol);
    CheckEquals(-99, workspace[[I, 3]], tol);
  end;
end;

procedure TMathTests.MatMulParallel_ReversedBuffers;
var a, b, c, bt: INDArray<Double>;
    pC, pBt: PByte;
begin
  a := nda.AsArray<Double>([[1, -2], [4, 5], [-3, 6]]);
  a := a[[NDIAll(-1), NDIAll]];
  b := nda.AsArray<Double>([[2, -1, 3], [1, 4, -2]]);
  c := nda.Empty<Double>([3, 3]);
  c := c[[NDIAll(-1), NDIAll]];
  bt := nda.Empty<Double>([3, 2]);
  bt := bt[[NDIAll(-1), NDIAll]];
  pC := c.Data;
  pBt := bt.Data;

  ndaMatMulParallel(a, b, c, bt, 2);

  CheckTrue(c.Data = pC);
  CheckTrue(bt.Data = pBt);
  CheckTrue(ndaAllClose(nda.AsArray<Double>([[0, 27, -21], [13, 16, 2], [0, -9, 7]]), c, tol));
  CheckTrue(ndaAllClose(TNDAMan.Transpose<Double>(b), bt, tol));
end;

procedure TMathTests.MatMulParallel_NegativeWorkers;
var a, b, c, bt: INDArray<Double>;
begin
  a := nda.Empty<Double>([3, 2]);
  b := nda.Empty<Double>([2, 3]);

  try
    ndaMatMulParallel(a, b, c, bt, -1);
    Fail('Expected EArgumentOutOfRangeException.');
  except
    on E: EArgumentOutOfRangeException do begin
      CheckFalse(Assigned(c));
      CheckFalse(Assigned(bt));
    end;
  end;
end;

procedure TMathTests.MatMulParallel_IncompatibleDimensions;
var a, b, c, bt: INDArray<Double>;
begin
  a := nda.Empty<Double>([3, 2]);
  b := nda.Empty<Double>([3, 4]);

  try
    ndaMatMulParallel(a, b, c, bt, 2);
    Fail('Expected ENDAShapeError.');
  except
    on E: ENDAShapeError do begin
      CheckFalse(Assigned(c));
      CheckFalse(Assigned(bt));
    end;
  end;
end;


{$endregion}

{$region 'ndaGramParallel'}

procedure TMathTests.CheckGramParallel(aH, aW, aMaxWorkers: Integer);
var a, g, expected, serial, legacy: INDArray<Double>;
    values: TNDAItems<Double>;
    I, J: Integer;
begin
  a := nda.Empty<Double>([aH, aW]);
  for I := 0 to aH - 1 do
    for J := 0 to aW - 1 do
      a.SetItem([I, J], ((I*7 + J*3) mod 13 - 6) / 4);
  expected := ndaDot(TNDAMan.Transpose<Double>(a), a);
  serial := ndaGramParallel(a, 1);
  legacy := ndaGram(a);

  g := ndaGramParallel(a, aMaxWorkers);

  CheckEquals([aW, aW], g.Shape);
  CheckTrue(ndaAllClose(expected, g, tol));
  CheckTrue(ndaAllClose(serial, g, tol));
  CheckTrue(ndaAllClose(legacy, g, tol));
  values := g;
  for I := 0 to aW - 1 do
    for J := I + 1 to aW - 1 do
      CheckEquals(values[[I, J]], values[[J, I]], 0);
end;

procedure TMathTests.GramParallel_Workers1;
begin
  CheckGramParallel(7, 9, 1);
end;

procedure TMathTests.GramParallel_Workers2;
begin
  CheckGramParallel(7, 9, 2);
end;

procedure TMathTests.GramParallel_Workers4;
begin
  CheckGramParallel(7, 9, 4);
end;

procedure TMathTests.GramParallel_AutomaticWorkers;
begin
  CheckGramParallel(7, 9, 0);
end;

procedure TMathTests.GramParallel_1x1;
begin
  CheckGramParallel(1, 1, 4);
end;

procedure TMathTests.GramParallel_SingleColumn;
begin
  CheckGramParallel(5, 1, 4);
end;

procedure TMathTests.GramParallel_SingleTile;
begin
  CheckGramParallel(3, 2, 4);
end;

procedure TMathTests.GramParallel_OddWidth;
begin
  CheckGramParallel(4, 3, 2);
end;

procedure TMathTests.GramParallel_EvenWidth;
begin
  CheckGramParallel(5, 4, 2);
end;

procedure TMathTests.GramParallel_UnevenTiles;
begin
  CheckGramParallel(8, 5, 4);
end;

procedure TMathTests.GramParallel_BoundaryInsideRow;
begin
  CheckGramParallel(9, 17, 4);
end;

procedure TMathTests.GramParallel_ExcessWorkers;
begin
  CheckGramParallel(3, 3, 100);
end;

procedure TMathTests.GramParallel_SingleInputRow;
begin
  CheckGramParallel(1, 7, 4);
end;

procedure TMathTests.GramParallel_LargerProduct;
begin
  CheckGramParallel(129, 65, 4);
end;

procedure TMathTests.GramParallel_FixedValues;
var a, g: INDArray<Double>;
begin
  a := nda.AsArray<Double>([[1, -2, 3], [4, 5, -6]]);

  g := ndaGramParallel(a, 2);

  CheckTrue(ndaAllClose(nda.AsArray<Double>([[17, 18, -21], [18, 29, -36], [-21, -36, 45]]), g, tol));
end;

procedure TMathTests.GramParallel_AllocatedResult;
var a, g: INDArray<Double>;
begin
  a := nda.AsArray<Double>([[1, -2, 3], [4, 5, -6]]);

  ndaGramParallel(a, g, 2);

  CheckEquals([3, 3], g.Shape);
  CheckTrue(ndaAllClose(nda.AsArray<Double>([[17, 18, -21], [18, 29, -36], [-21, -36, 45]]), g, tol));
end;

procedure TMathTests.GramParallel_ReusedResult;
var a, g: INDArray<Double>;
    pG: PByte;
begin
  a := nda.AsArray<Double>([[1, -2, 3], [4, 5, -6]]);
  g := nda.Full<Double>([3, 3], -99);
  pG := g.Data;

  ndaGramParallel(a, g, 2);

  CheckTrue(g.Data = pG);
  CheckTrue(ndaAllClose(nda.AsArray<Double>([[17, 18, -21], [18, 29, -36], [-21, -36, 45]]), g, tol));
end;

procedure TMathTests.GramParallel_ReusedBuffers;
var a, g, at, expected: INDArray<Double>;
    pG, pAt: PByte;
    I: Integer;
begin
  a := nda.AsArray<Double>([[1, -2, 3], [4, 5, -6]]);
  ndaGramParallel(a, g, at);
  pG := g.Data;
  pAt := at.Data;

  CheckEquals([3, 3], g.Shape);
  CheckEquals([3, 2], at.Shape);
  CheckTrue(ndaAllClose(ndaDot(TNDAMan.Transpose<Double>(a), a), g, tol));
  CheckTrue(ndaAllClose(TNDAMan.Transpose<Double>(a), at, tol));

  for I := 1 to 10 do begin
    a.SetItem([0, 0], I);
    expected := ndaDot(TNDAMan.Transpose<Double>(a), a);

    ndaGramParallel(a, g, at, 4);

    CheckTrue(g.Data = pG);
    CheckTrue(at.Data = pAt);
    CheckTrue(ndaAllClose(expected, g, tol));
    CheckTrue(ndaAllClose(TNDAMan.Transpose<Double>(a), at, tol));
  end;
end;

procedure TMathTests.GramParallel_StridedInput;
var a, g, at, original, expected: INDArray<Double>;
begin
  a := nda.AsArray<Double>([[1, 99, -2, 99, 3, 99], [4, 99, 5, 99, -6, 99]]);
  a := a[[NDIAll, NDIAll(2)]];
  original := TNDAUt.Copy<Double>(a);
  expected := ndaDot(TNDAMan.Transpose<Double>(original), original);

  ndaGramParallel(a, g, at, 2);

  CheckTrue(ndaAllClose(expected, g, tol));
  CheckTrue(ndaAllClose(TNDAMan.Transpose<Double>(original), at, tol));
  CheckTrue(ndaAllClose(original, a, tol));
end;

procedure TMathTests.GramParallel_TransposedInput;
var a, g: INDArray<Double>;
begin
  a := nda.AsArray<Double>([[1, 4], [-2, 5], [3, -6]]);
  a := TNDAMan.TransposedView<Double>(a);

  g := ndaGramParallel(a, 2);

  CheckTrue(ndaAllClose(nda.AsArray<Double>([[17, 18, -21], [18, 29, -36], [-21, -36, 45]]), g, tol));
end;

procedure TMathTests.GramParallel_ReversedInput;
var a, g: INDArray<Double>;
begin
  a := nda.AsArray<Double>([[1, -2, 3], [4, 5, -6]]);
  a := a[[NDIAll(-1), NDIAll(-1)]];

  g := ndaGramParallel(a, 2);

  CheckTrue(ndaAllClose(nda.AsArray<Double>([[45, -36, -21], [-36, 29, 18], [-21, 18, 17]]), g, tol));
end;

procedure TMathTests.GramParallel_PaddedBuffers;
var a, g, at, gStorage, atStorage: INDArray<Double>;
    values, workspace: TNDAItems<Double>;
    I: Integer;
begin
  a := nda.AsArray<Double>([[1, -2, 3], [4, 5, -6]]);
  gStorage := nda.Full<Double>([3, 5], -99);
  atStorage := nda.Full<Double>([3, 4], -99);
  g := gStorage[[NDIAll, NDIAllTo(2)]];
  at := atStorage[[NDIAll, NDIAllTo(1)]];

  ndaGramParallel(a, g, at, 2);

  CheckTrue(ndaAllClose(nda.AsArray<Double>([[17, 18, -21], [18, 29, -36], [-21, -36, 45]]), g, tol));
  CheckTrue(ndaAllClose(TNDAMan.Transpose<Double>(a), at, tol));
  values := gStorage;
  workspace := atStorage;
  for I := 0 to 2 do begin
    CheckEquals(-99, values[[I, 3]], tol);
    CheckEquals(-99, values[[I, 4]], tol);
    CheckEquals(-99, workspace[[I, 2]], tol);
    CheckEquals(-99, workspace[[I, 3]], tol);
  end;
end;

procedure TMathTests.GramParallel_ReversedRows;
var a, original, expected, g, at: INDArray<Double>;
begin
  a := nda.AsArray<Double>([[1, -2, 3], [4, 5, -6], [7, 8, -9], [-10, 11, 12], [13, -14, 15]]);
  a := a[[NDIAll(-1), NDIAll]];
  original := TNDAUt.Copy<Double>(a);
  expected := ndaDot(TNDAMan.Transpose<Double>(original), original);

  ndaGramParallel(a, g, at, 2);

  CheckTrue(ndaAllClose(expected, g, tol));
  CheckTrue(ndaAllClose(TNDAMan.Transpose<Double>(original), at, tol));
  CheckTrue(ndaAllClose(original, a, tol));
end;

procedure TMathTests.GramParallel_ReversedBuffers;
var a, g, at: INDArray<Double>;
    pG, pAt: PByte;
begin
  a := nda.AsArray<Double>([[1, -2, 3], [4, 5, -6]]);
  a := a[[NDIAll(-1), NDIAll]];
  g := nda.Empty<Double>([3, 3]);
  at := nda.Empty<Double>([3, 2]);
  g := g[[NDIAll(-1), NDIAll]];
  at := at[[NDIAll(-1), NDIAll]];
  pG := g.Data;
  pAt := at.Data;

  ndaGramParallel(a, g, at, 2);

  CheckTrue(g.Data = pG);
  CheckTrue(at.Data = pAt);
  CheckTrue(ndaAllClose(nda.AsArray<Double>([[4, 1], [5, -2], [-6, 3]]), at, tol), 'Transpose workspace');
  CheckTrue(ndaAllClose(nda.AsArray<Double>([[17, 18, -21], [18, 29, -36], [-21, -36, 45]]), g, tol), 'Gram values');
end;

procedure TMathTests.GramParallel_NegativeWorkers;
var a, g, at: INDArray<Double>;
begin
  a := nda.Empty<Double>([2, 3]);

  try
    ndaGramParallel(a, g, at, -1);
    Fail('Expected EArgumentOutOfRangeException.');
  except
    on E: EArgumentOutOfRangeException do begin
      CheckFalse(Assigned(g));
      CheckFalse(Assigned(at));
    end;
  end;
end;


{$endregion}

{$region 'Single Gram'}

{$ifdef BLAS}
procedure TMathTests.CheckGramSingleBLAS(aReverseWorkspace, aReverseResult, aParallel: Boolean);
var a, g, at, expected: INDArray<Single>;
begin
  a := nda.AsArray<Single>([[1, -2, 3], [4, 5, -6]]);
  expected := ndaDot(TNDAMan.Transpose<Single>(a), a);
  g := nda.Empty<Single>([3, 3]);
  at := nda.Empty<Single>([3, 2]);
  if aReverseWorkspace then
    at := at[[NDIAll(-1), NDIAll]];
  if aReverseResult then
    g := g[[NDIAll(-1), NDIAll]];
  savedSSyrk := CBLAS.ssyrk;
  CheckTrue(Assigned(savedSSyrk));
  ssyrkCalls := 0;
  CBLAS.ssyrk := CountedSSyrk;
  try
    if aParallel then
      ndaGramParallel(a, g, at, 2)
    else
      ndaGram(a, g, at);
  finally
    CBLAS.ssyrk := savedSSyrk;
  end;

  if aParallel or aReverseWorkspace or aReverseResult then
    CheckEquals(0, ssyrkCalls)
  else
    CheckEquals(1, ssyrkCalls);
  CheckTrue(ndaAllClose(expected, g, tol));
  CheckTrue(ndaAllClose(TNDAMan.Transpose<Single>(a), at, tol));
end;

procedure TMathTests.GramSingle_BLAS_ContiguousBuffers;
begin
  CheckGramSingleBLAS(False, False, False);
end;

procedure TMathTests.GramSingle_BLAS_ReversedWorkspace;
begin
  CheckGramSingleBLAS(True, False, False);
end;

procedure TMathTests.GramSingle_BLAS_ReversedResult;
begin
  CheckGramSingleBLAS(False, True, False);
end;

procedure TMathTests.GramParallelSingle_BLAS_UsesNative;
begin
  CheckGramSingleBLAS(False, False, True);
end;
{$endif}

procedure TMathTests.GramSingle_NilInput;
var a, g: INDArray<Single>;
begin
  ExpectedException := ENDAShapeError;

  g := ndaGram(a);
end;

procedure TMathTests.GramSingle_VectorInput;
var a, g: INDArray<Single>;
begin
  a := nda.Empty<Single>([3]);
  ExpectedException := ENDAShapeError;

  g := ndaGram(a);
end;

procedure TMathTests.GramSingle_TensorInput;
var a, g: INDArray<Single>;
begin
  a := nda.Empty<Single>([2, 3, 4]);
  ExpectedException := ENDAShapeError;

  g := ndaGram(a);
end;

procedure TMathTests.GramSingle_5x4;
var x, g: INDArray<Single>;
    m, expected: TArray<TArray<Single>>;
    I: Integer;
begin
  x := nda.AsArray<Single>([
    [ 1,  2,  3,  4],
    [ 5,  6,  7,  8],
    [ 9, 10, 11, 12],
    [13, 14, 15, 16],
    [17, 18, 19, 20]
  ]);
  TNDAUt.TryAsDynArray2D<Single>(ndaDot(TNDAMan.Transpose<Single>(x),x), expected);

  g := ndaGram(x);

  CheckTrue(TNDAUt.TryAsDynArray2D<Single>(g, m));
  CheckEquals(Length(expected), Length(m));
  for I := 0 to High(expected) do
    CheckEquals(expected[I], m[I], tol);
end;

procedure TMathTests.GramSingle_5x5;
var x, g: INDArray<Single>;
    m, expected: TArray<TArray<Single>>;
    I: Integer;
begin
  x := nda.AsArray<Single>([
    [ 1,  2,  3,  4,  5],
    [ 6,  7,  8,  9, 10],
    [11, 12, 13, 14, 15],
    [16, 17, 18, 19, 20],
    [21, 22, 23, 24, 25]
  ]);
  TNDAUt.TryAsDynArray2D<Single>(ndaDot(TNDAMan.Transpose<Single>(x),x), expected);

  g := ndaGram(x);

  CheckTrue(TNDAUt.TryAsDynArray2D<Single>(g, m));
  CheckEquals(Length(expected), Length(m));
  for I := 0 to High(expected) do
    CheckEquals(expected[I], m[I], tol);
end;

procedure TMathTests.GramSingle_1x1;
var x, g: INDArray<Single>;
    m: TNDAItems<Single>;
begin
  x := nda.AsArray<Single>([[-3]]);

  g := ndaGram(x);

  m := g;
  CheckEquals([1, 1], g.Shape);
  CheckEquals(9, m[[0, 0]], tol);
end;

procedure TMathTests.GramSingle_StridedInput;
var x, g, expected: INDArray<Single>;
begin
  x := nda.AsArray<Single>([[1, 99, -2, 99, 3, 99], [4, 99, 5, 99, -6, 99]]);
  x := x[[NDIAll, NDIAll(2)]];
  expected := nda.AsArray<Single>([[17, 18, -21], [18, 29, -36], [-21, -36, 45]]);

  ndaGram(x, g);

  CheckTrue(ndaAllClose(expected, g, tol));
end;

procedure TMathTests.GramSingle_PaddedBuffers;
var x, g, at, gStorage, atStorage, expected: INDArray<Single>;
    values, workspace: TNDAItems<Single>;
    I: Integer;
begin
  x := nda.AsArray<Single>([[1, -2, 3], [4, 5, -6]]);
  gStorage := nda.Full<Single>([3, 5], -99);
  atStorage := nda.Full<Single>([3, 4], -99);
  g := gStorage[[NDIAll, NDIAllTo(2)]];
  at := atStorage[[NDIAll, NDIAllTo(1)]];
  expected := nda.AsArray<Single>([[17, 18, -21], [18, 29, -36], [-21, -36, 45]]);

  ndaGram(x, g, at);

  CheckTrue(ndaAllClose(expected, g, tol));
  CheckTrue(ndaAllClose(nda.AsArray<Single>([[1, 4], [-2, 5], [3, -6]]), at, tol));
  values := gStorage;
  workspace := atStorage;
  for I := 0 to 2 do begin
    CheckEquals(-99, values[[I, 3]], tol);
    CheckEquals(-99, values[[I, 4]], tol);
    CheckEquals(-99, workspace[[I, 2]], tol);
    CheckEquals(-99, workspace[[I, 3]], tol);
  end;
end;

procedure TMathTests.GramSingle_ReversedWorkspace;
var x, g, at, expected: INDArray<Single>;
begin
  x := nda.AsArray<Single>([[1, -2, 3], [4, 5, -6]]);
  at := nda.Empty<Single>([3, 2]);
  at := at[[NDIAll(-1), NDIAll]];
  expected := nda.AsArray<Single>([[17, 18, -21], [18, 29, -36], [-21, -36, 45]]);

  ndaGram(x, g, at);

  CheckTrue(ndaAllClose(expected, g, tol));
  CheckTrue(ndaAllClose(nda.AsArray<Single>([[1, 4], [-2, 5], [3, -6]]), at, tol));
end;

procedure TMathTests.GramSingle_ReversedResult;
var x, g, at, expected: INDArray<Single>;
begin
  x := nda.AsArray<Single>([[1, -2, 3], [4, 5, -6]]);
  g := nda.Empty<Single>([3, 3]);
  g := g[[NDIAll(-1), NDIAll]];
  expected := nda.AsArray<Single>([[17, 18, -21], [18, 29, -36], [-21, -36, 45]]);

  ndaGram(x, g, at);

  CheckTrue(ndaAllClose(expected, g, tol));
end;

procedure TMathTests.GramSingle_ReusedBuffers;
var x, g, at: INDArray<Single>;
    pG, pAt: PByte;
begin
  x := nda.AsArray<Single>([[1, 2], [3, 4]]);
  ndaGram(x, g, at);
  pG := g.Data;
  pAt := at.Data;
  x.SetItem([0, 0], 2);

  ndaGram(x, g, at);

  CheckTrue(g.Data = pG);
  CheckTrue(at.Data = pAt);
  CheckTrue(ndaAllClose(nda.AsArray<Single>([[13, 16], [16, 20]]), g, tol));
  CheckTrue(ndaAllClose(nda.AsArray<Single>([[2, 3], [2, 4]]), at, tol));
end;

procedure TMathTests.CheckGramParallelSingle(aH, aW, aMaxWorkers: Integer);
var a, g, expected, serial, legacy: INDArray<Single>;
    values: TNDAItems<Single>;
    I, J: Integer;
begin
  a := nda.Empty<Single>([aH, aW]);
  for I := 0 to aH - 1 do
    for J := 0 to aW - 1 do
      a.SetItem([I, J], ((I*7 + J*3) mod 13 - 6) / 4);
  expected := ndaDot(TNDAMan.Transpose<Single>(a), a);
  serial := ndaGramParallel(a, 1);
  legacy := ndaGram(a);

  g := ndaGramParallel(a, aMaxWorkers);

  CheckEquals([aW, aW], g.Shape);
  CheckTrue(ndaAllClose(expected, g, tol));
  CheckTrue(ndaAllClose(serial, g, tol));
  CheckTrue(ndaAllClose(legacy, g, tol));
  values := g;
  for I := 0 to aW - 1 do
    for J := I + 1 to aW - 1 do
      CheckEquals(values[[I, J]], values[[J, I]], 0);
end;

procedure TMathTests.GramParallelSingle_Workers1;
begin
  CheckGramParallelSingle(7, 9, 1);
end;

procedure TMathTests.GramParallelSingle_Workers2;
begin
  CheckGramParallelSingle(7, 9, 2);
end;

procedure TMathTests.GramParallelSingle_Workers4;
begin
  CheckGramParallelSingle(7, 9, 4);
end;

procedure TMathTests.GramParallelSingle_AutomaticWorkers;
begin
  CheckGramParallelSingle(7, 9, 0);
end;

procedure TMathTests.GramParallelSingle_1x1;
begin
  CheckGramParallelSingle(1, 1, 4);
end;

procedure TMathTests.GramParallelSingle_SingleColumn;
begin
  CheckGramParallelSingle(5, 1, 4);
end;

procedure TMathTests.GramParallelSingle_TwoColumns;
begin
  CheckGramParallelSingle(3, 2, 4);
end;

procedure TMathTests.GramParallelSingle_OddWidth;
begin
  CheckGramParallelSingle(4, 3, 2);
end;

procedure TMathTests.GramParallelSingle_EvenWidth;
begin
  CheckGramParallelSingle(5, 4, 2);
end;

procedure TMathTests.GramParallelSingle_UnevenTiles;
begin
  CheckGramParallelSingle(8, 5, 4);
end;

procedure TMathTests.GramParallelSingle_BoundaryInsideRow;
begin
  CheckGramParallelSingle(9, 17, 4);
end;

procedure TMathTests.GramParallelSingle_ExcessWorkers;
begin
  CheckGramParallelSingle(3, 3, 100);
end;

procedure TMathTests.GramParallelSingle_SingleInputRow;
begin
  CheckGramParallelSingle(1, 7, 4);
end;

procedure TMathTests.GramParallelSingle_LargerProduct;
begin
  CheckGramParallelSingle(129, 65, 4);
end;

procedure TMathTests.GramParallelSingle_FixedValues;
var a, g: INDArray<Single>;
begin
  a := nda.AsArray<Single>([[1, -2, 3], [4, 5, -6]]);

  g := ndaGramParallel(a, 2);

  CheckTrue(ndaAllClose(nda.AsArray<Single>([[17, 18, -21], [18, 29, -36], [-21, -36, 45]]), g, tol));
end;

procedure TMathTests.GramParallelSingle_AllocatedResult;
var a, g: INDArray<Single>;
begin
  a := nda.AsArray<Single>([[1, -2, 3], [4, 5, -6]]);

  ndaGramParallel(a, g, 2);

  CheckEquals([3, 3], g.Shape);
  CheckTrue(ndaAllClose(nda.AsArray<Single>([[17, 18, -21], [18, 29, -36], [-21, -36, 45]]), g, tol));
end;

procedure TMathTests.GramParallelSingle_ReusedResult;
var a, g: INDArray<Single>;
    pG: PByte;
begin
  a := nda.AsArray<Single>([[1, -2, 3], [4, 5, -6]]);
  g := nda.Full<Single>([3, 3], -99);
  pG := g.Data;

  ndaGramParallel(a, g, 2);

  CheckTrue(g.Data = pG);
  CheckTrue(ndaAllClose(nda.AsArray<Single>([[17, 18, -21], [18, 29, -36], [-21, -36, 45]]), g, tol));
end;

procedure TMathTests.GramParallelSingle_ReusedBuffers;
var a, g, at, expected: INDArray<Single>;
    pG, pAt: PByte;
    I: Integer;
begin
  a := nda.AsArray<Single>([[1, -2, 3], [4, 5, -6]]);
  ndaGramParallel(a, g, at);
  pG := g.Data;
  pAt := at.Data;

  CheckEquals([3, 3], g.Shape);
  CheckEquals([3, 2], at.Shape);
  CheckTrue(ndaAllClose(ndaDot(TNDAMan.Transpose<Single>(a), a), g, tol));
  CheckTrue(ndaAllClose(TNDAMan.Transpose<Single>(a), at, tol));

  for I := 1 to 10 do begin
    a.SetItem([0, 0], I);
    expected := ndaDot(TNDAMan.Transpose<Single>(a), a);

    ndaGramParallel(a, g, at, 4);

    CheckTrue(g.Data = pG);
    CheckTrue(at.Data = pAt);
    CheckTrue(ndaAllClose(expected, g, tol));
    CheckTrue(ndaAllClose(TNDAMan.Transpose<Single>(a), at, tol));
  end;
end;

procedure TMathTests.GramParallelSingle_StridedInput;
var a, g, at, original, expected: INDArray<Single>;
begin
  a := nda.AsArray<Single>([[1, 99, -2, 99, 3, 99], [4, 99, 5, 99, -6, 99]]);
  a := a[[NDIAll, NDIAll(2)]];
  original := TNDAUt.Copy<Single>(a);
  expected := ndaDot(TNDAMan.Transpose<Single>(original), original);

  ndaGramParallel(a, g, at, 2);

  CheckTrue(ndaAllClose(expected, g, tol));
  CheckTrue(ndaAllClose(TNDAMan.Transpose<Single>(original), at, tol));
  CheckTrue(ndaAllClose(original, a, tol));
end;

procedure TMathTests.GramParallelSingle_TransposedInput;
var a, g: INDArray<Single>;
begin
  a := nda.AsArray<Single>([[1, 4], [-2, 5], [3, -6]]);
  a := TNDAMan.TransposedView<Single>(a);

  g := ndaGramParallel(a, 2);

  CheckTrue(ndaAllClose(nda.AsArray<Single>([[17, 18, -21], [18, 29, -36], [-21, -36, 45]]), g, tol));
end;

procedure TMathTests.GramParallelSingle_ReversedInput;
var a, g: INDArray<Single>;
begin
  a := nda.AsArray<Single>([[1, -2, 3], [4, 5, -6]]);
  a := a[[NDIAll(-1), NDIAll(-1)]];

  g := ndaGramParallel(a, 2);

  CheckTrue(ndaAllClose(nda.AsArray<Single>([[45, -36, -21], [-36, 29, 18], [-21, 18, 17]]), g, tol));
end;

procedure TMathTests.GramParallelSingle_PaddedBuffers;
var a, g, at, gStorage, atStorage: INDArray<Single>;
    values, workspace: TNDAItems<Single>;
    I: Integer;
begin
  a := nda.AsArray<Single>([[1, -2, 3], [4, 5, -6]]);
  gStorage := nda.Full<Single>([3, 5], -99);
  atStorage := nda.Full<Single>([3, 4], -99);
  g := gStorage[[NDIAll, NDIAllTo(2)]];
  at := atStorage[[NDIAll, NDIAllTo(1)]];

  ndaGramParallel(a, g, at, 2);

  CheckTrue(ndaAllClose(nda.AsArray<Single>([[17, 18, -21], [18, 29, -36], [-21, -36, 45]]), g, tol));
  CheckTrue(ndaAllClose(TNDAMan.Transpose<Single>(a), at, tol));
  values := gStorage;
  workspace := atStorage;
  for I := 0 to 2 do begin
    CheckEquals(-99, values[[I, 3]], tol);
    CheckEquals(-99, values[[I, 4]], tol);
    CheckEquals(-99, workspace[[I, 2]], tol);
    CheckEquals(-99, workspace[[I, 3]], tol);
  end;
end;

procedure TMathTests.GramParallelSingle_ReversedRows;
var a, original, expected, g, at: INDArray<Single>;
begin
  a := nda.AsArray<Single>([[1, -2, 3], [4, 5, -6], [7, 8, -9], [-10, 11, 12], [13, -14, 15]]);
  a := a[[NDIAll(-1), NDIAll]];
  original := TNDAUt.Copy<Single>(a);
  expected := ndaDot(TNDAMan.Transpose<Single>(original), original);

  ndaGramParallel(a, g, at, 2);

  CheckTrue(ndaAllClose(expected, g, tol));
  CheckTrue(ndaAllClose(TNDAMan.Transpose<Single>(original), at, tol));
  CheckTrue(ndaAllClose(original, a, tol));
end;

procedure TMathTests.GramParallelSingle_ReversedBuffers;
var a, g, at: INDArray<Single>;
    pG, pAt: PByte;
begin
  a := nda.AsArray<Single>([[1, -2, 3], [4, 5, -6]]);
  a := a[[NDIAll(-1), NDIAll]];
  g := nda.Empty<Single>([3, 3]);
  at := nda.Empty<Single>([3, 2]);
  g := g[[NDIAll(-1), NDIAll]];
  at := at[[NDIAll(-1), NDIAll]];
  pG := g.Data;
  pAt := at.Data;

  ndaGramParallel(a, g, at, 2);

  CheckTrue(g.Data = pG);
  CheckTrue(at.Data = pAt);
  CheckTrue(ndaAllClose(nda.AsArray<Single>([[4, 1], [5, -2], [-6, 3]]), at, tol), 'Transpose workspace');
  CheckTrue(ndaAllClose(nda.AsArray<Single>([[17, 18, -21], [18, 29, -36], [-21, -36, 45]]), g, tol), 'Gram values');
end;


{$endregion}

{$region 'Single matrix multiplication'}

{$ifdef BLAS}
procedure TMathTests.CheckMatMulSingleBLAS(aReverseA, aReverseWorkspace, aReverseResult, aParallel: Boolean);
var a, b, c, bt, expected, originalA, originalB: INDArray<Single>;
begin
  a := nda.AsArray<Single>([[1, -2, 3], [4, 5, -6], [-3, 6, 2]]);
  b := nda.AsArray<Single>([[2, -1, 3, 4], [1, 4, -2, 5], [-3, 6, 1, -2]]);
  if aReverseA then
    a := a[[NDIAll(-1), NDIAll]];
  originalA := TNDAUt.Copy<Single>(a);
  originalB := TNDAUt.Copy<Single>(b);
  expected := ndaDot(a, b);
  c := nda.Empty<Single>([3, 4]);
  bt := nda.Empty<Single>([4, 3]);
  if aReverseWorkspace then
    bt := bt[[NDIAll(-1), NDIAll]];
  if aReverseResult then
    c := c[[NDIAll(-1), NDIAll]];
  savedSGemm := CBLAS.sgemm;
  CheckTrue(Assigned(savedSGemm));
  sgemmCalls := 0;
  CBLAS.sgemm := CountedSGemm;
  try
    if aParallel then
      ndaMatMulParallel(a, b, c, bt, 2)
    else
      ndaMatMul(a, b, c, bt);
  finally
    CBLAS.sgemm := savedSGemm;
  end;

  if aReverseA or aReverseWorkspace or aReverseResult or aParallel then
    CheckEquals(0, sgemmCalls)
  else
    CheckEquals(1, sgemmCalls);
  CheckTrue(ndaAllClose(expected, c, tol));
  CheckTrue(ndaAllClose(TNDAMan.Transpose<Single>(b), bt, tol));
  CheckTrue(ndaAllClose(originalA, a, tol));
  CheckTrue(ndaAllClose(originalB, b, tol));
end;

procedure TMathTests.MatMulSingle_BLAS_ContiguousBuffers;
begin
  CheckMatMulSingleBLAS(False, False, False, False);
end;

procedure TMathTests.MatMulSingle_BLAS_ReversedA;
begin
  CheckMatMulSingleBLAS(True, False, False, False);
end;

procedure TMathTests.MatMulSingle_BLAS_ReversedWorkspace;
begin
  CheckMatMulSingleBLAS(False, True, False, False);
end;

procedure TMathTests.MatMulSingle_BLAS_ReversedResult;
begin
  CheckMatMulSingleBLAS(False, False, True, False);
end;

procedure TMathTests.MatMulParallelSingle_BLAS_UsesNative;
begin
  CheckMatMulSingleBLAS(False, False, False, True);
end;
{$endif}

procedure TMathTests.CheckMatMulSingle(aM, aK, aN: Integer);
var a, b, c, expected: INDArray<Single>;
    I, J: Integer;
begin
  a := nda.Empty<Single>([aM, aK]);
  b := nda.Empty<Single>([aK, aN]);
  for I := 0 to aM - 1 do
    for J := 0 to aK - 1 do
      a.SetItem([I, J], ((I*7 + J*3) mod 13 - 6) / 4);
  for I := 0 to aK - 1 do
    for J := 0 to aN - 1 do
      b.SetItem([I, J], ((I*5 + J*11) mod 17 - 8) / 8);
  expected := ndaDot(a, b);

  c := ndaMatMul(a, b);

  CheckEquals([aM, aN], c.Shape);
  CheckTrue(ndaAllClose(expected, c, tol));
end;

procedure TMathTests.MatMulSingle_EvenRowsEvenCols;
begin
  CheckMatMulSingle(4, 8, 6);
end;

procedure TMathTests.MatMulSingle_EvenRowsOddCols;
begin
  CheckMatMulSingle(4, 7, 3);
end;

procedure TMathTests.MatMulSingle_OddRowsEvenCols;
begin
  CheckMatMulSingle(3, 4, 6);
end;

procedure TMathTests.MatMulSingle_OddRowsOddCols;
begin
  CheckMatMulSingle(3, 5, 5);
end;

procedure TMathTests.MatMulSingle_1x1;
var a, b, c: INDArray<Single>;
begin
  a := nda.AsArray<Single>([[-3]]);
  b := nda.AsArray<Single>([[2]]);

  c := ndaMatMul(a, b);

  CheckEquals([1, 1], c.Shape);
  CheckTrue(ndaAllClose(nda.AsArray<Single>([[-6]]), c, tol));
end;

procedure TMathTests.MatMulSingle_SingleRow;
begin
  CheckMatMulSingle(1, 3, 5);
end;

procedure TMathTests.MatMulSingle_SingleColumn;
begin
  CheckMatMulSingle(5, 3, 1);
end;

procedure TMathTests.MatMulSingle_InnerDimension1;
begin
  CheckMatMulSingle(3, 1, 5);
end;

procedure TMathTests.MatMulSingle_FixedValues;
var a, b, c, expected: INDArray<Single>;
begin
  a := nda.AsArray<Single>([[1, -2, 3], [4, 5, -6]]);
  b := nda.AsArray<Single>([[2, -1], [1, 4], [-3, 2]]);
  expected := nda.AsArray<Single>([[-9, -3], [31, 4]]);

  c := ndaMatMul(a, b);

  CheckEquals([2, 2], c.Shape);
  CheckTrue(ndaAllClose(expected, c, tol));
end;

procedure TMathTests.MatMulSingle_AllocatedResult;
var a, b, c: INDArray<Single>;
begin
  a := nda.AsArray<Single>([[1, -2, 3], [4, 5, -6]]);
  b := nda.AsArray<Single>([[2, -1], [1, 4], [-3, 2]]);

  ndaMatMul(a, b, c);

  CheckEquals([2, 2], c.Shape);
  CheckTrue(ndaAllClose(nda.AsArray<Single>([[-9, -3], [31, 4]]), c, tol));
end;

procedure TMathTests.MatMulSingle_ReusedResult;
var a, b, c: INDArray<Single>;
    pC: PByte;
begin
  a := nda.AsArray<Single>([[1, -2, 3], [4, 5, -6]]);
  b := nda.AsArray<Single>([[2, -1], [1, 4], [-3, 2]]);
  c := nda.Full<Single>([2, 2], -99);
  pC := c.Data;

  ndaMatMul(a, b, c);

  CheckTrue(c.Data = pC);
  CheckTrue(ndaAllClose(nda.AsArray<Single>([[-9, -3], [31, 4]]), c, tol));
end;

procedure TMathTests.MatMulSingle_ReusedBuffers;
var a, b, c, bt: INDArray<Single>;
    pC, pBt: PByte;
begin
  a := nda.AsArray<Single>([[1, -2, 3], [4, 5, -6]]);
  b := nda.AsArray<Single>([[2, -1], [1, 4], [-3, 2]]);
  ndaMatMul(a, b, c, bt);

  CheckEquals([2, 2], c.Shape);
  CheckEquals([2, 3], bt.Shape);
  CheckTrue(ndaAllClose(nda.AsArray<Single>([[-9, -3], [31, 4]]), c, tol));
  CheckTrue(ndaAllClose(TNDAMan.Transpose<Single>(b), bt, tol));

  pC := c.Data;
  pBt := bt.Data;
  a.SetItem([0, 0], 2);
  b.SetItem([0, 1], 3);

  ndaMatMul(a, b, c, bt);

  CheckTrue(c.Data = pC);
  CheckTrue(bt.Data = pBt);
  CheckTrue(ndaAllClose(nda.AsArray<Single>([[-7, 4], [31, 20]]), c, tol));
  CheckTrue(ndaAllClose(nda.AsArray<Single>([[2, 1, -3], [3, 4, 2]]), bt, tol));
end;

procedure TMathTests.MatMulSingle_StridedInputs;
var a, b, c, bt: INDArray<Single>;
begin
  a := nda.AsArray<Single>([[1, 99, -2, 99, 3, 99], [4, 99, 5, 99, -6, 99]]);
  b := nda.AsArray<Single>([
    [2, 99, -1, 99], [99, 99, 99, 99],
    [1, 99, 4, 99], [99, 99, 99, 99],
    [-3, 99, 2, 99], [99, 99, 99, 99]
  ]);
  a := a[[NDIAll, NDIAll(2)]];
  b := b[[NDIAll(2), NDIAll(2)]];

  ndaMatMul(a, b, c, bt);

  CheckTrue(ndaAllClose(nda.AsArray<Single>([[-9, -3], [31, 4]]), c, tol));
  CheckTrue(ndaAllClose(nda.AsArray<Single>([[2, 1, -3], [-1, 4, 2]]), bt, tol));
  CheckTrue(ndaAllClose(nda.AsArray<Single>([[1, -2, 3], [4, 5, -6]]), a, tol));
end;

procedure TMathTests.MatMulSingle_TransposedInputs;
var a, b, c: INDArray<Single>;
begin
  a := nda.AsArray<Single>([[1, 4], [-2, 5], [3, -6]]);
  b := nda.AsArray<Single>([[2, 1, -3], [-1, 4, 2]]);
  a := TNDAMan.TransposedView<Single>(a);
  b := TNDAMan.TransposedView<Single>(b);

  c := ndaMatMul(a, b);

  CheckTrue(ndaAllClose(nda.AsArray<Single>([[-9, -3], [31, 4]]), c, tol));
end;

procedure TMathTests.MatMulSingle_ReversedInputs;
var a, b, c: INDArray<Single>;
begin
  a := nda.AsArray<Single>([[1, -2, 3], [4, 5, -6]]);
  b := nda.AsArray<Single>([[2, -1], [1, 4], [-3, 2]]);
  a := a[[NDIAll(-1), NDIAll(-1)]];
  b := b[[NDIAll(-1), NDIAll(-1)]];

  c := ndaMatMul(a, b);

  CheckTrue(ndaAllClose(nda.AsArray<Single>([[4, 31], [-3, -9]]), c, tol));
end;

procedure TMathTests.MatMulSingle_ReversedRows;
var a, b, c: INDArray<Single>;
begin
  a := nda.AsArray<Single>([[1, -2, 3], [4, 5, -6]]);
  b := nda.AsArray<Single>([[2, -1], [1, 4], [-3, 2]]);
  a := a[[NDIAll(-1), NDIAll]];

  c := ndaMatMul(a, b);

  CheckTrue(ndaAllClose(nda.AsArray<Single>([[31, 4], [-9, -3]]), c, tol));
end;

procedure TMathTests.MatMulSingle_PaddedBuffers;
var a, b, c, bt, cStorage, btStorage: INDArray<Single>;
    values, workspace: TNDAItems<Single>;
    pC, pBt: PByte;
    I: Integer;
begin
  a := nda.AsArray<Single>([[1, -2, 99], [4, 5, 99], [-3, 6, 99]]);
  a := a[[NDIAll, NDIAllTo(1)]];
  b := nda.AsArray<Single>([[2, -1, 3], [1, 4, -2]]);
  cStorage := nda.Full<Single>([3, 5], -99);
  btStorage := nda.Full<Single>([3, 4], -99);
  c := cStorage[[NDIAll, NDIAllTo(2)]];
  bt := btStorage[[NDIAll, NDIAllTo(1)]];
  pC := c.Data;
  pBt := bt.Data;

  ndaMatMul(a, b, c, bt);

  CheckTrue(c.Data = pC);
  CheckTrue(bt.Data = pBt);
  CheckTrue(ndaAllClose(nda.AsArray<Single>([[0, -9, 7], [13, 16, 2], [0, 27, -21]]), c, tol));
  CheckTrue(ndaAllClose(TNDAMan.Transpose<Single>(b), bt, tol));
  values := cStorage;
  workspace := btStorage;
  for I := 0 to 2 do begin
    CheckEquals(-99, values[[I, 3]], tol);
    CheckEquals(-99, values[[I, 4]], tol);
    CheckEquals(-99, workspace[[I, 2]], tol);
    CheckEquals(-99, workspace[[I, 3]], tol);
  end;
end;

procedure TMathTests.MatMulSingle_ReversedWorkspace;
var a, b, c, bt: INDArray<Single>;
    pBt: PByte;
begin
  a := nda.AsArray<Single>([[1, -2], [4, 5], [-3, 6]]);
  b := nda.AsArray<Single>([[2, -1, 3], [1, 4, -2]]);
  bt := nda.Empty<Single>([3, 2]);
  bt := bt[[NDIAll(-1), NDIAll]];
  pBt := bt.Data;

  ndaMatMul(a, b, c, bt);

  CheckTrue(bt.Data = pBt);
  CheckTrue(ndaAllClose(nda.AsArray<Single>([[0, -9, 7], [13, 16, 2], [0, 27, -21]]), c, tol));
  CheckTrue(ndaAllClose(TNDAMan.Transpose<Single>(b), bt, tol));
end;

procedure TMathTests.MatMulSingle_ReversedResult;
var a, b, c, bt: INDArray<Single>;
    pC: PByte;
begin
  a := nda.AsArray<Single>([[1, -2], [4, 5], [-3, 6]]);
  b := nda.AsArray<Single>([[2, -1, 3], [1, 4, -2]]);
  c := nda.Empty<Single>([3, 3]);
  c := c[[NDIAll(-1), NDIAll]];
  pC := c.Data;

  ndaMatMul(a, b, c, bt);

  CheckTrue(c.Data = pC);
  CheckTrue(ndaAllClose(nda.AsArray<Single>([[0, -9, 7], [13, 16, 2], [0, 27, -21]]), c, tol));
end;


procedure TMathTests.CheckMatMulParallelSingle(aM, aK, aN, aMaxWorkers: Integer);
var a, b, c, serial, expected: INDArray<Single>;
    I, J: Integer;
begin
  a := nda.Empty<Single>([aM, aK]);
  b := nda.Empty<Single>([aK, aN]);
  for I := 0 to aM - 1 do
    for J := 0 to aK - 1 do
      a.SetItem([I, J], ((I*7 + J*3) mod 13 - 6) / 4);
  for I := 0 to aK - 1 do
    for J := 0 to aN - 1 do
      b.SetItem([I, J], ((I*5 + J*11) mod 17 - 8) / 8);
  expected := ndaDot(a, b);
  serial := ndaMatMulParallel(a, b, 1);

  c := ndaMatMulParallel(a, b, aMaxWorkers);

  CheckEquals([aM, aN], c.Shape);
  CheckTrue(ndaAllClose(expected, c, tol));
  CheckTrue(ndaAllClose(serial, c, tol));
end;

procedure TMathTests.MatMulParallelSingle_Workers1;
begin
  CheckMatMulParallelSingle(11, 7, 5, 1);
end;

procedure TMathTests.MatMulParallelSingle_Workers2;
begin
  CheckMatMulParallelSingle(11, 7, 5, 2);
end;

procedure TMathTests.MatMulParallelSingle_Workers4;
begin
  CheckMatMulParallelSingle(11, 7, 5, 4);
end;

procedure TMathTests.MatMulParallelSingle_AutomaticWorkers;
begin
  CheckMatMulParallelSingle(11, 7, 5, 0);
end;

procedure TMathTests.MatMulParallelSingle_EvenRows;
begin
  CheckMatMulParallelSingle(14, 8, 6, 4);
end;

procedure TMathTests.MatMulParallelSingle_LargerProduct;
begin
  CheckMatMulParallelSingle(257, 129, 65, 4);
end;

procedure TMathTests.MatMulParallelSingle_ExcessWorkers;
begin
  CheckMatMulParallelSingle(3, 3, 4, 100);
end;

procedure TMathTests.MatMulParallelSingle_SingleRow;
begin
  CheckMatMulParallelSingle(1, 3, 5, 4);
end;

procedure TMathTests.MatMulParallelSingle_SingleColumn;
begin
  CheckMatMulParallelSingle(9, 3, 1, 4);
end;

procedure TMathTests.MatMulParallelSingle_InnerDimension1;
begin
  CheckMatMulParallelSingle(9, 1, 7, 4);
end;

procedure TMathTests.MatMulParallelSingle_ReusedResult;
var a, b, c: INDArray<Single>;
    pC: PByte;
begin
  a := nda.AsArray<Single>([[1, -2], [4, 5], [-3, 6]]);
  b := nda.AsArray<Single>([[2, -1, 3], [1, 4, -2]]);
  c := nda.Full<Single>([3, 3], -99);
  pC := c.Data;

  ndaMatMulParallel(a, b, c, 2);

  CheckTrue(c.Data = pC);
  CheckTrue(ndaAllClose(nda.AsArray<Single>([[0, -9, 7], [13, 16, 2], [0, 27, -21]]), c, tol));
end;

procedure TMathTests.MatMulParallelSingle_ReusedBuffers;
var a, b, c, bt, expected: INDArray<Single>;
    pC, pBt: PByte;
    I: Integer;
begin
  a := nda.AsArray<Single>([[1, -2], [4, 5], [-3, 6]]);
  b := nda.AsArray<Single>([[2, -1, 3], [1, 4, -2]]);
  ndaMatMulParallel(a, b, c, bt);
  pC := c.Data;
  pBt := bt.Data;

  CheckEquals([3, 3], c.Shape);
  CheckEquals([3, 2], bt.Shape);
  CheckTrue(ndaAllClose(ndaDot(a, b), c, tol));
  CheckTrue(ndaAllClose(TNDAMan.Transpose<Single>(b), bt, tol));

  for I := 1 to 10 do begin
    a.SetItem([0, 0], I);
    b.SetItem([0, 1], -I);
    expected := ndaDot(a, b);

    ndaMatMulParallel(a, b, c, bt, 4);

    CheckTrue(c.Data = pC);
    CheckTrue(bt.Data = pBt);
    CheckTrue(ndaAllClose(expected, c, tol));
    CheckTrue(ndaAllClose(TNDAMan.Transpose<Single>(b), bt, tol));
  end;
end;

procedure TMathTests.MatMulParallelSingle_StridedInputs;
var a, b, c, bt, expected, originalA, originalB: INDArray<Single>;
begin
  a := nda.AsArray<Single>([[1, 99, -2, 99], [4, 99, 5, 99], [-3, 99, 6, 99]]);
  b := nda.AsArray<Single>([[2, 99, -1, 99, 3, 99], [1, 99, 4, 99, -2, 99]]);
  a := a[[NDIAll, NDIAll(2)]];
  b := b[[NDIAll, NDIAll(2)]];
  originalA := TNDAUt.Copy<Single>(a);
  originalB := TNDAUt.Copy<Single>(b);
  expected := ndaDot(originalA, originalB);

  ndaMatMulParallel(a, b, c, bt, 2);

  CheckTrue(ndaAllClose(expected, c, tol));
  CheckTrue(ndaAllClose(TNDAMan.Transpose<Single>(originalB), bt, tol));
  CheckTrue(ndaAllClose(originalA, a, tol));
  CheckTrue(ndaAllClose(originalB, b, tol));
end;

procedure TMathTests.MatMulParallelSingle_TransposedInputs;
var a, b, c: INDArray<Single>;
begin
  a := nda.AsArray<Single>([[1, 4, -3], [-2, 5, 6]]);
  b := nda.AsArray<Single>([[2, 1], [-1, 4], [3, -2]]);
  a := TNDAMan.TransposedView<Single>(a);
  b := TNDAMan.TransposedView<Single>(b);

  c := ndaMatMulParallel(a, b, 2);

  CheckTrue(ndaAllClose(nda.AsArray<Single>([[0, -9, 7], [13, 16, 2], [0, 27, -21]]), c, tol));
end;

procedure TMathTests.MatMulParallelSingle_ReversedInputs;
var a, b, c, expected: INDArray<Single>;
begin
  a := nda.AsArray<Single>([[1, -2], [4, 5], [-3, 6]]);
  b := nda.AsArray<Single>([[2, -1, 3], [1, 4, -2]]);
  a := a[[NDIAll(-1), NDIAll(-1)]];
  b := b[[NDIAll(-1), NDIAll(-1)]];
  expected := nda.AsArray<Single>([[-21, 27, 0], [2, 16, 13], [7, -9, 0]]);

  c := ndaMatMulParallel(a, b, 2);

  CheckTrue(ndaAllClose(expected, c, tol));
end;

procedure TMathTests.MatMulParallelSingle_PaddedBuffers;
var a, b, c, bt, cStorage, btStorage: INDArray<Single>;
    values, workspace: TNDAItems<Single>;
    I: Integer;
begin
  a := nda.AsArray<Single>([[1, -2, 99], [4, 5, 99], [-3, 6, 99]]);
  a := a[[NDIAll, NDIAllTo(1)]];
  b := nda.AsArray<Single>([[2, -1, 3], [1, 4, -2]]);
  cStorage := nda.Full<Single>([3, 5], -99);
  btStorage := nda.Full<Single>([3, 4], -99);
  c := cStorage[[NDIAll, NDIAllTo(2)]];
  bt := btStorage[[NDIAll, NDIAllTo(1)]];

  ndaMatMulParallel(a, b, c, bt, 2);

  CheckTrue(ndaAllClose(nda.AsArray<Single>([[0, -9, 7], [13, 16, 2], [0, 27, -21]]), c, tol));
  CheckTrue(ndaAllClose(TNDAMan.Transpose<Single>(b), bt, tol));
  values := cStorage;
  workspace := btStorage;
  for I := 0 to 2 do begin
    CheckEquals(-99, values[[I, 3]], tol);
    CheckEquals(-99, values[[I, 4]], tol);
    CheckEquals(-99, workspace[[I, 2]], tol);
    CheckEquals(-99, workspace[[I, 3]], tol);
  end;
end;

procedure TMathTests.MatMulParallelSingle_ReversedBuffers;
var a, b, c, bt: INDArray<Single>;
    pC, pBt: PByte;
begin
  a := nda.AsArray<Single>([[1, -2], [4, 5], [-3, 6]]);
  a := a[[NDIAll(-1), NDIAll]];
  b := nda.AsArray<Single>([[2, -1, 3], [1, 4, -2]]);
  c := nda.Empty<Single>([3, 3]);
  c := c[[NDIAll(-1), NDIAll]];
  bt := nda.Empty<Single>([3, 2]);
  bt := bt[[NDIAll(-1), NDIAll]];
  pC := c.Data;
  pBt := bt.Data;

  ndaMatMulParallel(a, b, c, bt, 2);

  CheckTrue(c.Data = pC);
  CheckTrue(bt.Data = pBt);
  CheckTrue(ndaAllClose(nda.AsArray<Single>([[0, 27, -21], [13, 16, 2], [0, -9, 7]]), c, tol));
  CheckTrue(ndaAllClose(TNDAMan.Transpose<Single>(b), bt, tol));
end;

procedure TMathTests.MatMulParallelSingle_IncompatibleDimensions;
var a, b, c, bt: INDArray<Single>;
begin
  a := nda.Empty<Single>([3, 2]);
  b := nda.Empty<Single>([3, 4]);

  try
    ndaMatMulParallel(a, b, c, bt, 2);
    Fail('Expected ENDAShapeError.');
  except
    on E: ENDAShapeError do begin
      CheckFalse(Assigned(c));
      CheckFalse(Assigned(bt));
    end;
  end;
end;


{$endregion}

initialization

  RegisterTest(TMathTests.Suite);

end.
