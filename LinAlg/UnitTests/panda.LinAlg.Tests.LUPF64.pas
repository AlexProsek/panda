unit panda.LinAlg.Tests.LUPF64;

interface

uses
    TestFramework
  , panda.Nums
  , panda.Intfs
  , panda.Arrays
  , panda.ArrManip
  , panda.Math
  , panda.LinAlg.LUPF64
  , panda.LinAlg.LUP
  , panda.LinAlg.NumericF64
  , panda.LinAlg.Base
  , panda.Arithmetic
  , panda.DynArrayUtils
  , panda.Tests.NDATestCase
  ;

type
  TLUPDecompositionF64Tests = class(TNDATestCase)
  protected const
    dTol = 1e-14;
  protected
    solver: TLUPDecompositionF64;
    function BandMatrix(aN, aLower, aUpper: Integer): TDoubleArray2D;
    function PivotBandMatrix: TDoubleArray2D;
    function BandRHS(const A: TDoubleArray2D): TDoubleArray;
  public
    procedure SetUp; override;
    procedure TearDown; override;
    procedure CheckPartSol(const A: TDoubleArray2D; const b, res: TDoubleArray;
      aTol: Double = 0);
    procedure CheckHomSol(const A, Hom: TDoubleArray2D; aTol: Double = 0);
  published
    procedure LUDecomp;
    procedure LUSolve;
    procedure MatInvert;
    procedure LUSolveND;
    procedure LUDecompNDStrided;
    procedure MatInvertND;
    procedure MatInvertSmallScale;
    procedure DeterminantSmallScale;
    procedure BandLUReuse;
    procedure BandLUND;
    procedure BandDeterminantAndInverse;
    procedure BandSingularLU;
    procedure BandLUWorkBounds;
    procedure BandLUDenseEquivalence;
  end;

type
  TBlockedLUPF64Tests = class(TLUPDecompositionF64Tests)
  public
    procedure SetUp; override;
  end;

implementation

type
  TLUPAccess = class(TLUPDecompositionF64);

  TBandNumericF64 = class(TLinAlgNumericF64)
  public type
    PDouble = TLinAlgNumeric<Double>.PT;
  public
    PivotStarts, PivotEnds: TArray<NativeInt>;
    class var AxpyElements, DotElements, MaxAxpyCount, MaxDotCount: NativeInt;
    class procedure ResetCounts;
  public
    function ArgMax(const A: TArray<PDouble>; aRIdx, aCIdx,
      aLastRIdx: NativeInt): NativeInt; override;
    procedure Axpy(const a: Double; pX, pY: PDouble; aCount: NativeInt); override;
    procedure Dot(pX, pY: PDouble; aCount: NativeInt; out aRes: Double); override;
  end;

class procedure TBandNumericF64.ResetCounts;
begin
  AxpyElements := 0;
  DotElements := 0;
  MaxAxpyCount := 0;
  MaxDotCount := 0;
end;

function TBandNumericF64.ArgMax(const A: TArray<PDouble>; aRIdx, aCIdx,
  aLastRIdx: NativeInt): NativeInt;
var N: Integer;
begin
  N := Length(PivotStarts);
  SetLength(PivotStarts, N + 1);
  SetLength(PivotEnds, N + 1);
  PivotStarts[N] := aRIdx;
  PivotEnds[N] := aLastRIdx;
  Result := inherited ArgMax(A, aRIdx, aCIdx, aLastRIdx);
end;

procedure TBandNumericF64.Axpy(const a: Double; pX, pY: PDouble;
  aCount: NativeInt);
begin
  Inc(AxpyElements, aCount);
  if aCount > MaxAxpyCount then MaxAxpyCount := aCount;
  inherited;
end;

procedure TBandNumericF64.Dot(pX, pY: PDouble; aCount: NativeInt;
  out aRes: Double);
begin
  Inc(DotElements, aCount);
  if aCount > MaxDotCount then MaxDotCount := aCount;
  inherited;
end;

type
  TBandLUPF64 = class(TLUPDecompositionF64)
  protected
    function CreateNumeric: TLinAlgNumeric<Double>; override;
  public
    function Counts: TBandNumericF64;
  end;

function TBandLUPF64.CreateNumeric: TLinAlgNumeric<Double>;
begin
  Result := TBandNumericF64.Create;
end;

function TBandLUPF64.Counts: TBandNumericF64;
begin
  Result := TBandNumericF64(fNumeric);
end;

{$region 'TLUPDecompositionF64Tests'}

procedure TLUPDecompositionF64Tests.SetUp;
begin
  solver := TLUPDecompositionF64.Create;
end;

procedure TLUPDecompositionF64Tests.TearDown;
begin
  solver.Free;
end;

procedure TLUPDecompositionF64Tests.CheckPartSol(const A: TDoubleArray2D;
  const b, res: TDoubleArray; aTol: Double);
var mA, mb, mRes: INDArray<Double>;
begin
  if aTol <= 0 then aTol := dTol;

  mA := TNDAUt.AsArray<Double>(A);
  mb := TNDAUt.AsArray<Double>(b);
  mRes := TNDAUt.AsArray<Double>(res);

  CheckTrue(ndaAllClose(ndaDot(mA, mRes), mb, aTol));
end;

procedure TLUPDecompositionF64Tests.CheckHomSol(const A, Hom: TDoubleArray2D; aTol: Double);
var mA, mH: INDArray<Double>;
begin
  if aTol <= 0 then aTol := dTol;

  mA := TNDAUt.AsArray<Double>(A);
  mH := TNDAUt.AsArray<Double>(Hom);

  CheckTrue(ndaAllClose(ndaDot(mA, TNDAMan.Transpose<Double>(mH)), 0, aTol));
end;

procedure TLUPDecompositionF64Tests.LUDecomp;
var A, l, u: TDoubleArray2D;
    p: TArray<NativeInt>;
    mA, ml, mu: INDArray<Double>;
begin
  A := TDoubleArray2D.Create(
    TArray<Double>.Create(1, 1, 1),
    TArray<Double>.Create(2, 4, 8),
    TArray<Double>.Create(3, 9, 27)
  );
  mA := TNDAUt.AsArray<Double>(A);

  solver.InitializeWithoutCopy(A);
  p := Copy(TLUPAccess(solver).fP);

  l := TDynAUt.LowerTriangularize<Double>(A, -1);
  TDynAUt.SetDiagonal<Double>(l, 1);
  u := TDynAUt.UpperTriangularize<Double>(A);

  ml := TNDAUt.AsArray<Double>(l);
  mu := TNDAUt.AsArray<Double>(u);
  SetLength(p, 3);
  p := TDynAUt.InversePermutation(p);
  CheckTrue(ndaAllClose(ndaDot(ml, mu)[[NDI(p)]], mA, dTol));
end;

procedure TLUPDecompositionF64Tests.LUSolve;
var A: TArray<TArray<Double>>;
    b, res: TArray<Double>;
const tol = 1e-8;
begin
  SetLength(A, 3, 3);
  SetLength(b, 3);

  A[0,0] := 1; A[0,1] := 2; A[0,2] := -2;
  A[1,0] := 3; A[1,1] := 1; A[1,2] :=  0;
  A[2,0] := 0; A[2,1] :=-2; A[2,2] :=  6;

  b[0] := -1;
  b[1] := 3;
  b[2] := 8;

  CheckTrue(solver.InitializeWithoutCopy(A) = lsisInitialized);
  solver.Solve(b, res);

  CheckEquals([7/9, 2/3, 14/9], res, tol);
end;

procedure TLUPDecompositionF64Tests.MatInvert;
var A, IA: TDoubleArray2D;
    mA, mIA: INDArray<Double>;
begin
  A := TDoubleArray2D.Create(
    TArray<Double>.Create(1, 2, 3),
    TArray<Double>.Create(4, 2, 2),
    TArray<Double>.Create(5, 1, 7)
  );

  CheckTrue(solver.Initialize(A) = lsisInitialized);
  solver.Invert(IA);

  mA := TNDAUt.AsArray<Double>(A);
  mIA := TNDAUt.AsArray<Double>(IA);
  CheckTrue(ndaAllClose(ndaDot(mA, mIA), TNDAUt.Identity<Double>(3), dTol));
end;

procedure TLUPDecompositionF64Tests.LUSolveND;
var A, B, col: TDoubleArray2D;
    mA, mA0, mB, mRes: INDArray<Double>;
const tol = 1e-8;
begin
  A := TDoubleArray2D.Create(
    TArray<Double>.Create(1, 2, -2),
    TArray<Double>.Create(3, 1, 0),
    TArray<Double>.Create(0, -2, 6)
  );
  mA0 := TNDAUt.AsArray<Double>(A);
  mA := TNDAUt.AsArray<Double>(A);
  mB := TNDAUt.AsArray<Double>(TDoubleArray.Create(-1, 3, 8));

  CheckTrue(solver.InitializeWithoutCopy(mA) = lsisInitialized);
  solver.Solve(mB, mRes);

  CheckEquals(1, mRes.NDim);
  CheckTrue(ndaAllClose(mRes, TNDAUt.AsArray<Double>(TDoubleArray.Create(7/9, 2/3, 14/9)), tol));

  col := TDoubleArray2D.Create(
    TArray<Double>.Create(-1),
    TArray<Double>.Create(3),
    TArray<Double>.Create(8)
  );
  mA := TNDAUt.AsArray<Double>(A);
  mB := TNDAUt.AsArray<Double>(col);

  CheckTrue(solver.InitializeWithoutCopy(mA) = lsisInitialized);
  solver.Solve(mB, mRes);

  CheckEquals(2, mRes.NDim);
  CheckEquals(3, Integer(mRes.Shape[0]));
  CheckEquals(1, Integer(mRes.Shape[1]));
  CheckTrue(ndaAllClose(ndaDot(mA0, mRes), mB, tol));

  B := TDoubleArray2D.Create(
    TArray<Double>.Create(-1, 1),
    TArray<Double>.Create(3, 1),
    TArray<Double>.Create(8, 1)
  );
  mA := TNDAUt.AsArray<Double>(A);
  mB := TNDAUt.AsArray<Double>(B);

  CheckTrue(solver.InitializeWithoutCopy(mA) = lsisInitialized);
  solver.Solve(mB, mRes);

  CheckEquals(2, mRes.NDim);
  CheckEquals(3, Integer(mRes.Shape[0]));
  CheckEquals(2, Integer(mRes.Shape[1]));
  CheckTrue(ndaAllClose(ndaDot(mA0, mRes), mB, tol));
end;

procedure TLUPDecompositionF64Tests.LUDecompNDStrided;
var wide, lu, l, u: TDoubleArray2D;
    p: TArray<NativeInt>;
    mWide, mView, mA, ml, mu: INDArray<Double>;
    padded: TNDAItems<Double>;
begin
  wide := TDoubleArray2D.Create(
    TArray<Double>.Create(1, 1, 1, 0),
    TArray<Double>.Create(2, 4, 8, 0),
    TArray<Double>.Create(3, 9, 27, 0)
  );
  mWide := TNDAUt.AsArray<Double>(wide);
  mView := mWide[[NDIAll, NDISpan(0, 2)]];
  mA := TNDAUt.Copy<Double>(mView);

  CheckEquals(3, Integer(mView.Shape[0]));
  CheckEquals(3, Integer(mView.Shape[1]));
  CheckEquals(SizeOf(Double), Integer(mView.Strides[1]));
  CheckEquals(SizeOf(Double) * 4, Integer(mView.Strides[0]));

  CheckTrue(solver.InitializeWithoutCopy(mView) = lsisInitialized);
  p := Copy(TLUPAccess(solver).fP);

  CheckTrue(TNDAUt.TryAsDynArray2D<Double>(mView, lu));
  l := TDynAUt.LowerTriangularize<Double>(lu, -1);
  TDynAUt.SetDiagonal<Double>(l, 1);
  u := TDynAUt.UpperTriangularize<Double>(lu);
  ml := TNDAUt.AsArray<Double>(l);
  mu := TNDAUt.AsArray<Double>(u);
  SetLength(p, 3);
  p := TDynAUt.InversePermutation(p);

  CheckTrue(ndaAllClose(ndaDot(ml, mu)[[NDI(p)]], mA, dTol));

  padded := mWide;
  CheckEquals(0, padded[[0, 3]], 0);
  CheckEquals(0, padded[[1, 3]], 0);
  CheckEquals(0, padded[[2, 3]], 0);
end;

procedure TLUPDecompositionF64Tests.MatInvertND;
var A: TDoubleArray2D;
    mA, mIA: INDArray<Double>;
begin
  A := TDoubleArray2D.Create(
    TArray<Double>.Create(1, 2, 3),
    TArray<Double>.Create(4, 2, 2),
    TArray<Double>.Create(5, 1, 7)
  );
  mA := TNDAUt.AsArray<Double>(A);

  CheckTrue(solver.Initialize(mA) = lsisInitialized);
  solver.Invert(mIA);

  CheckTrue(ndaAllClose(ndaDot(mA, mIA), TNDAUt.Identity<Double>(3), dTol));
end;

procedure TLUPDecompositionF64Tests.MatInvertSmallScale;
var A, IA: TDoubleArray2D;
    mA, mIA: INDArray<Double>;
begin
  A := TDoubleArray2D.Create(
    TArray<Double>.Create(1e-13, 2e-13, 3e-13),
    TArray<Double>.Create(4e-13, 2e-13, 2e-13),
    TArray<Double>.Create(5e-13, 1e-13, 7e-13)
  );

  CheckTrue(solver.Initialize(A) = lsisInitialized);
  solver.Invert(IA);

  mA := TNDAUt.AsArray<Double>(A);
  mIA := TNDAUt.AsArray<Double>(IA);
  CheckTrue(ndaAllClose(ndaDot(mA, mIA), TNDAUt.Identity<Double>(3), dTol));
end;

procedure TLUPDecompositionF64Tests.DeterminantSmallScale;
var A: TDoubleArray2D;
    det: Double;
begin
  A := TDoubleArray2D.Create(
    TArray<Double>.Create(1e-13, 2e-13, 3e-13),
    TArray<Double>.Create(4e-13, 2e-13, 2e-13),
    TArray<Double>.Create(5e-13, 1e-13, 7e-13)
  );

  CheckTrue(solver.Initialize(A) = lsisInitialized);
  det := solver.Determinant;

  CheckEquals(-42e-39, det, 1e-51);
end;

function TLUPDecompositionF64Tests.BandMatrix(aN, aLower, aUpper: Integer): TDoubleArray2D;
var I, J: Integer;
begin
  SetLength(Result, aN, aN);
  for I := 0 to aN - 1 do
    for J := 0 to aN - 1 do
      if I = J then
        Result[I, J] := 12
      else if (I - J <= aLower) and (J - I <= aUpper) then
        Result[I, J] := (1 + (I + J) mod 3) / 4
      else
        Result[I, J] := 0;
end;

function TLUPDecompositionF64Tests.PivotBandMatrix: TDoubleArray2D;
begin
  Result := TDoubleArray2D.Create(
    TDoubleArray.Create(1, 2, 0, 0, 0),
    TDoubleArray.Create(4, 1, 3, 0, 0),
    TDoubleArray.Create(0, 5, 1, 2, 0),
    TDoubleArray.Create(0, 0, 6, 1, 4),
    TDoubleArray.Create(0, 0, 0, 7, 2)
  );
end;

function TLUPDecompositionF64Tests.BandRHS(const A: TDoubleArray2D): TDoubleArray;
var I, J: Integer;
begin
  SetLength(Result, Length(A));
  for I := 0 to High(A) do begin
    Result[I] := 0;
    for J := 0 to High(A[I]) do
      Result[I] := Result[I] + A[I, J] * (J + 1);
  end;
end;

procedure TLUPDecompositionF64Tests.BandLUReuse;
var A, LU, L, U, IA: TDoubleArray2D;
    P: TArray<NativeInt>;
    B, X: TDoubleArray;
    dense: TLUPDecompositionF64;
begin
  A := PivotBandMatrix;
  LU := TDynAUt.Clone<Double>(A);
  B := BandRHS(A);
  solver.LowerBandWidth := 1;
  solver.UpperBandWidth := 1;
  dense := TLUPDecompositionF64.Create;
  try
    CheckTrue(dense.Initialize(A) = lsisInitialized);

    CheckTrue(solver.InitializeWithoutCopy(LU) = lsisInitialized);
    P := Copy(TLUPAccess(solver).fP);
    solver.Solve(B, X);
    solver.Invert(IA);

    CheckPartSol(A, B, X);
    CheckEquals(9, Integer(P[5]));
    CheckEquals(3, LU[0, 2], 0);
    CheckEquals(0.25, LU[4, 0], 0);
    CheckEquals(solver.Determinant, dense.Determinant, dTol);
    L := TDynAUt.LowerTriangularize<Double>(LU, -1);
    TDynAUt.SetDiagonal<Double>(L, 1);
    U := TDynAUt.UpperTriangularize<Double>(LU);
    CheckTrue(ndaAllClose(ndaDot(TNDAUt.AsArray<Double>(L), TNDAUt.AsArray<Double>(U)),
      TNDAUt.AsArray<Double>(A)[[NDI(Copy(P, 0, 5))]], dTol));
    CheckTrue(ndaAllClose(ndaDot(TNDAUt.AsArray<Double>(A), TNDAUt.AsArray<Double>(IA)),
      TNDAUt.Identity<Double>(5), dTol));
  finally
    dense.Free;
  end;
end;

procedure TLUPDecompositionF64Tests.BandLUND;
var A, wide, B: TDoubleArray2D;
    M, V, MB, MX, MI: INDArray<Double>;
    items: TNDAItems<Double>;
    dense: TLUPDecompositionF64;
    I, J: Integer;
begin
  A := PivotBandMatrix;
  SetLength(wide, 5, 6);
  SetLength(B, 5, 2);
  for I := 0 to 4 do begin
    for J := 0 to 4 do
      wide[I, J] := A[I, J];
    wide[I, 5] := 99;
    B[I, 0] := I + 1;
    B[I, 1] := 2 - I;
  end;
  M := TNDAUt.AsArray<Double>(wide);
  V := M[[NDIAll, NDISpan(0, 4)]];
  MB := TNDAUt.AsArray<Double>(B);
  solver.LowerBandWidth := 1;
  solver.UpperBandWidth := 1;
  dense := TLUPDecompositionF64.Create;
  try
    CheckTrue(dense.Initialize(A) = lsisInitialized);

    CheckTrue(solver.InitializeWithoutCopy(V) = lsisInitialized);
    solver.Solve(MB, MX);
    solver.Invert(MI);

    CheckTrue(ndaAllClose(ndaDot(TNDAUt.AsArray<Double>(A), MX), MB, dTol));
    CheckTrue(ndaAllClose(ndaDot(TNDAUt.AsArray<Double>(A), MI), TNDAUt.Identity<Double>(5), dTol));
    CheckEquals(solver.Determinant, dense.Determinant, dTol);
    items := M;
    for I := 0 to 4 do
      CheckEquals(99, items[[I, 5]], 0);
  finally
    dense.Free;
  end;
end;

procedure TLUPDecompositionF64Tests.BandDeterminantAndInverse;
var A, IA: TDoubleArray2D;
    M, MI: INDArray<Double>;
    dense: TLUPDecompositionF64;
begin
  A := PivotBandMatrix;
  M := TNDAUt.AsArray<Double>(A);
  solver.LowerBandWidth := 1;
  solver.UpperBandWidth := 1;
  dense := TLUPDecompositionF64.Create;
  try
    CheckTrue(dense.Initialize(A) = lsisInitialized);
    CheckTrue(solver.Initialize(A) = lsisInitialized);
    CheckEquals(dense.Determinant, solver.Determinant, dTol);
    CheckTrue(dense.Initialize(A) = lsisInitialized);
    CheckTrue(solver.Initialize(M) = lsisInitialized);
    CheckEquals(dense.Determinant, solver.Determinant, dTol);

    CheckTrue(solver.Initialize(A) = lsisInitialized);
    solver.Invert(IA);
    CheckTrue(solver.Initialize(M) = lsisInitialized);
    solver.Invert(MI);

    CheckTrue(ndaAllClose(ndaDot(M, TNDAUt.AsArray<Double>(IA)), TNDAUt.Identity<Double>(5), dTol));
    CheckTrue(ndaAllClose(ndaDot(M, MI), TNDAUt.Identity<Double>(5), dTol));
    CheckTrue(ndaAllClose(M, TNDAUt.AsArray<Double>(A), 0));
  finally
    dense.Free;
  end;
end;

procedure TLUPDecompositionF64Tests.BandSingularLU;
var A: TDoubleArray2D;
begin
  A := TDoubleArray2D.Create(TDoubleArray.Create(1, 0, 0),
    TDoubleArray.Create(0, 0, 0), TDoubleArray.Create(0, 0, 2));
  solver.LowerBandWidth := 0;
  solver.UpperBandWidth := 0;

  CheckEquals(Ord(lsisSingular), Ord(solver.Initialize(A)));
  CheckFalse(solver.Initialized);
end;

procedure TLUPDecompositionF64Tests.BandLUWorkBounds;
var band: TBandLUPF64;
    LU: TDoubleArray2D;
    I: Integer;
begin
  LU := BandMatrix(32, 1, 1);
  band := TBandLUPF64.Create;
  try
    band.LowerBandWidth := 1;
    band.UpperBandWidth := 1;
    band.Counts.ResetCounts;

    CheckTrue(band.InitializeWithoutCopy(LU) = lsisInitialized);

    CheckEquals(32, Length(band.Counts.PivotStarts));
    for I := 0 to 31 do
      CheckTrue(band.Counts.PivotEnds[I] - band.Counts.PivotStarts[I] <= 1);
    CheckTrue(band.Counts.AxpyElements <= 2 * 32, 'Linear factorization work for fixed band');
    CheckTrue(band.Counts.MaxAxpyCount <= 2);
  finally
    band.Free;
  end;
end;

procedure TLUPDecompositionF64Tests.BandLUDenseEquivalence;
var A, LU, denseLU: TDoubleArray2D;
    P, denseP: TArray<NativeInt>;
    dense: TLUPDecompositionF64;
    lower, upper, I, J: Integer;
    ok: Boolean;
begin
  dense := TLUPDecompositionF64.Create;
  try
    for lower := 0 to 3 do
      for upper := 0 to 3 do begin
        A := BandMatrix(7, lower, upper);
        for I := 0 to 6 do
          for J := 0 to 6 do
            if (I - J <= lower) and (J - I <= upper) then
              A[I, J] := 1 + (I * 17 + J * 13) mod 11;
        LU := TDynAUt.Clone<Double>(A);
        denseLU := TDynAUt.Clone<Double>(A);
        solver.LowerBandWidth := lower;
        solver.UpperBandWidth := upper;

        ok := (dense.InitializeWithoutCopy(denseLU) = lsisInitialized);
        denseP := Copy(TLUPAccess(dense).fP);

        CheckEquals(ok, (solver.InitializeWithoutCopy(LU) = lsisInitialized));
        P := Copy(TLUPAccess(solver).fP);
        if ok then begin
          for I := 0 to High(P) do
            CheckEquals(Integer(denseP[I]), Integer(P[I]));
          for I := 0 to 6 do
            CheckEquals(denseLU[I], LU[I], dTol);
        end;
      end;

    A := PivotBandMatrix;
    LU := TDynAUt.Clone<Double>(A);
    solver.LowerBandWidth := 1;
    solver.UpperBandWidth := High(NativeInt);

    CheckTrue(solver.InitializeWithoutCopy(LU) = lsisInitialized);
    P := Copy(TLUPAccess(solver).fP);

    CheckTrue(dense.Initialize(A) = lsisInitialized);
    CheckEquals(dense.Determinant, solver.Determinant, dTol);
  finally
    dense.Free;
  end;
end;

{$endregion}

procedure TBlockedLUPF64Tests.SetUp;
begin
  inherited;
  solver.FactorizationMethod := lupBlocked;
  solver.BlockSize := 2;
end;

initialization

  RegisterTest(TLUPDecompositionF64Tests.Suite);
  RegisterTest(TBlockedLUPF64Tests.Suite);

end.
