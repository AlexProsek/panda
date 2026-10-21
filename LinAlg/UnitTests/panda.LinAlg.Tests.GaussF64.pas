unit panda.LinAlg.Tests.GaussF64;

interface

uses
    TestFramework
  , panda.Nums
  , panda.Intfs
  , panda.Arrays
  , panda.ArrManip
  , panda.Math
  , panda.LinAlg.Gauss
  , panda.LinAlg.GaussF64
  , panda.LinAlg.NumericF64
  , panda.LinAlg.Base
  , panda.Arithmetic
  , panda.DynArrayUtils
  , panda.Tests.NDATestCase
  ;

type
  TGaussElimF64Tests = class(TNDATestCase)
  protected const
    dTol = 1e-14;
  protected
    solver: TGaussElimF64;
    function BandMatrix(aN, aLower, aUpper: Integer): TDoubleArray2D;
    function PivotBandMatrix: TDoubleArray2D;
    function BandRHS(const A: TDoubleArray2D): TDoubleArray;
    procedure CheckBandSystem(const A: TDoubleArray2D; const B: TDoubleArray;
      aLower, aUpper: NativeInt; aNullity: Integer = 0);
  public
    procedure SetUp; override;
    procedure TearDown; override;
    procedure CheckPartSol(const A: TDoubleArray2D; const b, res: TDoubleArray;
      aTol: Double = 0);
    procedure CheckHomSol(const A, Hom: TDoubleArray2D; aTol: Double = 0);
  published
    procedure OneEquation;
    procedure WideMatrixFullRowRank;
    procedure TwoEquations;
    procedure ThreeEquations;
    procedure SolveMatrix4Hom2D;
    procedure SolveMatrix3x4Hom2D;
    procedure InitND;
    procedure InitNDStrided;
    procedure InitWithoutCopyND;
    procedure InitWithoutCopyNDReject;
    procedure InitEmptyRows;
    procedure InitWithoutCopyHomogeneous;
    procedure SolveRelativeTolerance;
    procedure SolveInexactRankDeficient;
    procedure SolveLargeRHS;
    procedure EstimateTolerance;
    procedure EstimateToleranceND;
    procedure EstimateToleranceReuse;
    procedure ZeroToleranceSetsBoth;
    procedure ToleranceAppliedAtSolve;
    procedure TolerancePivotOnly;
    procedure ToleranceFactorDefault;
    procedure ToleranceFactorApplied;
    procedure EstimateToleranceFactor;
    procedure SolveRankDeficientDefaultFactor;
    procedure BandDefaults;
    procedure BandDiagonal;
    procedure BandSingleEquation;
    procedure BandLowerTriangular;
    procedure BandUpperTriangular;
    procedure BandTridiagonal;
    procedure BandAsymmetric;
    procedure BandLowerOnly;
    procedure BandUpperOnly;
    procedure BandOversizedWidths;
    procedure BandNegativeWidths;
    procedure BandPivotFill;
    procedure BandRankDeficient;
    procedure BandToleranceSkippedPivot;
    procedure BandInconsistent;
    procedure BandRectangular;
    procedure BandReuse;
    procedure BandInitNDStrided;
    procedure BandInitWithoutCopy;
    procedure BandInitWithoutCopyND;
    procedure BandWorkBounds;
    procedure BandFallbackBounds;
    procedure BandPivotRange;
    procedure BandRectangularBounds;
  end;

implementation

type
  TBandNumericF64 = class(TLinAlgNumericF64)
  public type
    PDouble = TGaussElim<Double>.PT;
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
  TBandGaussElimF64 = class(TGaussElimF64)
  protected
    function CreateNumeric: TLinAlgNumeric<Double>; override;
  public
    function Counts: TBandNumericF64;
  end;

function TBandGaussElimF64.CreateNumeric: TLinAlgNumeric<Double>;
begin
  Result := TBandNumericF64.Create;
end;

function TBandGaussElimF64.Counts: TBandNumericF64;
begin
  Result := TBandNumericF64(fNumeric);
end;

{$region 'TGaussElimF64Tests'}

procedure TGaussElimF64Tests.SetUp;
begin
  solver := TGaussElimF64.Create;
end;

procedure TGaussElimF64Tests.TearDown;
begin
  solver.Free;
end;

procedure TGaussElimF64Tests.CheckPartSol(const A: TDoubleArray2D;
  const b, res: TDoubleArray; aTol: Double);
var mA, mb, mRes: INDArray<Double>;
begin
  if aTol <= 0 then aTol := dTol;

  mA := TNDAUt.AsArray<Double>(A);
  mb := TNDAUt.AsArray<Double>(b);
  mRes := TNDAUt.AsArray<Double>(res);

  CheckTrue(ndaAllClose(ndaDot(mA, mRes), mb, aTol));
end;

procedure TGaussElimF64Tests.CheckHomSol(const A, Hom: TDoubleArray2D; aTol: Double);
var mA, mH: INDArray<Double>;
begin
  if aTol <= 0 then aTol := dTol;
  
  mA := TNDAUt.AsArray<Double>(A);
  mH := TNDAUt.AsArray<Double>(Hom);

  CheckTrue(ndaAllClose(ndaDot(mA, TNDAMan.Transpose<Double>(mH)), 0, aTol));
end;

procedure TGaussElimF64Tests.WideMatrixFullRowRank;
var A, H: TDoubleArray2D;
    B, X: TDoubleArray;
begin
  // After the only row has a pivot, the remaining columns must be free variables.
  A := TDoubleArray2D.Create(TDoubleArray.Create(1, 2, 3));
  B := TDoubleArray.Create(4);

  CheckTrue(solver.Init(A, B));
  solver.Solve(X, H);

  CheckPartSol(A, B, X);
  CheckEquals(2, Length(H));
  CheckHomSol(A, H);
end;

procedure TGaussElimF64Tests.OneEquation;
var A, h: TDoubleArray2D;
    b, res: TDoubleArray;
begin
  A := TDoubleArray2D.Create(
    TArray<Double>.Create(2)
  );
  b := TArray<Double>.Create(6);

  CheckTrue(solver.Init(A, b));
  solver.Solve(res, h);

  CheckPartSol(A, b, res);
  CheckEquals(0, Length(h));
end;

procedure TGaussElimF64Tests.TwoEquations;
var A, h: TArray<TArray<Double>>;
    b, res: TArray<Double>;
begin
  SetLength(A, 2, 2);
  SetLength(b, 2);

  A[0,0] := 1; A[0,1] := 2;
  A[1,0] := 5; A[1,1] := 1;

  b[0] := 2;
  b[1] := 3;

  CheckTrue(solver.Init(A, b));
  solver.Solve(res, h);

  CheckEquals([4/9, 7/9], res, dTol);
  CheckEquals(0, Length(h));
end;

procedure TGaussElimF64Tests.ThreeEquations;
var A, h: TArray<TArray<Double>>;
    b, res: TArray<Double>;
begin
  SetLength(A, 3, 3);
  SetLength(b, 3);

  A[0,0] := 1; A[0,1] := 2; A[0,2] := -2;
  A[1,0] := 3; A[1,1] := 1; A[1,2] :=  0;
  A[2,0] := 0; A[2,1] :=-2; A[2,2] :=  6;

  b[0] := -1;
  b[1] := 3;
  b[2] := 8;

  CheckTrue(solver.Init(A, b));
  solver.Solve(res, h);

  CheckEquals([7/9, 2/3, 14/9], res, dTol);
  CheckEquals(0, Length(h));
end;

procedure TGaussElimF64Tests.SolveMatrix4Hom2D;
var A, h: TDoubleArray2D;
    b, res: TDoubleArray;
begin
  A := TDoubleArray2D.Create(
    TArray<Double>.Create(1, -3, 4, -8),
    TArray<Double>.Create(2, -6, 1, -2),
    TArray<Double>.Create(3, -9, 5, -10),
    TArray<Double>.Create(0, 0, 0, 0)
  );
  b := TArray<Double>.Create(2, -3, -1, 0);

  CheckTrue(solver.Init(A, b));
  solver.Solve(res, h);

  CheckHomSol(A,H);
  CheckPartSol(A,b,res);
end;

procedure TGaussElimF64Tests.SolveMatrix3x4Hom2D;
var A, h: TDoubleArray2D;
    b, res: TDoubleArray;
begin
  A := TDoubleArray2D.Create(
    TArray<Double>.Create(1, -3, 4, -8),
    TArray<Double>.Create(2, -6, 1, -2),
    TArray<Double>.Create(3, -9, 5, -10)
  );
  b := TArray<Double>.Create(2, -3, -1);

  CheckTrue(solver.Init(A, b));
  solver.Solve(res, h);

  CheckHomSol(A,H);
  CheckPartSol(A,b,res);
end;

procedure TGaussElimF64Tests.InitND;
var A: TDoubleArray2D;
    b, res: TDoubleArray;
    h: TDoubleArray2D;
    mA, mb: INDArray<Double>;
begin
  A := TDoubleArray2D.Create(
    TArray<Double>.Create(1, 2, -2),
    TArray<Double>.Create(3, 1, 0),
    TArray<Double>.Create(0, -2, 6)
  );
  b := TDoubleArray.Create(-1, 3, 8);
  mA := TNDAUt.AsArray<Double>(A);
  mb := TNDAUt.AsArray<Double>(b);

  CheckTrue(solver.Init(mA, mb));
  solver.Solve(res, h);

  CheckEquals([7/9, 2/3, 14/9], res, dTol);
  CheckEquals(0, Length(h));
end;

procedure TGaussElimF64Tests.InitNDStrided;
var wide: TDoubleArray2D;
    mWide, mView, mb: INDArray<Double>;
    b, res: TDoubleArray;
    h: TDoubleArray2D;
begin
  wide := TDoubleArray2D.Create(
    TArray<Double>.Create(1, 0, 2, 0, -2, 0),
    TArray<Double>.Create(3, 0, 1, 0, 0, 0),
    TArray<Double>.Create(0, 0, -2, 0, 6, 0)
  );
  b := TDoubleArray.Create(-1, 3, 8);
  mWide := TNDAUt.AsArray<Double>(wide);
  mView := mWide[[NDIAll, NDISpan(0, -1, 2)]];
  mb := TNDAUt.AsArray<Double>(b);

  CheckEquals(SizeOf(Double) * 2, Integer(mView.Strides[1]));

  CheckTrue(solver.Init(mView, mb));
  solver.Solve(res, h);

  CheckEquals([7/9, 2/3, 14/9], res, dTol);
  CheckEquals(0, Length(h));
end;

procedure TGaussElimF64Tests.InitWithoutCopyND;
var aug: TDoubleArray2D;
    mAug: INDArray<Double>;
    res: TDoubleArray;
    h: TDoubleArray2D;
begin
  aug := TDoubleArray2D.Create(
    TArray<Double>.Create(1, 2, -2, -1),
    TArray<Double>.Create(3, 1, 0, 3),
    TArray<Double>.Create(0, -2, 6, 8)
  );
  mAug := TNDAUt.AsArray<Double>(aug);

  CheckTrue(solver.InitWithoutCopy(mAug));
  solver.Solve(res, h);

  CheckEquals([7/9, 2/3, 14/9], res, dTol);
  CheckEquals(0, Length(h));
end;

procedure TGaussElimF64Tests.InitWithoutCopyNDReject;
var A, wide: TDoubleArray2D;
    mA, mWide, mView: INDArray<Double>;
begin
  A := TDoubleArray2D.Create(
    TArray<Double>.Create(1, 2),
    TArray<Double>.Create(3, 4)
  );
  mA := TNDAUt.AsArray<Double>(A);

  CheckFalse(solver.InitWithoutCopy(mA));

  wide := TDoubleArray2D.Create(
    TArray<Double>.Create(1, 0, 2, 0, 3, 0),
    TArray<Double>.Create(4, 0, 5, 0, 6, 0)
  );
  mWide := TNDAUt.AsArray<Double>(wide);
  mView := mWide[[NDIAll, NDISpan(0, -1, 2)]];

  CheckEquals(SizeOf(Double) * 2, Integer(mView.Strides[1]));
  CheckFalse(solver.InitWithoutCopy(mView));
end;

procedure TGaussElimF64Tests.InitEmptyRows;
var A: TDoubleArray2D;
begin
  SetLength(A, 2, 0);

  CheckFalse(solver.Init(A));

  CheckFalse(solver.Init(A, TDoubleArray.Create(1, 2)));
end;

procedure TGaussElimF64Tests.InitWithoutCopyHomogeneous;
var A, aug, h: TDoubleArray2D;
    res: TDoubleArray;
begin
  A := TDoubleArray2D.Create(
    TArray<Double>.Create(1, 2),
    TArray<Double>.Create(2, 4)
  );
  aug := TDoubleArray2D.Create(
    TArray<Double>.Create(1, 2, 0),
    TArray<Double>.Create(2, 4, 0)
  );

  CheckTrue(solver.InitWithoutCopy(aug));
  solver.Solve(res, h);

  CheckEquals([0, 0], res, 0);
  CheckEquals(1, Length(h));
  CheckHomSol(A, h);
end;

procedure TGaussElimF64Tests.SolveRelativeTolerance;
const s = 8.470329472543003e-22; // 2^-70, keeps the scaled elimination exact
var A, sA, h: TDoubleArray2D;
    b, sb, res: TDoubleArray;
    I, J: Integer;
begin
  A := TDoubleArray2D.Create(
    TArray<Double>.Create(1, -3, 4, -8),
    TArray<Double>.Create(2, -6, 1, -2),
    TArray<Double>.Create(3, -9, 5, -10)
  );
  b := TArray<Double>.Create(2, -3, -1);
  sA := TDynAUt.Clone<Double>(A);
  SetLength(sb, Length(b));
  for I := 0 to High(sA) do begin
    for J := 0 to High(sA[I]) do
      sA[I, J] := s * sA[I, J];
    sb[I] := s * b[I];
  end;

  CheckTrue(solver.Init(sA, sb));
  solver.Solve(res, h);

  CheckEquals(2, Length(h));
  CheckHomSol(A, h);
  CheckPartSol(A, b, res);
end;

procedure TGaussElimF64Tests.SolveInexactRankDeficient;
const tol = 1e-12;
var A, h: TDoubleArray2D;
    b, res: TDoubleArray;
    I: Integer;
begin
  SetLength(A, 4, 4);
  A[0] := TArray<Double>.Create(0.1, 0.7, 0.3, 0.9);
  A[1] := TArray<Double>.Create(0.6, 0.2, 0.8, 0.4);
  for I := 0 to 3 do begin
    A[2, I] := A[0, I] + A[1, I];
    A[3, I] := 0.3 * A[0, I] + 0.7 * A[1, I];
  end;
  b := TArray<Double>.Create(1.3, 2.9, 1.3 + 2.9, 0.3 * 1.3 + 0.7 * 2.9);

  CheckTrue(solver.Init(A, b));
  solver.Solve(res, h);

  CheckEquals(4, Length(res));
  CheckEquals(2, Length(h));
  CheckHomSol(A, h, tol);
  CheckPartSol(A, b, res, tol);
end;

procedure TGaussElimF64Tests.SolveLargeRHS;
const tol = 1e-3;
var A, h: TDoubleArray2D;
    b, res: TDoubleArray;
begin
  A := TDoubleArray2D.Create(
    TArray<Double>.Create(1, -3, 4, -8),
    TArray<Double>.Create(2, -6, 1, -2),
    TArray<Double>.Create(3, -9, 5, -10)
  );
  b := TArray<Double>.Create(1e10 / 3, 1e10 / 7, 1e10 / 3 + 1e10 / 7);

  CheckTrue(solver.Init(A, b));
  solver.Solve(res, h);

  CheckEquals(4, Length(res));
  CheckEquals(2, Length(h));
  CheckPartSol(A, b, res, tol);
end;

procedure TGaussElimF64Tests.EstimateTolerance;
const k = TGaussElimF64.cDefaultTolFactor;
var A: TDoubleArray2D;
    tol: TGaussTolerance;
begin
  A := TDoubleArray2D.Create(
    TArray<Double>.Create(1, -3, 4, -8),
    TArray<Double>.Create(2, -6, 1, -2),
    TArray<Double>.Create(3, -9, 5, -10)
  );

  tol := TGaussElimF64.EstimateTolerance(A, TDoubleArray.Create(2, -30, -1));

  CheckEquals(k * 4 * cEpsF64 * 10, tol.Pivot, 1e-25);
  CheckEquals(k * 4 * cEpsF64 * 30, tol.RHS, 1e-25);

  tol := TGaussElimF64.EstimateTolerance(A);

  CheckEquals(k * 4 * cEpsF64 * 10, tol.Pivot, 1e-25);
  CheckEquals(0, tol.RHS, 0);

  A := nil;
  tol := TGaussElimF64.EstimateTolerance(A);

  CheckEquals(0, tol.Pivot, 0);
  CheckEquals(0, tol.RHS, 0);
end;

procedure TGaussElimF64Tests.EstimateToleranceND;
const k = TGaussElimF64.cDefaultTolFactor;
var wide: TDoubleArray2D;
    mWide, mView, mb: INDArray<Double>;
    tol: TGaussTolerance;
begin
  wide := TDoubleArray2D.Create(
    TArray<Double>.Create(1, 0, 2, 0, -2, 0),
    TArray<Double>.Create(3, 0, 1, 0, 0, 0),
    TArray<Double>.Create(0, 0, -2, 0, 6, 0)
  );
  mWide := TNDAUt.AsArray<Double>(wide);
  mView := mWide[[NDIAll, NDISpan(0, -1, 2)]];
  mb := TNDAUt.AsArray<Double>(TDoubleArray.Create(-1, 3, 8));

  tol := TGaussElimF64.EstimateTolerance(mView, mb);

  CheckEquals(k * 3 * cEpsF64 * 6, tol.Pivot, 1e-25);
  CheckEquals(k * 3 * cEpsF64 * 8, tol.RHS, 1e-25);

  tol := TGaussElimF64.EstimateTolerance(mView);

  CheckEquals(k * 3 * cEpsF64 * 6, tol.Pivot, 1e-25);
  CheckEquals(0, tol.RHS, 0);
end;

procedure TGaussElimF64Tests.EstimateToleranceReuse;
const tol = 1e-3;
var A, h: TDoubleArray2D;
    b, res: TDoubleArray;
begin
  A := TDoubleArray2D.Create(
    TArray<Double>.Create(1, -3, 4, -8),
    TArray<Double>.Create(2, -6, 1, -2),
    TArray<Double>.Create(3, -9, 5, -10)
  );
  b := TArray<Double>.Create(1e10 / 3, 1e10 / 7, 1e10 / 3 + 1e10 / 7);
  solver.Tolerance := TGaussElimF64.EstimateTolerance(A, b);

  CheckTrue(solver.Init(A, b));
  solver.Solve(res, h);

  CheckEquals(4, Length(res));
  CheckEquals(2, Length(h));
  CheckPartSol(A, b, res, tol);

  CheckTrue(solver.Init(A, b));
  solver.Solve(res, h);

  CheckEquals(4, Length(res));
  CheckEquals(2, Length(h));
  CheckPartSol(A, b, res, tol);
end;

procedure TGaussElimF64Tests.ZeroToleranceSetsBoth;
var t: Double;
    tolerance: TGaussTolerance;
begin
  t := 1e-6;

  solver.ZeroTolerance := t;
  tolerance := solver.Tolerance;

  CheckEquals(t, solver.ZeroTolerance, 0);
  CheckEquals(t, tolerance.Pivot, 0);
  CheckEquals(t, tolerance.RHS, 0);
end;

procedure TGaussElimF64Tests.ToleranceAppliedAtSolve;
var A, h: TDoubleArray2D;
    b, res: TDoubleArray;
begin
  A := TDoubleArray2D.Create(
    TArray<Double>.Create(1, 0),
    TArray<Double>.Create(0, 1e-6)
  );
  b := TArray<Double>.Create(1, 0);

  CheckTrue(solver.Init(A, b));
  solver.ZeroTolerance := 1e-3;
  solver.Solve(res, h);

  CheckEquals(1, Length(h));

  CheckTrue(solver.Init(A, b));
  solver.ZeroTolerance := 0;
  solver.Solve(res, h);

  CheckEquals(0, Length(h));
  CheckEquals([1, 0], res, dTol);
end;

procedure TGaussElimF64Tests.TolerancePivotOnly;
const tol = 1e-3;
var A, h: TDoubleArray2D;
    b, res: TDoubleArray;
    t: TGaussTolerance;
begin
  A := TDoubleArray2D.Create(
    TArray<Double>.Create(1, -3, 4, -8),
    TArray<Double>.Create(2, -6, 1, -2),
    TArray<Double>.Create(3, -9, 5, -10)
  );
  b := TArray<Double>.Create(1e10 / 3, 1e10 / 7, 1e10 / 3 + 1e10 / 7);
  t.Pivot := 1e-10;
  t.RHS := 0;
  solver.Tolerance := t;

  CheckTrue(solver.Init(A, b));
  solver.Solve(res, h);

  CheckEquals(4, Length(res));
  CheckEquals(2, Length(h));
  CheckPartSol(A, b, res, tol);
end;

procedure TGaussElimF64Tests.ToleranceFactorDefault;
begin
  CheckEquals(TGaussElimF64.cDefaultTolFactor, solver.ToleranceFactor, 0);
end;

procedure TGaussElimF64Tests.ToleranceFactorApplied;
var A, h: TDoubleArray2D;
    b, res: TDoubleArray;
begin
  A := TDoubleArray2D.Create(
    TArray<Double>.Create(1, 0),
    TArray<Double>.Create(0, 1e-15)
  );
  b := TArray<Double>.Create(1, 0);

  CheckTrue(solver.Init(A, b));
  solver.Solve(res, h);

  CheckEquals(1, Length(h));

  CheckTrue(solver.Init(A, b));
  solver.ToleranceFactor := 1;
  solver.Solve(res, h);

  CheckEquals(0, Length(h));
  CheckEquals([1, 0], res, dTol);

  CheckTrue(solver.Init(A, b));
  solver.ToleranceFactor := 0;
  solver.Solve(res, h);

  CheckEquals(1, Length(h));
end;

procedure TGaussElimF64Tests.EstimateToleranceFactor;
var A: TDoubleArray2D;
    tol: TGaussTolerance;
begin
  A := TDoubleArray2D.Create(
    TArray<Double>.Create(1, -3, 4, -8),
    TArray<Double>.Create(2, -6, 1, -2),
    TArray<Double>.Create(3, -9, 5, -10)
  );

  tol := TGaussElimF64.EstimateTolerance(A, TDoubleArray.Create(2, -30, -1), 1);

  CheckEquals(4 * cEpsF64 * 10, tol.Pivot, 1e-28);
  CheckEquals(4 * cEpsF64 * 30, tol.RHS, 1e-28);

  tol := TGaussElimF64.EstimateTolerance(A, nil, 0);

  CheckEquals(TGaussElimF64.cDefaultTolFactor * 4 * cEpsF64 * 10, tol.Pivot, 1e-25);
end;

procedure TGaussElimF64Tests.SolveRankDeficientDefaultFactor;
const tol = 1e-10;
var A, h: TDoubleArray2D;
    b, res: TDoubleArray;
begin
  A := TDoubleArray2D.Create(
    TArray<Double>.Create(4.380000000000003, -0.38999999999999774, 11.549999999999999, 73.5),
    TArray<Double>.Create(24.04, 24.9, -19.28, -16.05),
    TArray<Double>.Create(-36.58, -40.83, 38.33, 70.05),
    TArray<Double>.Create(32.66, 32.989999999999995, -23.63, -8.799999999999999)
  );
  b := TArray<Double>.Create(614.52, -211.69, 712.03, -176.45999999999995);

  CheckTrue(solver.Init(A, b));
  solver.Solve(res, h);

  CheckEquals(4, Length(res));
  CheckEquals(2, Length(h));
  CheckHomSol(A, h, tol);
  CheckPartSol(A, b, res, tol);
end;

function TGaussElimF64Tests.BandMatrix(aN, aLower, aUpper: Integer): TDoubleArray2D;
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

function TGaussElimF64Tests.PivotBandMatrix: TDoubleArray2D;
begin
  Result := TDoubleArray2D.Create(
    TDoubleArray.Create(1, 2, 0, 0, 0),
    TDoubleArray.Create(4, 1, 3, 0, 0),
    TDoubleArray.Create(0, 5, 1, 2, 0),
    TDoubleArray.Create(0, 0, 6, 1, 4),
    TDoubleArray.Create(0, 0, 0, 7, 2)
  );
end;

function TGaussElimF64Tests.BandRHS(const A: TDoubleArray2D): TDoubleArray;
var I, J: Integer;
begin
  SetLength(Result, Length(A));
  for I := 0 to High(A) do begin
    Result[I] := 0;
    for J := 0 to High(A[I]) do
      Result[I] := Result[I] + A[I, J] * (J + 1);
  end;
end;

procedure TGaussElimF64Tests.CheckBandSystem(const A: TDoubleArray2D;
  const B: TDoubleArray; aLower, aUpper: NativeInt; aNullity: Integer);
var dense: TGaussElimF64;
    X, denseX, zeroB: TDoubleArray;
    H, denseH: TDoubleArray2D;
    I: Integer;
begin
  dense := TGaussElimF64.Create;
  try
    CheckTrue(dense.Init(A, B));
    CheckTrue(solver.Init(A, B));
    solver.LowerBandWidth := aLower;
    solver.UpperBandWidth := aUpper;

    dense.Solve(denseX, denseH);
    solver.Solve(X, H);

    CheckEquals(denseX, X, dTol);
    if B = nil then begin
      SetLength(zeroB, Length(A));
      CheckPartSol(A, zeroB, X, 1e-12);
    end else
      CheckPartSol(A, B, X, 1e-12);
    CheckEquals(aNullity, Length(H));
    CheckEquals(Length(denseH), Length(H));
    for I := 0 to High(H) do
      CheckEquals(denseH[I], H[I], dTol);
    if Length(H) > 0 then
      CheckHomSol(A, H);
  finally
    dense.Free;
  end;
end;

procedure TGaussElimF64Tests.BandDefaults;
begin
  CheckEquals(-1, Integer(solver.LowerBandWidth));
  CheckEquals(-1, Integer(solver.UpperBandWidth));
end;

procedure TGaussElimF64Tests.BandDiagonal;
var A: TDoubleArray2D;
begin
  A := BandMatrix(6, 0, 0);

  CheckBandSystem(A, BandRHS(A), 0, 0);
end;

procedure TGaussElimF64Tests.BandSingleEquation;
var A: TDoubleArray2D;
begin
  A := BandMatrix(1, 0, 0);

  CheckBandSystem(A, BandRHS(A), 0, 0);
end;

procedure TGaussElimF64Tests.BandLowerTriangular;
var A: TDoubleArray2D;
begin
  A := BandMatrix(6, 2, 0);

  CheckBandSystem(A, BandRHS(A), 2, 0);
end;

procedure TGaussElimF64Tests.BandUpperTriangular;
var A: TDoubleArray2D;
begin
  A := BandMatrix(6, 0, 2);

  CheckBandSystem(A, BandRHS(A), 0, 2);
end;

procedure TGaussElimF64Tests.BandTridiagonal;
var A: TDoubleArray2D;
begin
  A := BandMatrix(8, 1, 1);

  CheckBandSystem(A, BandRHS(A), 1, 1);
end;

procedure TGaussElimF64Tests.BandAsymmetric;
var A: TDoubleArray2D;
begin
  A := BandMatrix(8, 2, 1);

  CheckBandSystem(A, BandRHS(A), 2, 1);

  A := BandMatrix(8, 1, 3);

  CheckBandSystem(A, BandRHS(A), 1, 3);
end;

procedure TGaussElimF64Tests.BandLowerOnly;
var A: TDoubleArray2D;
begin
  A := BandMatrix(8, 1, 7);

  CheckBandSystem(A, BandRHS(A), 1, -1);
end;

procedure TGaussElimF64Tests.BandUpperOnly;
var A: TDoubleArray2D;
begin
  A := BandMatrix(8, 7, 1);

  CheckBandSystem(A, BandRHS(A), -1, 1);
end;

procedure TGaussElimF64Tests.BandOversizedWidths;
var A: TDoubleArray2D;
begin
  A := BandMatrix(6, 5, 5);

  CheckBandSystem(A, BandRHS(A), High(NativeInt), High(NativeInt));

  A := BandMatrix(6, 1, 5);

  CheckBandSystem(A, BandRHS(A), 1, High(NativeInt));
end;

procedure TGaussElimF64Tests.BandNegativeWidths;
var A: TDoubleArray2D;
begin
  A := BandMatrix(6, 5, 5);

  CheckBandSystem(A, BandRHS(A), Low(NativeInt), -2);
end;

procedure TGaussElimF64Tests.BandPivotFill;
var A: TDoubleArray2D;
begin
  A := PivotBandMatrix;

  CheckBandSystem(A, BandRHS(A), 1, 1);
end;

procedure TGaussElimF64Tests.BandRankDeficient;
var A: TDoubleArray2D;
begin
  A := TDoubleArray2D.Create(
    TDoubleArray.Create(2, 1, 0, 0, 0),
    TDoubleArray.Create(0, 0, 1, 0, 0),
    TDoubleArray.Create(0, 0, 2, 1, 0),
    TDoubleArray.Create(0, 0, 0, 3, 1),
    TDoubleArray.Create(0, 0, 0, 0, 4)
  );

  CheckBandSystem(A, BandRHS(A), 1, 1, 1);

  CheckBandSystem(A, nil, 1, 1, 1);
end;

procedure TGaussElimF64Tests.BandToleranceSkippedPivot;
var A, H, denseH: TDoubleArray2D;
    B, X, denseX: TDoubleArray;
    dense: TGaussElimF64;
begin
  A := TDoubleArray2D.Create(
    TDoubleArray.Create(4, 1, 0, 0),
    TDoubleArray.Create(0, 1e-10, 2, 0),
    TDoubleArray.Create(0, 0, 3, 1),
    TDoubleArray.Create(0, 0, 0, 4)
  );
  B := BandRHS(A);
  dense := TGaussElimF64.Create;
  try
    dense.ZeroTolerance := 1e-8;
    solver.ZeroTolerance := 1e-8;
    solver.LowerBandWidth := 1;
    solver.UpperBandWidth := 1;
    CheckTrue(dense.Init(A, B));
    CheckTrue(solver.Init(A, B));

    dense.Solve(denseX, denseH);
    solver.Solve(X, H);

    CheckEquals(denseX, X, dTol);
    CheckPartSol(A, B, X, 1e-8);
    CheckEquals(1, Length(H));
    CheckEquals(denseH[0], H[0], dTol);
    CheckHomSol(A, H, 1e-8);
  finally
    dense.Free;
  end;
end;

procedure TGaussElimF64Tests.BandInconsistent;
var A, H: TDoubleArray2D;
    X: TDoubleArray;
begin
  A := TDoubleArray2D.Create(TDoubleArray.Create(1, 0, 0),
    TDoubleArray.Create(0, 0, 0), TDoubleArray.Create(0, 0, 1));
  solver.LowerBandWidth := 0;
  solver.UpperBandWidth := 0;
  CheckTrue(solver.Init(A, TDoubleArray.Create(1, 1, 1)));

  solver.Solve(X, H);

  CheckEquals(0, Length(X));
  CheckEquals(0, Length(H));
end;

procedure TGaussElimF64Tests.BandRectangular;
var A: TDoubleArray2D;
begin
  A := TDoubleArray2D.Create(TDoubleArray.Create(1, 2, 0),
    TDoubleArray.Create(0, 3, 4));

  CheckBandSystem(A, BandRHS(A), 1, 1, 1);

  A := TDoubleArray2D.Create(TDoubleArray.Create(2, 1),
    TDoubleArray.Create(1, 3), TDoubleArray.Create(0, 4));

  CheckBandSystem(A, BandRHS(A), 1, 1);
end;

procedure TGaussElimF64Tests.BandReuse;
var A: TDoubleArray2D;
begin
  A := BandMatrix(6, 1, 1);

  CheckBandSystem(A, BandRHS(A), 1, 1);

  CheckTrue(solver.Init(A));

  CheckEquals(1, Integer(solver.LowerBandWidth));
  CheckEquals(1, Integer(solver.UpperBandWidth));

  A := BandMatrix(4, 3, 3);

  CheckBandSystem(A, BandRHS(A), -1, -1);

  A := BandMatrix(3, 0, 0);

  CheckBandSystem(A, BandRHS(A), 0, 0);
end;

procedure TGaussElimF64Tests.BandInitNDStrided;
var A, wide, H: TDoubleArray2D;
    B, X, paddedB: TDoubleArray;
    M, V, MB: INDArray<Double>;
    I, J: Integer;
begin
  A := PivotBandMatrix;
  B := BandRHS(A);
  SetLength(wide, 5, 10);
  SetLength(paddedB, 10);
  for I := 0 to 4 do begin
    for J := 0 to 4 do
      wide[I, 2 * J] := A[I, J];
    paddedB[2 * I] := B[I];
  end;
  M := TNDAUt.AsArray<Double>(wide);
  V := M[[NDIAll, NDISpan(0, -1, 2)]];
  MB := TNDAUt.AsArray<Double>(paddedB);
  solver.LowerBandWidth := 1;
  solver.UpperBandWidth := 1;
  CheckTrue(solver.Init(V, MB[[NDISpan(0, -1, 2)]]));

  solver.Solve(X, H);

  CheckPartSol(A, B, X);
  CheckEquals(0, Length(H));
  CheckTrue(ndaAllClose(M, TNDAUt.AsArray<Double>(wide), 0));
end;

procedure TGaussElimF64Tests.BandInitWithoutCopy;
var A, aug, H: TDoubleArray2D;
    B, X, row: TDoubleArray;
    I, J: Integer;
begin
  A := PivotBandMatrix;
  B := BandRHS(A);
  SetLength(aug, 5, 6);
  for I := 0 to 4 do begin
    for J := 0 to 4 do
      aug[I, J] := A[I, J];
    aug[I, 5] := B[I];
  end;
  row := aug[0];
  solver.LowerBandWidth := 1;
  solver.UpperBandWidth := 1;
  CheckTrue(solver.InitWithoutCopy(aug));

  solver.Solve(X, H);

  CheckPartSol(A, B, X);
  CheckEquals(0, Length(H));
  CheckTrue(@row[0] = @aug[0, 0]);
  CheckEquals(0, aug[0, 0], 0);
end;

procedure TGaussElimF64Tests.BandInitWithoutCopyND;
var A, aug, H: TDoubleArray2D;
    B, X: TDoubleArray;
    M, V: INDArray<Double>;
    items: TNDAItems<Double>;
    I, J: Integer;
begin
  A := PivotBandMatrix;
  B := BandRHS(A);
  SetLength(aug, 5, 7);
  for I := 0 to 4 do begin
    for J := 0 to 4 do
      aug[I, J] := A[I, J];
    aug[I, 5] := B[I];
    aug[I, 6] := 99;
  end;
  M := TNDAUt.AsArray<Double>(aug);
  V := M[[NDIAll, NDISpan(0, 5)]];
  solver.LowerBandWidth := 1;
  solver.UpperBandWidth := 1;
  CheckTrue(solver.InitWithoutCopy(V));

  solver.Solve(X, H);

  CheckPartSol(A, B, X);
  CheckEquals(0, Length(H));
  items := M;
  for I := 0 to 4 do
    CheckEquals(99, items[[I, 6]], 0);
end;

procedure TGaussElimF64Tests.BandWorkBounds;
var band: TBandGaussElimF64;
    A, H: TDoubleArray2D;
    B, X: TDoubleArray;
    I: Integer;
begin
  A := BandMatrix(32, 1, 1);
  B := BandRHS(A);
  band := TBandGaussElimF64.Create;
  try
    band.LowerBandWidth := 1;
    band.UpperBandWidth := 1;
    CheckTrue(band.Init(A, B));
    band.Counts.ResetCounts;

    band.Solve(X, H);

    CheckPartSol(A, B, X, 1e-12);
    CheckEquals(32, Length(band.Counts.PivotStarts));
    for I := 0 to 31 do
      CheckTrue(band.Counts.PivotEnds[I] - band.Counts.PivotStarts[I] <= 1, 'Bounded pivot search');
    CheckTrue(band.Counts.AxpyElements <= 3 * 32, 'Linear elimination work for fixed band');
    CheckTrue(band.Counts.DotElements <= 2 * 32, 'Linear back substitution work for fixed band');
    CheckTrue(band.Counts.MaxAxpyCount <= 3);
    CheckTrue(band.Counts.MaxDotCount <= 2);
  finally
    band.Free;
  end;
end;

procedure TGaussElimF64Tests.BandFallbackBounds;
var band: TBandGaussElimF64;
    A, H: TDoubleArray2D;
    B, X: TDoubleArray;
begin
  A := BandMatrix(6, 0, 1);
  A[1, 1] := 0;
  B := BandRHS(A);
  band := TBandGaussElimF64.Create;
  try
    band.LowerBandWidth := 0;
    band.UpperBandWidth := 1;
    CheckTrue(band.Init(A, B));
    band.Counts.ResetCounts;

    band.Solve(X, H);

    CheckPartSol(A, B, X);
    CheckEquals(0, Integer(band.Counts.PivotEnds[0]));
    CheckEquals(1, Integer(band.Counts.PivotEnds[1]));
    CheckEquals(5, Integer(band.Counts.PivotEnds[2]));
    CheckTrue(band.Counts.MaxDotCount > 1, 'Back substitution also returns to dense processing');
  finally
    band.Free;
  end;
end;

procedure TGaussElimF64Tests.BandPivotRange;
var A: TDoubleArray2D;
    rows: TArray<TGaussElimF64.PT>;
    band: TBandGaussElimF64;
    I: Integer;
begin
  A := TDoubleArray2D.Create(TDoubleArray.Create(2),
    TDoubleArray.Create(-2), TDoubleArray.Create(9));
  SetLength(rows, 3);
  for I := 0 to 2 do
    rows[I] := @A[I, 0];
  band := TBandGaussElimF64.Create;
  try
    CheckEquals(0, Integer(band.Counts.ArgMax(rows, 0, 0, 1)));
    CheckEquals(1, Integer(band.Counts.ArgMax(rows, 1, 0, 1)));
    CheckEquals(2, Integer(band.Counts.ArgMax(rows, 0, 0, 2)));
  finally
    band.Free;
  end;
end;

procedure TGaussElimF64Tests.BandRectangularBounds;
var A, H: TDoubleArray2D;
    B, X: TDoubleArray;
    band: TBandGaussElimF64;
    I: Integer;
begin
  A := TDoubleArray2D.Create(
    TDoubleArray.Create(1, 2, 0, 0, 0),
    TDoubleArray.Create(0, 3, 4, 0, 0), 
    TDoubleArray.Create(0, 0, 5, 6, 0)
  );
  B := BandRHS(A);
  band := TBandGaussElimF64.Create;
  try
    band.LowerBandWidth := 0;
    band.UpperBandWidth := 1;
    CheckTrue(band.Init(A, B));
    band.Counts.ResetCounts;

    band.Solve(X, H);

    CheckPartSol(A, B, X);
    CheckEquals(2, Length(H));
    for I := 0 to High(band.Counts.PivotEnds) do
      CheckEquals(2, Integer(band.Counts.PivotEnds[I]));
    CheckTrue(band.Counts.MaxDotCount > 1);
  finally
    band.Free;
  end;
end;

initialization

  RegisterTest(TGaussElimF64Tests.Suite);

end.
