unit panda.LinAlg.Tests.GaussF128;

interface

uses
    TestFramework
  , panda.NumsQP
  , panda.Intfs
  , panda.Arrays
  , panda.ArrManip
  , panda.LinAlg.GaussF128
  , panda.LinAlg.NumericF128
  ;

type
  TQPVector = TArray<TReal128>;
  TQPMatrix = TArray<TQPVector>;

  TGaussElimF128Tests = class(TTestCase)
  protected
    solver: TGaussElimF128;
    function Matrix: TQPMatrix;
    function BandMatrix: TQPMatrix;
    procedure CheckClose(const Expected, Actual: TReal128);
    procedure CheckSolution(const A: TQPMatrix; const B, X: TQPVector);
    procedure CheckInverse(const A, IA: TQPMatrix);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure UniqueSolution;
    procedure WideMatrix;
    procedure HomogeneousSystem;
    procedure DependentSystem;
    procedure InconsistentSystem;
    procedure AllZeroSystem;
    procedure PrecisionBeyondDouble;
    procedure TinyNonzeroRHS;
    procedure SolveLargeScale;
    procedure SolveSmallScale;
    procedure InitNDStrided;
    procedure InitWithoutCopyDynamic;
    procedure InitWithoutCopyND;
    procedure RejectNDLayouts;
    procedure EstimateTolerance;
    procedure EstimateToleranceWithoutInstance;
    procedure EstimateToleranceND;
    procedure EstimateToleranceEmpty;
    procedure EstimateToleranceInvalid;
    procedure EstimateToleranceFactor;
    procedure EstimateToleranceLargeScale;
    procedure EstimateToleranceSmallScale;
    procedure ToleranceAppliedAtSolve;
    procedure ToleranceFactorApplied;
    procedure TolerancePivotOnly;
    procedure ToleranceRHSOnly;
    procedure ToleranceBoundary;
    procedure ToleranceReuse;
    procedure ReinitializeTolerance;
    procedure CopyInputOwnership;
    procedure EmptyDot;
    procedure InPlaceVectors;
    procedure BandSolve;
    procedure BandFallback;
    procedure BandPivotRange;
  end;

implementation

type
  TGaussElimF128Access = class(TGaussElimF128);
  TNumericF128Access = class(TLinAlgNumericF128);

  TConstructionNumericF128 = class(TLinAlgNumericF128)
  public
    class var ConstructionCount: Integer;
    procedure AfterConstruction; override;
  end;

procedure TConstructionNumericF128.AfterConstruction;
begin
  inherited;
  Inc(ConstructionCount);
end;

function Q(aValue: Double): TReal128;
begin
  Result := aValue;
end;

function Pow2(aExponent: Integer): TReal128;
begin
  Result := cOneF128;
  Result.Exponent := aExponent;
end;

procedure TGaussElimF128Tests.SetUp;
begin
  solver := TGaussElimF128.Create;
end;

procedure TGaussElimF128Tests.TearDown;
begin
  solver.Free;
end;

function TGaussElimF128Tests.Matrix: TQPMatrix;
begin
  Result := TQPMatrix.Create(TQPVector.Create(Q(0), Q(2)),
    TQPVector.Create(Q(3), Q(4)));
end;

procedure TGaussElimF128Tests.CheckClose(const Expected, Actual: TReal128);
var Scale, Error: TReal128;
begin
  Scale := Expected.Abs;
  if Scale < cOneF128 then Scale := cOneF128;
  Error := (Actual - Expected).Abs;

  CheckTrue(Error <= Q(4096) * cEpsF128 * Scale, 'TReal128 residual');
end;

procedure TGaussElimF128Tests.CheckSolution(const A: TQPMatrix;
  const B, X: TQPVector);
var I, J: Integer;
    S: TReal128;
begin
  CheckEquals(Length(A[0]), Length(X));
  CheckEquals(Length(A), Length(B));
  for I := 0 to High(A) do begin
    S := cZeroF128;
    for J := 0 to High(X) do
      S := S + A[I, J] * X[J];

    CheckClose(B[I], S);
  end;
end;

procedure TGaussElimF128Tests.CheckInverse(const A, IA: TQPMatrix);
var I, J, K: Integer;
    S, Expected: TReal128;
begin
  CheckEquals(Length(A), Length(IA));
  for I := 0 to High(A) do
    for J := 0 to High(A) do begin
      S := cZeroF128;
      for K := 0 to High(A) do
        S := S + A[I, K] * IA[K, J];
      Expected := cZeroF128;
      if I = J then Expected := cOneF128;

      CheckClose(Expected, S);
    end;
end;

procedure TGaussElimF128Tests.UniqueSolution;
var A, H: TQPMatrix;
    B, X: TQPVector;
begin
  A := TQPMatrix.Create(
    TQPVector.Create(Q(1), Q(2)),
    TQPVector.Create(Q(5), Q(1))
  );
  B := TQPVector.Create(Q(2), Q(3));

  CheckTrue(solver.Init(A, B));
  solver.Solve(X, H);

  CheckEquals(0, Length(H));
  CheckSolution(A, B, X);
  CheckClose(Q(4) / Q(9), X[0]);
  CheckClose(Q(7) / Q(9), X[1]);
end;

procedure TGaussElimF128Tests.WideMatrix;
var A, H: TQPMatrix;
    B, X: TQPVector;
begin
  A := TQPMatrix.Create(TQPVector.Create(Q(1), Q(2), Q(3)));
  B := TQPVector.Create(Q(4));

  CheckTrue(solver.Init(A, B));
  solver.Solve(X, H);

  CheckEquals(2, Length(H));
  CheckSolution(A, B, X);
  CheckSolution(A, TQPVector.Create(Q(0)), H[0]);
  CheckSolution(A, TQPVector.Create(Q(0)), H[1]);
end;

procedure TGaussElimF128Tests.HomogeneousSystem;
var A, H: TQPMatrix;
    X: TQPVector;
begin
  A := TQPMatrix.Create(TQPVector.Create(Q(1), Q(2)));

  CheckTrue(solver.Init(A));
  solver.Solve(X, H);

  CheckEquals(1, Length(H));
  CheckTrue(X[0].IsExactZero);
  CheckTrue(X[1].IsExactZero);
  CheckSolution(A, TQPVector.Create(Q(0)), H[0]);
end;

procedure TGaussElimF128Tests.DependentSystem;
var A, H: TQPMatrix;
    B, X: TQPVector;
begin
  A := TQPMatrix.Create(
    TQPVector.Create(Q(1) / Q(3), Q(2) / Q(7)),
    TQPVector.Create(Q(2) / Q(3), Q(4) / Q(7))
  );
  B := TQPVector.Create(Q(1), Q(2));

  CheckTrue(solver.Init(A, B));
  solver.Solve(X, H);

  CheckEquals(1, Length(H));
  CheckSolution(A, B, X);
  CheckSolution(A, TQPVector.Create(Q(0), Q(0)), H[0]);
end;

procedure TGaussElimF128Tests.InconsistentSystem;
var A, H: TQPMatrix;
    X: TQPVector;
begin
  A := TQPMatrix.Create(
    TQPVector.Create(Q(1), Q(2)),
    TQPVector.Create(Q(2), Q(4))
  );

  CheckTrue(solver.Init(A, TQPVector.Create(Q(1), Q(3))));
  solver.Solve(X, H);

  CheckEquals(0, Length(X));
  CheckEquals(0, Length(H));
end;

procedure TGaussElimF128Tests.AllZeroSystem;
var A, H: TQPMatrix;
    X: TQPVector;
begin
  SetLength(A, 2, 2);
  A[0, 0] := -cZeroF128;

  CheckTrue(solver.Init(A));
  solver.Solve(X, H);

  CheckEquals(2, Length(H));
  CheckTrue(X[0].IsExactZero);
  CheckTrue(X[1].IsExactZero);
  CheckSolution(A, TQPVector.Create(Q(0), Q(0)), H[0]);
  CheckSolution(A, TQPVector.Create(Q(0), Q(0)), H[1]);
end;

procedure TGaussElimF128Tests.PrecisionBeyondDouble;
var A, H: TQPMatrix;
    B, X: TQPVector;
    D: TReal128;
begin
  D := Pow2(-80);
  A := TQPMatrix.Create(
    TQPVector.Create(Q(1), Q(1)),
    TQPVector.Create(Q(1), Q(1) + D)
  );
  B := TQPVector.Create(Q(2), Q(2) + D);

  CheckTrue(solver.Init(A, B));
  solver.Solve(X, H);

  CheckEquals(0, Length(H));
  CheckSolution(A, B, X);
  CheckClose(cOneF128, X[0]);
  CheckClose(cOneF128, X[1]);
end;

procedure TGaussElimF128Tests.TinyNonzeroRHS;
var H: TQPMatrix;
    X: TQPVector;
    B: TReal128;
begin
  B := Pow2(-200);

  CheckTrue(solver.Init(TQPMatrix.Create(
    TQPVector.Create(cOneF128)),
    TQPVector.Create(B)
  ));
  solver.Solve(X, H);

  CheckEquals(0, Length(H));
  CheckEquals(1, Length(X));
  CheckTrue(X[0] = B);
end;

procedure TGaussElimF128Tests.SolveLargeScale;
var A, H: TQPMatrix;
    B, X: TQPVector;
    S: TReal128;
begin
  S := Pow2(2000);
  A := TQPMatrix.Create(
    TQPVector.Create(S, Q(0)),
    TQPVector.Create(Q(0), Q(2) * S)
  );
  B := TQPVector.Create(Q(3) * S, Q(8) * S);

  CheckTrue(solver.Init(A, B));
  solver.Solve(X, H);

  CheckEquals(0, Length(H));
  CheckSolution(A, B, X);
  CheckClose(Q(3), X[0]);
  CheckClose(Q(4), X[1]);
end;

procedure TGaussElimF128Tests.SolveSmallScale;
var A, H: TQPMatrix;
    B, X: TQPVector;
    S: TReal128;
begin
  S := Pow2(-2000);
  A := TQPMatrix.Create(
    TQPVector.Create(S, Q(0)),
    TQPVector.Create(Q(0), Q(2) * S)
  );
  B := TQPVector.Create(Q(3) * S, Q(8) * S);

  CheckTrue(solver.Init(A, B));
  solver.Solve(X, H);

  CheckEquals(0, Length(H));
  CheckSolution(A, B, X);
  CheckClose(Q(3), X[0]);
  CheckClose(Q(4), X[1]);
end;

procedure TGaussElimF128Tests.InitNDStrided;
var A, H: TQPMatrix;
    X: TQPVector;
    M, V, B: INDArray<TReal128>;
begin
  M := TNDAUt.AsArray<TReal128>(Matrix);
  V := TNDAMan.TransposedView<TReal128>(M);
  B := TNDAUt.AsArray<TReal128>(TQPVector.Create(Q(1), Q(99), Q(2), Q(99)));
  CheckTrue(TNDAUt.TryAsDynArray2D<TReal128>(V, A));

  CheckTrue(solver.Init(V, B[[NDISpan(0, 3, 2)]]));
  M := nil;
  V := nil;
  B := nil;
  solver.Solve(X, H);

  CheckSolution(A, TQPVector.Create(Q(1), Q(2)), X);
end;

procedure TGaussElimF128Tests.InitWithoutCopyDynamic;
var A, H: TQPMatrix;
    X, Row: TQPVector;
begin
  A := TQPMatrix.Create(TQPVector.Create(Q(0), Q(2), Q(1)),
    TQPVector.Create(Q(3), Q(4), Q(2)));
  Row := A[0];

  CheckTrue(solver.InitWithoutCopy(A));
  A := nil;
  solver.Solve(X, H);

  CheckSolution(Matrix, TQPVector.Create(Q(1), Q(2)), X);
  CheckTrue(Row[0].IsExactZero);
  CheckTrue(Row[1] = Q(2));
end;

procedure TGaussElimF128Tests.InitWithoutCopyND;
var A, H: TQPMatrix;
    X: TQPVector;
    M, V: INDArray<TReal128>;
begin
  A := TQPMatrix.Create(
    TQPVector.Create(Q(0), Q(2), Q(1), Q(99)),
    TQPVector.Create(Q(3), Q(4), Q(2), Q(99))
  );
  M := TNDAUt.AsArray<TReal128>(A);
  V := M[[NDIAll, NDISpan(0, 2)]];

  CheckTrue(solver.InitWithoutCopy(V));
  V := nil;
  solver.Solve(X, H);

  CheckSolution(Matrix, TQPVector.Create(Q(1), Q(2)), X);
  CheckTrue(TNDAUt.TryAsDynArray2D<TReal128>(M, A));
  CheckTrue(A[0, 3] = Q(99));
  CheckTrue(A[1, 3] = Q(99));
end;

procedure TGaussElimF128Tests.RejectNDLayouts;
var M, V: INDArray<TReal128>;
begin
  M := TNDAUt.AsArray<TReal128>(Matrix);
  V := TNDAMan.TransposedView<TReal128>(M);

  CheckFalse(solver.InitWithoutCopy(M));

  V := TDynArrWrapper<TReal128>.Create(
    TQPVector.Create(Q(0), Q(2), Q(3), Q(4)), [2, 2]);
  V.UnsetFlags(NDAF_WRITEABLE);

end;

procedure TGaussElimF128Tests.EstimateToleranceWithoutInstance;
var A: TQPMatrix;
    B: TQPVector;
    tolerance: TGaussToleranceF128;
begin
  A := Matrix;
  B := TQPVector.Create(Q(1), Q(2));
  TConstructionNumericF128.ConstructionCount := 0;

  tolerance := TConstructionNumericF128.EstimateTolerance(A, B, 1);

  CheckTrue(tolerance.Pivot = Q(8) * cEpsF128);
  CheckTrue(tolerance.RHS = Q(4) * cEpsF128);
  CheckEquals(0, TConstructionNumericF128.ConstructionCount);

  tolerance := TConstructionNumericF128.EstimateTolerance(
    TNDAUt.AsArray<TReal128>(A), TNDAUt.AsArray<TReal128>(B), 1);

  CheckTrue(tolerance.Pivot = Q(8) * cEpsF128);
  CheckTrue(tolerance.RHS = Q(4) * cEpsF128);
  CheckEquals(0, TConstructionNumericF128.ConstructionCount);
end;

procedure TGaussElimF128Tests.EstimateTolerance;
var A: TQPMatrix;
    T: TGaussToleranceF128;
begin
  A := TQPMatrix.Create(
    TQPVector.Create(Q(1), Q(-3), Q(10)),
    TQPVector.Create(Q(2), Q(-6), Q(1))
  );

  T := TGaussElimF128.EstimateTolerance(A, TQPVector.Create(Q(2), Q(-30)));

  CheckTrue(T.Pivot = Q(3000) * cEpsF128);
  CheckTrue(T.RHS = Q(9000) * cEpsF128);
end;

procedure TGaussElimF128Tests.EstimateToleranceND;
var M, V, B: INDArray<TReal128>;
    T: TGaussToleranceF128;
begin
  M := TNDAUt.AsArray<TReal128>(TQPMatrix.Create(
    TQPVector.Create(Q(1), Q(99), Q(-3), Q(99), Q(10), Q(99)),
    TQPVector.Create(Q(2), Q(99), Q(-6), Q(99), Q(1), Q(99))
  ));
  V := M[[NDIAll, NDISpan(0, -1, 2)]];
  B := TNDAUt.AsArray<TReal128>(TQPVector.Create(Q(2), Q(99), Q(-30), Q(99)));

  T := TGaussElimF128.EstimateTolerance(V, B[[NDISpan(0, 3, 2)]]);

  CheckTrue(T.Pivot = Q(3000) * cEpsF128);
  CheckTrue(T.RHS = Q(9000) * cEpsF128);
end;

procedure TGaussElimF128Tests.EstimateToleranceEmpty;
var A: TQPMatrix;
    M: INDArray<TReal128>;
    T: TGaussToleranceF128;
begin
  A := nil;

  T := TGaussElimF128.EstimateTolerance(A);

  CheckTrue(T.Pivot.IsExactZero);
  CheckTrue(T.RHS.IsExactZero);

  M := nil;

  T := TGaussElimF128.EstimateTolerance(M);

  CheckTrue(T.Pivot.IsExactZero);
  CheckTrue(T.RHS.IsExactZero);
end;

procedure TGaussElimF128Tests.EstimateToleranceInvalid;
var A: TQPMatrix;
    M: INDArray<TReal128>;
    T: TGaussToleranceF128;
begin
  A := TQPMatrix.Create(TQPVector.Create(Q(1)), TQPVector.Create(Q(1), Q(2)));

  T := TGaussElimF128.EstimateTolerance(A);

  CheckTrue(T.Pivot.IsExactZero);
  CheckTrue(T.RHS.IsExactZero);

  M := TNDAUt.AsArray<TReal128>(TQPVector.Create(Q(1), Q(2)));

  T := TGaussElimF128.EstimateTolerance(M);

  CheckTrue(T.Pivot.IsExactZero);
  CheckTrue(T.RHS.IsExactZero);
end;

procedure TGaussElimF128Tests.EstimateToleranceFactor;
var T: TGaussToleranceF128;
begin
  T := TGaussElimF128.EstimateTolerance(Matrix, nil, 1);

  CheckTrue(T.Pivot = Q(8) * cEpsF128);
  CheckTrue(T.RHS.IsExactZero);

  T := TGaussElimF128.EstimateTolerance(Matrix, nil, -1);

  CheckTrue(T.Pivot = Q(800) * cEpsF128);
end;

procedure TGaussElimF128Tests.EstimateToleranceLargeScale;
var T: TGaussToleranceF128;
    S: TReal128;
begin
  S := Pow2(2000);

  T := TGaussElimF128.EstimateTolerance(TQPMatrix.Create(TQPVector.Create(S)),
    TQPVector.Create(-Q(2) * S));

  CheckTrue(T.Pivot = Q(100) * cEpsF128 * S);
  CheckTrue(T.RHS = Q(200) * cEpsF128 * S);
end;

procedure TGaussElimF128Tests.EstimateToleranceSmallScale;
var T: TGaussToleranceF128;
    S: TReal128;
begin
  S := Pow2(-2000);

  T := TGaussElimF128.EstimateTolerance(TQPMatrix.Create(TQPVector.Create(S)),
    TQPVector.Create(-Q(2) * S));

  CheckTrue(T.Pivot = Q(100) * cEpsF128 * S);
  CheckTrue(T.RHS = Q(200) * cEpsF128 * S);
end;

procedure TGaussElimF128Tests.ToleranceAppliedAtSolve;
var A, H: TQPMatrix;
    X: TQPVector;
    T: TReal128;
begin
  T := Pow2(-2000);

  solver.ZeroTolerance := T;

  CheckTrue(solver.ZeroTolerance = T);
  CheckTrue(solver.Tolerance.Pivot = T);
  CheckTrue(solver.Tolerance.RHS = T);

  T := Pow2(-80);
  A := TQPMatrix.Create(TQPVector.Create(Q(1), Q(0)),
    TQPVector.Create(Q(0), T));

  CheckTrue(solver.Init(A));
  solver.ZeroTolerance := T;
  solver.Solve(X, H);

  CheckEquals(1, Length(H));

  CheckTrue(solver.Init(A));
  solver.ZeroTolerance := 0;
  solver.Solve(X, H);

  CheckEquals(0, Length(H));
end;

procedure TGaussElimF128Tests.ToleranceFactorApplied;
var A, H: TQPMatrix;
    X: TQPVector;
begin
  A := TQPMatrix.Create(TQPVector.Create(Q(1), Q(0)),
    TQPVector.Create(Q(0), Q(8) * cEpsF128));

  CheckTrue(solver.Init(A));
  solver.Solve(X, H);

  CheckEquals(1, Length(H));

  CheckTrue(solver.Init(A));
  solver.ToleranceFactor := 1;
  solver.Solve(X, H);

  CheckEquals(0, Length(H));

  CheckTrue(solver.Init(A));
  solver.ToleranceFactor := 0;
  solver.Solve(X, H);

  CheckEquals(1, Length(H));
end;

procedure TGaussElimF128Tests.TolerancePivotOnly;
var H: TQPMatrix;
    X: TQPVector;
    T: TGaussToleranceF128;
begin
  T.Pivot := Pow2(-80);
  T.RHS := cZeroF128;
  solver.Tolerance := T;

  CheckTrue(solver.Init(Matrix, TQPVector.Create(Q(1), Q(2))));
  TGaussElimF128Access(solver).fNumeric.PrepareTolerance(TGaussElimF128Access(solver).fRows, 2, True);

  CheckTrue(TNumericF128Access(TGaussElimF128Access(solver).fNumeric).fCurTol = T.Pivot);
  CheckTrue(TNumericF128Access(TGaussElimF128Access(solver).fNumeric).fCurRHSTol = Q(400) * cEpsF128);

  solver.Solve(X, H);

  CheckSolution(Matrix, TQPVector.Create(Q(1), Q(2)), X);
end;

procedure TGaussElimF128Tests.ToleranceRHSOnly;
var T: TGaussToleranceF128;
begin
  T.Pivot := cZeroF128;
  T.RHS := Pow2(-80);
  solver.Tolerance := T;

  CheckTrue(solver.Init(Matrix, TQPVector.Create(Q(1), Q(2))));
  TGaussElimF128Access(solver).fNumeric.PrepareTolerance(TGaussElimF128Access(solver).fRows, 2, True);

  CheckTrue(TNumericF128Access(TGaussElimF128Access(solver).fNumeric).fCurTol = Q(800) * cEpsF128);
  CheckTrue(TNumericF128Access(TGaussElimF128Access(solver).fNumeric).fCurRHSTol = T.RHS);
end;

procedure TGaussElimF128Tests.ToleranceBoundary;
var A, H: TQPMatrix;
    X: TQPVector;
    T: TReal128;
begin
  T := Pow2(-2000);
  solver.ZeroTolerance := T;
  A := TQPMatrix.Create(TQPVector.Create(T));


  A := TQPMatrix.Create(TQPVector.Create(Q(0)));

  CheckTrue(solver.Init(A, TQPVector.Create(T)));
  solver.Solve(X, H);

  CheckEquals(1, Length(X), 'RHS equal to tolerance is consistent');
  CheckEquals(1, Length(H));

  CheckTrue(solver.Init(A, TQPVector.Create(Q(2) * T)));
  solver.Solve(X, H);

  CheckEquals(0, Length(X), 'RHS greater than tolerance is inconsistent');
  CheckEquals(0, Length(H));
end;

procedure TGaussElimF128Tests.ToleranceReuse;
var A, H: TQPMatrix;
    B, X: TQPVector;
    I: Integer;
begin
  A := Matrix;
  B := TQPVector.Create(Q(1), Q(2));
  solver.Tolerance := TGaussElimF128.EstimateTolerance(A, B);

  for I := 0 to 1 do begin
    CheckTrue(solver.Init(A, B));
    solver.Solve(X, H);

    CheckSolution(A, B, X);
  end;
end;

procedure TGaussElimF128Tests.ReinitializeTolerance;
var H: TQPMatrix;
    X: TQPVector;
    S: TReal128;
begin
  CheckTrue(solver.Init(TQPMatrix.Create(TQPVector.Create(Pow2(2000)))));
  solver.Solve(X, H);
  S := Pow2(-2000);

  CheckTrue(solver.Init(TQPMatrix.Create(TQPVector.Create(S)),
    TQPVector.Create(Q(3) * S)));
  solver.Solve(X, H);

  CheckEquals(0, Length(H));
  CheckEquals(1, Length(X));
  CheckClose(Q(3), X[0]);

  solver.Solve(X, H);

  CheckEquals(0, Length(X));
  CheckEquals(0, Length(H));
end;

procedure TGaussElimF128Tests.CopyInputOwnership;
var A, H: TQPMatrix;
    B, X: TQPVector;
begin
  A := Matrix;
  B := TQPVector.Create(Q(1), Q(2));

  CheckTrue(solver.Init(A, B));
  A[0, 0] := Q(99);
  B[0] := Q(99);
  A := nil;
  B := nil;
  solver.Solve(X, H);
  solver.Free;
  solver := nil;

  CheckSolution(Matrix, TQPVector.Create(Q(1), Q(2)), X);
end;

procedure TGaussElimF128Tests.EmptyDot;
var S: TReal128;
begin
  S := cOneF128;

  TGaussElimF128Access(solver).fNumeric.Dot(nil, nil, 0, S);

  CheckTrue(S.IsExactZero);
end;

procedure TGaussElimF128Tests.InPlaceVectors;
var X: TQPVector;
    S: TReal128;
begin
  X := TQPVector.Create(Q(1), Q(-2));

  TGaussElimF128Access(solver).fNumeric.Axpy(Q(2), @X[0], @X[0], Length(X));
  TGaussElimF128Access(solver).fNumeric.VecMul(@X[0], Q(-1), @X[0], Length(X));
  TGaussElimF128Access(solver).fNumeric.AbsMax(@X[0], Length(X), S);

  CheckTrue(X[0] = Q(-3));
  CheckTrue(X[1] = Q(6));
  CheckTrue(S = Q(6));
end;

function TGaussElimF128Tests.BandMatrix: TQPMatrix;
begin
  Result := TQPMatrix.Create(
    TQPVector.Create(Q(0), Q(2), Q(0), Q(0)),
    TQPVector.Create(Q(3), Q(1), Q(4), Q(0)),
    TQPVector.Create(Q(0), Q(5), Q(2), Q(3)),
    TQPVector.Create(Q(0), Q(0), Q(6), Q(4))
  );
end;

procedure TGaussElimF128Tests.BandSolve;
var A, H: TQPMatrix;
    B, X: TQPVector;
    I: Integer;
begin
  A := BandMatrix;
  B := TQPVector.Create(Q(4), Q(17), Q(28), Q(34));
  solver.LowerBandWidth := 1;
  solver.UpperBandWidth := 1;
  CheckTrue(solver.Init(A, B));

  solver.Solve(X, H);

  CheckSolution(A, B, X);
  CheckEquals(0, Length(H));
  for I := 0 to 3 do
    CheckClose(Q(I + 1), X[I]);
end;

procedure TGaussElimF128Tests.BandFallback;
var A, H: TQPMatrix;
    B, X: TQPVector;
begin
  A := TQPMatrix.Create(
    TQPVector.Create(Q(2), Q(1), Q(0), Q(0)),
    TQPVector.Create(Q(0), Q(0), Q(2), Q(0)),
    TQPVector.Create(Q(0), Q(0), Q(3), Q(1)),
    TQPVector.Create(Q(0), Q(0), Q(0), Q(4))
  );
  B := TQPVector.Create(Q(4), Q(6), Q(13), Q(16));
  solver.LowerBandWidth := 1;
  solver.UpperBandWidth := 1;
  CheckTrue(solver.Init(A, B));

  solver.Solve(X, H);

  CheckSolution(A, B, X);
  CheckEquals(1, Length(H));
  CheckSolution(A, TQPVector.Create(Q(0), Q(0), Q(0), Q(0)), H[0]);
end;

procedure TGaussElimF128Tests.BandPivotRange;
var A: TQPMatrix;
    rows: TArray<TGaussElimF128.PT>;
    I: Integer;
begin
  A := TQPMatrix.Create(
    TQPVector.Create(Q(2)),
    TQPVector.Create(Q(-2)), 
    TQPVector.Create(Q(9))
  );
  SetLength(rows, 3);
  for I := 0 to 2 do
    rows[I] := @A[I, 0];

  CheckEquals(0, Integer(TGaussElimF128Access(solver).fNumeric.ArgMax(rows, 0, 0, 1)));
  CheckEquals(1, Integer(TGaussElimF128Access(solver).fNumeric.ArgMax(rows, 1, 0, 1)));
  CheckEquals(2, Integer(TGaussElimF128Access(solver).fNumeric.ArgMax(rows, 0, 0, 2)));
end;

initialization

  RegisterTest(TGaussElimF128Tests.Suite);

end.
