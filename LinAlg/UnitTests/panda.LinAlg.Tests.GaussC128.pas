unit panda.LinAlg.Tests.GaussC128;
interface

uses
    TestFramework
  , panda.Nums
  , panda.Intfs
  , panda.Arrays
  , panda.ArrManip
  , panda.LinAlg.Gauss
  , panda.LinAlg.GaussC128
  , panda.LinAlg.NumericC128
  , panda.Tests.NDATestCase
  ;

type
  TCVector = TArray<TCmplx128>;
  TCMatrix = TArray<TCVector>;
  TGaussElimC128Tests = class(TNDATestCase)
  protected
    solver: TGaussElimC128;
    function Matrix: TCMatrix;
    function BandMatrix: TCMatrix;
    function Product(const A: TCMatrix; const X: TCVector): TCVector;
    procedure CheckSolution(const A: TCMatrix; const B, X: TCVector);
    procedure CheckInverse(const A, IA: TCMatrix);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure OneEquation;
    procedure UniqueSolution;
    procedure RectangularSolution;
    procedure HomogeneousSolution;
    procedure InconsistentSystem;
    procedure InitNDStrided;
    procedure InitWithoutCopyND;
    procedure RejectNDLayouts;
    procedure EstimateTolerance;
    procedure EstimateToleranceND;
    procedure EstimateInvalidInputs;
    procedure ToleranceControls;
    procedure ToleranceAppliedAtSolve;
    procedure ModulusZeroTest;
    procedure SeparateRHSTolerance;
    procedure InitWithoutCopy;
    procedure RankDeficientNonhomogeneous;
    procedure LargeRHSAutoTolerance;
    procedure IndependentToleranceFields;
    procedure EstimateStridedRHS;
    procedure BandSolve;
    procedure BandFallback;
    procedure BandPivotRange;

  end;

implementation

type
  TGaussElimC128Access = class(TGaussElimC128);

procedure TGaussElimC128Tests.SetUp;
begin
  solver := TGaussElimC128.Create;
end;

procedure TGaussElimC128Tests.TearDown;
begin
  solver.Free;
end;

function TGaussElimC128Tests.Matrix: TCMatrix;
begin
  Result := TCMatrix.Create(TCVector.Create(Cmplx(1, 1), Cmplx(2, -1)),
    TCVector.Create(Cmplx(0, 3), Cmplx(-1, 2)));
end;

function TGaussElimC128Tests.Product(const A: TCMatrix; const X: TCVector): TCVector;
var I, J: Integer;
begin
  SetLength(Result, Length(A));
  for I := 0 to High(A) do begin
    Result[I] := TCmplx128.Zero;
    for J := 0 to High(X) do
      Result[I] := Result[I] + A[I, J] * X[J];
  end;
end;

procedure TGaussElimC128Tests.CheckSolution(const A: TCMatrix; const B, X: TCVector);
var Y: TCVector;
    I: Integer;
begin
  CheckEquals(Length(A[0]), Length(X));
  Y := Product(A, X);
  CheckEquals(Length(B), Length(Y));
  for I := 0 to High(B) do
    CheckEquals(B[I], Y[I], 1e-12);
end;

procedure TGaussElimC128Tests.CheckInverse(const A, IA: TCMatrix);
var I, J, K: Integer;
    sum, expected: TCmplx128;
begin
  CheckEquals(Length(A), Length(IA));
  for I := 0 to High(A) do
    for J := 0 to High(A) do begin
      sum := TCmplx128.Zero;
      for K := 0 to High(A) do
        sum := sum + A[I, K] * IA[K, J];
      expected := TCmplx128.Zero;
      if I = J then expected := TCmplx128.ReUnit;
      CheckEquals(expected, sum, 1e-12);
    end;
end;

procedure TGaussElimC128Tests.OneEquation;
var A, H: TCMatrix;
    B, X: TCVector;
begin
  A := TCMatrix.Create(TCVector.Create(Cmplx(0, 2)));
  B := TCVector.Create(Cmplx(4, 6));

  CheckTrue(solver.Init(A, B));
  solver.Solve(X, H);

  CheckSolution(A, B, X);
  CheckEquals(Cmplx(3, -2), X[0], 1e-12);
  CheckEquals(0, Length(H));

end;

procedure TGaussElimC128Tests.UniqueSolution;
var A, H: TCMatrix;
    B, X: TCVector;
begin
  A := Matrix;
  B := Product(A, TCVector.Create(Cmplx(2, -1), Cmplx(-3, 2)));

  CheckTrue(solver.Init(A, B));
  solver.Solve(X, H);

  CheckSolution(A, B, X);
  CheckEquals(0, Length(H));
  CheckEquals(Cmplx(1, 1), A[0, 0], 0);
end;

procedure TGaussElimC128Tests.RectangularSolution;
var A, H: TCMatrix;
    B, X: TCVector;
begin
  A := TCMatrix.Create(TCVector.Create(Cmplx(1, 1), Cmplx(2, -1), Cmplx(0, 3)));
  B := TCVector.Create(Cmplx(4, -2));

  CheckTrue(solver.Init(A, B));
  solver.Solve(X, H);

  CheckSolution(A, B, X);
  CheckEquals(2, Length(H));
  CheckSolution(A, TCVector.Create(TCmplx128.Zero), H[0]);
  CheckSolution(A, TCVector.Create(TCmplx128.Zero), H[1]);
end;

procedure TGaussElimC128Tests.HomogeneousSolution;
var A, H: TCMatrix;
    X: TCVector;
begin
  A := TCMatrix.Create(
    TCVector.Create(Cmplx(1, 1), Cmplx(2, -1)),
    TCVector.Create(Cmplx(2, 2), Cmplx(4, -2))
  );

  CheckTrue(solver.Init(A));
  solver.Solve(X, H);

  CheckEquals(1, Length(H));
  CheckSolution(A, TCVector.Create(TCmplx128.Zero, TCmplx128.Zero), H[0]);
end;

procedure TGaussElimC128Tests.InconsistentSystem;
var A, H: TCMatrix;
    X: TCVector;
begin
  A := TCMatrix.Create(
    TCVector.Create(Cmplx(0, 1)),
    TCVector.Create(Cmplx(0, 2))
  );

  CheckTrue(solver.Init(A, TCVector.Create(Cmplx(1, 0), Cmplx(2, 1))));
  solver.Solve(X, H);

  CheckEquals(0, Length(X));
  CheckEquals(0, Length(H));
end;

procedure TGaussElimC128Tests.InitNDStrided;
var A, H: TCMatrix;
    B, X: TCVector;
    M, V: INDArray<TCmplx128>;
begin
  A := Matrix;
  B := TCVector.Create(Cmplx(2, 1), Cmplx(3, -1));
  M := TNDAUt.AsArray<TCmplx128>(A);
  V := TNDAMan.TransposedView<TCmplx128>(M);

  CheckTrue(solver.Init(V, TNDAUt.AsArray<TCmplx128>(B)));
  solver.Solve(X, H);

  CheckTrue(TNDAUt.TryAsDynArray2D<TCmplx128>(V, A));
  CheckSolution(A, B, X);
  CheckEquals(Cmplx(1, 1), A[0, 0], 0);
end;

procedure TGaussElimC128Tests.InitWithoutCopyND;
var A, H: TCMatrix;
    X: TCVector;
    M, V: INDArray<TCmplx128>;
begin
  A := TCMatrix.Create(
    TCVector.Create(Cmplx(0, 1), Cmplx(2, 0), Cmplx(3, 1), Cmplx(99, 1)),
    TCVector.Create(Cmplx(3, 0), Cmplx(0, 1), Cmplx(2, -1), Cmplx(99, 1))
  );
  M := TNDAUt.AsArray<TCmplx128>(A);
  V := M[[NDIAll, NDISpan(0, 2)]];

  CheckTrue(solver.InitWithoutCopy(V));
  solver.Solve(X, H);

  CheckSolution(TCMatrix.Create(
    TCVector.Create(Cmplx(0, 1), Cmplx(2, 0)),
    TCVector.Create(Cmplx(3, 0), Cmplx(0, 1))),
    TCVector.Create(Cmplx(3, 1), Cmplx(2, -1)), X);
  CheckTrue(TNDAUt.TryAsDynArray2D<TCmplx128>(M, A));
  CheckEquals(Cmplx(99, 1), A[0, 3], 0);
  CheckEquals(Cmplx(99, 1), A[1, 3], 0);
end;

procedure TGaussElimC128Tests.RejectNDLayouts;
var M, V: INDArray<TCmplx128>;
begin
  M := TNDAUt.AsArray<TCmplx128>(Matrix);
  V := TNDAMan.TransposedView<TCmplx128>(M);

  CheckFalse(solver.InitWithoutCopy(M));
end;

procedure TGaussElimC128Tests.EstimateTolerance;
var A: TCMatrix;
    T: TGaussTolerance;
begin
  A := TCMatrix.Create(TCVector.Create(Cmplx(3, 4), Cmplx(0, 2)));

  T := TGaussElimC128.EstimateTolerance(A, TCVector.Create(Cmplx(0, -10)), 1);

  CheckEquals(2 * cEpsF64 * 5, T.Pivot, 1e-25);
  CheckEquals(2 * cEpsF64 * 10, T.RHS, 1e-25);

  T := TGaussElimC128.EstimateTolerance(A, nil, 0);

  CheckEquals(100 * 2 * cEpsF64 * 5, T.Pivot, 1e-25);
  CheckEquals(0, T.RHS, 0);
end;

procedure TGaussElimC128Tests.EstimateToleranceND;
var A, B: INDArray<TCmplx128>;
    T: TGaussTolerance;
begin
  A := TNDAUt.AsArray<TCmplx128>(TCMatrix.Create(
    TCVector.Create(Cmplx(3, 4)), TCVector.Create(Cmplx(0, 2))));
  A := TNDAMan.TransposedView<TCmplx128>(A);
  B := TNDAUt.AsArray<TCmplx128>(TCVector.Create(Cmplx(0, -10)));

  T := TGaussElimC128.EstimateTolerance(A, B, 1);

  CheckEquals(2 * cEpsF64 * 5, T.Pivot, 1e-25);
  CheckEquals(2 * cEpsF64 * 10, T.RHS, 1e-25);
end;

procedure TGaussElimC128Tests.EstimateInvalidInputs;
var A: TCMatrix;
    T: TGaussTolerance;
    M: INDArray<TCmplx128>;
begin
  A := TCMatrix.Create(TCVector.Create(Cmplx(1, 2)), nil);

  T := TGaussElimC128.EstimateTolerance(A);

  CheckEquals(0, T.Pivot, 0);
  CheckEquals(0, T.RHS, 0);

  M := nil;
  T := TGaussElimC128.EstimateTolerance(M);

  CheckEquals(0, T.Pivot, 0);
  CheckEquals(0, T.RHS, 0);
end;

procedure TGaussElimC128Tests.ToleranceControls;
var A, H: TCMatrix;
    B, X: TCVector;
begin
  CheckEquals(100, solver.ToleranceFactor, 0);
  solver.ZeroTolerance := 0.25;

  CheckEquals(0.25, solver.Tolerance.Pivot, 0);
  CheckEquals(0.25, solver.Tolerance.RHS, 0);

  A := Matrix;
  B := TCVector.Create(Cmplx(1, 2), Cmplx(3, 4));
  solver.Tolerance := TGaussElimC128.EstimateTolerance(A, B);
  solver.ToleranceFactor := 1e20;

  CheckTrue(solver.Init(A, B));
  solver.Solve(X, H);

  CheckSolution(A, B, X);
end;

procedure TGaussElimC128Tests.ToleranceAppliedAtSolve;
var A, H: TCMatrix;
    X: TCVector;
begin
  A := TCMatrix.Create(TCVector.Create(Cmplx(0, 1)));
  CheckTrue(solver.Init(A));
  solver.ZeroTolerance := 2;

  solver.Solve(X, H);

  CheckEquals(1, Length(H));

  solver.ZeroTolerance := 0;
  solver.ToleranceFactor := 1e20;
  CheckTrue(solver.Init(A));
  solver.Solve(X, H);

  CheckEquals(1, Length(H));

  solver.ToleranceFactor := 0;
  CheckTrue(solver.Init(A));
  solver.Solve(X, H);

  CheckEquals(0, Length(H));
end;

procedure TGaussElimC128Tests.ModulusZeroTest;
var A, H: TCMatrix;
    X: TCVector;
begin
  A := TCMatrix.Create(TCVector.Create(Cmplx(0.8, 0.8)));
  solver.ZeroTolerance := 1;

  CheckTrue(solver.Init(A));
  solver.Solve(X, H);

  CheckEquals(0, Length(H));
end;

procedure TGaussElimC128Tests.SeparateRHSTolerance;
var A, H: TCMatrix;
    X: TCVector;
    T: TGaussTolerance;
begin
  A := TCMatrix.Create(TCVector.Create(Cmplx(1, 0)), TCVector.Create(Cmplx(0, 0)));
  T.Pivot := 1e-12;
  T.RHS := 1e-3;
  solver.Tolerance := T;

  CheckTrue(solver.Init(A, TCVector.Create(Cmplx(0, 1e10), Cmplx(0, 1e-4))));
  solver.Solve(X, H);

  CheckEquals(1, Length(X));
  CheckEquals(Cmplx(0, 1e10), X[0], 1e-12);

  T.RHS := 1e-5;
  solver.Tolerance := T;
  CheckTrue(solver.Init(A, TCVector.Create(Cmplx(0, 1e10), Cmplx(0, 1e-4))));
  solver.Solve(X, H);

  CheckEquals(0, Length(X));
end;

procedure TGaussElimC128Tests.InitWithoutCopy;
var A, Original, H: TCMatrix;
    X: TCVector;
    PRow: Pointer;
begin
  A := TCMatrix.Create(
    TCVector.Create(Cmplx(0, 1), Cmplx(2, 0), Cmplx(3, 1)),
    TCVector.Create(Cmplx(3, 0), Cmplx(0, 1), Cmplx(2, -1))
  );
  Original := TCMatrix.Create(
    TCVector.Create(Cmplx(0, 1), Cmplx(2, 0)),
    TCVector.Create(Cmplx(3, 0), Cmplx(0, 1))
  );
  PRow := @A[0, 0];

  CheckTrue(solver.InitWithoutCopy(A));
  solver.Solve(X, H);

  CheckSolution(Original, TCVector.Create(Cmplx(3, 1), Cmplx(2, -1)), X);
  CheckTrue(PRow = @A[0, 0]);
  CheckEquals(TCmplx128.Zero, A[0, 0], 0);
end;

procedure TGaussElimC128Tests.RankDeficientNonhomogeneous;
var A, H: TCMatrix;
    B, X: TCVector;
begin
  A := TCMatrix.Create(
    TCVector.Create(Cmplx(0.1, 0.2), Cmplx(0.3, -0.4)),
    TCVector.Create(Cmplx(0.3, 0.6), Cmplx(0.9, -1.2))
  );
  B := Product(A, TCVector.Create(Cmplx(1, 2), Cmplx(3, -1)));

  CheckTrue(solver.Init(A, B));
  solver.Solve(X, H);

  CheckEquals(1, Length(H));
  CheckSolution(A, B, X);
  CheckSolution(A, TCVector.Create(TCmplx128.Zero, TCmplx128.Zero), H[0]);
end;

procedure TGaussElimC128Tests.LargeRHSAutoTolerance;
var A, H: TCMatrix;
    B, X: TCVector;
begin
  A := TCMatrix.Create(
    TCVector.Create(Cmplx(0, 1), Cmplx(0, 0)),
    TCVector.Create(Cmplx(0, 0), Cmplx(1, 0))
  );
  B := TCVector.Create(Cmplx(1e10, 0), Cmplx(0, 1e10));

  CheckTrue(solver.Init(A, B));
  solver.Solve(X, H);

  CheckSolution(A, B, X);
  CheckEquals(0, Length(H));
end;

procedure TGaussElimC128Tests.IndependentToleranceFields;
var A, H: TCMatrix;
    X: TCVector;
    T: TGaussTolerance;
begin
  A := TCMatrix.Create(TCVector.Create(Cmplx(0, 1)), TCVector.Create(Cmplx(0, 0)));
  T.Pivot := 1e-12;
  T.RHS := 0;
  solver.Tolerance := T;

  CheckTrue(solver.Init(A, TCVector.Create(Cmplx(0, 1e10), Cmplx(0, 1e-4))));
  solver.Solve(X, H);

  CheckEquals(1, Length(X));

  A := TCMatrix.Create(TCVector.Create(Cmplx(0, 1)));
  T.Pivot := 0;
  T.RHS := 2;
  solver.Tolerance := T;

  CheckTrue(solver.Init(A));
  solver.Solve(X, H);

  CheckEquals(0, Length(H));

end;

procedure TGaussElimC128Tests.EstimateStridedRHS;
var A, B: INDArray<TCmplx128>;
    T: TGaussTolerance;
begin
  A := TNDAUt.AsArray<TCmplx128>(Matrix);
  B := TNDAUt.AsArray<TCmplx128>(TCVector.Create(Cmplx(3, 4), Cmplx(99, 0), Cmplx(0, 2)));
  B := B[[NDISpan(0, -1, 2)]];

  T := TGaussElimC128.EstimateTolerance(A, B, 1);

  CheckEquals(2 * cEpsF64 * 5, T.RHS, 1e-25);
end;

function TGaussElimC128Tests.BandMatrix: TCMatrix;
begin
  Result := TCMatrix.Create(
    TCVector.Create(Cmplx(0, 0), Cmplx(2, 1), Cmplx(0, 0), Cmplx(0, 0)),
    TCVector.Create(Cmplx(3, -1), Cmplx(1, 2), Cmplx(4, 1), Cmplx(0, 0)),
    TCVector.Create(Cmplx(0, 0), Cmplx(5, 2), Cmplx(2, -1), Cmplx(3, 1)),
    TCVector.Create(Cmplx(0, 0), Cmplx(0, 0), Cmplx(6, 1), Cmplx(4, -2))
  );
end;

procedure TGaussElimC128Tests.BandSolve;
var A, H: TCMatrix;
    B, X, expected: TCVector;
    I: Integer;
begin
  A := BandMatrix;
  expected := TCVector.Create(Cmplx(1, 1), Cmplx(2, -1), Cmplx(3, 2), Cmplx(4, -2));
  B := Product(A, expected);
  solver.LowerBandWidth := 1;
  solver.UpperBandWidth := 1;
  CheckTrue(solver.Init(A, B));

  solver.Solve(X, H);

  CheckSolution(A, B, X);
  CheckEquals(0, Length(H));
  for I := 0 to 3 do
    CheckEquals(expected[I], X[I], 1e-12);
end;

procedure TGaussElimC128Tests.BandFallback;
var A, H: TCMatrix;
    B, X: TCVector;
begin
  A := TCMatrix.Create(
    TCVector.Create(Cmplx(2, 0), Cmplx(1, 0), Cmplx(0, 0), Cmplx(0, 0)),
    TCVector.Create(Cmplx(0, 0), Cmplx(0, 0), Cmplx(2, 0), Cmplx(0, 0)),
    TCVector.Create(Cmplx(0, 0), Cmplx(0, 0), Cmplx(3, 0), Cmplx(1, 0)),
    TCVector.Create(Cmplx(0, 0), Cmplx(0, 0), Cmplx(0, 0), Cmplx(4, 0))
  );
  B := TCVector.Create(Cmplx(4, 0), Cmplx(6, 0), Cmplx(13, 0), Cmplx(16, 0));
  solver.LowerBandWidth := 1;
  solver.UpperBandWidth := 1;
  CheckTrue(solver.Init(A, B));

  solver.Solve(X, H);

  CheckSolution(A, B, X);
  CheckEquals(1, Length(H));
  CheckSolution(A, TCVector.Create(Cmplx(0, 0), Cmplx(0, 0), Cmplx(0, 0), Cmplx(0, 0)), H[0]);
end;

procedure TGaussElimC128Tests.BandPivotRange;
var A: TCMatrix;
    rows: TArray<TGaussElimC128.PT>;
    I: Integer;
begin
  A := TCMatrix.Create(
    TCVector.Create(Cmplx(3, 4)),
    TCVector.Create(Cmplx(0, -5)), 
    TCVector.Create(Cmplx(10, 0))
  );
  SetLength(rows, 3);
  for I := 0 to 2 do
    rows[I] := @A[I, 0];

  CheckEquals(0, Integer(TGaussElimC128Access(solver).fNumeric.ArgMax(rows, 0, 0, 1)));
  CheckEquals(1, Integer(TGaussElimC128Access(solver).fNumeric.ArgMax(rows, 1, 0, 1)));
  CheckEquals(2, Integer(TGaussElimC128Access(solver).fNumeric.ArgMax(rows, 0, 0, 2)));
end;

initialization

  RegisterTest(TGaussElimC128Tests.Suite);

end.
