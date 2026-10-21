unit panda.LinAlg.Tests.GaussRational;

interface

uses
    TestFramework
  , panda.NumsAP
  , panda.Intfs
  , panda.Arrays
  , panda.ArrManip
  , panda.LinAlg.GaussRational
  , panda.LinAlg.NumericRational
  ;

type
  TRVector = TArray<TRational>;
  TRMatrix = TArray<TRVector>;

  TGaussElimRationalTests = class(TTestCase)
  protected
    solver: TGaussElimRational;
    function Matrix: TRMatrix;
    function BandMatrix: TRMatrix;
    function Product(const A: TRMatrix; const X: TRVector): TRVector;
    procedure CheckRational(const Expected, Actual: TRational);
    procedure CheckSolution(const A: TRMatrix; const B, X: TRVector);
    procedure CheckInverse(const A, IA: TRMatrix);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure OneEquation;
    procedure UniqueFractionSolution;
    procedure OverdeterminedSolution;
    procedure UnderdeterminedSolution;
    procedure DependentNonhomogeneous;
    procedure HomogeneousSolution;
    procedure ExplicitHomogeneousRHS;
    procedure InconsistentSystem;
    procedure AllZeroSystem;
    procedure AllZeroInconsistent;
    procedure SkipZeroColumn;
    procedure TinyPivot;
    procedure LargeIntegers;
    procedure CopyInputOwnership;
    procedure RepeatedInitialization;
    procedure SolveConsumesInitialization;
    procedure InitWithoutCopyDynamic;
    procedure InitNDStrided;
    procedure InitNDHomogeneous;
    procedure InitWithoutCopyND;
    procedure RejectNDLayouts;
    procedure InvalidInitialization;
    procedure EmptyDot;
    procedure InPlaceVectors;
    procedure ExactAbsMax;
    procedure BandSolve;
    procedure BandFallback;
    procedure BandPivotRange;
  end;

implementation

type
  TGaussElimRationalAccess = class(TGaussElimRational);

function Q(aNum: Int64; aDenom: Int64 = 1): TRational;
begin
  Result := TRational.Fraction(TInteger(aNum), TInteger(aDenom));
end;

procedure TGaussElimRationalTests.SetUp;
begin
  solver := TGaussElimRational.Create;
end;

procedure TGaussElimRationalTests.TearDown;
begin
  solver.Free;
end;

function TGaussElimRationalTests.Matrix: TRMatrix;
begin
  Result := TRMatrix.Create(TRVector.Create(Q(0), Q(2)),
    TRVector.Create(Q(3), Q(4)));
end;

function TGaussElimRationalTests.Product(const A: TRMatrix;
  const X: TRVector): TRVector;
var I, J: Integer;
begin
  SetLength(Result, Length(A));
  for I := 0 to High(A) do begin
    Result[I] := Q(0);
    for J := 0 to High(X) do
      Result[I] := Result[I] + A[I, J] * X[J];
  end;
end;

procedure TGaussElimRationalTests.CheckRational(const Expected, Actual: TRational);
begin
  CheckTrue(Actual.Numerator.Count > 0, 'Numerator is initialized');
  CheckTrue(Actual.Denominator.Count > 0, 'Denominator is initialized');
  CheckFalse(Actual.Denominator.IsZero, 'Denominator is nonzero');
  CheckTrue((Expected - Actual).IsZero,
    'Expected ' + Expected.ToString + ', actual ' + Actual.ToString);
end;

procedure TGaussElimRationalTests.CheckSolution(const A: TRMatrix;
  const B, X: TRVector);
var Y: TRVector;
    I: Integer;
begin
  CheckEquals(Length(A[0]), Length(X));
  Y := Product(A, X);
  CheckEquals(Length(B), Length(Y));
  for I := 0 to High(B) do
    CheckRational(B[I], Y[I]);
end;

procedure TGaussElimRationalTests.CheckInverse(const A, IA: TRMatrix);
var I, J, K: Integer;
    S, Expected: TRational;
begin
  CheckEquals(Length(A), Length(IA));
  for I := 0 to High(A) do
    for J := 0 to High(A) do begin
      S := Q(0);
      for K := 0 to High(A) do
        S := S + A[I, K] * IA[K, J];
      Expected := Q(0);
      if I = J then Expected := Q(1);
      CheckRational(Expected, S);
    end;
end;

procedure TGaussElimRationalTests.OneEquation;
var A, H: TRMatrix;
    X: TRVector;
begin
  A := TRMatrix.Create(TRVector.Create(Q(2, 3)));

  CheckTrue(solver.Init(A, TRVector.Create(Q(5, 7))));
  solver.Solve(X, H);

  CheckEquals(1, Length(X));
  CheckRational(Q(15, 14), X[0]);
  CheckEquals(0, Length(H));
end;

procedure TGaussElimRationalTests.UniqueFractionSolution;
var A, H: TRMatrix;
    B, X: TRVector;
begin
  A := TRMatrix.Create(
    TRVector.Create(Q(1, 2), Q(1, 3)),
    TRVector.Create(Q(2, 5), Q(-3, 7))
  );
  B := Product(A, TRVector.Create(Q(2, 3), Q(-5, 4)));

  CheckTrue(solver.Init(A, B));
  solver.Solve(X, H);

  CheckSolution(A, B, X);
  CheckRational(Q(2, 3), X[0]);
  CheckRational(Q(-5, 4), X[1]);
  CheckEquals(0, Length(H));
  CheckRational(Q(1, 2), A[0, 0]);
end;

procedure TGaussElimRationalTests.OverdeterminedSolution;
var A, H: TRMatrix;
    B, X: TRVector;
begin
  A := TRMatrix.Create(
    TRVector.Create(Q(1), Q(0)),
    TRVector.Create(Q(0), Q(1)),
    TRVector.Create(Q(1), Q(1))
  );
  B := TRVector.Create(Q(1, 3), Q(2, 5), Q(11, 15));

  CheckTrue(solver.Init(A, B));
  solver.Solve(X, H);

  CheckSolution(A, B, X);
  CheckEquals(0, Length(H));
end;

procedure TGaussElimRationalTests.UnderdeterminedSolution;
var A, H: TRMatrix;
    B, X: TRVector;
    I: Integer;
begin
  A := TRMatrix.Create(TRVector.Create(Q(1, 2), Q(2, 3), Q(3, 4)));
  B := TRVector.Create(Q(5, 7));

  CheckTrue(solver.Init(A, B));
  solver.Solve(X, H);

  CheckSolution(A, B, X);
  CheckRational(Q(1), X[1]);
  CheckRational(Q(1), X[2]);
  CheckEquals(2, Length(H));
  for I := 0 to High(H) do
    CheckSolution(A, TRVector.Create(Q(0)), H[I]);
  CheckRational(Q(1), H[0, 1]);
  CheckRational(Q(0), H[0, 2]);
  CheckRational(Q(0), H[1, 1]);
  CheckRational(Q(1), H[1, 2]);
end;

procedure TGaussElimRationalTests.DependentNonhomogeneous;
var A, H: TRMatrix;
    B, X: TRVector;
begin
  A := TRMatrix.Create(
    TRVector.Create(Q(1, 3), Q(2, 7)),
    TRVector.Create(Q(2, 3), Q(4, 7))
  );
  B := TRVector.Create(Q(1, 5), Q(2, 5));

  CheckTrue(solver.Init(A, B));
  solver.Solve(X, H);

  CheckSolution(A, B, X);
  CheckEquals(1, Length(H));
  CheckSolution(A, TRVector.Create(Q(0), Q(0)), H[0]);
end;

procedure TGaussElimRationalTests.HomogeneousSolution;
var A, H: TRMatrix;
    X: TRVector;
begin
  A := TRMatrix.Create(
    TRVector.Create(Q(1, 3), Q(2, 7)),
    TRVector.Create(Q(2, 3), Q(4, 7))
  );

  CheckTrue(solver.Init(A));
  solver.Solve(X, H);

  CheckEquals(2, Length(X));
  CheckRational(Q(0), X[0]);
  CheckRational(Q(0), X[1]);
  CheckEquals(1, Length(H));
  CheckSolution(A, TRVector.Create(Q(0), Q(0)), H[0]);
end;

procedure TGaussElimRationalTests.ExplicitHomogeneousRHS;
var H: TRMatrix;
    X: TRVector;
begin
  CheckTrue(solver.Init(Matrix, TRVector.Create(Q(0), Q(0))));
  solver.Solve(X, H);

  CheckEquals(2, Length(X));
  CheckRational(Q(0), X[0]);
  CheckRational(Q(0), X[1]);
  CheckEquals(0, Length(H));
end;

procedure TGaussElimRationalTests.InconsistentSystem;
var H: TRMatrix;
    X: TRVector;
begin
  CheckTrue(solver.Init(TRMatrix.Create(
    TRVector.Create(Q(1, 3)),
    TRVector.Create(Q(2, 3))),
    TRVector.Create(Q(1, 5), Q(3, 5))
  ));
  solver.Solve(X, H);

  CheckEquals(0, Length(X));
  CheckEquals(0, Length(H));
end;

procedure TGaussElimRationalTests.AllZeroSystem;
var A, H: TRMatrix;
    X: TRVector;
begin
  A := TRMatrix.Create(TRVector.Create(Q(0), Q(0), Q(0)));

  CheckTrue(solver.Init(A));
  solver.Solve(X, H);

  CheckEquals(3, Length(H));
  CheckSolution(A, TRVector.Create(Q(0)), X);
  CheckRational(Q(1), H[0, 0]);
  CheckRational(Q(0), H[0, 1]);
  CheckRational(Q(0), H[0, 2]);
  CheckRational(Q(1), H[1, 1]);
  CheckRational(Q(1), H[2, 2]);
end;

procedure TGaussElimRationalTests.AllZeroInconsistent;
var H: TRMatrix;
    X: TRVector;
begin
  CheckTrue(solver.Init(TRMatrix.Create(
    TRVector.Create(Q(0), Q(0))),
    TRVector.Create(TRational.Fraction(1, TInteger(1) shl 1100))
  ));
  solver.Solve(X, H);

  CheckEquals(0, Length(X));
  CheckEquals(0, Length(H));
end;

procedure TGaussElimRationalTests.SkipZeroColumn;
var A, H: TRMatrix;
    B, X: TRVector;
begin
  A := TRMatrix.Create(
    TRVector.Create(Q(0), Q(2), Q(1)),
    TRVector.Create(Q(0), Q(0), Q(3))
  );
  B := TRVector.Create(Q(7), Q(9));

  CheckTrue(solver.Init(A, B));
  solver.Solve(X, H);

  CheckSolution(A, B, X);
  CheckEquals(1, Length(H));
  CheckSolution(A, TRVector.Create(Q(0), Q(0)), H[0]);
  CheckRational(Q(1), H[0, 0]);
end;

procedure TGaussElimRationalTests.TinyPivot;
var A, H: TRMatrix;
    X: TRVector;
    T: TRational;
begin
  T := TRational.Fraction(TInteger(1), TInteger(1) shl 1100);
  A := TRMatrix.Create(
    TRVector.Create(T, Q(0)),
    TRVector.Create(Q(0), Q(1))
  );

  CheckTrue(solver.Init(A, TRVector.Create(T * Q(3, 7), Q(2, 5))));
  solver.Solve(X, H);

  CheckRational(Q(3, 7), X[0]);
  CheckRational(Q(2, 5), X[1]);
  CheckEquals(0, Length(H));

end;

procedure TGaussElimRationalTests.LargeIntegers;
var A, H: TRMatrix;
    B, X: TRVector;
    N: TInteger;
    R: TRational;
begin
  N := (TInteger(1) shl 200) + 1;
  R := N;
  A := TRMatrix.Create(
    TRVector.Create(R, Q(1)),
    TRVector.Create(R + Q(1), Q(1))
  );
  B := Product(A, TRVector.Create(Q(2, 3), Q(-7, 11)));

  CheckTrue(solver.Init(A, B));
  solver.Solve(X, H);

  CheckRational(Q(2, 3), X[0]);
  CheckRational(Q(-7, 11), X[1]);
  CheckSolution(A, B, X);
end;

procedure TGaussElimRationalTests.CopyInputOwnership;
var A, H: TRMatrix;
    B, X: TRVector;
begin
  A := Matrix;
  B := TRVector.Create(Q(1), Q(2));

  CheckTrue(solver.Init(A, B));
  A := nil;
  B := nil;
  solver.Solve(X, H);
  solver.Free;
  solver := nil;

  CheckSolution(Matrix, TRVector.Create(Q(1), Q(2)), X);
end;

procedure TGaussElimRationalTests.RepeatedInitialization;
var H: TRMatrix;
    X, Y: TRVector;
begin
  CheckTrue(solver.Init(Matrix, TRVector.Create(Q(1), Q(2))));
  solver.Solve(X, H);

  CheckTrue(solver.Init(Matrix));
  solver.Solve(Y, H);

  CheckSolution(Matrix, TRVector.Create(Q(1), Q(2)), X);
  CheckRational(Q(0), Y[0]);
  CheckRational(Q(0), Y[1]);
end;

procedure TGaussElimRationalTests.SolveConsumesInitialization;
var H: TRMatrix;
    X: TRVector;
begin
  CheckTrue(solver.Init(Matrix));
  solver.Solve(X, H);
  solver.Solve(X, H);

  CheckEquals(0, Length(X));
  CheckEquals(0, Length(H));
end;

procedure TGaussElimRationalTests.InitWithoutCopyDynamic;
var A, H: TRMatrix;
    X, Row: TRVector;
begin
  A := TRMatrix.Create(
    TRVector.Create(Q(1), Q(2), Q(1)),
    TRVector.Create(Q(3), Q(4), Q(2))
  );
  Row := A[1];

  CheckTrue(solver.InitWithoutCopy(A));
  A := nil;
  solver.Solve(X, H);

  CheckSolution(TRMatrix.Create(
    TRVector.Create(Q(1), Q(2)),
    TRVector.Create(Q(3), Q(4))),
    TRVector.Create(Q(1), Q(2)), X);
  CheckRational(Q(0), Row[0]);
  CheckRational(Q(-2), Row[1]);
  CheckRational(Q(-1), Row[2]);
end;

procedure TGaussElimRationalTests.InitNDStrided;
var A, H: TRMatrix;
    X: TRVector;
    M, V, B: INDArray<TRational>;
begin
  M := TNDAUt.AsArray<TRational>(Matrix);
  V := TNDAMan.TransposedView<TRational>(M);
  B := TNDAUt.AsArray<TRational>(TRVector.Create(Q(1), Q(99), Q(2), Q(99)));
  CheckTrue(TNDAUt.TryAsDynArray2D<TRational>(V, A));

  CheckTrue(solver.Init(V, B[[NDISpan(0, 3, 2)]]));
  M := nil;
  V := nil;
  B := nil;
  solver.Solve(X, H);

  CheckSolution(A, TRVector.Create(Q(1), Q(2)), X);
end;

procedure TGaussElimRationalTests.InitNDHomogeneous;
var H: TRMatrix;
    X: TRVector;
begin
  CheckTrue(solver.Init(TNDAUt.AsArray<TRational>(Matrix)));
  solver.Solve(X, H);

  CheckRational(Q(0), X[0]);
  CheckRational(Q(0), X[1]);
  CheckEquals(0, Length(H));
end;

procedure TGaussElimRationalTests.InitWithoutCopyND;
var A, H: TRMatrix;
    X: TRVector;
    M, V: INDArray<TRational>;
begin
  A := TRMatrix.Create(
    TRVector.Create(Q(0), Q(2), Q(1), Q(99)),
    TRVector.Create(Q(3), Q(4), Q(2), Q(99))
  );
  M := TNDAUt.AsArray<TRational>(A);
  V := M[[NDIAll, NDISpan(0, 2)]];

  CheckTrue(solver.InitWithoutCopy(V));
  V := nil;
  solver.Solve(X, H);

  CheckSolution(Matrix, TRVector.Create(Q(1), Q(2)), X);
  CheckTrue(TNDAUt.TryAsDynArray2D<TRational>(M, A));
  CheckRational(Q(99), A[0, 3]);
  CheckRational(Q(99), A[1, 3]);
  CheckRational(Q(0), A[0, 0]);
end;

procedure TGaussElimRationalTests.RejectNDLayouts;
var M, V: INDArray<TRational>;
begin
  M := TNDAUt.AsArray<TRational>(Matrix);
  V := TNDAMan.TransposedView<TRational>(M);

  CheckFalse(solver.InitWithoutCopy(M), 'Require an augmented matrix');

  V := TDynArrWrapper<TRational>.Create(
    TRVector.Create(Q(0), Q(2), Q(3), Q(4)), [2, 2]);
  V.UnsetFlags(NDAF_WRITEABLE);

end;

procedure TGaussElimRationalTests.InvalidInitialization;
var A, H: TRMatrix;
    X: TRVector;
    M: INDArray<TRational>;
begin
  A := nil;

  CheckFalse(solver.Init(A));

  A := TRMatrix.Create(TRVector.Create(Q(1)), TRVector.Create(Q(1), Q(2)));

  CheckFalse(solver.Init(A));
  CheckFalse(solver.Init(Matrix, TRVector.Create(Q(1))));

  M := TNDAUt.AsArray<TRational>(TRVector.Create(Q(1), Q(2)));

  CheckFalse(solver.Init(M));
  solver.Solve(X, H);

  CheckEquals(0, Length(X));
  CheckEquals(0, Length(H));
end;

procedure TGaussElimRationalTests.EmptyDot;
var R: TRational;
begin
  TGaussElimRationalAccess(solver).fNumeric.Dot(nil, nil, 0, R);

  CheckRational(Q(0), R);
end;

procedure TGaussElimRationalTests.InPlaceVectors;
var V: TRVector;
begin
  V := TRVector.Create(Q(1, 3), Q(-2, 5));

  TGaussElimRationalAccess(solver).fNumeric.Axpy(Q(2), @V[0], @V[0], Length(V));

  CheckRational(Q(1), V[0]);
  CheckRational(Q(-6, 5), V[1]);

  TGaussElimRationalAccess(solver).fNumeric.VecMul(@V[0], Q(1, 2), @V[0], Length(V));

  CheckRational(Q(1, 2), V[0]);
  CheckRational(Q(-3, 5), V[1]);
end;

procedure TGaussElimRationalTests.ExactAbsMax;
var V: TRVector;
    R: TRational;
begin
  V := TRVector.Create(Q(-7, 3), Q(2), Q(0));

  TGaussElimRationalAccess(solver).fNumeric.AbsMax(@V[0], Length(V), R);

  CheckRational(Q(7, 3), R);

  TGaussElimRationalAccess(solver).fNumeric.AbsMax(nil, 0, R);

  CheckRational(Q(0), R);
end;

function TGaussElimRationalTests.BandMatrix: TRMatrix;
begin
  Result := TRMatrix.Create(
    TRVector.Create(Q(0), Q(2), Q(0), Q(0)),
    TRVector.Create(Q(3), Q(1), Q(4), Q(0)),
    TRVector.Create(Q(0), Q(5), Q(2), Q(3)),
    TRVector.Create(Q(0), Q(0), Q(6), Q(4))
  );
end;

procedure TGaussElimRationalTests.BandSolve;
var A, H: TRMatrix;
    B, X, expected: TRVector;
    I: Integer;
begin
  A := BandMatrix;
  expected := TRVector.Create(Q(1, 2), Q(-2, 3), Q(3, 4), Q(-4, 5));
  B := Product(A, expected);
  solver.LowerBandWidth := 1;
  solver.UpperBandWidth := 1;
  CheckTrue(solver.Init(A, B));

  solver.Solve(X, H);

  CheckSolution(A, B, X);
  CheckEquals(0, Length(H));
  for I := 0 to 3 do
    CheckRational(expected[I], X[I]);
end;

procedure TGaussElimRationalTests.BandFallback;
var A, H: TRMatrix;
    B, X: TRVector;
begin
  A := TRMatrix.Create(
    TRVector.Create(Q(2), Q(1), Q(0), Q(0)),
    TRVector.Create(Q(0), Q(0), Q(2), Q(0)),
    TRVector.Create(Q(0), Q(0), Q(3), Q(1)),
    TRVector.Create(Q(0), Q(0), Q(0), Q(4))
  );
  B := TRVector.Create(Q(4), Q(6), Q(13), Q(16));
  solver.LowerBandWidth := 1;
  solver.UpperBandWidth := 1;
  CheckTrue(solver.Init(A, B));

  solver.Solve(X, H);

  CheckSolution(A, B, X);
  CheckEquals(1, Length(H));
  CheckSolution(A, TRVector.Create(Q(0), Q(0), Q(0), Q(0)), H[0]);
end;

procedure TGaussElimRationalTests.BandPivotRange;
var A: TRMatrix;
    rows: TArray<TGaussElimRational.PT>;
    I: Integer;
begin
  A := TRMatrix.Create(
    TRVector.Create(Q(0)),
    TRVector.Create(Q(0)), 
    TRVector.Create(Q(1))
  );
  SetLength(rows, 3);
  for I := 0 to 2 do
    rows[I] := @A[I, 0];

  CheckEquals(0, Integer(TGaussElimRationalAccess(solver).fNumeric.ArgMax(rows, 0, 0, 1)));
  CheckEquals(1, Integer(TGaussElimRationalAccess(solver).fNumeric.ArgMax(rows, 1, 0, 1)));
  CheckEquals(2, Integer(TGaussElimRationalAccess(solver).fNumeric.ArgMax(rows, 0, 0, 2)));
end;

initialization

  RegisterTest(TGaussElimRationalTests.Suite);

end.
