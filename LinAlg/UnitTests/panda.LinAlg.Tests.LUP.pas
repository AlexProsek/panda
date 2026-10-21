unit panda.LinAlg.Tests.LUP;

interface

uses
    TestFramework
  , panda.Intfs
  , panda.Arrays
  , panda.LinAlg.LUP
  , panda.LinAlg.LUPF64
  , panda.LinAlg.Base
  , panda.LinAlg.NumericF64
  ;

type
  TLUPPointerTests = class(TTestCase)
  protected
    function Matrix5: TArray<TArray<Double>>;
    procedure CheckFactors(const A, LU: TArray<TArray<Double>>;
      const P: TArray<NativeInt>);
    procedure CheckBlockSize(aSize: NativeInt);
  published
    procedure Defaults;
    procedure BlockSizeOne;
    procedure PartialEdgeTiles;
    procedure SinglePanel;
    procedure OversizedPanel;
    procedure CrossPanelPivots;
    procedure SingularLeadingBlock;
    procedure SingularRejection;
    procedure SeparateAllocatedRows;
    procedure OffsetRows;
    procedure ReversedRows;
    procedure InvalidSettingsLeaveInputUnchanged;
    procedure OrdinaryIgnoresBlockSize;
    procedure BandFallback;
    procedure PivotToleranceBoundary;
    procedure ToleranceFromWholeMatrix;
    procedure PrepareToleranceOnce;
    procedure FactorConsumersIgnoreSettings;
    procedure ProviderOwnership;
    procedure NilProvider;
    procedure ProviderConstructionException;
    procedure SuccessRetainsAnchors;
    procedure SingularClearsAnchors;
    procedure PreparationExceptionClearsAnchors;
    procedure PanelExceptionClearsAnchors;
  end;

implementation

uses
    System.Classes
  , System.SysUtils
  , System.Math
  , panda.DynArrayUtils
  , panda.LinAlg.GaussF64
  ;

type
  TLUPAccess = class(TLUPDecompositionF64);

  TTrackingNumeric = class(TLinAlgNumericF64)
  public type
    PT = TLinAlgNumeric<Double>.PT;
  public
    class var DestroyCount: Integer;
    var PrepareCount, PivotCount: Integer;
    RaisePreparation: Boolean;
    destructor Destroy; override;
    procedure PrepareTolerance(const aRows: TArray<PT>; aCols: NativeInt;
      aRHS: Boolean); override;
    function ArgMax(const A: TArray<PT>; aRIdx, aCIdx,
      aLastRIdx: NativeInt): NativeInt; override;
  end;

  TInstanceNumeric = class(TLinAlgNumericF64)
  public type
    PT = TLinAlgNumeric<Double>.PT;
  public
    MultiplyCount, DotCount: Integer;
    function Multiply(const A, B: Double): Double; override;
    procedure Dot(pX, pY: PT; aCount: NativeInt; out aRes: Double); override;
  end;

  TNumericInstanceTests = class(TTestCase)
  published
    procedure ScalarDispatchUsesInstance;
    procedure VectorDispatchUsesInstance;
  end;

  TTestLUP = class(TLUPDecompositionF64)
  protected
    function CreateNumeric: TLinAlgNumeric<Double>; override;
  public
    function Numeric: TTrackingNumeric;
    function HasAnchors: Boolean;
  end;

  TTestGauss = class(TGaussElimF64)
  protected
    function CreateNumeric: TLinAlgNumeric<Double>; override;
  end;

  TNilLUP = class(TLUPDecompositionF64)
  protected
    function CreateNumeric: TLinAlgNumeric<Double>; override;
  end;

  TFailingNumeric = class(TTrackingNumeric)
  public
    procedure AfterConstruction; override;
  end;

  TFailingFactoryLUP = class(TLUPDecompositionF64)
  protected
    function CreateNumeric: TLinAlgNumeric<Double>; override;
  end;

  TFailingPanelLUP = class(TTestLUP)
  protected
    function FactorPanel(aStart, aWidth: NativeInt): Boolean; override;
  end;

  TLUPStateTests = class(TTestCase)
  protected
    fSolver: TTestLUP;
    function Matrix: TArray<TArray<Double>>;
    procedure CheckSolution(const aB, aX: TArray<Double>);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure RepeatedSolvesReuseFactors;
    procedure RepeatedMatrixSolves;
    procedure CopiedMatrixIndependent;
    procedure CopiedNDLayout;
    procedure BorrowedDynamicLifetime;
    procedure BorrowedNDLifetime;
    procedure InvalidVectorRHSPreservesFactors;
    procedure InvalidNDRHSPreservesFactors;
    procedure UninitializedVectorSolve;
    procedure UninitializedNDSolve;
    procedure UninitializedInverse;
    procedure UninitializedNDInverse;
    procedure UninitializedDeterminant;
    procedure SuccessfulReplacement;
    procedure InvalidReplacement;
    procedure SingularReplacement;
    procedure ExceptionalReplacement;
    procedure InvalidSettingsReplacement;
    procedure InvalidNDRHSRank;
    procedure ResultsOutliveSolver;
  end;

function TInstanceNumeric.Multiply(const A, B: Double): Double;
begin
  Inc(MultiplyCount);
  Result := inherited;
end;

procedure TInstanceNumeric.Dot(pX, pY: PT; aCount: NativeInt; out aRes: Double);
begin
  Inc(DotCount);
  inherited;
end;

procedure TNumericInstanceTests.ScalarDispatchUsesInstance;
var first, second: TInstanceNumeric;
    a, b: TLinAlgNumeric<Double>;
    value: Double;
begin
  first := TInstanceNumeric.Create;
  try
    second := TInstanceNumeric.Create;
    try
      a := first;
      b := second;

      value := a.Multiply(2, 3);

      CheckEquals(6, value, 0);
      CheckEquals(1, first.MultiplyCount);
      CheckEquals(0, second.MultiplyCount);

      value := b.Multiply(4, 5);

      CheckEquals(20, value, 0);
      CheckEquals(1, first.MultiplyCount);
      CheckEquals(1, second.MultiplyCount);
    finally
      second.Free;
    end;
  finally
    first.Free;
  end;
end;

procedure TNumericInstanceTests.VectorDispatchUsesInstance;
var first, second: TInstanceNumeric;
    a, b: TLinAlgNumeric<Double>;
    X, Y: TArray<Double>;
    value: Double;
begin
  first := TInstanceNumeric.Create;
  try
    second := TInstanceNumeric.Create;
    try
      a := first;
      b := second;
      X := TArray<Double>.Create(1, 2);
      Y := TArray<Double>.Create(3, 4);

      a.Dot(@X[0], @Y[0], Length(X), value);

      CheckEquals(11, value, 0);
      CheckEquals(1, first.DotCount);
      CheckEquals(0, second.DotCount);

      b.Dot(nil, nil, 0, value);

      CheckEquals(0, value, 0);
      CheckEquals(1, first.DotCount);
      CheckEquals(1, second.DotCount);
    finally
      second.Free;
    end;
  finally
    first.Free;
  end;
end;

destructor TTrackingNumeric.Destroy;
begin
  Inc(DestroyCount);
  inherited;
end;

procedure TTrackingNumeric.PrepareTolerance(const aRows: TArray<PT>;
  aCols: NativeInt; aRHS: Boolean);
begin
  Inc(PrepareCount);
  if RaisePreparation then raise EInvalidOp.Create('Preparation failure');
  inherited;
end;

function TTrackingNumeric.ArgMax(const A: TArray<PT>; aRIdx, aCIdx,
  aLastRIdx: NativeInt): NativeInt;
begin
  Inc(PivotCount);
  Result := inherited;
end;

function TTestLUP.CreateNumeric: TLinAlgNumeric<Double>;
begin
  Result := TTrackingNumeric.Create;
end;

function TTestLUP.Numeric: TTrackingNumeric;
begin
  Result := TTrackingNumeric(fNumeric);
end;

function TTestLUP.HasAnchors: Boolean;
begin
  Result := (Length(fRows) <> 0) or (fND <> nil) or
    (Length(fDyn) <> 0) or (Length(fBuf) <> 0) or (fWidth <> 0) or (Length(fLU) <> 0) or
    (Length(fP) <> 0);
end;

function TTestGauss.CreateNumeric: TLinAlgNumeric<Double>;
begin
  Result := TTrackingNumeric.Create;
end;

function TNilLUP.CreateNumeric: TLinAlgNumeric<Double>;
begin
  Result := nil;
end;

procedure TFailingNumeric.AfterConstruction;
begin
  inherited;
  raise EInvalidOp.Create('Provider construction failure');
end;

function TFailingFactoryLUP.CreateNumeric: TLinAlgNumeric<Double>;
begin
  Result := TFailingNumeric.Create;
end;

function TFailingPanelLUP.FactorPanel(aStart, aWidth: NativeInt): Boolean;
begin
  inherited;
  raise EInvalidOp.Create('Panel failure');
end;

function TLUPPointerTests.Matrix5: TArray<TArray<Double>>;
const rows: array[0..4] of Integer = (4, 2, 0, 3, 1);
var I, J, R: Integer;
begin
  SetLength(Result, 5);
  for I := 0 to 4 do begin
    // Allocate each row independently; a constant row stride is not available.
    SetLength(Result[I], 5);
    R := rows[I];
    for J := 0 to 4 do begin
      Result[I, J] := ((R * 3 + J * 2) mod 7 - 3) / 10;
      if R = J then Result[I, J] := Result[I, J] + 10;
    end;
  end;
end;

procedure TLUPPointerTests.CheckFactors(const A, LU: TArray<TArray<Double>>;
  const P: TArray<NativeInt>);
var I, J, K, N: Integer;
    L, S: Double;
begin
  N := Length(A);
  CheckEquals(N + 1, Length(P));
  for I := 0 to N - 1 do
    for J := 0 to N - 1 do begin
      S := 0;
      for K := 0 to Min(I, J) do begin
        if K = I then L := 1 else L := LU[I, K];
        S := S + L * LU[K, J];
      end;

      CheckEquals(A[P[I], J], S, 1e-12);
    end;
end;

procedure TLUPPointerTests.CheckBlockSize(aSize: NativeInt);
var solver: TLUPDecompositionF64;
    A, LU: TArray<TArray<Double>>;
    P: TArray<NativeInt>;
begin
  A := Matrix5;
  LU := TDynAUt.Clone<Double>(A);
  solver := TLUPDecompositionF64.Create;
  try
    solver.FactorizationMethod := lupBlocked;
    solver.BlockSize := aSize;

    CheckTrue(solver.InitializeWithoutCopy(LU) = lsisInitialized);
    P := Copy(TLUPAccess(solver).fP);

    CheckFactors(A, LU, P);
  finally
    solver.Free;
  end;
end;

procedure TLUPPointerTests.Defaults;
var solver: TLUPDecompositionF64;
begin
  solver := TLUPDecompositionF64.Create;
  try
    CheckFalse(solver.Initialized);
    CheckEquals(Ord(lupOrdinary), Ord(solver.FactorizationMethod));
    CheckEquals(32, Integer(solver.BlockSize));
    CheckEquals(-1, Integer(solver.LowerBandWidth));
    CheckEquals(-1, Integer(solver.UpperBandWidth));
    CheckEquals(0, solver.PivotTolerance, 0);
    CheckEquals(100, solver.ToleranceFactor, 0);
  finally
    solver.Free;
  end;
end;

procedure TLUPPointerTests.BlockSizeOne;
begin
  CheckBlockSize(1);
end;

procedure TLUPPointerTests.PartialEdgeTiles;
begin
  CheckBlockSize(2);

  CheckBlockSize(3);
end;

procedure TLUPPointerTests.SinglePanel;
begin
  CheckBlockSize(5);
end;

procedure TLUPPointerTests.OversizedPanel;
begin
  CheckBlockSize(High(NativeInt));
end;

procedure TLUPPointerTests.CrossPanelPivots;
var solver: TLUPDecompositionF64;
    A, LU: TArray<TArray<Double>>;
    P: TArray<NativeInt>;
begin
  A := Matrix5;
  LU := TDynAUt.Clone<Double>(A);
  solver := TLUPDecompositionF64.Create;
  try
    solver.FactorizationMethod := lupBlocked;
    solver.BlockSize := 2;

    CheckTrue(solver.InitializeWithoutCopy(LU) = lsisInitialized);
    P := Copy(TLUPAccess(solver).fP);

    CheckEquals(2, Integer(P[0]));
    CheckEquals(4, Integer(P[1]));
    CheckEquals(1, Integer(P[2]));
    CheckTrue(P[5] >= 7);
    CheckFactors(A, LU, P);
  finally
    solver.Free;
  end;
end;

procedure TLUPPointerTests.SingularLeadingBlock;
var solver: TLUPDecompositionF64;
    A, LU: TArray<TArray<Double>>;
    P: TArray<NativeInt>;
begin
  A := TArray<TArray<Double>>.Create(TArray<Double>.Create(0, 0, 1, 0),
    TArray<Double>.Create(0, 1, 0, 0), TArray<Double>.Create(1, 0, 0, 0),
    TArray<Double>.Create(0, 0, 0, 1));
  LU := TDynAUt.Clone<Double>(A);
  solver := TLUPDecompositionF64.Create;
  try
    solver.FactorizationMethod := lupBlocked;
    solver.BlockSize := 2;

    CheckTrue(solver.InitializeWithoutCopy(LU) = lsisInitialized);
    P := Copy(TLUPAccess(solver).fP);

    CheckFactors(A, LU, P);
  finally
    solver.Free;
  end;
end;

procedure TLUPPointerTests.SingularRejection;
var solver: TLUPDecompositionF64;
    LU: TArray<TArray<Double>>;
begin
  LU := Matrix5;
  LU[4] := Copy(LU[0]);
  solver := TLUPDecompositionF64.Create;
  try
    solver.FactorizationMethod := lupBlocked;
    solver.BlockSize := 2;

    CheckFalse(solver.InitializeWithoutCopy(LU) = lsisInitialized);
  finally
    solver.Free;
  end;
end;

procedure TLUPPointerTests.SeparateAllocatedRows;
begin
  CheckBlockSize(2);
end;

procedure TLUPPointerTests.OffsetRows;
var solver: TLUPDecompositionF64;
    A, backing, LU: TArray<TArray<Double>>;
    M, V: INDArray<Double>;
    P: TArray<NativeInt>;
    I, J: Integer;
begin
  A := Matrix5;
  SetLength(backing, 5, 7);
  for I := 0 to 4 do begin
    backing[I, 0] := 123;
    for J := 0 to 4 do backing[I, J + 1] := A[I, J];
    backing[I, 6] := 456;
  end;
  M := TNDAUt.AsArray<Double>(backing);
  V := M[[NDIAll, NDISpan(1, 5)]];
  solver := TLUPDecompositionF64.Create;
  try
    solver.FactorizationMethod := lupBlocked;
    solver.BlockSize := 2;

    CheckTrue(solver.InitializeWithoutCopy(V) = lsisInitialized);
    P := Copy(TLUPAccess(solver).fP);

    CheckTrue(TNDAUt.TryAsDynArray2D<Double>(V, LU));
    CheckFactors(A, LU, P);
    for I := 0 to 4 do begin
      CheckEquals(123, M.GetItem([I, 0]), 0);
      CheckEquals(456, M.GetItem([I, 6]), 0);
    end;
  finally
    solver.Free;
  end;
end;

procedure TLUPPointerTests.ReversedRows;
var solver: TLUPDecompositionF64;
    A, LU: TArray<TArray<Double>>;
    M, V: INDArray<Double>;
    P: TArray<NativeInt>;
begin
  M := TNDAUt.AsArray<Double>(Matrix5);
  V := M[[NDISpan(4, 0, -1), NDIAll]];
  CheckTrue(TNDAUt.TryAsDynArray2D<Double>(V, A));
  solver := TLUPDecompositionF64.Create;
  try
    solver.FactorizationMethod := lupBlocked;
    solver.BlockSize := 2;

    CheckTrue(solver.InitializeWithoutCopy(V) = lsisInitialized);
    P := Copy(TLUPAccess(solver).fP);

    CheckTrue(TNDAUt.TryAsDynArray2D<Double>(V, LU));
    CheckFactors(A, LU, P);
  finally
    solver.Free;
  end;
end;

procedure TLUPPointerTests.InvalidSettingsLeaveInputUnchanged;
var solver: TLUPDecompositionF64;
    A, LU: TArray<TArray<Double>>;
    P: TArray<NativeInt>;
    I: Integer;
begin
  A := Matrix5;
  LU := TDynAUt.Clone<Double>(A);
  solver := TLUPDecompositionF64.Create;
  try
    solver.FactorizationMethod := lupBlocked;
    solver.BlockSize := 0;

    CheckFalse(solver.InitializeWithoutCopy(LU) = lsisInitialized);
    P := Copy(TLUPAccess(solver).fP);

    CheckEquals(0, Length(P));
    for I := 0 to 4 do
      CheckTrue(CompareMem(@A[I, 0], @LU[I, 0], 5 * SizeOf(Double)));

    solver.BlockSize := -1;

    CheckFalse(solver.InitializeWithoutCopy(LU) = lsisInitialized);
    P := Copy(TLUPAccess(solver).fP);

    for I := 0 to 4 do
      CheckTrue(CompareMem(@A[I, 0], @LU[I, 0], 5 * SizeOf(Double)));
  finally
    solver.Free;
  end;
end;

procedure TLUPPointerTests.OrdinaryIgnoresBlockSize;
var solver: TLUPDecompositionF64;
    A, LU: TArray<TArray<Double>>;
    P: TArray<NativeInt>;
begin
  A := Matrix5;
  LU := TDynAUt.Clone<Double>(A);
  solver := TLUPDecompositionF64.Create;
  try
    solver.BlockSize := 0;

    CheckTrue(solver.InitializeWithoutCopy(LU) = lsisInitialized);
    P := Copy(TLUPAccess(solver).fP);

    CheckFactors(A, LU, P);
  finally
    solver.Free;
  end;
end;

procedure TLUPPointerTests.BandFallback;
var solver: TFailingPanelLUP;
    A, LU: TArray<TArray<Double>>;
    P: TArray<NativeInt>;
    upper: NativeInt;
begin
  A := TArray<TArray<Double>>.Create(TArray<Double>.Create(2, 1, 0),
    TArray<Double>.Create(1, 3, 1), TArray<Double>.Create(0, 1, 4));
  solver := TFailingPanelLUP.Create;
  try
    solver.FactorizationMethod := lupBlocked;
    solver.BlockSize := 2;
    for upper := -1 to 1 do begin
      if upper = 0 then Continue;
      LU := TDynAUt.Clone<Double>(A);
      solver.LowerBandWidth := 1;
      solver.UpperBandWidth := upper;

      CheckTrue(solver.InitializeWithoutCopy(LU) = lsisInitialized);
      P := Copy(TLUPAccess(solver).fP);

      CheckFactors(A, LU, P);
    end;

    LU := TDynAUt.Clone<Double>(A);
    solver.LowerBandWidth := -1;
    solver.UpperBandWidth := 1;

    CheckTrue(solver.InitializeWithoutCopy(LU) = lsisInitialized);
    P := Copy(TLUPAccess(solver).fP);

    CheckFactors(A, LU, P);
  finally
    solver.Free;
  end;
end;

procedure TLUPPointerTests.PivotToleranceBoundary;
var solver: TLUPDecompositionF64;
    LU: TArray<TArray<Double>>;
begin
  solver := TLUPDecompositionF64.Create;
  try
    solver.FactorizationMethod := lupBlocked;
    solver.PivotTolerance := 1;
    LU := TArray<TArray<Double>>.Create(TArray<Double>.Create(1));

    CheckFalse(solver.InitializeWithoutCopy(LU) = lsisInitialized);

    LU := TArray<TArray<Double>>.Create(TArray<Double>.Create(2));

    CheckTrue(solver.InitializeWithoutCopy(LU) = lsisInitialized);
  finally
    solver.Free;
  end;
end;

procedure TLUPPointerTests.ToleranceFromWholeMatrix;
var solver: TLUPDecompositionF64;
    A, LU: TArray<TArray<Double>>;
    P: TArray<NativeInt>;
begin
  A := TArray<TArray<Double>>.Create(TArray<Double>.Create(1e-12, 0, 0),
    TArray<Double>.Create(0, 1, 0), TArray<Double>.Create(0, 0, 1000));
  LU := TDynAUt.Clone<Double>(A);
  solver := TLUPDecompositionF64.Create;
  try
    solver.FactorizationMethod := lupBlocked;
    solver.BlockSize := 1;

    CheckFalse(solver.InitializeWithoutCopy(LU) = lsisInitialized);
    P := Copy(TLUPAccess(solver).fP);

    LU := TDynAUt.Clone<Double>(A);
    solver.PivotTolerance := 1e-15;

    CheckTrue(solver.InitializeWithoutCopy(LU) = lsisInitialized);
    P := Copy(TLUPAccess(solver).fP);

    CheckFactors(A, LU, P);
  finally
    solver.Free;
  end;
end;

procedure TLUPPointerTests.PrepareToleranceOnce;
var solver: TTestLUP;
    LU: TArray<TArray<Double>>;
begin
  LU := Matrix5;
  solver := TTestLUP.Create;
  try
    solver.FactorizationMethod := lupBlocked;
    solver.BlockSize := 1;

    CheckTrue(solver.InitializeWithoutCopy(LU) = lsisInitialized);

    CheckEquals(1, solver.Numeric.PrepareCount);
    CheckEquals(5, solver.Numeric.PivotCount);
  finally
    solver.Free;
  end;
end;

procedure TLUPPointerTests.FactorConsumersIgnoreSettings;
var consumer: TLUPDecompositionF64;
    A, IA: TArray<TArray<Double>>;
    B, X: TArray<Double>;
    I, J, K: Integer;
    S, expectedDet: Double;
begin
  A := Matrix5;
  SetLength(B, 5);
  for I := 0 to 4 do begin
    B[I] := 0;
    for J := 0 to 4 do B[I] := B[I] + A[I, J] * (J + 1);
  end;
  consumer := TLUPDecompositionF64.Create;
  try
    CheckTrue(consumer.Initialize(A) = lsisInitialized);
    expectedDet := consumer.Determinant;
    consumer.FactorizationMethod := lupBlocked;
    consumer.BlockSize := 0;
    consumer.PivotTolerance := 1e20;
    consumer.LowerBandWidth := 0;
    consumer.UpperBandWidth := 0;

    consumer.Solve(B, X);
    consumer.Invert(IA);

    for I := 0 to 4 do
      CheckEquals(I + 1, X[I], 1e-12);
    CheckTrue(consumer.Initialized);
    CheckEquals(expectedDet, consumer.Determinant, 0);
    for I := 0 to 4 do
      for J := 0 to 4 do begin
        S := 0;
        for K := 0 to 4 do S := S + A[I, K] * IA[K, J];

        CheckEquals(Ord(I = J), S, 1e-12);
      end;
  finally
    consumer.Free;
  end;
end;

procedure TLUPPointerTests.ProviderOwnership;
var lup: TTestLUP;
    gauss: TTestGauss;
begin
  TTrackingNumeric.DestroyCount := 0;
  lup := TTestLUP.Create;
  gauss := TTestGauss.Create;

  lup.Free;
  gauss.Free;

  CheckEquals(2, TTrackingNumeric.DestroyCount);
end;

procedure TLUPPointerTests.NilProvider;
var solver: TNilLUP;
    raised: Boolean;
begin
  solver := nil;
  raised := False;

  try
    solver := TNilLUP.Create;
  except
    on E: EInvalidOp do raised := True;
  end;
  solver.Free;

  CheckTrue(raised);
end;

procedure TLUPPointerTests.ProviderConstructionException;
var solver: TFailingFactoryLUP;
    raised: Boolean;
begin
  solver := nil;
  raised := False;
  TTrackingNumeric.DestroyCount := 0;

  try
    solver := TFailingFactoryLUP.Create;
  except
    on E: EInvalidOp do raised := True;
  end;
  solver.Free;

  CheckTrue(raised);
  CheckEquals(1, TTrackingNumeric.DestroyCount);
end;

procedure TLUPPointerTests.SuccessRetainsAnchors;
var solver: TTestLUP;
    M: INDArray<Double>;
    LU: TArray<TArray<Double>>;
begin
  solver := TTestLUP.Create;
  try
    solver.FactorizationMethod := lupBlocked;
    solver.BlockSize := 2;
    M := TNDAUt.AsArray<Double>(Matrix5);

    CheckTrue(solver.InitializeWithoutCopy(M) = lsisInitialized);

    CheckTrue(solver.HasAnchors);

    LU := Matrix5;
    solver.FactorizationMethod := lupOrdinary;

    CheckTrue(solver.InitializeWithoutCopy(LU) = lsisInitialized);

    CheckTrue(solver.HasAnchors);
  finally
    solver.Free;
  end;
end;

procedure TLUPPointerTests.SingularClearsAnchors;
var solver: TTestLUP;
    LU: TArray<TArray<Double>>;
    method: TLUPFactorizationMethod;
begin
  solver := TTestLUP.Create;
  try
    for method := lupOrdinary to lupBlocked do begin
      solver.FactorizationMethod := method;
      solver.BlockSize := 2;
      LU := Matrix5;
      LU[4] := Copy(LU[0]);

      CheckFalse(solver.InitializeWithoutCopy(LU) = lsisInitialized);

      CheckFalse(solver.HasAnchors);
    end;
  finally
    solver.Free;
  end;
end;

procedure TLUPPointerTests.PreparationExceptionClearsAnchors;
var solver: TTestLUP;
    M: INDArray<Double>;
    raised: Boolean;
begin
  M := TNDAUt.AsArray<Double>(Matrix5);
  raised := False;
  solver := TTestLUP.Create;
  try
    solver.FactorizationMethod := lupBlocked;
    solver.Numeric.RaisePreparation := True;

    try
      solver.InitializeWithoutCopy(M);
    except
      on E: EInvalidOp do raised := True;
    end;

    CheckTrue(raised);
    CheckFalse(solver.HasAnchors);

    solver.Numeric.RaisePreparation := False;

    CheckTrue(solver.InitializeWithoutCopy(M) = lsisInitialized);
  finally
    solver.Free;
  end;
end;

procedure TLUPPointerTests.PanelExceptionClearsAnchors;
var solver: TFailingPanelLUP;
    LU: TArray<TArray<Double>>;
    raised: Boolean;
begin
  LU := Matrix5;
  raised := False;
  solver := TFailingPanelLUP.Create;
  try
    solver.FactorizationMethod := lupBlocked;
    solver.BlockSize := 2;

    try
      solver.InitializeWithoutCopy(LU);
    except
      on E: EInvalidOp do raised := True;
    end;

    CheckTrue(raised);
    CheckFalse(solver.HasAnchors);
  finally
    solver.Free;
  end;
end;

procedure TLUPStateTests.SetUp;
begin
  inherited;
  fSolver := TTestLUP.Create;
end;

procedure TLUPStateTests.TearDown;
begin
  fSolver.Free;
  inherited;
end;

function TLUPStateTests.Matrix: TArray<TArray<Double>>;
begin
  Result := TArray<TArray<Double>>.Create(TArray<Double>.Create(0, 2),
    TArray<Double>.Create(3, 4));
end;

procedure TLUPStateTests.CheckSolution(const aB, aX: TArray<Double>);
begin
  CheckEquals(2, Length(aX));
  CheckEquals(aB[0], 2 * aX[1], 1e-12);
  CheckEquals(aB[1], 3 * aX[0] + 4 * aX[1], 1e-12);
end;

procedure TLUPStateTests.RepeatedSolvesReuseFactors;
var B, X: TArray<Double>;
    factors: TArray<TArray<Double>>;
    P: TArray<NativeInt>;
    method: TLUPFactorizationMethod;
    prepared, pivots, I: Integer;
begin
  for method := lupOrdinary to lupBlocked do begin
    fSolver.FactorizationMethod := method;
    fSolver.BlockSize := 1;
    CheckEquals(Ord(lsisInitialized), Ord(fSolver.Initialize(Matrix)));
    prepared := fSolver.Numeric.PrepareCount;
    pivots := fSolver.Numeric.PivotCount;
    factors := TDynAUt.Clone<Double>(fSolver.fLU);
    P := Copy(fSolver.fP);

    B := TArray<Double>.Create(4, 11);
    CheckEquals(Ord(lsssSolved), Ord(fSolver.Solve(B, X)));

    CheckSolution(B, X);

    B := TArray<Double>.Create(-6, 0);
    CheckEquals(Ord(lsssSolved), Ord(fSolver.Solve(B, X)));

    CheckSolution(B, X);
    CheckEquals(Ord(lsssSolved), Ord(fSolver.Solve(B, X)));
    CheckSolution(B, X);
    CheckTrue(fSolver.Initialized);
    CheckEquals(prepared, fSolver.Numeric.PrepareCount);
    CheckEquals(pivots, fSolver.Numeric.PivotCount);
    for I := 0 to 1 do
      CheckTrue(CompareMem(@factors[I, 0], @fSolver.fLU[I, 0], 2 * SizeOf(Double)));
    CheckTrue(CompareMem(@P[0], @fSolver.fP[0], Length(P) * SizeOf(NativeInt)));
  end;
end;

procedure TLUPStateTests.RepeatedMatrixSolves;
var B, X: INDArray<Double>;
    prepared, pivots: Integer;
begin
  CheckEquals(Ord(lsisInitialized), Ord(fSolver.Initialize(Matrix)));
  prepared := fSolver.Numeric.PrepareCount;
  pivots := fSolver.Numeric.PivotCount;
  B := TNDAUt.AsArray<Double>(TArray<TArray<Double>>.Create(
    TArray<Double>.Create(4, -6), TArray<Double>.Create(11, 0)));

  CheckEquals(Ord(lsssSolved), Ord(fSolver.Solve(B, X)));

  CheckEquals(1, X.GetItem([0, 0]), 1e-12);
  CheckEquals(2, X.GetItem([1, 0]), 1e-12);
  CheckEquals(4, X.GetItem([0, 1]), 1e-12);
  CheckEquals(-3, X.GetItem([1, 1]), 1e-12);

  B := TNDAUt.AsArray<Double>(TArray<Double>.Create(0, 3));
  CheckEquals(Ord(lsssSolved), Ord(fSolver.Solve(B, X)));

  CheckEquals(1, X.GetItem([0]), 1e-12);
  CheckEquals(0, X.GetItem([1]), 1e-12);
  CheckEquals(prepared, fSolver.Numeric.PrepareCount);
  CheckEquals(pivots, fSolver.Numeric.PivotCount);
end;

procedure TLUPStateTests.CopiedMatrixIndependent;
var A, original: TArray<TArray<Double>>;
    B, X: TArray<Double>;
    I: Integer;
begin
  A := Matrix;
  original := TDynAUt.Clone<Double>(A);
  B := TArray<Double>.Create(4, 11);

  CheckEquals(Ord(lsisInitialized), Ord(fSolver.Initialize(A)));

  for I := 0 to 1 do
    CheckTrue(CompareMem(@A[I, 0], @original[I, 0], 2 * SizeOf(Double)));
  A[0, 1] := 99;
  A := nil;

  CheckEquals(Ord(lsssSolved), Ord(fSolver.Solve(B, X)));

  CheckSolution(B, X);
end;

procedure TLUPStateTests.CopiedNDLayout;
var M, V, X: INDArray<Double>;
begin
  M := TNDAUt.AsArray<Double>(TArray<TArray<Double>>.Create(
    TArray<Double>.Create(0, 99, 2, 99), TArray<Double>.Create(3, 99, 4, 99)));
  V := M[[NDIAll, NDISpan(0, 3, 2)]];
  V.UnsetFlags(NDAF_WRITEABLE);

  CheckEquals(Ord(lsisInitialized), Ord(fSolver.Initialize(V)));

  CheckEquals(0, M.GetItem([0, 0]), 0);
  CheckEquals(3, M.GetItem([1, 0]), 0);
  M := nil;
  V := nil;
  CheckEquals(Ord(lsssSolved), Ord(fSolver.Solve(
    TNDAUt.AsArray<Double>(TArray<Double>.Create(4, 11)), X)));
  CheckEquals(1, X.GetItem([0]), 1e-12);
  CheckEquals(2, X.GetItem([1]), 1e-12);
end;

procedure TLUPStateTests.BorrowedDynamicLifetime;
var A: TArray<TArray<Double>>;
    B, X: TArray<Double>;
begin
  A := Matrix;
  B := TArray<Double>.Create(4, 11);
  CheckEquals(Ord(lsisInitialized), Ord(fSolver.InitializeWithoutCopy(A)));
  A := nil;

  CheckEquals(Ord(lsssSolved), Ord(fSolver.Solve(B, X)));

  CheckTrue(fSolver.HasAnchors);
  CheckSolution(B, X);
end;

procedure TLUPStateTests.BorrowedNDLifetime;
var A, X: INDArray<Double>;
begin
  A := TNDAUt.AsArray<Double>(Matrix);
  CheckEquals(Ord(lsisInitialized), Ord(fSolver.InitializeWithoutCopy(A)));
  A := nil;

  CheckEquals(Ord(lsssSolved), Ord(fSolver.Solve(
    TNDAUt.AsArray<Double>(TArray<Double>.Create(4, 11)), X)));

  CheckTrue(fSolver.HasAnchors);
  CheckEquals(1, X.GetItem([0]), 1e-12);
  CheckEquals(2, X.GetItem([1]), 1e-12);
end;

procedure TLUPStateTests.InvalidVectorRHSPreservesFactors;
var X: TArray<Double>;
begin
  CheckEquals(Ord(lsisInitialized), Ord(fSolver.Initialize(Matrix)));
  X := TArray<Double>.Create(99);

  CheckEquals(Ord(lsssInvalidInput), Ord(fSolver.Solve(TArray<Double>.Create(1), X)));

  CheckEquals(0, Length(X));
  CheckTrue(fSolver.Initialized);
  CheckEquals(Ord(lsssSolved), Ord(fSolver.Solve(TArray<Double>.Create(4, 11), X)));
  CheckSolution(TArray<Double>.Create(4, 11), X);
end;

procedure TLUPStateTests.InvalidNDRHSPreservesFactors;
var B, X: INDArray<Double>;
begin
  CheckEquals(Ord(lsisInitialized), Ord(fSolver.Initialize(Matrix)));
  B := TNDAUt.Empty<Double>([3, 2]);
  X := TNDAUt.Empty<Double>([1]);

  CheckEquals(Ord(lsssInvalidInput), Ord(fSolver.Solve(B, X)));

  CheckTrue(X = nil);
  CheckTrue(fSolver.Initialized);

  CheckEquals(Ord(lsssInvalidInput), Ord(fSolver.Solve(INDArray<Double>(nil), X)));

  CheckTrue(X = nil);
  CheckTrue(fSolver.Initialized);
  B := TNDAUt.AsArray<Double>(TArray<Double>.Create(4, 11));
  CheckEquals(Ord(lsssSolved), Ord(fSolver.Solve(B, X)));
  CheckEquals(1, X.GetItem([0]), 1e-12);
end;

procedure TLUPStateTests.UninitializedVectorSolve;
var X: TArray<Double>;
begin
  X := TArray<Double>.Create(99);

  CheckEquals(Ord(lsssNotInitialized), Ord(fSolver.Solve(TArray<Double>(nil), X)));

  CheckEquals(0, Length(X));
end;

procedure TLUPStateTests.UninitializedNDSolve;
var X: INDArray<Double>;
begin
  X := TNDAUt.Empty<Double>([1]);

  CheckEquals(Ord(lsssNotInitialized), Ord(fSolver.Solve(INDArray<Double>(nil), X)));

  CheckTrue(X = nil);
end;

procedure TLUPStateTests.UninitializedInverse;
var IA: TArray<TArray<Double>>;
    raised: Boolean;
begin
  IA := Matrix;
  raised := False;

  try
    fSolver.Invert(IA);
  except
    on E: EInvalidOperation do raised := True;
  end;

  CheckTrue(raised);
  CheckEquals(0, Length(IA));
end;

procedure TLUPStateTests.UninitializedNDInverse;
var IA: INDArray<Double>;
    raised: Boolean;
begin
  IA := TNDAUt.Empty<Double>([1]);
  raised := False;

  try
    fSolver.Invert(IA);
  except
    on E: EInvalidOperation do raised := True;
  end;

  CheckTrue(raised);
  CheckTrue(IA = nil);
end;

procedure TLUPStateTests.UninitializedDeterminant;
var raised: Boolean;
begin
  raised := False;

  try
    fSolver.Determinant;
  except
    on E: EInvalidOperation do raised := True;
  end;

  CheckTrue(raised);
end;

procedure TLUPStateTests.SuccessfulReplacement;
var A: TArray<TArray<Double>>;
    X: TArray<Double>;
begin
  CheckEquals(Ord(lsisInitialized), Ord(fSolver.Initialize(Matrix)));
  A := TArray<TArray<Double>>.Create(TArray<Double>.Create(5));

  CheckEquals(Ord(lsisInitialized), Ord(fSolver.Initialize(A)));

  CheckEquals(Ord(lsssSolved), Ord(fSolver.Solve(TArray<Double>.Create(15), X)));
  CheckEquals(1, Length(X));
  CheckEquals(3, X[0], 1e-12);
  CheckEquals(5, fSolver.Determinant, 1e-12);
end;

procedure TLUPStateTests.InvalidReplacement;
var A: TArray<TArray<Double>>;
    X: TArray<Double>;
begin
  CheckEquals(Ord(lsisInitialized), Ord(fSolver.Initialize(Matrix)));
  A := TArray<TArray<Double>>.Create(TArray<Double>.Create(1, 2));

  CheckEquals(Ord(lsisInvalidInput), Ord(fSolver.Initialize(A)));

  CheckFalse(fSolver.Initialized);
  CheckFalse(fSolver.HasAnchors);
  CheckEquals(Ord(lsssNotInitialized), Ord(fSolver.Solve(TArray<Double>.Create(4, 11), X)));
end;

procedure TLUPStateTests.SingularReplacement;
var A: TArray<TArray<Double>>;
begin
  CheckEquals(Ord(lsisInitialized), Ord(fSolver.Initialize(Matrix)));
  A := TArray<TArray<Double>>.Create(TArray<Double>.Create(1, 2),
    TArray<Double>.Create(2, 4));

  CheckEquals(Ord(lsisSingular), Ord(fSolver.InitializeWithoutCopy(A)));

  CheckFalse(fSolver.Initialized);
  CheckFalse(fSolver.HasAnchors);
end;

procedure TLUPStateTests.ExceptionalReplacement;
var raised: Boolean;
begin
  CheckEquals(Ord(lsisInitialized), Ord(fSolver.Initialize(Matrix)));
  fSolver.Numeric.RaisePreparation := True;
  raised := False;

  try
    fSolver.Initialize(Matrix);
  except
    on E: EInvalidOp do raised := True;
  end;

  CheckTrue(raised);
  CheckFalse(fSolver.Initialized);
  CheckFalse(fSolver.HasAnchors);
end;

procedure TLUPStateTests.InvalidSettingsReplacement;
begin
  CheckEquals(Ord(lsisInitialized), Ord(fSolver.Initialize(Matrix)));
  fSolver.FactorizationMethod := lupBlocked;
  fSolver.BlockSize := 0;

  CheckEquals(Ord(lsisInvalidInput), Ord(fSolver.InitializeWithoutCopy(Matrix)));

  CheckFalse(fSolver.Initialized);
  CheckFalse(fSolver.HasAnchors);
end;

procedure TLUPStateTests.InvalidNDRHSRank;
var B, X: INDArray<Double>;
begin
  CheckEquals(Ord(lsisInitialized), Ord(fSolver.Initialize(Matrix)));
  B := TNDAUt.Empty<Double>([2, 1, 1]);

  CheckEquals(Ord(lsssInvalidInput), Ord(fSolver.Solve(B, X)));

  CheckTrue(X = nil);
  CheckTrue(fSolver.Initialized);
end;

procedure TLUPStateTests.ResultsOutliveSolver;
var X: TArray<Double>;
    IA: TArray<TArray<Double>>;
begin
  CheckEquals(Ord(lsisInitialized), Ord(fSolver.Initialize(Matrix)));
  CheckEquals(Ord(lsssSolved), Ord(fSolver.Solve(TArray<Double>.Create(4, 11), X)));
  fSolver.Invert(IA);

  FreeAndNil(fSolver);

  CheckSolution(TArray<Double>.Create(4, 11), X);
  CheckEquals(-2/3, IA[0, 0], 1e-12);
  CheckEquals(1/3, IA[0, 1], 1e-12);
  CheckEquals(1/2, IA[1, 0], 1e-12);
  CheckEquals(0, IA[1, 1], 1e-12);
end;

initialization
  RegisterTest(TNumericInstanceTests.Suite);
  RegisterTest(TLUPPointerTests.Suite);
  RegisterTest(TLUPStateTests.Suite);

end.
