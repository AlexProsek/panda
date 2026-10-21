unit panda.LinAlg.LUP;

interface

uses
    panda.Intfs
  , panda.LinAlg.Base
  ;

type
  /// <summary>LU factorization algorithm; both produce the same packed factors.</summary>
  TLUPFactorizationMethod = (lupOrdinary, lupBlocked);

  /// <summary>
  ///   Square nonsingular LU factorization with row pivoting. Initialize prepares
  ///   factors once; Solve, Invert, and Determinant reuse them without modification.
  /// </summary>
  /// <remarks>
  ///   Initialize copies inputs; InitializeWithoutCopy retains writable storage.
  ///   Callers must not modify or resize borrowed storage while initialized;
  ///   in-place rows must not overlap. Every initialization attempt replaces the
  ///   previous state. Failure or an exception releases storage and leaves
  ///   Initialized False; borrowed inputs may contain partial factors.
  ///   Outputs retain independent values. No finite-value scan is performed.
  ///   Allocation/arithmetic exceptions propagate. Initialization costs O(N^3);
  ///   K RHS solves cost O(N^2 K). Calls on one instance must not overlap.
  /// </remarks>
  TLUPDecomposition<T> = class abstract(TLinearSolver<T>)
  public type
    PT = TLinAlgBase<T>.PT;
  protected
    fFactorizationMethod: TLUPFactorizationMethod;
    fBlockSize: NativeInt;
    // Owned copies use fLU; borrowed matrices use the inherited fDyn/fND anchors.
    fLU: TArray<TArray<T>>;
    fP: TArray<NativeInt>;
    procedure ClearAnchors; override;
    function InitializeBoundMatrix: TLinearSolverInitStatus;
    /// <summary>
    ///   Called after factorization, including rejected pivots, before failure
    ///   cleanup. Descendants may inspect protected factors; exceptions propagate.
    /// </summary>
    procedure FactorsPrepared(aSuccess: Boolean); virtual;
    procedure RequireInitialized;
    function ValidSettings: Boolean;
    procedure InvertRows(out aIA: TArray<TArray<T>>);
    procedure SolveVec(const ab: TArray<T>; aRes: TArray<T>);
    procedure SolveMat(aY: TArray<T>; aK: NativeInt);
    function DetDiag: T;
    function DecomposeBoundMatrix: Boolean;
    function DecomposeOrdinary: Boolean;
    function DecomposeBlocked: Boolean;
    function FactorPanel(aStart, aWidth: NativeInt): Boolean; virtual;
    procedure SolveBlockRow(aStart, aWidth, aCol, aCols: NativeInt); virtual;
    procedure UpdateTile(aStart, aWidth, aRow, aRows, aCol, aCols: NativeInt); virtual;
  public
    /// <summary>Creates the numerical provider; selects ordinary LU and block size 32.</summary>
    procedure AfterConstruction; override;
    /// <summary>
    ///   Copies and factors a nonempty square matrix using current settings.
    ///   Invalid dimensions/settings return lsisInvalidInput; rejected pivots
    ///   return lsisSingular. Any unsuccessful call leaves Initialized False.
    /// </summary>
    function Initialize(const aA: TArray<TArray<T>>): TLinearSolverInitStatus; overload; override;
    /// <summary>Copies any 2D layout, including read-only and strided views.</summary>
    function Initialize(const aA: INDArray<T>): TLinearSolverInitStatus; overload; override;
    /// <summary>
    ///   Factors and retains nonoverlapping square rows in place. On success
    ///   row references are placed in pivot order. Failure may modify input.
    ///   Callers must not modify or resize storage while initialized.
    /// </summary>
    function InitializeWithoutCopy(const aA: TArray<TArray<T>>): TLinearSolverInitStatus; overload; override;
    /// <summary>
    ///   Factors and retains writable square storage with column stride SizeOf(T)
    ///   and nonoverlapping rows. Offset and negative row strides are accepted.
    ///   Factors occupy the view's row order. Failure may modify input.
    /// </summary>
    function InitializeWithoutCopy(const aA: INDArray<T>): TLinearSolverInitStatus; overload; override;
    /// <summary>
    ///   Solves using cached factors and an N-element RHS. Returns a new vector.
    ///   Checks initialization first; unsuccessful calls clear aRes. Rejected
    ///   RHS inputs preserve initialization. Inputs and factors are unchanged.
    /// </summary>
    function Solve(const ab: TArray<T>; out aRes: TArray<T>): TLinearSolverSolveStatus; overload; override;
    /// <summary>
    ///   Solves for an N-element vector or an N-by-K matrix of RHS columns.
    ///   Arbitrary RHS strides are copied; the independent output has the same
    ///   shape. Checks initialization first; rejected inputs clear aRes and
    ///   preserve factors. RHS matrix dimensions must be positive.
    /// </summary>
    function Solve(const aB: INDArray<T>; out aRes: INDArray<T>): TLinearSolverSolveStatus; overload; override;
    /// <summary>
    ///   Returns an independent inverse using cached factors. Raises
    ///   EInvalidOperation when uninitialized; clears aIA before checking.
    /// </summary>
    procedure Invert(out aIA: TArray<TArray<T>>); overload;
    /// <summary>Same as the dynamic-array overload, returning an independent ND array.</summary>
    procedure Invert(out aIA: INDArray<T>); overload;
    /// <summary>
    ///   Returns the determinant using cached factors. Raises EInvalidOperation
    ///   when uninitialized. Settings do not affect cached results.
    /// </summary>
    function Determinant: T;

    /// <summary>
    ///   Method read at initialization entry. Either nonnegative bandwidth selects
    ///   ordinary bandwidth-aware processing even when blocked LU is requested.
    ///   Changes leave cached factors usable. No automatic size threshold.
    /// </summary>
    property FactorizationMethod: TLUPFactorizationMethod read fFactorizationMethod write fFactorizationMethod;
    /// <summary>
    ///   Maximum panel/tile width, initially 32. A blocked request requires a
    ///   positive value, including bandwidth fallback; invalid settings are
    ///   rejected before changing inputs. Values above N use a single panel.
    ///   Changes apply at the next initialization and preserve cached factors.
    /// </summary>
    property BlockSize: NativeInt read fBlockSize write fBlockSize;
  end;

implementation

uses
    panda.Arrays
  , panda.DynArrayUtils
  , System.Classes
  , System.SysUtils
  , System.Math
  ;

{$POINTERMATH ON}

procedure TLUPDecomposition<T>.AfterConstruction;
begin
  inherited;
  fFactorizationMethod := lupOrdinary;
  fBlockSize := 32;
end;

function TLUPDecomposition<T>.ValidSettings: Boolean;
begin
  Result := (fFactorizationMethod = lupOrdinary) or
    ((fFactorizationMethod = lupBlocked) and (fBlockSize > 0));
end;

procedure TLUPDecomposition<T>.InvertRows(out aIA: TArray<TArray<T>>);
var I, N: NativeInt;
    Y: TArray<T>;
    v: T;
begin
  N := Length(fRows);
  SetLength(aIA, N, N);
  if N <= 0 then exit;

  SetLength(Y, N * N);
  FillZero(Y);
  v := fNumeric.One();
  for I := 0 to N - 1 do
    Y[I * N + fP[I]] := v;

  SolveMat(Y, N);

  for I := 0 to N - 1 do
    TNDAUt.Copy<T>(PByte(@Y[I * N]), PByte(@aIA[I, 0]), N);
end;

procedure TLUPDecomposition<T>.SolveVec(const ab: TArray<T>; aRes: TArray<T>);
var I, N: NativeInt;
    tmp: T;
begin
  N := Length(fRows);
  if N <= 0 then exit;

  for I := 0 to N - 1 do begin
    aRes[I] := ab[fP[I]];
    fNumeric.Dot(fRows[I], @aRes[0], I, tmp);
    aRes[I] := fNumeric.Subtract(aRes[I], tmp);
  end;

  aRes[N - 1] := fNumeric.Divide(aRes[N - 1], fRows[N - 1][N - 1]);
  for I := N - 2 downto 0 do begin
    fNumeric.Dot(fRows[I] + (I + 1), @aRes[I + 1], N - I - 1, tmp);
    aRes[I] := fNumeric.SubDiv(aRes[I], tmp, fRows[I][I]);
  end;
end;

procedure TLUPDecomposition<T>.SolveMat(aY: TArray<T>; aK: NativeInt);
var I, J, N: NativeInt;
    pRow: PT;
begin
  N := Length(fRows);
  if (N <= 0) or (aK <= 0) then exit;

  for I := 0 to N - 1 do begin
    pRow := @aY[I * aK];
    for J := 0 to I - 1 do
      fNumeric.Axpy(fNumeric.Negate(fRows[I][J]), @aY[J * aK], pRow, aK);
  end;

  for I := N - 1 downto 0 do begin
    pRow := @aY[I * aK];
    for J := I + 1 to N - 1 do
      fNumeric.Axpy(fNumeric.Negate(fRows[I][J]), @aY[J * aK], pRow, aK);
    fNumeric.VecMul(pRow, fNumeric.Reciprocal(fRows[I][I]), pRow, aK);
  end;
end;

function TLUPDecomposition<T>.DetDiag: T;
var I, N: NativeInt;
begin
  N := Length(fRows);
  Result := fRows[0][0];
  for I := 1 to N - 1 do
    Result := fNumeric.Multiply(Result, fRows[I][I]);
  if ((fP[N] - N) mod 2) <> 0 then
    Result := fNumeric.Negate(Result);
end;

procedure TLUPDecomposition<T>.ClearAnchors;
begin
  fLU := nil;
  fP := nil;
  inherited;
end;

procedure TLUPDecomposition<T>.FactorsPrepared(aSuccess: Boolean);
begin
end;

procedure TLUPDecomposition<T>.RequireInitialized;
begin
  if not fInitialized then
    raise EInvalidOperation.Create('LUP decomposition is not initialized');
end;

function TLUPDecomposition<T>.InitializeBoundMatrix: TLinearSolverInitStatus;
var success: Boolean;
begin
  success := DecomposeBoundMatrix;
  FactorsPrepared(success);
  if not success then exit(lsisSingular);
  fInitialized := True;
  Result := lsisInitialized;
end;

function TLUPDecomposition<T>.Initialize(const aA: TArray<TArray<T>>): TLinearSolverInitStatus;
var LU: TArray<TArray<T>>;
begin
  ClearAnchors;
  Result := lsisInvalidInput;
  if not ValidSettings then exit;
  try
    if not TDynAUt.SquareMatrixQ<T>(aA) or (Length(aA) = 0) then exit;
    LU := TDynAUt.Clone<T>(aA);
    if not BindDyn(LU) then exit;
    fLU := LU;
    Result := InitializeBoundMatrix;
  finally
    if not fInitialized then ClearAnchors;
  end;
end;

function TLUPDecomposition<T>.InitializeWithoutCopy(const aA: TArray<TArray<T>>): TLinearSolverInitStatus;
begin
  ClearAnchors;
  Result := lsisInvalidInput;
  if not ValidSettings then exit;
  try
    if not TDynAUt.SquareMatrixQ<T>(aA) or not BindDyn(aA) then exit;
    Result := InitializeBoundMatrix;
  finally
    if not fInitialized then ClearAnchors;
  end;
end;

function TLUPDecomposition<T>.Initialize(const aA: INDArray<T>): TLinearSolverInitStatus;
var LU: TArray<TArray<T>>;
    spec: TMatSpec;
begin
  ClearAnchors;
  Result := lsisInvalidInput;
  if not ValidSettings then exit;
  try
    if not NDMatrix(aA, spec) then exit;
    if (spec.NRows = 0) or (spec.NRows <> spec.NCols) then exit;
    if not CopyMatrix(aA, LU) or not BindDyn(LU) then exit;
    fLU := LU;
    Result := InitializeBoundMatrix;
  finally
    if not fInitialized then ClearAnchors;
  end;
end;

function TLUPDecomposition<T>.InitializeWithoutCopy(const aA: INDArray<T>): TLinearSolverInitStatus;
begin
  ClearAnchors;
  Result := lsisInvalidInput;
  if not ValidSettings then exit;
  try
    if not AliasND(aA, False) or (Length(fRows) = 0) then exit;
    Result := InitializeBoundMatrix;
  finally
    if not fInitialized then ClearAnchors;
  end;
end;

function TLUPDecomposition<T>.Solve(const ab: TArray<T>;
  out aRes: TArray<T>): TLinearSolverSolveStatus;
var N: NativeInt;
begin
  aRes := nil;
  if not fInitialized then exit(lsssNotInitialized);
  N := Length(fRows);
  if Length(ab) <> N then exit(lsssInvalidInput);
  SetLength(aRes, N);
  SolveVec(ab, aRes);
  Result := lsssSolved;
end;

function TLUPDecomposition<T>.Solve(const aB: INDArray<T>;
  out aRes: INDArray<T>): TLinearSolverSolveStatus;
var bSpec: TMatSpec;
    colB, colX, Y: TArray<T>;
    N, K, I, J: NativeInt;
    vec: Boolean;
begin
  aRes := nil;
  if not fInitialized then exit(lsssNotInitialized);
  Result := lsssInvalidInput;
  if aB = nil then exit;
  N := Length(fRows);

  vec := VectorQ(aB);
  if vec then begin
    if aB.Size <> N then exit;
    K := 1;
    aRes := TNDAUt.Empty<T>([N]);
  end else begin
    if not NDMatrix(aB, bSpec) then exit;
    if bSpec.NRows <> N then exit;
    K := bSpec.NCols;
    if K <= 0 then exit;
    aRes := TNDAUt.Empty<T>([N, K]);
  end;

  if K <= 1 then begin
    SetLength(colB, N);
    SetLength(colX, N);
    for J := 0 to K - 1 do begin
      if vec then
        Gather(aB.Data, aB.Strides[0], N, colB)
      else
        Gather(bSpec.Data + J * bSpec.CStep, bSpec.RStep, N, colB);
      SolveVec(colB, colX);
      Store(aRes, J, colX, vec);
    end;
  end else begin
    SetLength(Y, N * K);
    SetLength(colB, N);
    SetLength(colX, N);
    for J := 0 to K - 1 do begin
      Gather(bSpec.Data + J * bSpec.CStep, bSpec.RStep, N, colB);
      for I := 0 to N - 1 do
        Y[I * K + J] := colB[fP[I]];
    end;
    SolveMat(Y, K);
    for J := 0 to K - 1 do begin
      for I := 0 to N - 1 do
        colX[I] := Y[I * K + J];
      Store(aRes, J, colX, False);
    end;
  end;
  Result := lsssSolved;
end;

procedure TLUPDecomposition<T>.Invert(out aIA: TArray<TArray<T>>);
begin
  aIA := nil;
  RequireInitialized;
  InvertRows(aIA);
end;

procedure TLUPDecomposition<T>.Invert(out aIA: INDArray<T>);
var tmp: TArray<TArray<T>>;
begin
  aIA := nil;
  RequireInitialized;
  InvertRows(tmp);
  aIA := TNDAUt.AsArray<T>(tmp);
end;

function TLUPDecomposition<T>.Determinant: T;
begin
  RequireInitialized;
  Result := DetDiag;
end;

function TLUPDecomposition<T>.DecomposeOrdinary: Boolean;
var I, J, iMax, N, lower, upper, lastRow, lastCol: NativeInt;
    s, v: T;
begin
  N := Length(fRows);
  GetBandWidths(N, lower, upper);

  for I := 0 to N - 1 do begin
    lastRow := N - 1;
    if lower < lastRow - I then
      lastRow := I + lower;
    iMax := fNumeric.ArgMax(fRows, I, I, lastRow);
    if fNumeric.IsZeroTol(fRows[iMax][I]) then
      exit(False);

    if iMax <> I then begin
      J := fP[I];
      fP[I] := fP[iMax];
      fP[iMax] := J;
      SwapPivot(I, iMax);
      Inc(fP[N]);
    end;

    lastCol := N - 1;
    if upper < lastCol - I then
      lastCol := I + upper;
    s := fRows[I][I];
    for J := I + 1 to lastRow do begin
      v := fNumeric.Divide(fRows[J][I], s);
      fRows[J][I] := v;
      fNumeric.Axpy(fNumeric.Negate(v), fRows[I] + (I + 1), fRows[J] + (I + 1), lastCol - I);
    end;
  end;

  Result := True;
end;

function TLUPDecomposition<T>.DecomposeBoundMatrix: Boolean;
var N, I: NativeInt;
    spec: TMatSpec;
begin
  N := Length(fRows);
  SetLength(fP, N + 1);
  for I := 0 to N do
    fP[I] := I;
  fNumeric.PrepareTolerance(fRows, N, False);

  if
    (fFactorizationMethod = lupBlocked) and
    (fLowerBandWidth < 0) and (fUpperBandWidth < 0)
  then
    Result := DecomposeBlocked
  else
    Result := DecomposeOrdinary;

  if not Result then exit;

  if fND <> nil then begin
    GetMatSpec(fND, spec);
    PlaceRows(RowPointers(spec), fP, fWidth);
    fRows := RowPointers(spec);
  end else begin
    PlaceDynRows(fDyn, fP);
    fRows := RowPointers(fDyn);
  end;
end;

function TLUPDecomposition<T>.FactorPanel(aStart, aWidth: NativeInt): Boolean;
var K, I, J, pivot, N, last: NativeInt;
    s, v: T;
begin
  N := Length(fRows);
  last := aStart + aWidth - 1;
  for K := aStart to last do begin
    pivot := fNumeric.ArgMax(fRows, K, K, N - 1);
    if fNumeric.IsZeroTol(fRows[pivot][K]) then
      exit(False);

    if pivot <> K then begin
      SwapPivot(K, pivot);
      J := fP[K];
      fP[K] := fP[pivot];
      fP[pivot] := J;
      Inc(fP[N]);
    end;

    s := fRows[K][K];
    for I := K + 1 to N - 1 do begin
      v := fNumeric.Divide(fRows[I][K], s);
      fRows[I][K] := v;
      fNumeric.Axpy(fNumeric.Negate(v), fRows[K] + K + 1, fRows[I] + K + 1, last - K);
    end;
  end;
  Result := True;
end;

procedure TLUPDecomposition<T>.SolveBlockRow(aStart, aWidth, aCol, aCols: NativeInt);
var I, J: NativeInt;
begin
  for I := aStart to aStart + aWidth - 1 do
    for J := aStart to I - 1 do
      fNumeric.Axpy(fNumeric.Negate(fRows[I][J]), fRows[J] + aCol,
        fRows[I] + aCol, aCols);
end;

procedure TLUPDecomposition<T>.UpdateTile(aStart, aWidth, aRow, aRows, aCol, aCols: NativeInt);
var I, K: NativeInt;
begin
  for I := aRow to aRow + aRows - 1 do
    for K := aStart to aStart + aWidth - 1 do
      fNumeric.Axpy(fNumeric.Negate(fRows[I][K]), fRows[K] + aCol,
        fRows[I] + aCol, aCols);
end;

function TLUPDecomposition<T>.DecomposeBlocked: Boolean;
var N, K, W, I, J, H, C: NativeInt;
begin
  N := Length(fRows);
  K := 0;
  while K < N do begin
    W := Min(fBlockSize, N - K);
    if not FactorPanel(K, W) then exit(False);
    J := K + W;
    while J < N do begin
      C := Min(fBlockSize, N - J);
      SolveBlockRow(K, W, J, C);
      I := K + W;
      while I < N do begin
        H := Min(fBlockSize, N - I);
        UpdateTile(K, W, I, H, J, C);
        Inc(I, H);
      end;
      Inc(J, C);
    end;
    Inc(K, W);
  end;
  Result := True;
end;

end.
