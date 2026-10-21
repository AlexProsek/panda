unit panda.LinAlg.Cholesky;

interface

uses
    panda.Intfs
  , panda.LinAlg.Base
  ;

type
  /// <summary>
  ///   Real symmetric positive definite factorization A = L * Transpose(L).
  ///   Only the lower triangle is read and overwritten; the upper triangle is
  ///   ignored. Initialize prepares factors for repeated, non-destructive solves.
  /// </summary>
  /// <remarks>
  ///   Every initialization attempt replaces the previous state. Failure or an
  ///   exception releases storage and leaves Initialized False; borrowed inputs
  ///   may contain partial factors. Borrowed rows must not overlap, and callers
  ///   must not modify or resize retained storage while initialized. Row order
  ///   is preserved. Inherited bandwidth settings are unused (dense processing).
  ///   Finite scalar inputs are required; no finite-value scan is performed.
  ///   Arithmetic/allocation exceptions propagate. Initialization costs O(N^3)
  ///   and uses O(N) temporary column storage;
  ///   K RHS solves cost O(N^2 K). Calls on one instance must not overlap.
  /// </remarks>
  TCholeskyDecomposition<T> = class abstract(TLinearSolver<T>)
  protected
    /// <summary>Prepares the diagonal policy from the original lower triangle.</summary>
    procedure PrepareFactorization; virtual; abstract;
    /// <summary>Validates a diagonal residual and returns its positive square root.</summary>
    function TrySqrtPivot(const aValue: T; out aRoot: T): Boolean; virtual; abstract;
    function InitializeBoundMatrix: TLinearSolverInitStatus;
    procedure SolveVec(const ab: TArray<T>; aRes: TArray<T>);
  public
    /// <summary>Copies and factors a nonempty square matrix.</summary>
    function Initialize(const aA: TArray<TArray<T>>): TLinearSolverInitStatus; overload; override;
    /// <summary>Copies and factors any square 2D layout, including read-only views.</summary>
    function Initialize(const aA: INDArray<T>): TLinearSolverInitStatus; overload; override;
    /// <summary>
    ///   Retains and factors nonempty square storage with nonoverlapping rows.
    ///   Only the lower triangle is changed; failure may leave partial factors.
    /// </summary>
    function InitializeWithoutCopy(const aA: TArray<TArray<T>>): TLinearSolverInitStatus; overload; override;
    /// <summary>
    ///   Retains writable square storage with column stride SizeOf(T) and
    ///   nonoverlapping rows. Offset and negative row strides are accepted.
    ///   Only the lower triangle is changed; failure may leave partial factors.
    /// </summary>
    function InitializeWithoutCopy(const aA: INDArray<T>): TLinearSolverInitStatus; overload; override;
    /// <summary>
    ///   Solves an N-element RHS into an independent vector. Checks initialization
    ///   first. Unsuccessful calls clear aRes; invalid RHS inputs preserve factors.
    /// </summary>
    function Solve(const ab: TArray<T>; out aRes: TArray<T>): TLinearSolverSolveStatus; overload; override;
    /// <summary>
    ///   Solves an N-element vector or N-by-K matrix of RHS columns (K positive).
    ///   Arbitrary RHS strides are copied; independent output has the same shape.
    ///   Checks initialization first; unsuccessful calls clear aRes. Inputs and
    ///   factors are unchanged, and invalid RHS inputs preserve initialization.
    /// </summary>
    function Solve(const aB: INDArray<T>; out aRes: INDArray<T>): TLinearSolverSolveStatus; overload; override;
  end;

implementation

uses
    panda.Arrays
  , panda.DynArrayUtils
  ;

{$POINTERMATH ON}

function TCholeskyDecomposition<T>.InitializeBoundMatrix: TLinearSolverInitStatus;
var I, J, N: NativeInt;
    column: TArray<T>;
    root: T;
begin
  PrepareFactorization;
  N := Length(fRows);
  SetLength(column, N);
  for J := 0 to N - 1 do begin
    if not TrySqrtPivot(fRows[J][J], root) then exit(lsisNotPositiveDefinite);
    fRows[J][J] := root;
    for I := J + 1 to N - 1 do begin
      column[I] := fNumeric.Divide(fRows[I][J], root);
      fRows[I][J] := column[I];
    end;
    // Gather the column before updating contiguous lower-triangle row segments.
    for I := J + 1 to N - 1 do
      fNumeric.Axpy(fNumeric.Negate(column[I]), @column[J + 1],
        fRows[I] + (J + 1), I - J);
  end;
  fInitialized := True;
  Result := lsisInitialized;
end;

function TCholeskyDecomposition<T>.Initialize(const aA: TArray<TArray<T>>): TLinearSolverInitStatus;
var L: TArray<TArray<T>>;
begin
  ClearAnchors;
  try
    if not TDynAUt.SquareMatrixQ<T>(aA) then exit(lsisInvalidInput);
    L := TDynAUt.Clone<T>(aA);
    if not BindDyn(L) then exit(lsisInvalidInput);
    Result := InitializeBoundMatrix;
  finally
    if not fInitialized then ClearAnchors;
  end;
end;

function TCholeskyDecomposition<T>.Initialize(const aA: INDArray<T>): TLinearSolverInitStatus;
var L: TArray<TArray<T>>;
    spec: TMatSpec;
begin
  ClearAnchors;
  Result := lsisInvalidInput;
  try
    if not NDMatrix(aA, spec) then exit;
    if (spec.NRows = 0) or (spec.NRows <> spec.NCols) then exit;
    if not CopyMatrix(aA, L) or not BindDyn(L) then exit;
    Result := InitializeBoundMatrix;
  finally
    if not fInitialized then ClearAnchors;
  end;
end;

function TCholeskyDecomposition<T>.InitializeWithoutCopy(const aA: TArray<TArray<T>>): TLinearSolverInitStatus;
begin
  ClearAnchors;
  try
    if not (TDynAUt.SquareMatrixQ<T>(aA) and BindDyn(aA)) then exit(lsisInvalidInput);
    Result := InitializeBoundMatrix;
  finally
    if not fInitialized then ClearAnchors;
  end;
end;

function TCholeskyDecomposition<T>.InitializeWithoutCopy(const aA: INDArray<T>): TLinearSolverInitStatus;
begin
  ClearAnchors;
  try
    if not AliasND(aA, False) or (Length(fRows) = 0) then exit(lsisInvalidInput);
    Result := InitializeBoundMatrix;
  finally
    if not fInitialized then ClearAnchors;
  end;
end;

procedure TCholeskyDecomposition<T>.SolveVec(const ab: TArray<T>; aRes: TArray<T>);
var I, N: NativeInt;
    dot: T;
begin
  N := Length(fRows);
  for I := 0 to N - 1 do begin
    fNumeric.Dot(fRows[I], @aRes[0], I, dot);
    aRes[I] := fNumeric.SubDiv(ab[I], dot, fRows[I][I]);
  end;

  for I := N - 1 downto 1 do begin
    aRes[I] := fNumeric.Divide(aRes[I], fRows[I][I]);
    fNumeric.Axpy(fNumeric.Negate(aRes[I]), fRows[I], @aRes[0], I);
  end;

  aRes[0] := fNumeric.Divide(aRes[0], fRows[0][0]);
end;

function TCholeskyDecomposition<T>.Solve(const ab: TArray<T>; out aRes: TArray<T>): TLinearSolverSolveStatus;
var X: TArray<T>;
begin
  if not fInitialized then exit(lsssNotInitialized);
  if Length(ab) <> Length(fRows) then exit(lsssInvalidInput);

  SetLength(X, Length(fRows));
  SolveVec(ab, X);
  aRes := X;
  Result := lsssSolved;
end;

function TCholeskyDecomposition<T>.Solve(const aB: INDArray<T>; out aRes: INDArray<T>): TLinearSolverSolveStatus;
var spec: TMatSpec;
    B, X: TArray<T>;
    output: INDArray<T>;
    N, K, J: NativeInt;
    vec: Boolean;
begin
  if not fInitialized then exit(lsssNotInitialized);

  Result := lsssInvalidInput;
  if aB = nil then exit;
  N := Length(fRows);
  vec := VectorQ(aB);
  if vec then begin
    if aB.Size <> N then exit;
    K := 1;
    output := TNDAUt.Empty<T>([N]);
  end else begin
    if not NDMatrix(aB, spec) then exit;
    if (spec.NRows <> N) or (spec.NCols <= 0) then exit;
    K := spec.NCols;
    output := TNDAUt.Empty<T>([N, K]);
  end;
  SetLength(B, N);
  SetLength(X, N);
  for J := 0 to K - 1 do begin
    if vec then
      Gather(aB.Data, aB.Strides[0], N, B)
    else
      Gather(spec.Data + J * spec.CStep, spec.RStep, N, B);
    SolveVec(B, X);
    Store(output, J, X, vec);
  end;
  aRes := output;
  Result := lsssSolved;
end;

end.
