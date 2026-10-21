unit panda.LinAlg.Base;

interface

uses
    panda.Intfs
  ;

type
  /// <summary>
  ///   Absolute tolerances of <c>TGaussElimF64</c> and <c>TGaussElimC128</c>.
  ///   A value is treated as zero when its absolute value (modulus for complex
  ///   numbers) does not exceed the tolerance. A field that is not positive is
  ///   estimated from the data when needed, see the specialization's
  ///   <c>EstimateTolerance</c>.
  /// </summary>
  TGaussTolerance = record
    /// <summary>Tolerance of pivot and rank tests on the coefficients.</summary>
    Pivot: Double;
    /// <summary>
    ///   Tolerance of the consistency test of the right-hand side in
    ///   <c>Solve</c>. Not used by LUP initialization.
    /// </summary>
    RHS: Double;
  end;

  /// <summary>
  ///   Arithmetic and pivot policy shared by elimination and LUP algorithms.
  ///   Providers own settings and prepared tolerances, never matrix storage.
  ///   Arithmetic hooks dispatch on the provider instance and may use its state.
  ///   Row buffers must remain alive and contiguous within each row during calls.
  ///   Arithmetic and allocation exceptions propagate. Instances are not reentrant.
  /// </summary>
  TLinAlgNumeric<T> = class abstract
  public type
    PT = ^T;
  public
    /// <summary>
    ///   Selects a pivot within the inclusive row range in column aCIdx. The
    ///   caller checks the selected value with IsZeroTol; a zero-only range
    ///   must still return an index in that range. Bounds must be valid.
    /// </summary>
    function ArgMax(const A: TArray<PT>; aRIdx, aCIdx,
      aLastRIdx: NativeInt): NativeInt; virtual; abstract;
    /// <summary>
    ///   Prepares zero thresholds from the original coefficient rows. aCols
    ///   excludes the optional RHS at column aCols. Does not retain aRows.
    /// </summary>
    procedure PrepareTolerance(const aRows: TArray<PT>; aCols: NativeInt;
      aRHS: Boolean); virtual; abstract;
    /// <summary>Tests a coefficient against the prepared pivot threshold.</summary>
    function IsZeroTol(const A: T): Boolean; virtual; abstract;
    /// <summary>Tests a right-hand side against the prepared consistency threshold.</summary>
    function IsZeroRHSTol(const A: T): Boolean; virtual; abstract;
    /// <summary>Tests exact mathematical zero, without a prepared tolerance.</summary>
    function IsZeroValue(const A: T): Boolean; virtual; abstract;

    /// <summary>Returns mathematical one.</summary>
    function One: T; virtual; abstract;
    /// <summary>Returns mathematical zero, which need not equal Default(T).</summary>
    function Zero: T; virtual; abstract;
    /// <summary>Returns A - B.</summary>
    function Subtract(const A, B: T): T; virtual; abstract;
    /// <summary>Returns A / B using scalar arithmetic and its error rules.</summary>
    function Divide(const A, B: T): T; virtual; abstract;
    /// <summary>Returns -A.</summary>
    function Negate(const A: T): T; virtual; abstract;
    /// <summary>Returns A * B.</summary>
    function Multiply(const A, B: T): T; virtual; abstract;
    /// <summary>Returns 1 / A using scalar arithmetic and its error rules.</summary>
    function Reciprocal(const A: T): T; virtual; abstract;
    /// <summary>Returns (A - B) / C.</summary>
    function SubDiv(const A, B, C: T): T; virtual; abstract;
    /// <summary>Returns -A / B.</summary>
    function NegDiv(const A, B: T): T; virtual; abstract;
    /// <summary>
    ///   Writes the maximum magnitude of aCount contiguous values to aMax;
    ///   returns zero for an empty range. Complex providers store modulus in Re.
    /// </summary>
    procedure AbsMax(pData: PT; aCount: NativeInt; var aMax: T); virtual; abstract;
    /// <summary>
    ///   Updates aCount contiguous values as Y := Y + a*X. A zero count is
    ///   accepted. Source values stay unchanged unless X and Y are identical;
    ///   identical buffers are supported, other partial overlaps are not required.
    /// </summary>
    procedure Axpy(const a: T; pX, pY: PT; aCount: NativeInt); virtual; abstract;
    /// <summary>
    ///   Writes the ordinary, unconjugated dot product of aCount contiguous
    ///   values to aRes; an empty range produces mathematical zero.
    /// </summary>
    procedure Dot(pX, pY: PT; aCount: NativeInt; out aRes: T); virtual; abstract;
    /// <summary>
    ///   Writes Y := a*X for aCount contiguous values. Identical buffers and
    ///   a zero count are supported; other partial overlaps are not required.
    /// </summary>
    procedure VecMul(pX: PT; const a: T; pY: PT; aCount: NativeInt); virtual; abstract;
  end;

  /// <summary>
  ///   Common ancestor of the linear algebra solvers. Holds the rows of the
  ///   processed matrix as row pointers into a dynamic array, an
  ///   <c>INDArray</c> or an internal buffer, and owns the numerical provider
  ///   created by concrete descendants. Calls on one instance must not overlap.
  /// </summary>
  TLinAlgBase<T> = class abstract
  public type
    PT = TLinAlgNumeric<T>.PT;
  protected const
    cElSz = SizeOf(T);
  protected
    fRows: TArray<PT>;
    fWidth: Integer;
    fDyn: TArray<TArray<T>>;
    fND: INDArray<T>;
    fBuf: TArray<T>;
    fNumeric: TLinAlgNumeric<T>;
    fLowerBandWidth: NativeInt;
    fUpperBandWidth: NativeInt;
    /// <summary>Creates an owned numerical provider. Must not return nil.</summary>
    function CreateNumeric: TLinAlgNumeric<T>; virtual; abstract;
    procedure GetBandWidths(aSize: NativeInt; out aLower, aUpper: NativeInt);
    procedure ClearAnchors; virtual;
    procedure SwapPivot(aIdx1, aIdx2: NativeInt);
    function RowContiguous(const aSpec: TMatSpec): Boolean;
    function NDMatrix(const aA: INDArray<T>; out aSpec: TMatSpec): Boolean;
    function CopyMatrix(const aA: INDArray<T>; out aDst: TArray<TArray<T>>): Boolean;
    function BindDyn(const aA: TArray<TArray<T>>): Boolean;
    function AliasND(const aA: INDArray<T>; aAugmented: Boolean): Boolean;
    function RowPointers(const aSpec: TMatSpec): TArray<PT>; overload;
    function RowPointers(const aA: TArray<TArray<T>>): TArray<PT>; overload;
    procedure PlaceRows(const aPhys: TArray<PT>; const aP: TArray<NativeInt>;
      aWidth: NativeInt);
    procedure PlaceDynRows(aRows: TArray<TArray<T>>; const aP: TArray<NativeInt>);
    procedure Gather(aSrc: PByte; aStep, aCount: NativeInt; aDst: TArray<T>);
    procedure Store(const aDst: INDArray<T>; aCol: NativeInt; const aSrc: TArray<T>;
      aVector: Boolean);
    // Initializes mathematical zeros, which need not be Default(T).
    procedure FillZero(var aValues: TArray<T>);
  public
    /// <summary>Creates the provider and sets unrestricted bandwidths.</summary>
    procedure AfterConstruction; override;
    /// <summary>Releases the provider and any retained matrix anchors.</summary>
    destructor Destroy; override;
    /// <summary>
    ///   Trusted lower bandwidth of the original coefficients, initially -1
    ///   (unrestricted). Entries outside a nonnegative band must be exact zeros.
    ///   Widths are clipped to matrix dimensions; settings survive initialization.
    /// </summary>
    property LowerBandWidth: NativeInt read fLowerBandWidth write fLowerBandWidth;
    /// <summary>
    ///   Trusted upper bandwidth, initially -1. Both nonnegative widths bound
    ///   fill to lower + upper; an upper width alone does not restrict arithmetic.
    ///   Rectangular general elimination and skipped-pivot remainders run dense.
    ///   LUP consumers ignore widths; decomposition reads them at entry.
    /// </summary>
    property UpperBandWidth: NativeInt read fUpperBandWidth write fUpperBandWidth;
  end;

  /// <summary>Outcome of preparing a matrix for repeated linear solves.</summary>
  TLinearSolverInitStatus = (
    /// <summary>The factors are ready.</summary>
    lsisInitialized,
    /// <summary>The matrix dimensions, storage layout, or settings are invalid.</summary>
    lsisInvalidInput,
    /// <summary>A pivot was rejected by the solver's singularity policy.</summary>
    lsisSingular,
    /// <summary>A Cholesky diagonal residual was rejected by its pivot policy.</summary>
    lsisNotPositiveDefinite
  );

  /// <summary>Outcome of solving using an initialized linear solver.</summary>
  TLinearSolverSolveStatus = (
    /// <summary>An independent solution was returned.</summary>
    lsssSolved,
    /// <summary>The RHS dimensions or storage layout are invalid.</summary>
    lsssInvalidInput,
    /// <summary>No matrix has been successfully initialized.</summary>
    lsssNotInitialized
  );

  /// <summary>
  ///   Reusable solver for nonempty square systems with a unique solution.
  ///   Descendants define matrix requirements and numerical rejection policies.
  /// </summary>
  /// <remarks>
  ///   Every initialization attempt replaces the previous state. Failure or an
  ///   exception leaves Initialized False and releases retained storage; borrowed
  ///   inputs may contain partial factors. Callers must not modify or resize
  ///   borrowed storage while initialized. Solve reuses factors without changing
  ///   them or the RHS; results own independent storage. Arithmetic/allocation
  ///   exceptions propagate. Calls on one instance must not overlap. Current
  ///   dense descendants factor in O(N^3) and solve K RHS columns in O(N^2 K).
  /// </remarks>
  TLinearSolver<T> = class abstract(TLinAlgBase<T>)
  protected
    fInitialized: Boolean;
    procedure ClearAnchors; override;
  public
    /// <summary>Copies and factors a nonempty square matrix aA; returns its status.</summary>
    function Initialize(const aA: TArray<TArray<T>>): TLinearSolverInitStatus; overload; virtual; abstract;
    /// <summary>Copies and factors square aA with arbitrary strides, including read-only views.</summary>
    function Initialize(const aA: INDArray<T>): TLinearSolverInitStatus; overload; virtual; abstract;
    /// <summary>
    ///   Retains and factors square aA in place; returns its status. Rows must
    ///   not overlap. Row placement and triangle use depend on the descendant.
    /// </summary>
    function InitializeWithoutCopy(const aA: TArray<TArray<T>>): TLinearSolverInitStatus; overload; virtual; abstract;
    /// <summary>
    ///   Retains and factors writable square aA with column stride SizeOf(T)
    ///   and nonoverlapping rows. Offset and negative row strides are accepted.
    /// </summary>
    function InitializeWithoutCopy(const aA: INDArray<T>): TLinearSolverInitStatus; overload; virtual; abstract;
    /// <summary>
    ///   Solves N-element ab into independent aRes. Returns NotInitialized before
    ///   checking ab; unsuccessful statuses clear aRes and preserve factors.
    /// </summary>
    function Solve(const ab: TArray<T>; out aRes: TArray<T>): TLinearSolverSolveStatus; overload; virtual; abstract;
    /// <summary>
    ///   Solves an N-element vector or N-by-K RHS matrix aB, K positive, into
    ///   independent aRes of the same shape. Arbitrary RHS strides are accepted.
    ///   Returns NotInitialized before checking aB; unsuccessful statuses clear
    ///   aRes and preserve factors.
    /// </summary>
    function Solve(const aB: INDArray<T>; out aRes: INDArray<T>): TLinearSolverSolveStatus; overload; virtual; abstract;
    /// <summary>True after successful initialization until replacement or destruction.</summary>
    property Initialized: Boolean read fInitialized;
  end;

implementation

uses
    panda.Arrays
  , System.SysUtils
  ;

{$POINTERMATH ON}

procedure TLinearSolver<T>.ClearAnchors;
begin
  fInitialized := False;
  inherited;
end;

{$region 'TLinAlgBase<T>'}

procedure TLinAlgBase<T>.ClearAnchors;
begin
  fRows := nil;
  fDyn := nil;
  fND := nil;
  fBuf := nil;
  fWidth := 0;
end;

procedure TLinAlgBase<T>.AfterConstruction;
begin
  inherited;
  fLowerBandWidth := -1;
  fUpperBandWidth := -1;
  fNumeric := CreateNumeric;
  if fNumeric = nil then raise EInvalidOp.Create('Numerical provider must not be nil');
end;

destructor TLinAlgBase<T>.Destroy;
begin
  ClearAnchors;
  fNumeric.Free;
  inherited;
end;

procedure TLinAlgBase<T>.GetBandWidths(aSize: NativeInt; out aLower, aUpper: NativeInt);
begin
  aLower := aSize - 1;
  aUpper := aSize - 1;
  if fLowerBandWidth < 0 then exit;

  if fLowerBandWidth < aLower then
    aLower := fLowerBandWidth;
  if fUpperBandWidth < 0 then exit;

  // Compare before adding: declarations can be as large as High(NativeInt).
  if fUpperBandWidth < aUpper - aLower then
    aUpper := aLower + fUpperBandWidth;
end;

procedure TLinAlgBase<T>.SwapPivot(aIdx1, aIdx2: NativeInt);
var p: PT;
begin
  if aIdx1 = aIdx2 then exit;

  p := fRows[aIdx1];
  fRows[aIdx1] := fRows[aIdx2];
  fRows[aIdx2] := p;
end;

function TLinAlgBase<T>.RowContiguous(const aSpec: TMatSpec): Boolean;
begin
  Result := (aSpec.CStep = cElSz) and (Abs(aSpec.RStep) >= aSpec.NCols * cElSz);
end;

function TLinAlgBase<T>.NDMatrix(const aA: INDArray<T>; out aSpec: TMatSpec): Boolean;
begin
  if not (Assigned(aA) and MatrixQ(aA)) then exit(False);
  GetMatSpec(aA, aSpec);
  Result := True;
end;

function TLinAlgBase<T>.CopyMatrix(const aA: INDArray<T>; out aDst: TArray<TArray<T>>): Boolean;
var spec: TMatSpec;
    I: NativeInt;
    p: PByte;
begin
  aDst := nil;
  if not NDMatrix(aA, spec) then exit(False);
  SetLength(aDst, spec.NRows, spec.NCols);
  p := spec.Data;
  for I := 0 to spec.NRows - 1 do begin
    if spec.NCols > 0 then
      TNDAUt.Copy<T>(p, PByte(@aDst[I][0]), spec.NCols, spec.CStep);
    Inc(p, spec.RStep);
  end;
  Result := True;
end;

function TLinAlgBase<T>.BindDyn(const aA: TArray<TArray<T>>): Boolean;
var I, N, W: Integer;
    rows: TArray<PT>;
begin
  Result := False;
  N := Length(aA);
  if N = 0 then exit;
  W := Length(aA[0]);
  SetLength(rows, N);
  for I := 0 to N - 1 do begin
    if Length(aA[I]) <> W then exit;
    if W > 0 then
      rows[I] := @aA[I][0]
    else
      rows[I] := nil;
  end;

  ClearAnchors;
  fDyn := aA;
  fRows := rows;
  fWidth := W;
  Result := True;
end;

function TLinAlgBase<T>.AliasND(const aA: INDArray<T>; aAugmented: Boolean): Boolean;
var spec: TMatSpec;
    nCols: NativeInt;
    rows: TArray<PT>;
begin
  if not (NDMatrix(aA, spec) and WriteableQ(aA)) then exit(False);

  nCols := spec.NRows;
  if aAugmented then Inc(nCols);

  if not ((spec.NCols = nCols) and RowContiguous(spec)) then exit(False);

  rows := RowPointers(spec);
  ClearAnchors;
  fND := aA;
  fRows := rows;
  fWidth := spec.NCols;
  Result := True;
end;

function TLinAlgBase<T>.RowPointers(const aSpec: TMatSpec): TArray<PT>;
var I: NativeInt;
    p: PByte;
begin
  SetLength(Result, aSpec.NRows);
  p := aSpec.Data;
  for I := 0 to aSpec.NRows - 1 do begin
    Result[I] := PT(p);
    Inc(p, aSpec.RStep);
  end;
end;

function TLinAlgBase<T>.RowPointers(const aA: TArray<TArray<T>>): TArray<PT>;
var I: Integer;
begin
  SetLength(Result, Length(aA));
  for I := 0 to High(aA) do
    if Length(aA[I]) > 0 then
      Result[I] := @aA[I][0]
    else
      Result[I] := nil;
end;

procedure TLinAlgBase<T>.PlaceRows(const aPhys: TArray<PT>; const aP: TArray<NativeInt>;
  aWidth: NativeInt);
var I, K, Src, N: NativeInt;
    tmp: TArray<T>;
    seen: TArray<Boolean>;
begin
  N := Length(aPhys);
  if (N <= 0) or (aWidth <= 0) then exit;
  SetLength(tmp, aWidth);
  SetLength(seen, N);

  for I := 0 to N - 1 do begin
    if seen[I] or (aP[I] = I) then Continue;
    TNDAUt.Copy<T>(PByte(aPhys[I]), PByte(@tmp[0]), aWidth);
    K := I;
    repeat
      seen[K] := True;
      Src := aP[K];
      if Src = I then
        TNDAUt.Copy<T>(PByte(@tmp[0]), PByte(aPhys[K]), aWidth)
      else
        TNDAUt.Copy<T>(PByte(aPhys[Src]), PByte(aPhys[K]), aWidth);
      K := Src;
    until Src = I;
  end;
end;

procedure TLinAlgBase<T>.PlaceDynRows(aRows: TArray<TArray<T>>; const aP: TArray<NativeInt>);
var I, K, Src, N: NativeInt;
    tmp: TArray<T>;
    seen: TArray<Boolean>;
begin
  N := Length(aRows);
  if N <= 0 then exit;
  SetLength(seen, N);

  for I := 0 to N - 1 do begin
    if seen[I] or (aP[I] = I) then Continue;
    tmp := aRows[I];
    K := I;
    repeat
      seen[K] := True;
      Src := aP[K];
      if Src = I then
        aRows[K] := tmp
      else
        aRows[K] := aRows[Src];
      K := Src;
    until Src = I;
  end;
end;

procedure TLinAlgBase<T>.Gather(aSrc: PByte; aStep, aCount: NativeInt; aDst: TArray<T>);
var I: NativeInt;
begin
  if aCount <= 0 then exit;
  if aStep = cElSz then
    TNDAUt.Copy<T>(aSrc, PByte(@aDst[0]), aCount)
  else
    for I := 0 to aCount - 1 do begin
      aDst[I] := PT(aSrc)^;
      Inc(aSrc, aStep);
    end;
end;

procedure TLinAlgBase<T>.Store(const aDst: INDArray<T>; aCol: NativeInt; const aSrc: TArray<T>;
  aVector: Boolean);
var I, N, step: NativeInt;
    p: PByte;
    strides: TArray<NativeInt>;
begin
  N := Length(aSrc);
  if N <= 0 then exit;
  strides := aDst.Strides;
  if aVector then begin
    step := strides[0];
    p := aDst.Data;
  end else begin
    step := strides[0];
    p := aDst.Data + aCol * strides[1];
  end;
  if step = cElSz then
    TNDAUt.Copy<T>(PByte(@aSrc[0]), p, N)
  else
    for I := 0 to N - 1 do begin
      PT(p)^ := aSrc[I];
      Inc(p, step);
    end;
end;

procedure TLinAlgBase<T>.FillZero(var aValues: TArray<T>);
var I: NativeInt;
    Z: T;
begin
  Z := fNumeric.Zero;
  for I := 0 to High(aValues) do
    aValues[I] := Z;
end;

{$endregion}

end.
