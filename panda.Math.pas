unit panda.Math;

interface

uses
    System.Math
  , System.SysUtils
  , panda.Consts
  , panda.Intfs
  , panda.Arrays
  , panda.Nums
{$ifdef BLAS}
  , libCBLAS
{$endif}
  ;

{$I AsmDefs.inc}

type
  TNDAMath = class(TNDAUt)
  public type
    // aRes <- dot({X}, {Y}) = (aRes := 0; for i in (0, N - 1): aRes += X[i]*Y[i])
    TDotFunc = procedure (N: NativeInt; X: PByte; IncX: NativeInt; Y: PByte; IncY: NativeInt; aRes: PByte);
  protected type
    // {Y} <- Outer(x, {Y}) = (for i in (0, N - 1): Y[i] := x*Y[i])
    TOuterProdFunc = procedure (N: NativeInt; X, Y: PByte);
  protected
    class procedure UpdateIdx(const aIdx: INDIndexSeq; const aValues: TArray<NativeInt>; aSkipIdx: Integer); static;

    class procedure DoInnerProd(const aA, aB, aRes: INDArray; aFunc: TDotFunc); static;
    class function MakeInnerProdResBuffer<T>(const aA, aB: INDArray<T>): INDArray<T>; static;
    class function InnerProdShape(const aAShape, aBShape: TArray<NativeInt>): TArray<NativeInt>; static;

    class procedure DoOuterProd(const aA, aRes: INDArray; aFnc: TOuterProdFunc); static;
  public
    class function TotalAtLvl<T>(const aArr: INDArray<T>; aLvl: Integer; aTotFunc: TNDAFuncNS<T>): INDArray<T>; static;
    class function TotalToLvl<T>(const aArr: INDArray<T>; aLvl: Integer; aTotFunc: TNDAFuncNS<T>): INDArray<T>; static;
  end;

function ndaRange(aHi: Integer): INDArray<Integer>; overload;
function ndaRange(aLo, aHi: Integer; aStep: Integer = 1): INDArray<Integer>; overload;
function ndaRange(aHi: Int64): INDArray<Int64>; overload;
function ndaRange(aLo, aHi: Int64; aStep: Int64 = 1): INDArray<Int64>; overload;
function ndaRange(aHi: Single): INDArray<Single>; overload;
function ndaRange(aLo, aHi: Single; aStep: Single = 1): INDArray<Single>; overload;
function ndaRange(aHi: Double): INDArray<Double>; overload;
function ndaRange(aLo, aHi: Double; aStep: Double = 1): INDArray<Double>; overload;

function ndaExp(const aArr: INDArray<Single>): INDArray<Single>;

function ndaSin(const aArr: INDArray<Single>): INDArray<Single>; overload;
function ndaSin(const aArr: INDArray<Double>): INDArray<Double>; overload;
function ndaSin(const aArr: INDArray<TCmplx128>): INDArray<Double>; overload;

/// <summary>
///   Gives the total of the elements in <c>aArr</c>.
/// </summary>
function ndaTotal(const aArr: INDArray<Integer>): INDArray<Integer>; overload;
function ndaTotal(const aArr: INDArray<Single>): INDArray<Single>; overload;
function ndaTotal(const aArr: INDArray<Double>): INDArray<Double>; overload;
/// <summary>
///   Gives totals all elements down to level <c>aLvl</c>.
/// </summary>
function ndaTotalToLvl(const aArr: INDArray<Single>; aLvl: Integer): INDArray<Single>; overload;
function ndaTotalToLvl(const aArr: INDArray<Double>; aLvl: Integer): INDArray<Double>; overload;
/// <summary>
///   Gives totals elements at level <c>aLvl</c>.
/// </summary>
function ndaTotalAtLvl(const aArr: INDArray<Integer>; aLvl: Integer): INDArray<Integer>; overload;
function ndaTotalAtLvl(const aArr: INDArray<Single>; aLvl: Integer): INDArray<Single>; overload;
function ndaTotalAtLvl(const aArr: INDArray<Double>; aLvl: Integer): INDArray<Double>; overload;

/// <summary>
///   Gives a product of vectors, matrices and tensors.
/// </summary>
function ndaDot(const aA, aB: INDArray<Single>): INDArray<Single>; overload;
procedure ndaDot(const aA, aB: INDArray<Single>; var aRes: INDArray<Single>); overload;
function ndaDot(const aA, aB: INDArray<Double>): INDArray<Double>; overload;

function ndaOuter(const aA, aB: INDArray<Single>): INDArray<Single>; overload;
function ndaOuter(const aA, aB: INDArray<Double>): INDArray<Double>; overload;

/// <summary> Returns the full symmetric Gram matrix A^T*A. </summary>
function ndaGram(const aA: INDArray<Double>): INDArray<Double>; overload;
/// <summary> Computes A^T*A, reusing aRes when it is assigned. </summary>
procedure ndaGram(const aA: INDArray<Double>; var aRes: INDArray<Double>); overload;
/// <summary> Computes A^T*A using <c>aAt</c> as a transpose workspace. </summary>
/// <remarks>
///   Allocates unassigned buffers. Assigned aRes and aAt must have shapes W x W
///   and W x H respectively, with contiguous rows, for an H x W input.
///   Uses BLAS dsyrk for compatible buffers when BLAS is defined.
///   The workspace receives the transpose of aA in both native and BLAS paths.
/// </remarks>
procedure ndaGram(const aA: INDArray<Double>; var aRes, aAt: INDArray<Double>); overload;

/// <summary> Returns the full symmetric A^T*A using native parallel evaluation. </summary>
/// <remarks>
///   Returns a newly allocated matrix and uses a temporary transpose workspace.
///   Input and worker-limit rules match the overload with an explicit aAt.
/// </remarks>
function ndaGramParallel(const aA: INDArray<Double>; aMaxWorkers: Integer = 0): INDArray<Double>; overload;
/// <summary> Computes A^T*A in parallel, allocating aRes when nil and otherwise reusing it. </summary>
/// <remarks>
///   Uses a temporary transpose workspace. Buffer and worker-limit rules match
///   the overload with an explicit aAt workspace.
/// </remarks>
procedure ndaGramParallel(const aA: INDArray<Double>; var aRes: INDArray<Double>;
  aMaxWorkers: Integer = 0); overload;
/// <summary> Computes the full symmetric A^T*A using balanced upper-triangular tile jobs. </summary>
/// <remarks>
///   For an H x W input, allocates unassigned aRes (W x W) and aAt (W x H).
///   Assigned shapes and contiguous rows are checked with assertions. Result rows
///   must occupy disjoint storage; padding and reversed row order are allowed.
///   Result and workspace storage must not overlap the input or each other.
///   Transposes aA into aAt once before starting jobs. Input and buffers must not
///   be changed by the caller until this call returns; all jobs complete first.
///   Always uses native kernels, even when BLAS is defined. Zero aMaxWorkers uses
///   TThread.ProcessorCount; one executes serially. Jobs are also limited by the
///   number of upper-triangular tiles. Small matrices may be slower in parallel.
///   Raises EArgumentOutOfRangeException for a negative worker limit and
///   ENDAShapeError for a nil or nonmatrix input.
/// </remarks>
procedure ndaGramParallel(const aA: INDArray<Double>; var aRes, aAt: INDArray<Double>;
  aMaxWorkers: Integer = 0); overload;

/// <summary> Returns A*B for matrices A (M x K) and B (K x N). </summary>
/// <remarks>
///   Returns a newly allocated M x N matrix. Uses a temporary transpose of B
///   and copies A temporarily when its rows are not contiguous.
///   Raises ENDAShapeError for nil/nonmatrix inputs or incompatible dimensions.
/// </remarks>
function ndaMatMul(const aA, aB: INDArray<Double>): INDArray<Double>; overload;
/// <summary> Computes A*B, allocating aRes when nil and otherwise reusing it. </summary>
/// <remarks>
///   Uses a temporary transpose workspace. Input, result and layout requirements
///   are the same as for the overload with an explicit aBt workspace.
/// </remarks>
procedure ndaMatMul(const aA, aB: INDArray<Double>; var aRes: INDArray<Double>); overload;
/// <summary> Computes A*B using <c>aBt</c> as a transpose workspace. </summary>
/// <remarks>
///   For A (M x K) and B (K x N), allocates unassigned aRes (M x N) and aBt (N x K).
///   Assigned buffers are reused without resizing; their shapes and contiguous
///   rows are checked with assertions. Padded and reversed row order are allowed.
///   Result and workspace storage must not overlap either input or each other.
///   Refreshes aBt with the transpose of B on every call. Copies A temporarily
///   only when its rows are not contiguous. Uses BLAS dgemm for compatible
///   buffers when BLAS is defined, otherwise uses native dot kernels.
///   Raises ENDAShapeError for nil/nonmatrix inputs or incompatible dimensions.
/// </remarks>
procedure ndaMatMul(const aA, aB: INDArray<Double>; var aRes, aBt: INDArray<Double>); overload;

/// <summary> Returns A*B using native kernels and at most aMaxWorkers parallel jobs. </summary>
/// <remarks>
///   Returns a newly allocated matrix. Uses the same input and temporary buffer
///   rules as ndaMatMulParallel with an explicit transpose workspace.
/// </remarks>
function ndaMatMulParallel(const aA, aB: INDArray<Double>; aMaxWorkers: Integer = 0): INDArray<Double>; overload;
/// <summary> Computes A*B in parallel, allocating aRes when nil and otherwise reusing it. </summary>
/// <remarks>
///   Uses a temporary transpose workspace. Input, result and worker-limit rules
///   are the same as for the overload with an explicit aBt workspace.
/// </remarks>
procedure ndaMatMulParallel(const aA, aB: INDArray<Double>; var aRes: INDArray<Double>;
  aMaxWorkers: Integer = 0); overload;
/// <summary> Computes A*B with native kernels using disjoint groups of output rows. </summary>
/// <remarks>
///   For A (M x K) and B (K x N), allocates unassigned aRes (M x N) and aBt (N x K).
///   Assigned shapes and contiguous rows are checked with assertions. Output rows
///   must occupy disjoint storage; padding and reversed row order are allowed.
///   Result and workspace storage must not overlap either input or each other.
///   Packs A when needed and transposes B into aBt once before starting jobs.
///   Inputs and buffers must not be changed by the caller until this call returns.
///   Waits for all jobs to complete. Uses native kernels even when BLAS is defined.
///   Zero aMaxWorkers selects TThread.ProcessorCount; one executes serially.
///   The number of jobs is also limited by the number of output row pairs,
///   including a final odd row. Small products may be slower with parallel jobs.
///   Raises EArgumentOutOfRangeException for a negative worker limit and
///   ENDAShapeError for nil/nonmatrix inputs or incompatible dimensions.
/// </remarks>
procedure ndaMatMulParallel(const aA, aB: INDArray<Double>; var aRes, aBt: INDArray<Double>;
  aMaxWorkers: Integer = 0); overload;

implementation

uses
    panda.cvMath
  , panda.cvArithmetic
  , panda.Arithmetic
  , panda.ArrManip
  , panda.DynArrayUtils
  , System.Classes
  , System.Threading
  ;

{$EXCESSPRECISION OFF} // to prevent Single -> Double conversion by x64 compiler

{$region 'TNDAMath'}

class procedure TNDAMath.UpdateIdx(const aIdx: INDIndexSeq; const aValues: TArray<NativeInt>; aSkipIdx: Integer);
var I: Integer;
begin
  Assert(Length(aValues) = Length(aIdx) - 1);
  for I := 0 to aSkipIdx - 1 do
    PNativeInt(aIdx[I].RawIndexData)^ := aValues[I];
  for I := aSkipIdx + 1 to High(aIdx) do
    PNativeInt(aIdx[I].RawIndexData)^ := aValues[I - 1];
end;

class function TNDAMath.TotalAtLvl<T>(const aArr: INDArray<T>; aLvl: Integer; aTotFunc: TNDAFuncNS<T>): INDArray<T>;
var shape: TNDAShape;
    axes: TArray<Integer>;
    view: INDArray<T>;
    viewEntry: TVecView<T>;
    itA, itRes: TNDAIt;
    s: NativeInt;
    I: Integer;
    tot: T;
begin
  if aArr.NDim = 1 then begin
    Assert((aLvl = 0) or (aLvl = -1));
    aTotFunc(aArr, tot);
    Result := TNDScalar<T>.Create(tot);
    exit;
  end;

  if aLvl < 0 then
    aLvl := aArr.NDim + aLvl;
  shape := TDynAUt.Drop<NativeInt>(aArr.Shape, [aLvl]);
  Result := TNDABuffer<T>.Create(shape);
  s := aArr.Strides[aLvl];
  SetLength(axes, aArr.NDim - 1);
  for I := 0 to aLvl - 1 do
    axes[I] := I;
  for I := aLvl to High(axes) do
    axes[I] := I + 1;

  itRes := TNDAIt.Create(Result);
  itA := TNDAIt.Create(aArr, 0, aArr.NDim - 2, axes);
  try
    viewEntry := TVecView<T>.Create(aArr.Data, aArr.Shape[aLvl], s);
    view := viewEntry;
    while itRes.MoveNext do begin
      itA.MoveNext;
      viewEntry.SetData(itA.Current);
      aTotFunc(view, tot);
      TNDA<T>.PT(itRes.Current)^ := tot;
    end;
  finally
    itRes.Free;
    itA.Free;
  end;
end;

class function TNDAMath.TotalToLvl<T>(const aArr: INDArray<T>; aLvl: Integer; aTotFunc: TNDAFuncNS<T>): INDArray<T>;
var tmp: INDArray<T>;
begin
  aLvl := Min(aLvl, aArr.NDim - 1);
  tmp := aArr;
  while aLvl >= 0  do begin
    tmp := TotalAtLvl<T>(tmp, 0, aTotFunc);
    Dec(aLvl);
  end;
  Result := tmp;
end;

class function TNDAMath.InnerProdShape(const aAShape, aBShape: TNDAShape): TNDAShape;
var I, nDimA, nDimB: Integer;
begin
  nDimA := Length(aAShape);
  nDimB := Length(aBShape);
  Assert((nDimA > 0) and (nDimB > 0) and (aAShape[nDimA - 1] = aBShape[0]));
  SetLength(Result, nDimA + nDimB - 2);
  for I := 0 to nDimA - 2 do
    Result[I] := aAShape[I];
  for I := 1 to nDimB - 1 do
    Result[nDimA + I - 2] := aBShape[I];
end;

class function TNDAMath.MakeInnerProdResBuffer<T>(const aA, aB: INDArray<T>): INDArray<T>;
var shRes: TNDAShape;
begin
  shRes := InnerProdShape(aA.Shape, aB.Shape);
  if Length(shRes) = 0 then
    Result := TNDScalar<T>.Create(Default(T))
  else
    Result := TNDABuffer<T>.Create(shRes);
end;

class procedure TNDAMath.DoInnerProd(const aA, aB, aRes: INDArray; aFunc: TDotFunc);
var shA, shB, shRes: TNDAShape;
    axes: TArray<Integer>;
    aInc, bInc, resInc, count: NativeInt;
    bElSz: NativeInt;
    I, nDimA, nDimB, nDim: Integer;
    itA, itB, itRes: TNDAIt;
    buff: TBytes;
    pRes, pBuff: PByte;
begin
  shA := aA.Shape;
  shB := aB.Shape;
  shRes := aRes.Shape;
  nDimA := Length(shA);
  nDimB := Length(shB);
  nDim := Length(shRes);
  Assert(
    (nDimA > 0) and (nDimB > 0) and (shA[nDimA - 1] = shB[0]) and
    (nDim = nDimA + nDimB - 2) and
    CContiguousQ(aRes)
  );

  count := shB[0];
  aInc := aA.Strides[nDimA - 1];
  bInc := aB.Strides[0];
  if nDim = 0 then begin // nDimA = nDimB = 1
    aFunc(count, aA.Data, aInc, aB.Data, bInc, aRes.Data);
    exit;
  end;

  if nDimB = 1 then begin
    Assert(SameUpToQ(shA, shRes, nDimA - 2));
    pRes := aRes.Data;
    resInc := aRes.ItemSize;

    itA := TNDAIt.Create(aA, nDimA - 2);
    try
      pBuff := GetPackedDataPtr(aB, buff);
      bInc := aB.ItemSize;
      while itA.MoveNext do begin
        aFunc(count, itA.Current, aInc, pBuff, bInc, pRes);
        Inc(pRes, resInc);
      end;
    finally
      itA.Free;
    end;
    exit;
  end;

  if nDimA = 1 then begin
    Assert(SameQ(TDynAUt.Drop<NativeInt>(shB, 1), shRes));
    pRes := aRes.Data;
    resInc := aRes.ItemSize;

    itB := TNDAIt.Create(aB, 1, -1);
    try
      pBuff := GetPackedDataPtr(aA, buff);
      aInc := aA.ItemSize;
      while itB.MoveNext do begin
        aFunc(count, pBuff, aInc, itB.Current, bInc, pRes);
        Inc(pRes, resInc);
      end;
    finally
      itB.Free;
    end;
    exit;
  end;

  Assert(SameQ(shRes, InnerProdShape(shA, shB)));
  itA := TNDAIt.Create(aA, nDimA - 2);
  itB := TNDAIt.Create(aB, 1, -1);
  axes := TDynAUt.Range_I32(aRes.NDim - 1, 0, -1);
  axes := TArray<Integer>.Create(1, 2, 0);
  SetLength(axes, nDim);
  for I := 0 to nDim - 1 do
    axes[I] := (I + nDimA - 1) mod nDim;
  itRes := TNDAIt.Create(aRes, 0, -1, axes);
  try
    bElSz := aB.ItemSize;
    SetLength(buff, count * bElSz);
    while itB.MoveNext do begin
      PackBytes(itB.Current, PByte(buff), count, bInc, bElSz);
      while itA.MoveNext do begin
        itRes.MoveNext;
        aFunc(count, itA.Current, aInc, PByte(buff), bElSz, itRes.Current);
      end;
      itA.Reset;
    end;
  finally
    itA.Free;
    itB.Free;
    itRes.Free;
  end;
end;

class procedure TNDAMath.DoOuterProd(const aA, aRes: INDArray; aFnc: TOuterProdFunc);
var shA, shRes: TNDAShape;
    I, nDimA, nDimRes, N: NativeInt;
    itA, itRes: TNDAIt;
begin
  shA := aA.Shape;
  shRes := aRes.Shape;
  nDimA := Length(shA);
  nDimRes := Length(shRes);
  Assert((nDimA > 0) and (nDimRes > nDimA) and CContiguousQ(aRes));

  N := shRes[nDimA];
  for I := nDimA + 1 to nDimRes - 1 do
    N := N * shRes[I];

  itA := TNDAIt.Create(aA);
  itRes := TNDAIt.Create(aRes, nDimA - 1);
  try
    while itA.MoveNext do begin
      itRes.MoveNext;
      aFnc(N, itA.Current, itRes.Current);
    end;
  finally
    itA.Free;
    itRes.Free;
  end;
end;

{$endregion}

{$region 'ndaRange'}

function ndaRange(aHi: Integer): INDArray<Integer>;
begin
  Result := NDARange(0, aHi);
end;

function ndaRange(aLo, aHi, aStep: Integer): INDArray<Integer>;
var count: NativeInt;
begin
  Assert(aStep <> 0);
  count := Max(0, (aHi - aLo) div aStep);
  Result := TNDABuffer<Integer>.Create([count]);
  if count > 0 then
    cvRange(PInteger(Result.Data), count, aLo, aStep);
end;

function ndaRange(aHi: Int64): INDArray<Int64>;
begin
  Result := ndaRange(0, aHi);
end;

function ndaRange(aLo, aHi, aStep: Int64): INDArray<Int64>;
var count: NativeInt;
begin
  Assert(aStep <> 0);
  count := Max(0, (aHi - aLo) div aStep);
  Result := TNDABuffer<Int64>.Create([count]);
  if count > 0 then
    cvRange(PInt64(Result.Data), count, aLo, aStep);
end;

function ndaRange(aHi: Single): INDArray<Single>;
begin
  Result := ndaRange(0, aHi);
end;

function ndaRange(aLo, aHi, aStep: Single): INDArray<Single>;
var count: NativeInt;
begin
  Assert(aStep <> 0);
  count := Max(0, Floor((aHi - aLo) / aStep));
  Result := TNDABuffer<Single>.Create([count]);
  if count > 0 then
    cvRange(PSingle(Result.Data), count, aLo, aStep);
end;

function ndaRange(aHi: Double): INDArray<Double>;
begin
  Result := ndaRange(0, aHi);
end;

function ndaRange(aLo, aHi: Double; aStep: Double = 1): INDArray<Double>;
var count: NativeInt;
begin
  Assert(aStep <> 0);
  count := Max(0, Floor((aHi - aLo) / aStep));
  Result := TNDABuffer<Double>.Create([count]);
  if count > 0 then
    cvRange(PDouble(Result.Data), count, aLo, aStep);
end;

{$endregion}

{$region 'ndaExp'}

function ndaExp(const aArr: INDArray<Single>): INDArray<Single>;
begin
  Result := TNDAUt.Copy<Single>(aArr);
  cvExp(PSingle(Result.Data), aArr.Size);
end;

{$endregion}

{$region 'ndaSin'}

function ndaSin(const aArr: INDArray<Single>): INDArray<Single>;
begin

end;

function ndaSin(const aArr: INDArray<Double>): INDArray<Double>;
begin
  Sin(PDouble(aArr.Data)^);
end;

function ndaSin(const aArr: INDArray<TCmplx128>): INDArray<Double>;
begin

end;

{$endregion}

{$region 'ndaTotal'}

function ndaTotal(const aArr: INDArray<Integer>): INDArray<Integer>;
begin
  Result := ndaTotalAtLvl(aArr, 0);
end;

function ndaTotal(const aArr: INDArray<Single>): INDArray<Single>;
begin
  Result := ndaTotalAtLvl(aArr, 0);
end;

function ndaTotal(const aArr: INDArray<Double>): INDArray<Double>;
begin
  Result := ndaTotalAtLvl(aArr, 0);
end;

function _TotalI32(aData: PByte; aStep, aCount: NativeInt): Integer;
var pEnd: PByte;
begin
  Result := 0;
  pEnd := aData + aStep * aCount;
  while aData < pEnd do begin
    Inc(Result, PInteger(aData)^);
    Inc(aData, aStep);
  end;
end;

procedure Total(const aArr: INDArray<Integer>; var aRes: Integer); overload;
var it: TNDAIt;
    sz: NativeInt;
    lvl: Integer;
begin
  if CContiguousQ(aArr) then begin
    aRes := cvTotal(PInteger(aArr.Data), aArr.Size);
    exit
  end;

  if aArr.NDim = 1 then begin
    aRes := _TotalI32(aArr.Data, aArr.Strides[0], aArr.Size);
    exit;
  end;

  aRes := 0;
  lvl := GetCContLvl(aArr, sz);
  it := TNDAIt.Create(aArr, lvl - 1);
  try
    if sz = SizeOf(Integer) then begin
      while it.MoveNext do
        Inc(aRes, PInteger(it.Current)^);
    end else begin
      sz := sz div SizeOf(Integer);
      while it.MoveNext do
        Inc(aRes, cvTotal(PInteger(it.Current), sz));
    end;
  finally
    it.Free;
  end;
end;

function _TotalF32(aData: PByte; aStep, aCount: NativeInt): Single;
var pEnd: PByte;
begin
  Result := 0;
  pEnd := aData + aStep * aCount;
  while aData < pEnd do begin
    Result := Result + PSingle(aData)^;
    Inc(aData, aStep);
  end;
end;

procedure Total(const aArr: INDArray<Single>; var aRes: Single); overload;
var it: TNDAIt;
    sz: NativeInt;
    lvl: Integer;
begin
  if CContiguousQ(aArr) then begin
    aRes := cvTotal(PSingle(aArr.Data), aArr.Size);
    exit;
  end;

  if aArr.NDim = 1 then begin
    aRes := _TotalF32(aArr.Data, aArr.Strides[0], aArr.Size);
    exit;
  end;

  aRes := 0;
  lvl := GetCContLvl(aArr, sz);
  it := TNDAIt.Create(aArr, lvl - 1);
  try
    if sz = SizeOf(Single) then begin
      while it.MoveNext do
        aRes := aRes + PSingle(it.Current)^;
    end else begin
      sz := sz div SizeOf(Single);
      while it.MoveNext do
        aRes := aRes + cvTotal(PSingle(it.Current), sz);
    end;
  finally
    it.Free;
  end;
end;

function _TotalF64(aData: PByte; aStep, aCount: NativeInt): Double;
var pEnd: PByte;
begin
  Result := 0;
  pEnd := aData + aStep * aCount;
  while aData < pEnd do begin
    Result := Result + PDouble(aData)^;
    Inc(aData, aStep);
  end;
end;

procedure Total(const aArr: INDArray<Double>; var aRes: Double); overload;
var it: TNDAIt;
    sz: NativeInt;
    lvl: Integer;
begin
  if CContiguousQ(aArr) then begin
    aRes := cvTotal(PDouble(aArr.Data), aArr.Size);
    exit;
  end;

  if aArr.NDim = 1 then begin
    aRes := _TotalF64(aArr.Data, aArr.Strides[0], aArr.Size);
    exit;
  end;

  aRes := 0;
  lvl := GetCContLvl(aArr, sz);
  it := TNDAIt.Create(aArr, lvl - 1);
  try
    if sz = SizeOf(Double) then begin
      while it.MoveNext do
        aRes := aRes + PDouble(it.Current)^;
    end else begin
      sz := sz div SizeOf(Double);
      while it.MoveNext do
        aRes := aRes + cvTotal(PDouble(it.Current), sz);
    end;
  finally
    it.Free;
  end;
end;

function ndaTotalToLvl(const aArr: INDArray<Single>; aLvl: Integer): INDArray<Single>;
begin
  Result := TNDAMath.TotalToLvl<Single>(aArr, aLvl, Total);
end;

function ndaTotalToLvl(const aArr: INDArray<Double>; aLvl: Integer): INDArray<Double>;
begin
  Result := TNDAMath.TotalToLvl<Double>(aArr, aLvl, Total);
end;

function ndaTotalAtLvl(const aArr: INDArray<Integer>; aLvl: Integer): INDArray<Integer>;
begin
  Result := TNDAMath.TotalAtLvl<Integer>(aArr, aLvl, Total);
end;

function ndaTotalAtLvl(const aArr: INDArray<Single>; aLvl: Integer): INDArray<Single>;
begin
  Result := TNDAMath.TotalAtLvl<Single>(aArr, aLvl, Total);
end;

function ndaTotalAtLvl(const aArr: INDArray<Double>; aLvl: Integer): INDArray<Double>;
begin
  Result := TNDAMath.TotalAtLvl<Double>(aArr, aLvl, Total);
end;

{$endregion}

{$region 'ndaDot'}

{$ifdef BLAS}

procedure sdot(aN: NativeInt; pX: PByte; aXInc: NativeInt; pY: PByte; aYInc: NativeInt; aRes: PByte);
begin
  PSingle(aRes)^ := cblas.sdot(aN, PSingle(pX), aXInc shr 2, PSingle(pY), aYInc shr 2);
end;

procedure ddot(aN: NativeInt; pX: PByte; aXInc: NativeInt; pY: PByte; aYInc: NativeInt; aRes: PByte);
begin
  PDouble(aRes)^ := cblas.ddot(aN, PDouble(pX), aXInc shr 3, PDouble(pY), aYInc shr 3);
end;

{$else}

procedure sdot(N: NativeInt; X: PByte; IncX: NativeInt; Y: PByte; IncY: NativeInt; aRes: PByte);
begin
  PSingle(aRes)^ := panda.cvArithmetic.sdot(N, X, IncX, Y, IncY);
end;

procedure ddot(N: NativeInt; X: PByte; IncX: NativeInt; Y: PByte; IncY: NativeInt; aRes: PByte);
begin
  PDouble(aRes)^ := panda.cvArithmetic.ddot(N, X, IncX, Y, IncY);
end;

{$endif}

function ndaDot(const aA, aB: INDArray<Single>): INDArray<Single>;
begin
  Result := TNDAMath.MakeInnerProdResBuffer<Single>(aA, aB);
  TNDAMath.DoInnerProd(aA, aB, Result, sdot)
end;

procedure ndaDot(const aA, aB: INDArray<Single>; var aRes: INDArray<Single>);
begin
  TNDAMath.DoInnerProd(aA, aB, aRes, sdot);
end;

function ndaDot(const aA, aB: INDArray<Double>): INDArray<Double>;
begin
  Result := TNDAMath.MakeInnerProdResBuffer<Double>(aA, aB);
  TNDAMath.DoInnerProd(aA, aB, Result, ddot);
end;

{$endregion}

{$region 'ndaOuter'}

procedure souter_vloop(N: NativeInt; X, Y: PByte);
begin
  sscal(PSingle(Y), N, PSingle(X)^);
end;

procedure douter_vloop(N: NativeInt; X, Y: PByte);
begin
  dscal(PDouble(Y), N, PDouble(X)^);
end;

function ndaOuter(const aA, aB: INDArray<Single>): INDArray<Single>;
begin
  Result := TNDAUt.Full<Single>(TDynAUt.Concat<NativeInt>(aA.Shape, aB.Shape), aB);
  TNDAMath.DoOuterProd(aA, Result, souter_vloop);
end;

function ndaOuter(const aA, aB: INDArray<Double>): INDArray<Double>;
begin
  Result := TNDAUt.Full<Double>(TDynAUt.Concat<NativeInt>(aA.Shape, aB.Shape), aB);
  TNDAMath.DoOuterProd(aA, Result, souter_vloop);
end;

{$endregion}

{$region 'ndaGram'}

function ndaGram(const aA: INDArray<Double>): INDArray<Double>;
var At: INDArray<Double>;
begin
  ndaGram(aA, Result, At);
end;

procedure ndaGram(const aA: INDArray<Double>; var aRes: INDArray<Double>);
var At: INDArray<Double>;
begin
  ndaGram(aA, aRes, At);
end;

procedure PrepareGram(const aA: INDArray<Double>; var aRes, aAt: INDArray<Double>;
  aParallel: Boolean = False);
var sh: TNDAShape;
    w, h: NativeInt;
begin
  if not (Assigned(aA) and MatrixQ(aA)) then
    raise ENDAShapeError.CreateFmt(csInvArrayDim, [2]);

  sh := aA.Shape;
  w := sh[1];
  h := sh[0];

  if not Assigned(aRes) then
    aRes := TNDAUt.Empty<Double>([w, w]);
  if not Assigned(aAt) then
    aAt := TNDAUt.Empty<Double>([w, h]);

  Assert(
    MatrixQ(aRes) and (aRes.Shape[0] = w) and (aRes.Shape[1] = w) and
    MatrixQ(aAt) and (aAt.Shape[0] = w) and (aAt.Shape[1] = h) and
    (aAt.Strides[1] = cF64Sz)
  );

  // The blocked transpose assumes forward source rows when columns are contiguous.
  if aParallel and (aA.Strides[0] < 0) and (aA.Strides[1] = cF64Sz) then
    TNDAUt.Fill<Double>(aAt, TNDAMan.TransposedView<Double>(aA))
  else
    TNDAMan.Transpose<Double>(aA, aAt, [1, 0]);
end;

function GramTileRows(aWidth: NativeInt): NativeInt;
begin
{$if defined(ASMx64)}
  Result := aWidth div 2 + (aWidth and 1);
{$else}
  Result := aWidth;
{$endif}
end;

function GramTriangleCount(aRows: Int64): Int64;
begin
  if (aRows and 1) = 0 then
    Result := (aRows div 2)*(aRows + 1)
  else
    Result := aRows*(aRows div 2 + 1);
end;

function GramTilePrefix(aRows, aRow: NativeInt): Int64;
begin
  Result := GramTriangleCount(aRows) - GramTriangleCount(aRows - aRow);
end;

procedure GramNative(const aAt, aRes: INDArray<Double>; aFirst, aCount: Int64);
var w, h, b, lo, hi, mid, blockRow, blockCol, I, J: NativeInt;
    srcWStep, dstWStep: NativeInt;
    pSrc, pDst: PByte;
    d00: Double;
{$if defined(ASMx64)}
    d01, d10, d11: Double;
{$endif}
begin
  w := aAt.Shape[0];
  h := aAt.Shape[1];
  b := GramTileRows(w);
  srcWStep := aAt.Strides[0];
  dstWStep := aRes.Strides[0];
  pSrc := aAt.Data;
  pDst := aRes.Data;

  // Find the first tile once, then walk the triangular range sequentially.
  lo := 0;
  hi := b - 1;
  while lo < hi do begin
    mid := lo + (hi - lo + 1) div 2;
    if GramTilePrefix(b, mid) <= aFirst then
      lo := mid
    else
      hi := mid - 1;
  end;
  blockRow := lo;
  blockCol := blockRow + NativeInt(aFirst - GramTilePrefix(b, blockRow));
  while aCount > 0 do begin
{$if defined(ASMx64)}
    I := blockRow*2;
    J := blockCol*2;
    if (I + 1 < w) and (J + 1 < w) then begin
      dot2x2(
        PDouble(pSrc + I*srcWStep), PDouble(pSrc + (I+1)*srcWStep),
        PDouble(pSrc + J*srcWStep), PDouble(pSrc + (J+1)*srcWStep),
        h, d00, d01, d10, d11
      );
      PDouble(pDst +     I*dstWStep +     J*cF64Sz)^ := d00;
      PDouble(pDst +     I*dstWStep + (J+1)*cF64Sz)^ := d01;
      PDouble(pDst + (I+1)*dstWStep +     J*cF64Sz)^ := d10;
      PDouble(pDst + (I+1)*dstWStep + (J+1)*cF64Sz)^ := d11;

      PDouble(pDst +     J*dstWStep +     I*cF64Sz)^ := d00;
      PDouble(pDst +     J*dstWStep + (I+1)*cF64Sz)^ := d10;
      PDouble(pDst + (J+1)*dstWStep +     I*cF64Sz)^ := d01;
      PDouble(pDst + (J+1)*dstWStep + (I+1)*cF64Sz)^ := d11;
    end else begin
      dot(PDouble(pSrc + I*srcWStep), PDouble(pSrc + J*srcWStep), h, d00);
      PDouble(pDst + I*dstWStep + J*cF64Sz)^ := d00;
      PDouble(pDst + J*dstWStep + I*cF64Sz)^ := d00;
      if I + 1 < w then begin
        dot(PDouble(pSrc + (I+1)*srcWStep), PDouble(pSrc + J*srcWStep), h, d01);
        PDouble(pDst + (I+1)*dstWStep + J*cF64Sz)^ := d01;
        PDouble(pDst + J*dstWStep + (I+1)*cF64Sz)^ := d01;
      end;
    end;
{$else}
    I := blockRow;
    J := blockCol;
    dot(PDouble(pSrc + I*srcWStep), PDouble(pSrc + J*srcWStep), h, d00);
    PDouble(pDst + I*dstWStep + J*cF64Sz)^ := d00;
    PDouble(pDst + J*dstWStep + I*cF64Sz)^ := d00;
{$endif}
    Dec(aCount);
    Inc(blockCol);
    if blockCol = b then begin
      Inc(blockRow);
      blockCol := blockRow;
    end;
  end;
end;

procedure ndaGram(const aA: INDArray<Double>; var aRes, aAt: INDArray<Double>);
{$ifdef BLAS}
var w, h, I, J: NativeInt;
    srcWStep, dstWStep: NativeInt;
    pSrc, pDst: PByte;
{$endif}
begin
  PrepareGram(aA, aRes, aAt);
{$ifdef BLAS}
  w := aAt.Shape[0];
  h := aAt.Shape[1];
  srcWStep := aAt.Strides[0];
  dstWStep := aRes.Strides[0];
  pSrc := aAt.Data;
  pDst := aRes.Data;
  // BLAS leading dimensions are positive element counts represented by LongInt.
  if 
    (srcWStep >= h*cF64Sz) and (dstWStep >= w*cF64Sz) and
{$if SizeOf(NativeInt) > SizeOf(LongInt)}
    (w <= High(LongInt)) and (h <= High(LongInt)) and
    (srcWStep div cF64Sz <= High(LongInt)) and
    (dstWStep div cF64Sz <= High(LongInt)) and
{$endif}
    (srcWStep mod cF64Sz = 0) and (dstWStep mod cF64Sz = 0) 
  then begin
    cblas.dsyrk(cblasRowMajor, cblasUpper, cblasNoTrans,
      w, h, 1, PDouble(pSrc), srcWStep div cF64Sz,
      0, PDouble(pDst), dstWStep div cF64Sz
    );
    for I := 0 to w - 1 do
      for J := I + 1 to w - 1 do
        PDouble(pDst + J*dstWStep + I*cF64Sz)^ := PDouble(pDst + I*dstWStep + J*cF64Sz)^;
    
    exit;
  end;
{$endif}
  GramNative(aAt, aRes, 0, GramTriangleCount(GramTileRows(aAt.Shape[0])));
end;

function ndaGramParallel(const aA: INDArray<Double>; aMaxWorkers: Integer): INDArray<Double>;
var At: INDArray<Double>;
begin
  ndaGramParallel(aA, Result, At, aMaxWorkers);
end;

procedure ndaGramParallel(const aA: INDArray<Double>; var aRes: INDArray<Double>;
  aMaxWorkers: Integer);
var At: INDArray<Double>;
begin
  ndaGramParallel(aA, aRes, At, aMaxWorkers);
end;

procedure ndaGramParallel(const aA: INDArray<Double>; var aRes, aAt: INDArray<Double>;
  aMaxWorkers: Integer);
type TJob = record
    First, Count: Int64;
  end;
var At, G: INDArray<Double>;
    jobs: TArray<TJob>;
    tileCount, first, count: Int64;
    w: NativeInt;
    jobCount, I: Integer;
begin
  if aMaxWorkers < 0 then
    raise EArgumentOutOfRangeException.Create(csArgumentOutOfRange);

  PrepareGram(aA, aRes, aAt, True);
  w := aAt.Shape[0];
  Assert((aRes.Strides[1] = cF64Sz) and
    ((w = 1) or (Abs(aRes.Strides[0]) >= w*cF64Sz)));

  if aMaxWorkers = 0 then
    aMaxWorkers := TThread.ProcessorCount;
  tileCount := GramTriangleCount(GramTileRows(w));
  jobCount := Max(aMaxWorkers, 1);
  if tileCount < jobCount then
    jobCount := Integer(tileCount);
  if jobCount = 1 then begin
    GramNative(aAt, aRes, 0, tileCount);
    exit;
  end;

  At := aAt;
  G := aRes;
  SetLength(jobs, jobCount);
  first := 0;
  for I := 0 to jobCount - 1 do begin
    count := tileCount div jobCount;
    if I < tileCount mod jobCount then
      Inc(count);
    jobs[I].First := first;
    jobs[I].Count := count;
    Inc(first, count);
  end;
  TParallel.For(0, jobCount - 1,
    procedure (aJob: Integer)
    begin
      GramNative(At, G, jobs[aJob].First, jobs[aJob].Count);
    end
  );
end;

{$endregion}

{$region 'ndaMatMul'}

function ndaMatMul(const aA, aB: INDArray<Double>): INDArray<Double>;
var Bt: INDArray<Double>;
begin
  ndaMatMul(aA, aB, Result, Bt);
end;

procedure ndaMatMul(const aA, aB: INDArray<Double>; var aRes: INDArray<Double>);
var Bt: INDArray<Double>;
begin
  ndaMatMul(aA, aB, aRes, Bt);
end;

function PrepareMatMul(const aA, aB: INDArray<Double>; var aRes, aBt: INDArray<Double>): INDArray<Double>;
var shA, shB: TNDAShape;
    m, n, k: NativeInt;
begin
  if not (Assigned(aA) and Assigned(aB)) then
    raise ENDAShapeError.CreateFmt(csInvArrayDim, [2]);
  if not (MatrixQ(aA) and MatrixQ(aB)) then
    raise ENDAShapeError.CreateFmt(csInvArrayDim, [2]);

  shA := aA.Shape;
  shB := aB.Shape;
  m := shA[0];
  k := shA[1];
  n := shB[1];
  if k <> shB[0] then
    raise ENDAShapeError.Create('Matrix multiplication error: inner dimensions don''t match.');

  if not Assigned(aRes) then
    aRes := TNDAUt.Empty<Double>([m, n]);
  if not Assigned(aBt) then
    aBt := TNDAUt.Empty<Double>([n, k]);

  Assert(
    MatrixQ(aRes) and (aRes.Shape[0] = m) and (aRes.Shape[1] = n) and
    MatrixQ(aBt) and (aBt.Shape[0] = n) and (aBt.Shape[1] = k) and
    (aRes.Strides[1] = cF64Sz) and (aBt.Strides[1] = cF64Sz)
  );

  Result := aA;
  if Result.Strides[1] <> cF64Sz then
    Result := TNDAUt.Copy<Double>(Result);
  TNDAMan.Transpose<Double>(aB, aBt, [1, 0]);
end;

procedure MatMulNative(const aA, aBt, aRes: INDArray<Double>);
var m, n, k, I, J: NativeInt;
    aStep, btStep, dstStep: NativeInt;
    pA, pBt, pDst: PByte;
{$if defined(ASMx64)}
    m2, n2: NativeInt;
    d00, d01, d10, d11: Double;
{$endif}
begin
  m := aA.Shape[0];
  k := aA.Shape[1];
  n := aBt.Shape[0];
  aStep := aA.Strides[0];
  btStep := aBt.Strides[0];
  dstStep := aRes.Strides[0];
  pA := aA.Data;
  pBt := aBt.Data;
  pDst := aRes.Data;

{$if defined(ASMx64)}
  m2 := (m and not(NativeUInt(1)));
  n2 := (n and not(NativeUInt(1)));
  I := 0;
  while I < m2 do begin
    J := 0;
    while J < n2 do begin
      dot2x2(
        PDouble(pA + I*aStep), PDouble(pA + (I+1)*aStep),
        PDouble(pBt + J*btStep), PDouble(pBt + (J+1)*btStep),
        k, d00, d01, d10, d11
      );
      PDouble(pDst +     I*dstStep +     J*cF64Sz)^ := d00;
      PDouble(pDst +     I*dstStep + (J+1)*cF64Sz)^ := d01;
      PDouble(pDst + (I+1)*dstStep +     J*cF64Sz)^ := d10;
      PDouble(pDst + (I+1)*dstStep + (J+1)*cF64Sz)^ := d11;
      Inc(J, 2);
    end;
    if (n and 1) <> 0 then begin
      dot(PDouble(pA + I*aStep), PDouble(pBt + n2*btStep), k, d00);
      PDouble(pDst + I*dstStep + n2*cF64Sz)^ := d00;
      dot(PDouble(pA + (I+1)*aStep), PDouble(pBt + n2*btStep), k, d10);
      PDouble(pDst + (I+1)*dstStep + n2*cF64Sz)^ := d10;
    end;
    Inc(I, 2);
  end;
  if (m and 1) <> 0 then
    for J := 0 to n - 1 do
      dot(PDouble(pA + m2*aStep), PDouble(pBt + J*btStep), k,
        PDouble(pDst + m2*dstStep + J*cF64Sz)^);
{$else}
  for I := 0 to m - 1 do
    for J := 0 to n - 1 do
      dot(PDouble(pA + I*aStep), PDouble(pBt + J*btStep), k,
        PDouble(pDst + I*dstStep + J*cF64Sz)^);
{$endif}
end;

procedure ndaMatMul(const aA, aB: INDArray<Double>; var aRes, aBt: INDArray<Double>);
var A: INDArray<Double>;
{$ifdef BLAS}
    m, n, k, aStep, btStep, dstStep: NativeInt;
{$endif}
begin
  A := PrepareMatMul(aA, aB, aRes, aBt);
{$ifdef BLAS}
  m := A.Shape[0];
  k := A.Shape[1];
  n := aBt.Shape[0];
  aStep := A.Strides[0];
  btStep := aBt.Strides[0];
  dstStep := aRes.Strides[0];
  // BLAS leading dimensions are positive element counts represented by LongInt.
  if
    (aStep >= k*cF64Sz) and (btStep >= k*cF64Sz) and (dstStep >= n*cF64Sz) and
{$if SizeOf(NativeInt) > SizeOf(LongInt)}
    (m <= High(LongInt)) and (n <= High(LongInt)) and (k <= High(LongInt)) and
    (aStep div cF64Sz <= High(LongInt)) and
    (btStep div cF64Sz <= High(LongInt)) and
    (dstStep div cF64Sz <= High(LongInt)) and
{$endif}
    (aStep mod cF64Sz = 0) and (btStep mod cF64Sz = 0) and (dstStep mod cF64Sz = 0)
  then begin
    cblas.dgemm(cblasRowMajor, cblasNoTrans, cblasTrans,
      m, n, k, 1, PDouble(A.Data), aStep div cF64Sz,
      PDouble(aBt.Data), btStep div cF64Sz, 0, PDouble(aRes.Data), dstStep div cF64Sz
    );
    exit;
  end;
{$endif}
  MatMulNative(A, aBt, aRes);
end;

function ndaMatMulParallel(const aA, aB: INDArray<Double>; aMaxWorkers: Integer): INDArray<Double>;
var Bt: INDArray<Double>;
begin
  ndaMatMulParallel(aA, aB, Result, Bt, aMaxWorkers);
end;

procedure ndaMatMulParallel(const aA, aB: INDArray<Double>; var aRes: INDArray<Double>;
  aMaxWorkers: Integer);
var Bt: INDArray<Double>;
begin
  ndaMatMulParallel(aA, aB, aRes, Bt, aMaxWorkers);
end;

procedure ndaMatMulParallel(const aA, aB: INDArray<Double>; var aRes, aBt: INDArray<Double>;
  aMaxWorkers: Integer);
var A, Bt: INDArray<Double>;
    rowsA, rowsC: TArray<INDArray<Double>>;
    m, k, n, pairs, jobs, firstPair, count, rowStart, rowCount, I: NativeInt;
begin
  if aMaxWorkers < 0 then
    raise EArgumentOutOfRangeException.Create(csArgumentOutOfRange);

  A := PrepareMatMul(aA, aB, aRes, aBt);
  m := A.Shape[0];
  k := A.Shape[1];
  n := aBt.Shape[0];
  Assert((m = 1) or (Abs(aRes.Strides[0]) >= n*cF64Sz));

  if aMaxWorkers = 0 then
    aMaxWorkers := TThread.ProcessorCount;
  pairs := m div 2 + (m and 1);
  jobs := Min(Max(aMaxWorkers, 1), pairs);
  if jobs = 1 then begin
    MatMulNative(A, aBt, aRes);
    exit;
  end;

  Bt := aBt;
  SetLength(rowsA, jobs);
  SetLength(rowsC, jobs);
  firstPair := 0;
  for I := 0 to jobs - 1 do begin
    count := pairs div jobs;
    if I < pairs mod jobs then
      Inc(count);
    rowStart := firstPair*2;
    rowCount := Min(count*2, m - rowStart);
    rowsA[I] := A[NDIBlockSeq([rowStart, 0], [rowCount, k])];
    rowsC[I] := aRes[NDIBlockSeq([rowStart, 0], [rowCount, n])];
    Inc(firstPair, count);
  end;

  TParallel.For(0, Integer(jobs) - 1,
    procedure (aJob: Integer)
    begin
      MatMulNative(rowsA[aJob], Bt, rowsC[aJob]);
    end
  );
end;

{$endregion}

end.
