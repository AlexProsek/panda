unit panda.Math;

interface

uses
    System.Math
  , System.SysUtils
  , panda.Consts
  , panda.Intfs
  , panda.Arrays
{$ifdef BLAS}
  , LibCBLAS
  , System.IOUtils
{$endif}
  ;

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

/// <summary>
///   Gives the total of the elements in <c>aArr</c>.
/// </summary>
function ndaTotal(const aArr: INDArray<Integer>): INDArray<Integer>; overload;
function ndaTotal(const aArr: INDArray<Single>): INDArray<Single>; overload;
function ndaTotal(const aArr: INDArray<Double>): INDArray<Double>; overload;
/// <summary>
///   Gives totals all elements down to level <c>aLvl</c>.
/// </summary>
function ndaTotalToLvl(const aArr: INDArray<Single>; aLvl: Integer): INDArray<Single>;
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
function ndaDot(const aA, aB: INDArray<Double>): INDArray<Double>; overload;

function ndaOuter(const aA, aB: INDArray<Single>): INDArray<Single>; overload;
function ndaOuter(const aA, aB: INDArray<Double>): INDArray<Double>; overload;

implementation

uses
    panda.cvMath
  , panda.cvArithmetic
  , panda.Arithmetic
  , panda.DynArrayUtils
  ;

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
    // todo: check aLvl (has to be 1)
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
    ContiguousQ(aRes)
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

procedure Total(const aArr: INDArray<Integer>; var aRes: Integer); overload;
var it: TNDAIt;
    sz: NativeInt;
    lvl: Integer;
begin
  if CContiguousQ(aArr) then begin
    aRes := cvTotal(PInteger(aArr.Data), aArr.Size);
    exit
  end;

  aRes := 0;
  lvl := GetCContLvl(aArr, sz);
  it := TNDAIt.Create(aArr, lvl - 1);
  try
    if sz = SizeOf(Integer) then begin
      while it.MoveNext do
        Inc(aRes, PInteger(it.Current)^);
    end else begin
      while it.MoveNext do
        Inc(aRes, cvTotal(PInteger(it.Current), sz));
    end;
  finally
    it.Free;
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

  aRes := 0;
  lvl := GetCContLvl(aArr, sz);
  it := TNDAIt.Create(aArr, lvl - 1);
  try
    if sz = SizeOf(Single) then begin
      while it.MoveNext do
        aRes := aRes + PSingle(it.Current)^;
    end else begin
      while it.MoveNext do
        aRes := aRes + cvTotal(PSingle(it.Current), sz);
    end;
  finally
    it.Free;
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

  aRes := 0;
  lvl := GetCContLvl(aArr, sz);
  it := TNDAIt.Create(aArr, lvl - 1);
  try
    if sz = SizeOf(Double) then begin
      while it.MoveNext do
        aRes := aRes + PDouble(it.Current)^;
    end else begin
      while it.MoveNext do
        aRes := aRes + cvTotal(PDouble(it.Current), sz);
    end;
  finally
    it.Free;
  end;
end;

function ndaTotalToLvl(const aArr: INDArray<Single>; aLvl: Integer): INDArray<Single>;
begin
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

{$ifdef BLAS}

procedure LoadPandaBLAS;
var s: String;
begin
  s := GetEnvironmentVariable('PANDA_BLAS');
{$if defined(Win32)}
  s := TPath.Combine(s, 'Win32\libopenblas.dll');
{$elseif defined(Win64)}
  s := TPath.Combine(s, 'Win64\libopenblas.dll');
{$else}
   s := '';
{$endif}
  if not TFile.Exists(s) then
    raise Exception.Create('BLAS not found.'#13#10 +
      'Please set PANDA_BLAS environment variable.'
    );

  LoadLibCBLAS(s);
end;

initialization

  LoadPandaBLAS;

finalization

  FreeLibCBLAS;

{$endif}

end.
