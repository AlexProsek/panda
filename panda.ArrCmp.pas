unit panda.ArrCmp;

interface

uses
    panda.Intfs
  , panda.Arrays
  , panda.Nums
  , panda.cvCmp
  , panda.cvArithmetic
  , System.Math
  ;

procedure ndaMax(const aA: Single; const aB: INDArray<Single>; var aRes: INDArray<Single>); overload;
procedure ndaMax(const aA, aB: INDArray<Single>; var aRes: INDArray<Single>); overload;

procedure ndaThreshold(const aA: INDArray<Byte>; aT: Byte; var aRes: INDArray<Byte>); overload;

function ndaAllIsFinite(const aA: INDArray<Single>): Boolean; inline; overload;
function ndaAllIsFinite(const aA: INDArray<Double>): Boolean; inline; overload;

{$region 'low-level functions'}

type
  TFinitePredicate = function (pValues: PByte; aCount, aStep: NativeInt): Boolean;

function _IsFinite_F32(pValues: PByte; aCount, aStep: NativeInt): Boolean;
function _IsFinite_F64(pValues: PByte; aCount, aStep: NativeInt): Boolean;

function _AllIsFinite(const aA: INDArray; aPred: TFinitePredicate): Boolean;

{$endregion}

implementation

{$EXCESSPRECISION OFF} // to prevent Single -> Double conversion by x64 compiler

{$region 'Min/Max functions'}

procedure MaxL_F32(N: NativeInt; L: PByte; IncL: NativeInt; R: PByte; IncR: NativeInt);
var pEnd: PByte;
    s: Single;
begin
  if IncL = 0 then begin
    // R <- Max(l,  R)
    s := PSingle(L)^;
    if IncR = cF32Sz then begin
      VecMax(s, PSingle(R), PSingle(R), N);
      exit;
    end;

    pEnd := R + N * IncR;
    while R < pEnd do begin
      PSingle(R)^ := Max(s,  PSingle(R)^);
      Inc(R, IncR);
    end;

    exit;
  end;

  // R <- Max(L, R)
  if (IncL = cF32Sz) and (IncR = cF32Sz) then begin
    VecMax(PSingle(L), PSingle(R), PSingle(R), N);
    exit;
  end;

  pEnd := R + N * IncR;
  while R < pEnd do begin
    PSingle(R)^ := Max(PSingle(L)^, PSingle(R)^);
    Inc(L, IncL);
    Inc(R, IncR);
  end;
end;

procedure MaxR_F32(N: NativeInt; L: PByte; IncL: NativeInt; R: PByte; IncR: NativeInt);
begin
  MaxL_F32(N, R, IncR, L, IncL);
end;

procedure ndaMax(const aA: Single; const aB: INDArray<Single>; var aRes: INDArray<Single>);
begin
  TNDAUt.Map<Single>(aA, aB, aRes, MaxL_F32);
end;

procedure ndaMax(const aA, aB: INDArray<Single>; var aRes: INDArray<Single>);
begin
  TNDAUt.Map<Single>(aA, aB, aRes, MaxL_F32, MaxR_F32);
end;

{$endregion}

{$region 'Thresholding'}

procedure ThresholdL_UI8(N: NativeInt; L: PByte; IncL: NativeInt; R: PByte; IncR: NativeInt);
var pEnd: PByte;
    t: Byte;
begin
  if IncL = 0 then begin
    // R <- Threshold(l, R)
    t := L^;
    if IncR = cI8Sz then begin
      VecThreshold(R, R, t, N);
      exit;
    end;

    pEnd := R + N * IncR;
    while R < pEnd do begin
      if R^ < t  then
        R^ := 0;
      Inc(R, IncR);
    end;

    exit;
  end;

  // R <- Threshold(L, R)
//  if (IncL = cI8Sz) and (IncR = cI8Sz) then begin
//    VecThresholdLT(R, R, L, N);
//    exit;
//  end;

  pEnd := R + N * IncR;
  while R < pEnd do begin
    if R^ < L^ then
      R^ := 0;
    Inc(L, IncL);
    Inc(R, IncR);
  end;
end;

procedure ndaThreshold(const aA: INDArray<Byte>; aT: Byte; var aRes: INDArray<Byte>);
begin
  TNDAUt.Map<Byte>(aT, aA, aRes, ThresholdL_UI8);
end;

{$endregion}

{$region 'Masking'}

{$endregion}

{$region 'Predicates'}

function IsFinite(const aValue: Single): Boolean; inline; overload;
begin
  Result := not (IsNan(aValue) or IsInfinite(aValue));
end;

function IsFinite(const aValue: Double): Boolean; inline; overload;
begin
  Result := not (IsNan(aValue) or IsInfinite(aValue));
end;

function _IsFinite_F32(pValues: PByte; aCount, aStep: NativeInt): Boolean;
var pEnd: PByte;
begin
  pEnd := pValues + aCount * aStep;
  while pValues <> pEnd do begin
    if not IsFinite(PSingle(pValues)^) then exit(False);
    Inc(pValues, aStep);
  end;
  Result := True;
end;

function _IsFinite_F64(pValues: PByte; aCount, aStep: NativeInt): Boolean;
var pEnd: PByte;
begin
  pEnd := pValues + aCount * aStep;
  while pValues <> pEnd do begin
    if not IsFinite(PDouble(pValues)^) then exit(False);
    Inc(pValues, aStep);
  end;
  Result := True;
end;

function _AllIsFinite(const aA: INDArray; aPred: TFinitePredicate): Boolean;
var it: TNDASliceIt;
    cnt, s: NativeInt;
    nDim: Integer;
begin
  nDim := aA.NDim;

  if nDim = 0 then
    exit(aPred(aA.Data, 1, aA.ItemSize));

  if nDim = 1 then
    exit(aPred(aA.Data, aA.Shape[0], aA.Strides[0]));

  it := TNDASliceIt.Create(aA, 0, nDim - 2);
  try
    cnt := aA.Shape[nDim - 1];
    s := aA.Strides[nDim - 1];
    while it.MoveNext do
      if not aPred(it.CurrentSlice.Data, cnt, s) then exit(False);
    Result := True;
  finally
    it.Free;
  end;
end;

function ndaAllIsFinite(const aA: INDArray<Single>): Boolean;
begin
  Result := _AllIsFinite(aA, _IsFinite_F32);
end;

function ndaAllIsFinite(const aA: INDArray<Double>): Boolean;
begin
  Result := _AllIsFinite(aA, _IsFinite_F64);
end;

{$endregion}

end.
