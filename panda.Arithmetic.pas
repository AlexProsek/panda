unit panda.Arithmetic;

interface

uses
    panda.Intfs
  , panda.Consts
  , panda.Arrays
  , System.SysUtils
  , System.TypInfo
  ;

type
  TNDAArith = class(TNDAUt)
  public type
    TNDAEqComparer<T> = function (pA, pB: PByte; aAStep, aBStep, aCount: NativeInt; aTol: T): Boolean;
  protected
    class function iEqCmp(pA, pB: PByte; aAStep, aBStep, aCount: NativeInt; aTol: Integer): Boolean; static;
    class function sEqCmp(pA, pB: PByte; aAStep, aBStep, aCount: NativeInt; aTol: Single): Boolean; static;
  public
    class function AllClose<T>(const aA: INDArray<T>; const aB: T; aCmp: TNDAEqComparer<T>; const aTol: T): Boolean; overload; static;
    class function AllClose<T>(const aA, aB: INDArray<T>; aCmp: TNDAEqComparer<T>; const aTol: T): Boolean; overload; static;
  end;

  TTensorI32 = record
  private
    fArr: INDArray<Integer>;
  public
    class operator Implicit(const aArr: INDArray<Integer>): TTensorI32;
    class operator Implicit(const aArr: TTensorI32): INDArray<Integer>;
    class operator Add(const A, B: TTensorI32): TTensorI32;
    class operator Subtract(const A, B: TTensorI32): TTensorI32;
    class operator Multiply(const A, B: TTensorI32): TTensorI32;

    property NDA: INDArray<Integer> read fArr;
  end;

  TTensorI64 = record
  private
    fArr: INDArray<Int64>;
    function GetPart(const aIdx: INDIndexSeq): TTensorI64;
    procedure SetPart(const aIdx: INDIndexSeq; const aValue: TTensorI64);
    function GetShape: TNDAShape;
  public
    class operator Implicit(const aArr: INDArray<Int64>): TTensorI64;
    class operator Implicit(const aArr: TTensorI64): INDArray<Int64>;
    class operator Add(const A, B: TTensorI64): TTensorI64;
    class operator Subtract(const A, B: TTensorI64): TTensorI64;
    class operator Multiply(const A, B: TTensorI64): TTensorI64;

    property NDA: INDArray<Int64> read fArr;
    property Shape: TNDAShape read GetShape;
    property Part[const aIdx: INDIndexSeq]: TTensorI64 read GetPart write SetPart; default;
  end;

  TTensorF32 = record
  private
    fArr: INDArray<Single>;
    function GetPart(const aIdx: INDIndexSeq): TTensorF32;
    procedure SetPart(const aIdx: INDIndexSeq; const aValue: TTensorF32);
    function GetShape: TNDAShape;
  public
    class operator Implicit(const aArr: INDArray<Single>): TTensorF32;
    class operator Implicit(const aArr: TTensorF32): INDArray<Single>;
    class operator Implicit(const aArr: TTensorI32): TTensorF32;
    class operator Add(const A, B: TTensorF32): TTensorF32;
    class operator Add(const A: TTensorF32; B: Single): TTensorF32;
    class operator Add(A: Single; const B: TTensorF32): TTensorF32;
    class operator Subtract(const A, B: TTensorF32): TTensorF32;
    class operator Subtract(const A: TTensorF32; B: Single): TTensorF32;
    class operator Subtract(A: Single; const B: TTensorF32): TTensorF32;
    class operator Multiply(const A, B: TTensorF32): TTensorF32;
    class operator Multiply(const A: TTensorF32; B: Single): TTensorF32;
    class operator Multiply(A: Single; const B: TTensorF32): TTensorF32;
    class operator Divide(const A, B: TTensorF32): TTensorF32;
    class operator Divide(const A: TTensorF32; B: Single): TTensorF32;
    class operator Divide(A: Single; const B: TTensorF32): TTensorF32;
    procedure AddTo(const aArr: TTensorF32); overload;
    procedure AddTo(const aValue: Single); overload;
    procedure SubtractFrom(const aArr: TTensorF32); overload;
    procedure SubtractFrom(const aValue: Single); overload;
    procedure MultiplyBy(const aArr: TTensorF32); overload;
    procedure MultiplyBy(const aValue: Single); overload;
    procedure DivideBy(const aArr: TTensorF32); overload;
    procedure DivideBy(const aValue: Single); overload;

    property NDA: INDArray<Single> read fArr;
    property Shape: TNDAShape read GetShape;
    property Part[const aIdx: INDIndexSeq]: TTensorF32 read GetPart write SetPart; default;
  end;

  TTensorF64 = record
  private
    fArr: INDArray<Double>;
    function GetPart(const aIdx: INDIndexSeq): TTensorF64;
    procedure SetPart(const aIdx: INDIndexSeq; const aValue: TTensorF64);
    function GetShape: TNDAShape;
  public
    class operator Implicit(const aArr: INDArray<Double>): TTensorF64;
    class operator Implicit(const aArr: TTensorF64): INDArray<Double>;
    class operator Implicit(const aArr: INDArray<Single>): TTensorF64;
    class operator Add(const A, B: TTensorF64): TTensorF64;
    class operator Subtract(const A, B: TTensorF64): TTensorF64;
    class operator Multiply(const A, B: TTensorF64): TTensorF64;
    class operator Divide(const A, B: TTensorF64): TTensorF64;
    class operator Divide(const A: TTensorF64; B: Double): TTensorF64;
    class operator Divide(A: Double; const B: TTensorF64): TTensorF64;
    procedure AddTo(const aArr: TTensorF64); overload;
    procedure AddTo(const aValue: Double); overload;
    procedure SubtractFrom(const aArr: TTensorF64); overload;
    procedure SubtractFrom(const aValue: Double); overload;
    procedure MultiplyBy(const aArr: TTensorF64); overload;
    procedure MultiplyBy(const aValue: Double); overload;
    procedure DivideBy(const aArr: TTensorF64); overload;
    procedure DivideBy(const aValue: Double); overload;

    property NDA: INDArray<Double> read fArr;
    property Shape: TNDAShape read GetShape;
    property Part[const aIdx: INDIndexSeq]: TTensorF64 read GetPart write SetPart; default;
  end;

  function ndaAllClose(const aA, aB: INDArray<Integer>; aTol: Integer = 0): Boolean;  overload;
  function ndaAllClose(const aA, aB: INDArray<Single>; aTol: Single = 1e-5): Boolean; overload;
  function ndaAllClose(const aA: INDArray<Single>; aB: Single; aTol: Single = 1e-5): Boolean; overload;

  function ndaAdd(const aArrays: array of INDArray; var aRes: INDArray): Boolean; overload;
  function ndaAdd(const aArrays: array of INDArray<Integer>): INDArray<Integer>; overload;
  function ndaAdd(const aArrays: array of INDArray<Int64>): INDArray<Int64>; overload;
  function ndaAdd(const aArrays: array of INDArray<Single>): INDArray<Single>; overload;
  function ndaAdd(const aArrays: array of INDArray<Double>): INDArray<Double>; overload;

  function ndaMultiply(const aArrays: array of INDArray; var aRes: INDArray): Boolean; overload;
  function ndaMultiply(const aArrays: array of INDArray<Integer>): INDArray<Integer>; overload;
  function ndaMultiply(const aArrays: array of INDArray<Int64>): INDArray<Int64>; overload;
  function ndaMultiply(const aArrays: array of INDArray<Single>): INDArray<Single>; overload;
  function ndaMultiply(const aArrays: array of INDArray<Double>): INDArray<Double>; overload;

implementation

uses
    panda.cvArithmetic
  , System.Math
  ;

{$region 'TNDAArith'}

class function TNDAArith.iEqCmp(pA, pB: PByte; aAStep, aBStep, aCount: NativeInt; aTol: Integer): Boolean;
var pEnd: PByte;
    i: Integer;
begin
  if aAStep = 0 then begin
    i := PInteger(pA)^;
    pEnd := pB + aBStep * aCount;
    while pB < pEnd do begin
      if Abs(PInteger(pB)^ - i) > aTol then exit(False);
      Inc(pB, aBStep);
    end;
    exit(True);
  end;

  if aBStep = 0 then begin
    i := PInteger(pB)^;
    pEnd := pA + aAStep * aCount;
    while pA < pEnd do begin
      if Abs(PInteger(pA)^ - i) > aTol then exit(False);
      Inc(pA, aAStep);
    end;
    exit(True);
  end;

  pEnd := pA + aAStep * aCount;
  while pA < pEnd do begin
    if Abs(PInteger(pA)^ - PInteger(pB)^) > aTol then exit(False);
    Inc(pA, aAStep);
    Inc(pB, aAStep);
  end;
  Result := True;
end;

class function TNDAArith.sEqCmp(pA, pB: PByte; aAStep, aBStep, aCount: NativeInt; aTol: Single): Boolean;
var pEnd: PByte;
    s: Single;
begin
  if aAStep = 0 then begin
    s := PSingle(pA)^;
    pEnd := pB + aBStep * aCount;
    while pB < pEnd do begin
      if Abs(PSingle(pB)^ - s) > aTol then exit(False);
      Inc(pB, aBStep);
    end;
    exit(True);
  end;

  if aBStep = 0 then begin
    s := PSingle(pB)^;
    pEnd := pA + aAStep * aCount;
    while pA < pEnd do begin
      if Abs(PSingle(pA)^ - s) > aTol then exit(False);
      Inc(pA, aAStep);
    end;
    exit(True);
  end;

  pEnd := pA + aAStep * aCount;
  while pA < pEnd do begin
    if Abs(PSingle(pA)^ - PSingle(pB)^) > aTol then exit(False);
    Inc(pA, aAStep);
    Inc(pB, aBStep);
  end;
  Result := True;
end;

class function TNDAArith.AllClose<T>(const aA: INDArray<T>; const aB: T; aCmp: TNDAEqComparer<T>; const aTol: T): Boolean;
var itA: TNDAIt;
    count, sA: NativeInt;
    hiDim: Integer;
begin
  hiDim := aA.NDim - 1;
  count := aA.Shape[hiDim];
  sA := aA.Strides[hiDim];
  if hiDim = 0 then begin
    Result := aCmp(aA.Data, @aB, sA, 0, count, aTol);
    exit;
  end;

  itA := TNDAIt.Create(aA, hiDim - 1);
  try
    while itA.MoveNext do
      if not aCmp(itA.Current, @aB, sA, 0, count, aTol) then exit(False);
    Result := True;
  finally
    itA.Free;
  end;
end;

class function TNDAArith.AllClose<T>(const aA, aB: INDArray<T>; aCmp: TNDAEqComparer<T>; const aTol: T): Boolean;
var itA, itB: TNDAIt;
    count, sA, sB: NativeInt;
    hiDim: Integer;
begin
  if not SameShapeQ(aA, aB) then
    raise ENDAIndexError.Create('Error Message');

  hiDim := aA.NDim - 1;
  count := aA.Shape[hiDim];
  sA := aA.Strides[hiDim];
  sB := aB.Strides[hiDim];
  if hiDim = 0 then begin
    Result := aCmp(aA.Data, aB.Data, sA, sB, count, aTol);
    exit;
  end;

  itA := TNDAIt.Create(aA, hiDim - 1);
  itB := TNDAIt.Create(aB, hiDim - 1);
  try
    while itA.MoveNext and itB.MoveNext do
      if not aCmp(itA.Current, itB.Current, sA, sB, count, aTol) then exit(False);
    Result := True;
  finally
    itA.Free;
    itB.Free;
  end;
end;

{$endregion}

{$region 'TTensorI32'}

{$region 'LR functions'}

procedure AddL_I32(N: NativeInt; L: PByte; IncL: NativeInt; R: PByte; IncR: NativeInt);
var pEnd: PByte;
    s: Integer;
begin
  if IncL = 0 then begin
    // R <- L + R
    s := PInteger(L)^;
    pEnd := R + N * IncR;
    while R < pEnd do begin
      PInteger(R)^ := s + PInteger(R)^;
      Inc(R, IncR);
    end;

    exit;
  end;

  // R <- L + R
  pEnd := R + N * IncL;
  while R < pEnd do begin
    PInteger(R)^ := PInteger(L)^ + PInteger(R)^;
    Inc(L, IncL);
    Inc(R, IncR);
  end;
end;

procedure AddR_I32(N: NativeInt; L: PByte; IncL: NativeInt; R: PByte; IncR: NativeInt);
begin
  AddL_I32(N, R, IncR, L, IncL);
end;

procedure SubL_I32(N: NativeInt; L: PByte; IncL: NativeInt; R: PByte; IncR: NativeInt);
var pEnd: PByte;
    s: Integer;
begin
  if IncL = 0 then begin
    // R <- L - R
    s := PInteger(L)^;
    pEnd := R + N * IncR;
    while R < pEnd do begin
      PInteger(R)^ := s - PInteger(R)^;
      Inc(R, IncR);
    end;

    exit;
  end;

  // R <- L - R
  pEnd := R + N * IncL;
  while R < pEnd do begin
    PInteger(R)^ := PInteger(L)^ - PInteger(R)^;
    Inc(L, IncL);
    Inc(R, IncR);
  end;
end;

procedure SubR_I32(N: NativeInt; L: PByte; IncL: NativeInt; R: PByte; IncR: NativeInt);
var pEnd: PByte;
    s: Integer;
begin
  if IncR = 0 then begin
    // L <- L - R
    s := PInteger(R)^;
    pEnd := L + N * IncL;
    while L < pEnd do begin
      PInteger(L)^ := PInteger(L)^ - s;
      Inc(L, IncL);
    end;

    exit;
  end;

  // L <- L - R
  pEnd := L + N * IncL;
  while L < pEnd do begin
    PInteger(L)^ := PInteger(L)^ - PInteger(R)^;
    Inc(L, IncL);
    Inc(R, IncR);
  end;
end;

procedure MulL_I32(N: NativeInt; L: PByte; IncL: NativeInt; R: PByte; IncR: NativeInt);
var pEnd: PByte;
    s: Integer;
begin
  if IncL = 0 then begin
    // R <- L * R
    s := PInteger(L)^;
    pEnd := R + N * IncR;
    while R < pEnd do begin
      PInteger(R)^ := s * PInteger(R)^;
      Inc(R, IncR);
    end;

    exit;
  end;

  // R <- L * R
  pEnd := R + N * IncL;
  while R < pEnd do begin
    PInteger(R)^ := PInteger(L)^ * PInteger(R)^;
    Inc(L, IncL);
    Inc(R, IncR);
  end;
end;

procedure MulR_I32(N: NativeInt; L: PByte; IncL: NativeInt; R: PByte; IncR: NativeInt);
begin
  MulL_I32(N, R, IncR, L, IncL);
end;

{$endregion}

class operator TTensorI32.Implicit(const aArr: INDArray<Integer>): TTensorI32;
begin
  Result.fArr := aArr;
end;

class operator TTensorI32.Implicit(const aArr: TTensorI32): INDArray<Integer>;
begin
  Result := aArr.fArr;
end;

class operator TTensorI32.Add(const A, B: TTensorI32): TTensorI32;
begin
  Result.fArr := nil;
  TNDAUt.Map<Integer>(A, B, Result.fArr, AddL_I32, AddR_I32);
end;

class operator TTensorI32.Subtract(const A, B: TTensorI32): TTensorI32;
begin
  Result.fArr := nil;
  TNDAUt.Map<Integer>(A, B, Result.fArr, SubL_I32, SubR_I32);
end;

class operator TTensorI32.Multiply(const A, B: TTensorI32): TTensorI32;
begin
  Result.fArr := nil;
  TNDAUt.Map<Integer>(A, B, Result.fArr, MulL_I32, MulR_I32);
end;

{$endregion}

{$region 'TTensorI64'}

{$region 'LR functions'}

procedure AddL_I64(N: NativeInt; L: PByte; IncL: NativeInt; R: PByte; IncR: NativeInt);
var pEnd: PByte;
    s: Int64;
begin
  if IncL = 0 then begin
    // R <- L + R
    s := PInteger(L)^;
    pEnd := R + N * IncR;
    while R < pEnd do begin
      PInt64(R)^ := s + PInt64(R)^;
      Inc(R, IncR);
    end;

    exit;
  end;

  // R <- L + R
  pEnd := R + N * IncL;
  while R < pEnd do begin
    PInt64(R)^ := PInt64(L)^ + PInt64(R)^;
    Inc(L, IncL);
    Inc(R, IncR);
  end;
end;

procedure AddR_I64(N: NativeInt; L: PByte; IncL: NativeInt; R: PByte; IncR: NativeInt);
begin
  AddL_I64(N, R, IncR, L, IncL);
end;

procedure SubL_I64(N: NativeInt; L: PByte; IncL: NativeInt; R: PByte; IncR: NativeInt);
var pEnd: PByte;
    s: Int64;
begin
  if IncL = 0 then begin
    // R <- L - R
    s := PInteger(L)^;
    pEnd := R + N * IncR;
    while R < pEnd do begin
      PInt64(R)^ := s - PInt64(R)^;
      Inc(R, IncR);
    end;

    exit;
  end;

  // R <- L - R
  pEnd := R + N * IncL;
  while R < pEnd do begin
    PInt64(R)^ := PInt64(L)^ - PInt64(R)^;
    Inc(L, IncL);
    Inc(R, IncR);
  end;
end;

procedure SubR_I64(N: NativeInt; L: PByte; IncL: NativeInt; R: PByte; IncR: NativeInt);
var pEnd: PByte;
    s: Int64;
begin
  if IncR = 0 then begin
    // L <- L - R
    s := PInteger(R)^;
    pEnd := L + N * IncL;
    while L < pEnd do begin
      PInt64(L)^ := PInt64(L)^ - s;
      Inc(L, IncL);
    end;

    exit;
  end;

  // L <- L - R
  pEnd := L + N * IncL;
  while L < pEnd do begin
    PInt64(L)^ := PInt64(L)^ - PInt64(R)^;
    Inc(L, IncL);
    Inc(R, IncR);
  end;
end;

procedure MulL_I64(N: NativeInt; L: PByte; IncL: NativeInt; R: PByte; IncR: NativeInt);
var pEnd: PByte;
    s: Integer;
begin
  if IncL = 0 then begin
    // R <- L * R
    s := PInteger(L)^;
    pEnd := R + N * IncR;
    while R < pEnd do begin
      PInt64(R)^ := s * PInt64(R)^;
      Inc(R, IncR);
    end;

    exit;
  end;

  // R <- L * R
  pEnd := R + N * IncL;
  while R < pEnd do begin
    PInt64(R)^ := PInt64(L)^ * PInt64(R)^;
    Inc(L, IncL);
    Inc(R, IncR);
  end;
end;

procedure MulR_I64(N: NativeInt; L: PByte; IncL: NativeInt; R: PByte; IncR: NativeInt);
begin
  MulL_I64(N, R, IncR, L, IncL);
end;

{$endregion}

class operator TTensorI64.Implicit(const aArr: INDArray<Int64>): TTensorI64;
begin
  Result.fArr := aArr;
end;

class operator TTensorI64.Implicit(const aArr: TTensorI64): INDArray<Int64>;
begin
  Result := aArr.fArr;
end;

class operator TTensorI64.Add(const A, B: TTensorI64): TTensorI64;
begin
  Result.fArr := nil;
  TNDAUt.Map<Int64>(A, B, Result.fArr, AddL_I64, AddR_I64);
end;

class operator TTensorI64.Subtract(const A, B: TTensorI64): TTensorI64;
begin
  Result.fArr := nil;
  TNDAUt.Map<Int64>(A, B, Result.fArr, SubL_I64, SubR_I64);
end;

class operator TTensorI64.Multiply(const A, B: TTensorI64): TTensorI64;
begin
  Result.fArr := nil;
  TNDAUt.Map<Int64>(A, B, Result.fArr, MulL_I64, MulR_I64);
end;

function TTensorI64.GetPart(const aIdx: INDIndexSeq): TTensorI64;
begin
  Result := fArr[aIdx];
end;

procedure TTensorI64.SetPart(const aIdx: INDIndexSeq; const aValue: TTensorI64);
begin
  fArr[aIdx] := aValue;
end;

function TTensorI64.GetShape: TNDAShape;
begin
  Result := fArr.Shape;
end;

{$endregion}

{$region 'TTensorF32'}

{$region 'LR functions'}

procedure AddL_F32(N: NativeInt; L: PByte; IncL: NativeInt; R: PByte; IncR: NativeInt);
var pEnd: PByte;
    s: Single;
begin
  if IncL = 0 then begin
    // R <- l + R
    s := PSingle(L)^;
    if IncR = cF32Sz then begin
      VecAdd(PSingle(R), s, PSingle(R), N);
      exit;
    end;

    s := PSingle(L)^;
    pEnd := R + N * IncR;
    while R < pEnd do begin
      PSingle(R)^ := s + PSingle(R)^;
      Inc(R, IncR);
    end;

    exit;
  end;

  // R <- L + R
  if (IncL = cF32Sz) and (IncR = cF32Sz) then begin
    VecAdd(PSingle(L), PSingle(R), PSingle(R), N);
    exit;
  end;

  pEnd := R + N * IncL;
  while R < pEnd do begin
    PSingle(R)^ := PSingle(L)^ + PSingle(R)^;
    Inc(L, IncL);
    Inc(R, IncR);
  end;
end;

procedure AddR_F32(N: NativeInt; L: PByte; IncL: NativeInt; R: PByte; IncR: NativeInt);
begin
  AddL_F32(N, R, IncR, L, IncL);
end;

procedure SubL_F32(N: NativeInt; L: PByte; IncL: NativeInt; R: PByte; IncR: NativeInt);
var pEnd: PByte;
    s: Single;
begin
  if IncL = 0 then begin
    // R <- L - R
    s := PSingle(L)^;
    if IncR = cF32Sz then begin
      VecSub(s, PSingle(R), PSingle(R), N);
      exit;
    end;

    pEnd := R + N * IncR;
    while R < pEnd do begin
      PSingle(R)^ := s - PSingle(R)^;
      Inc(R, IncR);
    end;

    exit;
  end;

  // R <- L - R
  pEnd := R + N * IncL;
  if (IncL = cF32Sz) and (IncR = cF32Sz) then begin
    VecSub(PSingle(L), PSingle(R), PSingle(R), N);
    exit;
  end;

  while R < pEnd do begin
    PSingle(R)^ := PSingle(L)^ - PSingle(R)^;
    Inc(L, IncL);
    Inc(R, IncR);
  end;
end;

procedure SubR_F32(N: NativeInt; L: PByte; IncL: NativeInt; R: PByte; IncR: NativeInt);
var pEnd: PByte;
    s: Single;
begin
  if IncR = 0 then begin
    // L <- L - R
    s := PSingle(R)^;
    if IncL = cF32Sz then begin
      VecSub(PSingle(L), s, PSingle(L), N);
      exit;
    end;

    pEnd := L + N * IncL;
    while L < pEnd do begin
      PSingle(L)^ := PSingle(L)^ - s;
      Inc(L, IncL);
    end;

    exit;
  end;

  // L <- L - R
  pEnd := L + N * IncL;
  if (IncL = cF32Sz) and (IncR = cF32Sz) then begin
    VecSub(PSingle(L), PSingle(R), PSingle(L), N);
    exit;
  end;

  while L < pEnd do begin
    PSingle(L)^ := PSingle(L)^ - PSingle(R)^;
    Inc(L, IncL);
    Inc(R, IncR);
  end;
end;

procedure MulL_F32(N: NativeInt; L: PByte; IncL: NativeInt; R: PByte; IncR: NativeInt);
var pEnd: PByte;
    s: Single;
begin
  if IncL = 0 then begin
    // R <- l * R
    s := PSingle(L)^;
    if IncR = cF32Sz then begin
      VecMul(PSingle(L), s, PSingle(R), N);
      exit;
    end;

    pEnd := R + N * IncR;
    while R < pEnd do begin
      PSingle(R)^ := s * PSingle(R)^;
      Inc(R, IncR);
    end;

    exit;
  end;

  // R <- L * R
  if (IncL = cF32Sz) and (IncR = cF32Sz) then begin
    VecMul(PSingle(L), PSingle(R), PSingle(R), N);
    exit;
  end;

  pEnd := R + N * IncL;
  while R < pEnd do begin
    PSingle(R)^ := PSingle(L)^ * PSingle(R)^;
    Inc(L, IncL);
    Inc(R, IncR);
  end;
end;

procedure MulR_F32(N: NativeInt; L: PByte; IncL: NativeInt; R: PByte; IncR: NativeInt);
begin
  MulL_F32(N, R, IncR, L, IncL);
end;

procedure DivL_F32(N: NativeInt; L: PByte; IncL: NativeInt; R: PByte; IncR: NativeInt);
var pEnd: PByte;
    s: Single;
begin
  if IncL = 0 then begin
    // R <- l / R
    s := PSingle(L)^;
    pEnd := R + N * IncR;
    while R < pEnd do begin
      PSingle(R)^ := s / PSingle(R)^;
      Inc(R, IncR);
    end;

    exit;
  end;

  // R <- L / R
  pEnd := R + N * IncL;
  while R < pEnd do begin
    PSingle(R)^ := PSingle(L)^ / PSingle(R)^;
    Inc(L, IncL);
    Inc(R, IncR);
  end;
end;

procedure DivR_F32(N: NativeInt; L: PByte; IncL: NativeInt; R: PByte; IncR: NativeInt);
var pEnd: PByte;
    s: Single;
begin
  if IncR = 0 then begin
    // L <- L / r
    s := PSingle(R)^;
    if IncL = cF32Sz then begin
      VecMul(PSingle(L), 1/s, PSingle(L), N);
      exit;
    end;

    pEnd := L + N * IncL;
    while L < pEnd do begin
      PSingle(L)^ := PSingle(L)^ / s;
      Inc(L, IncL);
    end;

    exit;
  end;

  // L <- L / R
  pEnd := L + N * IncL;
  while L < pEnd do begin
    PSingle(L)^ := PSingle(L)^ / PSingle(R)^;
    Inc(L, IncL);
    Inc(R, IncR);
  end;
end;

{$endregion}

class operator TTensorF32.Implicit(const aArr: INDArray<Single>): TTensorF32;
begin
  Result.fArr := aArr;
end;

class operator TTensorF32.Implicit(const aArr: TTensorF32): INDArray<Single>;
begin
  Result := aArr.fArr;
end;

class operator TTensorF32.Implicit(const aArr: TTensorI32): TTensorF32;
begin
  Result := TNDAUt.AsType<Single>(aArr.NDA);
end;

class operator TTensorF32.Add(const A, B: TTensorF32): TTensorF32;
begin
  Result.fArr := nil;
  TNDAUt.Map<Single>(A, B, Result.fArr, AddL_F32, AddR_F32);
end;

class operator TTensorF32.Add(const A: TTensorF32; B: Single): TTensorF32;
begin
  Result.fArr := nil;
  TNDAUt.Map<Single>(A, B, Result.fArr, AddR_F32);
end;

class operator TTensorF32.Add(A: Single; const B: TTensorF32): TTensorF32;
begin
  Result := nil;
  TNDAUt.Map<Single>(A, B, Result.fArr, AddL_F32);
end;

class operator TTensorF32.Subtract(const A, B: TTensorF32): TTensorF32;
begin
  Result.fArr := nil;
  TNDAUt.Map<Single>(A, B, Result.fArr, SubL_F32, SubR_F32);
end;

class operator TTensorF32.Subtract(const A: TTensorF32; B: Single): TTensorF32;
begin
  Result.fArr := nil;
  TNDAUt.Map<Single>(A, B, Result.fArr, SubR_F32);
end;

class operator TTensorF32.Subtract(A: Single; const B: TTensorF32): TTensorF32;
begin
  Result.fArr := nil;
  TNDAUt.Map<Single>(A, B, Result.fArr, SubL_F32);
end;

class operator TTensorF32.Multiply(const A, B: TTensorF32): TTensorF32;
begin
  Result.fArr := nil;
  TNDAUt.Map<Single>(A, B, Result.fArr, MulL_F32, MulR_F32);
end;

class operator TTensorF32.Multiply(const A: TTensorF32; B: Single): TTensorF32;
begin
  Result.fArr := nil;
  TNDAUt.Map<Single>(A, B, Result.fArr, MulR_F32);
end;

class operator TTensorF32.Multiply(A: Single; const B: TTensorF32): TTensorF32;
begin
  Result.fArr := nil;
  TNDAUt.Map<Single>(A, B, Result.fArr, MulL_F32);
end;

class operator TTensorF32.Divide(const A, B: TTensorF32): TTensorF32;
begin
  Result.fArr := nil;
  TNDAUt.Map<Single>(A, B, Result.fArr, DivL_F32, DivR_F32);
end;

class operator TTensorF32.Divide(const A: TTensorF32; B: Single): TTensorF32;
begin
  Result.fArr := nil;
  TNDAUt.Map<Single>(A, B, Result.fArr, DivR_F32);
end;

class operator TTensorF32.Divide(A: Single; const B: TTensorF32): TTensorF32;
begin
  Result.fArr := nil;
  TNDAUt.Map<Single>(A, B, Result.fArr, DivL_F32);
end;

procedure TTensorF32.AddTo(const aArr: TTensorF32);
begin
  TNDAArith.MapR(fArr, aArr.fArr, AddR_F32);
end;

procedure TTensorF32.AddTo(const aValue: Single);
begin
  TNDAArith.MapR(fArr, @aValue, AddR_F32);
end;

procedure TTensorF32.SubtractFrom(const aArr: TTensorF32);
begin
  TNDAArith.MapR(fArr, aArr.fArr, SubR_F32);
end;

procedure TTensorF32.SubtractFrom(const aValue: Single);
begin
  TNDAArith.MapR(fArr, @aValue, SubR_F32);
end;

procedure TTensorF32.MultiplyBy(const aArr: TTensorF32);
begin
  TNDAArith.MapR(fArr, aArr.fArr, MulR_F32);
end;

procedure TTensorF32.MultiplyBy(const aValue: Single);
begin
  TNDAArith.MapR(fArr, @aValue, MulR_F32);
end;

procedure TTensorF32.DivideBy(const aArr: TTensorF32);
begin
  TNDAArith.MapR(fArr, aArr.fArr, DivR_F32);
end;

procedure TTensorF32.DivideBy(const aValue: Single);
begin
  TNDAArith.MapR(fArr, @aValue, DivR_F32);
end;

function TTensorF32.GetPart(const aIdx: INDIndexSeq): TTensorF32;
begin
  Result := fArr[aIdx];
end;

procedure TTensorF32.SetPart(const aIdx: INDIndexSeq; const aValue: TTensorF32);
begin
  fArr[aIdx] := aValue;
end;

function TTensorF32.GetShape: TNDAShape;
begin
  Result := fArr.Shape;
end;

{$endregion}

{$region 'TNDARecF64'}

{$region 'LR functions'}

procedure AddL_F64(N: NativeInt; L: PByte; IncL: NativeInt; R: PByte; IncR: NativeInt);
var pEnd: PByte;
    s: Double;
begin
  if IncL = 0 then begin
    // R <- L + R
    s := PDouble(L)^;
    pEnd := R + N * IncR;
    while R < pEnd do begin
      PDouble(R)^ := s + PDouble(R)^;
      Inc(R, IncR);
    end;

    exit;
  end;

  // R <- L + R
  pEnd := R + N * IncL;
  while R < pEnd do begin
    PDouble(R)^ := PDouble(L)^ + PDouble(R)^;
    Inc(L, IncL);
    Inc(R, IncR);
  end;
end;

procedure AddR_F64(N: NativeInt; L: PByte; IncL: NativeInt; R: PByte; IncR: NativeInt);
begin
  AddL_F32(N, R, IncR, L, IncL);
end;

procedure SubL_F64(N: NativeInt; L: PByte; IncL: NativeInt; R: PByte; IncR: NativeInt);
var pEnd: PByte;
    s: Double;
begin
  if IncL = 0 then begin
    // R <- L - R
    s := PSingle(L)^;
    pEnd := R + N * IncR;
    while R < pEnd do begin
      PDouble(R)^ := s - PDouble(R)^;
      Inc(R, IncR);
    end;

    exit;
  end;

  // R <- L - R
  pEnd := R + N * IncL;
  while R < pEnd do begin
    PDouble(R)^ := PDouble(L)^ - PDouble(R)^;
    Inc(L, IncL);
    Inc(R, IncR);
  end;
end;

procedure SubR_F64(N: NativeInt; L: PByte; IncL: NativeInt; R: PByte; IncR: NativeInt);
var pEnd: PByte;
    s: Double;
begin
  if IncR = 0 then begin
    // L <- L - R
    s := PDouble(R)^;
    pEnd := L + N * IncL;
    while L < pEnd do begin
      PDouble(L)^ := PDouble(L)^ - s;
      Inc(L, IncL);
    end;

    exit;
  end;

  // L <- L - R
  pEnd := L + N * IncL;
  while L < pEnd do begin
    PDouble(L)^ := PDouble(L)^ - PDouble(R)^;
    Inc(L, IncL);
    Inc(R, IncR);
  end;
end;

procedure MulL_F64(N: NativeInt; L: PByte; IncL: NativeInt; R: PByte; IncR: NativeInt);
var pEnd: PByte;
    s: Double;
begin
  if IncL = 0 then begin
    // R <- L * R
    s := PDouble(L)^;
    pEnd := R + N * IncR;
    while R < pEnd do begin
      PDouble(R)^ := s * PDouble(R)^;
      Inc(R, IncR);
    end;

    exit;
  end;

  // R <- L * R
  pEnd := R + N * IncL;
  while R < pEnd do begin
    PDouble(R)^ := PDouble(L)^ * PDouble(R)^;
    Inc(L, IncL);
    Inc(R, IncR);
  end;
end;

procedure MulR_F64(N: NativeInt; L: PByte; IncL: NativeInt; R: PByte; IncR: NativeInt);
begin
  MulL_F32(N, R, IncR, L, IncL);
end;

procedure DivL_F64(N: NativeInt; L: PByte; IncL: NativeInt; R: PByte; IncR: NativeInt);
var pEnd: PByte;
    s: Double;
begin
  if IncL = 0 then begin
    // R <- L / R
    s := PDouble(L)^;
    pEnd := R + N * IncR;
    while R < pEnd do begin
      PDouble(R)^ := s / PDouble(R)^;
      Inc(R, IncR);
    end;

    exit;
  end;

  // R <- L / R
  pEnd := R + N * IncL;
  while R < pEnd do begin
    PDouble(R)^ := PDouble(L)^ / PDouble(R)^;
    Inc(L, IncL);
    Inc(R, IncR);
  end;
end;

procedure DivR_F64(N: NativeInt; L: PByte; IncL: NativeInt; R: PByte; IncR: NativeInt);
var pEnd: PByte;
    s: Double;
begin
  if IncR = 0 then begin
    // L <- L / R
    s := PDouble(R)^;
    pEnd := L + N * IncL;
    while L < pEnd do begin
      PDouble(L)^ := PDouble(L)^ / s;
      Inc(L, IncL);
    end;

    exit;
  end;

  // L <- L / R
  pEnd := L + N * IncL;
  while L < pEnd do begin
    PDouble(L)^ := PDouble(L)^ / PDouble(R)^;
    Inc(L, IncL);
    Inc(R, IncR);
  end;
end;

{$endregion}

class operator TTensorF64.Implicit(const aArr: INDArray<Double>): TTensorF64;
begin
  Result.fArr := aArr;
end;

class operator TTensorF64.Implicit(const aArr: TTensorF64): INDArray<Double>;
begin
  Result := aArr.fArr;
end;

class operator TTensorF64.Implicit(const aArr: INDArray<Single>): TTensorF64;
begin
  Result := TNDAUt.AsType<Double>(aArr);
end;

class operator TTensorF64.Add(const A, B: TTensorF64): TTensorF64;
begin
  Result.fArr := nil;
  TNDAUt.Map<Double>(A, B, Result.fArr, AddL_F64, AddR_F64);
end;

class operator TTensorF64.Subtract(const A, B: TTensorF64): TTensorF64;
begin
  Result.fArr := nil;
  TNDAUt.Map<Double>(A, B, Result.fArr, SubL_F32, SubR_F64);
end;

class operator TTensorF64.Multiply(const A, B: TTensorF64): TTensorF64;
begin
  Result.fArr := nil;
  TNDAUt.Map<Double>(A, B, Result.fArr, MulL_F64, MulR_F64);
end;

class operator TTensorF64.Divide(const A, B: TTensorF64): TTensorF64;
begin
  Result.fArr := nil;
  TNDAUt.Map<Double>(A, B, Result.fArr, DivL_F64, DivR_F64);
end;

class operator TTensorF64.Divide(const A: TTensorF64; B: Double): TTensorF64;
begin
  Result.fArr := nil;
  TNDAUt.Map<Double>(A, B, Result.fArr, DivR_F64);
end;

class operator TTensorF64.Divide(A: Double; const B: TTensorF64): TTensorF64;
begin
  Result.fArr := nil;
  TNDAUt.Map<Double>(A, B, Result.fArr, DivL_F64);
end;

procedure TTensorF64.AddTo(const aArr: TTensorF64);
begin
  TNDAArith.MapR(fArr, aArr.fArr, AddR_F64);
end;

procedure TTensorF64.AddTo(const aValue: Double);
begin
  TNDAArith.MapR<Double>(fArr, aValue, AddR_F64);
end;

procedure TTensorF64.SubtractFrom(const aArr: TTensorF64);
begin
  TNDAArith.MapR(fArr, aArr.fArr, AddR_F64);
end;

procedure TTensorF64.SubtractFrom(const aValue: Double);
begin
  TNDAArith.MapR<Double>(fArr, aValue, SubR_F64);
end;

procedure TTensorF64.MultiplyBy(const aArr: TTensorF64);
begin
  TNDAArith.MapR(fArr, aArr.fArr, AddR_F64);
end;

procedure TTensorF64.MultiplyBy(const aValue: Double);
begin
  TNDAArith.MapR<Double>(fArr, aValue, MulR_F64);
end;

procedure TTensorF64.DivideBy(const aArr: TTensorF64);
begin
  TNDAArith.MapR(fArr, aArr.fArr, AddR_F64);
end;

procedure TTensorF64.DivideBy(const aValue: Double);
begin
  TNDAArith.MapR<Double>(fArr, aValue, DivR_F64);
end;

function TTensorF64.GetPart(const aIdx: INDIndexSeq): TTensorF64;
begin
  Result := fArr[aIdx];
end;

procedure TTensorF64.SetPart(const aIdx: INDIndexSeq; const aValue: TTensorF64);
begin
  fArr[aIdx] := aValue;
end;

function TTensorF64.GetShape: TNDAShape;
begin
  Result := fArr.Shape;
end;

{$endregion}

{$region 'ndaAllClose'}

function ndaAllClose(const aA, aB: INDArray<Integer>; aTol: Integer): Boolean;
begin
  Result := TNDAArith.AllClose<Integer>(aA, aB, TNDAArith.iEqCmp, aTol);
end;

function ndaAllClose(const aA, aB: INDArray<Single>; aTol: Single): Boolean;
begin
  Result := TNDAArith.AllClose<Single>(aA, aB, TNDAArith.sEqCmp, aTol);
end;

function ndaAllClose(const aA: INDArray<Single>; aB: Single; aTol: Single): Boolean;
begin
  Result := TNDAArith.AllClose<Single>(aA, aB, TNDAArith.sEqCmp, aTol);
end;

{$endregion}

{$region 'ndaAdd'}

function ndaAdd(const aArrays: array of INDArray<Integer>): INDArray<Integer>;
var I, count: Integer;
    arr: TTensorI32;
begin
  count := Length(aArrays);
  Assert(count > 0);
  arr := aArrays[0];
  for I := 1 to count - 1 do
    arr := arr + aArrays[I];
  Result := arr;
end;

function ndaAdd(const aArrays: array of INDArray<Int64>): INDArray<Int64>;
var I, count: Integer;
    arr: TTensorI64;
begin
  count := Length(aArrays);
  Assert(count > 0);
  arr := aArrays[0];
  for I := 1 to count - 1 do
    arr := arr + aArrays[I];
  Result := arr;
end;

function ndaAdd(const aArrays: array of INDArray<Single>): INDArray<Single>;
var I, count: Integer;
    arr: TTensorF32;
begin
  count := Length(aArrays);
  Assert(count > 0);
  arr := aArrays[0];
  for I := 1 to count - 1 do
    arr := arr + aArrays[I];
  Result := arr;
end;

function ndaAdd(const aArrays: array of INDArray<Double>): INDARray<Double>;
var I, count: Integer;
    arr: TTensorF64;
begin
  count := Length(aArrays);
  Assert(count > 0);
  arr := aArrays[0];
  for I := 1 to count - 1 do
    arr := arr + aArrays[I];
  Result := arr;
end;

function ndaAdd(const aArrays: array of INDArray; var aRes: INDArray): Boolean;
var resShape: TNDAShape;
    t: PTypeInfo;
    td: PTypeData;
begin
  Assert(Length(aArrays) > 0);
  Result := False;
  try
    if not CompatibleQ(aArrays, resShape, t) then exit;

    td := GetTypeData(t);
    case t^.Kind of
      tkInteger:
        case td^.OrdType of
//          otULong: aRes := ndaAdd(TNDACon.AsType<Cardinal>(aArrays));
          otSLong: aRes := ndaAdd(TNDAUt.AsType<Integer>(aArrays));
        else
          exit;
        end;

      tkFloat:
        case td^.FloatType  of
          ftSingle: aRes := ndaAdd(TNDAUt.AsType<Single>(aArrays));
          ftDouble: aRes := ndaAdd(TNDAUt.AsType<Double>(aArrays));
        else
          exit;
        end;

      tkInt64:
        if td^.MinInt64Value < 0 then
          aRes := ndaAdd(TNDAUt.AsType<Int64>(aArrays));
//        else
//          aRes := ndaAdd(TNDACon.AsType<UInt64>(aArrays));
    else
      exit;
    end;
    Result := True;
  except
    on ENDAError do begin

    end;
  end;
end;

{$endregion}

{$region 'ndaMultiply'}

function ndaMultiply(const aArrays: array of INDArray<Integer>): INDArray<Integer>;
var I, count: Integer;
    arr: TTensorI32;
begin
  count := Length(aArrays);
  Assert(count > 0);
  arr := aArrays[0];
  for I := 1 to count - 1 do
    arr := arr + aArrays[I];
  Result := arr;
end;

function ndaMultiply(const aArrays: array of INDArray<Int64>): INDArray<Int64>;
var I, count: Integer;
    arr: TTensorI64;
begin
  count := Length(aArrays);
  Assert(count > 0);
  arr := aArrays[0];
  for I := 1 to count - 1 do
    arr := arr * aArrays[I];
  Result := arr;
end;

function ndaMultiply(const aArrays: array of INDArray<Single>): INDArray<Single>;
var I, count: Integer;
    arr: TTensorF32;
begin
  count := Length(aArrays);
  Assert(count > 0);
  arr := aArrays[0];
  for I := 1 to count - 1 do
    arr := arr * aArrays[I];
  Result := arr;
end;

function ndaMultiply(const aArrays: array of INDArray<Double>): INDARray<Double>;
var I, count: Integer;
    arr: TTensorF64;
begin
  count := Length(aArrays);
  Assert(count > 0);
  arr := aArrays[0];
  for I := 1 to count - 1 do
    arr := arr * aArrays[I];
  Result := arr;
end;

function ndaMultiply(const aArrays: array of INDArray; var aRes: INDArray): Boolean;
var resShape: TNDAShape;
    t: PTypeInfo;
    td: PTypeData;
begin
  Assert(Length(aArrays) > 0);
  Result := False;
  try
    if not CompatibleQ(aArrays, resShape, t) then exit;

    td := GetTypeData(t);
    case t^.Kind of
      tkInteger:
        case td^.OrdType of
//          otULong: aRes := ndaAdd(TNDACon.AsType<Cardinal>(aArrays));
          otSLong: aRes := ndaMultiply(TNDAUt.AsType<Integer>(aArrays));
        else
          exit;
        end;

      tkFloat:
        case td^.FloatType  of
          ftSingle: aRes := ndaMultiply(TNDAUt.AsType<Single>(aArrays));
          ftDouble: aRes := ndaMultiply(TNDAUt.AsType<Double>(aArrays));
        else
          exit;
        end;

      tkInt64:
        if td^.MinInt64Value < 0 then
          aRes := ndaMultiply(TNDAUt.AsType<Int64>(aArrays));
//        else
//          aRes := ndaAdd(TNDACon.AsType<UInt64>(aArrays));
    else
      exit;
    end;
    Result := True;
  except
    on ENDAError do begin

    end;
  end;
end;

{$endregion}

end.
