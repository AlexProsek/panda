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

end.
