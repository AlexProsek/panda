unit panda.ArrCmp;

interface

uses
    panda.Intfs
  , panda.Arrays
  , panda.Nums
  , panda.cvArithmetic
  , System.Math
  ;

procedure ndaMax(const aA: Single; const aB: INDArray<Single>; var aRes: INDArray<Single>); overload;
procedure ndaMax(const aA, aB: INDArray<Single>; var aRes: INDArray<Single>); overload;


implementation

{$EXCESSPRECISION OFF} // to prevent Single -> Double conversion by x64 compiler

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

end.
