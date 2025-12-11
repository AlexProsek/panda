unit panda.vCvt;

interface

procedure cvt_I32F32(N: NativeInt; X: PByte; XInc: NativeInt; Y: PByte; YInc: NativeInt);
procedure cvt_F32F64(N: NativeInt; X: PByte; XInc: NativeInt; Y: PByte; YInc: NativeInt);
procedure cvt_F64F32(N: NativeInt; X: PByte; XInc: NativeInt; Y: PByte; YInc: NativeInt);

implementation

uses
    panda.cvCvt
  ;

procedure cvt_I32F32(N: NativeInt; X: PByte; XInc: NativeInt; Y: PByte; YInc: NativeInt);
var pEnd: PByte;
begin
  if (XInc <> SizeOf(Integer)) or (YInc <> SizeOf(Single)) then begin
    pEnd := X + XInc * N;
    while X < pEnd do begin
      PSingle(Y)^ := PInteger(X)^;
      Inc(X, XInc);
      Inc(Y, YInc);
    end;
  end else
    cvt(PInteger(X), PSingle(Y), N);
end;

procedure cvt_F32F64(N: NativeInt; X: PByte; XInc: NativeInt; Y: PByte; YInc: NativeInt);
var pEnd: PByte;
begin
  if (XInc <> SizeOf(Single)) or (YInc <> SizeOf(Double)) then begin
    pEnd := X + XInc * N;
    while X < pEnd do begin
      PDouble(Y)^ := PSingle(X)^;
      Inc(X, XInc);
      Inc(Y, YInc);
    end;
  end else
    cvt(PSingle(X), PDouble(Y), N);
end;

procedure cvt_F64F32(N: NativeInt; X: PByte; XInc: NativeInt; Y: PByte; YInc: NativeInt);
var pEnd: PByte;
begin
  if (XInc <> SizeOf(Double)) or (YInc <> SizeOf(Single)) then begin
    pEnd := X + XInc * N;
    while X < pEnd do begin
      PSingle(Y)^ := PDouble(X)^;
      Inc(X, XInc);
      Inc(Y, YInc);
    end;
  end else
    cvt(PDouble(X), PSingle(Y), N);
end;

end.
