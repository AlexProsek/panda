unit panda.vCvt;

interface

procedure cvt_UI8F32(N: NativeInt; X: PByte; XInc: NativeInt; Y: PByte; Yinc: NativeInt);
procedure cvt_I32F32(N: NativeInt; X: PByte; XInc: NativeInt; Y: PByte; YInc: NativeInt);
procedure cvt_I32F64(N: NativeInt; X: PByte; XInc: NativeInt; Y: PByte; YInc: NativeInt);
procedure cvt_I32I64(N: NativeInt; X: PByte; XInc: NativeInt; Y: PByte; YInc: NativeInt);
procedure cvt_I32C128(N: NativeInt; X: PByte; XInc: NativeInt; Y: PByte; YInc: NativeInt);
procedure cvt_I64F64(N: NativeInt; X: PByte; XInc: NativeInt; Y: PByte; YInc: NativeInt);
procedure cvt_I64C128(N: NativeInt; X: PByte; XInc: NativeInt; Y: PByte; YInc: NativeInt);
procedure cvt_F32UI8(N: NativeInt; X: PByte; XInc: NativeInt; Y: PByte; YInc: NativeInt);
procedure cvt_F32F64(N: NativeInt; X: PByte; XInc: NativeInt; Y: PByte; YInc: NativeInt);
procedure cvt_F64F32(N: NativeInt; X: PByte; XInc: NativeInt; Y: PByte; YInc: NativeInt);
procedure cvt_F64C128(N: NativeInt; X: PByte; XInc: NativeInt; Y: PByte; YInc: NativeInt);

implementation

uses
    panda.cvCvt
  , panda.Nums
  ;

procedure cvt_UI8F32(N: NativeInt; X: PByte; XInc: NativeInt; Y: PByte; Yinc: NativeInt);
var pEnd: PByte;
begin
  if (XInc <> SizeOf(Byte)) or (YInc <> SizeOf(Single)) then begin
    pEnd := Y + YInc * N;
    while Y <> pEnd do begin
      PSingle(Y)^ := X^;
      Inc(X, XInc);
      Inc(Y, YInc);
    end;
  end else
    cvt(PByte(X), PSingle(Y), N);
end;

procedure cvt_I32F32(N: NativeInt; X: PByte; XInc: NativeInt; Y: PByte; YInc: NativeInt);
var pEnd: PByte;
begin
  if (XInc <> SizeOf(Integer)) or (YInc <> SizeOf(Single)) then begin
    pEnd := Y + YInc * N;
    while Y <> pEnd do begin
      PSingle(Y)^ := PInteger(X)^;
      Inc(X, XInc);
      Inc(Y, YInc);
    end;
  end else
    cvt(PInteger(X), PSingle(Y), N);
end;

procedure cvt_I32F64(N: NativeInt; X: PByte; XInc: NativeInt; Y: PByte; YInc: NativeInt);
var pEnd: PByte;
begin
  if (XInc <> SizeOf(Integer)) or (YInc <> SizeOf(Double)) then begin
    pEnd := Y + YInc * N;
    while Y <> pEnd do begin
      PDouble(Y)^ := PInteger(X)^;
      Inc(X, XInc);
      Inc(Y, YInc);
    end;
  end else
    cvt(PInteger(X), PDouble(Y), N);
end;

procedure cvt_I32I64(N: NativeInt; X: PByte; XInc: NativeInt; Y: PByte; YInc: NativeInt);
var pEnd: PByte;
begin
  if (XInc <> SizeOf(Integer)) or (YInc <> SizeOf(Int64)) then begin
    pEnd := Y + YInc * N;
    while Y <> pEnd do begin
      PInt64(Y)^ := PInteger(X)^;
      Inc(X, XInc);
      Inc(Y, YInc);
    end;
  end else
    cvt(PInteger(X), PInt64(Y), N);
end;

procedure cvt_I32C128(N: NativeInt; X: PByte; XInc: NativeInt; Y: PByte; YInc: NativeInt);
var pEnd: PByte;
begin
  pEnd := Y + YInc * N;
  while Y <> pEnd do begin
    with PCmplx128(Y)^ do begin
      Re := PInteger(X)^;
      Im := 0;
    end;
    Inc(X, XInc);
    Inc(Y, YInc);
  end;
end;

procedure cvt_I64F64(N: NativeInt; X: PByte; XInc: NativeInt; Y: PByte; YInc: NativeInt);
var pEnd: PByte;
begin
  if (XInc <> SizeOf(Int64)) or (YInc <> SizeOf(Double)) then begin
    pEnd := Y + YInc * N;
    while Y <> pEnd do begin
      PDouble(Y)^ := PInt64(X)^;
      Inc(X, XInc);
      Inc(Y, YInc);
    end;
  end else
    cvt(PInt64(X), PDouble(Y), N);
end;

procedure cvt_I64C128(N: NativeInt; X: PByte; XInc: NativeInt; Y: PByte; YInc: NativeInt);
var pEnd: PByte;
begin
  pEnd := Y + YInc * N;
  while Y <> pEnd do begin
    with PCmplx128(Y)^ do begin
      Re := PInt64(X)^;
      Im := 0;
    end;
    Inc(X, XInc);
    Inc(Y, YInc);
  end;
end;

procedure cvt_F32UI8(N: NativeInt; X: PByte; XInc: NativeInt; Y: PByte; YInc: NativeInt);
var pEnd: PByte;
    s: NativeInt;
begin
  if (XInc <> SizeOf(Single)) or (YInc <> SizeOf(Byte)) then begin
    pEnd := Y + YInc * N;
    while Y <> pEnd do begin
      s := Round(PSingle(X)^);
      if s < 0 then
        Y^ := 0
      else if s > 255 then
        Y^ := 255
      else
        Y^ := s;
      Inc(X, XInc);
      Inc(Y, YInc);
    end;
  end else
    cvt(PSingle(X), PByte(Y), N);
end;

procedure cvt_F32F64(N: NativeInt; X: PByte; XInc: NativeInt; Y: PByte; YInc: NativeInt);
var pEnd: PByte;
begin
  if (XInc <> SizeOf(Single)) or (YInc <> SizeOf(Double)) then begin
    pEnd := Y + YInc * N;
    while Y <> pEnd do begin
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
    pEnd := Y + YInc * N;
    while Y <> pEnd do begin
      PSingle(Y)^ := PDouble(X)^;
      Inc(X, XInc);
      Inc(Y, YInc);
    end;
  end else
    cvt(PDouble(X), PSingle(Y), N);
end;

procedure cvt_F64C128(N: NativeInt; X: PByte; XInc: NativeInt; Y: PByte; YInc: NativeInt);
var pEnd: PByte;
begin
  pEnd := Y + YInc * N;
  while Y <> pEnd do begin
    with PCmplx128(Y)^ do begin
      Re := PDouble(X)^;
      Im := 0;
    end;
    Inc(X, XInc);
    Inc(Y, YInc);
  end;
end;

end.
