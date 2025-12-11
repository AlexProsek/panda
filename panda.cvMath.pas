unit panda.cvMath;

interface

uses
    System.Math
  ;

//{$define NoASM}

{$I AsmDefs.inc}

procedure cvRange(aData: PInteger; aCount: NativeInt; aLo, aStep: Integer); overload;
procedure cvRange(aData: PInt64; aCount: NativeInt; aLo, aStep: Int64); overload;
procedure cvRange(aData: PSingle; aCount: NativeInt; aLo, aStep: Single); overload;
procedure cvRange(aData: PDouble; aCount: NativeInt; aLo, aStep: Double); overload;

procedure cvExp(aData: PSingle; aCount: NativeInt); overload;
procedure cvExp(aData: PDouble; aCount: NativeInt); overload;

function cvTotal(aData: PInteger; aCount: NativeInt): Integer; overload;
function cvTotal(aData: PSingle; aCount: NativeInt): Single; overload;
function cvTotal(aData: PDouble; aCount: NativeInt): Double; overload;

implementation

{$region 'cvRange'}

procedure cvRange(aData: PInteger; aCount: NativeInt; aLo, aStep: Integer);
var pEnd: PByte;
begin
  pEnd := PByte(aData) + aCount * SizeOf(Integer);
  while PByte(aData) < pEnd do begin
    aData^ := aLo;
    Inc(aLo, aStep);
    Inc(aData);
  end;
end;

procedure cvRange(aData: PInt64; aCount: NativeInt; aLo, aStep: Int64);
var pEnd: PByte;
begin
  pEnd := PByte(aData) + aCount * SizeOf(Int64);
  while PByte(aData) < pEnd do begin
    aData^ := aLo;
    Inc(aLo, aStep);
    Inc(aData);
  end;
end;

procedure cvRange(aData: PSingle; aCount: NativeInt; aLo, aStep: Single);
var pEnd: PByte;
begin
  pEnd := PByte(aData) + aCount * SizeOf(Single);
  while PByte(aData) < pEnd do begin
    aData^ := aLo;
    aLo := aLo + aStep;
    Inc(aData);
  end;
end;

procedure cvRange(aData: PDouble; aCount: NativeInt; aLo, aStep: Double);
var pEnd: PByte;
begin
  pEnd := PByte(aData) + aCount * SizeOf(Double);
  while PByte(aData) < pEnd do begin
    aData^ := aLo;
    aLo := aLo + aStep;
    Inc(aData);
  end;
end;

{$endregion}

{$region 'cvExp'}

procedure cvExp(aData: PSingle; aCount: NativeInt);
var pEnd: PByte;
begin
  pEnd := PByte(aData) + aCount * SizeOf(Single);
  while PByte(aData) < pEnd do begin
    aData^ := System.Exp(aData^);
    Inc(aData);
  end;
end;

procedure cvExp(aData: PDouble; aCount: NativeInt);
var pEnd: PByte;
begin
  pEnd := PByte(aData) + aCount * SizeOf(Double);
  while PByte(aData) < pEnd do begin
    aData^ := System.Exp(aData^);
    Inc(aData);
  end;
end;

{$endregion}

{$region 'cvTotal'}

function cvTotal(aData: PInteger; aCount: NativeInt): Integer;
{$if defined(ASMx64)}
// RCX <- aData, RDX <- aCount, RAX <- result
asm
  xorps xmm0, xmm0
  mov r8, rdx
  shr rdx, 3
  jz @rest
@L:
  movupd xmm1, [rcx]
  paddd xmm0, xmm1
  add rcx, 16
  movupd xmm1, [rcx]
  paddd xmm0, xmm1
  add rcx, 16
  dec rdx
  jnz @L

  // Horizontal sum of xmm0
  movhlps xmm1, xmm0
  paddd xmm0, xmm1
  movaps xmm1, xmm0
  shufps xmm1, xmm1, 1
  paddd xmm0, xmm1

@rest:
  and r8, 7
  jz @end
@Lrest:
  movss xmm1, [rcx]
  paddd xmm0, xmm1
  add rcx, 4
  dec r8
  jnz @Lrest

@end:
  movd eax, xmm0
end;
{$else}
var pEnd: PByte;
begin
  Result := 0;
  pEnd := PByte(aData) + aCount * SizeOf(Integer);
  while PByte(aData) < pEnd do begin
    Inc(Result, aData^);
    Inc(aData);
  end;
end;
{$endif}

function cvTotal(aData: PSingle; aCount: NativeInt): Single;
{$if defined(ASMx64)}
// RCX <- aData, RDX <- aCount, xmm0 <- result
asm
  xorps xmm0, xmm0
  mov r8, rdx
  shr rdx, 3
  jz @rest
@L:
  movups xmm1, [rcx]
  addps xmm0, xmm1
  add rcx, 16
  movups xmm1, [rcx]
  addps xmm0, xmm1
  add rcx, 16
  dec rdx
  jnz @L

  // Horizontal sum of xmm0
  movhlps xmm1, xmm0
  addps xmm0, xmm1
  movaps xmm1, xmm0
  shufps xmm1, xmm1, 1
  addss xmm0, xmm1

@rest:
  and r8, 7
  jz @end
@Lrest:
  movss xmm1, [rcx]
  addss xmm0, xmm1
  add rcx, 4
  dec r8
  jnz @Lrest

@end:
end;
{$else}
var pEnd: PByte;
    res: Double;
begin
  res := 0;
  pEnd := PByte(aData) + aCount * SizeOf(Single);
  while PByte(aData) < pEnd do begin
    res := res + aData^;
    Inc(aData);
  end;
  Result := Single(res);
end;
{$endif}

function cvTotal(aData: PDouble; aCount: NativeInt): Double;
{$if defined(ASMx64)}
// RCX <- aData, RDX <- aCount, xmm0 <- result
asm
  xorps xmm0, xmm0
  mov r8, rdx
  shr rdx, 2
  jz @rest
@L:
  movups xmm1, [rcx]
  addpd xmm0, xmm1
  add rcx, 16
  movups xmm1, [rcx]
  addpd xmm0, xmm1
  add rcx, 16
  dec rdx
  jnz @L

  // Horizontal sum of xmm0
  movhlps xmm1, xmm0
  addsd xmm0, xmm1

@rest:
  and r8, 3
  jz @end
@Lrest:
  movsd xmm1, [rcx]
  addsd xmm0, xmm1
  add rcx, 8
  dec r8
  jnz @Lrest

@end:
end;
{$else}
var pEnd: PByte;
begin
  Result := 0;
  pEnd := PByte(aData) + aCount * SizeOf(Double);
  while PByte(aData) < pEnd do begin
    Result := Result + aData^;
    Inc(aData);
  end;
end;
{$endif}

{$endregion}

end.
