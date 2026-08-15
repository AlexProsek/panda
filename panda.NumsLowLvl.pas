unit panda.NumsLowLvl;

interface

{$I AsmDefs.inc}

type
{$ifdef Limb64}
  TLimb = UInt64;
{$else}
  TLimb = Cardinal;
{$endif}
  PLimb = ^TLimb;
  TLimbArray = TArray<TLimb>;

const
  I64_HI_BIT      = $8000000000000000;
  I64_SIGN_BIT    = I64_HI_BIT;
  I64_MASK        = $FFFFFFFFFFFFFFFF;
  I64_BIT_COUNT   = 64;

  I32_HI_BIT      = $80000000;
  I32_SIGN_BIT    = I32_HI_BIT;
  I32_MASK        = $FFFFFFFF;
  I32_BIT_COUNT   = 32;

  F64_EXP_BIAS    = 1023;
  F64_FRAC_OFFSET = 12;
  F64_FRAC_MASK   = $000FFFFFFFFFFFFF;

{$ifdef Limb64}
  W_HI_BIT          = I64_HI_BIT;
  W_BIT_COUNT       = I64_BIT_COUNT;
  W_LOG2_BITS       = 6; // Log2(W_BIT_COUNT)
  W_LOG2_BYTES      = 3;
  W_MASK            = I64_MASK;
  W_HI_MASK         = $FFFFFFFF00000000;
  W_LO_MASK         = $00000000FFFFFFFF;
{$else}
  W_HI_BIT          = I32_HI_BIT;
  W_BIT_COUNT       = I32_BIT_COUNT;
  W_LOG2_BITS       = 5;
  W_LOG2_BYTES      = 2;
  W_MASK            = I32_MASK;
  W_HI_MASK         = $FFFF0000;
  W_LO_MASK         = $0000FFFF;
{$endif}
  W_SIGN_BIT        = W_HI_BIT;
  W_BYTE_COUNT      = W_BIT_COUNT div 8;

  cLimbSize     = SizeOf(TLimb);
{$ifdef Limb64}
  cLimbBits     = 64;
  cTopBitIdx    = 63;
  cTopBit       = UInt64(1) shl 63;
  cLimbRemMask  = $3f;
{$else}
  cLimbBits     = 32;
  cTopBitIdx    = 31;
  cWordBits     = 32;
  cTopBit       = Cardinal(1) shl 31;
  cLimbRemMask  = $1f;
{$endif}
  cNibbleMask   = $f;
  cHexNumSymbols: array [0..cNibbleMask] of Char =
    ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');


/// <summary>
///   Evaluates in-place addition <c>B <- A + B</c>. It's supposed that <c>A</c> and <c>B</c> are
///   positive numbers. It's also supposed that the both numbers have the same size.
/// </summary>
/// <remarks>
///   <para>
///     &#x2022; Function returns 1 when result overflows the output buffer
///   </para>
/// </remarks>
function _ipAdd(pA, pB: Pointer; n: NativeInt): TLimb;
/// <summary>
///   Evaluates in-place subtraction <c>B <- A - B</c>. It's supposed that <c>A</c> and <c>B</c> are
///   positive numbers and <c>A</c> is greather than or equal to the <c>B</c>. It's also
///   supposed that the both numbers have the same size.
/// </summary>
procedure _ipSub(pA, pB: Pointer; n: NativeInt);
/// <summary>
///   Evaluates <c>A + D</c>, where <c>D</c> is machine precision unsigned integer.
///   Result is stored in <c>A</c> buffer.
/// </summary>
function _Inc(pA: Pointer; D: TLimb; n: NativeInt): TLimb;

procedure _Dec(pA, pB: Pointer; n: NativeInt);

procedure _Mul(pA, pB, pRes: PByte; nA, nB: NativeInt);
procedure _ShortMul(pA, pB, pRes: PByte; nA: NativeInt);
function _ShortDivMod(pA, pB, pRes: PByte; nA: NativeInt): TLimb;
procedure _LongDivMod(pA, pB, pQ: PByte; nA: NativeInt; nB: NativeInt);

/// <summary>
///   Executes left bit shift.
/// </summary>
/// <param name="pSrc">Points to the  source data.</param>
/// <param name="pDst">Points to the  of the destination data.</param>
/// <param name="Count">
///   Number of bits. Result will be shifted by this count. It's supposed that
///   <c>Count</c> is less than or eqaul 31 (number of bits of the word).
/// </param>
/// <param name="Nsrc">Number of source's words.</param>
/// <returns>
///   Carry from the heighest word.
/// </returns>
function _shl(pSrc, pDst: PByte; count: Byte; Nsrc: NativeInt): TLimb;
/// <summary>
///   Executes right bit shift.
/// </summary>
/// <param name="pSrc">Points to the source data.</param>
/// <param name="pDst">Points to the the destination data.</param>
/// <param name="Count">
///   Number of bits. Result will be shifted by this count. It's supposed that
///   <c>Count</c> is less than or eqaul 31 (number of bits of the word).
/// </param>
/// <param name="Nsrc">Number of source's limbs.</param>
function _shr(pSrc, pDst: PByte; count: Byte; Nsrc: NativeInt): TLimb;
/// <summary>
///   Executes bit shift in the direction according to <c>aDigits</c> signum
/// </summary>
/// <remarks>
///   <para>
///     &#x2022; For <c>aDigits > 0</c> is executed left shift and for
///     <c>aDigits</c> is executed right shift.
///   </para>
///   <para>
///     &#x2022; Unlinke <c>_shr</c>, <c>_shl</c> the <c>aDigits</c> can be arbitrary number.
///   </para>
/// </remarks>
procedure _ipLongShift(pA: PLimb; aCount: Integer; aDigits: NativeInt);

/// <summary>
///   Finds offset of the most significant bit of the number <c>A</c>.
/// </summary>
/// <remarks>
///   Index 1 corresponds with the least significant digit. If the <c>A</c>
///   is equal to zero then function returns zero.
/// </remarks>
function _TopBitPos(A: TLimb): NativeInt; overload;
function _TopBitPos(pA: PLimb; N: NativeInt): NativeInt; overload;
function _CountLeadingZeros(A: TLimb): NativeInt; {$ifndef ASM}inline;{$endif} overload;
function _CountLeadingZeros(pA: PLimb; N: NativeInt): NativeInt; overload;
function _CountTrailingZeros(A: TLimb): NativeInt; overload;
function _IsZero(pA: PLimb; N: NativeInt): Boolean;
/// <summary>
///   Returns the number of limbs without leading zero limbs.
/// </summary>
/// <param name="pA">points to the buffer</param>
/// <param name="N">length of the buffer (number of the buffer's limbs)</param>
function _Length(pA: PByte; N: NativeInt): NativeInt;

function _GetLimb(pA: PByte; I: NativeInt): TLimb; inline;

{$region 'helper functions'}

function GCD(A, B: UInt64): UInt64;
function HiLimbBaseQuotient(aValue: TLimb): TLimb; {$ifndef CPUx64}inline;{$endif}
function IntPwr(A: TLimb; aExponent: Integer): TLimb;

{$endregion}

implementation

{$region 'helper functions'}

function GCD(A, B: UInt64): UInt64;

  procedure CheckOrder(var A, B: UInt64);
  var tmp: UInt64;
  begin
    if A < B then begin
      tmp := A;
      A := B;
      B := tmp;
    end;
  end;

var tmp: UInt64;
begin
  if (A = 0) and (B = 0) then exit(0);

  CheckOrder(A, B);
  while B <> 0 do begin
    tmp := B;
    B := A mod B;
    A := tmp;
    CheckOrder(A, B);
  end;
  Result := A;
end;

function HiLimbBaseQuotient(aValue: TLimb): TLimb;
{$if defined(ASMx64)}
asm
  // rcx <- aValue
  cmp rcx, $FFFFFFFFFFFFFFFF
  jne @Q
  mov rax, 1
  ret
@Q:
  inc rcx
  mov rdx, 1
  xor rax, rax
  div rcx
end;
{$else}
begin
  Result := $100000000 div (UInt64(aValue) + 1);
end;
{$endif}

function IntPwr(A: TLimb; aExponent: Integer): TLimb;
var Y, Z: TLimb;
    N: Integer absolute aExponent;
    t: Integer;
begin
  Assert(aExponent > 0);
  Y := 1;
  Z := A;
  while True do begin
    t := N and 1;
    N := N shr 1;
    if t = 1 then Y := Z * Y;
    if N = 0 then break;
    Z := Z * Z;
  end;
  Result := Y;
end;

{$endregion}

function _ipAdd(pA, pB: Pointer; n: NativeInt): TLimb;
{$if defined(ASMx86)}
asm
  //pA -> EAX, pB -> EDX, n -> ECX
  push ebx
  push edi
  push esi
  mov esi, eax
  mov edi, edx
  xor edx, edx //EDX will be used to store a carry flag
@L:
  bt edx, 0 //set the carry flag
  mov eax, [esi]
  mov ebx, [edi]
  adc eax, ebx
  mov edx, 0
  adc edx, 0
  mov [edi], eax
  add esi, 4
  add edi, 4
  dec ecx
  jnz @L

  mov eax, edx //return the last carry flag
  pop esi
  pop edi
  pop ebx
end;
{$elseif defined(ASMx64)}
asm
  // pA -> RCX, pB -> RDX, n -> R8
  mov r10, rcx
  mov r11, rdx
  mov rcx, r8
  shr rcx, 1
  jz @rest
  xor rdx, rdx // RDX will used for carry flag
  // pA -> R10, pB -> R11
@L:
  bt rdx, 0 // set the carry flag
  mov rax, [r10]
  adc rax, [r11]
  mov rdx, 0
  adc rdx, 0
  mov [r11], rax
  add r10, 8
  add r11, 8

  bt rdx, 0
  mov rax, [r10]
  adc rax, [r11]
  mov rdx, 0
  adc rdx, 0
  mov [r11], rax
  add r10, 8
  add r11, 8

  dec rcx
  jnz @L

@rest:
  and r8, 1
  jz @end
  bt rdx, 0
  mov rax, [r10]
  adc rax, [r11]
  mov rdx, 0
  adc rdx, 0
  mov [r11], rax
@end:
  mov rax, rdx
end;
{$else}
var tmp: UInt64;
    pair: array [0..1] of Cardinal absolute tmp;
    carry: Cardinal;
    pAc, pBc: PCardinal;
    pEnd: PByte;
begin
  carry := 0;
  pAc := pA;
  pBc := pB;
  pEnd := PByte(pA) + n * SizeOf(Cardinal);
  while PByte(pAc) < pEnd do begin
    tmp := pAc^;
    Inc(tmp, pBc^);
    Inc(tmp, carry);
    pBc^ := pair[0];
    carry := pair[1];
    Inc(pAc);
    Inc(pBc);
  end;
  Result := carry;
end;
{$endif}

procedure _ipSub(pA, pB: Pointer; n: NativeInt);
{$if defined(ASMx86)}
asm
  //pA -> EAX, pB -> EDX, n -> ECX
  push ebx
  push esi
  push edi
  mov esi, eax
  mov edi, edx
  xor edx, edx //EDX will be used to store a carry flag
@L:
  bt edx, 0 //set the carry flag
  mov eax, [esi]
  mov ebx, [edi]
  sbb eax, ebx
  mov edx, 0
  adc edx, 0
  mov [edi], eax
  add esi, 4
  add edi, 4
  dec ecx
  jnz @L

  pop edi
  pop esi
  pop ebx
end;
{$elseif defined(ASMx64)}
asm
  // pA -> RCX, pB -> RDX, n -> R8
  mov r10, rcx
  mov r11, rdx
  mov rcx, r8
  shr rcx, 1
  jz @rest
  xor rdx, rdx
@L:
  bt rdx, 0 // set the carry flag
  mov rax, [r10]
  sbb rax, [r11]
  mov rdx, 0
  adc rdx, 0
  mov [r11], rax
  add r10, 8
  add r11, 8

  bt rdx, 0
  mov rax, [r10]
  sbb rax, [r11]
  mov rdx, 0
  adc rdx, 0
  mov [r11], rax
  add r10, 8
  add r11, 8

  dec rcx
  jnz @L

@rest:
  and r8, 1
  jz @end
  bt rdx, 0
  mov rax, [r10]
  sbb rax, [r11]
  mov rdx, 0
  adc rdx, 0
  mov [r11], rax
@end:
end;
{$else}
var tmp: UInt64;
    pair: array [0..1] of Cardinal absolute tmp;
    carry: Cardinal;
    pAc, pBc: PCardinal;
    pEnd: PByte;
begin
  carry := 0;
  pAc := pA;
  pBc := pB;
  pEnd := PByte(pA) + n * cLimbSize;
  while PByte(pAc) < pEnd do begin
    tmp := pAc^;
    pair[1] := 1;
    Dec(tmp, pBc^);
    Dec(tmp, carry);
    pBc^ := pair[0];
    carry := 1 xor pair[1];
    Inc(pAc);
    Inc(pBc);
  end;
end;
{$endif}

function _Inc(pA: Pointer; D: TLimb; n: NativeInt): TLimb;
{$if defined(ASMx86)}
asm
  //EAX <- pA, EDX <- D, ECX <- n
  push esi
  mov esi, eax
@L:
  mov eax, [esi]
  add eax, edx
  mov edx, 0
  adc edx, 0
  mov [esi], eax
  add esi, 4
  dec ecx
  jnz @L

  mov eax, edx
  pop esi
end;
{$elseif defined(ASMx64)}
asm
  // RCX <- pA, RDX <- D, R8 <- n
@L:
  mov rax, [rcx]
  add rax, rdx
  mov rdx, 0
  adc rdx, 0
  mov [rcx], rax
  add rcx, 8
  dec r8
  jnz @L

  mov rax, rdx
end;
{$else}
var tmp: UInt64;
    pair: array [0..1] of Cardinal absolute tmp;
    pAc: PCardinal;
    pEnd: PByte;
begin
  pAc := PCardinal(pA);
  pEnd := pA + n * cLimbSize;
  while PByte(pAc) < pEnd do begin
    tmp := pAc^;
    Inc(tmp, D);
    pAc^ := pair[0];
    D := pair[1];
    Inc(pAc);
  end;
  Result := D;
end;
{$endif}

procedure _Dec(pA, pB: Pointer; n: NativeInt);
{$if defined(ASMx86)}
asm
  //pA -> EAX, pB -> EDX, n -> ECX
  push ebx
  push esi
  push edi
  mov esi, eax
  mov edi, edx
  xor edx, edx //EDX will be used to store a carry flag
@L:
  bt edx, 0 //set the carry flag
  mov eax, [esi]
  mov ebx, [edi]
  sbb eax, ebx
  mov edx, 0
  adc edx, 0
  mov [esi], eax
  add esi, 4
  add edi, 4
  dec ecx
  jnz @L

  pop edi
  pop esi
  pop ebx
end;
{$elseif defined(ASMx64)}
asm
  mov r10, rcx
  mov r11, rdx
  xor rdx, rdx
@L:
  bt rdx, 0 // set the carry flag
  mov rax, [r10]
  sbb rax, [r11]
  mov rdx, 0
  adc rdx, 0
  mov [r10], rax
  add r10, 8
  add r11, 8
  dec r8
  jnz @L
end;
{$else}
var tmp: UInt64;
    pair: array [0..1] of Cardinal absolute tmp;
    carry: Cardinal;
    pEnd: PByte;
begin
  carry := 0;
  pEnd := PByte(pA) + n * cLimbSize;
  while PByte(pA) < pEnd do begin
    tmp := PCardinal(pA)^;
    pair[1] := 1;
    Dec(tmp, PCardinal(pB)^);
    Dec(tmp, carry);
    PCardinal(pA)^ := pair[0];
    carry := 1 xor pair[1];
    Inc(PCardinal(pA));
    Inc(PCardinal(pB));
  end;
end;
{$endif}

procedure _Mul(pA, pB, pRes: PByte; nA, nB: NativeInt);
{$if defined(ASMx86)}
asm
  push esi
  push edi
  //EAX <- pA, EDX <- pB, ECX <- pRes, [ESP + 12] <- nA, [ESP + 8] <- nB
  mov esi, eax //ESI <- pA
  mov edi, ecx //EDI <- pRes
  push ebx
  push 0 //k in [ESP + $C]
  push 0 //j in [ESP + $8] {j = B's index}
  push 0 //i in [ESP + $4] {i = A's index}
  push edx //[ESP] contains pB

@M3: //outer loop; initialize inner loop (i <- 0, k <- 0)
  mov dword ptr [esp + $4], 0
  mov dword ptr [esp + $C], 0
  mov ecx, [esp + $4]
@M4: //inner loop
  mov eax, [esi + 4 * ecx] //EAX <- u_i
  mov ebx, [esp]
  mov ebx, [ebx]
  mul ebx //(EAX:EDX) <- u_i x v_j
  add ecx, [esp + $8] //ECX <- i + j
  add eax, [edi + 4 * ecx] //EAX += w_{i + j}
  adc edx, 0 //(EAX:EDX) <- u_i x v_j + w_{i + j}
  add eax, [esp + $C]
  adc edx, 0 // t := (EAX:EDX) <- u_i x v_j + w_{i + j} + k
  mov [edi + 4 * ecx], eax // w_{i + j} <- t mod base
  mov [esp + $c], edx //k <- Floor(t / base)
//M5: Inc(i); if (i < nA) then @M4 else w_{j+m} <- k
  mov ecx, [esp + $4]
  inc ecx
  cmp ecx, nA
  je @E1
  mov [esp + $4], ecx //Inc(i)
  jmp @M4
@E1: //end of inner loop
  mov ecx, [esp + $8] //ECX <- j
  add ecx, nA
  mov [edi + 4 * ecx], edx // w_{j + m} <- k
//M6: Inc(j); if (j < nB) then @M3 else @E
  mov ecx, [esp + $8]
  inc ecx
  cmp ecx, nB
  je @E2
  mov [esp + $8], ecx
  add [esp], 4
  jmp @M3
@E2: //end of outer loop
  add esp, $10 //skip local vars
  pop ebx
  pop edi
  pop esi
end;
{$elseif defined(ASMx64)}
asm
  // pA -> RCX, pB -> RDX, pRes -> R8, nA -> R9
  push rdi
  mov r10, rcx // R10 <- pA
  mov r11, rdx // R11 <- pB
  mov rdi, r8 // RDI <- pRes
  push 0 // k in [RSP + 8]
  push 0 // j in [RSP] {j = B's index}
@M3: //outer loop; initialize inner loop (i <- 0, k <- 0)
  mov qword ptr [rsp + 8], 0 // k <- 0
  xor rcx, rcx // RCX <- i = 0 (A's index)
@M4: //inner loop
  mov rax, [r10 + 8 * rcx] // RAX <- u_i
  mul qword ptr [r11] // (RAX:RDX) <- u_i x v_j
  mov r8, rcx
  add r8, [rsp] // R8 <- i + j
  add rax, [rdi + 8 * r8] // RAX += w_{i + j}
  adc rdx, 0 // (RAX:RDX) <- u_i x v_j + w_{i + j}
  add rax, [rsp + 8]
  adc rdx, 0 // t := (RAX:RDX) <- u_i x v_j + w_{i + j} + k
  mov [rdi + 8 * r8], rax // w_{i + j} <- t mod base
  mov [rsp + 8], rdx // k <- Floor(t / base)
//M5: Inc(i); if (i < nA) then @M4 else w_{j+m} <- k
  inc rcx
  cmp rcx, nA
  je @E1
  jmp @M4
@E1: //end of inner loop
  mov rcx, [rsp] // RCX <- j
  mov rax, rcx
  add rax, nA
  mov [rdi + 8 * rax], rdx // w_{j + m} <- k
//M6: Inc(j); if (j < nB) then @M3 else @E
  inc rcx
  cmp rcx, nB
  je @E2
  mov [rsp], rcx
  add r11, 8
  jmp @M3
@E2: //end of outer loop
  add rsp, 16 //skip local vars (j, k)
  pop rdi
end;
{$else}
var I, J: Integer;
    K: Cardinal;
    tmp, V: UInt64;
    pair: array [0..1] of Cardinal absolute tmp;
    pW: PByte;
begin
  for J := 0 to nB - 1 do begin
    V := PCardinal(pB + J * cI32Sz)^;// V <- v_j
    if V = 0 then
      PCardinal(pRes + (J + nA) * cI32Sz)^ := 0
    else begin
      K := 0;
      for I := 0 to nA - 1 do begin
        pW := pRes + (I + J) * cI32Sz;
        tmp := PCardinal(pA + I * cI32Sz)^; // tmp <- u_i
        tmp := tmp * V;
        Inc(tmp, PCardinal(pW{w_i+j})^);
        Inc(tmp, K{carry});
        PCardinal(pW)^ := pair[0];
        K := pair[1];
      end;
      PCardinal(pRes + (J + nA) * cI32Sz)^ := K;
    end;
  end;
end;
{$endif}

procedure _ShortMul(pA, pB, pRes: PByte; nA: NativeInt);
{$if defined(ASMx86)}
asm
  // EAX <- pA, EDX <- pB, ECX <- pRes, [ESP] <- nA
  push esi
  push edi
  push ebx

  mov esi, eax
  mov edi, ecx
  mov ecx, [esp + 20]
  mov eax, [edx]
  mov [esp + 20], eax // [ESP + 12] <- B
  xor ebx, ebx // EBX <- 0, EBX is used for carry of (A[j] * B) div 2^b

@L:
  mov eax, [esi]
  mul [esp + 20] // (EAX:EDX) <- A[i] * B
  add eax, ebx // add carry
  adc edx, 0
  mov [edi], eax
  mov ebx, edx
  add esi, 4
  add edi, 4
  dec ecx
  jnz @L

  mov [edi], ebx

  pop ebx
  pop edi
  pop esi
end;
{$elseif defined(ASMx64)}
asm
  // RCX <- pA, RDX <- pB, R8 <- pRes, R9 <- nA
{$ifndef unroll2}
  mov r10, rcx  // R10 <- pA
  mov r11, r8   // R11 <- pRes
  mov rcx, r9   // RCX <- nA
  mov r8, [rdx] // R8 <- B
  xor r9, r9 // R9 is used for carry (A[j] * B) div 2^b

@L:
  mov rax, [r10]
  mul r8 // (RAX:RDX) <- A[i] * B
  add rax, r9 // add carry
  adc rdx, 0
  mov [r11], rax
  mov r9, rdx
  add r10, 8
  add r11, 8
  dec rcx
  jnz @L

  mov [r11], r9
{$else}
  mov r10, rcx  // R10 <- pA
  mov r11, r8   // R11 <- pRes
  mov rcx, r9   // RCX <- nA
  mov r8, [rdx] // R8 <- B
  xor r9, r9 // R9 is used for carry (A[j] * B) div 2^b

  mov rax, rcx
  and rax, 1
  jz @S

  mov rax, [r10]
  mul r8
  add rax, r9
  adc rdx, 0
  mov [r11], rax
  mov r9, rdx
  add r10, 8
  add r11, 8

@S:
  shr rcx, 1 // RCX <- nA div 2
  jz @E

@L:
  mov rax, [r10]
  mul r8 // (RAX:RDX) <- A[i] * B
  add rax, r9 // add carry
  adc rdx, 0
  mov [r11], rax
  mov r9, rdx
  add r10, 8
  add r11, 8

  mov rax, [r10]
  mul r8
  add rax, r9
  adc rdx, 0
  mov [r11], rax
  mov r9, rdx
  add r10, 8
  add r11, 8
  dec rcx
  jnz @L

@E:
  mov [r11], r9
{$endif}
end;
{$else}
begin

end;
{$endif}

function _ShortDivMod(pA, pB, pRes: PByte; nA: NativeInt): TLimb;
{$if defined(ASMx86)}
//EAX <- pA, EDX <- pB, ECX <- pRes, [ESP + $c] <- nA
asm
  push ebx
  push edi
  push esi
  mov ebx, [edx]      //EBX <- pB^
  mov edi, ecx        //EDI <- pRes
  mov ecx, nA         //ECX <- NA
  mov esi, eax        //ESI <- pA

  lea esi, [esi + 4 * ecx - 4] //ESI <- @u_{n - 1}
  lea edi, [edi + 4 * ecx - 4] //EDI <- @w_{n - 1}
  xor edx, edx //EDX <- r := 0
@L:
  mov eax, [esi] //(EAX:EDX) <- r * b + u_j
  div ebx //EAX <- (r*b + u_j) div v; EDX <- (r*b + u_j) mod v
  mov [edi], eax //store quotient
  sub esi, 4
  sub edi, 4
  dec ecx
  jnz @L

  mov eax, edx //returns remainder
  pop esi
  pop edi
  pop ebx
end;
{$elseif defined(ASMx64)}
asm
  // RCX <- pA, RDX <- pB, R8 <- pRes, R9 <- nA
  mov r10, rcx
  mov r11, [rdx] // r11 <- B

  lea r10, [r10 + 8 * r9 - 8] // r10 <- @A[n - 1]
  lea r8, [r8 + 8 * r9 - 8] // r8 <- @Res[n - 1]
  xor rdx, rdx // RDX <- r := 0
@L:
  mov rax, [r10] // (RAX:RDX) <- r * b + A[j]
  div r11 // RAX <- (r*b + A[j]) div B; RDX <- (r*b + A[j]) mod B
  mov [r8], rax // store quotient
  sub r10, 8
  sub r8, 8
  dec r9
  jnz @L

  mov rax, rdx
end;
{$else}
var tmp, q, r: UInt64;
    pEnd: PByte;
    v: Cardinal;
    pair: array [0..1] of Cardinal absolute tmp;
begin
  pEnd := pA;
  pA := pA + nA * cLimbSize - cI32Sz;
  pRes := pRes + nA * cLimbSize - cI32Sz;
  tmp := 0;
  v := PCardinal(pB)^;
  while pA >= pEnd do begin
    pair[0] := PCardinal(pA)^;
    DivMod(tmp, v, q, r);
    PCardinal(pRes)^ := Cardinal(q);
    pair[1] := r;
    Dec(pRes, 4);
    Dec(pA, 4);
  end;
  Result := r;
end;
{$endif}

procedure _LongDivMod(pA, pB, pQ: PByte; nA: NativeInt; nB: NativeInt);
{$if defined(ASMx86)}
asm
  push ebx
  push esi
  push edi
  mov esi, pA
  mov edi, pQ // (quotient result)
  push 1 // [ESP + $18] <- carry
  push 0 // [ESP + $14] <- i + j
  push 0 // [ESP + $10] <- i
  push 0 // [ESP + $c] <- r
  push 0 // [ESP + 8] <- q
  mov eax, nA
  sub eax, nB
  push eax // j in [ESP + 4] <- m = nA - nB
  push pB // [ESP] -> v_0
  shl eax, 2
  add edi, eax // [edi] -> @Q[m]

@D3: //q evaluation
  mov ecx, nB
  add ecx, [esp + 4] //ECX <- n + j
  mov edx, [esi + 4 * ecx] //EDX <- u_{n + j}
  mov eax, [esi + 4 * ecx - 4] //EAX <- u_{n+j-1} => (EDX:EAX)<- u_{n+j}b + u_{n+j-1}
  mov ecx, nB
  mov ebx, [esp]
  mov ebx, [ebx + 4 * ecx - 4] // EBX <- v_{n - 1}
  cmp edx, ebx
  jae @1D3
  div ebx
  mov [esp + $8], eax //store q
  mov [esp + $c], edx //store r
  jmp @2D3
@1D3:
  mov ecx, nB
  add ecx, [esp + 4] //ECX <- n + j
  mov edx, [esi + 4 * ecx - 4] //EDX <- u_{j + n - 1}
  mov eax, $ffffffff //(EDX:EAX) <- u_{j + n - 1} * b + (b - 1)
  jmp @4D3
@3D3:
  mov eax, [esp + $8] //EAX <- q
  dec eax //Dec(q)
  mov edx, [esp + $c] //EDX <- r
@4D3:
  mov [esp + $8], eax //store q
  mov ecx, nB
  mov ebx, [esp]
  add edx, [ebx + 4 * ecx - 4] //EDX <- r + v_{n - 1}
  jc @D4
  mov [esp + $c], edx //store r
  mov eax, [esp + $8] //EAX <- q
@2D3: //Test whether q*v_{n - 2} <= r*b + u _{j + n - 2}
  mov ecx, nB
  mov ebx, [esp]
  mul [ebx + 4 * ecx - 8] //(EDX:EAX) <- q * v_{n - 2}
  cmp edx, [esp + $c] //cmp(EDX, r)
  jb @D4
  ja @3D3
  mov ecx, nB
  cmp eax, [esi + 4 * ecx - 8] //cmp(EAX, u_{j + n - 2}
  ja @3D3
@D4: //Multiplication and subtraction
  mov [esp + $18], 0 //set carry
  mov [esp + $10], 0 //i <- 0
  mov ecx, [esp + $4]
  mov [esp + $14], ecx //(i + j) <- j
@2D4:
  mov ecx, [esp + $10] //ECX <- i
  mov ebx, [esp]
  mov eax, [ebx + 4 * ecx] // EAX <- v_i
  mul [esp + $8] //(EDX: EAX) <- q * v_i
  mov ecx, [esp + $14] //ECX <- i + j
  sub [esi + 4 * ecx], eax //u_{i + j} - q * v_i
  adc edx, 0
  mov ebx, [esp + $18] //load last carry
  sub [esi + 4 * ecx], ebx //subtract the last carry flag
  adc edx, 0
  mov [esp + $18], edx //store carry
  inc [esp + $10] //Inc(i)
  inc [esp + $14] //Inc(i + j)
  mov ecx, [esp + $10] //ECX <- i
  cmp ecx, nB
  jbe @2D4
@D5: //Remainder test
  mov eax, [esp + $8] //EAX <- q
  mov [edi], eax //q_j <- q
  cmp edx, 0
  je @D7
@D6:
  dec eax
  mov [edi], eax //q_j <- q - 1
  mov [esp + $10], 0 //i <- 0
  mov ecx, [esp + $4]
  mov [esp + $14], ecx //(i + j) <- j
@1D6: //addition
  mov ecx, [esp + $10]
  cmp ecx, nB
  jae @D7
  mov eax, 0
@2D6:
  mov ecx, [esp + $10]
  mov ebx, [esp]
  add eax, [ebx + 4 * ecx] //EAX += v_{i}
  mov ecx, [esp + $14] //ECX <- (i + j)
  add eax, [esi + 4 * ecx] //EAX += u_{i + j}
  mov [esi + 4 * ecx], eax
  inc [esp + $10] //Inc(i)
  inc [esp + $14] //Inc(i + j)
  jnc @1D6
  mov eax, 1
  mov ecx, [esp + $10]
  cmp ecx, nB
  jna @2D6 //if i <= nB then @2D6
@D7: //j-loop
  mov ecx, [esp + $4]
  dec ecx
  sub edi, 4
  mov [esp + $4], ecx
  cmp ecx, 0
  jge @D3
  add esp, $1c //skip local vars
  pop edi
  pop esi
  pop ebx
end;
{$elseif defined(ASMx64)}
asm
  push rbx
  push rsi
  push rdi
  push r12
  mov rsi, pA
  mov rdi, pQ // (quotient result)
  mov r10, 1 // R10 <- carry
  mov r11, 0 // R11 <- i + j
  mov r12, 0 // R12 <- i
  push 0 // [RSP + $18] <- r
  push 0 // [RSP + $10] <- q
  mov rax, nA
  sub rax, nB
  push rax // j in [RSP + 8] <- m = nA - nB
  push pB // [RSP] -> v_0
  shl rax, 3
  add rdi, rax // [RDI] -> @Q[m]

@D3: //q evaluation
  mov rcx, nB
  add rcx, [rsp + $8] //RCX <- n + j
  mov rdx, [rsi + 8 * rcx] //RDX <- u_{n + j}
  mov rax, [rsi + 8 * rcx - 8] //RAX <- u_{n+j-1} => (RDX:RAX)<- u_{n+j}b + u_{n+j-1}
  mov rcx, nB
  mov rbx, [rsp]
  mov rbx, [rbx + 8 * rcx - 8] // RBX <- v_{n - 1}
  cmp rdx, rbx
  jae @1D3
  div rbx
  mov [rsp + $10], rax //store q
  mov [rsp + $18], rdx //store r
  jmp @2D3
@1D3:
  mov rcx, nB
  add rcx, [rsp + $8] //RCX <- n + j
  mov rdx, [rsi + 8 * rcx - 8] //RDX <- u_{j + n - 1}
  mov rax, $ffffffffffffffff //(RDX:RAX) <- u_{j + n - 1} * b + (b - 1)
  jmp @4D3
@3D3:
  mov rax, [rsp + $10] //RAX <- q
  dec rax //Dec(q)
  mov rdx, [rsp + $18] //RDX <- r
@4D3:
  mov [rsp + $10], rax //store q
  mov rcx, nB
  mov rbx, [rsp]
  add rdx, [rbx + 8 * rcx - 8] //RDX <- r + v_{n - 1}
  jc @D4
  mov [rsp + $18], rdx //store r
  mov rax, [rsp + $10] //RAX <- q
@2D3: //Test whether q*v_{n - 2} <= r*b + u _{j + n - 2}
  mov rcx, nB
  mov rbx, [rsp]
  mul qword ptr [rbx + 8 * rcx - 16] //(RDX:RAX) <- q * v_{n - 2}
  cmp rdx, [rsp + $18] //cmp(RDX, r)
  jb @D4
  ja @3D3
  mov rcx, nB
  cmp rax, [rsi + 8 * rcx - 16] //cmp(RAX, u_{j + n - 2}
  ja @3D3
@D4: //Multiplication and subtraction
  mov r10, 0 //set carry
  mov r12, 0 //i <- 0
  mov rcx, [rsp + $8]
  mov r11, rcx //(i + j) <- j
@2D4:
  mov rcx, r12 //RCX <- i
  mov rbx, [rsp]
  mov rax, [rbx + 8 * rcx] // RAX <- v_i
  mul qword ptr [rsp + $10] //(RDX: RAX) <- q * v_i
  mov rcx, r11 //RCX <- i + j
  sub [rsi + 8 * rcx], rax //u_{i + j} - q * v_i
  adc rdx, 0
  mov rbx, r10 //load last carry
  sub [rsi + 8 * rcx], rbx //subtract the last carry flag
  adc rdx, 0
  mov r10, rdx //store carry
  inc r12 //Inc(i)
  inc r11 //Inc(i + j)
  mov rcx, r12 //RCX <- i
  cmp rcx, nB
  jbe @2D4
@D5: //Remainder test
  mov rax, [rsp + $10] //RAX <- q
  mov [rdi], rax //q_j <- q
  cmp rdx, 0
  je @D7
@D6:
  dec rax
  mov [rdi], rax //q_j <- q - 1
  mov r12, 0 //i <- 0
  mov rcx, [rsp + $8]
  mov r11, rcx //(i + j) <- j
@1D6: //addition
  mov rcx, r12
  cmp rcx, nB
  jae @D7
  mov rax, 0
@2D6:
  mov rcx, r12
  mov rbx, [rsp]
  add rax, [rbx + 8 * rcx] //RAX += v_{i}
  mov rcx, r11 //RCX <- (i + j)
  add rax, [rsi + 8 * rcx] //RAX += u_{i + j}
  mov [rsi + 8 * rcx], rax
  inc r12 //Inc(i)
  inc r11 //Inc(i + j)
  jnc @1D6
  mov rax, 1
  mov rcx, r12
  cmp rcx, nB
  jna @2D6 //if i <= nB then @2D6
@D7: //j-loop
  mov rcx, [rsp + $8]
  dec rcx
  sub rdi, 8
  mov [rsp + $8], rcx
  cmp rcx, 0
  jge @D3
  add rsp, $20 //skip local vars
  pop r12
  pop rdi
  pop rsi
  pop rbx
end;
{$else}
var I, J, m, carry: Integer;
    tmp, tmp2, q, r: UInt64;
    pair: array [0..1] of Cardinal absolute tmp;
    pU: PCardinal;
const wsz = SizeOf(Cardinal); //word size
      cBase: UInt64 = $100000000;
begin
  m := nA - nB;
  Inc(PCardinal(pQ), m);
  for J := m downto 0 do begin
    DivMod(PUInt64(pA + (J + nB - 1) * wsz)^, PCardinal(pB + (nB - 1) * wsz)^, q, r);
    while (q >= cBase) or (q * PCardinal(pB + (nB - 2) * wsz)^ > cBase * r + PCardinal(pA + (J + nB - 2) * wsz)^) do
    begin
      Inc(r, PCardinal(pB + (nB - 1) * wsz)^);
      Dec(q);
      if r >= cBase then break;
    end;
    // (u_{j+n},u_{j+n-1},...,u_j} - q*{0,v_{n-1},...,v_0}
    carry := 0;
    tmp := 0; tmp2 := 0;
    for I := 0 to nB - 1 do begin
      pU := PCardinal(pA + (J + I) * wsz);
      tmp := pU^;
      pair[1] := 1;
      tmp2 := q * PCardinal(pB + I * wsz)^ + PCardinal(PByte(@tmp2) + wsz)^; //q * v_i
      Dec(tmp, PCardinal(@tmp2)^);
      Dec(tmp, carry);
      carry := 1 xor pair[1];
      pU^ := pair[0]; //U_{j+i} - (q * v_i) mod cBase
    end;
    Inc(pU);
    pU^ := pU^ - PCardinal(PByte(@tmp2) + wsz)^ - Cardinal(carry);
    PCardinal(pQ)^ := Cardinal(q);
    if carry < 0 then begin
      Dec(PCardinal(pQ)^, 1);
      carry := 0;
      for I := 0 to nB - 1 do begin
        pU := PCardinal(pA + (J + I) * wsz);
        tmp := Cardinal(pU^) + PCardinal(pB + I * wsz)^;
      end;
    end;
    Dec(PCardinal(pQ));
  end;
end;
{$endif}

function _shl(pSrc, pDst: PByte; count: Byte; Nsrc: NativeInt): TLimb;
{$if defined(ASMx86)}
asm
  //EAX <- pSrc, EDX <- pDst, ECX <- count
  push ebx
  push edi
  push esi
  mov ebx, Nsrc
  dec ebx
  shl ebx, 2
  add eax, ebx
  add edx, ebx
  mov esi, eax //ESI points to the highest value of the source buffer
  mov edi, edx //EDI points to the highest value of the destination buffer
  xor eax, eax
  mov edx, eax

  mov eax, [esi]
  shld edx, eax, cl
  sub esi, 4
  push edx //push the carry from the heighest limb
  cmp ebx, 0
  jz @E
@1:
  mov edx, eax
  mov eax, [esi]
  shld edx, eax, cl
  mov [edi], edx
  sub esi, 4
  sub edi, 4
  sub ebx, 4
  jnz @1
@E:
  shl eax, cl
  mov [edi], eax
  pop eax
  pop esi
  pop edi
  pop ebx
end;
{$elseif defined(ASMx64)}
// RCX <- pSrc, RDX <- pDst, R8 <- count, R9 <- Nsrc
asm
  push r12
  lea r10, [rcx + 8 * r9 - 8] // R10 points to the highest limb of the source buffer
  lea r11, [rdx + 8 * r9 - 8] // R11 points to the highes limb of the destionation buffer
  mov rcx, r8
  xor r8, r8
  mov rax, [r10]
  shld r8, rax, cl // R8 <- carry from the heighest limb
  sub r10, 8
  dec r9
  jz @E
  mov r12, r9
  shr r9, 1
  jz @rest
@L:
  mov rdx, rax
  mov rax, [r10]
  shld rdx, rax, cl
  mov [r11], rdx
  sub r10, 8
  sub r11, 8

  mov rdx, rax
  mov rax, [r10]
  shld rdx, rax, cl
  mov [r11], rdx
  sub r10, 8
  sub r11, 8

  dec r9
  jnz @L

@rest:
  and r12, 1
  jz @E
  mov rdx, rax
  mov rax, [r10]
  shld rdx, rax, cl
  mov [r11], rdx
  sub r11, 8
@E:
  shl rax, cl
  mov [r11], rax
  mov rax, r8
  pop r12
end;
{$else}
var tmp: UInt64;
    pair: array [0..1] of Cardinal absolute tmp;
    pEnd: PByte;
    D: Cardinal;
begin
  pEnd := pSrc;
  pSrc := pSrc + (Nsrc - 1) * cI32Sz;
  pDst := pDst + (Nsrc - 1) * cI32Sz;
  pair[0] := PCardinal(pSrc)^;
  pair[1] := 0;
  tmp := tmp shl count;
  Result := pair[1];
  D := pair[0];
  Dec(pSrc, cI32Sz);
  while pSrc >= pEnd do begin
    pair[0] := PCardinal(pSrc)^;
    pair[1] := 0;
    tmp := tmp shl count;
    pair[1] := pair[1] or D;
    D := pair[0];
    PCardinal(pDst)^ := pair[1];
    Dec(pSrc, cI32Sz);
    Dec(pDSt, cI32Sz);
  end;
  PCardinal(pDst)^ := D;
end;
{$endif}

function _shr(pSrc, pDst: PByte; count: Byte; Nsrc: NativeInt): TLimb;
{$if defined(ASMx86)}
asm
  //EAX <- pSrc, EDX <- pDst, ECX <- count
  push ebx
  push esi
  push edi
  mov ebx, Nsrc
  mov esi, eax
  mov edi, edx
  xor edx, edx
  mov eax, [esi]
  shrd edx, eax, cl
  push edx //store carry from the lowest limb
  add esi, 4
  dec ebx
  jz @E
@1:
  mov edx, eax
  mov eax, [esi]
  shrd edx, eax, cl
  mov [edi], edx
  add esi, 4
  add edi, 4
  dec ebx
  jnz @1
@E:
  shr eax, cl
  mov [edi], eax
  pop eax
  pop edi
  pop esi
  pop ebx
end;
{$elseif defined(ASMx64)}
asm
  push r12
  mov r10, rcx
  mov r11, rdx
  mov rcx, r8
  xor r8, r8
  mov rax, [r10]
  shrd r8, rax, cl // R8 <- carry from the lowest limb
  add r10, 8
  dec r9
  jz @E
  mov r12, r9
  shr r9, 1
  jz @rest
@L:
  mov rdx, rax
  mov rax, [r10]
  shrd rdx, rax, cl
  mov [r11], rdx
  add r10, 8
  add r11, 8

  mov rdx, rax
  mov rax, [r10]
  shrd rdx, rax, cl
  mov [r11], rdx
  add r10, 8
  add r11, 8

  dec r9
  jnz @L

@rest:
  and r12, 1
  jz @E
  mov rdx, rax
  mov rax, [r10]
  shrd rdx, rax, cl
  mov [r11], rdx
  add r11, 8
@E:
  shr rax, cl
  mov [r11], rax
  mov rax, r8 // RAX <- carry
  pop r12
end;
{$else}
var tmp: UInt64;
    pair: array [0..1] of Cardinal absolute tmp;
    pEnd: PByte;
    D: Cardinal;
begin
  pEnd := pSrc + Nsrc * cLimbSize;
  pair[1] := PCardinal(pSrc)^;
  pair[0] := 0;
  tmp := tmp shr count;
  Result := pair[0];
  D := pair[1];
  Inc(pSrc, cI32Sz);
  while pSrc < pEnd do begin
    pair[1] := PCardinal(pSrc)^;
    pair[0] := 0;
    tmp := tmp shr count;
    PCardinal(pDst)^ := pair[0] or D;
    D := pair[1];
    Inc(pSrc, cI32Sz);
    Inc(pDst, cI32Sz);
  end;
  PCardinal(pDst)^ := D;
end;
{$endif}

procedure _ipLongShift(pA: PLimb; aCount: Integer; aDigits: NativeInt);
var offset: NativeInt;
    p: PLimb;
    bRight: Boolean;
const wSz = SizeOf(TLimb);
begin
  if aDigits = 0 then exit;

  bRight := (aDigits < 0);
  aDigits := System.Abs(aDigits);
  offset := aDigits shr W_LOG2_BITS;
  if offset >= aCount then begin
    FillChar(pA^, aCount * cLimbSize, 0);
    exit;
  end;
  Dec(aCount, offset);
  aDigits := aDigits and cLimbRemMask;
  p := pA;

  if bRight then begin
    Inc(p, offset);
    if aDigits > 0 then
      _shr(PByte(p), PByte(pA), aDigits, aCount)
    else
      Move(p^, pA^, aCount * wSz);
    p := pA;
    Inc(p, aCount);
    FillChar(p^, offset * wSz, 0);
  end else begin
    Inc(pA, offset);
    if aDigits > 0 then
      _shl(PByte(p), PByte(pA), aDigits, aCount)
    else
      Move(p^, pA^, aCount * wSz);
    FillChar(p^, offset * wSz, 0);
  end;
end;

function _TopBitPos(A: TLimb): NativeInt;
{$if defined(ASMx86)}
asm
  cmp eax, 0
  jnz @Scan
  xor eax, eax
  ret
@Scan:
  bsr edx, eax
  inc edx
  mov eax, edx
end;
{$elseif defined(ASMx64)}
asm
  cmp rcx, 0
  jnz @Scan
  xor rax, rax
  ret
@Scan:
  bsr rax, rcx
  inc rax
end;
{$else}
begin
  Result := 0;
  if A = 0 then exit;
  while A and cTopBit = 0 do begin
    Inc(Result);
    A := A shl 1;
  end;
  Result := W_BIT_COUNT - Result;
end;
{$endif}

function _TopBitPos(pA: PLimb; N: NativeInt): NativeInt;
{$if defined(ASMx86)}
asm
  lea ecx, [eax + 4 * edx - 4] // ESI points to the highest limb
  shl edx, 5 // EDX contains number of bits of the whole number
@Words:
  mov eax, [ecx]
  cmp eax, 0
  jnz @Bits
  sub ecx, 4
  sub edx, 32
  and edx, edx
  jz @E
  jmp @Words
@Bits:
  bsr ecx, eax
  inc ecx
  sub edx, 32
  add edx, ecx
@E:
  mov eax, edx
end;
{$elseif defined(ASMx64)}
asm
  // RCX <- pA, RDX <- N
  lea r10, [rcx + 8 * rdx - 8] // R10 points to the highest limb
  shl rdx, 6   // RDX contains bit count of the whole number
@Limbs:
  mov rax, [r10]
  cmp rax, 0
  jnz @Bits
  sub r10, 8
  sub rdx, 64
  and rdx, rdx
  jz @E
  jmp @Limbs
@Bits:
  bsr rcx, rax
  inc rcx
  sub rdx, 64
  add rdx, rcx
@E:
  mov rax, rdx
end;
{$else}
var pEnd: PByte;
    pos: Integer;
begin
  pEnd := PByte(pA);
  Inc(pA, N - 1);
  Result := N shl 5;
  while PByte(pA) >= pEnd do begin
    pos := _TopBitPos(PLimb(pA)^);
    Dec(Result, 32);
    if pos > 0 then exit(Result + pos);
    Dec(pA);
  end;
end;
{$endif}

function _GetLimb(pA: PByte; I: NativeInt): TLimb;
begin
  Result := PLimb(pA + I * cLimbSize)^;
end;

function _CountLeadingZeros(A: TLimb): NativeInt;
{$if defined(ASMx86)}
asm
  bsr edx, eax
  jnz @compl
  mov eax, 32
  ret
@compl:
  mov eax, 31
  sub eax, edx
end;
{$elseif defined(ASMx64)}
asm
  bsr rdx, rcx
  jnz @compl
  mov rax, 64
  ret
@compl:
  mov rax, 63
  sub rax, rdx
end;
{$else}
begin
  Result := cLimbBits - _TopBitPos(A);
end;
{$endif}

function _CountLeadingZeros(pA: PLimb; N: NativeInt): NativeInt;
var pEnd: PByte;
    v: TLimb;
begin
  Result := 0;
  pEnd := PByte(pA);
  pA := PLimb(PByte(pA) + (N - 1) * cLimbSize);
  while PByte(pA) >= pEnd do begin
    v := pA^;
    if v <> 0 then begin
      Inc(Result, _CountLeadingZeros(v));
      exit;
    end;
    Inc(Result, cLimbBits);
    Dec(pA);
  end;
end;

function _CountTrailingZeros(A: TLimb): NativeInt;
{$if defined(ASMx86)}
asm
  bsf eax, eax
  jnz @end
  mov eax, 32
@end:
end;
{$elseif defined(ASMx64)}
asm
  bsf rax, rcx
  jnz @end
  mov rax, 64
@end:
end;
{$else}
 begin
   if A = 0 then exit(32);
   Result := 0;
   if (A and $FFFF) = 0 then begin
     Inc(Result, 16);
     A := (A shr 16) or $10000;
   end;

   while (A and 1) = 0 do begin
     A := A shr 1;
     Inc(Result);
   end;
 end;
{$endif}

function _IsZero(pA: PLimb; N: NativeInt): Boolean;
var pEnd: PByte;
begin
  pEnd := PByte(pA) + N * cLimbSize;
  while PByte(pA) < pEnd do begin
    if pA^ <> 0 then exit(False);
    Inc(pA);
  end;
  Result := True;
end;

function _Length(pA: PByte; N: NativeInt): NativeInt;
var p: PByte;
begin
  p := pA + (N - 1) * SizeOf(TLimb);
  while p >= pA do begin
    if p^ <> 0 then break;
    Dec(p, SizeOf(TLimb));
  end;
  Result := (p - pA) div SizeOf(TLimb) + 1;
end;

end.
