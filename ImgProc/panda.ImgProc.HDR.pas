unit panda.ImgProc.HDR;

interface

uses
    panda.Intfs
  , panda.Arrays
  , panda.ImgProc.Types
  , panda.ImgProc.Images
  ;

{$I AsmDefs.inc}

type
  TWeightingFunction = function (const aValue: Double): Double;

{$region 'Weighting functions'}

function HatWeight(const aValue: Double): Double;

{$endregion}

/// <summary>
///   Estimates 256 log-response values from intensity samples indexed by
///   [sample, image] and one log exposure per image. Weighted data equations
///   and weighted second differences scaled by aSmoothingLambda are fitted
///   by normal equations, with the response at intensity 128 anchored to zero.
/// </summary>
/// <remarks>
///   Samples and exposures must be nonempty, sample rows must match the image
///   count, and aWeightingFunction must be assigned.
///   Invalid inputs or an underdetermined equation count raise EArgumentException.
///   Numerical rank failure raises EInvalidOp. Exposures and callback results
///   must be finite; arithmetic, allocation and callback exceptions propagate.
/// </remarks>
function DebevecResponseCurve(
  const aIntensitySamples   : TArray<TArray<Byte>>;
  const aLogExposures       : TArray<Double>;
  aSmoothingLambda          : Integer;
  aWeightingFunction        : TWeightingFunction
): TArray<Single>;

procedure RadianceMap(
  const aImages: array of IImage<Byte>;
  const aCRF: TArray<Single>;
  const aLogExposures: TArray<Single>;
  var aResult: IImage<Single>;
  const aWeights: TArray<Single> = nil
);

{$region 'low-level functions'}

procedure _RMCombine2(
  const aPtrs         : TArray<PByte>;
  pDst                : PSingle;
  const aG            : TArray<Single>;
  const aLogTs        : TArray<Single>;
  const aWeights      : TArray<Single>;
  aCount              : NativeInt
);

procedure _RMCombine3(
  const aPtrs         : TArray<PByte>;
  pDst                : PSingle;
  const aG            : TArray<Single>;
  const aLogTs        : TArray<Single>;
  const aWeights      : TArray<Single>;
  aCount              : NativeInt
);

procedure _RMCombine(
  const aPtrs         : TArray<PByte>;
  pDst                : PSingle;
  const aG            : TArray<Single>;
  const aLogTs        : TArray<Single>;
  const aWeights      : TArray<Single>;
  aCount              : NativeInt
);

{$endregion}

implementation

uses
    System.SysUtils
  , panda.LinAlg.LeastSquaresF64
  , panda.DynArrayUtils
  ;

{$EXCESSPRECISION OFF} // to prevent Single -> Double conversion by x64 compiler

const
  MONO8_LO    = 0;
  MONO8_HI    = 255;
  MONO8_RNG   = 256;

{$region 'Weighting functions'}

function HatWeight(const aValue: Double): Double;
begin
  if aValue < MONO8_HI/2 then
    Result := aValue + 1
  else
    Result := MONO8_RNG - aValue;
end;

{$endregion}

{$region 'low-level functions'}

procedure _RMCombine2(
  const aPtrs         : TArray<PByte>;
  pDst                : PSingle;
  const aG            : TArray<Single>;
  const aLogTs        : TArray<Single>;
  const aWeights      : TArray<Single>;
  aCount              : NativeInt
);
{$if defined(ASMx64)}
// RCX <- @aPtrs[0], RDX <- pDst, R8 <- @aG[0], R9 <- @aLogTs[0],
// [RBP + $30] <- @aWeights[0], [RPB + $38] <- aCount
asm
  sub rsp, 120
  movdqu [rsp], xmm6
  movdqu [rsp + 16], xmm7
  movdqu [rsp + 32], xmm8
  movdqu [rsp + 48], xmm9
  movdqu [rsp + 64], xmm10
  mov [rsp + 80], rbx
  mov [rsp + 88], rsi
  mov [rsp + 96], rdi
  mov [rsp + 104], r12


  mov r11, [rcx]              // R11 <- aPtrs[0]
  mov r12, [rcx + 8]          // R12 <- aPtrs[1]

  movss xmm6, [r9]
  shufps xmm6, xmm6, 0        // xmm6 <- 4 x aLogTs[0]
  movss xmm7, [r9 + 4]
  shufps xmm7, xmm7, 0        // xmm7 <- 4 x aLogTs[1]

  xor rax, rax
  mov rdi, rdx                // RDX <- pDst
  mov r9, [rbp + $30]         // R9 <- @aWeights[0]
  mov r10, [rbp + $38]        // R10 <- aCount
  shr r10, 2
  jz @rest
@L:
  // 1st image
  mov al, [r11]               // RAX  <- z := aPtrs[0]^
  movss xmm1, [r9 + 4*rax]    // xmm1 <- aWeights[z]
  movss xmm0, [r8 + 4*rax]    // xmm0 <- aG[z]
  mov al, [r11 + 1]           // RAX  <- z := (aPtrs[0] + 1)^
  movss xmm3, [r9 + 4*rax]
  movss xmm2, [r8 + 4*rax]
  unpcklps xmm1, xmm3
  unpcklps xmm0, xmm2
  mov al, [r11 + 2]           // RAX  <- z := (aPtrs[0] + 2)^
  movss xmm3, [r9 + 4*rax]
  movss xmm2, [r8 + 4*rax]
  mov al, [r11 + 3]           // RAX  <- z := (aPtrs[0] + 1)^
  movss xmm5, [r9 + 4*rax]
  movss xmm4, [r8 + 4*rax]
  unpcklps xmm3, xmm5
  unpcklps xmm2, xmm4
  movlhps xmm1, xmm3          // xmm1 <- w0[0:3]
  movlhps xmm0, xmm2          // xmm0 <- g0[0:3]

  movaps xmm9, xmm0           // xmm9 <- g0
  subps xmm9, xmm6            // xmm9 <- g0 - logTs[0]
  mulps xmm9, xmm1            // xmm9 <- (g0 - logTs[0]) * w0
  movaps xmm10, xmm1          // xmm10 <- w0

  // 2nd image
  mov al, [r12]               // RAX  <- z := aPtrs[1]^
  movss xmm1, [r9 + 4*rax]    // xmm1 <- aWeights[z]
  movss xmm0, [r8 + 4*rax]    // xmm0 <- aG[z]
  mov al, [r12 + 1]           // RAX  <- z := (aPtrs[1] + 1)^
  movss xmm3, [r9 + 4*rax]
  movss xmm2, [r8 + 4*rax]
  unpcklps xmm1, xmm3
  unpcklps xmm0, xmm2
  mov al, [r12 + 2]           // RAX  <- z := (aPtrs[1] + 2)^
  movss xmm3, [r9 + 4*rax]
  movss xmm2, [r8 + 4*rax]
  mov al, [r12 + 3]           // RAX  <- z := (aPtrs[1] + 1)^
  movss xmm5, [r9 + 4*rax]
  movss xmm4, [r8 + 4*rax]
  unpcklps xmm3, xmm5
  unpcklps xmm2, xmm4
  movlhps xmm1, xmm3          // xmm1 <- w1[0:3]
  movlhps xmm0, xmm2          // xmm2 <- g1[0:3]

  movaps xmm3, xmm0           // xmm3 <- g1
  subps xmm3, xmm7            // xmm3 <- g1 - logTs[1]
  mulps xmm3, xmm1            // xmm3 <- (g1 - logTs[1]) * w1
  addps xmm9, xmm3            // xmm9 <- (g0 - logTs[0])*w1 + (g1 - logTs[1])*w2
  addps xmm10, xmm1           // xmm10 <- w0 + w1
  divps xmm9, xmm10
  movups [rdi], xmm9

  add rdi, 16
  add r11, 4
  add r12, 4
  dec r10
  jnz @L

@rest:
  mov r10, [rbp + $38]
  and r10, 3
  jz @end
@Lrest:
  mov al, [r11]               // RAX <- z := aPts[0]^
  movss xmm0, [r9 + 4*rax]    // xmm0 <- aWeights[z]
  movss xmm1, [r8 + 4*rax]    // xmm1 <- aG[z]
  subss xmm1, xmm6
  mulss xmm1, xmm0
  movss xmm9, xmm1
  movss xmm10, xmm0
  mov al, [r12]
  movss xmm0, [r9 + 4*rax]
  movss xmm1, [r8 + 4*rax]
  subss xmm1, xmm7
  mulss xmm1, xmm0
  addss xmm9, xmm1
  addss xmm10, xmm0
  divss xmm9, xmm10
  movss [rdi], xmm9

  add rdi, 4
  inc r11
  inc r12
  dec r10
  jnz @Lrest

@end:
  movdqu xmm6, [rsp]
  movdqu xmm7, [rsp + 16]
  movdqu xmm9, [rsp + 48]
  movdqu xmm10, [rsp + 64]
  mov rbx, [rsp + 80]
  mov rsi, [rsp + 88]
  mov rdi, [rsp + 96]
  mov r12, [rsp + 104]
  add rsp, 120
end;
{$else}
var J, imgCount, z: Integer;
    s, sw, wz: Double;
begin
  Assert(Length(aPtrs) = 3);

  while aCount > 0 do begin
    z := aPtrs[0]^;
    Inc(aPtrs[0]);
    wz := aWeights[z];
    s := (aG[z] - aLogTs[0]) * wz;
    sw :=  wz;

    z := aPtrs[1]^;
    Inc(aPtrs[1]);
    wz := aWeights[z];
    s := s + (aG[z] - aLogTs[1]) * wz;
    sw := sw + wz;

    pDst^ := s / sw;
    Inc(pDst);
    Dec(aCount);
  end;
end;
{$endif}

procedure _RMCombine3(
  const aPtrs         : TArray<PByte>;
  pDst                : PSingle;
  const aG            : TArray<Single>;
  const aLogTs        : TArray<Single>;
  const aWeights      : TArray<Single>;
  aCount              : NativeInt
);
{$if defined(ASMx64)}
// RCX <- @aPtrs[0], RDX <- pDst, R8 <- @aG[0], R9 <- @aLogTs[0],
// [RBP + $30] <- @aWeights[0], [RPB + $38] <- aCount
asm
  sub rsp, 120
  movdqu [rsp], xmm6
  movdqu [rsp + 16], xmm7
  movdqu [rsp + 32], xmm8
  movdqu [rsp + 48], xmm9
  movdqu [rsp + 64], xmm10
  mov [rsp + 80], rbx
  mov [rsp + 88], rsi
  mov [rsp + 96], rdi
  mov [rsp + 104], r12
  mov [rsp + 112], r13


  mov r11, [rcx]              // R11 <- aPtrs[0]
  mov r12, [rcx + 8]          // R12 <- aPtrs[1]
  mov r13, [rcx + 16]         // R13 <- aPtrs[2]

  movss xmm6, [r9]
  shufps xmm6, xmm6, 0        // xmm6 <- 4 x aLogTs[0]
  movss xmm7, [r9 + 4]
  shufps xmm7, xmm7, 0        // xmm7 <- 4 x aLogTs[1]
  movss xmm8, [r9 + 8]
  shufps xmm8, xmm8, 0        // xmm8 <- 4 x aLogTs[2]

  xor rax, rax
  mov rdi, rdx                // RDX <- pDst
  mov r9, [rbp + $30]         // R9 <- @aWeights[0]
  mov r10, [rbp + $38]        // R10 <- aCount
  shr r10, 2
  jz @rest
@L:
  // 1st image
  mov al, [r11]               // RAX  <- z := aPtrs[0]^
  movss xmm1, [r9 + 4*rax]    // xmm1 <- aWeights[z]
  movss xmm0, [r8 + 4*rax]    // xmm0 <- aG[z]
  mov al, [r11 + 1]           // RAX  <- z := (aPtrs[0] + 1)^
  movss xmm3, [r9 + 4*rax]
  movss xmm2, [r8 + 4*rax]
  unpcklps xmm1, xmm3
  unpcklps xmm0, xmm2
  mov al, [r11 + 2]           // RAX  <- z := (aPtrs[0] + 2)^
  movss xmm3, [r9 + 4*rax]
  movss xmm2, [r8 + 4*rax]
  mov al, [r11 + 3]           // RAX  <- z := (aPtrs[0] + 1)^
  movss xmm5, [r9 + 4*rax]
  movss xmm4, [r8 + 4*rax]
  unpcklps xmm3, xmm5
  unpcklps xmm2, xmm4
  movlhps xmm1, xmm3          // xmm1 <- w0[0:3]
  movlhps xmm0, xmm2          // xmm0 <- g0[0:3]

  movaps xmm9, xmm0           // xmm9 <- g0
  subps xmm9, xmm6            // xmm9 <- g0 - logTs[0]
  mulps xmm9, xmm1            // xmm9 <- (g0 - logTs[0]) * w0
  movaps xmm10, xmm1          // xmm10 <- w0

  // 2nd image
  mov al, [r12]               // RAX  <- z := aPtrs[1]^
  movss xmm1, [r9 + 4*rax]    // xmm1 <- aWeights[z]
  movss xmm0, [r8 + 4*rax]    // xmm0 <- aG[z]
  mov al, [r12 + 1]           // RAX  <- z := (aPtrs[1] + 1)^
  movss xmm3, [r9 + 4*rax]
  movss xmm2, [r8 + 4*rax]
  unpcklps xmm1, xmm3
  unpcklps xmm0, xmm2
  mov al, [r12 + 2]           // RAX  <- z := (aPtrs[1] + 2)^
  movss xmm3, [r9 + 4*rax]
  movss xmm2, [r8 + 4*rax]
  mov al, [r12 + 3]           // RAX  <- z := (aPtrs[1] + 1)^
  movss xmm5, [r9 + 4*rax]
  movss xmm4, [r8 + 4*rax]
  unpcklps xmm3, xmm5
  unpcklps xmm2, xmm4
  movlhps xmm1, xmm3          // xmm1 <- w1[0:3]
  movlhps xmm0, xmm2          // xmm2 <- g1[0:3]

  movaps xmm3, xmm0           // xmm3 <- g1
  subps xmm3, xmm7            // xmm3 <- g1 - logTs[1]
  mulps xmm3, xmm1            // xmm3 <- (g1 - logTs[1]) * w1
  addps xmm9, xmm3            // xmm9 <- (g0 - logTs[0])*w1 + (g1 - logTs[1])*w2
  addps xmm10, xmm1           // xmm10 <- w0 + w1

  // 3nd image
  mov al, [r13]               // RAX  <- z := aPtrs[2]^
  movss xmm1, [r9 + 4*rax]    // xmm1 <- aWeights[z]
  movss xmm0, [r8 + 4*rax]    // xmm0 <- aG[z]
  mov al, [r13 + 1]           // RAX  <- z := (aPtrs[2] + 1)^
  movss xmm3, [r9 + 4*rax]
  movss xmm2, [r8 + 4*rax]
  unpcklps xmm1, xmm3
  unpcklps xmm0, xmm2
  mov al, [r13 + 2]           // RAX  <- z := (aPtrs[2] + 2)^
  movss xmm3, [r9 + 4*rax]
  movss xmm2, [r8 + 4*rax]
  mov al, [r13 + 3]           // RAX  <- z := (aPtrs[2] + 1)^
  movss xmm5, [r9 + 4*rax]
  movss xmm4, [r8 + 4*rax]
  unpcklps xmm3, xmm5
  unpcklps xmm2, xmm4
  movlhps xmm1, xmm3          // xmm1 <- w2[0:3]
  movlhps xmm0, xmm2          // xmm2 <- g2[0:3]

  movaps xmm3, xmm0           // xmm3 <- g2
  subps xmm3, xmm8            // xmm3 <- g2 - logTs[2]
  mulps xmm3, xmm1            // xmm3 <- (g2 - logTs[2]) * w2
  addps xmm9, xmm3
  addps xmm10, xmm1           // xmm10 <- w0 + w1 + w2
  divps xmm9, xmm10
  movups [rdi], xmm9

  add rdi, 16
  add r11, 4
  add r12, 4
  add r13, 4
  dec r10
  jnz @L

@rest:
  mov r10, [rbp + $38]
  and r10, 3
  jz @end
@Lrest:
  mov al, [r11]               // RAX <- z := aPts[0]^
  movss xmm0, [r9 + 4*rax]    // xmm0 <- aWeights[z]
  movss xmm1, [r8 + 4*rax]    // xmm1 <- aG[z]
  subss xmm1, xmm6
  mulss xmm1, xmm0
  movss xmm9, xmm1
  movss xmm10, xmm0
  mov al, [r12]
  movss xmm0, [r9 + 4*rax]
  movss xmm1, [r8 + 4*rax]
  subss xmm1, xmm7
  mulss xmm1, xmm0
  addss xmm9, xmm1
  addss xmm10, xmm0
  mov al, [r13]
  movss xmm0, [r9 + 4*rax]
  movss xmm1, [r8 + 4*rax]
  subss xmm1, xmm8
  mulss xmm1, xmm0
  addss xmm9, xmm1
  addss xmm10, xmm0
  divss xmm9, xmm10
  movss [rdi], xmm9

  add rdi, 4
  inc r11
  inc r12
  inc r13
  dec r10
  jnz @Lrest

@end:
  movdqu xmm6, [rsp]
  movdqu xmm7, [rsp + 16]
  movdqu xmm8, [rsp + 32]
  movdqu xmm9, [rsp + 48]
  movdqu xmm10, [rsp + 64]
  mov rbx, [rsp + 80]
  mov rsi, [rsp + 88]
  mov rdi, [rsp + 96]
  mov r12, [rsp + 104]
  mov r13, [rsp + 112]
  add rsp, 120
end;
{$else}
var J, imgCount, z: Integer;
    s, sw, wz: Double;
begin
  Assert(Length(aPtrs) = 3);

  while aCount > 0 do begin
    z := aPtrs[0]^;
    Inc(aPtrs[0]);
    wz := aWeights[z];
    s := (aG[z] - aLogTs[0]) * wz;
    sw :=  wz;

    z := aPtrs[1]^;
    Inc(aPtrs[1]);
    wz := aWeights[z];
    s := s + (aG[z] - aLogTs[1]) * wz;
    sw := sw + wz;

    z := aPtrs[2]^;
    Inc(aPtrs[2]);
    wz := aWeights[z];
    s := s + (aG[z] - aLogTs[2]) * wz;
    sw := sw + wz;

    pDst^ := s / sw;
    Inc(pDst);
    Dec(aCount);
  end;
end;
{$endif}

procedure _RMCombine(
  const aPtrs         : TArray<PByte>;
  pDst                : PSingle;
  const aG            : TArray<Single>;
  const aLogTs        : TArray<Single>;
  const aWeights      : TArray<Single>;
  aCount              : NativeInt
);
{$if defined(ASMx64)}
// RCX <- @aPtrs[0], RDX <- pDst, R8 <- @aG[0], R9 <- @aLogTs[0],
// [RBP + $30] <- @aWeights[0], [RPB + $38] <- aCount
asm
  sub rsp, 120
  movdqu [rsp], xmm6
  movdqu [rsp + 16], xmm7
  movdqu [rsp + 32], xmm8
  movdqu [rsp + 48], xmm9
  movdqu [rsp + 64], xmm10
  mov [rsp + 80], rbx
  mov [rsp + 88], rsi
  mov [rsp + 96], rdi
  mov [rsp + 104], r12
  mov [rsp + 112], r13


  mov r13, r9                 // R13 <- @aLogTs
  mov rbx, [r13 - 8]          // RBX <- Length(aLogTs)
  mov rdi, rdx                // RDX <- pDst
  xor rax, rax
  xor rdx, rdx
  mov r9, [rbp + $30]         // R9 <- @aWeights[0]
  mov r10, [rbp + $38]        // R10 <- aCount
  shr r10, 2
  jz @rest
@L:
  xorps xmm9, xmm9
  xorps xmm10, xmm10
  xor r11, r11
@LInner:
  mov r12, [rcx + 8*r11]
  add r12, rdx
  mov al, [r12]               // RAX  <- z := aPtrs[i]^
  movss xmm1, [r9 + 4*rax]    // xmm1 <- aWeights[z]
  movss xmm0, [r8 + 4*rax]    // xmm0 <- aG[z]
  mov al, [r12 + 1]           // RAX  <- z := (aPtrs[i] + 1)^
  movss xmm3, [r9 + 4*rax]
  movss xmm2, [r8 + 4*rax]
  unpcklps xmm1, xmm3
  unpcklps xmm0, xmm2
  mov al, [r12 + 2]           // RAX  <- z := (aPtrs[i] + 2)^
  movss xmm3, [r9 + 4*rax]
  movss xmm2, [r8 + 4*rax]
  mov al, [r12 + 3]           // RAX  <- z := (aPtrs[i] + 1)^
  movss xmm5, [r9 + 4*rax]
  movss xmm4, [r8 + 4*rax]
  unpcklps xmm3, xmm5
  unpcklps xmm2, xmm4
  movlhps xmm1, xmm3          // xmm1 <- wi[0:3]
  movlhps xmm0, xmm2          // xmm0 <- gi[0:3]

  movss xmm6, [r13 + 4*r11]   // xmm6 <- logTs[i]
  shufps xmm6, xmm6, 0        // xmm6 <- 4 x logTs[i]
  subps xmm0, xmm6            // xmm0 <- gi - logTs[i]
  mulps xmm0, xmm1            // xmm0 <- (gi - logTs[i]) * w0
  addps xmm9, xmm0            // xmm9 <- sum(gj - logTs[j]*wj) + (gi- logTs[i])*wi
  addps xmm10, xmm1           // xmm10 <- sum(wj) + wi

  inc r11
  cmp r11, rbx
  jl @LInner

  divps xmm9, xmm10
  movups [rdi], xmm9

  add rdi, 16
  add rdx, 4
  dec r10
  jnz @L

@rest:
  mov r10, [rbp + $38]
  and r10, 3
  jz @end
@Lrest:
  xorps xmm9, xmm9
  xorps xmm10, xmm10
  xor r11, r11
@LrestInner:
  mov r12, [rcx + 8*r11]
  mov al, [r12 + rdx]         // RAX  <- z := aPtrs[i]^
  movss xmm0, [r9 + 4*rax]    // xmm0 <- aWeights[z]
  movss xmm1, [r8 + 4*rax]    // xmm1 <- aG[z]
  movss xmm6, [r13 + 4*r11]   // xmm6 <- logTs[i]
  subss xmm1, xmm6
  mulss xmm1, xmm0
  addss xmm9, xmm1
  addss xmm10, xmm0
  inc r11
  cmp r11, rbx
  jl @LrestInner

  divss xmm9, xmm10
  movss [rdi], xmm9

  add rdi, 4
  inc rdx
  dec r10
  jnz @Lrest

@end:
  movdqu xmm6, [rsp]
  movdqu xmm7, [rsp + 16]
  movdqu xmm8, [rsp + 32]
  movdqu xmm9, [rsp + 48]
  movdqu xmm10, [rsp + 64]
  mov rbx, [rsp + 80]
  mov rsi, [rsp + 88]
  mov rdi, [rsp + 96]
  mov r12, [rsp + 104]
  mov r13, [rsp + 112]
  add rsp, 120
end;
{$else}
var J, imgCount, z: Integer;
    s, sw, wz: Double;
begin
  imgCount := Length(aPtrs);

  while aCount > 0 do begin
    s := 0;
    sw := 0;
    for J := 0 to imgCount - 1 do begin
      z := aPtrs[J]^;
      Inc(aPtrs[J]);
      wz := aWeights[z];
      s := s + (aG[z] - aLogTs[J]) * wz;
      sw := sw + wz;
    end;
    pDst^ := s / sw;
    Inc(pDst);
    Dec(aCount);
  end;
end;
{$endif}

{$endregion}

function DebevecResponseCurve(
  const aIntensitySamples   : TArray<TArray<Byte>>;
  const aLogExposures       : TArray<Double>;
  aSmoothingLambda          : Integer;
  aWeightingFunction        : TWeightingFunction
): TArray<Single>;
var A: TNDAMatItems<Double>;
    b: TNDAVecItems<Double>;
    X: INDArray<Double>;
    I, J, K, z, numSamples, numImages: Integer;
    w: Double;
const
  zmin = MONO8_LO;
  zmax = MONO8_HI;
  intensityRange = MONO8_HI;
begin
  numSamples := Length(aIntensitySamples);
  numImages := Length(aLogExposures);
  Assert(
    (numSamples > 1) and (numImages > 1) and Assigned(aWeightingFunction) and
    (Length(aIntensitySamples[0]) = numImages) and TDynAUt.MatrixQ<Byte>(aIntensitySamples) and
    (aSmoothingLambda > 0)
  );

  A := TNDAUt.Full<Double>([numImages * numSamples + intensityRange, numSamples + intensityRange + 1], 0);
  b := TNDAUt.Full<Double>([A.RowCount], 0);

  k := 0;
  for I := 0 to numSamples - 1 do begin
    for J := 0 to numImages - 1 do begin
      z := aIntensitySamples[I, J];
      w := aWeightingFunction(z);
      A[k , z] := w;
      A[k, intensityRange + 1 + i] := -w;
      b[k] := w * aLogExposures[j];
      Inc(k);
    end;
  end;

  for z := zmin + 1 to zmax - 1 do begin
    w := aWeightingFunction(z);
    A[k, z - 1] := w * aSmoothingLambda;
    A[k, z] := -2 * w * aSmoothingLambda;
    A[k, z + 1] := w * aSmoothingLambda;
    Inc(k);
  end;

  A[k, (zmax - zmin) div 2 + 1] := 1;

  case LeastSquares(INDArray<Double>(A), INDArray<Double>(b), X) of
    lssInvalidInput:
      raise EArgumentException.Create('DebevecResponseCurve: invalid least-squares dimensions');
    lssRankDeficient:
      raise EInvalidOp.Create('DebevecResponseCurve: numerically rank-deficient equations');
  end;

  TNDAUt.TryAsDynArray<Single>(TNDAUt.AsType<Single>(X[[NDISpan(0, intensityRange)]]), Result);
end;

procedure RadianceMap(
  const aImages: array of IImage<Byte>;
  const aCRF: TArray<Single>;
  const aLogExposures: TArray<Single>;
  var aResult: IImage<Single>;
  const aWeights: TArray<Single>
);
var I, J, w, h, imgCount: Integer;
    ws: TArray<NativeInt>;
    wf: TArray<Single>;
    ptrs, prows: TArray<PByte>;
    pDst: PSingle;
    img: IImage<Byte>;
begin
  imgCount := Length(aImages);
  Assert((imgCount > 0) and (imgCount = Length(aLogExposures)));
  Assert(Length(aCRF) = MONO8_RNG);

  SetLength(ws, imgCount);
  SetLength(ptrs, imgCount);
  SetLength(prows, imgCount);
  w := aImages[0].Width;
  h := aImages[0].Height;
  for I := 0 to imgCount - 1 do begin
    img := aImages[I];
    Assert((img.Width = w) and (img.Height = h));
    ws[I] := img.WidthStep;
    ptrs[I] := img.Data;
  end;

  if not Assigned(aWeights) then begin
    SetLength(wf, MONO8_RNG);
    for I := 0 to High(wf) do
      wf[I] := HatWeight(I);
  end;

  if not Assigned(aResult) then
    aResult := TNDAImg<Single>.Create(w, h);
  Assert((aResult.Width = w) and (aResult.Height = h));
  pDst := PSingle(aResult.Data);

  case imgCount of
    2: begin
      for I := 0 to h - 1 do begin
        for J := 0 to imgCount - 1 do
          prows[J] := ptrs[J] + I * ws[J];
        _RMCombine2(prows, pDst, aCRF, aLogExposures, wf, w);
        Inc(pDst, w);
      end;
    end;

    3: begin
      for I := 0 to h - 1 do begin
        for J := 0 to imgCount - 1 do
          prows[J] := ptrs[J] + I * ws[J];
        _RMCombine3(prows, pDst, aCRF, aLogExposures, wf, w);
        Inc(pDst, w);
      end;
    end;

  else
    for I := 0 to h - 1 do begin
      for J := 0 to imgCount - 1 do
        prows[J] := ptrs[J] + I * ws[J];
      _RMCombine(prows, pDst, aCRF, aLogExposures, wf, w);
      Inc(pDst, w);
    end;
  end;
end;

end.
