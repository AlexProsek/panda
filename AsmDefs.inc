{$ifdef PurePascal}
  {$define NoASM}
{$endif}

{$ifdef FPC}
  {$ASMMODE intel}
{$else}
  {$if CompilerVersion < 33.0}
    {$undef AVX}
    {$undef AVX2}
  {$ifend}
  {$if CompilerVersion < 34.0}
    {$undef AVX2}
  {$ifend}
{$endif}

{$if defined(AVX2)}
  {$undef AVX}
{$endif}


{$if (not defined(NoASM)) }
  {$if defined(CPUX86)}
    {$define ASMx86}
    {$undef ASMx64}
    {$define ASM}
  {$elseif defined(CPUX64)}
    {$define ASMx64}
    {$undef ASMx86}
    {$define ASM}
  {$endif}
{$endif}

{$undef RANGEON}
{$ifopt R+}
  {$define RANGEON}
{$endif}

{$undef OVERFLOWON}
{$ifopt Q+}
  {$define OVERFLOWON}
{$endif}

const
// CPU_X86_V2 expects SSE, SSE2, SSE3, SSSE3, SSE4_1, SSE4_2  (minimal cpu baseline for asm code)
// CPU_X86_V3 expects CPU_X84_V2, AVX, AVX2
{$ifdef NoASM}
  CPU_X86_V2 = False;
  CPU_X86_V3 = False;
{$else}
  CPU_X86_V2 = True;
{$ifdef AVX}
  CPU_X86_V3 = True;
{$else}
  CPU_X86_V3 = False;
{$endif}
{$endif}
