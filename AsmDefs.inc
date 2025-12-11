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
