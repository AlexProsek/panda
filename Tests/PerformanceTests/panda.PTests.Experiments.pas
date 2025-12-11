unit panda.PTests.Experiments;

interface

uses
    TestFramework
  , panda.Tests.NDATestCase
  , panda.cvCvt
  ;

{$I AsmDefs.inc}

{$ifdef ASMx64}
type
  TExperimentTests = class(TNDAPerformanceTestCase)
  protected const
    N = 1000000;
  protected
    procedure SSELoop;
    procedure SSEMul;
    procedure SSEAdd;
    procedure SSEDiv;
  published
    procedure SSEArithComp_F64;
  end;
{$endif}

implementation

{$ifdef ASMx64}

procedure TExperimentTests.SSELoop;
asm
  mov rcx, TExperimentTests.N
@L:
  dec rcx
  jnz @L
end;

procedure TExperimentTests.SSEMul;
const cOne: Double = 1.0;
asm
  mov rcx, TExperimentTests.N
  xorpd xmm0, xmm0
  addsd xmm0, cOne
@L:
  mulpd xmm0, xmm0
  dec rcx
  jnz @L
end;

procedure TExperimentTests.SSEAdd;
const c: Double = 1e-10;
asm
  mov rcx, TExperimentTests.N
  xorpd xmm0, xmm0
  addsd xmm0, c
@L:
  addpd xmm0, xmm0
  dec rcx
  jnz @L
end;

procedure TExperimentTests.SSEDiv;
const cDenom: Double = 1.0;
asm
  mov rcx, TExperimentTests.N
  xorpd xmm0, xmm0
  movddup xmm1, cDenom
@L:
  divpd xmm0, xmm1
  dec rcx
  jnz @L
end;

procedure TExperimentTests.SSEArithComp_F64;
var tL: Double;
const cLCnt = 10;
begin
  SW.Start;
  DoTestLoop(SSELoop, cLCnt);
  SW.Stop;
  tL := -SW.ElapsedMilisecondsEx / cLCnt;

  SWStart;
  DoTestLoop(SSEAdd, cLCnt);
  SWStop('Add', tL);

  SWStart;
  DoTestLoop(SSEMul, cLCnt);
  SWStop('Mul', tL);

  SWStart;
  DoTestLoop(SSEDiv, cLCnt);
  SWStop('Div', tL);
end;

{$endif}

initialization

{$if defined(ASMx64)}
  RegisterTest(TExperimentTests.Suite);
{$endif}

end.
