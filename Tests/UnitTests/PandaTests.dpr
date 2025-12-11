program PandaTests;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  DUnitTestRunner,
  panda.Tests.Arrays in 'panda.Tests.Arrays.pas',
  panda.Tests.Arithmetic in 'panda.Tests.Arithmetic.pas',
  panda.Tests.Math in 'panda.Tests.Math.pas',
  panda.Tests.ArrManip in 'panda.Tests.ArrManip.pas',
  panda.Tests.Formatter in 'panda.Tests.Formatter.pas',
  panda.Arithmetic in '..\..\panda.Arithmetic.pas',
  panda.Arrays in '..\..\panda.Arrays.pas',
  panda.ArrManip in '..\..\panda.ArrManip.pas',
  panda.cvArithmetic in '..\..\panda.cvArithmetic.pas',
  panda.cvMath in '..\..\panda.cvMath.pas',
  panda.DynArrayUtils in '..\..\panda.DynArrayUtils.pas',
  panda.Formatter in '..\..\panda.Formatter.pas',
  panda.Intfs in '..\..\panda.Intfs.pas',
  panda.Math in '..\..\panda.Math.pas',
  pandalib in '..\..\pandalib.pas',
  panda.Tests.vecArithmetic in 'panda.Tests.vecArithmetic.pas',
  panda.consts in '..\..\panda.consts.pas',
  panda.Tests.NDATestCase in '..\panda.Tests.NDATestCase.pas',
  panda.Utils.StopWatch in '..\..\Utils\panda.Utils.StopWatch.pas',
  panda.Broadcasting in '..\..\panda.Broadcasting.pas',
  panda.Tests.Broadcasting in 'panda.Tests.Broadcasting.pas',
  panda.Tests.cvMath in 'panda.Tests.cvMath.pas',
  panda.cvCvt in '..\..\panda.cvCvt.pas',
  panda.vCvt in '..\..\panda.vCvt.pas',
  panda.Tests.cvCvt in 'panda.Tests.cvCvt.pas';

{$R *.RES}

begin
{$ifdef MSWINDOWS}
{$WARN SYMBOL_PLATFORM OFF}
  if DebugHook <> 0 then ReportMemoryLeaksOnShutDown := True;
{$endif}

  DUnitTestRunner.RunRegisteredTests;
end.

