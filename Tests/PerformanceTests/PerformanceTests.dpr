program PerformanceTests;
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
  panda.PTests.Math in 'panda.PTests.Math.pas',
  panda.Tests.NDATestCase in '..\panda.Tests.NDATestCase.pas',
  panda.Intfs in '..\..\panda.Intfs.pas',
  pandalib in '..\..\pandalib.pas',
  panda.PTests.vecArithmetic in 'panda.PTests.vecArithmetic.pas',
  panda.Utils.StopWatch in '..\..\Utils\panda.Utils.StopWatch.pas',
  panda.PTests.cvMath in 'panda.PTests.cvMath.pas',
  panda.PTests.cvCvt in 'panda.PTests.cvCvt.pas',
  panda.cvCvt in '..\..\panda.cvCvt.pas',
  panda.PTests.Experiments in 'panda.PTests.Experiments.pas',
  panda.PTests.Conv in 'panda.PTests.Conv.pas',
  panda.BLASInit in '..\..\panda.BLASInit.pas';

{$R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.

