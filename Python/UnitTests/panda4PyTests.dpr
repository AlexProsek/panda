program panda4PyTests;
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
  pynda.Arrays in '..\pynda.Arrays.pas',
  pynda.Tests in 'pynda.Tests.pas',
  panda.Tests.NDATestCase in '..\..\Tests\panda.Tests.NDATestCase.pas',
  panda.Utils.StopWatch in '..\..\Utils\panda.Utils.StopWatch.pas';

{$R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.

