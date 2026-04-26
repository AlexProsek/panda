program ImgProcPerformanceTests;
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
  panda.ImgProc.PTests.ImgResize in 'panda.ImgProc.PTests.ImgResize.pas',
  panda.Tests.NDATestCase in '..\..\Tests\panda.Tests.NDATestCase.pas',
  panda.ImgProc.ImgResize in '..\panda.ImgProc.ImgResize.pas',
  panda.ImgProc.PTests.CSCvt in 'panda.ImgProc.PTests.CSCvt.pas',
  panda.ImgProc.PTests.RLE in 'panda.ImgProc.PTests.RLE.pas',
  panda.ImgProc.CSCvt in '..\panda.ImgProc.CSCvt.pas',
  panda.ImgProc.RLE in '..\panda.ImgProc.RLE.pas';

{$R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.

