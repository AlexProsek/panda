program ImgProcTests;
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
  panda.ImgProc.ImgResize in '..\panda.ImgProc.ImgResize.pas',
  panda.ImgProc.Tests.ImgResize in 'panda.ImgProc.Tests.ImgResize.pas',
  panda.Tests.NDATestCase in '..\..\Tests\panda.Tests.NDATestCase.pas',
  panda.ImgProc.Types in '..\panda.ImgProc.Types.pas',
  panda.ImgProc.CSCvt in '..\panda.ImgProc.CSCvt.pas',
  panda.ImgProc.Images in '..\panda.ImgProc.Images.pas',
  panda.ImgProc.io in '..\panda.ImgProc.io.pas',
  panda.ImgProc.VCLImages in '..\panda.ImgProc.VCLImages.pas',
  panda.ImgProc.RLE in '..\panda.ImgProc.RLE.pas',
  panda.ImgProc.Tests.CSCvt in 'panda.ImgProc.Tests.CSCvt.pas',
  panda.ImgProc.Tests.RLE in 'panda.ImgProc.Tests.RLE.pas';

{$R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.

