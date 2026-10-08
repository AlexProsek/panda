program NNPerformanceTests;
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
  panda.NN.PoolingLayer in '..\panda.NN.PoolingLayer.pas',
  panda.NN.PTests.PoolingLayer in 'panda.NN.PTests.PoolingLayer.pas',
  panda.Tests.NDATestCase in '..\..\Tests\panda.Tests.NDATestCase.pas',
  panda.NN in '..\panda.NN.pas',
  panda.ImgProc.ImgResize in '..\..\ImgProc\panda.ImgProc.ImgResize.pas',
  panda.NN.PTests.NetTests in 'panda.NN.PTests.NetTests.pas',
  panda.ImgProc.Images in '..\..\ImgProc\panda.ImgProc.Images.pas',
  panda.ImgProc.io in '..\..\ImgProc\panda.ImgProc.io.pas',
  panda.ImgProc.Types in '..\..\ImgProc\panda.ImgProc.Types.pas',
  panda.ImgProc.RLE in '..\..\ImgProc\panda.ImgProc.RLE.pas',
  panda.ImgProc.VCLImages in '..\..\ImgProc\panda.ImgProc.VCLImages.pas',
  panda.NN.Importer in '..\panda.NN.Importer.pas',
  panda.NN.ImageEncoder in '..\panda.NN.ImageEncoder.pas',
  panda.BLASInit in '..\..\panda.BLASInit.pas';

{$R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.

