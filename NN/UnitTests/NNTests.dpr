program NNTests;
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
  panda.NN.Tests in 'panda.NN.Tests.pas',
  panda.Tests.NDATestCase in '..\..\Tests\panda.Tests.NDATestCase.pas',
  panda.NN in '..\panda.NN.pas',
  panda.NN.Importer in '..\panda.NN.Importer.pas',
  panda.NN.PoolingLayer in '..\panda.NN.PoolingLayer.pas',
  panda.ImgProc.Types in '..\..\ImgProc\panda.ImgProc.Types.pas',
  panda.ImgProc.Images in '..\..\ImgProc\panda.ImgProc.Images.pas',
  panda.ImgProc.ImgResize in '..\..\ImgProc\panda.ImgProc.ImgResize.pas',
  panda.MAT4io in '..\..\panda.MAT4io.pas',
  panda.NN.Tests.PoolingLayer in 'panda.NN.Tests.PoolingLayer.pas',
  panda.NN.ImageEncoder in '..\panda.NN.ImageEncoder.pas';

{$R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.

