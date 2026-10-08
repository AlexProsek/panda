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
  panda.PTests.cvArithmetic in 'panda.PTests.cvArithmetic.pas',
  panda.Utils.StopWatch in '..\..\Utils\panda.Utils.StopWatch.pas',
  panda.PTests.cvMath in 'panda.PTests.cvMath.pas',
  panda.PTests.cvCvt in 'panda.PTests.cvCvt.pas',
  panda.cvCvt in '..\..\panda.cvCvt.pas',
  panda.PTests.Conv in 'panda.PTests.Conv.pas',
  panda.BLASInit in '..\..\panda.BLASInit.pas',
  panda.PTests.Nums in 'panda.PTests.Nums.pas',
  panda.PTests.Poly in 'panda.PTests.Poly.pas',
  panda.PTests.ArrManip in 'panda.PTests.ArrManip.pas',
  panda.PTests.DynArrUtils in 'panda.PTests.DynArrUtils.pas',
  panda.PTests.Sorting in 'panda.PTests.Sorting.pas',
  panda.PTests.cvCmp in 'panda.PTests.cvCmp.pas',
  panda.PTests.MPNums in 'panda.PTests.MPNums.pas',
  panda.Arithmetic in '..\..\panda.Arithmetic.pas',
  panda.Arrays in '..\..\panda.Arrays.pas',
  panda.ArrManip in '..\..\panda.ArrManip.pas',
  panda.Conv in '..\..\panda.Conv.pas',
  panda.cvArithmetic in '..\..\panda.cvArithmetic.pas',
  panda.fft in '..\..\panda.fft.pas',
  panda.Math in '..\..\panda.Math.pas',
  panda.Nums in '..\..\panda.Nums.pas',
  panda.NumsLowLvl in '..\..\panda.NumsLowLvl.pas',
  panda.NumsQP in '..\..\panda.NumsQP.pas',
  panda.PTests.FFT in 'panda.PTests.FFT.pas';

{$R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.

