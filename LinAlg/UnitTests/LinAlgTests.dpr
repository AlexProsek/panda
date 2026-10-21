program LinAlgTests;
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
  panda.LinAlg.LeastSquaresF64 in '..\panda.LinAlg.LeastSquaresF64.pas',
  panda.LinAlg.Tests.LeastSquaresF64 in 'panda.LinAlg.Tests.LeastSquaresF64.pas',
  panda.LinAlg.Tests.Solver in 'panda.LinAlg.Tests.Solver.pas',
  panda.LinAlg.Cholesky in '..\panda.LinAlg.Cholesky.pas',
  panda.LinAlg.CholeskyF64 in '..\panda.LinAlg.CholeskyF64.pas',
  panda.LinAlg.Tests.CholeskyF64 in 'panda.LinAlg.Tests.CholeskyF64.pas',
  panda.LinAlg.Base in '..\panda.LinAlg.Base.pas',
  panda.LinAlg.NumericF64 in '..\panda.LinAlg.NumericF64.pas',
  panda.LinAlg.NumericF128 in '..\panda.LinAlg.NumericF128.pas',
  panda.LinAlg.NumericC128 in '..\panda.LinAlg.NumericC128.pas',
  panda.LinAlg.NumericRational in '..\panda.LinAlg.NumericRational.pas',
  panda.LinAlg.LUP in '..\panda.LinAlg.LUP.pas',
  panda.LinAlg.LUPF64 in '..\panda.LinAlg.LUPF64.pas',
  panda.LinAlg.LUPF128 in '..\panda.LinAlg.LUPF128.pas',
  panda.LinAlg.LUPC128 in '..\panda.LinAlg.LUPC128.pas',
  panda.LinAlg.LUPRational in '..\panda.LinAlg.LUPRational.pas',
  panda.LinAlg.Tests.LUP in 'panda.LinAlg.Tests.LUP.pas',
  panda.LinAlg.Tests.LUPF64 in 'panda.LinAlg.Tests.LUPF64.pas',
  panda.LinAlg.Tests.LUPF128 in 'panda.LinAlg.Tests.LUPF128.pas',
  panda.LinAlg.Tests.LUPC128 in 'panda.LinAlg.Tests.LUPC128.pas',
  panda.LinAlg.Tests.LUPRational in 'panda.LinAlg.Tests.LUPRational.pas',
  panda.LinAlg.Gauss in '..\panda.LinAlg.Gauss.pas',
  panda.LinAlg.GaussF64 in '..\panda.LinAlg.GaussF64.pas',
  panda.LinAlg.RefineF64 in '..\panda.LinAlg.RefineF64.pas',
  panda.LinAlg.GaussF128 in '..\panda.LinAlg.GaussF128.pas',
  panda.LinAlg.GaussC128 in '..\panda.LinAlg.GaussC128.pas',
  panda.LinAlg.GaussRational in '..\panda.LinAlg.GaussRational.pas',
  panda.LinAlg.Tests.GaussRational in 'panda.LinAlg.Tests.GaussRational.pas',
  panda.LinAlg.Tests.GaussC128 in 'panda.LinAlg.Tests.GaussC128.pas',
  panda.LinAlg.Tests.GaussF64 in 'panda.LinAlg.Tests.GaussF64.pas',
  panda.LinAlg.Tests.RefineF64 in 'panda.LinAlg.Tests.RefineF64.pas',
  panda.LinAlg.Tests.GaussF128 in 'panda.LinAlg.Tests.GaussF128.pas',
  panda.Tests.NDATestCase in '..\..\Tests\panda.Tests.NDATestCase.pas';

{$R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.

