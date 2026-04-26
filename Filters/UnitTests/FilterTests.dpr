program FilterTests;
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
  panda.Filters.TVFilter in '..\panda.Filters.TVFilter.pas',
  panda.Filters.Tests.TVFilter in 'panda.Filters.Tests.TVFilter.pas',
  panda.ImgProc.io in '..\..\ImgProc\panda.ImgProc.io.pas',
  panda.ImgProc.Types in '..\..\ImgProc\panda.ImgProc.Types.pas',
  panda.Filters.OrderStatFilters in '..\panda.Filters.OrderStatFilters.pas',
  panda.Filters.Tests.OrdStatFilters in 'panda.Filters.Tests.OrdStatFilters.pas',
  panda.Tests.NDATestCase in '..\..\Tests\panda.Tests.NDATestCase.pas';

{$R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.

