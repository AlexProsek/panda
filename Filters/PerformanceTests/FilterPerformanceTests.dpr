program FilterPerformanceTests;
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
  panda.Filters.PTests.TVFilter in 'panda.Filters.PTests.TVFilter.pas',
  panda.Filters.PTests.OrdStatFilters in 'panda.Filters.PTests.OrdStatFilters.pas';

{$R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.

