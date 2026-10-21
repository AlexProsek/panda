program LinAlgPerformanceTests;
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
  System.Classes,
  System.SysUtils,
  TestFramework,
  panda.LinAlg.Base in '..\panda.LinAlg.Base.pas',
  panda.LinAlg.NumericF64 in '..\panda.LinAlg.NumericF64.pas',
  panda.LinAlg.LUP in '..\panda.LinAlg.LUP.pas',
  panda.LinAlg.LUPF64 in '..\panda.LinAlg.LUPF64.pas',
  panda.LinAlg.Cholesky in '..\panda.LinAlg.Cholesky.pas',
  panda.LinAlg.CholeskyF64 in '..\panda.LinAlg.CholeskyF64.pas',
  panda.LinAlg.PTests.CholeskyF64 in 'panda.LinAlg.PTests.CholeskyF64.pas',
  panda.LinAlg.Gauss in '..\panda.LinAlg.Gauss.pas',
  panda.LinAlg.GaussF64 in '..\panda.LinAlg.GaussF64.pas',
  panda.LinAlg.PTests.LUP in 'panda.LinAlg.PTests.LUP.pas',
  panda.LinAlg.PTests.GaussF64 in 'panda.LinAlg.PTests.GaussF64.pas',
  panda.Tests.NDATestCase in '..\..\Tests\panda.Tests.NDATestCase.pas',
  panda.Utils.StopWatch in '..\..\Utils\panda.Utils.StopWatch.pas';

{$R *.RES}

// Optional comma-separated leaf test names support focused benchmark confirmations.
function SelectTests(const aTest: ITest; aNames: TStrings): Boolean;
var children: IInterfaceList;
    I: Integer;
begin
  children := aTest.Tests;
  if (children = nil) or (children.Count = 0) then
    Result := aNames.IndexOf(aTest.Name) >= 0
  else begin
    Result := False;
    for I := 0 to children.Count - 1 do
      if SelectTests(children[I] as ITest, aNames) then Result := True;
  end;
  aTest.Enabled := Result;
end;

procedure ConfigureTests;
var names: TStringList;
begin
  if ParamCount = 0 then exit;
  names := TStringList.Create;
  try
    names.StrictDelimiter := True;
    names.Delimiter := ',';
    names.DelimitedText := ParamStr(1);
    if not SelectTests(RegisteredTests, names) then
      raise Exception.Create('No matching performance test names');
    Writeln('Selected ', RegisteredTests.CountEnabledTestCases, ' performance tests');
  finally
    names.Free;
  end;
end;

begin
  ConfigureTests;
  DUnitTestRunner.RunRegisteredTests;
end.

