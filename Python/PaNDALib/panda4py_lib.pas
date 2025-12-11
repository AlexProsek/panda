unit panda4py_lib;

interface

uses
    PythonEngine
  , pynda.Arrays
  ;

// for python 3.x
function PyInit_pandapy: PPyObject; cdecl;

var
  gEngine: TPythonEngine;

implementation

function PyInit_pandapy: PPyObject;
begin
  Result := nil;
  try
    // initialize Python enigne
    gEngine := TPythonEngine.Create(nil);
    gEngine.AutoFinalize := False;
    gEngine.UseLastKnownVersion := False;
    gEngine.RegVersion := '3.12';  //<-- Use the same version as the python 3.x your main program uses
    gEngine.APIVersion := 1013;
    gEngine.DllName := 'python312.dll';
    gEngine.LoadDll;

    TPyAPI.Initialize;
    Result := TPyAPI.PandaModule.Module;
  except
  end;
end;

initialization

finalization
  TPyAPI.Finalize;
  gEngine.Free;
end.

