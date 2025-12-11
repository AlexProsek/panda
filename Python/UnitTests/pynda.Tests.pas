unit pynda.Tests;

interface

uses
    TestFramework
  , PythonVersions
  , PythonEngine
  , panda.Intfs
  , panda.Arrays
  , pynda.Arrays
  , panda.Tests.NDATestCase
  , Vcl.Forms
  , System.IOUtils
  , System.SysUtils
  , Windows
  ;

type
  TTestPyEngine = class(TPythonEngine)
  end;

  TPyTestCase = class abstract(TNDATestCase)
  protected class var
    fEngine: TTestPyEngine;
  protected
    procedure SetVar(const aName: String; aValue: PPyObject);
  public
    class constructor Create;
    class destructor Destroy;

    class property Engine: TTestPyEngine read fEngine;
  end;

  TPyApiTests = class(TPyTestCase)
  published
    procedure DefaultDescr;
    procedure IntTypestr;
    procedure IntDescr;
    procedure RecDescr;
    procedure NestedRecDescr;
  end;

  TPyArrayTests = class(TPyTestCase)
  published
    procedure SetItem1D;
    procedure FillArr1D;
    procedure Reshape;
    procedure Reverse2D;
  end;

  TNumpyTests = class(TPyTestCase)
  public
    class constructor Create;
  published
    procedure CvtPaNDA2Numpy;
    procedure UpdatePandaBuffViaNumpy;
    procedure TestReadOnlyArray;
    procedure UnsetNumpyWriteableFlag;
    procedure SetNumpyWriteableFlag;
    procedure NumpyArrWrapper;
  end;

implementation

{$region 'TPyTestCase'}

class constructor TPyTestCase.Create;
var s: String;
begin
  s := TPath.GetDirectoryName(Application.ExeName);
  s := TPath.Combine(s, '..\..\..\envs\py312_np2');
  // if not TDirectory.Exists(s) then ...
  if not SetEnvironmentVariable('PYTHONHOME', PChar(s)) then begin
    // exception
  end;
  s := Format('%s;%s', [
     TPath.Combine(s, 'Lib'),
     TPath.Combine(s, 'Lib\site-packages')
  ]);
  if not SetEnvironmentVariable('PYTHONPATH', PChar(s)) then begin
    // exception
  end;

  fEngine := TTestPyEngine.Create(nil);
  fEngine.DoOpenDll('Python312.dll');
  fEngine.AfterLoad;
  TPyAPI.Initialize;

  fEngine.ExecString('import pandapy as nda');
end;

class destructor TPyTestCase.Destroy;
begin
  TPyAPI.Finalize;
  fEngine.Free;
end;

procedure TPyTestCase.SetVar(const aName: String; aValue: PPyObject);
var dict: PPyObject;
begin
  with Engine do begin
    dict := PyModule_GetDict(GetMainModule);
    PyDict_SetItemString(dict, PAnsiChar(AnsiString(aName)), aValue);
  end;
end;

{$endregion}

{$region 'TPyApiTests'}

type
  TRGB = packed record
    R, G, B: Byte;
  end;

  TTestRec = record
    X: Word;
    Clr: TRGB;
  end;

type
  TPyAPI = class(pynda.Arrays.TPyAPI)
  end;

procedure TPyApiTests.DefaultDescr;
var descr: PPyObject;
    s: String;
begin
  with Engine do begin
    descr := TPyAPI.DefaultDescr(AnsiString('<i4'));
    try
      CheckTrue(Assigned(descr));
      SetVar('descr', descr);
      s := EvalStringAsStr('str(descr)');
      CheckEquals('[('''', ''<i4'')]', s);
    finally
      Py_XDECREF(descr);
    end;
  end;
end;

procedure TPyApiTests.IntTypestr;
var s: AnsiString;
begin
  s := TPyAPI.PyTypeStr(TypeInfo(Integer));
  CheckEquals('<i4', String(s));
end;

procedure TPyApiTests.IntDescr;
var s: String;
    sDescr: AnsiString;
    descr: PPyObject;
begin
  with Engine do begin
    TPyAPI.PyTypeDescr(TypeInfo(Integer), sDescr, descr);
    try
      CheckTrue(Assigned(descr));
      SetVar('descr', descr);
      s := EvalStringAsStr('str(descr)');
      CheckEquals('[('''', ''<i4'')]', s);
    finally
      Py_XDECREF(descr);
    end;
  end;
end;

procedure TPyApiTests.RecDescr;
var s: String;
    sDescr: AnsiString;
    descr: PPyObject;
begin
  with Engine do begin
    TPyAPI.PyTypeDescr(TypeInfo(TRGB), sDescr, descr);
    try
      CheckEquals('|V3', String(sDescr));
      CheckTrue(Assigned(descr));
      SetVar('descr', descr);
      s := EvalStringAsStr('str(descr)');
      CheckEquals('[(''R'', ''|u1''), (''G'', ''|u1''), (''B'', ''|u1'')]', s);
    finally
      Py_XDECREF(descr);
    end;
  end;
end;

procedure TPyApiTests.NestedRecDescr;
var s: String;
    sDescr: AnsiString;
    descr: PPyObject;
begin
  with Engine do begin
    TPyAPI.PyTypeDescr(TypeInfo(TTestRec), sDescr, descr);
    try
      CheckEquals('|V6', String(sDescr));
      CheckTrue(Assigned(descr));
      SetVar('descr', descr);
      s := EvalStringAsStr('str(descr)');
      CheckEquals('[(''X'', ''<u2''), ' +
        '(''Clr'', [(''R'', ''|u1''), (''G'', ''|u1''), (''B'', ''|u1'')]), ' +
        '('''', ''|V1'')]', s);
    finally
      Py_XDECREF(descr);
    end;
  end;
end;

{$endregion}

{$region 'TPyArrayTests'}

procedure TPyArrayTests.SetItem1D;
var a: INDArray<Integer>;
    dict, pyArr: PPyObject;
    items: TArray<Integer>;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3]);

  pyArr := TPyAPI.CreatePyArray(a);
  try
    with Engine do begin
      dict := PyModule_GetDict(GetMainModule);
      PyDict_SetItemString(dict, PAnsiChar('a'), pyArr);
      ExecString('a[1] = 4');
    end;
    TNDAUt.TryAsDynArray<Integer>(a, items);
    CheckEquals(3, Length(items));
    CheckEquals(1, items[0]);
    CheckEquals(4, items[1]);
    CheckEquals(3, items[2]);
  finally
    Engine.Py_XDECREF(pyArr);
  end;
end;

procedure TPyArrayTests.FillArr1D;
var a: INDArray<Integer>;
    dict, pyArr: PPyObject;
    items: TArray<Integer>;
    I: Integer;
begin
  a := TNDAUt.AsArray<Integer>([0, 0, 0]);

  pyArr := TPyAPI.CreatePyArray(a);
  try
    with Engine do begin
      dict := PyModule_GetDict(GetMainModule);
      PyDict_SetItemString(dict, PAnsiChar('a'), pyArr);
      ExecString('a[::] = 4');
    end;
    TNDAUt.TryAsDynArray<Integer>(a, items);
    CheckEquals(3, Length(items));
    for I := 0 to High(items) do
      CheckEquals(4, items[I]);
  finally
    Engine.Py_XDECREF(pyArr);
  end;
end;

procedure TPyArrayTests.Reshape;
var a: INDArray<Integer>;
    res: INDArray;
    dict, pyArr, pyRes: PPyObject;
    items: TArray<TArray<Integer>>;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6]]);

  pyRes := nil;
  pyArr := TPyAPI.CreatePyArray(a);
  try
    with Engine do begin
      dict := PyModule_GetDict(GetMainModule);
      PyDict_SetItemString(dict, PAnsiChar('a'), pyArr);
      pyRes := EvalString('a.reshape((3, 2))');
    end;
    CheckTrue(TPyAPI.TryAsArray(pyRes, res));
    CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(res, items));

    CheckEquals(3, Length(items));
    CheckEquals([1, 2], items[0]);
    CheckEquals([3, 4], items[1]);
    CheckEquals([5, 6], items[2]);
  finally
    Engine.Py_XDECREF(pyArr);
    Engine.Py_XDECREF(pyRes);
  end;
end;

procedure TPyArrayTests.Reverse2D;
var a: INDArray<Integer>;
    res: INDArray;
    dict, pyArr, pyRes: PPyObject;
    items: TArray<TArray<Integer>>;
begin
  a := TNDAUt.AsArray<Integer>([[1, 2, 3], [4, 5, 6], [7, 8, 9]]);

  pyRes := nil;
  pyArr := TPyAPI.CreatePyArray(a);
  try
    with Engine do begin
      dict := PyModule_GetDict(GetMainModule);
      PyDict_SetItemString(dict, PAnsiChar('a'), pyArr);
      pyRes := EvalString('a[::-1]');
    end;
    CheckTrue(TPyAPI.TryAsArray(pyRes, res));
    CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(res, items));

    CheckEquals(3, Length(items));
    CheckEquals([7, 8, 9], items[0]);
    CheckEquals([4, 5, 6], items[1]);
    CheckEquals([1, 2, 3], items[2]);
  finally
    Engine.Py_XDECREF(pyArr);
    Engine.Py_XDECREF(pyRes);
  end;
end;

{$endregion}

{$region 'TNumpyTests'}

class constructor TNumpyTests.Create;
begin
  fEngine.ExecString('import numpy as np');
end;

procedure TNumpyTests.CvtPaNDA2Numpy;
var a: INDArray<Integer>;
    pyArr: PPyObject;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3]);

  pyArr := TPyAPI.CreateNumpyArray(a);
  try
    CheckEquals('numpy.ndarray', String(pyArr^.ob_type^.tp_name));
  finally
    Engine.Py_XDECREF(pyArr);
  end;
end;

procedure TNumpyTests.UpdatePandaBuffViaNumpy;
var a: INDArray<Integer>;
    dict, pyArr: PPyObject;
    items: TArray<Integer>;
    I: Integer;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3]);

  pyArr := TPyAPI.CreateNumpyArray(a);
  try
    with Engine do begin
      dict := PyModule_GetDict(GetMainModule);
      PyDict_SetItemString(dict, PAnsiChar('a'), pyArr);
      ExecString('a[::] = 4');
    end;
    TNDAUt.TryAsDynArray<Integer>(a, items);
    CheckEquals(3, Length(items));
    for I := 0 to High(items) do
      CheckEquals(4, items[I]);
  finally
    Engine.Py_XDECREF(pyArr);
  end;
end;

procedure TNumpyTests.TestReadOnlyArray;
var a: INDArray<Integer>;
    dict, pyArr: PPyObject;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3]);
  a.UnsetFlags(NDAF_WRITEABLE);

  pyArr := TPyAPI.CreateNumpyArray(a);
  try
    with Engine do begin
      dict := PyModule_GetDict(GetMainModule);
      PyDict_SetItemString(dict, 'a', pyArr);

      ExpectedException := EPyValueError;
      ExecString('a[::] = 4');
    end;
  finally
    Engine.Py_XDECREF(pyArr);
  end;
end;

procedure TNumpyTests.UnsetNumpyWriteableFlag;
var a: INDArray<Integer>;
    dict, pyArr: PPyObject;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3]);

  pyArr := TPyAPI.CreateNumpyArray(a);
  try
    with Engine do begin
      dict := PyModule_GetDict(GetMainModule);
      PyDict_SetItemString(dict, 'a', pyArr);
      ExecString('a.setflags(write = 0)');

      ExpectedException := EPyValueError;
      ExecString('a[::] = 4');
    end;
  finally
    Engine.Py_XDECREF(pyArr);
  end;
end;

procedure TNumpyTests.SetNumpyWriteableFlag;
var a: INDArray<Integer>;
    dict, pyArr: PPyObject;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3]);
  a.UnsetFlags(NDAF_WRITEABLE);

  pyArr := TPyAPI.CreateNumpyArray(a);
  try
    with Engine do begin
      dict := PyModule_GetDict(GetMainModule);
      PyDict_SetItemString(dict, 'a', pyArr);

      ExpectedException := EPyValueError;
      ExecString('a.setflags(write = 1)');
    end;
  finally
    Engine.Py_XDECREF(pyArr);
  end;
end;

procedure TNumpyTests.NumpyArrWrapper;
var dict, pyArr: PPyObject;
    a: INDArray;
    items: TArray<Integer>;
    I: Integer;
begin
  with Engine do begin
    ExecString('a = np.array([1, 2, 3], dtype = ''i4'')');
    dict := PyModule_GetDict(GetMainModule);
    pyArr := PyDict_GetItemString(dict, 'a');
    try
      CheckTrue(TPyAPI.TryAsArray(pyArr, a));
      CheckTrue(TNDAUt.TryAsDynArray<Integer>(a, items));
      CheckEquals(3, Length(items));
      for I := 0 to High(items) do
        CheckEquals(I + 1, items[I]);
    finally
      Py_DECREF(pyArr);
    end;
  end;
end;

{$endregion}

initialization

  RegisterTest(TPyApiTests.Suite);
  RegisterTest(TPyArrayTests.Suite);
  RegisterTest(TNumpyTests.Suite);

end.
