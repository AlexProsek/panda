unit pynda.Arrays;

interface

uses
    panda.Intfs
  , panda.consts
  , panda.Arrays
  , panda.ArrManip
  , panda.Formatter
  , pandalib
  , pynda.PyListParser
  , PythonEngine
  , System.Generics.Collections
  , System.SysUtils
  , System.TypInfo
  , System.Rtti
  , System.Math
  ;

//typedef struct {
//  int two;              /* contains the integer 2 -- simple sanity check */
//  int nd;               /* number of dimensions */
//  char typekind;        /* kind in array --- character code of typestr */
//  int itemsize;         /* size of each element */
//  int flags;            /* flags indicating how the data should be interpreted */
//                        /*   must set ARR_HAS_DESCR bit to validate descr */
//  Py_ssize_t *shape;    /* A length-nd array of shape information */
//  Py_ssize_t *strides;  /* A length-nd array of stride information */
//  void *data;           /* A pointer to the first element of the array */
//  PyObject *descr;      /* NULL or data-description (same as descr key
//                                of __array_interface__) -- must set ARR_HAS_DESCR
//                                flag or this will be ignored. */
//}

{$MINENUMSIZE 4}

type
  PyArrayInterface = {$IFDEF CPUX86}packed{$ENDIF}record
    two: Integer;
    nd: Integer;
    typekind: AnsiChar;
    itemsize: Integer;
    flags: Integer;
    shape: PNativeUInt;
    strides: PNativeUInt;
    data: PByte;
    descr: PPyObject;
  end;
  PPyArrayInterface = ^PyArrayInterface;

const
  NPY_ARRAY_C_CONTIGUOUS    = $0001;
  NPY_ARRAY_F_CONTIGUOUS    = $0002;
  NPY_ARRAY_ALIGNED         = $0100;
  NPY_ARRAY_NOTSWAPPED      = $0200;
  NPY_ARRAY_WRITEABLE       = $0400;
  NPY_ARR_HAS_DESCR         = $0800;

type
  TPyNDArray = class(TPyObject)
    ndim: Integer;
  protected type
    TAttrGetter = function: PPyObject of object;
  protected
    fArr: INDArray;
    fAttrs: TDictionary<String, TAttrGetter>;
  {$region 'Attr getters'}
    function GetNDim: PPyObject;
    function GetShape: PPyObject;
    function GetStrides: PPyObject;
    function GetArrIntf: PPyObject;
  {$endregion}
    function GetPart(const aIdx: INDIndexSeq): PPyObject;
    function SetPart(const aIdx: INDIndexSeq; const aArr: INDArray): Boolean;
    function Dim2Py(const aValue: TArray<NativeInt>): PPyObject;
    function TryParseIdx(aObj: PPyObject; const aShape: TNDAShape; out aIdx: INDIndexSeq): Boolean;
    function TryParseIdxSet(aObj: PPyObject; aLength: NativeInt; out aIdxs: TArray<NativeInt>): Boolean;
    function TryParseIntArr(aObj: PPyObject; out aIdxs: TArray<NativeInt>): Boolean;
  public
    constructor CreateWith(PythonType: TPythonType; args, kwds: PPyObject); override;
    procedure AfterConstruction; override;

    // Basic services
    function GetAttrO(key: PPyObject) : PPyObject; override;
    function SetAttrO(key, value: PPyObject) : Integer; override;
    function Repr: PPyObject; override;
    // Mapping services
    function MpLength : NativeInt; override;
    function MpSubscript( obj : PPyObject) : PPyObject; override;
    function MpAssSubscript( obj1, obj2 : PPyObject) : Integer; override;

    // Number services
    function NbAdd(obj: PPyObject): PPyObject; override;
    function NbMultiply(obj: PPyObject): PPyObject; override;

    // Class methods
    class procedure RegisterMethods(PythonType: TPythonType); override;

    // Interface methods
    function Reshape(args: PPyObject) : PPyObject; cdecl;
  end;

  TNumpyArrayWrapper<T> = class(TNDA<T>)
  protected
    fData: PByte;
    fNumpyArr: PPyObject;
  public
    constructor Create(aArr: PPyObject; const aArrIntf: PyArrayInterface);
    destructor Destroy; override;
    function Data: PByte; override;
  end;

  TPyAPI = class
  public type
    TPyCapsule_GetPointer = function (capsule: PPyObject; name: PAnsiChar): Pointer; cdecl;
    TPySlice_Unpack = function (slice: PPyObject; var start, stop, step: NativeInt): Integer; cdecl;
  public class var
    PyCapsule_GetPointer: TPyCapsule_GetPointer;
    PySlice_Unpack: TPySlice_Unpack;

    PandaModule: TPythonModule;
    NDAType: TPythonType;
  protected class var
    NumpyModule: PPyObject;
    NumpyArr: PPyObject;
    NumpyArrType: PPyObject;
  protected
    class function DefaultDescr(const aDescr: AnsiString): PPyObject;
    class function FieldDescr(const aField, aDescr: AnsiString): PPyObject; overload;
    class function FieldDescr(const aField: AnsiString; aDescr: PPyObject): PPyObject; overload;
    class function PyTypeStr(aType: PTypeInfo): AnsiString;
  public
    class procedure Initialize;
    class procedure Finalize;
    class function CreatePyArray(const aArr: INDArray): PPyObject;
    class function CreateNumpyArray(const aArr: INDArray): PPyObject;
    class function TryAsArray(aObj: PPyObject; out aArr: INDArray): Boolean;
    class procedure PyTypeDescr(aType: PTypeInfo; out aTypeStr: AnsiString;
      out aDescr: PPyObject);
  end;

implementation

uses
  AnsiStrings
  ;

type
  TPyEngine = class(TPythonEngine)
  end;

{$region 'TPyAPI'}

class procedure TPyAPI.Initialize;
var engine: TPyEngine;
    v: PPyObject;
    s: String;
begin
  engine := TPyEngine(GetPythonEngine);

  with engine do begin
    PyCapsule_GetPointer  := Import('PyCapsule_GetPointer');
    PySlice_Unpack        := Import('PySlice_Unpack');

    NumpyModule := PyImport_ImportModule('numpy');
    if Assigned(NumpyModule) then begin
      v := PyObject_GetAttrString(NumpyModule, '__version__');
      if Assigned(v) then begin
        s := PyUnicodeAsString(v);
        Py_DECREF(v);
      end;
      NumpyArr := PyObject_GetAttrString(NumpyModule, 'array');
      NumpyArrType := PyObject_GetAttrString(NumpyModule, 'ndarray');
    end;
  end;

  // initialize pandapy module
  PandaModule := TPythonModule.Create(nil);
  PandaModule.Engine := engine;
  PandaModule.ModuleName := 'pandapy';
  PandaModule.Initialize;

  // create TPyNDArray type
  NDAType := TPythonType.Create(nil);
  NDAType.Engine := engine;
  NDAType.Module := PandaModule;
  NDAType.TypeName := 'array';
  NDAType.PyObjectClass := TPyNDArray;
  NDAType.Services.Mapping := [msLength, msSubscript, msAssSubscript];
  NDAType.Services.Number := [nsAdd, nsMultiply];
  NDAType.Initialize;
end;

class procedure TPyAPI.Finalize;
begin
  PandaModule.Free;
  NDAType.Free;
  if PythonOK then with GetPythonEngine do begin
    Py_XDECREF(NumpyModule);
    Py_XDECREF(NumpyArr);
  end;
  NumpyModule := nil;
  NumpyArr := nil;
end;

class function TPyAPI.CreatePyArray(const aArr: INDArray): PPyObject;
begin
  Result := NDAType.CreateInstance;
  (PythonToDelphi(Result) as TPyNDArray).fArr := aArr;
end;

class function TPyAPI.CreateNumpyArray(const aArr: INDArray): PPyObject;
var buff, args, kwargs: PPyObject;
begin
  buff := NDAType.CreateInstance;
  (PythonToDelphi(buff) as TPyNDArray).fArr := aArr;

  with GetPythonEngine do begin
    args := PyTuple_New(1);
    PyTuple_SetItem(args, 0, buff); // stolen buff ref - don't release manually

    kwargs := PyDict_New;
    PyDict_SetItemString(kwargs, 'copy', Py_False);

    Result := PyObject_Call(NumpyArr, args, kwargs);

    Py_DECREF(args);
    Py_DECREF(kwargs);
  end;
end;

class function TPyAPI.TryAsArray(aObj: PPyObject; out aArr: INDArray): Boolean;
var capsule: PPyObject;
    p: PPyArrayInterface;
begin
  Assert(Assigned(aObj));
  with GetPythonEngine do begin
    if IsType(aObj, NDAType.TheTypePtr) then begin
      aArr := (PythonToDelphi(aObj) as TPyNDArray).fArr;
      exit(True);
    end;

    if PyObject_IsInstance(aObj, NumpyArrType) <> 1 then exit(False);
    
    capsule := PyObject_GetAttrString(aObj, '__array_struct__');
    if capsule = nil then exit(False);
    try
      p := TPyAPI.PyCapsule_GetPointer(capsule, nil);
      if (p = nil) or (p^.two <> 2) then exit(False);

      case p^.typekind of
        'i':
          case p^.itemsize of
            4: aArr := TNumpyArrayWrapper<Integer>.Create(aObj, p^);
            8: aArr := TNumpyArrayWrapper<Int64>.Create(aObj, p^);
          else
            exit(False);
          end;
      else
        exit(False);
      end;
    finally
      Py_DECREF(capsule)
    end;

  end;
  Result := True;
end;

class function TPyAPI.PyTypeStr(aType: PTypeInfo): AnsiString;
begin
  case aType^.Kind of
//    tkChar:

    tkInteger, tkEnumeration:
      case GetTypeData(aType)^.OrdType of
        otSByte: Result := 'i1';
        otUByte: Result := 'u1';
        otSWord: Result := 'i2';
        otUWord: Result := 'u2';
        otSLong: Result := 'i4';
        otULong: Result := 'u4';
      end;

    tkFloat:
      case GetTypeData(aType)^.FloatType of
        ftSingle: Result := 'f4';
        ftDouble: Result := 'f8';
      end;

    tkInt64:
      if GetTypeData(aType)^.MinInt64Value = 0 then
        Result := 'u8'
      else
        Result := 'i8';
  else
    // todo
    raise ENotImplemented.Create('Not implemented yet.');
  end;

  if Result[2] = '1' then
    Result := '|' + Result
  else
  {$ifdef ENDIAN_BIG}
    Reuslt := '>' + Result;
  {$else}
    Result := '<' + Result;
  {$endif}
end;

class function TPyAPI.DefaultDescr(const aDescr: AnsiString): PPyObject;
var d: PPyObject;
begin
  with GetPythonEngine do begin
    d := PyTuple_New(2);
    PyTuple_SetItem(d, 0, PyUnicode_FromString(''));
    PyTuple_SetItem(d, 1, PyUnicode_FromString(PAnsiChar(aDescr)));

    Result := PyList_New(1);
    PyList_SetItem(Result, 0, d);
  end;
end;

class function TPyAPI.FieldDescr(const aField, aDescr: AnsiString): PPyObject;
begin
  with GetPythonEngine do begin
    Result := PyTuple_New(2);
    PyTuple_SetItem(Result, 0, PyUnicode_FromString(PAnsiChar(aField)));
    PyTuple_SetItem(Result, 1, PyUnicode_FromString(PAnsiChar(aDescr)));
  end;
end;

class function TPyAPI.FieldDescr(const aField: AnsiString; aDescr: PPyObject): PPyObject;
begin
  with GetPythonEngine do begin
    Result := PyTuple_New(2);
    PyTuple_SetItem(Result, 0, PyUnicode_FromString(PAnsiChar(aField)));
    PyTuple_SetItem(Result, 1, aDescr);
  end;
end;

class procedure TPyAPI.PyTypeDescr(aType: PTypeInfo; out aTypeStr: AnsiString;
  out aDescr: PPyObject);
var ctx: TRttiContext;
    t: TRttiRecordType;
    flds: TArray<TRttiField>;
    I, pos: Integer;
    fldTypeStr: AnsiString;
    itemDescr: PPyObject;
begin
  with GetPythonEngine do begin
    if aType^.Kind <> tkRecord then begin
      aTypeStr := PyTypeStr(aType);
      itemDescr := FieldDescr('', aTypeStr);
      aDescr := PyList_New(1);
      PyList_SetItem(aDescr, 0, itemDescr);
      exit;
    end;

    aDescr := PyList_New(0);
    ctx := TRttiContext.Create;
    t := (ctx.GetType(aType) as TRttiRecordType);
    aTypeStr := AnsiString(Format('|V%d', [t.TypeSize]));
    flds := t.GetDeclaredFields;
    pos := 0;
    for I := 0 to High(flds) do with flds[I] do begin
      if pos < Offset then begin
        itemDescr := FieldDescr('', AnsiStrings.Format('|V%d', [Offset - pos]));
        PyList_Append(aDescr, itemDescr);
        Py_DECREF(itemDescr);
        pos := Offset;
      end;
      if FieldType.TypeKind = tkRecord then begin
        PyTypeDescr(FieldType.Handle, fldTypeStr, itemDescr);
        itemDescr := FieldDescr(AnsiString(Name), itemDescr);
      end else begin
        fldTypeStr := PyTypeStr(FieldType.Handle);
        itemDescr := FieldDescr(AnsiString(Name), fldTypeStr);
      end;
      PyList_Append(aDescr, itemDescr);
      Py_DECREF(itemDescr);
      Inc(pos, FieldType.TypeSize);
    end;
    if pos < t.TypeSize then begin
      itemDescr := FieldDescr('', AnsiStrings.Format('|V%d', [t.TypeSize - pos]));
      PyList_Append(aDescr, itemDescr);
      Py_DECREF(itemDescr);
    end;
  end;
end;

{$endregion}

{$region 'TPyNDArray'}

constructor TPyNDArray.CreateWith(PythonType: TPythonType; args, kwds: PPyObject);
var engine: TPythonEngine;
    parser: TPyListParser;
    argCnt: NativeInt;
begin
  inherited CreateWith(PythonType, args, kwds);
  engine := GetPythonEngine;

  with engine do begin
    argCnt :=  PyTuple_Size(args);
    if argCnt = 0 then begin
      // todo: raise Python error
      exit;
    end;

    parser := TPyListParser.Create(engine);
    try
      if not parser.Read(PyTuple_GetItem(args, 0), fArr) then begin
        // todo: raise Python error
        exit;
      end;
    finally
      parser.Free;
    end;
  end;
end;

procedure TPyNDArray.AfterConstruction;
begin
  inherited;
  fAttrs := TDictionary<String, TAttrGetter>.Create;

  fAttrs.Add('ndim',  GetNDim);
  fAttrs.Add('shape', GetShape);
  fAttrs.Add('__array_interface__', GetArrIntf);
end;

function TPyNDArray.GetAttrO(key: PPyObject) : PPyObject;
var attrName: String;
    getter: TAttrGetter;
begin
  with GetPythonEngine do begin
    attrName := PyObjectAsString(key);
    if fAttrs.TryGetValue(attrName, getter) then
      Result := getter()
    else
      Result := inherited GetAttrO(key);
  end;
end;

function TPyNDArray.SetAttrO(key, value: PPyObject) : Integer;
begin
  with GetPythonEngine do begin
    Result := inherited SetAttrO(key, value);
  end;
end;

function TPyNDArray.Repr: PPyObject;
var fmt: TNDAFormatter;
begin
  fmt := TNDAFormatter.Create;
  try
    fmt.Prefix := 'array(';
    fmt.Suffix := ')';
    fmt.BeginBracket := '[';
    fmt.EndBracket := ']';
    fmt.NewLineType := nltEscSeq;

    with GetPythonEngine do
      Result := VariantAsPyObject(fmt.GetString(fArr));
  finally
    fmt.Free;
  end;
end;

function TPyNDArray.GetPart(const aIdx: INDIndexSeq): PPyObject;
var arr: INDArray;
begin
  with GetPythonEngine do begin
    arr := TNDAMan.GetPart(fArr, aIdx);
    if not Assigned(arr) then begin
      PyErr_SetString(PyExc_IndexError^, 'Item data is not accessible.');
      exit(nil);
    end;

    Result := PythonType.CreateInstance;
    (PythonToDelphi(Result) as TPyNDArray).fArr := arr;
  end;
end;

function TPyNDArray.SetPart(const aIdx: INDIndexSeq; const aArr: INDArray): Boolean;
var res: Integer;
begin
  with GetPythonEngine do begin
    res := TNDAMan.SetPart(fArr, aidx, aArr);
    if res <> 0 then begin
      PyErr_SetString(PyExc_IndexError^, 'Item types don''t match.');
      exit(False);
    end;

    Result := True;
  end;
end;

function TPyNDArray.MpLength: NativeInt;
begin
  if fArr.NDim > 0 then
    Result := fArr.Shape[0]
  else
    Result := 0;
end;

function  TPyNDArray.MpSubscript(obj: PPyObject) : PPyObject;
var idxSeq: INDIndexSeq;
begin
  if not TryParseIdx(obj, fArr.Shape, idxSeq) then exit(nil);
  Result := GetPart(idxSeq);
end;

function  TPyNDArray.MpAssSubscript(obj1, obj2: PPyObject) : Integer;
var engine: TPythonEngine;
    parser: TPyListParser;
    shape: TArray<NativeInt>;
    idxSeq: INDIndexSeq;
    view: INDArray;
    ival: Integer;
    td: PTypeData;
    t: Char;
begin
  engine := GetPythonEngine;

  with engine do begin
    if not TryParseIdx(obj1, fArr.Shape, idxSeq) then exit(-1);

    if PyLong_Check(obj2) then begin
      ival := PyLong_AsLong(obj2);
      td := GetTypeData(fArr.GetItemType);
      case fArr.GetItemType^.Kind of
        tkInteger:
          case td^.OrdType of
            otSLong: (fArr as INDArray<Integer>)[idxSeq] := TNDAUt.Scalar<Integer>(iVal);
            otULong: begin
              if ival < 0 then begin
                // error
                exit(-1);
              end;
              (fArr as INDArray<Cardinal>)[idxSeq] := TNDAUt.Scalar<Cardinal>(ival);
            end
          else
            // error
            exit(-1);
          end;

        tkFloat:
          case td^.FloatType of
            ftSingle: (fArr as INDArray<Single>)[idxSeq] := TNDAUt.Scalar<Single>(ival);
            ftDouble: (fArr as INDArray<Double>)[idxSeq] := TNDAUt.Scalar<Double>(iVal);
          else
            // error
            exit(-1);
          end;
      else
        // error
        exit(-1);
      end;
      exit(0);
    end;

    parser := TPyListParser.Create(engine);
    try
      if not parser.GetFormat(obj2, t, shape) then begin
        // error
        exit(-1);
      end;

      case t of
        DT_INT: begin
          if not SameQ(fArr.GetItemType, TypeInfo(Integer)) then begin
            // error
            exit(-1);
          end;
          view := (fArr as INDArray<Integer>)[idxSeq];
        end;

        DT_DOUBLE_FLOAT: begin
          if not DoubleQ(fArr.GetItemType) then begin
            // error
            exit(-1);
          end;
          view := (fArr as INDArray<Double>)[idxSeq];
        end;
      end;
      if not parser.CopyTo(obj2, view) then
        exit(-1);
    finally
      parser.Free;
    end;
  end;

  Result := 0;
end;

class procedure TPyNDArray.RegisterMethods(PythonType: TPythonType);
begin
  inherited;
  with PythonType do begin
    AddMethod( 'reshape', @TPyNDArray.Reshape, 'array.reshape(new_shape)' );
  end;
end;

function TPyNDArray.Dim2Py(const aValue: TArray<NativeInt>): PPyObject;
var I: Integer;
begin
  with GetPythonEngine do begin
    Result := PyTuple_New(Length(aValue));
    for I := 0 to High(aValue) do
      PyTuple_SetItem(Result, I, PyLong_FromLong(aValue[I]));
  end;
end;

function TPyNDArray.TryParseIdx(aObj: PPyObject; const aShape: TNDAShape;
  out aIdx: INDIndexSeq): Boolean;
var I, len, idx: NativeInt;
    start, stop, step: NativeInt;
    idxs: TArray<NativeInt>;
    item: PPyObject;
begin
  Result := False;

  with GetPythonEngine do begin
    // Integer index
    if PyLong_Check(aObj) then begin
      if Length(aShape) = 0 then begin
        PyErr_SetString(PyExc_IndexError^, PAnsiChar(AnsiString(csInvIdxForScalar)));
        exit;
      end;
      idx := PyLong_AsLong(aObj);

      if not InRange(idx, 0, aShape[0]) then begin
        PyErr_SetString(PyExc_IndexError^, 'Index out of range');
        exit;
      end;
      SetLength(aIdx, 1);
      aIdx[0] := NDI(idx);
      exit(True);
    end;

    // Slice index
    if PySlice_Check(aObj) then begin
      if PySlice_GetIndices(PPySliceObject(aObj), aShape[0], start, stop, step) < 0 then begin
        PyErr_SetString(PyExc_IndexError^, 'Invalid slice index');
        exit;
      end;
      SetLength(aIdx, 1);
      aIdx[0] := NDISpan(start, stop - Sign(step), step);
      exit(True);
    end;

    // List index
    if PyList_Check(aObj) then begin
      if not TryParseIdxSet(aObj, aShape[0], idxs) then begin
        PyErr_SetString(PyExc_IndexError^, 'Indices out of range');
        exit;
      end;
      SetLength(aIdx, 1);
      aIdx[0] := NDI(idxs);
      exit(True);
    end;

    // Tuple index
    if PyTuple_Check(aObj) then begin
      len := PyTuple_Size(aObj);
      SetLength(aIdx, len);
      for I := 0 to len - 1 do begin
        item := PyTuple_GetItem(aObj, I);
        // Integer index
        if PyLong_Check(item) then begin
          idx := PyLong_AsLong(item);
          if not InRange(idx, 0, aShape[I]) then begin
            PyErr_SetString(PyExc_IndexError^, 'Index out of range');
            exit;
          end;
          aIdx[I] := NDI(idx);
          continue;
        end;

        // Slice index
        if PySlice_Check(item) then begin
          if PySlice_GetIndices(PPySliceObject(item), aShape[I], start, stop, step) < 0 then begin
            PyErr_SetString(PyExc_IndexError^, 'Invalid slice index');
            exit;
          end;
          aIdx[I] := NDISpan(start, stop - 1, step);
          continue;
        end;

        // List index
        if PyList_Check(item) then begin
          if not TryParseIdxSet(item, aShape[I], idxs) then begin
            PyErr_SetString(PyExc_IndexError^, 'Indices out of range');
            exit;
          end;
          aIdx[I] := NDI(idxs);
          continue;
        end;
      end;
      exit(True);
    end;
  end;
end;

function TPyNDArray.TryParseIdxSet(aObj: PPyObject; aLength: NativeInt;
  out aIdxs: TArray<NativeInt>): Boolean;
var I, len: NativeInt;
    item: PPyObject;
begin
  with GetPythonEngine do begin
    len := PyList_Size(aObj);
    SetLength(aIdxs, len);
    for I := 0 to High(aIdxs) do begin
      item := PyList_GetItem(aObj, I);
      if PyLong_Check(item) then begin
        aIdxs[I] := PyLong_AsLong(item);
        if Abs(aIdxs[I]) >= aLength then exit(False);
        continue;
      end;

      exit(False);
    end;
  end;
  Result := True;
end;

function TPyNDArray.TryParseIntArr(aObj: PPyObject; out aIdxs: TArray<NativeInt>): Boolean;
var I, count: NativeInt;
    item: PPyObject;
begin
  with GetPythonEngine do begin
    if PyList_Check(aObj) then begin
      count := PyList_Size(aObj);
      SetLength(aIdxs, count);
      for I := 0 to High(aIdxs) do begin
        item := PyList_GetItem(aObj, I);
        if not PyLong_Check(item) then exit(False);
        aIdxs[I] := PyLong_AsLong(item);
      end;
      exit(True);
    end;

    if PyTuple_Check(aObj) then begin
      count := PyTuple_Size(aObj);
      SetLength(aIdxs, count);
      for I := 0 to High(aIdxs) do begin
        item := PyTuple_GetItem(aObj, I);
        if not PyLong_Check(item) then exit(False);
        aIdxs[I] := PyLong_AsLong(item);
      end;
      exit(True);
    end;

    Result := False;
  end;
end;

{$region 'Number services'}

function TPyNDArray.NbAdd(obj: PPyObject): PPyObject;
var arr: TPyNDArray;
    res: INDArray;
begin
  with GetPythonEngine do begin
    if IsType(obj, PythonType.TheTypePtr) then begin
      arr := (PythonToDelphi(obj) as TPyNDArray);
      if nda.Add([fArr, arr.fArr], res) then begin
        Result := PythonType.CreateInstance;
        (PythonToDelphi(Result) as TPyNDArray).fArr := res;
        exit;
      end;
    end;

    PyErr_SetString(PyExc_TypeError^, 'Unsupported operand type for +');
    Result := nil;
  end;
end;

function TPyNDArray.NbMultiply(obj: PPyObject): PPyObject;
var arr: TPyNDArray;
    res: INDArray;
begin
  with GetPythonEngine do begin
    if IsType(obj, PythonType.TheTypePtr) then begin
      arr := (PythonToDelphi(obj) as TPyNDArray);
      if nda.Multiply([fArr, arr.fArr], res) then begin
        Result := PythonType.CreateInstance;
        (PythonToDelphi(Result) as TPyNDArray).fArr := res;
        exit;
      end;
    end;

    PyErr_SetString(PyExc_TypeError^, 'Unsupported operand type for *');
    Result := nil;
  end;
end;

{$endregion}

{$region 'Interface methods'}

function TPyNDArray.Reshape(args: PPyObject): PPyObject;
var arr: INDArray;
    shape: TArray<NativeInt>;
    shapeArg: PPyObject;
    td: PTypeData;
begin
  with GetPythonEngine do begin
    Adjust(@Self);
    if not (
      (PyArg_ParseTuple(args, 'O', @shapeArg) <> 0)  and
      TryParseIntArr(shapeArg, shape))
    then
      exit(nil);

    td := GetTypeData(fArr.GetItemType);
    case fArr.GetItemType^.Kind of
      tkInteger:
        case td^.OrdType of
          otSLong: arr := (fArr as INDArray<Integer>).Reshape(shape);
          otULong: arr := (fArr as INDArray<Cardinal>).Reshape(shape);
        end;
      tkFloat:
        case td^.FloatType of
          ftSingle: arr := (fArr as INDArray<Single>).Reshape(shape);
          ftDouble: arr := (fArr as INDArray<Double>).Reshape(shape);
        end;
    end;

    if not Assigned(arr) then begin
      PyErr_SetString(PyExc_TypeError^,
        PAnsiChar(AnsiString(Format('Unsupported array type %s', [fArr.GetItemType^.Name])))
      );
      exit(nil);
    end;

    Result := PythonType.CreateInstance;
    (PythonToDelphi(Result) as TPyNDArray).fArr := arr;
  end;
end;

{$endregion}

{$region 'Attr getters'}

function TPyNDArray.GetNDim: PPyObject;
begin
  Result := GetPythonEngine.PyLong_FromLong(fArr.NDim);
end;

function TPyNDArray.GetShape: PPyObject;
begin
  Result := Dim2Py(fArr.Shape);
end;

function TPyNDArray.GetStrides: PPyObject;
begin
  Result := Dim2Py(fArr.Strides);
end;

function TPyNDArray.GetArrIntf: PPyObject;
var val, descr: PPyObject;
    iarr: TArray<NativeInt>;
    typeStr: AnsiString;
    I: Integer;
begin
  with GetPythonEngine do begin
    iarr := fArr.Shape;
    Result := PyDict_New();

    if (fArr.Flags and NDAF_WRITEABLE) = 0 then I := 1 else I := 0;
    val := Py_BuildValue('(Ki)', UInt64(fArr.Data), I);
    PyDict_SetItemString(Result, 'data', val);
    Py_DECREF(val);

    val := PyTuple_New(Length(iarr));
    for I := 0 to High(iarr) do
      PyTuple_SetItem(val, I, PyLong_FromLongLong(iarr[I]));
    PyDict_SetItemString(Result, 'shape', val);
    Py_DECREF(val);

    if (fArr.Flags and NDAF_C_CONTIGUOUS) = 0 then begin
      iarr := fArr.Strides;
      val := PyTuple_New(Length(iarr));
      for I := 0 to High(iarr) do
        PyTuple_SetItem(val, I, PyLong_FromLongLong(iarr[I]));
      PyDict_SetItemString(Result, 'strides', val);
      Py_DECREF(val);
    end else
      PyDict_SetItemString(Result, 'strides', Py_None);

    TPyAPI.PyTypeDescr(fArr.GetItemType, typeStr, descr);
    val := PyUnicode_FromString(PAnsiChar(typeStr));
    PyDict_SetItemString(Result, 'typestr', val);
    Py_DECREF(val);
    PyDict_SetItemString(Result, 'descr', descr);
    Py_DECREF(descr);

    val := PyLong_FromLong(3);
    PyDict_SetItemString(Result, 'version', val);
    Py_DECREF(val);
  end;
end;

{$endregion}

{$endregion}

{$region 'TNumpyArrayWrapper<T>'}

constructor TNumpyArrayWrapper<T>.Create(aArr: PPyObject; const aArrIntf: PyArrayInterface);
begin
  fNumpyArr := aArr;
  GetPythonEngine.Py_INCREF(fNumpyArr);
  with aArrIntf do begin
    SetLength(fShape, nd);
    SetLength(fStrides, nd);
    Move(shape^, fShape[0], nd * SizeOf(NativeUInt));
    Move(strides^, fStrides[0], nd * SizeOf(NativeUInt));
    fData := aArrIntf.data;

    fFlags := 0;
    if (flags and NPY_ARRAY_WRITEABLE) <> 0 then
      fFlags := NDAF_WRITEABLE;
  end;
end;

destructor TNumpyArrayWrapper<T>.Destroy;
begin
  GetPythonEngine.Py_DECREF(fNumpyArr);
  inherited;
end;

function TNumpyArrayWrapper<T>.Data: PByte;
begin
  Result := fData;
end;

{$endregion}

end.
