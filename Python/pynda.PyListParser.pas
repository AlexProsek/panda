unit pynda.PyListParser;

interface

uses
    PythonEngine
  , panda.Intfs
  , System.Generics.Collections
  , System.TypInfo
  ;

const
  // data type codes
  DT_CHARCTER               = 'S1';
  DT_BOOL                   = '?';
  DT_UNSIGNED_CHAR          = 'B';
  DT_DOUBLE_COMPLEX         = 'D';
  DT_LOND_DOUBLE_COMPLEX    = 'G';
  DT_SINGLE_COMPLEX         = 'F';
  DT_UNSIGNED_INT           = 'I';
  DT_UNSIGNED_SHORT         = 'H';
  DT_UNSIGNED_LONG_INT      = 'L';
  DT_OBJECT                 = 'O';
  DT_UNSIGNED_LONG_LONG_INT = 'Q';
  DT_STRING                 = 'S';
  DT_UNICODE                = 'U';
  DT_VOID                   = 'V';
  DT_SIGNED_CHAR            = 'b';
  DT_DOUBLE_FLOAT           = 'd';
  DT_LONG_FLOAT             = 'g';
  DT_SINGLE_FLOAT           = 'f';
  DT_INT                    = 'i';
  DT_SHORT                  = 'h';
  DT_LONG_INT               = 'l';
  DT_LONG_LONG_INT          = 'q';

type
  TPyListParser = class
  protected type
    TSizeGetter = function (obj: PPyObject): NativeInt; cdecl;
    TItemGetter = function (obj: PPyObject; i: NativeInt): PPyObject; cdecl;
    TTypeCheck = function (obj: PPyObject): Boolean of object;

    TItData = record
      Coll: PPyObject;
      Last: NativeInt;
      Pos: NativeInt;
      It: TItemGetter;
    end;

  protected
    fEngine: TPythonEngine;
    fSt: TArray<TItData>;
    fDims: TList<NativeInt>;
    procedure InitStack(aValue: PPyObject);
    function MoveNext(var aDimCheck: Boolean): Boolean;
    procedure GetGetters(obj: PPyObject; out aSizeGetter: Pointer; out aItemGetter: Pointer);
  public
    constructor Create(aEngine: TPythonEngine);
    destructor Destroy; override;
    function GetFormat(aValue: PPyObject; out aType: Char; out aDims: TArray<NativeInt>): Boolean;
    function CopyTo(aValue: PPyObject; const aArr: INDArray): Boolean;
    function Read(aValue: PPyObject; out aArr: INDArray): Boolean;

    property Engine: TPythonEngine read fEngine;
  end;

implementation

uses
    panda.Arrays
  ;

{$region 'TPyArrayParser'}

constructor TPyListParser.Create(aEngine: TPythonEngine);
begin
  fEngine := aEngine;
  fDims := TList<NativeInt>.Create;
end;

destructor TPyListParser.Destroy;
begin
  fDims.Free;
  inherited;
end;

procedure TPyListParser.GetGetters(obj: PPyObject; out aSizeGetter: Pointer; out aItemGetter: Pointer);
begin
  with Engine do begin
    if PyList_Check(obj) then begin
      aSizeGetter := @PyList_Size;
      aItemGetter := @PyList_GetItem;
      exit;
    end;
    if PyTuple_Check(obj) then begin
      aSizeGetter := @PyTuple_Size;
      aItemGetter := @PyTuple_GetItem;
      exit;
    end;
  end;
  aSizeGetter := nil;
  aItemGetter := nil;
end;

procedure TPyListParser.InitStack(aValue: PPyObject);
var szGetter, itGetter: Pointer;
    I: Integer;
begin
  for I := 0 to High(fSt) do begin
    GetGetters(aValue, szGetter, itGetter);
    with fSt[I] do begin
      Coll := aValue;
      Last := TSizeGetter(szGetter)(aValue) - 1;
      Pos := 0;
      It := TItemGetter(itGetter);
      aValue := It(aValue, 0);
    end;
  end;
end;

function TPyListParser.MoveNext(var aDimCheck: Boolean): Boolean;
var I, J: Integer;
    sz: NativeInt;
    value: PPyObject;
    szGetter, itGetter: Pointer;
begin
  I := fDims.Count - 1;
  with fSt[I] do
    if Pos < Last then begin
      Inc(Pos);
      exit(True);
    end;

  while I > 0 do begin
    Dec(I);
    with fSt[I] do begin
      if Pos < Last then begin
        Inc(Pos);
        with fSt[I] do
          value := It(Coll, Pos);
        for J := I + 1 to High(fSt) do begin
          GetGetters(value, szGetter, itGetter);
          if szGetter = nil then begin
            aDimCheck := False;
            exit(True);
          end;

          sz := TSizeGetter(szGetter)(value);
          if sz <> fDims[J] then begin
            aDimCheck := False;
            exit(True);
          end;

          with fSt[J] do begin
            Coll := value;
            Last := sz - 1;
            Pos := 0;
            It := TItemGetter(itGetter);
            value := It(Coll, 0);
          end;
        end;
        exit(True);
      end;
    end;
  end;

  Result := False;
end;

function TPyListParser.GetFormat(aValue: PPyObject; out aType: Char;
  out aDims: TArray<NativeInt>): Boolean;
var count: NativeInt;
    szGetter, itGetter: Pointer;
    typeCheck: TTypeCheck;
    value: PPyObject;
    dimCheck: Boolean;
begin
  fDims.Clear;
  value := aValue;
  GetGetters(value, szGetter, itGetter);
  while szGetter <> nil do begin
    count := TSizeGetter(szGetter)(value);
    if count <= 0 then exit(False);
    fDims.Add(count);
    value := TItemGetter(itGetter)(value, 0);
    GetGetters(value, szGetter, itGetter);
  end;
  aDims := fDims.ToArray;

  SetLength(fSt, Length(aDims));
  dimCheck := True;
  with Engine do begin
    if PyFloat_Check(value) then begin
      aType := DT_DOUBLE_FLOAT;
      typeCheck := PyFloat_Check;
    end else
    if PyLong_Check(value) then begin
      aType := DT_INT;
      typeCheck := PyLong_Check;
    end else
      exit(False);

    InitStack(aValue);
    while MoveNext(dimCheck) do begin
      if not dimCheck then exit(False);
      with fSt[High(fSt)] do
        value := It(Coll, Pos);
      if (value = nil) or (not typeCheck(value)) then exit(False);
    end;
  end;

  Result := True;
end;

function TPyListParser.CopyTo(aValue: PPyObject; const aArr: INDArray): Boolean;
var J, count: NativeInt;
    shape: TArray<NativeInt>;
    szGetter, itGetter: Pointer;
    value: PPyObject;
    dimCheck: Boolean;
    it: TNDAIt;
begin
  fDims.Clear;
  value := aValue;
  GetGetters(value, szGetter, itGetter);
  while szGetter <> nil do begin
    count := TSizeGetter(szGetter)(value);
    if count <= 0 then exit(False);
    fDims.Add(count);
    value := TItemGetter(itGetter)(value, 0);
    GetGetters(value, szGetter, itGetter);
  end;

  shape := aArr.Shape;
  if Length(shape) <> fDims.Count then exit(False);
  for J := 0 to High(shape) do
    if fDims[J] <> shape[J] then exit(False);

  it := TNDAIt.Create(aArr);
  try
    SetLength(fSt, fDims.Count);
    dimCheck := True;
    with Engine do begin
      it.MoveNext;
      if PyFloat_Check(value) then begin
        if DoubleQ(aArr.GetItemType) then exit(False);
        InitStack(aValue);
        PDouble(it.Current)^ := PyFloat_AsDouble(value);
        while MoveNext(dimCheck) do begin
          it.MoveNext;
          if not dimCheck then exit(False);
          with fSt[High(fSt)] do
            value := It(Coll, Pos);
          if (value = nil) or (not PyFloat_Check(value)) then exit(False);
          PDouble(it.Current)^ := PyFloat_AsDouble(value);
        end;
      end else
      if PyLong_Check(value) then begin
        if IntQ(aArr.GetItemType) then exit(False);
        InitStack(aValue);
        PInteger(it.Current)^ := PyLong_AsLong(value);
        while MoveNext(dimCheck) do begin
          it.MoveNext;
          if not dimCheck then exit(False);
          with fSt[High(fSt)] do
            value := It(Coll, Pos);
          if (value = nil) or (not PyLong_Check(value)) then exit(False);
          PInteger(it.Current)^ := PyLong_AsLong(value);
        end;
      end else
        exit(False);
    end;
    Result := True;
  finally
    it.Free;
  end;
end;

function TPyListParser.Read(aValue: PPyObject; out aArr: INDArray): Boolean;
var dims: TArray<NativeInt>;
    t: Char;
begin
  if not GetFormat(aValue, t, dims) then exit(False);

  case t of
    DT_INT:           aArr := TNDABuffer<Integer>.Create(dims);
    DT_DOUBLE_FLOAT:  aArr := TNDABuffer<Double>.Create(dims);
  else
    exit(False);
  end;

  Result := CopyTo(aValue, aArr);
end;

{$endregion}

end.
