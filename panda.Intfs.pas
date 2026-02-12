unit panda.Intfs;

interface

{$ifdef FPC}
  {$mode delphiunicode}{$H+}
{$endif}

uses
{$ifndef FPC}
    System.TypInfo
  , System.SysUtils
{$else}
    TypInfo
  , SysUtils
{$endif}
  ;

type
  TNDIndexType = (nditInt, nditSpan, nditSet);
  TNDIndexTypes = set of TNDIndexType;

  INDIndex = interface
  ['{7FCF9599-630F-4FC8-A2ED-34015A4AF18C}']
    function IndexType: TNDIndexType;
    /// <summary>
    ///   Points to the firts item of indices array.
    /// </summary>
    function RawIndexData: PByte;
  end;
  INDIndexSeq = array of INDIndex;

  INDIntIndex = interface(INDIndex)
  ['{D39179EC-BF0E-4C96-BCC3-B0E5FB22385B}']
    function Value: NativeInt;
  end;

  INDSpanIndex = interface(INDIndex)
  ['{78F0F0E9-5159-4712-A74A-4B5946FC21C1}']
    function Low: NativeInt;
    function High: NativeInt;
    function Step: NativeInt;
  end;

  INDSetIndex = interface(INDIndex)
  ['{8C7A8B48-51EB-49D2-BEBA-235D4117441C}']
    function GetItem(I: NativeInt): NativeInt;
    function GetCount: NativeInt;
    function Indices: TArray<NativeInt>;

    property Item[I: NativeInt]: NativeInt read GetItem; default;
    property Count: NativeInt read GetCount;
  end;

const
  NDAF_C_CONTIGUOUS     = $0001;
  NDAF_F_CONTIGUOUS     = $0002;
  NDAF_OWNDATA          = $0004;
  NDAF_ALIGNED          = $0100;
  NDAF_WRITEABLE        = $0400;

  NDAF_CONTIGUOUS = NDAF_C_CONTIGUOUS or NDAF_F_CONTIGUOUS;
  NDAF_COW = NDAF_C_CONTIGUOUS or NDAF_OWNDATA or NDAF_WRITEABLE;

type
  TNDAShape = TArray<NativeInt>;

  INDArray = interface
  ['{54C51300-E37C-45AF-ABC6-856905499C7D}']
    function GetItemType: PTypeInfo;
    function Data: PByte;
    function NDim: Integer;
    function Shape: TArray<NativeInt>;
    function Strides: TArray<NativeInt>;
    function ItemSize: Integer;
    function Size: NativeInt;
    function Flags: Cardinal;
    procedure SetFlags(aValue: Cardinal);
    procedure UnsetFlags(aValue: Cardinal);
  end;

  INDArray<T> = interface(INDArray)
  ['{8875EF1F-39E5-4AE9-B741-F355B841C28A}']
    function GetPart(const aIdx: INDIndexSeq): INDArray<T>;
    procedure SetPart(const aIdx: INDIndexSeq; const aValue: INDArray<T>);
    function GetItem(const aIdx: array of NativeInt): T;
    procedure SetItem(const aIdx: array of NativeInt; const aValue: T);
    function Reshape(const aShape: array of NativeInt): INDArray<T>;

    property Part[const aIdx: INDIndexSeq]: INDArray<T> read GetPart write SetPart; default;
//    property Item[const aIdx: array of NativeInt]: T read GetItem write SetItem;
  end;

  INDScalar<T> = interface(INDArray<T>)
  ['{50432928-387E-4896-AF4F-D6DD36F7A5B0}']
    function GetValue: T;
    procedure SetValue(const aValue: T);

    property Value: T read GetValue write SetValue;
  end;

  INDAVec<T> = interface(INDArray<T>)
  ['{AE177E2B-BF20-4461-97FD-6EA459D41368}']
    function Stride: NativeInt;
    function Length: NativeInt;
  end;

  TNDArray<T> = record
  private
    fArr: INDArray<T>;
  public
    class operator Explicit(const aArr: INDArray): TNDArray<T>;
    function GetPart(const aIdx: INDIndexSeq): INDArray<T>;
    procedure SetPart(const aIdx: INDIndexSeq; const aValue: INDArray<T>);
    function NDim: Integer;
    function Shape: TArray<NativeInt>;

    property Part[const aIdx: INDIndexSeq]: INDArray<T> read GetPart write SetPart; default;
  end;

  TNDAItems<T> = record
  private
    fArr: INDArray<T>;
  public
    class operator Implicit(const aArr: INDArray<T>): TNDAItems<T>;
    class operator Implicit(const aArr: TNDAItems<T>): INDArray<T>;
    function GetItem(const aIdx: array of NativeInt): T;
    procedure SetItem(const aIdx: array of NativeInt; const aValue: T);
    function NDim: Integer;
    function Shape: TArray<NativeInt>;

    property Item[const aIdx: array of NativeInt]: T read GetItem write SetItem; default;
    property Base: INDArray<T> read fArr;
  end;

  // TMatSpec describes a rectangular 2D array
  TMatSpec = record
    Data: PByte;
    ElType: PTypeInfo;
    ElSize: Integer;
    NRows: NativeInt;
    NCols: NativeInt;
    RStep: NativeInt;
    CStep: NativeInt;
  end;

  // In place procedure types ( X <- F(X, Y) or Y <- F(X, Y) )
  TIPProcVS<T> = procedure (N: NativeInt; X: PByte; IncX: NativeInt; const Y: T);
  TIPProcVV = procedure (N: NativeInt; X: PByte; IncX: NativeInt; Y: PByte; IncY: NativeInt);

  TNDAFuncNS<T> = procedure (const aArr: INDArray<T>; var aRes: T);
  TNDAFuncNS<T, U> = procedure (const aArr: INDArray<T>; var aRes: U);
  TNDAFuncVVS<T> = procedure (const aX, aY: INDAVec<T>; var aRes: T);
  TNDAFuncVVV<T> = procedure (const aX, aY: INDAVec<T>; var aRes: INDAVec<T>);

  ENDAError = class(Exception);
  ENDAShapeError = class(ENDAError);
  ENDAPartError = class(ENDAError);
  ENDAIndexError = class(ENDAError);
  ENDAWriteError = class(ENDAError);
  ENDACastError = class(ENDAError);
  ENDAMapError = class(ENDAError);

  function SameQ(aT1, aT2: PTypeInfo): Boolean; overload;
  function DoubleQ(aType: PTypeInfo): Boolean; inline;
  function IntQ(aType: PTypeInfo): Boolean; inline;
  function CContiguousQ(const aArr: INDArray): Boolean; inline;
  function FContiguousQ(const aArr: INDArray): Boolean; inline;
  function ContiguousQ(const aArr: INDArray): Boolean; inline;
  function WriteableQ(const aArr: INDArray): Boolean; inline;

  procedure GetMatSpec(const aArr: INDArray; out aSpec: TMatSpec);
  procedure SwapMatAxes(var aSpec: TMatSpec);

implementation

procedure GetMatSpec(const aArr: INDArray; out aSpec: TMatSpec);
var s: TArray<NativeInt>;
begin
  Assert(aArr.NDim = 2);
  with aSpec do begin
    Data := aArr.Data;
    ElType := aArr.GetItemType;
    ElSize := aArr.ItemSize;
    s := aArr.Shape;
    NRows := s[0];
    NCols := s[1];
    s := aArr.Strides;
    RStep := s[0];
    CStep := s[1];
  end;
end;

procedure SwapMatAxes(var aSpec: TMatSpec);
var tmp: NativeInt;
begin
  with aSpec do begin
    tmp := NRows; NRows := NCols; NCols := tmp;
    tmp := RStep; RStep := CStep; CStep := tmp;
  end;
end;

function SameQ(aT1, aT2: PTypeInfo): Boolean;
var td1, td2: PTypeData;
begin
  if aT1^.Kind <> aT2^.Kind then exit(False);

  td1 := GetTypeData(aT1);
  td2 := GetTypeData(aT2);

  case aT1^.Kind of
    tkInteger:  Result := (td1^.OrdType = td2^.OrdType);
    tkInt64:    Result := (td1^.MinInt64Value = td2^.MinInt64Value);
    tkFloat:    Result := (td1^.FloatType = td2^.FloatType);
  else
    Result := (aT1 = aT2);
  end;
end;

function DoubleQ(aType: PTypeInfo): Boolean;
begin
  Result := (aType^.Kind = tkFloat) and (GetTypeData(aType)^.FloatType = ftDouble);
end;

function IntQ(aType: PTypeInfo): Boolean;
begin
{$if SizeOf(NativeInt) = 4}
  Result := (aType^.Kind = tkInteger) and (GetTypeData(aType)^.OrdType = otSLong);
{$else}
  Result := (aType^.Kind = tkInt64);
{$endif}
end;

function CContiguousQ(const aArr: INDArray): Boolean;
begin
  Result := (aArr.Flags and NDAF_C_CONTIGUOUS) <> 0;
end;

function FContiguousQ(const aArr: INDArray): Boolean;
begin
  Result := (aArr.Flags and NDAF_F_CONTIGUOUS) <> 0;
end;

function ContiguousQ(const aArr: INDArray): Boolean;
begin
  Result := (aArr.Flags and NDAF_CONTIGUOUS) <> 0;
end;

function WriteableQ(const aArr: INDArray): Boolean;
begin
  Result := (aArr.Flags and NDAF_WRITEABLE) <> 0;
end;

{$region 'TNDArray<T>'}

class operator TNDArray<T>.Explicit(const aArr: INDArray): TNDArray<T>;
begin
  if not SameQ(aArr.GetItemType, TypeInfo(T)) then
    raise ENDACastError.Create('Incompatible types');

  if not Supports(aArr, INDArray<T>, Result.fArr) then
    raise ENDACastError.CreateFmt('INDArray<%s> is not supported', [GetTypeName(TypeInfo(T))]);
end;

function TNDArray<T>.GetPart(const aIdx: INDIndexSeq): INDArray<T>;
begin
  Result := fArr.GetPart(aIdx);
end;

procedure TNDArray<T>.SetPart(const aIdx: INDIndexSeq; const aValue: INDArray<T>);
begin
  fArr.SetPart(aIdx, aValue);
end;

function TNDArray<T>.NDim: Integer;
begin
  Result := fArr.NDim;
end;

function TNDArray<T>.Shape: TArray<NativeInt>;
begin
  Result := fArr.Shape;
end;

{$endregion}

{$region 'TNDAItems<T>'}

class operator TNDAItems<T>.Implicit(const aArr: INDArray<T>): TNDAItems<T>;
begin
  Result.fArr := aArr;
end;

class operator TNDAItems<T>.Implicit(const aArr: TNDAItems<T>): INDArray<T>;
begin
  Result := aArr.fArr;
end;

function TNDAItems<T>.GetItem(const aIdx: array of NativeInt): T;
begin
  Result := fArr.GetItem(aIdx);
end;

procedure TNDAItems<T>.SetItem(const aIdx: array of NativeInt; const aValue: T);
begin
  fArr.SetItem(aIdx, aValue);
end;

function TNDAItems<T>.NDim: Integer;
begin
  Result := fArr.NDim;
end;

function TNDAItems<T>.Shape: TArray<NativeInt>;
begin
  Result := fArr.Shape;
end;

{$endregion}

end.
