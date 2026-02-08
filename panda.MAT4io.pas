unit panda.MAT4io;

interface

uses
    panda.Intfs
  , panda.Nums
  , panda.Arrays
  , panda.ArrManip
  , panda.cvCvt
  , System.SysUtils
  , System.Classes
  , System.TypInfo
  , System.Math
  , System.Generics.Collections
  ;

type
  TMAT4Type = (mtNumeric, mtTextMat, mtSparseMat);
  TMAT4NumType = (ntLittleEndian, ntBigEndian, ntVAXDFloat, ntVAXGFloat, ntCray);

  TMAT4Header = packed record
    Fmt: Cardinal;
    MRows: Cardinal;
    NCols: Cardinal;
    Imagf: Cardinal;
    NameLen: Cardinal;
    procedure Init(const aM: INDArray; aNameLen: Integer;
      aNumType: TMAT4NumType);
  end;
  PMAT4Header = ^TMAT4Header;

  TMAT4io = class abstract
  protected
    fStream: TStream;
    fOwnStream: Boolean;
  public
    constructor Create(aStream: TStream; aOwnStream: Boolean = False); overload;
    destructor Destroy; override;
  end;

  TMAT4Exporter = class(TMAT4io)
  protected
    procedure WriteHeader(const aH: TMAT4Header);
    procedure WriteName(const aName: String);
    procedure WriteItems1(aIt: TNDAIt; pRe, pIm: PByte);
    procedure WriteItems2(aIt: TNDAIt; pRe, pIm: PByte);
    procedure WriteItems4(aIt: TNDAIt; pRe, pIm: PByte);
    procedure WriteItems8(aIt: TNDAIt; pRe, pIm: PByte);
  public
    constructor Create(const aFileName: String); overload;
    procedure WriteMatrix(const aMat: INDArray; const aName: String = '');
  end;

  TMAT4Importer = class(TMAT4io)
  public type
    TElType = (etUInt8, etInt16, etUInt16, etInt32, etSingle, etDouble,
      etCmplx64, etCmplx128);
  protected type
    TMatInfo = record
      Position: Int64;
      MatType: TMAT4Type;
      ElType: TElType;
      MRows, NCols: Cardinal;
      Name: String;
    end;
  protected
    fIndex: TArray<TMatInfo>;
    fIndexLoaded: Boolean;
    fStartPos: Int64;
    procedure ReadIndex;
    function ReadName(aNameLen: Integer): String;
    procedure ParseFmt(aFmt: Cardinal; out M, O, P, T: Byte);
    function GetMatIndex(const aName: String; out aIdx: NativeInt): Boolean;
    function ReadBuffer<T>(const aMatInfo: TMatInfo): INDArray<T>;
    function AdjustCmplxMat<T, U>(const aArr: INDArray<T>): INDArray<T>;
  public
    constructor Create(const aFileName: String); overload;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function ReadAll: TArray<INDArray>;
    function ReadLabels: TArray<String>;
    function Read<T>(I: NativeInt): INDArray<T>;
    function TryRead<T>(I: NativeInt; out aArr: INDArray<T>): Boolean;
    function TryReadTextArray(I: NativeInt; out aArr: TArray<String>): Boolean;
    function MatType(I: NativeInt): TMAT4Type;
    function MatElementType(I: NativeInt): TElType;
  end;

  EMAT4Error = class(Exception);

procedure MAT4Save(const aFileName: String; const aMat: INDArray; const aMatName: String = '');
function MAT4Load(const aFileName: String): TArray<INDArray>;

implementation

const
{$ifdef BIG_ENDIAN}
  cMAT4NUMType = ntBigEndian;
{$else}
  cMAT4NumType = ntLittleEndian;
{$endif}
  cUnsuppType = 'Unsupported element type %s';
  cInvFmt     = 'Invalid MAT4 format.';

procedure MAT4Save(const aFileName: String; const aMat: INDArray; const aMatName: String);
var exporter: TMAT4Exporter;
begin
  Assert(Assigned(aMat));
  exporter := TMAT4Exporter.Create(aFileName);
  try
    exporter.WriteMatrix(aMat, aMatName);
  finally
    exporter.Free;
  end;
end;

function MAT4Load(const aFileName: String): TArray<INDArray>;
var importer: TMAT4Importer;
begin
  importer := TMAT4Importer.Create(aFileName);
  try
    Result := importer.ReadAll;
  finally
    importer.Free;
  end;
end;

{$region 'TMAT4Header'}

procedure TMAT4Header.Init(const aM: INDArray; aNameLen: Integer; aNumType: TMAT4NumType);
var ti: PTypeInfo;
    pd: PTypeData;
    M, T, O, P: Byte;
    cmplx: Byte;
begin
  cmplx := 0;
  M := Ord(aNumType);
  T := Ord(mtNumeric);
  O := 0; // always zero (reserved for future use)
  ti := aM.GetItemType;
  pd := GetTypeData(ti);
  case ti^.Kind of
    tkInteger:
      case pd^.OrdType of
        otUByte: P := 5;
        otSWord: P := 3;
        otUWord: P := 4;
        otSLong: P := 2;
      else
        raise EMAT4Error.CreateFmt(cUnsuppType, [ti^.Name]);
      end;

    tkFloat:
      case pd^.FloatType of
        ftSingle: P := 1;
        ftDouble: P := 0;
      else
        raise EMAT4Error.CreateFmt(cUnsuppType, [ti^.Name]);
      end;

    tkRecord: begin
      if ti = TypeInfo(TCmplx64) then begin
        P := 1;
        cmplx := 1;
      end else
      if ti = TypeInfo(TCmplx128) then begin
        P := 0;
        cmplx := 1;
      end else
        raise EMAT4Error.CreateFmt(cUnsuppType, [ti^.Name]);
    end
  else
    raise EMAT4Error.CreateFmt(cUnsuppType, [ti^.Name]);
  end;
  Fmt := 1000 * M + 100 * O  + 10 * P + T;

  if aM.NDim > 2 then
    raise EMat4Error.CreateFmt('Too high dimension %d (only 1 or 2 is supported).', [aM.NDim]);

  if aM.NDim = 1 then begin
    MRows := 1;
    NCols := aM.Shape[0];
  end else begin
    MRows := aM.Shape[0];
    NCols := aM.Shape[1];
  end;
  Imagf := cmplx;
  NameLen := aNameLen + 1; // +1 for termination character \0
end;

{$endregion}

{$region 'TMAT4io'}

constructor TMAT4io.Create(aStream: TStream; aOwnStream: Boolean);
begin
  fStream := aStream;
  fOwnStream := aOwnStream;
end;

destructor TMAT4io.Destroy;
begin
  if fOwnStream then
    fStream.Free;
end;

{$endregion}

{$region 'TMAT4Exporter'}

constructor TMAT4Exporter.Create(const aFileName: String);
var fs: TFileStream;
begin
  fs := TFileStream.Create(aFileName, fmCreate or fmOpenWrite);
  Create(fs, True);
end;

procedure TMAT4Exporter.WriteHeader(const aH: TMAT4Header);
begin
  fStream.Write(aH, SizeOf(aH));
end;

procedure TMAT4Exporter.WriteName(const aName: String);
var s: AnsiString;
const cNull: AnsiChar = #0;
begin
  if Length(aName) > 0 then begin
    s := AnsiString(aName);
    fStream.Write(s[1], Length(s));
  end;
  fStream.WriteData(cNull);
end;

procedure TMAT4Exporter.WriteItems1(aIt: TNDAIt; pRe, pIm: PByte);
begin
  Assert(pIm = nil);
  while aIt.MoveNext do begin
    pRe^ := aIt.Current^;
    Inc(pRe);
  end;
end;

procedure TMAT4Exporter.WriteItems2(aIt: TNDAIt; pRe, pIm: PByte);
begin
  Assert(pIm = nil);
  while aIt.MoveNext do begin
    PWord(pRe)^ := PWord(aIt.Current)^;
    Inc(pRe, SizeOf(Word));
  end;
end;

procedure TMAT4Exporter.WriteItems4(aIt: TNDAIt; pRe, pIm: PByte);
begin
  if pIm = nil then begin
    while aIt.MoveNext do begin
      PCardinal(pRe)^ := PCardinal(aIt.Current)^;
      Inc(pRe, SizeOf(Cardinal));
    end;
  end else begin
    while aIt.MoveNext do begin
      PCardinal(pRe)^ := PCardinal(aIt.Current)^;
      PCardinal(pIm)^ := PCardinal(aIt.Current + 4)^;
      Inc(pRe, SizeOf(Cardinal));
      Inc(pIm, SizeOf(Cardinal));
    end;
  end;
end;

procedure TMAT4Exporter.WriteItems8(aIt: TNDAIt; pRe, pIm: PByte);
begin
  if pIm = nil then begin
    while aIt.MoveNext do begin
      PInt64(pRe)^ := PInt64(aIt.Current)^;
      Inc(pRe, SizeOf(Int64));
    end;
  end else begin
    while aIt.MoveNext do begin
      PInt64(pRe)^ := PInt64(aIt.Current)^;
      PInt64(pIm)^ := PInt64(aIt.Current + 8)^;
      Inc(pRe, SizeOf(Int64));
      Inc(pIm, SizeOf(Int64));
    end;
  end;
end;

procedure TMAT4Exporter.WriteMatrix(const aMat: INDArray; const aName: String);
var it: TNDAIt;
    h: TMAT4Header;
    elSz: Integer;
    buff: TBytes;
    pRe, pIm: PByte;
begin
  elSz := aMat.ItemSize;
  h.Init(aMat, Length(aName), cMAT4NumType);
  WriteHeader(h);
  WriteName(aName);

  if
    (h.Imagf = 0) and ((h.NCols = 1) or (h.MRows = 1)) and
    CContiguousQ(aMat)
  then begin
    fStream.Write(aMat.Data^, Max(h.NCols, h.MRows) * elSz);
    exit;
  end;

  SetLength(buff, h.MRows * h.NCols * elSz);
  if h.Imagf = 0 then begin
    pRe := PByte(buff);
    pIm := nil;
  end else begin
    pRe := PByte(buff);
    pIm := PByte(@buff[Length(buff) div 2]);
    elSz := elSz shr 1;
  end;

  if (h.MRows = 1) or (h.NCols = 1) then
    it := TNDAIt.Create(aMat)
  else
    it := TNDAIt.Create(aMat, 0, -1, [1, 0]);
  try
    case elSz of
      1: WriteItems1(it, pRe, pIm);
      2: WriteItems2(it, pRe, pIm);
      4: WriteItems4(it, pRe, pIm);
      8: WriteItems8(it, pRe, pIm);
    end;
  finally
    it.Free;
  end;
  fStream.Write(buff, 0, Length(buff));
end;

{$endregion}

{$region 'TMAT4Importer'}

constructor TMAT4Importer.Create(const aFileName: String);
var fs: TFileStream;
begin
  fs := TFileStream.Create(aFileName, fmOpenRead);
  Create(fs, True);
end;

procedure TMAT4Importer.AfterConstruction;
begin
  inherited;
  fStartPos := fStream.Position;
  fIndexLoaded := False;
end;

procedure TMAT4Importer.BeforeDestruction;
begin
  inherited;
end;

procedure TMAT4Importer.ReadIndex;
var h: TMAT4Header;
    idx: TList<TMatInfo>;
    m, o, p, t: Byte;
    sz: Cardinal;
    mi: TMatInfo;
    mt: TMAT4Type;
    et: TElType;
begin
  idx := TList<TMatInfo>.Create;
  try
    fStream.Position := fStartPos;
    while fStream.Size - fStream.Position >= SizeOf(TMAT4Header) do begin
      fStream.Read(h, SizeOf(TMAT4Header));
      ParseFmt(h.Fmt, m, o, p, t);
      case P of
        0: begin
          if h.Imagf > 0 then
            et := etCmplx128
          else
            et := etDouble;
          sz := 8;
        end;
        1: begin
          if h.Imagf > 0 then
            et := etCmplx64
          else
            et := etSingle;
          sz := 4;
        end;
        2: begin
          et := etInt32;
          sz := 4;
        end;
        3: begin
          et := etInt16;
          sz := 2;
        end;
        4: begin
          et := etUInt16;
          sz := 2;
        end;
        5: begin
          et := etUInt8;
          sz := 1;
        end;
      else
        raise EMAT4Error.Create(cInvFmt);
      end;
      case t of
        0: mt := mtNumeric;
        1: mt := mtTextMat;
        2: mt := mtSparseMat;
      else
        raise EMAT4Error.Create(cInvFmt);
      end;
      if h.Imagf > 0 then
        sz := 2 * sz;
      with mi do begin
        Position := fStream.Position + h.NameLen;
        ElType := et;
        MatType := mt;
        NCols := h.NCols;
        MRows := h.MRows;
        Name := ReadName(h.NameLen);
      end;
      idx.Add(mi);
      fStream.Seek(h.MRows * h.NCols * sz, soFromCurrent);
    end;
    fStream.Position := fStartPos;
    fIndex := idx.ToArray;
    fIndexLoaded := True;
  finally
    idx.Free;
  end;
end;

function TMAT4Importer.ReadName(aNameLen: Integer): String;
var s: AnsiString;
begin
  if aNameLen > 1 then begin
    SetLength(s, aNameLen - 1);
    fStream.Read(s[1], aNameLen - 1);
    Result := String(s);
  end;
  fStream.Seek(1, soFromCurrent);
end;

function TMAT4Importer.GetMatIndex(const aName: String; out aIdx: NativeInt): Boolean;
var I: NativeInt;
begin
  for I := 0 to High(fIndex) do
    if fIndex[I].Name = aName then begin
      aIdx := I;
      exit(True);
    end;
  Result := False;
end;

procedure TMAT4Importer.ParseFmt(aFmt: Cardinal; out M, O, P, T: Byte);
var q, r: Word;
begin
  if (aFmt and $FFFF0000) <> 0 then
    bswap32(@aFmt, @aFmt, 4);
  q := Word(aFmt);
  DivMod(q, 10, q, r);
  T := r;
  DivMod(q, 10, q, r);
  P := r;
  DivMod(q, 10, q, r);
  O := r;
  DivMod(q, 10, q, r);
  M := r;
end;

function TMAT4Importer.ReadLabels: TArray<String>;
var I: NativeInt;
begin
  if not fIndexLoaded then ReadIndex;

  SetLength(Result, Length(fIndex));
  for I := 0 to High(Result) do
    Result[I] := fIndex[I].Name;
end;

function TMAT4Importer.ReadAll: TArray<INDArray>;
var I, top: NativeInt;
begin
  if not fIndexLoaded then ReadIndex;

  SetLength(Result, Length(fIndex));
  top := 0;
  for I := 0 to High(fIndex) do
    with fIndex[I] do begin
      if MatType <> mtNumeric then continue;

      case ElType of
        etUInt8:    Result[top] := Read<UInt8>(I);
        etInt16:    Result[top] := Read<Int16>(I);
        etUInt16:   Result[top] := Read<UInt16>(I);
        etInt32:    Result[top] := Read<Int32>(I);
        etSingle:   Result[top] := Read<Single>(I);
        etDouble:   Result[top] := Read<Double>(I);
        etCmplx64:  Result[top] := Read<TCmplx64>(I);
        etCmplx128: Result[top] := Read<TCmplx128>(I);
      else
        continue;
      end;
      Inc(top);
    end;
  SetLength(Result, top);
end;

function TMAT4Importer.ReadBuffer<T>(const aMatInfo: TMatInfo): INDArray<T>;
begin
  with aMatInfo do begin
    fStream.Seek(Position, soFromBeginning);
    if MRows = 1 then begin
      if NCols = 1 then begin
        Result := TNDScalar<T>.Create;
        fStream.Read(Result.Data^, SizeOf(T));
        exit;
      end;
      Result := TNDABuffer<T>.Create([NCols]);
      fStream.Read(Result.Data^, NCols * SizeOf(T));
      exit;
    end;

    if NCols = 1 then begin
      Result := TNDABuffer<T>.Create([MRows, 1]);
      fStream.Read(Result.Data^, MRows * SizeOf(T));
      exit;
    end;

    Result := TNDABuffer<T>.Create([NCols, MRows]);
    fStream.Read(Result.Data^, MRows * NCols * SizeOf(T));
  end;
end;

function TMAT4Importer.AdjustCmplxMat<T, U>(const aArr: INDArray<T>): INDArray<T>;
var src, dst: INDArray<U>;
    rCnt, cCnt: NativeInt;
begin
  Assert(2*SizeOf(U) = SizeOf(T));
  if aArr.NDim = 1 then begin
    cCnt := aArr.Shape[0];
    Result := TNDABuffer<T>.Create([cCnt]);
    src := TNDPackedArray<U>.Create(aArr.Data, [2, cCnt]);
    dst := TNDPackedArray<U>.Create(Result.Data, [cCnt, 2]);
    TNDAMan.Transpose<U>(src, dst, [1, 0]);
  end else begin
    rCnt := aArr.Shape[0];
    cCnt := aArr.Shape[1];
    Result := TNDABuffer<T>.Create([cCnt, rCnt]);
    src := TNDPackedArray<U>.Create(aArr.Data, [2, rCnt, cCnt]);
    dst := TNDPackedArray<U>.Create(Result.Data, [cCnt, rCnt, 2]);
    TNDAMan.Transpose<U>(src, dst, [2, 1, 0]);
  end;
end;

function TMAT4Importer.Read<T>(I: NativeInt): INDArray<T>;
begin
  if not TryRead<T>(I, Result) then Result := nil;
end;

function TMAT4Importer.TryRead<T>(I: NativeInt; out aArr: INDArray<T>): Boolean;
var arr: INDArray;
    et: TElType;
begin
  if not fIndexLoaded then ReadIndex;

  if not (InRange(I, 0, High(fIndex)) and (fIndex[I].MatType = mtNumeric)) then
    exit(False);

  et := fIndex[I].ElType;
  case et of
    etUInt8:    arr := ReadBuffer<UInt8>(fIndex[I]);
    etInt16:    arr := ReadBuffer<Int16>(fIndex[I]);
    etUInt16:   arr := ReadBuffer<UInt16>(fIndex[I]);
    etInt32:    arr := ReadBuffer<Int32>(fIndex[I]);
    etSingle:   arr := ReadBuffer<Single>(fIndex[I]);
    etDouble:   arr := ReadBuffer<Double>(fIndex[I]);
    etCmplx64: begin
      arr := ReadBuffer<TCmplx64>(fIndex[I]);
      arr := AdjustCmplxMat<TCmplx64, Single>(arr as INDArray<TCmplx64>);
    end;
    etCmplx128: begin
      arr := ReadBuffer<TCmplx128>(fIndex[I]);
      arr := AdjustCmplxMat<TCmplx128, Double>(arr as INDArray<TCmplx128>);
    end
  else
    exit(False);
  end;
  Result := TNDAUt.TryAsType<T>(arr, aArr);
  if
    Result and (aArr.NDim > 1) and
    (not (et in [etCmplx64, etCmplx128])) // complex matrices are already transposed
  then
    aArr := TNDAMan.Transpose<T>(aArr);
end;

function TMAT4Importer.TryReadTextArray(I: NativeInt; out aArr: TArray<String>): Boolean;
var chars: INDArray<AnsiChar>;
    pRow: PByte;
    J: NativeInt;
    s: AnsiString;
begin
  if not fIndexLoaded then ReadIndex;

  if not (InRange(I, 0, High(fIndex)) and (fIndex[I].MatType = mtTextMat)) then
    exit(False);

  with fIndex[I] do begin
    fStream.Seek(Position, soFromBeginning);
    case fIndex[I].ElType of
      etUInt8: begin
        chars := TNDABuffer<AnsiChar>.Create([NCols, MRows]);
        fStream.Read(chars.Data^, NCols * MRows);
      end;
    else
      exit(False);
    end;

    if (MRows > 1) and (NCols > 1) then
      chars := TNDAMan.Transpose<AnsiChar>(chars);

    SetLength(aArr, MRows);
    SetLength(s, NCols);
    pRow := chars.Data;
    for J := 0 to MRows - 1 do begin
      Move(pRow^, s[1], NCols);
      aArr[J] := String(s);
      Inc(pRow, NCols);
    end;
  end;
  Result := True;
end;

function TMAT4Importer.MatType(I: NativeInt): TMAT4Type;
begin
  if not fIndexLoaded then ReadIndex;
  Result := fIndex[I].MatType;
end;

function TMAT4Importer.MatElementType(I: NativeInt): TElType;
begin
  if not fIndexLoaded then ReadIndex;
  Result := fIndex[I].ElType;
end;

{$endregion}

end.
