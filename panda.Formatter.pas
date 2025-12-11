unit panda.Formatter;

interface

uses
    panda.Intfs
  , panda.Arrays
  , System.SysUtils
  , System.TypInfo
  , System.Math
  , System.Generics.Collections
  ;

type
  TNewLineType = (nltNone, nltDA, nltEscSeq);

  TFmtIt = class(TNDAIt)
  protected

  public
    function IsLast(I: Integer): Boolean;
  end;

  TNDAFmtBase = class abstract
  protected
    fFmt: TFormatSettings;
    fArrBegin: String;
    fArrEnd: String;
    fItemDelim: String;
  public
    procedure AfterConstruction; override;

    property BeginBracket: String read fArrBegin write fArrBegin;
    property EndBracket: String read fArrEnd write fArrEnd;
    property ItemDelimiter: String read fItemDelim write fItemDelim;
  end;

  TNDAFormatter = class(TNDAFmtBase)
  protected type
    TItemFmtProc = function (const aValue): String of object;
  protected
    fPrefix, fSuffix: String;
    fNLType: TNewLineType;
    fFormatter: TItemFmtProc;
    function Format_Int32(const aValue): String;
    function Format_Real32(const aValue): String;
    function Format_Real64(const aValue): String;
    function Format_Extended(const aValue): String;
    function GetFormatter(const aArr: INDArray): TItemFmtProc; virtual;
    function WriteArray1D(aSB: TStringBuilder; aData: PByte; aCount, aStep: NativeInt): String;
  public
    procedure AfterConstruction; override;
    function GetString(const aArr: INDArray): String;

    property Prefix: String read fPrefix write fPrefix;
    property Suffix: String read fSuffix write fSuffix;
    property NewLineType: TNewLineType read fNLType write fNLType;
  end;

  TTokenParser = class abstract
  public
    procedure AddToken(const aStr: String; aTokenID: Integer); virtual; abstract;
    function Push(aChar: Char): Integer; virtual; abstract;
    procedure Reset; virtual; abstract;
    procedure Clear; virtual; abstract;
  end;

  TTokenTreeParser = class(TTokenParser)
  protected type
    TNode = class(TObjectDictionary<Char, TNode>)
    protected
      fTokenID: Integer;
    public
      constructor Create(aTokenID: Integer);
    end;
  protected
    fRoot: TNode;
  public
    procedure AddToken(const aStr: String; aTokenID: Integer); override;
    function Push(aChar: Char): Integer; override;
    procedure Reset; override;
    procedure Clear; override;
  end;

  TCharTokenParser = class(TTokenParser)
  protected
    fTokenMap: TDictionary<Char, Integer>;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure AddToken(const aStr: String; aTokenID: Integer); override;
    function Push(aChar: Char): Integer; override;
    procedure Reset; override;
    procedure Clear; override;
  end;

  TNDAParser = class(TNDAFmtBase)
  protected const
    TID_BEGIN_BRACKET = 1;
    TID_END_BRACKET   = 2;
    TID_ITEM_DELIM    = 4;
  protected type

    TLvlData = record
      Dim: NativeInt;
      Idx: NativeInt;
    end;

    TElemParser = function (const aStr: String; out aValue): Boolean of object;

  protected
    fType: PTypeInfo;
    fItemSz: Integer;
    fElemParser: TElemParser;
    fLvls: TArray<TLvlData>;
    fLvl: Integer;
    procedure SetType(aValue: PTypeInfo);
    function Parse_Int32(const aStr: String; out aValue): Boolean;
    function Parse_UInt32(const aStr: String; out aValue): Boolean;
    function Parse_Int64(const aStr: String; out aValue): Boolean;
    function Parse_UInt64(const aStr: String; out aValue): Boolean;
    function Parse_Real32(const aStr: String; out aValue): Boolean;
    function Parse_Real64(const aStr: String; out aValue): Boolean;
    function CreateTokenParser: TTokenParser;
    function CreateArray(const aData: TBytes; const aShape: TNDAShape): INDArray;
    function ParseScalar(const aStr: String; out aArr: INDArray): Boolean;
  public
    procedure AfterConstruction; override;
    function Parse(const aStr: String; out aArr: INDArray): Boolean;

    property ElementType: PTypeInfo read fType write SetType;
  end;

implementation

{$region 'TFmtIt'}

function TFmtIt.IsLast(I: Integer): Boolean;
begin
  with fSt[I] do
    Result := (Offset = Last - Step);
end;

{$endregion}

{$region 'TNDAFmtBase'}

procedure TNDAFmtBase.AfterConstruction;
begin
  inherited;
  fItemDelim := ',';
  fArrBegin := '{';
  fArrEnd := '}';
  fFmt := TFormatSettings.Create;
  fFmt.DecimalSeparator := '.';
end;

{$endregion}

{$region 'TNDAFormatter'}

procedure TNDAFormatter.AfterConstruction;
begin
  inherited;
  fPrefix := '';
  fSuffix := '';
  fNLType := nltNone;
  fItemDelim := ', ';
end;

function TNDAFormatter.Format_Int32(const aValue): String;
begin
  Result := IntToStr(Integer(aValue));
end;

function TNDAFormatter.Format_Real32(const aValue): String;
begin
  Result := FloatToStr(Single(aValue), fFmt);
end;

function TNDAFormatter.Format_Real64(const aValue): String;
begin
  Result := FloatToStr(Double(aValue), fFmt);
end;

function TNDAFormatter.Format_Extended(const aValue): String;
begin
  Result := FloatToStr(Extended(aValue), fFmt);
end;

function TNDAFormatter.GetFormatter(const aArr: INDArray): TItemFmtProc;
var ti: PTypeInfo;
begin
  Result := nil;
  ti := aArr.GetItemType;

  case ti^.Kind of
    tkInteger: begin
      case ti^.TypeData^.OrdType of
//        otSByte: ;
//        otUByte: ;
//        otSWord: ;
//        otUWord: ;
        otSLong:  Result := Format_Int32;
//        otULong: ;
      end;
    end;

    tkFloat: begin
      case ti^.TypeData^.FloatType of
        ftSingle:   Result := Format_Real32;
        ftDouble:   Result := Format_Real64;
        ftExtended: Result := Format_Extended;
      end;
    end
  end;
end;

function TNDAFormatter.WriteArray1D(aSB: TStringBuilder; aData: PByte; aCount, aStep: NativeInt): String;
var pEnd: PByte;
begin
  aSB.Append(fArrBegin);
  pEnd := aData + (aCount - 1) * aStep;
  while aData < pEnd do begin
    aSb.Append(fFormatter(aData^)).Append(fItemDelim);
    Inc(aData, aStep);
  end;
  aSB.Append(fFormatter(aData^));
  aSB.Append(fArrEnd);
end;

function TNDAFormatter.GetString(const aArr: INDArray): String;
var I, nDim, hiLvl: Integer;
    hiSz, step: NativeInt;
    sb: TStringBuilder;
    it: TFmtIt;
begin
  fFormatter := GetFormatter(aArr);
  if not Assigned(fFormatter) then
    raise ENotImplemented.CreateFmt('No writer for %s is defined.',
      [aArr.GetItemType^.Name]
    );

  nDim := aArr.NDim;
  if nDim = 0 then begin // scalar value
    Result := fFormatter(aArr.Data^);
    exit;
  end;

  sb := TStringBuilder.Create;
  try
    hiLvl := nDim - 1;
    hiSz := aArr.Shape[hiLvl];
    step := aArr.Strides[hiLvl];
    sb.Capacity := Max(100, (Length(fItemDelim) + 1) * aArr.Size);

    if fPrefix <> '' then sb.Append(fPrefix);
    if hiLvl = 0 then  // 1D array
      WriteArray1D(sb, aArr.Data, hiSz, step)
    else begin
      it := TFmtIt.Create(aArr, hiLvl - 1);
      try
        hiSz := aArr.Shape[hiLvl];
        for I := 0 to hiLvl - 1 do
          sb.Append(fArrBegin);
        while it.MoveNext do begin
          WriteArray1D(sb, it.Current, hiSz, step);
          for I := hiLvl - 1 downto 0 do begin
            if it.IsLast(I) then
              sb.Append(fArrEnd)
            else begin
              sb.Append(fItemDelim);
              break;
            end;
          end;
        end;
      finally
        it.Free;
      end;
    end;
    if fSuffix <> '' then sb.Append(fSuffix);
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

{$endregion}

{$region 'TTokenTreeParser'}

{$region 'TTokenTreeParser.TNode'}

constructor TTokenTreeParser.TNode.Create(aTokenID: Integer);
begin

end;

{$endregion}

procedure TTokenTreeParser.AddToken(const aStr: String; aTokenID: Integer);
begin

end;

function TTokenTreeParser.Push(aChar: Char): Integer;
begin

end;

procedure TTokenTreeParser.Reset;
begin

end;

procedure TTokenTreeParser.Clear;
begin

end;

{$endregion}

{$region 'TCharTokenParser'}

procedure TCharTokenParser.AfterConstruction;
begin
  inherited;
  fTokenMap := TDictionary<Char, Integer>.Create;
end;

procedure TCharTokenParser.BeforeDestruction;
begin
  inherited;
  fTokenMap.Free;
end;

procedure TCharTokenParser.AddToken(const aStr: String; aTokenID: Integer);
begin
  Assert(Length(aStr) = 1);
  fTokenMap.Add(aStr[1], aTokenID);
end;

function TCharTokenParser.Push(aChar: Char): Integer;
begin
  if not fTokenMap.TryGetValue(aChar, Result) then
    Result := -1;
end;

procedure TCharTokenParser.Reset;
begin

end;

procedure TCharTokenParser.Clear;
begin
  fTokenMap.Clear;
end;


{$endregion}

{$region 'TNDAParser'}

procedure TNDAParser.AfterConstruction;
begin
  inherited;
  ElementType := TypeInfo(Double);
end;

function TNDAParser.Parse_Int32(const aStr: String; out aValue): Boolean;
begin
  Result := TryStrToInt(aStr, Integer(aValue));
end;

function TNDAParser.Parse_UInt32(const aStr: String; out aValue): Boolean;
begin
  Result := TryStrToUInt(aStr, Cardinal(aValue));
end;

function TNDAParser.Parse_Int64(const aStr: String; out aValue): Boolean;
begin
  Result := TryStrToInt64(aStr, Int64(aValue));
end;

function TNDAParser.Parse_UInt64(const aStr: String; out aValue): Boolean;
begin
  Result := TryStrToUInt64(aStr, UInt64(aValue));
end;

function TNDAParser.Parse_Real32(const aStr: String; out aValue): Boolean;
begin
  Result := TryStrToFloat(aStr, Single(aValue), fFmt);
end;

function TNDAParser.Parse_Real64(const aStr: String; out aValue): Boolean;
begin
  Result := TryStrToFloat(aStr, Double(aValue), fFmt);
end;

function TNDAParser.Parse(const aStr: String; out aArr: INDArray): Boolean;
var I, pos, buffPos: NativeInt;
    tp: TTokenParser;
    t: Integer;
    bEndBr: Boolean;
    shape: TNDAShape;
    buff: TBytes;
    s: String;
begin
  if not aStr.StartsWith(fArrBegin) then begin
    Result := ParseScalar(aStr, aArr);
    exit;
  end;

  tp := CreateTokenParser();
  try
    fLvls := nil;
    fLvl := -1;
    SetLength(buff, 64 * fItemSz);
    buffPos := 0;
    pos := 1;
    bEndBr := False;
    for I := 1 to Length(aStr) do begin
      t := tp.Push(aStr[I]);
      case t of
        TID_BEGIN_BRACKET: begin
          Inc(fLvl);
          if fLvl > High(fLvls) then begin
            SetLength(fLvls, fLvl + 1);
            with fLvls[fLvl] do begin
              Dim := -1;
              Idx := -1;
            end;
          end;
          pos := I + 1;
        end;

        TID_ITEM_DELIM, TID_END_BRACKET: begin
          if not bEndBr then begin
            s := aStr.Substring(pos - 1, I - pos);
            if fElemParser(s, buff[buffPos]) then begin
              Inc(buffPos, fItemSz);
              if buffPos >= Length(buff) then
                SetLength(buff, 2 * Length(buff));
            end else begin
              // error
              exit(False);
            end;
          end;

          with fLvls[fLvl] do begin
            Inc(Idx);
            if (Dim > 0) and (Idx >= Dim)  then begin
              // error
              exit(False);
            end;
          end;

          bEndBr := (t = TID_END_BRACKET);
          if bEndBr then begin
            with fLvls[fLvl] do begin
              if Dim < 0 then
                Dim := Idx + 1;
              Idx := -1;
            end;
            Dec(fLvl);
          end;
          pos := I + 1;
        end;
      else
        continue;
      end;
      tp.Reset;
    end;

    SetLength(shape, Length(fLvls));
    for I := 0 to High(fLvls) do
      shape[I] := fLvls[I].Dim;
    aArr := CreateArray(buff, shape);
    Result := True;
  finally
    tp.Free;
  end;
end;

function TNDAParser.CreateTokenParser: TTokenParser;
begin
  if
    (Length(fArrBegin) = 1) and (Length(fArrEnd) = 1) and
    (Length(fItemDelim) = 1)
  then
    Result := TCharTokenParser.Create
  else
    Result := TTokenTreeParser.Create;

  Result.AddToken(fArrBegin,  TID_BEGIN_BRACKET);
  Result.AddToken(fArrEnd,    TID_END_BRACKET);
  Result.AddToken(fItemDelim, TID_ITEM_DELIM);
end;

function TNDAParser.CreateArray(const aData: TBytes; const aShape: TNDAShape): INDArray;
var td: PTypeData;
begin
  td := GetTypeData(fType);
  case fType^.Kind of
    tkInteger:
      case td^.OrdType of
        otSLong: Result := TNDABuffer<Integer>.Create(aShape);
        otULong: Result := TNDABuffer<Cardinal>.Create(aShape);
      else
        exit;
      end;

    tkFloat:
      case td^.FloatType of
        ftSingle: Result := TNDABuffer<Single>.Create(aShape);
        ftDouble: Result := TNDABuffer<Double>.Create(aShape);
      else
        exit;
      end;

    tkInt64:
      if td^.MinInt64Value = 0 then
        Result := TNDABuffer<UInt64>.Create(aShape)
      else
        Result := TNDABuffer<Int64>.Create(aShape);
  else
    exit;
  end;

  Move(aData[0], Result.Data^, GetSize(aShape) * fItemSz);
end;

function TNDAParser.ParseScalar(const aStr: String; out aArr: INDArray): Boolean;
var td: PTypeData;
begin
  td := GetTypeData(fType);
  case fType^.Kind of
    tkInteger:
      case td^.OrdType of
        otSLong: aArr := TNDScalar<Integer>.Create(0);
        otULong: aArr := TNDScalar<Cardinal>.Create(0);
      else
        exit(False);
      end;

    tkFloat:
      case td^.FloatType of
        ftSingle: aArr := TNDScalar<Single>.Create(0);
        ftDouble: aArr := TNDScalar<Double>.Create(0);
      else
        exit(False);
      end;

    tkInt64:
      if td^.MinInt64Value = 0 then
        aArr := TNDScalar<UInt64>.Create(0)
      else
        aArr := TNDScalar<Int64>.Create(0);
  else
    exit(False);
  end;

  Result := fElemParser(aStr, aArr.Data^);
end;

procedure TNDAParser.SetType(aValue: PTypeInfo);
var td: PTypeData;
begin
  if aValue = fType then exit;

  td := GetTypeData(aValue);
  case aValue^.Kind of
    tkInteger:
      case td^.OrdType of
        otSLong: fElemParser := Parse_Int32;
        otULong: fElemParser := Parse_UInt32;
      else
        exit;
      end;

    tkFloat:
      case td^.FloatType of
        ftSingle: fElemParser := Parse_Real32;
        ftDouble: fElemParser := Parse_Real64;
      else
        exit;
      end;

    tkInt64:
      if td^.MinInt64Value = 0 then
        fElemParser := Parse_UInt64
      else
        fElemParser := Parse_Int64;
  end;

  fItemSz := GetItemSize(aValue);
  fType := aValue;
end;

{$endregion}

end.
