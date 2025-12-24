unit panda.ArrManip;

interface

uses
    panda.Intfs
  , panda.Arrays
  ;

type
  TNDAMan = class(TNDAUt)
  public const
    UNSUPPORTED_TYPE      = -1;
    UNSUPPORTED_TYPE_CVT  = -2;
    INV_CAST              = -3;
  protected
    class function FlippedAxesIdx(aDim: Integer; const aAxes: array of Integer): INDIndexSeq; static;
  public
    class function GetPart(const aArr: INDArray; const aIdx: INDIndexSeq): INDArray; static;
    class function SetPart(const aArr: INDArray; const aIdx: INDIndexSeq; const aValue: INDArray): Integer; static;

    /// <summary>
    ///  <c>Flatten<T>(aArr)</c> returns a copy of the array collapsed into one dimesion.
    /// </summary>
    class function Flatten<T>(const aArr: INDArray<T>): INDArray<T>; static;
    class function Transpose<T>(const aArr: INDArray<T>): INDArray<T>; overload; static;
    class function Transpose<T>(const aArr: INDArray<T>; const aAxes: TArray<Integer>): INDArray<T>; overload; static;
    /// <summary>
    ///   <c>Flip<T>(aArr, axis)</c> reverses the order of elements in an array along the given axis.
    /// </summary>
    class function Flip<T>(const aArr: INDArray<T>; aAxis: Integer = 0): INDArray<T>; overload; static;
    class function Flip<T>(const aArr: INDArray<T>; aAxes: array of Integer): INDArray<T>; overload; static;
    class function FlipAll<T>(const aArr: INDArray<T>): INDArray<T>; static;
  end;

implementation

uses
    panda.DynArrayUtils
  , System.TypInfo
  ;

{$region 'TNDAMan'}

class function TNDAMan.GetPart(const aArr: INDArray; const aIdx: INDIndexSeq): INDArray;
begin
  with aArr.GetItemType^ do begin
    case Kind of
      tkInteger:
        case TypeData^.OrdType of
          otSByte: Result := TNDArray<Int8>(aArr)[aIdx];
          otUByte: Result := TNDArray<UInt8>(aArr)[aIdx];
          otSWord: Result := TNDArray<Int16>(aArr)[aIdx];
          otUWord: Result := TNDArray<UInt16>(aArr)[aIdx];
          otSLong: Result := TNDArray<Int32>(aArr)[aIdx];
          otULong: Result := TNDArray<UInt32>(aArr)[aIdx];
        else
          exit(nil);
        end;

      tkFloat:
        case TypeData^.FloatType of
          ftSingle:   Result := TNDArray<Single>(aArr)[aIdx];
          ftDouble:   Result := TNDArray<Double>(aArr)[aIdx];
        else
          exit(nil);
        end;

      tkInt64:
        if TypeData^.MinInt64Value = 0 then
          Result := TNDArray<UInt64>(aArr)[aIdx]
        else
          Result := TNDArray<Int64>(aArr)[aIdx];
    else
      exit(nil);
    end;
  end;
end;

class function TNDAMan.SetPart(const aArr: INDArray; const aIdx: INDIndexSeq; const aValue: INDArray): Integer;
var cvtFnc: TIPProcVV;
    val: INDArray;
begin
  if SameQ(aArr.GetItemType, aValue.GetItemType) then
    val := aValue
  else if TryGetCvtFunc(aValue.GetItemType, aArr.GetItemType, cvtFnc) then begin
    val := Empty(aArr.GetItemType, aValue.Shape);
    MapSS(aValue, val, cvtFnc);
  end else
    exit(INV_CAST);

  with aArr.GetItemType^ do begin
    case Kind of
      tkInteger:
        case TypeData^.OrdType of
          otSByte, otUByte: (aArr as INDArray<Byte>)[aIdx] := (val as INDArray<Byte>);
          otSWord, otUWord: (aArr as INDArray<Word>)[aIdx] := (val as INDArray<Word>);
          otSLong, otULong: (aArr as INDArray<Integer>)[aIdx] := (val as INDArray<Integer>);
        end;

      tkFloat:
        case TypeData^.FloatType of
          ftSingle: (aArr as INDArray<Single>)[aIdx] := (val as INDArray<Single>);
          ftDouble: (aArr as INDArray<Double>)[aIdx] := (val as INDArray<Double>);
        end;

      tkInt64:
        (aArr as INDArray<Int64>)[aIdx] := (val as INDArray<Int64>);
    else
      exit(UNSUPPORTED_TYPE);
    end;
  end;

  Result := 0;
end;

class function TNDAMan.Flatten<T>(const aArr: INDArray<T>): INDArray<T>;
begin
  Result := TNDABuffer<T>.Create([aArr.Size]);
  Copy<T>(aArr, Result.Data);
end;

class function TNDAMan.Transpose<T>(const aArr: INDArray<T>): INDArray<T>;
begin
  Result := Transpose<T>(aArr, TDynAUt.Range_I32(aArr.NDim - 1, 0, -1));
end;

class function TNDAMan.Transpose<T>(const aArr: INDArray<T>; const aAxes: TArray<Integer>): INDArray<T>;
var shape: TArray<NativeInt>;
    it: TNDAIt;
    p: TNDA<T>.PT;
begin
  shape := Permute<NativeInt>(aArr.Shape, aAxes);
  Result := TNDABuffer<T>.Create(shape);
  p := TNDA<T>.PT(Result.Data);
  it := TNDAIt.Create(aArr, 0, -1, aAxes);
  try
    while it.MoveNext do begin
      p^ := TNDA<T>.PT(it.Current)^;
      Inc(p);
    end;
  finally
    it.Free;
  end;
end;

class function TNDAMan.FlippedAxesIdx(aDim: Integer; const aAxes: array of Integer): INDIndexSeq;
var s: TArray<Integer>;
    I: Integer;
begin
  SetLength(Result, aDim);
  for I := 0 to High(Result) do
    Result[I] := TNDSpanIdx.Create(0, -1, 1);
  for I := 0 to High(aAxes) do
    with TNDSpanIdx(Result[aAxes[I]]) do begin
      Low := -1;
      High := 0;
      Step := -1;
    end;
end;

class function TNDAMan.Flip<T>(const aArr: INDArray<T>; aAxis: Integer = 0): INDArray<T>;
begin
  Result := Flip<T>(aArr, [aAxis]);
end;

class function TNDAMan.Flip<T>(const aArr: INDArray<T>; aAxes: array of Integer): INDArray<T>;
var idx: INDIndexSeq;
begin
  idx := FlippedAxesIdx(aArr.NDim, aAxes);
  Result := TNDArrayView<T>.Create(aArr, idx);
end;

class function TNDAMan.FlipAll<T>(const aArr: INDArray<T>): INDArray<T>;
begin
  Result := Flip<T>(aArr, TDynAUt.Range_I32(0, aArr.NDim - 1));
end;

{$endregion}

end.
