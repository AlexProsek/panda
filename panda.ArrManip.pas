unit panda.ArrManip;

interface

uses
    panda.Intfs
  , panda.Arrays
  , System.TypInfo
  ;

type
  TNDAMan = class(TNDAUt)
  public const
    UNSUPPORTED_TYPE      = -1;
    UNSUPPORTED_TYPE_CVT  = -2;
    INV_CAST              = -3;
  protected
    class function FlippedAxesIdx(aDim: Integer; const aAxes: array of Integer): INDIndexSeq; static;
    class procedure _MatCpy<T>(const aSrc, aDst: TMatSpec); static;
    class procedure _Tr<T>(const aSrc, aDst: INDArray<T>; const aAxes: TArray<Integer>); static;
  public
    class function GetPart(const aArr: INDArray; const aIdx: INDIndexSeq): INDArray; static;
    class function SetPart(const aArr: INDArray; const aIdx: INDIndexSeq; const aValue: INDArray): Integer; static;

    /// <summary>
    ///  <c>Flatten<T>(aArr)</c> returns a copy of the array collapsed into one dimesion.
    /// </summary>
    class function Flatten<T>(const aArr: INDArray<T>): INDArray<T>; static;
    class function Transpose<T>(const aArr: INDArray<T>): INDArray<T>; overload; static;
    class function Transpose<T>(const aArr: INDArray<T>; const aAxes: TArray<Integer>): INDArray<T>; overload; static;
    class procedure Transpose<T>(const aSrc, aDst: INDArray<T>; const aAxes: TArray<Integer>); overload; static;
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
  ;

{$region 'TNDAMan'}

class function TNDAMan.GetPart(const aArr: INDArray; const aIdx: INDIndexSeq): INDArray;
begin
  with aArr.GetItemType^ do begin
    case Kind of
      tkInteger:
        case TypeData^.OrdType of
          otSByte: Result := (aArr as INDArray<Int8>)[aIdx];
          otUByte: Result := (aArr as INDArray<UInt8>)[aIdx];
          otSWord: Result := (aArr as INDArray<Int16>)[aIdx];
          otUWord: Result := (aArr as INDArray<UInt16>)[aIdx];
          otSLong: Result := (aArr as INDArray<Int32>)[aIdx];
          otULong: Result := (aArr as INDArray<UInt32>)[aIdx];
        else
          exit(nil);
        end;

      tkFloat:
        case TypeData^.FloatType of
          ftSingle:   Result := (aArr as INDArray<Single>)[aIdx];
          ftDouble:   Result := (aArr as INDArray<Double>)[aIdx];
        else
          exit(nil);
        end;

      tkInt64:
        if TypeData^.MinInt64Value = 0 then
          Result := (aArr as INDArray<UInt64>)[aIdx]
        else
          Result := (aArr as INDArray<Int64>)[aIdx];
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

class procedure TNDAMan._MatCpy<T>(const aSrc, aDst: TMatSpec);
var pSrc, pDst, pSrcRow, pDstRow: PByte;
    pEnd, pRowEnd: PByte;
    rCnt, cCnt: NativeInt;
    srcRStep, srcCStep, dstRStep, dstCStep, srcRWidth: NativeInt;
begin
  rCnt := aSrc.NRows;
  cCnt := aSrc.NCols;
  srcRStep := aSrc.RStep;
  srcCStep := aSrc.CStep;
  dstRStep := aDst.RStep;
  dstCStep := aDst.CStep;
  pSrcRow := aSrc.Data;
  pDstRow := aDst.Data;
  srcRWidth := cCnt * srcCStep;
  pEnd := pSrcRow + rCnt * srcRStep;
  while pSrcRow <> pEnd do begin
    pSrc := pSrcRow;
    pDst := pDstRow;
    pRowEnd := pSrc + srcRWidth;
    while pSrc <> pRowEnd do begin
      TNDA<T>.PT(pDst)^ := TNDA<T>.PT(pSrc)^;
      Inc(pSrc, srcCStep);
      Inc(pDst, dstCStep);
    end;
    Inc(pSrcRow, srcRStep);
    Inc(pDstRow, dstRStep);
  end;
end;

class procedure TNDAMan._Tr<T>(const aSrc, aDst: INDArray<T>; const aAxes: TArray<Integer>);
var itSrc, itDst: TNDASliceIt;
    srcMat, dstMat: TMatSpec;
    nDim: Integer;
begin
  nDim := aSrc.NDim;
  if nDim < 2 then begin
    TNDAUt.Fill<T>(aDst, aSrc);
    exit;
  end;

  if nDim = 2 then begin
    GetMatSpec(aSrc, srcMat);
    GetMatSpec(aDst, dstMat);
    SwapMatAxes(dstMat);
    _MatCpy<T>(srcMat, dstMat);
    exit;
  end;

  itSrc := TNDASliceIt.Create(aSrc, 0, -3, aAxes);
  itDst := TNDASliceIt.Create(aDst, 0, -3);
  try
    GetMatSpec(itSrc.CurrentSlice, srcMat);
    GetMatSpec(itDst.CurrentSlice, dstMat);
    while itSrc.MoveNext and itDst.MoveNext do begin
      srcMat.Data := itSrc.Current;
      dstMat.Data := itDst.Current;
      _MatCpy<T>(srcMat, dstMat);
    end;
  finally
    itSrc.Free;
    itDst.Free;
  end;
end;

class function TNDAMan.Transpose<T>(const aArr: INDArray<T>): INDArray<T>;
begin
  Result := Transpose<T>(aArr, TDynAUt.Range_I32(aArr.NDim - 1, 0, -1));
end;

class function TNDAMan.Transpose<T>(const aArr: INDArray<T>; const aAxes: TArray<Integer>): INDArray<T>;
begin
  Result := TNDABuffer<T>.Create(Permute<NativeInt>(aArr.Shape, aAxes));
  _Tr<T>(aArr, Result, aAxes);
end;

class procedure TNDAMan.Transpose<T>(const aSrc, aDst: INDArray<T>; const aAxes: TArray<Integer>);
var shape: TArray<NativeInt>;
const sErrMsg = 'Transpose error: output array shape doesn''t match.';
begin
  if aSrc.NDim <> aDst.NDim then
    raise ENDAShapeError.Create(sErrMsg);
  shape := Permute<NativeInt>(aSrc.Shape, aAxes);
  if not SameQ(shape, aDst.Shape) then
    raise ENDAShapeError.Create(sErrMsg);

  _Tr<T>(aSrc, aDst, aAxes);
end;

class function TNDAMan.FlippedAxesIdx(aDim: Integer; const aAxes: array of Integer): INDIndexSeq;
var I: Integer;
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
