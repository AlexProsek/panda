unit panda.ArrManip;

interface

uses
    panda.Intfs
  , panda.Arrays
  , panda.Nums
  , System.TypInfo
  ;

{$I AsmDefs.inc}

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

{$region 'low-level functions'}

// Returns True if a matrix has continuous rows
function CRowsQ(const aMat: TMatSpec): Boolean; inline;
// Returns True if a matrix has continuous columns
function CColsQ(const aMat: TMatSpec): Boolean; inline;

procedure Tr4x4_4B(pSrc, pDst: PByte; aSrcStep, aDstStep: NativeInt);
procedure TrRxC_4B(pSrc, pDst: PByte; aSrcStep, aDstStep: NativeInt; R, C: NativeInt);
procedure CTr_4B(pSrc, pDst: PByte; aSrcRCnt, aSrcCCnt, aSrcRStep, aDstRStep: NativeInt);

{$endregion}

implementation

uses
    panda.DynArrayUtils
  ;

{$region 'low-level functions'}

function CRowsQ(const aMat: TMatSpec): Boolean;
begin
  Result := (aMat.ElSize = aMat.CStep);
end;

function CColsQ(const aMat: TMatSpec): Boolean;
begin
  Result := (aMat.ElSize = aMat.RStep);
end;

procedure Tr4x4_4B(pSrc, pDst: PByte; aSrcStep, aDstStep: NativeInt);
{$if defined(ASMx64)}
// RCX <- pSrc, RDX <- pDst, R8 <- aSrcStep, R9 <- aDstStep
asm
  movupd xmm0, [rcx]
  add rcx, r8
  movupd xmm1, [rcx]
  add rcx, r8
  movupd xmm2, [rcx]
  add rcx, r8
  movupd xmm3, [rcx]

  movaps  xmm4, xmm0
  unpcklps xmm4, xmm1        // xmm4 = a0 b0 a1 b1

  movaps  xmm5, xmm0
  unpckhps xmm5, xmm1        // xmm5 = a2 b2 a3 b3

  movaps  xmm0, xmm2
  unpcklps xmm0, xmm3        // xmm0 = c0 d0 c1 d1

  movaps  xmm1, xmm2
  unpckhps xmm1, xmm3        // xmm1 = c2 d2 c3 d3

  movaps  xmm2, xmm4
  shufps  xmm2, xmm0, $44    // col0 = a0 b0 c0 d0

  movaps  xmm3, xmm4
  shufps  xmm3, xmm0, $EE    // col1 = a1 b1 c1 d1

  movaps  xmm4, xmm5
  shufps  xmm4, xmm1, $44    // col2 = a2 b2 c2 d2

  movaps  xmm5, xmm5
  shufps  xmm5, xmm1, $EE    // col3 = a3 b3 c3 d3

  // xmm2, xmm3, xmm4, xmm5 = transposed rows
  movupd [rdx], xmm2
  add rdx, r9
  movupd [rdx], xmm3
  add rdx, r9
  movupd [rdx], xmm4
  add rdx, r9
  movupd [rdx], xmm5
end;
{$else}
begin
  PSingle(pDst)^              := PSingle(pSrc)^;
  PSingle(pDst + aDstStep)^   := PSingle(pSrc + cF32Sz)^;
  PSingle(pDst + 2*aDstStep)^ := PSingle(pSrc + 2*cF32Sz)^;
  pSingle(pDst + 3*aDstStep)^ := PSingle(pSrc + 3*cF32Sz)^;

  Inc(pSrc, aSrcStep);
  Inc(pDst, cF32Sz);

  PSingle(pDst)^              := PSingle(pSrc)^;
  PSingle(pDst + aDstStep)^   := PSingle(pSrc + cF32Sz)^;
  PSingle(pDst + 2*aDstStep)^ := PSingle(pSrc + 2*cF32Sz)^;
  pSingle(pDst + 3*aDstStep)^ := PSingle(pSrc + 3*cF32Sz)^;

  Inc(pSrc, aSrcStep);
  Inc(pDst, cF32Sz);

  PSingle(pDst)^              := PSingle(pSrc)^;
  PSingle(pDst + aDstStep)^   := PSingle(pSrc + cF32Sz)^;
  PSingle(pDst + 2*aDstStep)^ := PSingle(pSrc + 2*cF32Sz)^;
  pSingle(pDst + 3*aDstStep)^ := PSingle(pSrc + 3*cF32Sz)^;

  Inc(pSrc, aSrcStep);
  Inc(pDst, cF32Sz);

  PSingle(pDst)^              := PSingle(pSrc)^;
  PSingle(pDst + aDstStep)^   := PSingle(pSrc + cF32Sz)^;
  PSingle(pDst + 2*aDstStep)^ := PSingle(pSrc + 2*cF32Sz)^;
  pSingle(pDst + 3*aDstStep)^ := PSingle(pSrc + 3*cF32Sz)^;
end;
{$endif}

procedure TrRxC_4B(pSrc, pDst: PByte; aSrcStep, aDstStep: NativeInt; R, C: NativeInt);
var I: NativeInt;
    pEnd: PByte;
begin
  pEnd := pSrc + R * aSrcStep;
  while pSrc < pEnd do begin
    for I := 0 to C - 1 do
      PSingle(pDst + I*aDstStep)^ := PSingle(pSrc + I*cF32Sz)^;
    Inc(pSrc, aSrcStep);
    Inc(pDst, cF32Sz);
  end;
end;

procedure CTr_4B(pSrc, pDst: PByte; aSrcRCnt, aSrcCCnt, aSrcRStep, aDstRStep: NativeInt);
var pEnd, pSrcBlock, pDstBlock, pRowEnd: PByte;
    srcRWb: NativeInt;
    RRest, CRest: NativeInt;
const cBlockSz = 4;
begin
  srcRWb := ((aSrcCCnt shr 2) shl 2) * cF32Sz;
  CRest := aSrcCCnt and (cBlockSz - 1);
  RRest := aSrcRcnt and (cBlockSz - 1);
  pEnd := pSrc + ((aSrcRCnt shr 2) shl 2) * aSrcRStep;
  while pSrc < pEnd do begin
    pRowEnd := pSrc + srcRWb;
    pSrcBlock := pSrc;
    pDstBlock := pDst;
    while pSrcBlock < pRowEnd do begin
      Tr4x4_4B(pSrcBlock, pDstBlock, aSrcRStep, aDstRStep);
      Inc(pDstBlock, 4 * aDstRStep);
      Inc(pSrcBlock, 16);
    end;
    // remaining right block transposition
    if CRest > 0 then
      TrRxC_4B(pSrcBlock, pDstBlock, aSrcRStep, aDstRStep, 4, CRest);
    Inc(pSrc, 4 * aSrcRStep);
    Inc(pDst, 16);
  end;
  // remaining bottom blocks transposition
  if RRest > 0 then begin
    pRowEnd := pSrc + srcRWb;
    while pSrc < pRowEnd do begin
      TrRxC_4B(pSrc, pDst, aSrcRStep, aDstRStep, RRest, 4);
      Inc(pDst, 4 * aDstRStep);
      Inc(pSrc, 16);
    end;
    // right-bottom corner transposition
    TrRxC_4B(pSrc, pDst, aSrcRStep, aDstRStep, RRest, CRest);
  end;
end;

{$endregion}

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
    if CRowsQ(srcMat) and CRowsQ(dstMat) then begin
      case srcMat.ElSize of
        cF32Sz: CTr_4B(srcMat.data, dstMat.Data, srcMat.NRows, srcMat.NCols,
          srcMat.RStep, dstMat.RStep
        );
      else
        SwapMatAxes(dstMat);
        _MatCpy<T>(srcMat, dstMat);
      end;
    end else begin
      SwapMatAxes(dstMat);
      _MatCpy<T>(srcMat, dstMat);
    end;
    exit;
  end;

  itSrc := TNDASliceIt.Create(aSrc, 0, -3, aAxes);
  itDst := TNDASliceIt.Create(aDst, 0, -3);
  try
    GetMatSpec(itSrc.CurrentSlice, srcMat);
    GetMatSpec(itDst.CurrentSlice, dstMat);
    if CColsQ(srcMat) and CRowsQ(dstMat) then begin
      case srcMat.ElSize of
        cF32Sz: begin
          while itSrc.MoveNext and itDst.MoveNext do begin
            CTr_4B(itSrc.Current, itDst.Current,
              // swap source axes back because they are swapped by iterator
              srcMat.NCols, srcMat.NRows,
              srcMat.CStep, dstMat.RStep
            );
          end;
        end;
      else
        while itSrc.MoveNext and itDst.MoveNext do begin
          srcMat.Data := itSrc.Current;
          dstMat.Data := itDst.Current;
          _MatCpy<T>(srcMat, dstMat);
        end;
      end;
    end else begin
      while itSrc.MoveNext and itDst.MoveNext do begin
        srcMat.Data := itSrc.Current;
        dstMat.Data := itDst.Current;
        _MatCpy<T>(srcMat, dstMat);
      end;
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
