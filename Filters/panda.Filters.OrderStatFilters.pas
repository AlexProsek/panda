unit panda.Filters.OrderStatFilters;

interface

uses
    panda.Intfs
  , panda.Nums
  , panda.Arrays
  , panda.ArrManip
  , panda.Sorting
  , System.Math
  , System.Threading
  , System.SysUtils
  ;

type
  TFilter1D = class abstract
  protected
    fRadius: Integer;
    procedure SetRadius(aValue: Integer); virtual;
  public
    procedure AfterConstruction; override;
    procedure Execute(aSrc, aDst: PByte; aCount: NativeInt); virtual; abstract;

    property Radius: Integer read fRadius write SetRadius;
  end;

  TFilter2D = class abstract
  protected
    fHRadius, fVRadius: Integer;
    procedure SetHRadius(aValue: Integer); virtual;
    procedure SetVRadius(aValue: Integer); virtual;
  public
    procedure AfterConstruction; override;
    procedure Execute(pSrc, pDst: PByte; aSrcWStep, aDstWStep, aW, aH: NativeInt); virtual; abstract;

    property HRadius: Integer read fHRadius write SetHRadius;
    property VRadius: Integer read fVRadius write SetVRadius;
  end;

  TMFilter1D = class abstract(TFilter1D)
  protected
    fUseHGW: Boolean;
    // MGetV shoud do elementwise min/max
    procedure MGetV(aA, aB, aRes: PByte; aN: NativeInt); virtual; abstract;
  public
    procedure AfterConstruction; override;

    // Use Van Herk/Gil Werman algorithm?
    property UseHGWMethod: Boolean read fUseHGW write fUseHGW;
  end;

  // common class for Min/Max 1D filters
  TMFilter1D<T> = class abstract(TMFilter1D)
  protected const
    cElSz = SizeOf(T);
  protected type
    TArrayOfT = array [0..0] of T;
    PArrayOfT = ^TArrayOfT;
    PT = ^T;
  protected
    // MGetN should return a min/max of N samples
    function MGetN(aSrc: PByte; aN: NativeInt): T; virtual; abstract;
    // MGet2 should return min/max of pair (aA, aB).
    function MGet2(const aA, aB: T): T; virtual; abstract;
    procedure MGetV(aA, aB, aRes: PByte; aN: NativeInt); override;

    // Van Herk/Gil Werman algorithm for min/max filter
    procedure HGWFilter(pA, pB: PByte; aN: Integer);
    procedure HGWBlockEval(aA, aBuff: PByte; aN: NativeInt); virtual;
  public
    procedure Execute(aSrc, aDst: PByte; aN: NativeInt); overload; override;
  end;

  TMFilter1DUI8 = class(TMFilter1D<Byte>)
  public
    procedure AfterConstruction; override;
  end;

  TMinFilter1DUI8 = class(TMFilter1DUI8)
  protected
    function MGetN(aSrc: PByte; aN: NativeInt): Byte; override;
    function MGet2(const aA, aB: Byte): Byte; override;
    procedure MGetV(aA, aB, aRes: PByte; aN: NativeInt); override;
  end;

  TMinFilter1DF64 = class(TMFilter1D<Double>)
  protected
    function MGetN(aSrc: PByte; aN: NativeInt): Double; override;
    function MGet2(const aA, aB: Double): Double; override;
    procedure MGetV(aA, aB, aRes: PByte; aN: NativeInt); override;
  end;

  TMaxFilter1DUI8 = class(TMFilter1DUI8)
    function MGetN(aSrc: PByte; aN: NativeInt): Byte; override;
    function MGet2(const aA, aB: Byte): Byte; override;
    procedure MGetV(aA, aB, aRes: PByte; aN: NativeInt); override;
  end;

  TMaxFilter1DF64 = class(TMFilter1D<Double>)
  protected
    function MGetN(aSrc: PByte; aN: NativeInt): Double; override;
    function MGet2(const aA, aB: Double): Double; override;
    procedure MGetV(aA, aB, aRes: PByte; aN: NativeInt); override;
  end;

  // base class for general Min/Max filters
  TGMFilter2D = class abstract
  protected
    fParallelize: Boolean;
    procedure SetParallelize(aValue: Boolean); virtual;
  public
    procedure Execute(pSrc, pDst: PByte; aSrcWStep, aDstWStep, aW, aH: NativeInt); virtual; abstract;

    property Parallelize: Boolean read fParallelize write SetParallelize;
  end;

  TBoxMFilter2D = class abstract(TGMFilter2D)
  protected
    fFilter1D: TMFilter1D;
    fHRadius, fVRadius: Integer;
    function GetUseHGW: Boolean;
    procedure SetUseHGW(aValue: Boolean);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property HRadius: Integer read fHRadius write fHRadius;
    property VRadius: Integer read fVRadius write fVRadius;
    // Use Van Herk/Gil Werman algorithm?
    property UseHGWMethod: Boolean read GetUseHGW write SetUseHGW;
  end;

  TBoxMFilter2D<T> = class abstract(TBoxMFilter2D)
  protected const
    cElSz = SizeOf(T);
  protected
    fBuff: INDArray<T>;
    procedure AdjustBuffers(aW, aH: NativeInt);
    procedure _ExecNaive(pSrc, pDst: PByte; aSrcWStep, aDstWStep, aW, aH: NativeInt);
    procedure _ExecHGW(pSrc, pDst: PByte; aSrcWStep, aDstWStep, aW, aH: NativeInt);
  public
    procedure Execute(pSrc, pDst: PByte; aSrcWStep, aDstWStep, aW, aH: NativeInt); override;
  end;

  TBoxMinFilter2DUI8 = class(TBoxMFilter2D<Byte>)
  public
    procedure AfterConstruction; override;
  end;

  TBoxMaxFilter2DUI8 = class(TBoxMFilter2D<Byte>)
  public
    procedure AfterConstruction; override;
  end;

  TMFilter2D = class abstract(TGMFilter2D)
  protected
    fKernel: INDArray<Byte>;
    fKerW, fKerH: NativeInt;
    fKerIdxs: TArray<TMatIdx>;
    fEvalMargins: Boolean;
    function GetElemSize: Integer; virtual; abstract;
    // it should do elementwise aRes <- M(aA, aB)
    procedure MGetV(aA, aB, aRes: PByte; aN: NativeInt); virtual; abstract;
    procedure _Exec(pSrc, pDst: PByte; aSrcWStep, aDstWStep, aW, aH: NativeInt); virtual;
    procedure _ExecMargins(pSrc, pDst: PByte; aSrcWStep, aDstWStep, aW, aH: NativeInt); virtual; abstract;
    procedure SetKernel(const aValue: INDArray<Byte>); virtual;
  public
    procedure AfterConstruction; override;
    procedure Assign(aSrc: TMFilter2D); virtual;
    procedure Execute(pSrc, pDst: PByte; aSrcWStep, aDstWStep, aW, aH: NativeInt); override;

    property Kernel: INDArray<Byte> read fKernel write SetKernel;
  end;
  TMFilter2DClass = class of TMFilter2D;

  TMFilter2D<T> = class abstract(TMFilter2D)
  protected const
    cElSz = SizeOf(T);
  protected type
    PT = ^T;
  protected
    // initial value for intermidiate buffer, it should return MinValueOf(T) for Max filter
    // and MaxValueOf(T) for Min filter
    function InitValue: T; virtual; abstract;
    function GetElemSize: Integer; override;
    procedure _ExecMargins(pSrc, pDst: PByte; aSrcWStep, aDstWStep, aW, aH: NativeInt); override;
  end;

  TMinFilter2DUI8 = class(TMFilter2D<Byte>)
  protected
    procedure MGetV(aA, aB, aRes: PByte; aN: NativeInt); override;
    function InitValue: Byte; override;
  end;

  TMaxFilter2DUI8 = class(TMFilter2D<Byte>)
  protected
    procedure MGetV(aA, aB, aRes: PByte; aN: NativeInt); override;
    function InitValue: Byte; override;
  end;

  // base class for filters with Diamond or Circle kernel
  TDCMFilter2D = class abstract(TGMFilter2D)
  public type
    TKernelType = (ktRect, ktDiamond, ktCircle);
  protected
    fRadius: Integer;
    fKerType: TKernelType;
    fBoxFilter: TBoxMFilter2D;
    fRestFilter: TMFilter2D;
    fBuffB, fBuffR: INDArray;
    procedure SetRadius(aValue: Integer);
    procedure SetKerType(aValue: TKernelType);
    procedure UpdateKernelMask;
    procedure InitBuffers(aW, aH: NativeInt); virtual; abstract;
    procedure MGetV(aA, aB, aRes: PByte; aN: NativeInt); virtual; abstract;
    procedure SetParallelize(aValue: Boolean); override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Execute(pSrc, pDst: PByte; aSrcWStep, aDstWStep, aW, aH: NativeInt); override;

    property Radius: Integer read fRadius write SetRadius;
    property KernelType: TKernelType read fKerType write SetKerType;
  end;

  TDCMFilter2D<T> = class abstract(TDCMFilter2D)
  protected
    procedure InitBuffers(aW, aH: NativeInt); override;
  end;

  TMinDCFilter2DUI8 = class(TDCMFilter2D<Byte>)
  protected
    procedure MGetV(aA, aB, aRes: PByte; aN: NativeInt);  override;
  public
    procedure AfterConstruction; override;
  end;

  THistUI8 = array [0..255] of NativeInt;
  PHistUI8 = ^THistUI8;

  TMedianTrackerUI8 = record
  private
    fCount, fHalf: NativeInt;
    fCHist: array [0..15] of NativeInt; // coarse histogram
    fFHist: array [0..15, 0..15] of NativeInt; // fine histogram
    procedure _Add(aNew: Byte); inline;
    procedure _Remove(aOld: Byte); inline;
  public
    procedure Init(const aHist: THistUI8); overload;
    procedure Replace(aOld, aNew: Byte); overload; inline;
    procedure Replace(aOld, aNew: PByte; aStep, aCount: NativeInt); overload; inline;
    // Add() is intended for borders where window size is variable
    procedure Add(aNew: Byte); overload; inline;
    procedure Add(aNew: PByte; aStep, aCount: NativeInt); overload; inline;
    procedure Remove(aOld: Byte); overload; inline;
    procedure Remove(aOld: PByte; aStep, aCount: NativeInt); overload; inline;
    function Median: Byte; inline;
  end;

  TMedianFilter1DUI8 = class(TFilter1D)
  protected
    procedure ExecMain(pSrc, pDst: PByte; aCount: NativeInt);
    procedure ExecMargins(pSrc, pDst: PByte; aCount: NativeInt);
  public
    procedure Execute(pSrc, pDst: PByte; aCount: NativeInt); override;
  end;

  TMedianFilter1DF64 = class(TFilter1D)
  protected const
    csErrMsg = 'MedianFilter1DF64 failed.';
  protected
    procedure ExecMain(pSrc, pDst: PDouble; aCount: NativeInt);
    procedure ExecMargins(pSrc, pDst: PDouble; aCount: NativeInt);
  public
    procedure Execute(pSrc, pDst: PByte; aCount: NativeInt); override;
  end;

  TMedianFilter2DUI8 = class(TFilter2D)
  protected const
    MARGIN_H    = 1;
    MARGIN_V    = 2;
    MARGIN_ALL  = MARGIN_H or MARGIN_V;
  protected
    procedure UpdateHist(var aHist: THistUI8; pSrc: PByte; aSrcWStep, aW, aH: NativeInt);
    procedure Exec(pSrc, pDst: PByte; aSrcWStep, aDstWStep, aW, aH: NativeInt);
    procedure ExecCH_H32(pSrc, pDst: PByte; aSrcWStep, aDstWStep, aW, aH: NativeInt);
    procedure ExecCH_H16(pSrc, pDst: PByte; aSrcWStep, aDstWStep, aW, aH: NativeInt);
    procedure Exec3x3(pSrc, pDst: PByte; aSrcWStep, aDstWStep, aW, aH: NativeInt);
    procedure Exec5x5(pSrc, pDst: PByte; aSrcWStep, aDstWStep, aW, aH: NativeInt);
    procedure ExecMargins(pSrc, pDst: PByte; aSrcWStep, aDstWStep, aW, aH: NativeInt;
      aFlags: Integer = MARGIN_ALL);
    procedure ExecHMargin(pSrc, pDst: PByte; aSrcWStep, aDstWStep, aW, aH: NativeInt; aTop: Boolean);
    procedure ExecVMargin(pSrc, pDst: PByte; aSrcWStep, aDstWStep, aW, aH: NativeInt; aLeft: Boolean);
    procedure ExecCorner(pSrc, pDst: PByte; aSrcWStep, aDstWStep, aW, aH: NativeInt; aTop, aLeft: Boolean);
  public
    procedure Execute(pSrc, pDst: PByte; aSrcWStep, aDstWStep, aW, aH: NativeInt); override;
  end;

{$region 'Sorting networks'}

type
  TBytes6 = array [0..5] of Byte;

procedure Sort6(var aSrc: TBytes6); inline;

{$endregion}

implementation

uses
    panda.cvArithmetic
  ;

{$I AsmDefs.inc}

{$region 'utils'}

procedure SortPair(var A, B: Byte); inline;
var tmp: Byte;
begin
  if A > B then begin
    tmp := A;
    A := B;
    B := tmp;
  end;
end;

procedure SortPairs(pA, pB, pMin, pMax: PByte; aCount: NativeInt); overload; inline;
begin
  VecMinMax(PUInt8(pA), PUInt8(pB), PUInt8(pMin), PUInt8(pMax), aCount);
end;

// pA^ <- Min(pA^, pB^), pB^ <- Max(pA^, pB^)
procedure SortPairs(pA, pB: PByte; aCount: NativeInt); overload; inline;
begin
  VecMinMax(PUInt8(pA), PUInt8(pB), PUInt8(pA), PUInt8(pB), aCount);
end;

procedure Sort6(var aSrc: TBytes6);
begin
  SortPair(aSrc[0], aSrc[5]); SortPair(aSrc[1], aSrc[3]); SortPair(aSrc[2], aSrc[4]);
  SortPair(aSrc[1], aSrc[2]); SortPair(aSrc[3], aSrc[4]);
  SortPair(aSrc[0], aSrc[3]); SortPair(aSrc[2], aSrc[5]);
  SortPair(aSrc[0], aSrc[1]); SortPair(aSrc[2], aSrc[3]); SortPair(aSrc[4], aSrc[5]);
  SortPair(aSrc[1], aSrc[2]); SortPair(aSrc[3], aSrc[4]);
end;

{$endregion}

{$region 'TFilter1D'}

procedure TFilter1D.AfterConstruction;
begin
  inherited;
  fRadius := 2;
end;

procedure TFilter1D.SetRadius(aValue: Integer);
begin
  if aValue > 0 then
    fRadius := aValue;
end;

{$endregion}

{$region 'TFilter2D'}

procedure TFilter2D.AfterConstruction;
begin
  inherited;
  fHRadius := 1;
  fVRadius := 1;
end;

procedure TFilter2D.SetHRadius(aValue: Integer);
begin
  if aValue > 0 then
    fHRadius := aValue;
end;

procedure TFilter2D.SetVRadius(aValue: Integer);
begin
  if aValue > 0 then
    fVRadius := aValue;
end;

{$endregion}

{$region 'TMFilter1D'}

procedure TMFilter1D.AfterConstruction;
begin
  inherited;
  fUseHGW := True;
end;

{$endregion}

{$region 'TMFilter1D<T>'}

procedure TMFilter1D<T>.MGetV(aA, aB, aRes: PByte; aN: NativeInt);
var pEnd: PByte;
begin
  pEnd := aA + aN * SizeOf(T);
  while aA < pEnd do begin
    PT(aRes)^ := MGet2(PT(aA)^, PT(aB)^);
    Inc(aRes, cElSz);
    Inc(aA, cElSz);
    Inc(aB, cElSz);
  end;
end;

procedure TMFilter1D<T>.HGWBlockEval(aA, aBuff: PByte; aN: NativeInt);
{$ifdef RANGEON}
  {$R-}
{$endif}
var I: NativeInt;
    A, L, R: PArrayOfT;
const cStep = 8;
begin
  A := PArrayOfT(aA);
  L := PArrayOfT(aBuff);
  R := PArrayOfT(aBuff + (aN - 1) * cElSz);

  R[0] := A[0];
  for I := 1 to aN - 2 do
    R[I] := MGet2(R[I - 1], A[I]);
  I := 0;
  L[aN - 2] := A[I - 1];
  for I := 1 to aN - 2 do
    L[aN - I - 2] := MGet2(L[aN - I - 1], A[-I - 1]);
end;
{$ifdef RANGEON}
  {$R+}
{$endif}

procedure TMFilter1D<T>.HGWFilter(pA, pB: PByte; aN: Integer);
var R, U, s: NativeInt;
    buff: TArray<T>;
    pL, pR: PByte;
begin
  R := fRadius;
  s := 2 * R + 1;
  SetLength(buff, 2*s - 2);
  pL := PByte(buff);
  pR := PByte(buff) + (s - 1)*cElSz;
  U := 2 * R;
  while U < aN - R do begin
    HGWBlockEval(pA + U * cElSz, pL, s);
    MGetV(pL, pR, pB + (U - R) * cElSz, s - 1);
    Inc(U, s - 1);
  end;
end;

procedure TMFilter1D<T>.Execute(aSrc, aDst: PByte; aN: NativeInt);
var pSrc, pDst, pEnd: PByte;
    I, count: NativeInt;
    m: T;
begin
  if fRadius >= aN then begin
    m := MGetN(aSrc, aN);
    pEnd := aDst + aN * cElSz;
    while aDst < pEnd do begin
      PT(aDst)^ := m;
      Inc(aDst, cElSz);
    end;
    exit;
  end;

  pSrc := aSrc;
  pDst := aDst;
  count := Min(fRadius + 1, aN);
  m := MGetN(pSrc, count);
  PT(pDst)^ := m;
  Inc(pDst, cElSz);
  Inc(pSrc, count * cElSz);
  for I := 1 to fRadius - 1 do begin
    m := MGet2(PT(pSrc)^, m);
    PT(pDst)^ := m;
    Inc(pSrc, cElSz);
    Inc(pDst, cElSz);
  end;

  if fUseHGW then
    HGWFilter(aSrc, aDst, aN)
  else begin
    pSrc := aSrc;
    pDst := aDst + fRadius * cElSz;
    count := aN - 2*fRadius;
    if count > 0 then begin
      Move(pSrc^, pDst^, count * cElSz);
      for I := 1 to 2*fRadius do
        MGetV(pSrc + I * cElSz, pDst, pDst, count);
    end;
  end;

  pSrc := aSrc + (aN - fRadius - 1) * cElSz;
  pDst := aDst + (aN - 1) * cElSz;
  count := Min(fRadius + 1, aN);
  m := MGetN(pSrc, count);
  PT(pDst)^ := m;
  Dec(pDst, cElSz);
  Dec(pSrc, cElSz);
  if fUseHGW then
    count := 2 * fRadius
  else
    count := fRadius;
  for I := 1 to count - 1 do begin
    m := MGet2(PT(pSrc)^, m);
    PT(pDst)^ := m;
    Dec(pSrc, cElSz);
    Dec(pDst, cElSz);
  end;
end;

{$endregion}

{$region 'TMFilter1DUI8'}

procedure TMFilter1DUI8.AfterConstruction;
begin
  inherited;
{$ifdef ASM}
  fUseHGW := False; // SSE version is faster
{$endif}
end;

{$endregion}

{$region 'TMinFilter1DUI8'}

function TMinFilter1DUI8.MGetN(aSrc: PByte; aN: NativeInt): Byte;
begin
  Result := Min_NU8(PUInt8(aSrc), aN);
end;

function TMinFilter1DUI8.MGet2(const aA, aB: Byte): Byte;
begin
  if aA < aB then
    Result := aA
  else
    Result := aB;
end;

procedure TMinFilter1DUI8.MGetV(aA, aB, aRes: PByte; aN: NativeInt);
begin
  VecMin(PUInt8(aA), PUInt8(aB), PUInt8(aRes), aN);
end;

{$endregion}

{$region 'TMinFilter1DF64'}

function TMinFilter1DF64.MGetN(aSrc: PByte; aN: NativeInt): Double;
var pEnd: PByte;
    v: Double;
begin
  pEnd := PByte(aSrc) + aN * SizeOf(Double);
  Result := PDouble(aSrc)^;
  Inc(aSrc, cF64Sz);
  while aSrc < pEnd do begin
    v := PDouble(aSrc)^;
    if v < Result then
      Result := v;
    Inc(aSrc, cF64Sz);
  end;
end;

function TMinFilter1DF64.MGet2(const aA, aB: Double): Double;
begin
  if aA < aB then
    Result := aA
  else
    Result := aB;
end;

procedure TMinFilter1DF64.MGetV(aA, aB, aRes: PByte; aN: NativeInt);
begin
  VecMin(PDouble(aA), PDouble(aB), PDouble(aRes), aN);
end;

{$endregion}

{$region 'TMaxFilter1DUI8'}

function TMaxFilter1DUI8.MGetN(aSrc: PByte; aN: NativeInt): Byte;
var pEnd: PByte;
begin
  pEnd := aSrc + aN;
  Result := aSrc^;
  Inc(aSrc);
  while aSrc < pEnd do begin
    if aSrc^ > Result then
      Result := aSrc^;
    Inc(aSrc);
  end;
end;

function TMaxFilter1DUI8.MGet2(const aA, aB: Byte): Byte;
begin
  if aA > aB then
    Result := aA
  else
    Result := aB;
end;

procedure TMaxFilter1DUI8.MGetV(aA, aB, aRes: PByte; aN: NativeInt);
begin
  VecMax(PUInt8(aA), PUInt8(aB), PUInt8(aRes), aN);
end;

{$endregion}

{$region 'TMaxFilter1DF64'}

function TMaxFilter1DF64.MGetN(aSrc: PByte; aN: NativeInt): Double;
var pEnd: PByte;
    v: Double;
begin
  pEnd := aSrc + aN * SizeOf(Double);
  Result := PDouble(aSrc)^;
  Inc(aSrc, cF64Sz);
  while aSrc < pEnd do begin
    v := PDouble(aSrc)^;
    if v > Result then
      Result := v;
    Inc(aSrc, cF64Sz);
  end;
end;

function TMaxFilter1DF64.MGet2(const aA, aB: Double): Double;
begin
  if aA > aB then
    Result := aA
  else
    Result := aB;
end;

procedure TMaxFilter1DF64.MGetV(aA, aB, aRes: PByte; aN: NativeInt);
begin
  VecMax(PDouble(aA), PDouble(aB), PDouble(aRes), aN);
end;

{$endregion}

{$region 'TGMFilter2D'}

procedure TGMFilter2D.SetParallelize(aValue: Boolean);
begin
  fParallelize := aValue;
end;

{$endregion}

{$region 'TBoxMFilter2D'}

procedure TBoxMFilter2D.AfterConstruction;
begin
  inherited;
  fHRadius := 2;
  fVRadius := 2;
end;

procedure TBoxMFilter2D.BeforeDestruction;
begin
  fFilter1D.Free;
  inherited;
end;

function TBoxMFilter2D.GetUseHGW: Boolean;
begin
  Result := fFilter1D.UseHGWMethod;
end;

procedure TBoxMFilter2D.SetUseHGW(aValue: Boolean);
begin
  fFilter1D.UseHGWMethod := aValue;
end;

{$endregion}

{$region 'TBoxMFilter2D<T>'}

procedure TBoxMFilter2D<T>._ExecNaive(pSrc, pDst: PByte; aSrcWStep, aDstWStep, aW, aH: NativeInt);
var I, J, J0, J1, buffWStep: NativeInt;
    pSrcRow, pDstRow: PByte;
begin
  fFilter1D.Radius := fHRadius;
  pSrcRow := pSrc;
  pDstRow := fBuff.Data;
  buffWStep := fBuff.Strides[0];
  for I := 0 to aH - 1 do begin
    fFilter1D.Execute(pSrcRow, pDstRow, aW);
    Inc(pSrcRow, aSrcWStep);
    Inc(pDstRow, buffWStep);
  end;

  pSrc := fBuff.Data;
  pDstRow := pDst;
  for I := 0 to aH - 1 do begin
    J0 := Max(0, I - fVRadius);
    J1 := Min(aH - 1, I + fVRadius);
    pSrcRow := pSrc + J0 * buffWStep;
    Move(pSrcRow^, pDstRow^, aW * cElSz);
    for J := J0 + 1 to J1 do
      fFilter1D.MGetV(pSrc + J * buffWStep, pDStRow, pDstRow, aW);
    Inc(pDstRow, aDstWStep);
  end;
end;

procedure TBoxMFilter2D<T>._ExecHGW(pSrc, pDst: PByte; aSrcWStep, aDstWStep, aW, aH: NativeInt);
var pSrcRow, pDstRow: PByte;
    I, buffWStep: NativeInt;
    dst: INDArray<T>;
begin
  pSrcRow := pSrc;
  pDstRow := pDst;
  fFilter1D.Radius := fHRadius;
  for I := 0 to aH - 1 do begin
    fFilter1D.Execute(pSrcRow, pDstRow, aW);
    Inc(pSrcRow, aSrcWStep);
    Inc(pDstRow, aDstWStep);
  end;

  dst := TNDArray<T>.Create(pDst, [aH, aW], [aDstWStep, SizeOf(T)]);
  TNDAMan.Transpose<T>(dst, fBuff[[NDI(0)]], [1, 0]);

  pSrcRow := fBuff.Data;
  pDstRow := fBuff[[NDI(1)]].Data;
  buffWStep := fBuff.Strides[1];
  fFilter1D.Radius := fVRadius;
  for I := 0 to aW - 1 do begin
    fFilter1D.Execute(pSrcRow, pDstRow, aH);
    Inc(pSrcRow, buffWStep);
    Inc(pDstRow, buffWStep);
  end;

  TNDAMan.Transpose<T>(fBuff[[NDI(1)]], dst, [1, 0]);
end;

procedure TBoxMFilter2D<T>.Execute(pSrc, pDst: PByte; aSrcWStep, aDstWStep, aW, aH: NativeInt);
begin
  AdjustBuffers(aW, aH);
  if UseHGWMethod then
    _ExecHGW(pSrc, pDst, aSrcWStep, aDstWStep, aW, aH)
  else
    _ExecNaive(pSrc, pDst, aSrcWStep, aDstWStep, aW, aH);
end;

procedure TBoxMFilter2D<T>.AdjustBuffers(aW, aH: NativeInt);
begin
  if Assigned(fBuff) and (fBuff.Shape[1] = aH) and (fBuff.Shape[2] = aW) then
    exit;

  if UseHGWMethod then
    fBuff := TNDABuffer<T>.Create([2, aH, aW])
  else
    fBuff := TNDABuffer<T>.Create([aH, aW]);
end;

{$endregion}

{$region 'TMinFilter2DUI8'}

procedure TBoxMinFilter2DUI8.AfterConstruction;
begin
  fFilter1D := TMinFilter1DUI8.Create;
  inherited;
end;

{$endregion}

{$region 'TMaxFilter2DUI8'}

procedure TBoxMaxFilter2DUI8.AfterConstruction;
begin
  fFilter1D := TMaxFilter1DUI8.Create;
  inherited;
end;

{$endregion}

{$region 'TMFilter2D'}

procedure TMFilter2D.AfterConstruction;
begin
  inherited;
  fEvalMargins := True;
  fParallelize := False;
end;

procedure TMFilter2D.Assign(aSrc: TMFilter2D);
var kerLen: NativeInt;
begin
  fKernel := aSrc.Kernel;
  fKerW := aSrc.fKerW;
  fKerH := aSrc.fKerH;
  kerLen := Length(aSrc.fKerIdxs);
  SetLength(fKerIdxs, kerLen);
  Move(aSrc.fKerIdxs[0], fKerIdxs[0], kerLen * SizeOf(TMatIdx));
  fEvalMargins := aSrc.fEvalMargins;
end;

procedure TMFilter2D.SetKernel(const aValue: INDArray<Byte>);
var I, J, top: NativeInt;
    m: TNDAMatItems<Byte>;
begin
  Assert(aValue.NDim = 2);

  m := aValue;
  SetLength(fKerIdxs, m.RowCount * m.ColCount);
  top := 0;
  fKerW := m.ColCount;
  fKerH := m.RowCount;
  for I := 0 to fKerH - 1 do
    for J := 0 to fKerW - 1 do
      if m[I, J] > 0 then with fKerIdxs[top] do begin
        RIdx := I;
        CIdx := J;
        Inc(top);
      end;
  SetLength(fKerIdxs, top);
end;

procedure TMFilter2D._Exec(pSrc, pDst: PByte; aSrcWStep, aDstWStep, aW, aH: NativeInt);
var pSrcRow, pDstRow: PByte;
    I, J, w, rx, ry, elSz: NativeInt;
begin
  elSz := GetElemSize();
  rx := fKerW div 2;
  ry := fKerH div 2;
  pSrcRow := pSrc;
  pDstRow := pDst + ry * aDstWStep + rx * elSz;
  w := aW - fKerW + 1;
  for I := 0 to aH - fKerH do begin
    with fKerIdxs[0] do
      pSrc := pSrcRow + RIdx * aSrcWStep + CIdx * elSz;
    Move(pSrc^, pDstRow^, w * elSz);
    for J := 1 to High(fKerIdxs) do begin
      with fKerIdxs[J] do
        pSrc := pSrcRow + RIdx * aSrcWStep + CIdx * elSz;
      MGetV(pSrc, pDstRow, pDstRow, w);
    end;
    Inc(pSrcRow, aSrcWStep);
    Inc(pDstRow, aDstWStep);
  end;
end;

procedure TMFilter2D.Execute(pSrc, pDst: PByte; aSrcWStep, aDstWStep, aW, aH: NativeInt);
var I, N, h, hr: NativeInt;
    w: TArray<TMFilter2D>;

  procedure InitFilter(aFilter: TMFilter2D);
  begin
    aFilter.Assign(Self);
    aFilter.fEvalMargins := False;
    aFilter.Parallelize := False;
  end;

begin
  if fParallelize then begin
    N := aH div fKerH - 1;
    N := Min(N, TThreadPool.Current.MaxWorkerThreads div 2 (* = ProcessorCount*));
    if N <= 1 then begin
      _Exec(pSrc, pDst, aSrcWStep, aDstWStep, aW, aH);
      exit;
    end;

    h := (aH - fKerH) div N;
    SetLength(w, N);
    for I := 0 to High(w) do begin
      w[I] := TMFilter2DClass(Self.ClassType).Create;
      InitFilter(w[I]);
    end;

    try
      TParallel.For(0, N - 1,
        procedure (I: NativeInt)
        var src, dst: PByte;
        begin
          src := pSrc + I * h * aSrcWStep;
          dst := pDst + I * h * aDstWStep;
          w[I].Execute(src, dst, aSrcWStep, aDstWStep, aW, h + fKerH);
        end
      );
    finally
      for I := 0 to High(w) do
        w[I].Free;
    end;

    hr := (aH - fkerH) mod N;
    if hr > 0 then
      _Exec(
        pSrc + N * h * aSrcWStep, pDst + N * h * aDstWStep,
        aSrcWStep, aDstWStep, aW, hr + fKerH
      );
  end else
    _Exec(pSrc, pDst, aSrcWStep, aDstWStep, aW, aH);

  if fEvalMargins then
    _ExecMargins(pSrc, pDst, aSrcWStep, aDstWstep, aW, aH);
end;

{$endregion}

{$region 'TMFilter2D<T>'}

procedure TMFilter2D<T>._ExecMargins(pSrc, pDst: PByte; aSrcWStep, aDstWStep, aW, aH: NativeInt);
var src, buff: INDArray<T>;
    dstPtr: PByte;
    rx, ry: NativeInt;
begin
  rx := fKerW div 2;
  ry := fKerH div 2;
  src := TNDArray<T>.Create(pSrc, [aH, aW], [aSrcWStep, SizeOf(T)]);
  buff := TNDABuffer<T>.Create([ry + fKerH - 1, aW + fKerW]);

  // top margin
  TNDAUt.Fill<T>(buff, InitValue());
  buff[[NDISpan(ry, ry + fKerH - 2), NDISpan(rx, rx + aW - 1)]] := src[[NDISpan(0, fkerH - 2), NDIAll]];
  dstPtr := pDst - ry * aDstWStep - rx * cElSz;
  _Exec(buff.Data, dstPtr, buff.Strides[0], aDstWStep, aW + fKerW - 1, ry + fKerH - 1);

  // bottom margin
  TNDAUt.Fill<T>(buff, InitValue());
  buff[[NDISpan(0, fKerH - 2), NDISpan(rx, rx + aW - 1)]] := src[[NDISpan(-(fKerH - 2) - 1, -1), NDIAll]];
  dstPtr := pDst + (aH - fKerH + 1) * aDstWStep - rx * cElSz;
  _Exec(buff.Data, dstPtr, buff.Strides[0], aDstWStep, aW + fKerW - 1, ry + fkerH - 1);

  buff := TNDABuffer<T>.Create([aH, rx + fKerW - 1]);

  // left margin
  TNDAUt.Fill<T>(buff, InitValue());
  buff[[NDIAll, NDISpan(rx, rx + fKerW - 2)]] := src[[NDIAll, NDISpan(0, fKerW - 2)]];
  dstPtr := pDst - rx * cElSz;
  _Exec(buff.Data, dstPtr, buff.Strides[0], aDstWStep, rx + fKerW - 1, aH);

  // right margin
  TNDAUt.Fill<T>(buff, InitValue());
  buff[[NDIAll, NDISpan(0, fKerW - 2)]] := src[[NDIAll, NDISpan(-(fKerW - 2) - 1)]];
  dstPtr := pDst + (aW - fKerW + 1) * cElSz;
  _Exec(buff.Data, dstPtr, buff.Strides[0], aDstWStep, rx + fKerW - 1, aH);
end;

function TMFilter2D<T>.GetElemSize: Integer;
begin
  Result := SizeOf(T);
end;

{$endregion}

{$region 'TMinFilter2DUI8'}

procedure TMinFilter2DUI8.MGetV(aA, aB, aRes: PByte; aN: NativeInt);
begin
  VecMin(PUInt8(aA), PUInt8(aB), PUInt8(aRes), aN);
end;

function TMinFilter2DUI8.InitValue: Byte;
begin
  Result := 255;
end;

{$endregion}

{$region 'TMaxFilter2DUI8'}

procedure TMaxFilter2DUI8.MGetV(aA, aB, aRes: PByte; aN: NativeInt);
begin
  VecMax(PUInt8(aA), PUInt8(aB), PUInt8(aRes), aN);
end;

function TMaxFilter2DUI8.InitValue: Byte;
begin
  Result := 0;
end;

{$endregion}

{$region 'TDCMFilter2D'}

procedure TDCMFilter2D.AfterConstruction;
begin
  inherited;
  fKerType := ktDiamond;
  fRadius := 2;
end;

procedure  TDCMFilter2D.BeforeDestruction;
begin
  fBoxFilter.Free;
  fRestFilter.Free;
  inherited;
end;

procedure TDCMFilter2D.SetRadius(aValue: Integer);
begin
  if (aValue <= 0) or (aValue = fRadius) then exit;

  fRadius := aValue;
  UpdateKernelMask;
end;

procedure TDCMFilter2D.SetKerType(aValue: TKernelType);
begin
  if aValue = fKerType then exit;

  fKerType := aValue;
  UpdateKernelMask;
end;

procedure TDCMFilter2D.UpdateKernelMask;
var I, J, w, r, bx, by, ai, aj, ai2, aj2: Integer;
    x, y, rlim: Double;
    k: TNDAMatItems<Byte>;
begin
  r := fRadius;
  w := 2 * r + 1;
  case fKerType of
    ktRect: begin
      fBoxFilter.HRadius := r;
      fBoxFilter.VRadius := r;
      exit;
    end;

    ktDiamond: begin
      bx := r div 2;
      by := r - bx;
      k := TNDAUt.Empty<Byte>([w, w]);
      for J := 0 to w - 1 do begin
        aj := Abs(J - r);
        for I := 0 to w - 1 do begin
          ai := Abs(I - r);
          if ((ai > bx) or (aj > by)) and (ai + aj <= r) then
            k[J, I] := 255
          else
            k[J, I] := 0;
        end;
      end;
    end;

    ktCircle: begin
      rlim := Sqr(r + 1/2);
      x := (r + 1/2) / Sqrt(2);
      if Sqr(Floor(x)) + Sqr(Floor(x) + 1) <= rlim then
        y := x + 1
      else
        y := x;
      bx := Ceil(x);
      by := Ceil(y);
      x := Sqr(bx);
      y := Sqr(by);
      Dec(bx);
      Dec(by);
      k := TNDAUt.Empty<Byte>([w, w]);
      for J := 0 to w - 1 do begin
        aj := Abs(J - r);
        aj2 := aj * aj;
        for I := 0 to w - 1 do begin
          ai := Abs(I - r);
          ai2 := ai * ai;
          if ((ai2 >= x) or (aj2 >= y)) and (ai2 + aj2 < rlim) then
            k[J, I] := 255
          else
            k[J, I] := 0;
        end;
      end;
    end;
  else
    exit;
  end;

  fBoxFilter.HRadius := bx;
  fBoxFilter.VRadius := by;
  fRestFilter.Kernel := k;
end;

procedure TDCMFilter2D.Execute(pSrc, pDst: PByte; aSrcWStep, aDstWStep, aW, aH: NativeInt);
var I, BWStep, RWStep: NativeInt;
    pB, pR: PByte;
begin
  if fKerType = ktRect then begin
    fBoxFilter.Execute(pSrc, pDst, aSrcWStep, aDstWStep, aW, aH);
    exit;
  end;

  InitBuffers(aW, aH);
  pB := fBuffB.Data;
  pR := fBuffR.Data;
  BWStep := fBuffB.Strides[0];
  RWStep := fBuffr.Strides[0];
  fBoxFilter.Execute(pSrc, pB, aSrcWStep, BWStep, aW, aH);
  fRestFilter.Execute(pSrc, pR, aSrcWStep, RWStep, aW, aH);
  for I := 0 to aH - 1 do begin
    MGetV(pB, pR, pDst, aW);
    Inc(pDst, aDstWStep);
    Inc(pB, BWStep);
    Inc(pR, RWStep);
  end;
end;

procedure TDCMFilter2D.SetParallelize(aValue: Boolean);
begin
  inherited;
  fBoxFilter.Parallelize := aValue;
  fRestFilter.Parallelize := aValue;
end;

{$endregion}

{$region 'TDCMFilter2D<T>'}

procedure TDCMFilter2D<T>.InitBuffers(aW, aH: NativeInt);
begin
  if Assigned(fBuffB) and (fBuffB.Shape[0] = aH) and (fBuffB.Shape[1] = aW) then
    exit;

  fBuffB := TNDABuffer<T>.Create([aH, aW]);
  fBuffR := TNDABuffer<T>.Create([aH, aW]);
end;

{$endregion}

{$region 'TMinDCFilter2DUI8'}

procedure TMinDCFilter2DUI8.AfterConstruction;
begin
  fBoxFilter := TBoxMinFilter2DUI8.Create;
  fRestFilter := TMinFilter2DUI8.Create;
  inherited;
end;

procedure TMinDCFilter2DUI8.MGetV(aA, aB, aRes: PByte; aN: NativeInt);
begin
  VecMin(PUInt8(aA), PUInt8(aB), PUInt8(aRes), aN);
end;

{$endregion}

{$region 'TMedianTrackerUI8'}

procedure TMedianTrackerUI8.Init(const aHist: THistUI8);
var I, J, K: Integer;
    v: NativeInt;
begin
  FillChar(fFHist, SizeOf(fFHist), 0);
  FillChar(fCHist, SizeOf(fCHist), 0);
  fCount := 0;
  for I := 0 to High(aHist) do begin
    v := aHist[I];
    J := I shr 4;
    K := I and 15;
    fFHist[J][K] := v;
    Inc(fCHist[J], v);
    Inc(fCount, v);
  end;
  fHalf := fCount div 2;
end;

procedure TMedianTrackerUI8._Add(aNew: Byte);
var J: Integer;
begin
  J := aNew shr 4;
  Inc(fFHist[J, aNew and 15]);
  Inc(fCHist[J]);
end;

procedure TMedianTrackerUI8._Remove(aOld: Byte);
var J: Integer;
begin
  J := aOld shr 4;
  Dec(fFHist[J, aOld and 15]);
  Dec(fCHist[J]);
end;

procedure TMedianTrackerUI8.Replace(aOld, aNew: Byte);
begin
  _Remove(aOld);
  _Add(aNew);
end;

procedure TMedianTrackerUI8.Replace(aOld, aNew: PByte; aStep, aCount: NativeInt);
var pEnd: PByte;
begin
  pEnd := aOld + aCount * aStep;
  while aOld <> pEnd do begin
    _Remove(aOld^);
    _Add(aNew^);
    Inc(aOld, aStep);
    Inc(aNew, aStep);
  end;
end;

procedure TMedianTrackerUI8.Add(aNew: Byte);
begin
  _Add(aNew);
  Inc(fCount);
  fHalf := fCount div 2;
end;

procedure TMedianTrackerUI8.Add(aNew: PByte; aStep, aCount: NativeInt);
var pEnd: PByte;
begin
  pEnd := aNew + aCount * aStep;
  while aNew <> pEnd do begin
    _Add(aNew^);
    Inc(aNew, aStep);
  end;
  Inc(fCount, aCount);
  fHalf := fCount div 2;
end;

procedure TMedianTrackerUI8.Remove(aOld: Byte);
begin
  _Remove(aOld);
  Dec(fCount);
  fHalf := fCount div 2;
end;

procedure TMedianTrackerUI8.Remove(aOld: PByte; aStep, aCount: NativeInt);
var pEnd: PByte;
begin
  pEnd := aOld + aCount * aStep;
  while aOld <> pEnd do begin
    _Remove(aOld^);
    Inc(aOld, aStep);
  end;
  Dec(fCount, aCount);
  fHalf := fCount div 2;
end;

function TMedianTrackerUI8.Median: Byte;
var I, J: Integer;
    s, v: NativeInt;
begin
  J := 15;
  s := 0;
  for I := 0 to 15 do begin
    v := fCHist[I];
    if s + v <= fHalf then
      Inc(s, v)
    else begin
      J := I;
      break;
    end;
  end;

  for I := 0 to 15 do begin
    v := fFHist[J, I];
    if s + v <= fHalf then
      Inc(s, v)
    else begin
      Result := Byte((J shl 4) + I);
      exit;
    end;
  end;

  Result := 255;
end;

{$endregion}

{$region 'TMedianFilter1DUI8'}

procedure TMedianFilter1DUI8.ExecMain(pSrc, pDst: PByte; aCount: NativeInt);
var pNew, pOld, pOut, pEnd: PByte;
    mt: TMedianTrackerUI8;
    H: THistUI8;
    r: Integer;
begin
  r := fRadius;
  if (aCount - 2 * r) <= 0 then exit;

  FillChar(H, SizeOf(H), 0);
  pNew := pSrc;
  pEnd := pSrc + 2 * r + 1;
  while pNew < pEnd do begin
    Inc(H[pNew^]);
    Inc(pNew);
  end;
  mt.Init(H);

  pOld := pSrc;
  pNew := pEnd;
  pOut := pDst + r;
  pEnd := pSrc + aCount;
  while pNew < pEnd do begin
    pOut^ := mt.Median;
    mt.Replace(pOld^, pNew^);
    Inc(pOld);
    Inc(pNew);
    Inc(pOut);
  end;
  pOut^ := mt.Median;
end;

procedure TMedianFilter1DUI8.ExecMargins(pSrc, pDst: PByte; aCount: NativeInt);
var pIn, pOut: PByte;
    H: THistUI8;
    mt: TMedianTrackerUI8;
    I, r: Integer;
begin
  r := fRadius;
  FillChar(H, SizeOf(H), 0);
  pIn := pSrc;
  pOut := pDst;
  for I := 0 to r do begin
    Inc(H[pIn^]);
    Inc(pIn);
  end;
  mt.Init(H);
  for I := 0 to r - 1 do begin
    pOut^ := mt.Median;
    mt.Add(pIn^);
    Inc(H[pIn^]);
    Inc(pIn);
    Inc(pOut);
  end;

  FillChar(H, SizeOf(H), 0);
  pIn := pSrc + aCount - 1;
  pOut := pDst + aCount - 1;
  for I := 0 to r do begin
    Inc(H[pIn^]);
    Dec(pIn);
  end;
  mt.Init(H);
  for I := 0 to r - 1 do begin
    pOut^ := mt.Median;
    mt.Add(pIn^);
    Inc(H[pIn^]);
    Dec(pIn);
    Dec(pOut);
  end;
end;

procedure TMedianFilter1DUI8.Execute(pSrc, pDst: PByte; aCount: NativeInt);
begin
  ExecMain(pSrc, pDst, aCount);
  ExecMargins(pSrc, pDst, aCount);
end;

{$endregion}

{$region 'TMedianFilter1DF64'}

procedure TMedianFilter1DF64.ExecMain(pSrc, pDst: PDouble; aCount: NativeInt);
var kernel: TArray<Double>;
    kSz, I, J, idx, pos: NativeInt;
    pOut, pOld, pNew: PDouble;
    r: Integer;
    pEnd: PByte;
begin
  Assert((fRadius > 0) and (aCount > 0));

  r := fRadius;
  kSz := 2 * r + 1;
  SetLength(kernel, kSz);
  pNew := pSrc;
  pOut := pDst;
  Inc(pOut, r);
  pEnd := PByte(pDst) + (aCount - r) * cF64Sz;
  for I := 0 to kSz - 1 do begin
    kernel[I] := pNew^;
    Inc(pNew);
  end;
  pOld := pSrc;
  pNew := pSrc;
  Inc(pNew, kSz);
  TSortUtF64.QuickSort(kernel);
  pOut^ := kernel[r];
  Inc(pOut);
  while PByte(pOut) < pEnd do begin
    if not TSortUtF64.BinarySearch(kernel, pOld^, idx) then
      raise EInvalidOp.Create(csErrMsg);
    pos := idx;
    if (idx > 0) and (pNew^ < kernel[idx - 1]) then begin
      if pNew^ <= kernel[0] then
        J := 0
      else begin
        if not TSortUtF64.SearchPos(kernel, pNew^, J, 0, idx - 1) then
          raise EInvalidOp.Create(csErrMsg);
        Inc(J);
      end;
      for I := idx downto J + 1 do
        kernel[I] := kernel[I - 1];
      pos := J;
    end;
    if (pos = idx) and (idx < kSz - 1) and (pNew^ > kernel[idx + 1]) then begin
      if pNew^ >= kernel[kSz - 1] then
        J := kSz - 1
      else
        if not TSortUtF64.SearchPos(kernel, pNew^, J, idx + 1, kSz - 1) then
          raise EInvalidOp.Create(csErrMsg);
      for I := idx to J - 1 do
        kernel[I] := kernel[I + 1];
      pos := J;
    end;
    kernel[pos] := pNew^;
    pOut^ := kernel[r];
    Inc(pOut);
    Inc(pOld);
    Inc(pNew);
  end;
end;

procedure TMedianFilter1DF64.ExecMargins(pSrc, pDst: PDouble; aCount: NativeInt);
var kernel: TArray<Double>;
    I, J, cnt: Integer;
    pIn, pOut: PDouble;
    r: Integer;
    pEnd: PByte;
begin
  r := fRadius;
  SetLength(kernel, 2 * r + 1);

  cnt := Min(r + 1, aCount);
  pIn := pSrc;
  for I := 0 to cnt - 1 do begin
    kernel[I] := pIn^;
    Inc(pIn);
  end;
  pEnd := PByte(pDst) + Min(aCount, r) * cF64Sz;
  pOut := pDst;
  while PByte(pOut) < pEnd do begin
    TSortUtF64.QuickSort(kernel, 0, cnt - 1);
    J := cnt shr 1;
    if (cnt and 1) = 0 then
      pOut^ := 1/2*(kernel[J - 1] + kernel[J])
    else
      pOut^ := kernel[J];
    Inc(pOut);
    kernel[cnt] := pIn^;
    Inc(cnt);
    Inc(pIn);
  end;

  cnt := Min(r + 1, aCount);
  pIn := pSrc;
  Inc(pIn, aCount - 1);
  for I := 0 to cnt - 1 do begin
    kernel[I] := pIn^;
    Dec(pIn);
  end;
  pEnd := PByte(pDst) + (aCount - Min(aCount, r) - 1) * cF64Sz;
  pOut := pDst;
  Inc(pOut, aCount - 1);
  while PByte(pOut) > pEnd do begin
    TSortUtF64.QuickSort(kernel, 0, cnt - 1);
    J := cnt shr 1;
    if (cnt and 1) = 0 then
      pOut^ := 1/2*(kernel[J - 1] + kernel[J])
    else
      pOut^ := kernel[J];
    Dec(pOut);
    kernel[cnt] := pIn^;
    Inc(cnt);
    Dec(pIn);
  end;
end;

procedure TMedianFilter1DF64.Execute(pSrc, pDst: PByte; aCount: NativeInt);
begin
  Assert(aCount > 0);
  ExecMain(PDouble(pSrc), PDouble(pDst), aCount);
  ExecMargins(PDouble(pSrc), PDouble(pDst), aCount);
end;

{$endregion}

{$region 'TMedianFilter2DUI8'}

procedure TMedianFilter2DUI8.UpdateHist(var aHist: THistUI8; pSrc: PByte; aSrcWStep, aW, aH: NativeInt);
var pEnd, pRow, pRowEnd: PByte;
begin
  pRow := pSrc;
  pEnd := pRow + aH * aSrcWStep;
  pRowEnd := pRow + aW;
  while pRow <> pEnd do begin
    pSrc := pRow;
    while pSrc < pRowEnd do begin
      Inc(aHist[pSrc^]);
      Inc(pSrc);
    end;
    Inc(pRow, aSrcWStep);
    Inc(pRowEnd, aSrcWStep);
  end;
end;

procedure TMedianFilter2DUI8.Exec(pSrc, pDst: PByte; aSrcWStep, aDstWStep, aW, aH: NativeInt);
var H: THistUI8;
    mt: TMedianTrackerUI8;
    winSzX, winSzY, I, J: Nativeint;
    pInRow, pOutRow, pInOld, pInNew: PByte;
    bForward: Boolean;
begin
  winSzX := 2*fHRadius + 1;
  winSzY := 2*fVRadius + 1;
  pDst := pDst + fVRadius * aDstWStep + fHRadius;

  //initialize histogram
  FillChar(H, SizeOf(H), 0);
  UpdateHist(H, pSrc, aSrcWStep, winSzX, winSzY);
  mt.Init(H);

  bForward := True;
  for I := 0 to aH - winSzY do begin
    pOutRow := pDst + I * aDstWStep;
    pInRow := pSrc + I * aSrcWStep;
    if bForward then begin
      for J := 0 to aW - winSzX - 1 do begin // J-index of the old column
        (pOutRow + J)^ := mt.Median;
        pInOld := pInRow + J;
        pInNew := pInOld + winSzX;
        mt.Replace(pInOld, pInNew, aSrcWStep, winSzY);
      end;
      J := aW - winSzX;
    end else begin
      for J :=  aW - winSzX - 1 downto 0 do begin // J-index of the new column
        (pOutRow + J + 1)^ := mt.Median;
        pInNew := pInRow + J;
        pInOld := pInNew + winSzX;
        mt.Replace(pInOld, pInNew, aSrcWStep, winSzY);
      end;
      J := 0;
    end;
    (pOutRow + J)^ := mt.Median;
    bForward := not bForward;
    //update the histogram on moving along the y axis
    if I < aH - winSzY then begin
      pInOld := pSrc + I * aSrcWStep + J;
      pInNew := pInOld + winSzY * aSrcWstep;
      mt.Replace(pInOld, pInNew, 1, winSzX);
    end;
  end;
end;

type
  THistI32 = record
    C: array [0..15] of Integer;
    F: array [0..15, 0..15] of Integer;
    procedure Add(aValue: Byte); overload; inline;
    procedure Add(const aH: THistI32); overload; inline;
    procedure Sub(aValue: Byte); overload; inline;
    procedure Sub(const aH: THistI32); overload; inline;
    function Median(aQ: Integer): Byte; inline;
  end;

procedure THistI32.Add(aValue: Byte);
var I: Integer;
begin
  I := aValue shr 4;
  Inc(C[I]);
  Inc(F[I, aValue and 15]);
end;

procedure THistI32.Add(const aH: THistI32);
begin
  VecAdd(PInteger(@C), PInteger(@aH.C), PInteger(@C), 16 + 256);
end;

procedure THistI32.Sub(aValue: Byte);
var I: Integer;
begin
  I := aValue shr 4;
  Dec(C[I]);
  Dec(F[I, aValue and 15]);
end;

procedure THistI32.Sub(const aH: THistI32);
begin
  VecSub(PInteger(@C), PInteger(@aH.C), PInteger(@C), 16 + 256);
end;

function THistI32.Median(aQ: Integer): Byte;
var I, J: Integer;
    s, v: NativeInt;
begin
  J := 15;
  s := 0;
  for I := 0 to 15 do begin
    v := C[I];
    if s + v <= aQ then
      Inc(s, v)
    else begin
      J := I;
      break;
    end;
  end;

  for I := 0 to 15 do begin
    v := F[J, I];
    if s + v <= aQ then
      Inc(s, v)
    else begin
      Result := Byte((J shl 4) + I);
      exit;
    end;
  end;

  Result := 255;
end;

type
  THistI16 = record
    C: array [0..15] of UInt16;
    F: array [0..15, 0..15] of UInt16;
    procedure Add(aValue: Byte); overload; inline;
    procedure Add(const aH: THistI16); overload; inline;
    procedure Sub(aValue: Byte); overload; inline;
    procedure Sub(const aH: THistI16); overload; inline;
    function Median(aQ: Integer): Byte; inline;
  end;

procedure THistI16.Add(aValue: Byte);
var I: Integer;
begin
  I := aValue shr 4;
  Inc(C[I]);
  Inc(F[I, aValue and 15]);
end;

procedure THistI16.Add(const aH: THistI16);
begin
  VecAdd(PUInt16(@C), PUInt16(@aH.C), PUInt16(@C), 16 + 256);
end;

procedure THistI16.Sub(aValue: Byte);
var I: Integer;
begin
  I := aValue shr 4;
  Dec(C[I]);
  Dec(F[I, aValue and 15]);
end;

procedure THistI16.Sub(const aH: THistI16);
begin
  VecSub(PUInt16(@C), PUInt16(@aH.C), PUInt16(@C), 16 + 256);
end;

function THistI16.Median(aQ: Integer): Byte;
var I, J: Integer;
    s, v: NativeInt;
begin
  J := 15;
  s := 0;
  for I := 0 to 15 do begin
    v := C[I];
    if s + v <= aQ then
      Inc(s, v)
    else begin
      J := I;
      break;
    end;
  end;

  for I := 0 to 15 do begin
    v := F[J, I];
    if s + v <= aQ then
      Inc(s, v)
    else begin
      Result := Byte((J shl 4) + I);
      exit;
    end;
  end;

  Result := 255;
end;

procedure TMedianFilter2DUI8.ExecCH_H32(pSrc, pDst: PByte; aSrcWStep, aDstWStep, aW, aH: NativeInt);
var H: THistI32;
    CH: TArray<THistI32>;
    winSzX, winSzY, I, J, q, kCnt: Nativeint;
    pInRow, pOutRow, pInOld, pInNew: PByte;
begin
  winSzX := 2*fHRadius + 1;
  winSzY := 2*fVRadius + 1;
  pDst := pDst + fVRadius * aDstWStep;

  SetLength(CH, aW);
  pInRow := pSrc;
  for I := 0 to winSzY - 1 do begin
    for J := 0 to aW - 1 do
      CH[J].Add((pInRow + J)^);
    Inc(pInRow, aSrcWStep);
  end;

  pOutRow := pDst;
  for I := 0 to aH - winSzY do begin
    FillChar(H, SizeOf(H), 0);
    for J := 0 to fHRadius do
      H.Add(CH[J]);

    kCnt := (fHRadius + 1) * winSzY;
    for J := 0 to fHRadius - 1 do begin
      (pOutRow + J)^ := H.Median(kCnt div 2);
      H.Add(CH[J + fHRadius + 1]);
      Inc(kCnt, winSzY);
    end;

    q := kCnt div 2;
    for J := fHRadius to aW - winSzX do begin
      (pOutRow + J)^ := H.Median(q);
      H.Sub(CH[J - fHRadius]);
      H.Add(CH[J + fHRadius + 1]);
    end;

    for J := aW - winSzX + 1 to aW - 1 do begin
      (pOutRow + J)^ := H.Median(kCnt div 2);
      H.Sub(CH[J - fHRadius]);
      Dec(kCnt, winSzY);
    end;

    if I < aH - winSzY then begin
      pInOld := pSrc;
      pInNew := pSrc + winSzY * aSrcWStep;
      for J := 0 to aW - 1 do begin
        with CH[J] do begin
          Sub(pInOld^);
          Add(pInNew^);
        end;
        Inc(pInOld);
        Inc(pInNew);
      end;

      Inc(pOutRow, aDstWStep);
      Inc(pSrc, aSrcWStep);
    end;
  end;
end;

procedure TMedianFilter2DUI8.ExecCH_H16(pSrc, pDst: PByte; aSrcWStep, aDstWStep, aW, aH: NativeInt);
var H: THistI16;
    CH: TArray<THistI16>;
    winSzX, winSzY, I, J, q, kCnt: Nativeint;
    pInRow, pOutRow, pInOld, pInNew: PByte;
begin
  winSzX := 2*fHRadius + 1;
  winSzY := 2*fVRadius + 1;
  pDst := pDst + fVRadius * aDstWStep;

  SetLength(CH, aW);
  pInRow := pSrc;
  for I := 0 to winSzY - 1 do begin
    for J := 0 to aW - 1 do
      CH[J].Add((pInRow + J)^);
    Inc(pInRow, aSrcWStep);
  end;

  pOutRow := pDst;
  for I := 0 to aH - winSzY do begin
    FillChar(H, SizeOf(H), 0);
    for J := 0 to fHRadius do
      H.Add(CH[J]);

    kCnt := (fHRadius + 1) * winSzY;
    for J := 0 to fHRadius - 1 do begin
      (pOutRow + J)^ := H.Median(kCnt div 2);
      H.Add(CH[J + fHRadius + 1]);
      Inc(kCnt, winSzY);
    end;

    q := kCnt div 2;
    for J := fHRadius to aW - winSzX do begin
      (pOutRow + J)^ := H.Median(q);
      H.Sub(CH[J - fHRadius]);
      H.Add(CH[J + fHRadius + 1]);
    end;

    for J := aW - winSzX + 1 to aW - 1 do begin
      (pOutRow + J)^ := H.Median(kCnt div 2);
      H.Sub(CH[J - fHRadius]);
      Dec(kCnt, winSzY);
    end;

    if I < aH - winSzY then begin
      pInOld := pSrc;
      pInNew := pSrc + winSzY * aSrcWStep;
      for J := 0 to aW - 1 do begin
        with CH[J] do begin
          Sub(pInOld^);
          Add(pInNew^);
        end;
        Inc(pInOld);
        Inc(pInNew);
      end;

      Inc(pOutRow, aDstWStep);
      Inc(pSrc, aSrcWStep);
    end;
  end;
end;

procedure TMedianFilter2DUI8.Exec3x3(pSrc, pDst: PByte; aSrcWStep, aDstWStep, aW, aH: NativeInt);
var buff: TArray<Byte>;
    p, q: array [0..8] of PByte;
    pDstRow: PByte;
    w, I, J: NativeInt;
begin
  pDst := pDst + aDstWStep + 1;

  w := ((aW - 2) div 16) * 16;
//  w := aW - 2;
  SetLength(buff, 9 * w);
  for I := 0 to High(q) do
    q[I] := @buff[I * w];

  p[0] := pSrc;
  p[1] := p[0] + 1;
  p[2] := p[0] + 2;
  p[3] := pSrc + aSrcWStep;
  p[4] := p[3] + 1;
  p[5] := p[3] + 2;
  p[6] := pSrc + 2 * aSrcWStep;
  p[7] := p[6] + 1;
  p[8] := p[6] + 2;

  pDstRow := pDst;
  for I := 0 to aH - 3 do begin
    //  This sorting network is used:
    //  0 -o-----o---o-----o-------------------------------
    //  1 -o--o--o---|-----|---o-----o---------------------
    //  2 ----o------|-----|---|-----|------o------o-----o-
    //  3 -o-----o---o--o--o---|-----|------|------|-----|-
    //  4 -o--o--o------|------o--o--o------|------o--o--o-
    //  5 ----o---------|---------|------o--o--o------|----
    //  6 -o-----o------o---------|------|-----|------o----
    //  7 -o--o--o----------------o------|-----|-----------
    //  8 ----o--------------------------o-----o-----------

    SortPairs(p[0], p[1], q[0], q[1], w);
    SortPairs(p[3], p[4], q[3], q[4], w);
    SortPairs(p[6], p[7], q[6], q[7], w);
    SortPairs(q[1], p[2], q[1], q[2], w);
    SortPairs(q[4], p[5], q[4], q[5], w);
    SortPairs(q[7], p[8], q[7], q[8], w);

    SortPairs(q[0], q[1], w); SortPairs(q[3], q[4], w); SortPairs(q[6], q[7], w);
    SortPairs(q[0], q[3], w); SortPairs(q[3], q[6], w); SortPairs(q[0], q[3], w);
    SortPairs(q[1], q[4], w); SortPairs(q[4], q[7], w); SortPairs(q[1], q[4], w);
    SortPairs(q[5], q[8], w); SortPairs(q[2], q[5], w); SortPairs(q[5], q[8], w);
    SortPairs(q[2], q[4], w); SortPairs(q[4], q[6], w); SortPairs(q[2], q[4], w);

    Move(q[4]^, pDstRow^, w);
    for J := 0 to High(p) do
      Inc(p[J], aSrcWStep);
    Inc(pDstRow, aDstWStep);
  end;

  if aW - 2 - w > 0 then begin
    pDst := pDst - aDstWStep + w - 1;
    Exec(pSrc + w - 1, pDst, aSrcWStep, aDstWStep, aW - w, aH);
  end;
end;

procedure TMedianFilter2DUI8.Exec5x5(pSrc, pDst: PByte; aSrcWStep, aDstWStep, aW, aH: NativeInt);
var buff: TArray<Byte>;
    p, q: array [0..24] of PByte;
    pDstRow: PByte;
    w, I, J: NativeInt;
begin
  pDst := pDst + 2 * aDstWStep + 2;

  w := ((aW - 4) div 16) * 16;
  SetLength(buff, 25 * w);
  for I := 0 to High(q) do
    q[I] := @buff[I * w];

  for I := 0 to 4 do
    for J := 0 to 4 do
      p[5*I + J] := pSrc + I * aSrcWStep + J;

  pDstRow := pDst;
  for I := 0 to aH - 5 do begin
    SortPairs(p[0], p[1], q[0], q[1], w);
    SortPairs(p[2], p[3], q[2], q[3], w);
    SortPairs(p[4], p[5], q[4], q[5], w);
    SortPairs(p[6], p[7], q[6], q[7], w);
    SortPairs(p[8], p[9], q[8], q[9], w);
    SortPairs(p[10], p[11], q[10], q[11], w);
    SortPairs(p[12], p[13], q[12], q[13], w);
    SortPairs(p[14], p[15], q[14], q[15], w);
    SortPairs(p[16], p[17], q[16], q[17], w);
    SortPairs(p[18], p[19], q[18], q[19], w);
    SortPairs(p[20], p[21], q[20], q[21], w);
    SortPairs(p[22], p[23], q[22], q[23], w);

    SortPairs(q[0], q[2], w);
    SortPairs(q[1], q[3], w);
    SortPairs(q[4], q[6], w);
    SortPairs(q[5], q[7], w);
    SortPairs(q[8], q[10], w);
    SortPairs(q[9], q[11], w);
    SortPairs(q[12], q[14], w);
    SortPairs(q[13], q[15], w);
    SortPairs(q[16], q[18], w);
    SortPairs(q[17], q[19], w);
    SortPairs(q[21], q[22], w);
    SortPairs(q[23], p[24], q[23], q[24], w);

    SortPairs(q[0],  q[4], w);  SortPairs(q[1],  q[5], w);  SortPairs(q[2],  q[6], w);
    SortPairs(q[3],  q[7], w);  SortPairs(q[8],  q[12], w); SortPairs(q[9],  q[13], w);
    SortPairs(q[10], q[14], w); SortPairs(q[11], q[15], w); SortPairs(q[18], q[21], w);
    SortPairs(q[20], q[23], w); SortPairs(q[22], q[24], w);

    SortPairs(q[0],  q[8], w);  SortPairs(q[1],  q[9], w);  SortPairs(q[2],  q[10], w);
    SortPairs(q[3],  q[11], w); SortPairs(q[4],  q[12], w); SortPairs(q[5],  q[13], w);
    SortPairs(q[6],  q[14], w); SortPairs(q[7],  q[15], w); SortPairs(q[16], q[20], w);
    SortPairs(q[17], q[22], w); SortPairs(q[19], q[24], w); SortPairs(q[21], q[23], w);

    SortPairs(q[1],  q[18], w); SortPairs(q[3],  q[21], w); SortPairs(q[5],  q[23], w);
    SortPairs(q[6],  q[19], w); SortPairs(q[11], q[14], w); SortPairs(q[15], q[24], w);

    SortPairs(q[1],  q[16], w); SortPairs(q[3],  q[17], w); SortPairs(q[6],  q[9], w);
    SortPairs(q[7],  q[11], w); SortPairs(q[13], q[19], w); SortPairs(q[14], q[23], w);

    SortPairs(q[0],  q[1], w);  SortPairs(q[2],  q[16], w); SortPairs(q[3],  q[8], w);
    SortPairs(q[7],  q[20], w); SortPairs(q[10], q[13], w); SortPairs(q[11], q[22], w);
    SortPairs(q[15], q[23], w);

    SortPairs(q[1],  q[2], w);  SortPairs(q[5],  q[10], w); SortPairs(q[7],  q[18], w);
    SortPairs(q[11], q[21], w); SortPairs(q[15], q[20], w); SortPairs(q[19], q[22], w);

    SortPairs(q[4],  q[7], w);  SortPairs(q[5],  q[6], w);  SortPairs(q[9],  q[18], w);
    SortPairs(q[10], q[17], w); SortPairs(q[11], q[12], w); SortPairs(q[13], q[21], w);
    SortPairs(q[14], q[15], w); SortPairs(q[19], q[20], w); SortPairs(q[22], q[23], w);

    SortPairs(q[7],  q[8], w);  SortPairs(q[9],  q[10], w); SortPairs(q[11], q[16], w);
    SortPairs(q[12], q[17], w); SortPairs(q[13], q[18], w); SortPairs(q[19], q[21], w);
    SortPairs(q[20], q[22], w);

    SortPairs(q[5],  q[11], w); SortPairs(q[6],  q[16], w); SortPairs(q[7],  q[9], w);
    SortPairs(q[8],  q[10], w); SortPairs(q[12], q[13], w); SortPairs(q[14], q[19], w);
    SortPairs(q[15], q[18], w);

    SortPairs(q[6], q[9], w);   SortPairs(q[8],  q[11], w); SortPairs(q[10], q[16], w);
    SortPairs(q[12], q[14], w); SortPairs(q[15], q[17], w);

    SortPairs(q[9], q[11], w);  SortPairs(q[10], q[12], w); SortPairs(q[13], q[14], w);
    SortPairs(q[15], q[16], w);

    SortPairs(q[11], q[12], w);
    SortPairs(q[13], q[15], w);
    SortPairs(q[12], q[13], w);

    Move(q[12]^, pDstRow^, w);
    for J := 0 to High(p) do
      Inc(p[J], aSrcWStep);
    Inc(pDstRow, aDstWStep);
  end;

  if aW - 4 - w > 0 then begin
    pDst := pDst - 2 * aDstWStep + w - 2;
    Exec(pSrc + w - 2, pDst, aSrcWStep, aDstWStep, aW - w, aH);
  end;
end;

procedure TMedianFilter2DUI8.ExecMargins(pSrc, pDst: PByte; aSrcWStep, aDstWStep,
  aW, aH: NativeInt; aFlags: Integer);
begin
  if (aFlags and MARGIN_H) <> 0 then begin
    ExecHMargin(pSrc, pDst, aSrcWStep, aDstWStep, aW, aH, True);
    ExecHMargin(pSrc, pDst, aSrcWStep, aDstWStep, aW, aH, False);
  end;

  if (aFlags and MARGIN_V) <> 0 then begin
    ExecVMargin(pSrc, pDst, aSrcWStep, aDstWStep, aW, aH, True);
    ExecVMargin(pSrc, pDst, aSrcWStep, aDstWStep, aW, aH, False);
  end;

  if aFlags <> 0 then begin
    ExecCorner(pSrc, pDst, aSrcWStep, aDstWStep, aW, aH, True, True);
    ExecCorner(pSrc, pDst, aSrcWStep, aDstWStep, aW, aH, True, False);
    ExecCorner(pSrc, pDst, aSrcWStep, aDstWStep, aW, aH, False, True);
    ExecCorner(pSrc, pDst, aSrcWStep, aDstWStep, aW, aH, False, False);
  end;
end;

procedure TMedianFilter2DUI8.ExecHMargin(pSrc, pDst: PByte; aSrcWStep, aDstWStep, aW, aH: NativeInt; aTop: Boolean);
var H: THistUI8;
    mt: TMedianTrackerUI8;
    winSzX, winSzY, I, J: NativeInt;
    pOutRow: PByte;
    bForward: Boolean;
begin
  winSzX := 2*fHRadius + 1;
  winSzY := fVRadius + 1;
  pDst := pDst + fHRadius;
  FillChar(H, SizeOf(H), 0);
  if not aTop then begin
    pSrc := pSrc + (aH - 1) * aSrcWStep;
    pDst := pDst + (aH - 1) * aDstWStep;
    aSrcWStep := -aSrcWStep;
    aDstWStep := -aDstWStep;
  end;
  UpdateHist(H, pSrc, aSrcWStep, winSzX, winSzY);
  mt.Init(H);

  bForward := True;
  for I := 0 to fVRadius - 1 do begin
    pOutRow := pDst + I * aDstWStep;
    if bForward then begin
      for J := 0 to aW - winSzX - 1 do begin // J-index of the old column
        (pOutRow + J)^ := mt.Median;
        mt.Replace(pSrc + J, pSrc + J + winSzX, aSrcWStep, winSzY);
      end;
      J := aW - winSzX;
    end else begin
      for J :=  aW - winSzX - 1 downto 0 do begin // J-index of the new column
        (pOutRow + J + 1)^ := mt.Median;
        mt.Replace(pSrc + J + winSzX, pSrc + J, aSrcWStep, winSzY);
      end;
      J := 0;
    end;
    (pOutRow + J)^ := mt.Median;
    bForward := not bForward;
    //update the histogram on moving along the y axis
    mt.Add(pSrc + J + winSzY * aSrcWStep, 1, winSzX);
    Inc(winSzY);
  end;
end;

procedure TMedianFilter2DUI8.ExecVMargin(pSrc, pDst: PByte; aSrcWStep, aDstWStep, aW, aH: NativeInt; aLeft: Boolean);
var H: THistUI8;
    mt: TMedianTrackerUI8;
    winSzX, winSzY, I, J, xStep: NativeInt;
    pOutCol: PByte;
    bForward: Boolean;
begin
  winSzX := fHRadius + 1;
  winSzY := 2*fVRadius + 1;
  pDst := pDst + fVRadius * aDstWStep;
  FillChar(H, SizeOf(H), 0);
  if aLeft then begin
    UpdateHist(H, pSrc, aSrcWStep, winSzX, winSzY);
    xStep := 1;
  end else begin
    UpdateHist(H, pSrc + aW - winSzX, aSrcWStep, winSzX, winSzY);
    pSrc := pSrc + aW - 1;
    pDst := pDst + aW - 1;
    xStep := -1;
  end;
  mt.Init(H);

  bForward := True;
  for I := 0 to fHRadius - 1 do begin
    pOutCol := pDst + xStep * I;
    if bForward then begin
      for J := 0 to aH - winSzY - 1 do begin // J-index of the old row
        (pOutCol + J * aDstWStep)^ := mt.Median;
        mt.Replace(pSrc + J * aSrcWStep, pSrc + (J + winSzY) * aSrcWStep, xStep, winSzX);
      end;
      J := aH - winSzY;
    end else begin
      for J :=  aH - winSzY - 1 downto 0 do begin // J-index of the new row
        (pOutCol + (J + 1) * aDstWStep)^ := mt.Median;
        mt.Replace(pSrc + (J + winSzY) * aSrcWStep, pSrc + J * aSrcWStep, xStep, winSzX);
      end;
      J := 0;
    end;
    (pOutCol + J * aDstWStep)^ := mt.Median;
    bForward := not bForward;
    //update the histogram on moving along the x axis
    mt.Add(pSrc + J * aSrcWStep + xStep * winSzX, aSrcWStep, winSzY);
    Inc(winSzX);
  end;
end;

procedure TMedianFilter2DUI8.ExecCorner(pSrc, pDst: PByte; aSrcWStep, aDstWStep, aW, aH: NativeInt;
  aTop, aLeft: Boolean);
var H: THistUI8;
    mt: TMedianTrackerUI8;
    I, J, winSzX, winSzY, xStep: Integer;
    pOutRow: PByte;
    bForward: Boolean;
begin
  winSzX := fHRadius + 1;
  winSzY := fVRadius + 1;
  FillChar(H, SizeOf(H), 0);
  if not aTop then begin
    pSrc := pSrc + (aH - 1) * aSrcWStep;
    pDst := pDst + (aH - 1) * aDstWStep;
    aSrcWStep := -aSrcWStep;
    aDstWStep := -aDstWStep;
  end;
  if aLeft then begin
    UpdateHist(H, pSrc, aSrcWStep, winSzX, winSzY);
    xStep := 1;
  end else begin
    UpdateHist(H, pSrc + aW - winSzX, aSrcWStep, winSzX, winSzY);
    xStep := -1;
    pSrc := pSrc + aW - 1;
    pDst := pDst + aW - 1;
  end;
  mt.Init(H);

  bForward := True;
  for I := 0 to fVRadius - 1 do begin
    pOutRow := pDst + I * aDstWStep;
    if bForward then begin
      for J := 0 to fHRadius - 1 do begin
        (pOutRow + J * xStep)^ := mt.Median;
        mt.Add(pSrc + (J + fHRadius) * xStep, aSrcWStep, winSzY);
      end;
      J := fHRadius;
    end else begin
      for J :=  fHRadius - 1 downto 0 do begin
        (pOutRow + (J + 1) * xStep)^ := mt.Median;
        mt.Remove(pSrc + (J + fHRadius) * xStep, aSrcWStep, winSzY);
      end;
      J := 0;
    end;
    (pOutRow + J * xStep)^ := mt.Median;
    bForward := not bForward;
    mt.Add(pSrc + winSzY * aSrcWStep, xStep, J + fHRadius + 1);
    Inc(winSzY);
  end;
end;

procedure TMedianFilter2DUI8.Execute(pSrc, pDst: PByte; aSrcWStep, aDstWStep, aW, aH: NativeInt);
const
  V2_R_THRESHOLD = 4;
  V3_R_THRESHOLD = 3;
begin
  if (fHRadius = 1) and (fVRadius = 1) then begin
    Exec3x3(pSrc, pDst, aSrcWStep, aDstWStep, aW, aH);
    ExecMargins(pSrc, pDst, aSrcWStep, aDstWStep, aW, aH);
    exit;
  end;

  if (fHRadius = 2) and (fVRadius = 2) then begin
    Exec5x5(pSrc, pDst, aSrcWStep, aDstWStep, aW, aH);
    ExecMargins(pSrc, pDst, aSrcWStep, aDstWStep, aW, aH);
    exit;
  end;

  if
    (not CPU_X86_V2) or
    ((not CPU_X86_V3) and (fHRadius <= V3_R_THRESHOLD) and (fVRadius <= V3_R_THRESHOLD)) or
    ((fHRadius <= V2_R_THRESHOLD) and (fVRadius <= V2_R_THRESHOLD))
  then begin
    Exec(pSrc, pDst, aSrcWStep, aDstWStep, aW, aH);
    ExecMargins(pSrc, pDst, aSrcWStep, aDstWStep, aW, aH);
    exit;
  end;

  if (2*fHRadius + 1) * (2*fVRadius + 1) < High(UInt16) then
    ExecCH_H16(pSrc, pDst, aSrcWStep, aDstWStep, aW, aH)
  else
    ExecCH_H32(pSrc, pDst, aSrcWStep, aDstWStep, aW, aH);
  ExecMargins(pSrc, pDst, aSrcWStep, aDstWStep, aW, aH, MARGIN_H);
end;

{$endregion}

end.
