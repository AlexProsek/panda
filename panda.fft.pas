unit panda.fft;

interface

uses
    panda.Intfs
  , panda.Arrays
  , panda.ArrManip
  , panda.Nums
  , panda.Math
  , panda.Arithmetic
  , panda.IntFactor
  , panda.cvArithmetic
  , panda.DynArrayUtils
  , System.Math
  , System.SysUtils
  ;

{$I AsmDefs.inc}

type
  TVec<T> = record
  public type
    PT = ^T;
  public
    Data: PByte;
    Stride: NativeInt;
    Length: NativeInt;
    procedure Init(aData: PByte; aLength: NativeInt); overload;
    procedure Init(aData: PByte; aLength, aStride: NativeInt); overload;
    function GetItem(I: NativeInt): T; inline;
    procedure SetItem(I: NativeInt; const aValue: T); inline;
    function GetItemPtr(I: NativeInt): PByte; inline;
    function Span(aStartIdx, aLength: NativeInt; aStepMultiplier: NativeInt = 1): TVec<T>; inline;

    property Item[I: NativeInt]: T read GetItem write SetItem; default;
    property ItemPtr[I: NativeInt]: PByte read GetItemPtr;
  end;

  TVecC64 = TVec<TCmplx64>;
  TVecC128 = TVec<TCmplx128>;

  TIdxPair = record
    I1, I2: Integer;
  end;
  PIdxPair = ^TIdxPair;

  TFFTDirection = (fdForward, fdInverse);

  TFFTEvalBase = class abstract
  protected
    fN: NativeInt;
    fFactors: TArray<TFactorUI64>;
    fFactTop: Integer;
    fFactPwr: Integer;
    fBRPIdxs: TArray<TIdxPair>;
    fDLBlockSize: Integer;
    fDLThreshold: Integer;
    fInitialized: Boolean;
    procedure InitBuffers(N: NativeInt); virtual; abstract;
    procedure InitFactors(N: NativeInt); virtual; abstract;
    procedure InitFunctions; virtual; abstract;
  {$region 'Getters/Setters'}
    procedure SetRecursiveMethodThreshold(aValue: Integer);
  {$endregion}
  public
    procedure AfterConstruction; override;
    procedure Init(N: NativeInt); overload; virtual;

    property RecursiveMethodThreshold: Integer read fDLThreshold
      write SetRecursiveMethodThreshold;
  end;

  TFFTEvalBase<TF, TC> = class abstract(TFFTEvalBase)
  protected type
    PF = ^TF;
    PC = ^TC;
    TBRPFunc  = procedure (const aIdxs: TArray<TIdxPair>; const aData: TVec<TC>);
    TTFFunc1 = procedure (const aVec1: TVec<TC>; aSgn: Integer = 1);
    TVecFunc1 = procedure (const aVec1: TVec<TC>);
    TVecFunc2 = procedure (const aVec1, aVec2: TVec<TC>);
    TVecFunc3 = procedure (const aVec1, aVec2, aVec3: TVec<TC>);
  protected const
    cFSz = SizeOf(TF);
    cCSz = SizeOf(TC);
  protected
    fW, fBuff: INDArray<TC>;
    fWv, fBuffv: TVec<TC>;
    fW2: INDArray<TC>;
    fWv2: TVec<TC>;

    fFnc_EvalTwiddleFactors: TTFFunc1;
    fFnc_pack: TVecFunc2;
    fFnc_split2: TVecFunc2;
    fFnc_split4: TVecFunc2;
    fFnc_BRP: TBRPFunc;       // bit-reversal permutation
    fFnc_DLW: TVecFunc2;      // Danielson-Lanczos inplace method for 2^M sample
    fFnc_fftN3: TVecFunc1;
    fFnc_fftN4: TVecFunc1;
    fFnc_fftN5: TVecFunc1;
    fFnc_fftcomb2: TVecFunc2;
    fFnc_fftcomb3: TVecFunc2;
    fFnc_fftcomb4: TVecFunc2;
    fFnc_fftcomb5: TVecFunc2;

    procedure FFT(const aSrc, aDst: TVec<TC>);
    procedure FFT2(const aSrc, aDst: TVec<TC>);
    procedure FFT3(const aSrc, aDst: TVec<TC>);
    procedure FFT4(const aSrc, aDst: TVec<TC>);
    procedure FFT5(const aSrc, aDst: TVec<TC>);

    procedure InitBuffers(N: NativeInt); override;
    procedure InitFactors(N: NativeInt); override;
  end;

  TFFTEval<TF, TC> = class(TFFTEvalBase<TF, TC>)
  protected
    fDir: TFFTDirection;
    procedure InitBuffers(N: NativeInt); override;
  {$region 'Getters/Setters'}
    procedure SetDir(aValue: TFFTDirection);
  {$endregion}
  public
    procedure AfterConstruction; override;
    procedure Execute(const aSrc: INDArray<TC>; var aDst: INDArray<TC>);

    property Direction: TFFTDirection read fDir write SetDir;
  end;

  TRealFFTEvalBase<TF, TC> = class abstract(TFFTEvalBase<TF, TC>)
  protected
    fDir: TFFTDirection;
    procedure InitBuffers(N: NativeInt); override;
  public
    procedure Init(N: NativeInt); overload; override;
  end;

  TRealFFTEval<TF, TC> = class abstract(TRealFFTEvalBase<TF, TC>)
  protected type
    TFTRecombFnc =  procedure (const src, W, dst: TVec<TC>);
  protected
    fFullSpectrum: Boolean;
    fFnc_FTRecombFull: TFTRecombFnc;
    fFnc_FTRecombHalf: TFTRecombFnc;
  {$region 'Getters/Setters'}
    procedure SetFullSpectrum(aValue: Boolean);
    function GetResult: TVec<TC>;
  {$endregion}
  public
    procedure Init(N: NativeInt; aFullSpectrum: Boolean); overload;
    procedure Execute(const aSrc: INDArray<TF>; var aDst: INDArray<TC>); overload;
    // Do the evaluation and the result is left in the internal buffer.
    procedure Execute(const aSrc: INDArray<TF>); overload;

    property FullSpectrum: Boolean read fFullSpectrum write SetFullSpectrum;
    // Returns vector that points to the internal buffer. It can be useful
    // when only a part of the result is needed.
    property Result: TVec<TC> read GetResult;
  end;

  TRealIFFTEval<TF, TC> = class abstract(TRealFFTEvalBase<TF, TC>)
  protected type
    TFTReconstructionFnc =  procedure (const src, W, dst: TVec<TC>);
    TNormFnc = procedure (aSrc, aDst: PByte; aCount: NativeInt);
  protected
    fNormalize: Boolean;
    fFnc_Norm: TNormFnc;
    fFnc_FTReconstruct: TFTReconstructionFnc;
  {$region 'Getters/Setters'}
    function GetResult: TVec<TF>;
  {$endregion}
  public
    procedure AfterConstruction; override;
    procedure Execute(const aSrc: INDArray<TC>; var aDst: INDArray<TF>); overload;
    procedure Execute(const aSrc: INDArray<TC>); overload;

    // Returns vector that points to the internal buffer. It can be useful
    // when only a part of the result is needed.
    property Result: TVec<TF> read GetResult;
    property Normalize: Boolean read fNormalize write fNormalize;

  end;

  TRealFFTFilter<TF, TC> = class abstract
  public type
    TSpectrumFilter = procedure (const aSpectrum: INDArray<TC>) of object;
  protected
    fFilter: TSpectrumFilter;
    fFwdFFT: TRealFFTEval<TF, TC>;
    fInvFFT: TRealIFFTEval<TF, TC>;
  public
    procedure Execute(const aSrc: INDArray<TF>; var aDst: INDArray<TF>);

    property SpectrumFilter: TSpectrumFilter read fFilter write fFilter;
  end;

  // slNative - the final transposition is not executed, it safes some memory traffic.
  // It can be useful when some action is executed in frequency domain (i.e. lowpass filter)
  // and then an inverse transformation is executed. It saves two transpositions.
  T2DSpectrumLayout = (slNormal, slNative);

  TFFTEvalBase2D<TF, TC> = class abstract
  protected type
    TRowFFT = class(TFFTEval<TF, TC>)
    protected
      procedure InitFunctions; override;
    public
      procedure Evaluate(const aSrc: TVec<TC>); overload;
      procedure Evaluate(const aSrc, aDst: TVec<TC>); overload;

      property Buff: TVec<TC> read fBuffv;
    end;

    TTrFnc4 = procedure (pSrc, pDst: PByte; aSrcRCnt, aSrcCCnt, aSrcRStep, aDstRStep: NativeInt);
  protected const
    cCSz = SizeOf(TC);
  protected
    fNCols, fNRows: NativeInt;
    fRowFFT, fColFFT: TRowFFT;
    fSpectrumLayout: T2DSpectrumLayout;
    procedure InitFunctions; virtual; abstract;
  {$region 'Getters/Setters'}
    function GetRecursiveMethodThreshold: Integer;
    procedure SetRecursiveMethodThreshold(aValue: Integer);
  {$endregion}
  public
    constructor Create;
    destructor Destroy; override;
    procedure Init(aNRows, aNCols: NativeInt); overload; virtual;

    property RecursiveMethodThreshold: Integer read GetRecursiveMethodThreshold
      write SetRecursiveMethodThreshold;
    property SpectrumLayout: T2DSpectrumLayout read fSpectrumLayout write fSpectrumLayout;
  end;

  TRealFFTEval2D<TF, TC> = class abstract(TFFTEvalBase2D<TF, TC>)
  protected type
    TVecFunc2 = TFFTEvalBase<TF, TC>.TVecFunc2;
    TVecFunc3 = TFFTEvalBase<TF, TC>.TVecFunc3;
    TInterleaveRowsFunc = procedure(pRow1, pRow2, pDst: PByte; aCount: NativeInt);
  protected
    fFullSpectrum: Boolean;
    fBuff, fRowBuff: INDArray<TC>;
    fCBuff: INDArray<TC>;

    fFnc_RealRowPairFTRecomb: TVecFunc3;
    fFnc_InterleaveRows: TInterleaveRowsFunc;
    fFnc_Copy: TVecFunc2;
    fFnc_Tr4: TFFTEvalBase2D<TF, TC>.TTrFnc4;
  public
    procedure Init(aNRows, aNCols: NativeInt); overload; override;
    procedure Init(aNRows, aNCols: NativeInt; aFullSpectrum: Boolean); overload;
    procedure Execute(const aSrc: INDArray<TF>; var aDst: INDArray<TC>);
  end;

  TRealIFFTEval2D<TF, TC> = class abstract(TFFTEvalBase2D<TF, TC>)
  protected type
    TVecFunc2 = TFFTEvalBase<TF, TC>.TVecFunc2;
    TVecFunc3 = TFFTEvalBase<TF, TC>.TVecFunc3;
    TSplitRowsFunc = procedure(pSrc, pRow1, pRow2: PByte; aCount: NativeInt);
  protected
    fBuff, fRowBuff: INDArray<TC>;
    fCBuff: INDArray<TC>;

    fFnc_RealRowPairReconstruction: TVecFunc3;
    fFnc_SplitRows: TSplitRowsFunc;
    fFnc_Copy: TVecFunc2;
    fFnc_Tr4: TFFTEvalBase2D<TF, TC>.TTrFnc4;
    procedure ColFFTNative(const aSrc, aDst: INDArray<TC>);
  public
    procedure AfterConstruction; override;
    procedure Execute(const aSrc: INDArray<TC>; var aDst: INDArray<TF>);
  end;

  TRealFFTFilter2D<TF, TC> = class abstract
  public type
    TSpectrumFilter = procedure (const aSpectrum: INDArray<TC>) of object;
  protected
    fFilter: TSpectrumFilter;
    fFwdFFT: TRealFFTEval2D<TF, TC>;
    fInvFFT: TRealIFFTEval2D<TF, TC>;
  public
    procedure Execute(const aSrc: INDArray<TF>; var aDst: INDArray<TF>);

    property SpectrumFilter: TSpectrumFilter read fFilter write fFilter;
  end;

  TFFTEvalF32 = class(TFFTEval<Single, TCmplx64>)
  protected
    procedure InitFunctions; override;
  end;

  TRealFFTEvalF32 = class(TRealFFTEval<Single, TCmplx64>)
  protected
    procedure InitFunctions; override;
  end;

  TRealIFFTEvalF32 = class(TRealIFFTEval<Single, TCmplx64>)
  protected
    procedure InitFunctions; override;
  end;

  TRealFFTEval2DF32 = class(TRealFFTEval2D<Single, TCmplx64>)
  protected
    procedure InitFunctions; override;
  end;

  TFFTEvalF64 = class(TFFTEval<Double, TCmplx128>)
  protected
    procedure InitFunctions; override;
  end;

  TRealFFTEvalF64 = class(TRealFFTEval<Double, TCmplx128>)
  protected
    procedure InitFunctions; override;
  end;

  TRealIFFTEvalF64 = class(TRealIFFTEval<Double, TCmplx128>)
  protected
    procedure InitFunctions; override;
  end;

  TRealFFTEval2DF64 = class(TRealFFTEval2D<Double, TCmplx128>)
  protected
    procedure InitFunctions; override;
  end;

  TRealIFFTEval2DF64 = class(TRealIFFTEval2D<Double, TCmplx128>)
  protected
    procedure InitFunctions; override;
  end;


function PowerOfTwoQ(N: NativeInt): Boolean; inline;
function NearestLowerPowerOfTwo(N: NativeInt): NativeInt;
// BRPIndices() returns a list of indices with bit-reversal ordering.
function BRPIndices(N: Integer): TArray<TIdxPair>;
// Evaluates W[k] := Table[Exp[2*aSgn*Pi*I*k/N], {k, 0, N - 1}],
// aSgn should be 1 or -1.
procedure EvalTwiddleFactors(const aW: TVecC64; aSgn: Integer = 1); overload;
procedure EvalTwiddleFactors(const aW: TVecC128; aSgn: Integer = 1); overload;
function GetHammingNumbers(aMaxValue: NativeInt): TArray<NativeInt>;

//Danielson-Lanczos routine
// This algorithm is taken from "Numeric recipes in C but
// a twiddle factors table is used instead of the trigonometric recurrence
procedure DLW(const aData, aW: TVecC64); overload;
procedure DLW(const aData, aW: TVecC128); overload;

{$region 'low-level functions'}

procedure _perm(const aIdxs: TArray<TIdxPair>; const aData: TVecC64); overload;
procedure _perm(const aIdxs: TArray<TIdxPair>; const aData: TVecC128); overload;

procedure _fft3(const data: TVecC64); overload;
procedure _fft3(const data: TVecC128); overload;

procedure _ifft3(const data: TVecC64); overload;
procedure _ifft3(const data: TVecC128); overload;

procedure _fft4(const data: TVecC64); overload;
procedure _fft4(const data: TVecC128); overload;

procedure _ifft4(const data: TVecC64); overload;
procedure _ifft4(const data: TVecC128); overload;

procedure _fft5(const data: TVecC64); overload;
procedure _fft5(const data: TVecC128); overload;

procedure _ifft5(const data: TVecC64); overload;
procedure _ifft5(const data: TVecC128); overload;

{$endregion}

type
  EFFTError = class(Exception);
  EFFTSizeError = class(EFFTError);

const
  cPositiveSizeErrorMsg = 'Data size must be greater than zero.';
  cEvenSizeErrorMsg = 'Real FFT is implemented only for even sample size.';
  cInvSizeErrorMsg = 'Output buffer size is %d but %d is expected.';

type
  TFFTProps = record
  private
    fRMThreshold: Integer;
  {$region 'Getters/Setters'}
    procedure SetRMThreshold(aValue: Integer);
  {$endregion}
  public
    property RecursiveMethodThreshold: Integer read fRMThreshold write SetRMThreshold;
  end;

var
  g_FFTProps: TFFTProps;



implementation

{$EXCESSPRECISION OFF} // to prevent Single -> Double conversion by x64 compiler

const cBitRevTable256: array [0..255] of Byte =
(
  $00, $80, $40, $C0, $20, $A0, $60, $E0, $10, $90, $50, $D0, $30, $B0, $70, $F0,
  $08, $88, $48, $C8, $28, $A8, $68, $E8, $18, $98, $58, $D8, $38, $B8, $78, $F8,
  $04, $84, $44, $C4, $24, $A4, $64, $E4, $14, $94, $54, $D4, $34, $B4, $74, $F4,
  $0C, $8C, $4C, $CC, $2C, $AC, $6C, $EC, $1C, $9C, $5C, $DC, $3C, $BC, $7C, $FC,
  $02, $82, $42, $C2, $22, $A2, $62, $E2, $12, $92, $52, $D2, $32, $B2, $72, $F2,
  $0A, $8A, $4A, $CA, $2A, $AA, $6A, $EA, $1A, $9A, $5A, $DA, $3A, $BA, $7A, $FA,
  $06, $86, $46, $C6, $26, $A6, $66, $E6, $16, $96, $56, $D6, $36, $B6, $76, $F6,
  $0E, $8E, $4E, $CE, $2E, $AE, $6E, $EE, $1E, $9E, $5E, $DE, $3E, $BE, $7E, $FE,
  $01, $81, $41, $C1, $21, $A1, $61, $E1, $11, $91, $51, $D1, $31, $B1, $71, $F1,
  $09, $89, $49, $C9, $29, $A9, $69, $E9, $19, $99, $59, $D9, $39, $B9, $79, $F9,
  $05, $85, $45, $C5, $25, $A5, $65, $E5, $15, $95, $55, $D5, $35, $B5, $75, $F5,
  $0D, $8D, $4D, $CD, $2D, $AD, $6D, $ED, $1D, $9D, $5D, $DD, $3D, $BD, $7D, $FD,
  $03, $83, $43, $C3, $23, $A3, $63, $E3, $13, $93, $53, $D3, $33, $B3, $73, $F3,
  $0B, $8B, $4B, $CB, $2B, $AB, $6B, $EB, $1B, $9B, $5B, $DB, $3B, $BB, $7B, $FB,
  $07, $87, $47, $C7, $27, $A7, $67, $E7, $17, $97, $57, $D7, $37, $B7, $77, $F7,
  $0F, $8F, $4F, $CF, $2F, $AF, $6F, $EF, $1F, $9F, $5F, $DF, $3F, $BF, $7F, $FF
);

function _BitRevInt32(aValue: Integer): Integer; inline;
begin
  Result :=
    (cBitRevTable256[aValue and $ff] shl 24) or
    (cBitRevTable256[(aValue shr 8) and $ff] shl 16) or
    (cBitRevTable256[(aValue shr 16) and $ff] shl 8) or
    (cBitRevTable256[(aValue shr 24) and $ff]);
end;

function PowerOfTwoQ(N: NativeInt): Boolean;
begin
  Result := (N > 0) and ((N and (N - 1)) = 0);
end;

function NearestLowerPowerOfTwo(N: NativeInt): NativeInt;
{$if defined(ASMx64)}
// RCX <- N
asm
  bsr rcx, rcx
  mov rax, 1
  shl rax, cl
end;
{$else}
begin
  Assert(N > 0);

  Result := 1;
  while Result <= N do
    Result := Result shl 1;
  Result := Result shr 1;
end;
{$endif}

function BRPIndices(N: Integer): TArray<TIdxPair>;
var I, J, offset, top: Integer;
begin
  Assert(PowerOfTwoQ(N));

  offset := 32 - Round(Log2(N));
  SetLength(Result, N);
  top := 0;
  for I := 0 to N - 1 do begin
    J := _BitRevInt32(I) shr offset;
    if I < J then begin
      with Result[top] do begin
        I1 := I;
        I2 := J;
      end;
      Inc(top);
    end;
  end;
  SetLength(Result, top);
end;

procedure EvalTwiddleFactors(const aW: TVecC64; aSgn: Integer);
var N, N2, N4, k: NativeInt;
    wr, wi, wrp, a, b: Double;
    pW, pWj, pWk: PCmplx64;
begin
  Assert((aW.Stride = cC64Sz) and (Abs(aSgn) = 1));

  N := aW.Length;
  pW := PCmplx64(aW.Data);
  case N of
    1: begin
      pW^ := 1;
      exit;
    end;
    2: begin
      pW^ := 1;
      Inc(pW);
      pW^ := -1;
      exit;
    end;
    3: begin
      pW^ := 1;
      Inc(pW);
      pW^.Init(-0.5, aSgn*Sqrt(3)/2);
      Inc(pW);
      pW^.Init(-0.5, -aSgn*Sqrt(3)/2);
      exit;
    end;
    4: begin
      pW^.Init(1, 0);
      Inc(pW);
      pW^.Init(0, aSgn);
      Inc(pW);
      pW^.Init(-1, 0);
      Inc(pW);
      pW^.Init(0, -aSgn);
      exit;
    end;
  end;

  N2 := N - (N shr 1);
  N4 := N shr 2;
  pWk := pW;
  if (N and 1) = 1 then begin
    wr := 1; wi := 0;
    a := Sin(Pi*aSgn/N);
    a := 2*a*a;
    b := Sin(2*Pi*aSgn/N);
    for k := 0 to N4 do begin
      pWk^.Init(wr, wi);
      wrp := wr;
      wr := wr - (wrp*a + wi*b);
      wi := wi - (wi*a - wrp*b);
      Inc(pWk);
    end;

    SinCos(2*Pi*aSgn*(N4 + 1)/N, wi, wr);
    for k := N4 + 1 to N2 - 1 do begin
      pWk^.Init(wr, wi);
      wrp := wr;
      wr := wr - (wrp*a + wi*b);
      wi := wi - (wi*a - wrp*b);
      Inc(pWk);
    end;
  end else begin // (N and 1) = 0
    wr := 1; wi := 0;
    a := Sin(Pi*aSgn/N);
    a := 2*a*a;
    b := Sin(2*Pi*aSgn/N);
    for k := 0 to N4 do begin
      pWk^.Init(wr, wi);
      wrp := wr;
      wr := wr - (wrp*a + wi*b);
      wi := wi - (wi*a - wrp*b);
      Inc(pWk);
    end;

    pWj := pWk;
    Dec(pWj, 2 - (N2 and 1));
    for k := N4 + 1 to N2 - 1 do begin
      with pWj^ do
        pWk^.Init(-Re, Im);
      Dec(pWj);
      Inc(pWk);
    end;
  end;

  pWj := pW;
  Inc(pWj, N2 - 1);
  pWk := pW;
  Inc(pWk, N2);
  if (N and 1) = 0 then begin
    pWk^.Init(-1, 0);
    Inc(pWk);
  end;

  while PByte(pWj) > PByte(pW) do begin
    with pWj^ do
      pWk^.Init(Re, -Im);
    Inc(pWk);
    Dec(pWj);
  end;
end;

procedure EvalTwiddleFactors(const aW: TVecC128; aSgn: Integer);
var N, N2, N4, k: NativeInt;
    wr, wi, wrp, a, b: Double;
    pW, pWj, pWk: PCmplx128;
begin
  Assert((aW.Stride = cC128Sz) and (Abs(aSgn) = 1));

  N := aW.Length;
  pW := PCmplx128(aW.Data);
  case N of
    1: begin
      pW^ := 1;
      exit;
    end;
    2: begin
      pW^ := 1;
      Inc(pW);
      pW^ := -1;
      exit;
    end;
    3: begin
      pW^ := 1;
      Inc(pW);
      pW^.Init(-0.5, aSgn*Sqrt(3)/2);
      Inc(pW);
      pW^.Init(-0.5, -aSgn*Sqrt(3)/2);
      exit;
    end;
    4: begin
      pW^.Init(1, 0);
      Inc(pW);
      pW^.Init(0, aSgn);
      Inc(pW);
      pW^.Init(-1, 0);
      Inc(pW);
      pW^.Init(0, -aSgn);
      exit;
    end;
  end;

  N2 := N - (N shr 1);
  N4 := N shr 2;
  pWk := pW;
  if (N and 1) = 1 then begin
    wr := 1; wi := 0;
    a := Sin(Pi*aSgn/N);
    a := 2*a*a;
    b := Sin(2*Pi*aSgn/N);
    for k := 0 to N4 do begin
      pWk^.Init(wr, wi);
      wrp := wr;
      wr := wr - (wrp*a + wi*b);
      wi := wi - (wi*a - wrp*b);
      Inc(pWk);
    end;

    SinCos(2*Pi*aSgn*(N4 + 1)/N, wi, wr);
    for k := N4 + 1 to N2 - 1 do begin
      pWk^.Init(wr, wi);
      wrp := wr;
      wr := wr - (wrp*a + wi*b);
      wi := wi - (wi*a - wrp*b);
      Inc(pWk);
    end;
  end else begin // (N and 1) = 0
    wr := 1; wi := 0;
    a := Sin(Pi*aSgn/N);
    a := 2*a*a;
    b := Sin(2*Pi*aSgn/N);
    for k := 0 to N4 do begin
      pWk^.Init(wr, wi);
      wrp := wr;
      wr := wr - (wrp*a + wi*b);
      wi := wi - (wi*a - wrp*b);
      Inc(pWk);
    end;

    pWj := pWk;
    Dec(pWj, 2 - (N2 and 1));
    for k := N4 + 1 to N2 - 1 do begin
      with pWj^ do
        pWk^.Init(-Re, Im);
      Dec(pWj);
      Inc(pWk);
    end;
  end;

  pWj := pW;
  Inc(pWj, N2 - 1);
  pWk := pW;
  Inc(pWk, N2);
  if (N and 1) = 0 then begin
    pWk^.Init(-1, 0);
    Inc(pWk);
  end;

  while PByte(pWj) > PByte(pW) do begin
    with pWj^ do
      pWk^.Init(Re, -Im);
    Inc(pWk);
    Dec(pWj);
  end;
end;

function GetHammingNumbers(aMaxValue: NativeInt): TArray<NativeInt>;
var h: TArray<NativeInt>;
    i2, i3, i5, n2, n3, n5, next, count: NativeInt;
begin
  SetLength(h, 128);
  h[0] := 1;
  i2 := 0;
  i3 := 0;
  i5 := 0;
  count := 1;
  next := 2;
  while next < aMaxValue do begin
    n2 := h[i2] * 2;
    n3 := h[i3] * 3;
    n5 := h[i5] * 5;
    next := n2;
    if n3 < next then next := n3;
    if n5 < next then next := n5;

    h[count] := next;
    Inc(count);
    if count > High(h) then
      SetLength(h, 2 * Length(h));

    if n2 = next then Inc(i2);
    if n3 = next then Inc(i3);
    if n5 = next then Inc(i5);    
  end;

  SetLength(h, count);
end;

{$region 'TVec<T>'}

procedure TVec<T>.Init(aData: PByte; aLength: NativeInt);
begin
  Data := aData;
  Stride := SizeOf(T);
  Length := aLength;
end;

procedure TVec<T>.Init(aData: PByte; aLength, aStride: NativeInt);
begin
  Data := aData;
  Stride := aStride;
  Length := aLength;
end;

function TVec<T>.GetItem(I: NativeInt): T;
begin
  Result := PT(Data + I * Stride)^;
end;

procedure TVec<T>.SetItem(I: NativeInt; const aValue: T);
begin
  PT(Data + I * Stride)^ := aValue;
end;

function TVec<T>.GetItemPtr(I: NativeInt): PByte;
begin
  Result := Data + I * Stride;
end;

function TVec<T>.Span(aStartIdx, aLength: NativeInt; aStepMultiplier: NativeInt): TVec<T>;
begin
  Result.Data := Data + aStartIdx * Stride;
  Result.Stride := aStepMultiplier * Stride;
  Result.Length := aLength;
end;

{$endregion}

const
  cImSgnMaskF64: array [0..1] of UInt64 = (0, $8000000000000000);
  cReSgnMaskF64: array [0..1] of UInt64 = ($8000000000000000, 0);
  cImSgnMaskF32: array [0..3] of UInt32 = (0, $80000000, 0, $80000000);
  cReSgnMaskF32: array [0..3] of UInt32 = ($80000000, 0, $80000000, 0);
  cLoC64SgnMask: array [0..3] of UInt32 = ($80000000, $80000000, 0, 0);
  cHiC64SgnMask: array [0..3] of UInt32 = (0, 0, $80000000, $80000000);
  I: TCmplx128 = (Re: 0.0; Im: 1.0);

procedure _pack(const aSrc, aDst: TVecC64); overload;
{$if defined(ASMx64)}
// RCX <- @aSrc, @RDX <- aDst
asm
  mov r9, [rdx]           // R9 <- aDst.Data
  mov r8, [rcx + 16]      // R8 <- aSrc.Length
  mov rdx, [rcx + 8]      // RDX <- aSrc.Stride
  mov rcx, [rcx]          // RCX <- aSrc.Data
  mov r10, r8
  shr r8, 1
  jz @rest
@L:
  movq xmm0, [rcx]
  movq [r9], xmm0
  movq xmm1, [rcx + rdx]
  movq [r9 + 8], xmm1
  lea rcx, rcx + 2*rdx
  add r9, 16
  dec r8
  jnz @L
@rest:
  and r10, 1
  jz @end
  movupd xmm0, [rcx]
  movupd [r9], xmm0
@end:
end;
{$else}
var pSrc, pDst, pEnd: PByte;
    srcStep: NativeInt;
begin
  Assert((aDst.Length >= aSrc.Length) and (aDst.Stride = cC64Sz));
  pSrc := aSrc.Data;
  pDst := aDst.Data;
  pEnd := pDst + aDst.Length * cC64Sz;
  srcStep := aSrc.Stride;
  while pDst < pEnd do begin
    PCmplx64(pDst)^ := PCmplx64(pSrc)^;
    Inc(pSrc, srcStep);
    Inc(pDst, cC64Sz);
  end;
end;
{$endif}

procedure _pack(const aSrc, aDst: TVecC128); overload;
{$if defined(ASMx86)}
// EAX <- @aSrc, EDX <- @aDst
asm
  push esi
  push edi
  mov esi, [eax]      // ESI <- aSrc
  mov edi, [edx]      // EDI <- aRes
  mov ecx, [eax + 8]  // ECX <- aSrc.Length
  mov edx, [eax + 4]  // EDX <- aSrc.Stride
  mov eax, ecx        // EAX <- aSrc.Length
  shr ecx, 1
  jz @rest
@L:
  movupd xmm0, [esi]
  movupd [edi], xmm0
  add esi, edx
  add edi, 16

  movupd xmm0, [esi]
  movupd [edi], xmm0
  add esi, edx
  add edi, 16

  dec ecx
  jnz @L
@rest:
  and eax, 1
  jz @end
  movupd xmm0, [esi]
  movupd [edi], xmm0
@end:
  pop edi
  pop esi
end;
{$elseif defined(ASMx64)}
// RCX <- @aSrc, @RDX <- aDst
asm
  mov r9, [rdx]           // R9 <- aDst.Data
  mov r8, [rcx + 16]      // R8 <- aSrc.Length
  mov rdx, [rcx + 8]      // RDX <- aSrc.Stride
  mov rcx, [rcx]          // RCX <- aSrc.Data
  mov r10, r8
  shr r8, 1
  jz @rest
@L:
  movupd xmm0, [rcx]
  movupd [r9], xmm0
  add rcx, rdx
  add r9, 16

  movupd xmm0, [rcx]
  movupd [r9], xmm0
  add rcx, rdx
  add r9, 16

  dec r8
  jnz @L
@rest:
  and r10, 1
  jz @end
  movupd xmm0, [rcx]
  movupd [r9], xmm0
@end:
end;
{$else}
var pSrc, pDst, pEnd: PByte;
    srcStep: NativeInt;
begin
  Assert((aDst.Length >= aSrc.Length) and (aDst.Stride = cC128Sz));
  pSrc := aSrc.Data;
  pDst := aDst.Data;
  pEnd := pDst + aDst.Length * cC128Sz;
  srcStep := aSrc.Stride;
  while pDst < pEnd do begin
    PCmplx128(pDst)^ := PCmplx128(pSrc)^;
    Inc(pSrc, srcStep);
    Inc(pDst, cC128Sz);
  end;
end;
{$endif}

procedure _split2C64(const aSrc, aDst: TVecC64);
{$if defined(ASMx64)}
// RCX <- @aSrc, RDX <- @aDst
asm
  push rsi
  push rdi
  push rbx

  mov rdi, rdx            // RDI <- @aDst
  mov rax, [rcx + 16]     // RAX <- N
  shr rax, 1              // RAX <- N/2
  mul rax, [rdx + 8]      // RAX <- N/2 * aDst.Stride
  mov rbx, rax
  mov rdx, rdi            // RDX <- @aDst

  mov rsi, [rcx]          // RSI <- aSrc.Data
  mov rdi, [rdx]          // RDI <- aDst.Data
  mov rax, [rcx + 8]      // RAX <- aSrc.Stride
  mov rdx, [rdx + 8]      // RDX <- aDst.Stride
  mov rcx, [rcx + 16]     // RCX <- N := aSrc.Length
  shr rcx, 1              // RCX <- N4 := N/2

  lea r8, rdi + rbx       // R8  <- aDst.Data + N2 * aDst.Stride
@L:
  movq xmm0, [rsi]
  movq [rdi], xmm0
  movq xmm1, [rsi + rax]
  movq [r8], xmm1
  lea rsi, rsi + 2*rax
  add rdi, rdx
  add r8, rdx
  dec rcx
  jnz @L

  pop rbx
  pop rdi
  pop rsi
end;
{$else}
var N, srcStep, dstStep: NativeInt;
    pSrc, pDst0, pDst1: PByte;
    pEnd: PByte;
begin
  N := aSrc.Length;
  srcStep := aSrc.Stride;
  dstStep := aDst.Stride;
  pSrc := aSrc.Data;
  pEnd := pSrc + srcStep * N;
  pDst0 := aDst.Data;
  pDst1 := pDst0 + dstStep * (N shr 1);
  while pSrc < pEnd do begin
    PCmplx64(pDst0)^ := PCmplx64(pSrc)^;
    Inc(pDst0, dstStep);
    Inc(pSrc, srcStep);
    PCmplx64(pDst1)^ := PCmplx64(pSrc)^;
    Inc(pDst1, dstStep);
    Inc(pSrc, srcStep);
  end;
end;
{$endif}

procedure _split2C128(const aSrc, aDst: TVecC128);
{$if defined(ASMx64)}
// RCX <- aSrc, RDX <- aDst
asm
  push rsi
  push rdi
  push rbx

  mov rdi, rdx            // RDI <- @aDst
  mov rax, [rcx + 16]     // RAX <- N
  shr rax, 1              // RAX <- N/2
  mul rax, [rdx + 8]      // RAX <- N/2 * aDst.Stride
  mov rbx, rax
  mov rdx, rdi            // RDX <- @aDst

  mov rsi, [rcx]          // RSI <- aSrc.Data
  mov rdi, [rdx]          // RDI <- aDst.Data
  mov rax, [rcx + 8]      // RAX <- aSrc.Stride
  mov rdx, [rdx + 8]      // RDX <- aDst.Stride
  mov rcx, [rcx + 16]     // RCX <- N := aSrc.Length
  shr rcx, 1              // RCX <- N4 := N/2

  lea r8, rdi + rbx       // R8  <- aDst.Data + N2 * aDst.Stride
@L:
  movupd xmm0, [rsi]
  movupd [rdi], xmm0
  movupd xmm1, [rsi + rax]
  movupd [r8], xmm1
  lea rsi, rsi + 2*rax
  add rdi, rdx
  add r8, rdx
  dec rcx
  jnz @L

  pop rbx
  pop rdi
  pop rsi
end;
{$else}
var N, srcStep, dstStep: NativeInt;
    pSrc, pDst0, pDst1: PByte;
    pEnd: PByte;
begin
  N := aSrc.Length;
  srcStep := aSrc.Stride;
  dstStep := aDst.Stride;
  pSrc := aSrc.Data;
  pEnd := pSrc + srcStep * N;
  pDst0 := aDst.Data;
  pDst1 := pDst0 + dstStep * (N shr 1);
  while pSrc < pEnd do begin
    PCmplx128(pDst0)^ := PCmplx128(pSrc)^;
    Inc(pDst0, dstStep);
    Inc(pSrc, srcStep);
    PCmplx128(pDst1)^ := PCmplx128(pSrc)^;
    Inc(pDst1, dstStep);
    Inc(pSrc, srcStep);
  end;
end;
{$endif}

procedure _split4C64(const aSrc, aDst: TVecC64);
{$if defined(ASMx64)}
// RCX <- @aSrc, RDX <- @aDst
asm
  push rsi
  push rdi
  push rbx

  mov rdi, rdx            // RDI <- @aDst
  mov rax, [rcx + 16]     // RAX <- N
  shr rax, 2              // RAX <- N/4
  mul rax, [rdx + 8]      // RAX <- N/4 * aDst.Stride
  mov rbx, rax
  mov rdx, rdi            // RDX <- @aDst

  mov rsi, [rcx]          // RSI <- aSrc.Data
  mov rdi, [rdx]          // RDI <- aDst.Data
  mov rax, [rcx + 8]      // RAX <- aSrc.Stride
  mov rdx, [rdx + 8]      // RDX <- aDst.Stride
  mov rcx, [rcx + 16]     // RCX <- N := aSrc.Length
  shr rcx, 2              // RCX <- N4 := N/4

  lea r8, rdi + rbx       // R8  <- aDst.Data + N4 * aDst.Stride
  lea r9, r8 + rbx        // R9  <- aDst.Data + 2* N4 * aDst.Stride
  lea r10, r9 + rbx       // R10 <- aDst.Data + 3* N4 * aDst.Stride
@L:
  lea r11, rsi + 2*rax
  movq xmm0, [rsi]
  movq [rdi], xmm0
  movq xmm1, [rsi + rax]
  movq [r8], xmm1
  movq xmm2, [r11]
  movq [r9], xmm2
  movq xmm3, [r11 + rax]
  movq [r10], xmm3
  lea rsi, rsi + 4*rax
  add rdi, rdx
  add r8, rdx
  add r9, rdx
  add r10, rdx
  dec rcx
  jnz @L

  pop rbx
  pop rdi
  pop rsi
end;
{$else}
var N, N4, srcStep, dstStep: NativeInt;
    pSrc, pDst0, pDst1, pDst2, pDst3: PByte;
    pEnd: PByte;
begin
  N := aSrc.Length;
  N4 := N shr 2;
  srcStep := aSrc.Stride;
  dstStep := aDst.Stride;
  pSrc := aSrc.Data;
  pEnd := pSrc + srcStep * N;
  pDst0 := aDst.Data;
  pDst1 := pDst0 + 1 * N4 * dstStep;
  pDst2 := pDst0 + 2 * N4 * dstStep;
  pDst3 := pDst0 + 3 * N4 * dstStep;
  while pSrc < pEnd do begin
    PCmplx64(pDst0)^ := PCmplx64(pSrc)^;
    Inc(pDst0, dstStep);
    Inc(pSrc, srcStep);
    PCmplx64(pDst1)^ := PCmplx64(pSrc)^;
    Inc(pDst1, dstStep);
    Inc(pSrc, srcStep);
    PCmplx64(pDst2)^ := PCmplx64(pSrc)^;
    Inc(pDst2, dstStep);
    Inc(pSrc, srcStep);
    PCmplx64(pDst3)^ := PCmplx64(pSrc)^;
    Inc(pDst3, dstStep);
    Inc(pSrc, srcStep);
  end;
end;
{$endif}

procedure _split4C128(const aSrc, aDst: TVecC128);
{$if defined(ASMx64)}
// RCX <- @aSrc, RDX <- @aDst
asm
  push rsi
  push rdi
  push rbx

  mov rdi, rdx            // RDI <- @aDst
  mov rax, [rcx + 16]     // RAX <- N
  shr rax, 2              // RAX <- N/4
  mul rax, [rdx + 8]      // RAX <- N/4 * aDst.Stride
  mov rbx, rax
  mov rdx, rdi            // RDX <- @aDst

  mov rsi, [rcx]          // RSI <- aSrc.Data
  mov rdi, [rdx]          // RDI <- aDst.Data
  mov rax, [rcx + 8]      // RAX <- aSrc.Stride
  mov rdx, [rdx + 8]      // RDX <- aDst.Stride
  mov rcx, [rcx + 16]     // RCX <- N := aSrc.Length
  shr rcx, 2              // RCX <- N4 := N/4

  lea r8, rdi + rbx       // R8  <- aDst.Data + N4 * aDst.Stride
  lea r9, r8 + rbx        // R9  <- aDst.Data + 2* N4 * aDst.Stride
  lea r10, r9 + rbx       // R10 <- aDst.Data + 3* N4 * aDst.Stride
@L:
  lea r11, rsi + 2*rax
  movupd xmm0, [rsi]
  movupd [rdi], xmm0
  movupd xmm1, [rsi + rax]
  movupd [r8], xmm1
  movupd xmm2, [r11]
  movupd [r9], xmm2
  movupd xmm3, [r11 + rax]
  movupd [r10], xmm3
  lea rsi, rsi + 4*rax
  add rdi, rdx
  add r8, rdx
  add r9, rdx
  add r10, rdx
  dec rcx
  jnz @L

  pop rbx
  pop rdi
  pop rsi
end;
{$else}
var N, N4, srcStep, dstStep: NativeInt;
    pSrc, pDst0, pDst1, pDst2, pDst3: PByte;
    pEnd: PByte;
begin
  N := aSrc.Length;
  N4 := N shr 2;
  srcStep := aSrc.Stride;
  dstStep := aDst.Stride;
  pSrc := aSrc.Data;
  pEnd := pSrc + srcStep * N;
  pDst0 := aDst.Data;
  pDst1 := pDst0 + 1 * N4 * dstStep;
  pDst2 := pDst0 + 2 * N4 * dstStep;
  pDst3 := pDst0 + 3 * N4 * dstStep;
  while pSrc < pEnd do begin
    PCmplx128(pDst0)^ := PCmplx128(pSrc)^;
    Inc(pDst0, dstStep);
    Inc(pSrc, srcStep);
    PCmplx128(pDst1)^ := PCmplx128(pSrc)^;
    Inc(pDst1, dstStep);
    Inc(pSrc, srcStep);
    PCmplx128(pDst2)^ := PCmplx128(pSrc)^;
    Inc(pDst2, dstStep);
    Inc(pSrc, srcStep);
    PCmplx128(pDst3)^ := PCmplx128(pSrc)^;
    Inc(pDst3, dstStep);
    Inc(pSrc, srcStep);
  end;
end;
{$endif}

procedure _perm(const aIdxs: TArray<TIdxPair>; const aData: TVecC64);
{$if defined(ASMx64)}
// RCX <- @aIdxs, RDX <- @aData
asm
  mov rax, rcx        // RAX <- @fIdxs[0]
  mov rdx, [rdx]       // RDX <- @aData[0]
  test rax, rax
  jz @end
  mov rcx, [rax - 8]   // RCX <- Length(fIdxs)
  xor r8, r8
  xor r9, r9
@L:
  mov r8d, [rax]
  mov r9d, [rax + 4]
  lea r10, rdx + 8*r8
  lea r11, rdx + 8*r9
  movq xmm0, [r10]
  movq xmm1, [r11]
  movq [r10], xmm1
  movq [r11], xmm0
  add rax, 8
  dec rcx
  jnz @L
@end:
end;
{$else}
var I: Integer;
    p1, p2: PCmplx64;
    tmp: TCmplx64;
    p0: PByte;
begin
  p0 := aData.Data;
  for I := 0 to High(aIdxs) do
    with aIdxs[I] do begin
      p1 := PCmplx64(p0 + I1 * cC64Sz);
      p2 := PCmplx64(p0 + I2 * cC64Sz);
      tmp := p1^;
      p1^ := p2^;
      p2^ := tmp;
    end;
end;
{$endif}

procedure _perm(const aIdxs: TArray<TIdxPair>; const aData: TVecC128);
{$if defined(ASMx86)}
// EAX <- @aIdxs, EDX <- aData
asm
  push esi
  push edi
  mov edx, [edx]        // EDX <- @aData[0]
  test eax, eax
  jz @end
  mov ecx, [eax - 4]    // ECX <- Length(fIdxs)
@L:
  mov esi, [eax]
  mov edi, [eax + 4]
  shl esi, 4
  shl edi, 4
  add esi, edx
  add edi, edx
  movupd xmm0, [esi]
  movupd xmm1, [edi]
  movupd [esi], xmm1
  movupd [edi], xmm0
  add eax, 8
  dec ecx
  jnz @L
@end:
  pop edi
  pop esi
end;
{$elseif defined(ASMx64)}
// RCX <- @aIdxs, RDX <- @aData
asm
  mov rax, rcx          // RAX <- @fIdxs[0]
  mov rdx, [rdx]        // RDX <- @aData[0]
  test rax, rax
  jz @end
  mov rcx, [rax - 8]    // RCX <- Length(fIdxs)
  xor r8, r8
  xor r9, r9
@L:
  mov r8d, [rax]
  mov r9d, [rax + 4]
  shl r8, 4
  shl r9, 4
  lea r10, rdx + r8
  lea r11, rdx + r9
  movupd xmm0, [r10]
  movupd xmm1, [r11]
  movupd [r10], xmm1
  movupd [r11], xmm0
  add rax, 8
  dec rcx
  jnz @L
@end:
end;
{$else}
var I: Integer;
    p1, p2: PCmplx128;
    tmp: TCmplx128;
    p0: PByte;
begin
  p0 := aData.Data;
  for I := 0 to High(aIdxs) do
    with aIdxs[I] do begin
      p1 := PCmplx128(p0 + I1 * cC128Sz);
      p2 := PCmplx128(p0 + I2 * cC128Sz);
      tmp := p1^;
      p1^ := p2^;
      p2^ := tmp;
    end;
end;
{$endif}

procedure _copyC64(const aSrc, aDst: TVecC64);
var pSrc, pDst, pEnd: PByte;
begin
  Assert((aSrc.Length = aDst.Length) and (aSrc.Length > 0));
  pSrc := aSrc.Data;
  pDst := aDst.Data;
  pEnd := aSrc.Data + aSrc.Stride * aSrc.Length;
  while pSrc < pEnd do begin
    PCmplx64(pDst)^ := PCmplx64(pSrc)^;
    Inc(pSrc, aSrc.Stride);
    Inc(pDst, aDst.Stride);
  end;
end;

procedure _copyC128(const aSrc, aDst: TVecC128);
{$if defined(ASMx64)}
// RCX <- aSrc, RDX <- aDst
asm
  mov r8, [rcx + 8]       // R8 <- aSrc.Stride
  mov r9, [rdx + 8]       // R9 <- aDst.Stride
  mov rax, [rcx + 16]     // RAX <- aSrc.Length
  mov rcx, [rcx]          // RCX <- @aSrc[0]
  mov rdx, [rdx]          // RDX <- @aDst[0]
  mov r10, rax
  shr rax, 1
  jz @rest
@L:
  movupd xmm0, [rcx]      // xmm0 <- aSrc[I]
  movupd [rdx], xmm0      // aDst[I] <- xmm0
  add rcx, r8
  add rdx, r9
  movupd xmm1, [rcx]      // xmm1 <- aSrc[I + 1]
  movupd [rdx], xmm1      // aDst[I + 1] <- xmm1
  add rcx, r8
  add rdx, r9
  dec rax
  jnz @L

@rest:
  and r10, 1
  jz @end
  movupd xmm0, [rcx]
  movupd [rdx], xmm0
@end:
end;
{$else}
var pSrc, pDst, pEnd: PByte;
begin
  Assert((aSrc.Length = aDst.Length) and (aSrc.Length > 0));
  pSrc := aSrc.Data;
  pDst := aDst.Data;
  pEnd := aSrc.Data + aSrc.Stride * aSrc.Length;
  while pSrc < pEnd do begin
    PCmplx128(pDst)^ := PCmplx128(pSrc)^;
    Inc(pSrc, aSrc.Stride);
    Inc(pDst, aDst.Stride);
  end;
end;
{$endif}

procedure _fftcomb2(const data, w: TVecC64); overload;
{$if defined(ASMx64)}
// RCX <- @data, RDX <- @w
asm
  mov rax, [rdx]        // RAX <- @W
  mov r11, [rcx + 16]   // R11 <- N
  shr r11, 1            // R11 <- N/2
  mov r8, [rdx + 8]     // R8 <- w.Stride
  mov rcx, [rcx]        // RCX <- @data[0]
  lea rdx, rcx + 8*r11  // RDX <- @data[N/2]
@L:
  movq xmm1, [rax]          // xmm1 <- (wr, wi)
  pshufd xmm1, xmm1, $14    // xmm1 <- (wr, wi, wi, wr)
  movq xmm2, [rdx]          // xmm2 <- (ar, ai) := (offt[k].re, offt[k].im)
  pshufd xmm2, xmm2, $50    // xmm2 <- (ar, ar, ai, ai)
  mulps xmm1, xmm2          // xmm1 <- (wr*ar, wi*ar, wi*ai, wr*ai)
  movhlps xmm2, xmm1        // xmm2 <- (wi*ai, wr*ai)
  addsubps xmm1, xmm2       // xmm1 <- w*fft[k]

  movq xmm3, [rcx]          // xmm1 <- efft[k]
  movq xmm4, xmm3
  addps xmm3, xmm1          // xmm0 <- efft[k] + w[k] * offt[k]
  subps xmm4, xmm1          // xmm1 <- efft[k] - w[k] * offt[k]

  movq [rcx], xmm3
  movq [rdx], xmm4
  add rcx, 8
  add rdx, 8
  add rax, r8
  dec r11
  jnz @L

@end:
end;
{$else}
var pe, po, pw, pEnd: PByte;
    efft, offt: TVecC64;
    c: TCmplx64;
    N2: NativeInt;
begin
  N2 := w.Length;
  efft := data.Span(0, N2);
  offt := data.Span(N2, N2);
  pe := efft.Data;
  po := offt.Data;
  pw := w.Data;
  pEnd := pe + efft.Stride * efft.Length;
  while pe < pEnd do begin
    c := PCmplx64(pw)^ * PCmplx64(po)^;
    PCmplx64(po)^ := PCmplx64(pe)^ - c;
    PCmplx64(pe)^ := PCmplx64(pe)^ + c;
    Inc(pe, efft.Stride);
    Inc(po, offt.Stride);
    Inc(pw, w.Stride);
  end;
end;
{$endif}

procedure _fftcomb2(const data, w: TVecC128); overload;
{$if defined(ASMx86)}
// EAX <- @efft, EDX <- @offt, ECX <- @w
// EAX <- @data, EDX <- @w
asm
  push esi
  push edi
  push ebx
  mov ebx, [edx + 4]  // EBX <- w.stride
  mov esi, [eax]      // ESI <- @data[0]
  mov eax, [edx]      // EAX <- @w
  mov ecx, [edx + 8]  // ECX <- N
  mov edi, ecx
  shl edi, 4
  add edi, esi        // EDI <- @data[N/2]
@L:
  movddup xmm2, [edi]     // xmm2 <- (ar, ar) := (offt[i].re, offt[i].re)
  movddup xmm3, [edi + 8] // xmm3 <- (ai, ai) := (offt[i].im, offt[i].im)
  movupd xmm1, [eax]      // xmm1 <- (wr, wi)
  mulpd xmm2, xmm1        // xmm2 <- (br*ar, bi*ar)
  mulpd xmm3, xmm1        // xmm3 <- (br*ai, bi*ai)
  pshufd xmm3, xmm3, $4e  // xmm3 <- (bi*ai, br*ai)
  addsubpd xmm2, xmm3     // xmm2 <- a * b = offt[i] * w[i]

  movupd xmm1, [esi]      // xmm1 <- efft[i]
  movapd xmm0, xmm1
  addpd xmm0, xmm2        // xmm0 <- efft[i] + w[i] * offt[i]
  subpd xmm1, xmm2        // xmm1 <- efft[i] - w[i] * offt[i]

  movupd [esi], xmm0
  movupd [edi], xmm1
  add esi, 16
  add edi, 16
  add eax, ebx
  dec ecx
  jnz @L

@end:
  pop ebx
  pop edi
  pop esi
end;
{$elseif defined(ASMx64)}
// RCX <- @data, RDX <- @w
asm
  mov r8, rdx
  mov rax, [r8]       // RAX <- @W
  mov r11, [rcx + 16] // R11 <- N
  shr r11, 1          // R11 <- N/2
  mov r8, [r8 + 8]    // R8 <- w.Stride
  mov rcx, [rcx]      // RCX <- @data[0]
  mov rdx, r11
  shl rdx, 4          // RDX <- N/2 * SizeOf(TCmplx128)
  add rdx, rcx        // RDX <- @data[N/2]
  shr r11, 1
@L:
  movddup xmm2, [rdx]     // xmm2 <- (ar, ar) := (offt[i].re, offt[i].re)
  movddup xmm3, [rdx + 8] // xmm3 <- (ai, ai) := (offt[i].im, offt[i].im)
  movupd xmm1, [rax]      // xmm1 <- (wr, wi)
  mulpd xmm2, xmm1        // xmm2 <- (br*ar, bi*ar)
  mulpd xmm3, xmm1        // xmm3 <- (br*ai, bi*ai)
  pshufd xmm3, xmm3, $4e  // xmm3 <- (bi*ai, br*ai)
  addsubpd xmm2, xmm3     // xmm2 <- a * b = offt[i] * w[i]

  movupd xmm1, [rcx]      // xmm1 <- efft[i]
  movapd xmm0, xmm1
  addpd xmm0, xmm2        // xmm0 <- efft[i] + w[i] * offt[i]
  subpd xmm1, xmm2        // xmm1 <- efft[i] - w[i] * offt[i]

  movupd [rcx], xmm0
  movupd [rdx], xmm1
  add rcx, 16
  add rdx, 16
  add rax, r8

  movddup xmm2, [rdx]     // xmm2 <- (ar, ar) := (offt[i].re, offt[i].re)
  movddup xmm3, [rdx + 8] // xmm3 <- (ai, ai) := (offt[i].im, offt[i].im)
  movupd xmm1, [rax]      // xmm1 <- (wr, wi)
  mulpd xmm2, xmm1        // xmm2 <- (br*ar, bi*ar)
  mulpd xmm3, xmm1        // xmm3 <- (br*ai, bi*ai)
  pshufd xmm3, xmm3, $4e  // xmm3 <- (bi*ai, br*ai)
  addsubpd xmm2, xmm3     // xmm2 <- a * b = offt[i] * w[i]

  movupd xmm1, [rcx]      // xmm1 <- efft[i]
  movapd xmm0, xmm1
  addpd xmm0, xmm2        // xmm0 <- efft[i] + w[i] * offt[i]
  subpd xmm1, xmm2        // xmm1 <- efft[i] - w[i] * offt[i]

  movupd [rcx], xmm0
  movupd [rdx], xmm1
  add rcx, 16
  add rdx, 16
  add rax, r8

  dec r11
  jnz @L

@end:
end;
{$else}
var pe, po, pw, pEnd: PByte;
    efft, offt: TVecC128;
    c: TCmplx128;
    N2: NativeInt;
begin
  N2 := w.Length;
  efft := data.Span(0, N2);
  offt := data.Span(N2, N2);
  pe := efft.Data;
  po := offt.Data;
  pw := w.Data;
  pEnd := pe + efft.Stride * efft.Length;
  while pe < pEnd do begin
    c := PCmplx128(pw)^ * PCmplx128(po)^;
    PCmplx128(po)^ := PCmplx128(pe)^ - c;
    PCmplx128(pe)^ := PCmplx128(pe)^ + c;
    Inc(pe, efft.Stride);
    Inc(po, offt.Stride);
    Inc(pw, w.Stride);
  end;
end;
{$endif}

type
  TC3s = record
    _1, _2: Single;
  end;

  TC3d = record
    _1, _2: Double;
  end;

const
  c3s:  TC3s = (_1: -0.5; _2: 0.8660254037844386);
  c3si: TC3s = (_1: -0.5; _2: -0.8660254037844386);

  c3d:  TC3d = (_1: -0.5; _2: 0.8660254037844386);
  c3di: TC3d = (_1: -0.5; _2: -0.8660254037844386);

procedure __fft3(const data: TVecC64; const c3: TC3s); overload;
{$if defined(ASMx64)}
// RCX <- data, RDX <- @c3
asm
  sub rsp, 56 // 48 for xmm6..xmm8 backup + 8 for stack alignment
  movupd [rsp], xmm6
  movupd [rsp + 16], xmm7
  movupd [rsp + 32], xmm8

  movss xmm4, [rdx]         // xmm4 <- c3._1
  movss xmm5, [rdx + 4]     // xmm5 <- c3._2
  pshufd xmm4, xmm4, 0
  pshufd xmm5, xmm5, 0

  mov r8, [rcx]             // R8 <- @data[0]
  movq xmm8, [r8]           // xmm8 <- x0 := data[0]
  movq xmm6, [r8 + 8]       // xmm6 <- x1 := data[1]
  movq xmm7, [r8 + 16]      // xmm7 <- x2 := data[2]

  movq xmm2, xmm6           // xmm2 <- t1
  addps xmm2, xmm7          // xmm2 <- s12 := t1 + t2
  movq xmm3, xmm6           // xmm3 <- t1
  subps xmm3, xmm7          // xmm3 <- d12 := t1 - t2
  pshufd xmm3, xmm3, 1      // xmm4 <- d12r := (d12.im, d12.re)
  mulps xmm3, xmm5          // xmm3 <- d12r * c3_2
  xorps xmm0, xmm0
  addsubps xmm0, xmm3
  movq xmm3, xmm0           // xmm3 <- (-d12.im*c3_2, d12.re*c3_2)

  movq xmm0, xmm8
  addps xmm0, xmm2
  movq [r8], xmm0           // fft0[0] := t0 + s12

  mulps xmm2, xmm4          // xmm2 <- s12 * c3_1
  addps xmm2, xmm8          // xmm2 <- t0 + s12*c3_1
  movq xmm0, xmm2
  addps xmm0, xmm3          // xmm0 <- t0 + s12*c3_1 + (-d12.im*c3_2, d12.re*c3_2)
  movq [r8 + 8], xmm0

  subps xmm2, xmm3          // xmm0 <- t0 + s12*c3_1 - (-d12.im*c3_2, d12.re*c3_2)
  movq [r8 + 16], xmm2

  movupd xmm8, [rsp + 32]
  movupd xmm7, [rsp + 16]
  movupd xmm6, [rsp]
  add rsp, 56
end;
{$else}
var t0, t1, t2, s12, d12, tmp: TCmplx64;
begin
  t0 := data[0];
  t1 := data[1];
  t2 := data[2];
  s12 := t1 + t2;
  d12 := t1 - t2;

  data[0] := t0 + s12;

  tmp.Re := t0.Re + s12.Re * c3._1 - d12.Im * c3._2;
  tmp.Im := t0.Im + s12.Im * c3._1 + d12.Re * c3._2;
  data[1] := tmp;

  tmp.Re := t0.Re + s12.Re * c3._1 + d12.Im * c3._2;
  tmp.Im := t0.Im + s12.Im * c3._1 - d12.Re * c3._2;
  data[2] := tmp;
end;
{$endif}

procedure _fft3(const data: TVecC64); overload;
begin
  __fft3(data, c3s);
end;

procedure _ifft3(const data: TVecC64); overload;
begin
  __fft3(data, c3si);
end;

procedure __fft3(const data: TVecC128; const c3: TC3d); overload;
{$if defined(ASMx64)}
// RCX <- @data, RDX <- @c3
asm
  sub rsp, 56 // 48 for xmm6..xmm8 backup + 8 for stack alignment
  movupd [rsp], xmm6
  movupd [rsp + 16], xmm7
  movupd [rsp + 32], xmm8

  mov r8, [rcx]             // R8 <- @data[0]
  movddup xmm4, [rdx]       // xmm4 <- c3._1
  movddup xmm5, [rdx + 8]   // xmm5 <- c3._2

  mov r8, [rcx]             // R8 <- @data[0]
  movupd xmm8, [r8]         // xmm8 <- x0 := data[0]
  movupd xmm6, [r8 + 16]    // xmm6 <- x1 := data[1]
  movupd xmm7, [r8 + 32]    // xmm7 <- x2 := data[2]

  movapd xmm2, xmm6         // xmm2 <- t1
  addpd xmm2, xmm7          // xmm2 <- s12 := t1 + t2
  movapd xmm3, xmm6         // xmm3 <- t1
  subpd xmm3, xmm7          // xmm3 <- d12 := t1 - t2
  pshufd xmm3, xmm3, $4e    // xmm4 <- d12r := (d12.im, d12.re)
  mulpd xmm3, xmm5          // xmm3 <- d12r * c3_2
  xorps xmm0, xmm0
  addsubpd xmm0, xmm3
  movapd xmm3, xmm0         // xmm3 <- (-d12.im*c3_2, d12.re*c3_2)

  movapd xmm0, xmm8
  addpd xmm0, xmm2
  movupd [r8], xmm0         // fft0[0] := t0 + s12

  mulpd xmm2, xmm4          // xmm2 <- s12 * c3_1
  addpd xmm2, xmm8          // xmm2 <- t0 + s12*c3_1
  movapd xmm0, xmm2
  addpd xmm0, xmm3          // xmm0 <- t0 + s12*c3_1 + (-d12.im*c3_2, d12.re*c3_2)
  movupd [r8 + 16], xmm0

  subpd xmm2, xmm3          // xmm0 <- t0 + s12*c3_1 - (-d12.im*c3_2, d12.re*c3_2)
  movupd [r8 + 32], xmm2

  movupd xmm8, [rsp + 32]
  movupd xmm7, [rsp + 16]
  movupd xmm6, [rsp]
  add rsp, 56
end;
{$else}
var t0, t1, t2, s12, d12, tmp: TCmplx128;
begin
  t0 := data[0];
  t1 := data[1];
  t2 := data[2];
  s12 := t1 + t2;
  d12 := t1 - t2;

  data[0] := t0 + s12;

  tmp.Re := t0.Re + s12.Re * c3._1 - d12.Im * c3._2;
  tmp.Im := t0.Im + s12.Im * c3._1 + d12.Re * c3._2;
  data[1] := tmp;

  tmp.Re := t0.Re + s12.Re * c3._1 + d12.Im * c3._2;
  tmp.Im := t0.Im + s12.Im * c3._1 - d12.Re * c3._2;
  data[2] := tmp;
end;
{$endif}

procedure _fft3(const data: TVecC128); overload;
begin
  __fft3(data, c3d);
end;

procedure _ifft3(const data: TVecC128);
begin
  __fft3(data, c3di);
end;

procedure __fftcomb3(const data, w: TVecC64; const c3: TC3s); overload;
{$if defined(ASMx64)}
// RCX <- data, RDX <- w, R8 <- @c3
asm
  sub rsp, 56 // 48 for xmm6..xmm8 backup + 8 for RBX
  movupd [rsp], xmm6
  movupd [rsp + 16], xmm7
  movupd [rsp + 32], xmm8
  mov [rsp + 48], rbx

  movss xmm4, [r8]      // xmm4 <- c3._1
  movss xmm5, [r8 + 4]  // xmm5 <- c3._2
  pshufd xmm4, xmm4, 0
  pshufd xmm5, xmm5, 0

  mov r8, [rcx]         // R8 <- @data[0]
  mov rcx, [rdx + 16]   // RCX <- N/3
  mov rax, rcx
  shl rax, 3            // RAX <- (N/3)*Sizeof(TCmplx64)
  lea r9, r8 + rax      // R9 <- @data[N/3]
  lea r10, r8 + 2*rax   // R10 <- @data[2N/3]
  mov rax, [rdx + 8]    // RAX <- w.stride
  mov rdx, [rdx]        // RDX <- @w[0]
  xor rbx, rbx          // RBX <- k*w.stride
@L:
  movupd xmm0, [rdx + rbx]  // xmm0 <- (wr, wi)
  pshufd xmm0, xmm0, $14    // xmm0 <- (wr, wi, wi, wr)
  movq xmm6, [r9]           // xmm6 <- (ar, ai) := (fft1[k].re, fft1[k].im)
  pshufd xmm6, xmm6, $50    // xmm6 <- (ar, ar, ai, ai)
  mulps xmm6, xmm0          // xmm6 <- (wr*ar, wi*ar, wi*ai, wr*ai)
  movhlps xmm2, xmm6        // xmm2 <- (wi*ai, wr*ai)
  addsubps xmm6, xmm2       // xmm6 < t1 := fft1[k] * w[k]

  movq xmm1, [rdx + 2*rbx]  // xmm1 <- w2 := w^2 = w[2*k]
  pshufd xmm1, xmm1, $14    // xmm1 <- (w2r, w2i, w2i, w2r)
  movq xmm7, [r10]          // xmm7 <- (ar, ai) := (fft2[k].re, fft2[k].im)
  pshufd xmm7, xmm7, $50    // xmm7 <- (ar, ar, ai, ai)
  mulps xmm7, xmm1          // xmm7 <- (w2r*ar, w2i*ar, w2i*ai, w2r*ai)
  movhlps xmm3, xmm7        // xmm3 <- (w2i*ai, w2r*ai)
  addsubps xmm7, xmm3       // xmm7 <- t2 := fft2[k] * w2

  movq xmm2, xmm6           // xmm2 <- t1
  addps xmm2, xmm7          // xmm2 <- s12 := t1 + t2
  movq xmm3, xmm6           // xmm3 <- t1
  subps xmm3, xmm7          // xmm3 <- d12 := t1 - t2
  pshufd xmm3, xmm3, $1     // xmm4 <- d12r := (d12.im, d12.re)
  mulps xmm3, xmm5          // xmm3 <- d12r * c3_2
  xorps xmm0, xmm0
  addsubps xmm0, xmm3
  movq xmm3, xmm0           // xmm3 <- (-d12.im*c3_2, d12.re*c3_2)

  movq xmm8, [r8]           // xmm8 <- fft0[k]
  movq xmm0, xmm8
  addps xmm0, xmm2          // xmm0 <- fft0[k] + s12
  movq [r8], xmm0

  mulps xmm2, xmm4          // xmm2 <- s12 * c3_1
  addps xmm2, xmm8          // xmm2 <- fft0[k] + s12*c3_1
  movq xmm0, xmm2
  addps xmm0, xmm3          // xmm0 <- fft0[k] + s12*c3_1 + (-d12.im*c3_2, d12.re*c3_2)
  movq [r9], xmm0

  subps xmm2, xmm3          // xmm0 <- fft0[k] + s12*c3_1 - (-d12.im*c3_2, d12.re*c3_2)
  movq [r10], xmm2

  add r8, 8
  add r9, 8
  add r10, 8
  add rbx, rax

  dec rcx
  jnz @L

  mov rbx, [rsp + 48]
  movupd xmm8, [rsp + 32]
  movupd xmm7, [rsp + 16]
  movupd xmm6, [rsp]
  add rsp, 56
end;
{$else}
var k, N3: NativeInt;
    t0, t1, t2, s12, d12, tmp: TCmplx64;
    fft0, fft1, fft2: TVecC64;
begin
  N3 := w.Length;
  fft0 := data.Span(0, N3);
  fft1 := data.Span(N3, N3);
  fft2 := data.Span(2*N3, N3);
  for k := 0 to N3 - 1 do begin
    t0 := fft0[k];
    t1 := fft1[k] * w[k];
    t2 := fft2[k] * w[2*k];
    s12 := t1 + t2;
    d12 := t1 - t2;

    fft0[k] := t0 + s12;

    tmp.Re := t0.Re + s12.Re * c3._1 - d12.Im * c3._2;
    tmp.Im := t0.Im + s12.Im * c3._1 + d12.Re * c3._2;
    fft1[k] := tmp;

    tmp.Re := t0.Re + s12.Re * c3._1 + d12.Im * c3._2;
    tmp.Im := t0.Im + s12.Im * c3._1 - d12.Re * c3._2;
    fft2[k] := tmp;
  end;
end;
{$endif}

procedure _fftcomb3(const data, w: TVecC64); overload;
begin
  __fftcomb3(data, w, c3s)
end;

procedure _ifftcomb3(const data, w: TVecC64); overload
begin
  __fftcomb3(data, w, c3si);
end;

procedure __fftcomb3(const data, w: TVecC128; const c3: TC3d); overload;
{$if defined(ASMx64)}
// RCX <- data, RDX <- w, R8 <- @c3
asm
  sub rsp, 56 // 48 for xmm6..xmm8 backup + 8 for RBX
  movupd [rsp], xmm6
  movupd [rsp + 16], xmm7
  movupd [rsp + 32], xmm8
  mov [rsp + 48], rbx

  movddup xmm4, [r8]      // xmm4 <- c3._1
  movddup xmm5, [r8 + 8]  // xmm5 <- c3._2
  mov r8, [rcx]           // R8 <- @data[0]
  mov rcx, [rdx + 16]     // RCX <- N/3
  mov rax, rcx
  shl rax, 4
  lea r9, r8 + rax        // R9 <- @data[N/3]
  lea r10, r8 + 2*rax     // R10 <- @data[2N/3]
  mov rax, [rdx + 8]      // RAX <- w.stride
  mov rdx, [rdx]          // RDX <- @w[0]
  xor rbx, rbx            // RBX <- k*w.stride
@L:
  movupd xmm0, [rdx + rbx]  // xmm0 <- (wr, wi)

  movddup xmm2, [r9]        // xmm2 <- (ar, ar) := (fft1[k].re, fft1[k].re)
  movddup xmm3, [r9 + 8]    // xmm3 <- (ai, ai) := (fft1[k].im, fft1[k].im)
  mulpd xmm2, xmm0          // xmm2 <- (wr*ar, wi*ar)
  mulpd xmm3, xmm0          // xmm3 <- (wr*ai, wi*ai)
  pshufd xmm3, xmm3, $4e    // xmm3 <- (wi*ai, wr*ai)
  addsubpd xmm2, xmm3       // xmm2 <- t1 := fft1[k] * w[k]
  movapd xmm6, xmm2

  movupd xmm0, [rdx + 2*rbx]// xmm0 <- w2 := w^2 = w[2*k]
  movddup xmm2, [r10]       // xmm2 <- (ar, ar) := (fft2[k].re, fft2[k].re)
  movddup xmm3, [r10 + 8]   // xmm3 <- (ai, ai) := (fft2[k].im, fft2[k].im)
  mulpd xmm2, xmm0          // xmm2 <- (w2r*ar, w2i*ar)
  mulpd xmm3, xmm0          // xmm3 <- (w2r*ai, w2i*ai)
  pshufd xmm3, xmm3, $4e    // xmm3 <- (w2i*ai, w2r*ai)
  addsubpd xmm2, xmm3       // xmm2 <- t2 := fft2[k] * w2
  movapd xmm7, xmm2

  movapd xmm2, xmm6         // xmm2 <- t1
  addpd xmm2, xmm7          // xmm2 <- s12 := t1 + t2
  movapd xmm3, xmm6         // xmm3 <- t1
  subpd xmm3, xmm7          // xmm3 <- d12 := t1 - t2
  pshufd xmm3, xmm3, $4e    // xmm4 <- d12r := (d12.im, d12.re)
  mulpd xmm3, xmm5          // xmm3 <- d12r * c3_2
  xorps xmm0, xmm0
  addsubpd xmm0, xmm3
  movapd xmm3, xmm0         // xmm3 <- (-d12.im*c3_2, d12.re*c3_2)

  movupd xmm8, [r8]         // xmm8 <- fft0[k]
  movapd xmm0, xmm8
  addpd xmm0, xmm2          // xmm0 <- fft0[k] + s12
  movupd [r8], xmm0

  mulpd xmm2, xmm4          // xmm2 <- s12 * c3_1
  addpd xmm2, xmm8          // xmm2 <- fft0[k] + s12*c3_1
  movapd xmm0, xmm2
  addpd xmm0, xmm3          // xmm0 <- fft0[k] + s12*c3_1 + (-d12.im*c3_2, d12.re*c3_2)
  movupd [r9], xmm0

  subpd xmm2, xmm3          // xmm0 <- fft0[k] + s12*c3_1 - (-d12.im*c3_2, d12.re*c3_2)
  movupd [r10], xmm2

  add r8, 16
  add r9, 16
  add r10, 16
  add rbx, rax

  dec rcx
  jnz @L

  mov rbx, [rsp + 48]
  movupd xmm8, [rsp + 32]
  movupd xmm7, [rsp + 16]
  movupd xmm6, [rsp]
  add rsp, 56
end;
{$else}
var k, N3: NativeInt;
    t0, t1, t2, s12, d12, tmp: TCmplx128;
    fft0, fft1, fft2: TVecC128;
begin
  N3 := w.Length;
  fft0 := data.Span(0, N3);
  fft1 := data.Span(N3, N3);
  fft2 := data.Span(2*N3, N3);
  for k := 0 to N3 - 1 do begin
    t0 := fft0[k];
    t1 := fft1[k] * w[k];
    t2 := fft2[k] * w[2*k];
    s12 := t1 + t2;
    d12 := t1 - t2;

    fft0[k] := t0 + s12;

    tmp.Re := t0.Re + s12.Re * c3._1 - d12.Im * c3._2;
    tmp.Im := t0.Im + s12.Im * c3._1 + d12.Re * c3._2;
    fft1[k] := tmp;

    tmp.Re := t0.Re + s12.Re * c3._1 + d12.Im * c3._2;
    tmp.Im := t0.Im + s12.Im * c3._1 - d12.Re * c3._2;
    fft2[k] := tmp;
  end;
end;
{$endif}

procedure _fftcomb3(const data, w: TVecC128); overload;
begin
  __fftcomb3(data, w, c3d);
end;

procedure _ifftcomb3(const data, w: TVecC128); overload;
begin
  __fftcomb3(data, w, c3di);
end;

procedure _fft4(const data: TVecC64); overload;
{$if defined(ASMx64)}
// RCX <- @data
asm
  mov rax, [rcx]            // RAX <- @data[0]
  movups xmm0, [rax]        // xmm0 <- (x0, x1) := (data[0], data[1])
  movaps xmm1, xmm0         // xmm1 <- (x0, x1)
  movups xmm2, [rax + 16]   // xmm2 <- (x2, x3) := (data[2], data[3])
  addps xmm0, xmm2          // xmm0 <- (t0, t2) := (x0 + x2, x1 + x3)
  subps xmm1, xmm2          // xmm1 <- (t1, t3) := (x0 - x2, x1 - x3)

  movhlps xmm2, xmm1        // xmm2 <- t3
  pshufd xmm2, xmm2, $11    // xmm2 <- (t3.im, t3.re, t3.im, t3.re)
  movupd xmm3, cImSgnMaskF32
  xorps xmm2, xmm3          // xmm2 <- (t3, t3) := (t3.im, -t3.re, t3.im, -t3.re)
  movhlps xmm2, xmm0        // xmm2 <- (t2, t3)
  movlhps xmm0, xmm1        // xmm1 <- (t0, t1)
  movaps xmm1, xmm0

  addps xmm0, xmm2          // xmm0 <- (t0 + t2, t1 + t3)
  subps xmm1, xmm2          // xmm1 <- (t0 - t2, t1 - t3)

  movq [rax], xmm0          // data[0] <- t0 + t2
  movhlps xmm0, xmm0
  movq [rax + 24], xmm0     // data[3] <- t1 + t3
  movq [rax + 16], xmm1     // data[2] <- t0 - t2
  movhlps xmm1, xmm1
  movq [rax + 8], xmm1      // data[1] <- t1 - t3
end;
{$else}
var x0, x1, x2, x3: TCmplx64;
    t0, t1, t2, t3: TCmplx64;
begin
  x0 := data[0];
  x1 := data[1];
  x2 := data[2];
  x3 := data[3];

  t0 := x0 + x2;
  t1 := x0 - x2;
  t2 := x1 + x3;
  t3 := x1 - x3;

  data[0] := t0 + t2;
  data[2] := t0 - t2;

  t3.Init(t3.Im, -t3.Re); // multiplication by -i: (a+bi)*(-i) = b-ai

  data[1] := t1 - t3;
  data[3] := t1 + t3;
end;
{$endif}

procedure _ifft4(const data: TVecC64); overload;
{$if defined(ASMx64)}
// RCX <- @data
asm
  mov rax, [rcx]            // RAX <- @data[0]
  movups xmm0, [rax]        // xmm0 <- (x0, x1) := (data[0], data[1])
  movaps xmm1, xmm0         // xmm1 <- (x0, x1)
  movups xmm2, [rax + 16]   // xmm2 <- (x2, x3) := (data[2], data[3])
  addps xmm0, xmm2          // xmm0 <- (t0, t2) := (x0 + x2, x1 + x3)
  subps xmm1, xmm2          // xmm1 <- (t1, t3) := (x0 - x2, x1 - x3)

  movhlps xmm2, xmm1        // xmm2 <- t3
  pshufd xmm2, xmm2, $11    // xmm2 <- (t3.im, t3.re, t3.im, t3.re)
  movupd xmm3, cImSgnMaskF32
  xorps xmm2, xmm3          // xmm2 <- (t3, t3) := (t3.im, -t3.re, t3.im, -t3.re)
  movhlps xmm2, xmm0        // xmm2 <- (t2, t3)
  movlhps xmm0, xmm1        // xmm1 <- (t0, t1)
  movaps xmm1, xmm0

  addps xmm0, xmm2          // xmm0 <- (t0 + t2, t1 + t3)
  subps xmm1, xmm2          // xmm1 <- (t0 - t2, t1 - t3)

  movq [rax], xmm0          // data[0] <- t0 + t2
  movhlps xmm0, xmm0
  movq [rax + 8], xmm0     // data[3] <- t1 + t3
  movq [rax + 16], xmm1     // data[2] <- t0 - t2
  movhlps xmm1, xmm1
  movq [rax + 24], xmm1      // data[1] <- t1 - t3
end;
{$else}
var x0, x1, x2, x3: TCmplx64;
    t0, t1, t2, t3: TCmplx64;
begin
  x0 := data[0];
  x1 := data[1];
  x2 := data[2];
  x3 := data[3];

  t0 := x0 + x2;
  t1 := x0 - x2;
  t2 := x1 + x3;
  t3 := x1 - x3;

  data[0] := t0 + t2;
  data[2] := t0 - t2;

  t3.Init(t3.Im, -t3.Re); // multiplication by -i: (a+bi)*(-i) = b-ai

  data[1] := t1 + t3;
  data[3] := t1 - t3;
end;
{$endif}

procedure _fft4(const data: TVecC128);
{$if defined(ASMx64)}
// RCX <- @data
asm
  mov rax, [rcx]            // RAX <- @data[0]
  movupd xmm0, [rax]        // xmm0 <- x0 := data[0]
  movupd xmm1, [rax + 16]   // xmm1 <- x1 := data[1]
  movupd xmm2, [rax + 32]   // xmm2 <- x2 := data[2]
  movups xmm3, [rax + 48]   // xmm3 <- x3 := data[3]

  movapd xmm4, xmm0
  addpd xmm0, xmm2          // xmm0 <- t0 := x0 + x2
  subpd xmm4, xmm2          // xmm4 <- t1 := x0 - x2
  movapd xmm5, xmm1
  addpd xmm1, xmm3          // xmm1 <- t2 := x1 + x3
  subpd xmm5, xmm3          // xmm5 <- t3 := x1 - x3

  movapd xmm2, xmm0         // xmm2 <- t0
  addpd xmm0, xmm1          // xmm0 <- t0 + t2
  subpd xmm2, xmm1          // xmm2 <- t0 - t1
  movupd [rax], xmm0        // data[0] := t0 + t2
  movupd [rax + 32], xmm2   // data[2] := t0 - t2

  pshufd xmm5, xmm5, $4e    // xmm5 <- (t3.im, t3.re)
  movupd xmm0, cImSgnMaskF64
  xorpd xmm5, xmm0          // xmm5 <- t3 := (t3.im, -t3.re)

  movapd xmm0, xmm4         // xmm0 <- t1
  subpd xmm0, xmm5          // xmm0 <- t1 - t3
  addpd xmm4, xmm5          // xmm4 <- t1 + t3
  movupd [rax + 16], xmm0   // data[1] := t1 - t3
  movupd [rax + 48], xmm4   // data[3] := t1 + t3
end;
{$else}
var x0, x1, x2, x3: TCmplx128;
    t0, t1, t2, t3: TCmplx128;
begin
  x0 := data[0];
  x1 := data[1];
  x2 := data[2];
  x3 := data[3];

  t0 := x0 + x2;
  t1 := x0 - x2;
  t2 := x1 + x3;
  t3 := x1 - x3;

  data[0] := t0 + t2;
  data[2] := t0 - t2;

  t3.Init(t3.Im, -t3.Re); // multiplication by -i: (a+bi)*(-i) = b-ai

  data[1] := t1 - t3;
  data[3] := t1 + t3;
end;
{$endif}

procedure _ifft4(const data: TVecC128);
{$if defined(ASMx64)}
// RCX <- @data
asm
  mov rax, [rcx]            // RAX <- @data[0]
  movupd xmm0, [rax]        // xmm0 <- x0 := data[0]
  movupd xmm1, [rax + 16]   // xmm1 <- x1 := data[1]
  movupd xmm2, [rax + 32]   // xmm2 <- x2 := data[2]
  movups xmm3, [rax + 48]   // xmm3 <- x3 := data[3]

  movapd xmm4, xmm0
  addpd xmm0, xmm2          // xmm0 <- t0 := x0 + x2
  subpd xmm4, xmm2          // xmm4 <- t1 := x0 - x2
  movapd xmm5, xmm1
  addpd xmm1, xmm3          // xmm1 <- t2 := x1 + x3
  subpd xmm5, xmm3          // xmm5 <- t3 := x1 - x3

  movapd xmm2, xmm0         // xmm2 <- t0
  addpd xmm0, xmm1          // xmm0 <- t0 + t2
  subpd xmm2, xmm1          // xmm2 <- t0 - t1
  movupd [rax], xmm0        // data[0] := t0 + t2
  movupd [rax + 32], xmm2   // data[2] := t0 - t2

  pshufd xmm5, xmm5, $4e    // xmm5 <- (t3.im, t3.re)
  movupd xmm0, cImSgnMaskF64
  xorpd xmm5, xmm0          // xmm5 <- t3 := (t3.im, -t3.re)

  movapd xmm0, xmm4         // xmm0 <- t1
  subpd xmm0, xmm5          // xmm0 <- t1 - t3
  addpd xmm4, xmm5          // xmm4 <- t1 + t3
  movupd [rax + 48], xmm0   // data[1] := t1 - t3
  movupd [rax + 16], xmm4   // data[3] := t1 + t3
end;
{$else}
var x0, x1, x2, x3: TCmplx128;
    t0, t1, t2, t3: TCmplx128;
begin
  x0 := data[0];
  x1 := data[1];
  x2 := data[2];
  x3 := data[3];

  t0 := x0 + x2;
  t1 := x0 - x2;
  t2 := x1 + x3;
  t3 := x1 - x3;

  data[0] := t0 + t2;
  data[2] := t0 - t2;

  t3.Init(t3.Im, -t3.Re); // multiplication by -i: (a+bi)*(-i) = b-ai

  data[1] := t1 + t3;
  data[3] := t1 - t3;
end;
{$endif}

procedure _fftcomb4(const data, w: TVecC64); overload;
{$if defined(ASMx64)}
// RCX <- @data, RDX <- @w
asm
  sub rsp, 56 // 16 for xmm6 backup + 16 for RBX, RSI + 8 for stack alignment
  movupd [rsp], xmm6
  movupd [rsp + 16], xmm7
  mov [rsp + 32], rbx
  mov [rsp + 40], rsi

  mov rax, [rcx + 16]
  shl rax, 1          // RAX <- N/4 * SizeOf(TCmplx64)
  mov r8, [rcx]       // R8  <- @data[0]
  lea r9, r8 + rax    // R9  <- @data[N/4]
  lea r10, r9 + rax   // R10 <- @data[2N/4]
  lea r11, r10 + rax  // R11 <- @data[3N/4]
  shr rax, 3          // RAX <- N/4
  mov rcx, [rdx + 8]  // RCX <- w.stride
  mov rdx, [rdx]      // RDX <- @w
  xor rbx, rbx        // RBX <- k*w.stride
  movdqu xmm7, cImSgnMaskF32
@L:
  movq xmm0, [r8]           // xmm0 <- fft0[k]

  movq xmm1, [rdx + rbx]    // xmm1 <- w1
  pshufd xmm1, xmm1, $14    // xmm1 <- (w1r, w1i, w1i, w1r)
  movq xmm2, [r9]           // xmm2 <- (ar, ai) := (fft1[k].re, fft1[k].im)
  pshufd xmm2, xmm2, $50    // xmm2 <- (ar, ar, ai, ai)
  mulps xmm1, xmm2          // xmm1 <- (w1r*ar, w1i*ar, w1i*ai, w1r*ai)
  movhlps xmm2, xmm1        // xmm2 <- (w1i*ai, w1r*ai)
  addsubps xmm1, xmm2       // xmm1 <- w*fft1[k]
  movlhps xmm0, xmm1        // xmm0 <- (x0, x1) := (fft0[k], w*fft1[k])

  movq xmm3, [rdx + 2*rbx]  // xmm3 <- w2 = w^2 = w[2*k]
  pshufd xmm3, xmm3, $14    // xmm3 <- (w2r, w2i, w2i, w2r)
  movq xmm4, [r10]          // xmm4 <- (ar, ai) := (fft2[k].re, fft2[k].im)
  pshufd xmm4, xmm4, $50    // xmm4 <- (ar, ar, ai, ai)
  mulps xmm3, xmm4          // xmm3 <- (w2r*ar, w2i*ar, w2i*ai, w2r*ai)
  movhlps xmm4, xmm3        // xmm4 <- (w2i*ai, w2r*ai)
  addsubps xmm3, xmm4
  movq xmm2, xmm3           // xmm2 <- w2*fft2[k]

  lea rsi, 2*rbx + rbx
  movq xmm5, [rdx + rsi]    // xmm5 <- w3 := w[k]^3 = w[3*k]
  pshufd xmm5, xmm5, $14    // xmm5 <- (w3r, w3i, w3i, w3r)
  movq xmm6, [r11]          // xmm6 <- (ar, ai) := (fft3[k].re, fft3[k].im)
  pshufd xmm6, xmm6, $50    // xmm6 <- (ar, ar, ai, ai)
  mulps xmm5, xmm6          // xmm5 <- (w3r*ar, w3i*ar, w3i*ai, w3r*ai)
  movhlps xmm6, xmm5        // xmm6 <- (w2i*ai, w2r*ai)
  addsubps xmm5, xmm6       // xmm5 <- w3*fft3[k]
  movlhps xmm2, xmm5        // xmm2 <- (x2, x3) <- (w2*fft2[k], w3*fft3[k])

  // (xmm0, xmm2) <- ((x0,x1), (x2,x3)) := ((fft0[k], fft1[k]*w1), (fft2[k]*w2, fft3[k]*w3))
  movaps xmm1, xmm0         // xmm1 <- (x0, x1)
  addps xmm0, xmm2          // xmm0 <- (t0, t2) := (x0 + x2, x1 + x3)
  subps xmm1, xmm2          // xmm1 <- (t1, t3) := (x0 - x2, x1 - x3)

  movhlps xmm2, xmm1        // xmm2 <- t3
  pshufd xmm2, xmm2, $11    // xmm2 <- (t3.im, t3.re, t3.im, t3.re)
  movupd xmm3, xmm7
  xorps xmm2, xmm3          // xmm2 <- (t3, t3) := (t3.im, -t3.re, t3.im, -t3.re)
  movhlps xmm2, xmm0        // xmm2 <- (t2, t3)
  movlhps xmm0, xmm1        // xmm1 <- (t0, t1)
  movaps xmm1, xmm0

  addps xmm0, xmm2          // xmm0 <- (t0 + t2, t1 + t3)
  subps xmm1, xmm2          // xmm1 <- (t0 - t2, t1 - t3)

  movq [r8], xmm0           // fft0[k] <- t0 + t2
  movhlps xmm0, xmm0
  movq [r11], xmm0          // fft3[k] <- t1 + t3
  movq [r10], xmm1          // fft2[k] <- t0 - t2
  movhlps xmm1, xmm1
  movq [r9], xmm1           // fft1[k] <- t1 - t3

  add r8, 8
  add r9, 8
  add r10, 8
  add r11, 8
  add rbx, rcx
  dec rax
  jnz @L

  movupd xmm6, [rsp]
  movupd xmm7, [rsp + 16]
  mov rbx, [rsp + 32]
  mov rsi, [rsp + 40]
  add rsp, 56
end;
{$else}
var
  k, N4: NativeInt;
  x0, x1, x2, x3: TCmplx64;
  t0, t1, t2, t3: TCmplx64;
  fft0, fft1, fft2, fft3: TVecC64;
begin
  N4 := w.Length;

  fft0 := data.Span(0, N4);
  fft1 := data.Span(N4, N4);
  fft2 := data.Span(2*N4, N4);
  fft3 := data.Span(3*N4, N4);

  for k := 0 to N4 - 1 do begin
    x0 := fft0[k];
    x1 := fft1[k] * w[k];
    x2 := fft2[k] * w[2*k];
    x3 := fft3[k] * w[3*k];

    t0 := x0 + x2;
    t1 := x0 - x2;
    t2 := x1 + x3;
    t3 := x1 - x3;

    fft0[k] := t0 + t2;
    fft2[k] := t0 - t2;

    t3.Init(t3.Im, -t3.Re); // multiplication by -i: (a+bi)*(-i) = b-ai

    fft1[k] := t1 - t3;
    fft3[k] := t1 + t3;
  end;
end;
{$endif}

procedure _ifftcomb4(const data, w: TVecC64); overload;
{$if defined(ASMx64)}
// RCX <- @data, RDX <- @w
asm
  sub rsp, 56 // 16 for xmm6 backup + 16 for RBX, RSI + 8 for stack alignment
  movupd [rsp], xmm6
  movupd [rsp + 16], xmm7
  mov [rsp + 32], rbx
  mov [rsp + 40], rsi

  mov rax, [rcx + 16]
  shl rax, 1          // RAX <- N/4 * SizeOf(TCmplx64)
  mov r8, [rcx]       // R8  <- @data[0]
  lea r9, r8 + rax    // R9  <- @data[N/4]
  lea r10, r9 + rax   // R10 <- @data[2N/4]
  lea r11, r10 + rax  // R11 <- @data[3N/4]
  shr rax, 3          // RAX <- N/4
  mov rcx, [rdx + 8]  // RCX <- w.stride
  mov rdx, [rdx]      // RDX <- @w
  xor rbx, rbx        // RBX <- k*w.stride
  movdqu xmm7, cImSgnMaskF32
@L:
  movq xmm0, [r8]           // xmm0 <- fft0[k]

  movq xmm1, [rdx + rbx]    // xmm1 <- w1
  pshufd xmm1, xmm1, $14    // xmm1 <- (w1r, w1i, w1i, w1r)
  movq xmm2, [r9]           // xmm2 <- (ar, ai) := (fft1[k].re, fft1[k].im)
  pshufd xmm2, xmm2, $50    // xmm2 <- (ar, ar, ai, ai)
  mulps xmm1, xmm2          // xmm1 <- (w1r*ar, w1i*ar, w1i*ai, w1r*ai)
  movhlps xmm2, xmm1        // xmm2 <- (w1i*ai, w1r*ai)
  addsubps xmm1, xmm2       // xmm1 <- w*fft1[k]
  movlhps xmm0, xmm1        // xmm0 <- (x0, x1) := (fft0[k], w*fft1[k])

  movq xmm3, [rdx + 2*rbx]  // xmm3 <- w2 = w^2 = w[2*k]
  pshufd xmm3, xmm3, $14    // xmm3 <- (w2r, w2i, w2i, w2r)
  movq xmm4, [r10]          // xmm4 <- (ar, ai) := (fft2[k].re, fft2[k].im)
  pshufd xmm4, xmm4, $50    // xmm4 <- (ar, ar, ai, ai)
  mulps xmm3, xmm4          // xmm3 <- (w2r*ar, w2i*ar, w2i*ai, w2r*ai)
  movhlps xmm4, xmm3        // xmm4 <- (w2i*ai, w2r*ai)
  addsubps xmm3, xmm4
  movq xmm2, xmm3           // xmm2 <- w2*fft2[k]

  lea rsi, 2*rbx + rbx
  movq xmm5, [rdx + rsi]    // xmm5 <- w3 := w[k]^3 = w[3*k]
  pshufd xmm5, xmm5, $14    // xmm5 <- (w3r, w3i, w3i, w3r)
  movq xmm6, [r11]          // xmm6 <- (ar, ai) := (fft3[k].re, fft3[k].im)
  pshufd xmm6, xmm6, $50    // xmm6 <- (ar, ar, ai, ai)
  mulps xmm5, xmm6          // xmm5 <- (w3r*ar, w3i*ar, w3i*ai, w3r*ai)
  movhlps xmm6, xmm5        // xmm6 <- (w2i*ai, w2r*ai)
  addsubps xmm5, xmm6       // xmm5 <- w3*fft3[k]
  movlhps xmm2, xmm5        // xmm2 <- (x2, x3) <- (w2*fft2[k], w3*fft3[k])

  // (xmm0, xmm2) <- ((x0,x1), (x2,x3)) := ((fft0[k], fft1[k]*w1), (fft2[k]*w2, fft3[k]*w3))
  movaps xmm1, xmm0         // xmm1 <- (x0, x1)
  addps xmm0, xmm2          // xmm0 <- (t0, t2) := (x0 + x2, x1 + x3)
  subps xmm1, xmm2          // xmm1 <- (t1, t3) := (x0 - x2, x1 - x3)

  movhlps xmm2, xmm1        // xmm2 <- t3
  pshufd xmm2, xmm2, $11    // xmm2 <- (t3.im, t3.re, t3.im, t3.re)
  movupd xmm3, xmm7
  xorps xmm2, xmm3          // xmm2 <- (t3, t3) := (t3.im, -t3.re, t3.im, -t3.re)
  movhlps xmm2, xmm0        // xmm2 <- (t2, t3)
  movlhps xmm0, xmm1        // xmm1 <- (t0, t1)
  movaps xmm1, xmm0

  addps xmm0, xmm2          // xmm0 <- (t0 + t2, t1 + t3)
  subps xmm1, xmm2          // xmm1 <- (t0 - t2, t1 - t3)

  movq [r8], xmm0           // fft0[k] <- t0 + t2
  movhlps xmm0, xmm0
  movq [r9], xmm0           // fft3[k] <- t1 + t3
  movq [r10], xmm1          // fft2[k] <- t0 - t2
  movhlps xmm1, xmm1
  movq [r11], xmm1          // fft1[k] <- t1 - t3

  add r8, 8
  add r9, 8
  add r10, 8
  add r11, 8
  add rbx, rcx
  dec rax
  jnz @L

  movupd xmm6, [rsp]
  movupd xmm7, [rsp + 16]
  mov rbx, [rsp + 32]
  mov rsi, [rsp + 40]
  add rsp, 56
end;
{$else}
var
  k, N4: NativeInt;
  x0, x1, x2, x3: TCmplx64;
  t0, t1, t2, t3: TCmplx64;
  fft0, fft1, fft2, fft3: TVecC64;
begin
  N4 := w.Length;

  fft0 := data.Span(0, N4);
  fft1 := data.Span(N4, N4);
  fft2 := data.Span(2*N4, N4);
  fft3 := data.Span(3*N4, N4);

  for k := 0 to N4 - 1 do begin
    x0 := fft0[k];
    x1 := fft1[k] * w[k];
    x2 := fft2[k] * w[2*k];
    x3 := fft3[k] * w[3*k];

    t0 := x0 + x2;
    t1 := x0 - x2;
    t2 := x1 + x3;
    t3 := x1 - x3;

    fft0[k] := t0 + t2;
    fft2[k] := t0 - t2;

    t3.Init(t3.Im, -t3.Re); // multiplication by -i: (a+bi)*(-i) = b-ai

    fft1[k] := t1 + t3;
    fft3[k] := t1 - t3;
  end;
end;
{$endif}

procedure _fftcomb4(const data, w: TVecC128); overload;
{$if defined(ASMx64)}
// RCX <- @data, RDX <- @w
asm
  sub rsp, 40 // 16 for xmm6 backup + 16 for RBX, RSI + 8 for stack alignment
  movupd [rsp], xmm6
  mov [rsp + 16], rbx
  mov [rsp + 24], rsi

  mov rax, [rcx + 16]
  shl rax, 2
  mov r8, [rcx]       // R8  <- @data[0]
  lea r9, r8 + rax    // R9  <- @data[N/4]
  lea r10, r9 + rax   // R10 <- @data[2N/4]
  lea r11, r10 + rax  // R11 <- @data[3N/4]
  shr rax, 4          // RAX <- N/4
  mov rcx, [rdx + 8]  // RCX <- w.stride
  mov rdx, [rdx]      // RDX <- @w
  xor rbx, rbx        // RBX <- k*w.stride
  movdqu xmm6, cImSgnMaskF64
@L:
  movupd xmm0, [rdx + rbx]  // xmm0 <- w1
  movddup xmm1, [r9]        // xmm1 <- (ar, ar) := (fft1[k].re, fft1[k].re)
  movddup xmm2, [r9 + 8]    // xmm2 <- (ai, ai) := (fft1[k].im, fft1[k].im)
  mulpd xmm1, xmm0          // xmm1 <- (ar*wr, ar*wi)
  mulpd xmm2, xmm0          // xmm2 <- (ai*wr, ai*wi)
  pshufd xmm2, xmm2, $4e    // xmm2 <- (ai*wi, ai*wr)
  addsubpd xmm1, xmm2       // xmm1 <- fft1[k] * w1

  movupd xmm2, [rdx + 2*rbx]// xmm0 <- w2 = w^2 = w[2*k]
  movddup xmm3, [r10]       // xmm3 <- (ar, ar) := (fft2[k].re, fft2[k].re)
  movddup xmm4, [r10 + 8]   // xmm4 <- (ai, ai) := (fft2[k].im, fft2[k].im)
  mulpd xmm3, xmm2          // xmm3 <- (ar*w2r, ar*w2i)
  mulpd xmm4, xmm2          // xmm4 <- (ai*w2r, ai*w2i)
  pshufd xmm4, xmm4, $4e    // xmm4 <- (ai*w2i, ai*w2r)
  addsubpd xmm3, xmm4
  movapd xmm2, xmm3         // xmm2 <- fft2[k]*w2

  lea rsi, 2*rbx + rbx
  movupd xmm0, [rdx + rsi]  // xmm2 <- w3 := w[k]^3 = w[3*k]
  movddup xmm3, [r11]       // xmm3 <- (ar, ar) := (fft3[k].re, fft3[k].re)
  movddup xmm4, [r11 + 8]   // xmm4 <- (ai, ai) := (fft3[k].im, fft3[k].im)
  mulpd xmm3, xmm0          // xmm3 <- (ar*w3r, ar*w3i)
  mulpd xmm4, xmm0          // xmm4 <- (ai*w3r, ai*w3i)
  pshufd xmm4, xmm4, $4e    // xmm4 <- (ai*w3i, ai*w3r)
  addsubpd xmm3, xmm4       // xmm3 <- fft3[k]*w3

  movupd xmm0, [r8]         // xmm0 <- fft0[k]

  // (xmm0, xmm1, xmm2, xmm3) <- (fft0[k], fft1[k]*w1, fft2[k]*w2, fft3[k]*w3)
  movapd xmm5, xmm2
  movapd xmm2, xmm0
  addpd xmm0, xmm5          // xmm0 <- t0 := x0 + x2
  subpd xmm2, xmm5          // xmm2 <- t1 := x0 - x2
  movapd xmm5, xmm3
  movapd xmm3, xmm1
  addpd xmm1, xmm5          // xmm1 <- t2 := x1 + x3
  subpd xmm3, xmm5          // xmm3 <- t3 := x1 - x3
  pshufd xmm3, xmm3, $4e    // xmm3 <- (t3.im, t3.re)
  xorpd xmm3, xmm6          // xmm3 <- (t3.im, -t3.re)

  movapd xmm5, xmm0
  addpd xmm0, xmm1          // xmm0 <- t0 + t2
  movupd [r8], xmm0
  subpd xmm5, xmm1          // xmm5 <- t0 - t2
  movupd [r10], xmm5

  movapd xmm5, xmm2
  subpd xmm5, xmm3          // xmm5 <- t1 - t3
  movupd [r9], xmm5
  addpd xmm2, xmm3          // xmm2 <- t1 + t3
  movupd [r11], xmm2

  add r8, 16
  add r9, 16
  add r10, 16
  add r11, 16
  add rbx, rcx
  dec rax
  jnz @L

  mov rsi, [rsp + 24]
  mov rbx, [rsp + 16]
  movupd xmm6, [rsp]
  add rsp, 40
end;
{$else}
var
  k, N4: NativeInt;
  x0, x1, x2, x3: TCmplx128;
  t0, t1, t2, t3: TCmplx128;
  fft0, fft1, fft2, fft3: TVecC128;
begin
  N4 := w.Length;

  fft0 := data.Span(0, N4);
  fft1 := data.Span(N4, N4);
  fft2 := data.Span(2*N4, N4);
  fft3 := data.Span(3*N4, N4);

  for k := 0 to N4 - 1 do begin
    x0 := fft0[k];
    x1 := fft1[k] * w[k];
    x2 := fft2[k] * w[2*k];
    x3 := fft3[k] * w[3*k];

    t0 := x0 + x2;
    t1 := x0 - x2;
    t2 := x1 + x3;
    t3 := x1 - x3;

    fft0[k] := t0 + t2;
    fft2[k] := t0 - t2;

    t3.Init(t3.Im, -t3.Re); // multiplication by -i: (a+bi)*(-i) = b-ai

    fft1[k] := t1 - t3;
    fft3[k] := t1 + t3;
  end;
end;
{$endif}

procedure _ifftcomb4(const data, w: TVecC128); overload;
{$if defined(ASMx64)}
// RCX <- @data, RDX <- @w
asm
  sub rsp, 40 // 16 for xmm6 backup + 16 for RBX, RSI + 8 for stack alignment
  movupd [rsp], xmm6
  mov [rsp + 16], rbx
  mov [rsp + 24], rsi

  mov rax, [rcx + 16]
  shl rax, 2
  mov r8, [rcx]       // R8  <- @data[0]
  lea r9, r8 + rax    // R9  <- @data[N/4]
  lea r10, r9 + rax   // R10 <- @data[2N/4]
  lea r11, r10 + rax  // R11 <- @data[3N/4]
  shr rax, 4          // RAX <- N/4
  mov rcx, [rdx + 8]  // RCX <- w.stride
  mov rdx, [rdx]      // RDX <- @w
  xor rbx, rbx        // RBX <- k*w.stride
  movdqu xmm6, cImSgnMaskF64
@L:
  movupd xmm0, [rdx + rbx]  // xmm0 <- w1
  movddup xmm1, [r9]        // xmm1 <- (ar, ar) := (fft1[k].re, fft1[k].re)
  movddup xmm2, [r9 + 8]    // xmm2 <- (ai, ai) := (fft1[k].im, fft1[k].im)
  mulpd xmm1, xmm0          // xmm1 <- (ar*wr, ar*wi)
  mulpd xmm2, xmm0          // xmm2 <- (ai*wr, ai*wi)
  pshufd xmm2, xmm2, $4e    // xmm2 <- (ai*wi, ai*wr)
  addsubpd xmm1, xmm2       // xmm1 <- fft1[k] * w1

  movupd xmm2, [rdx + 2*rbx]// xmm0 <- w2 = w^2 = w[2*k]
  movddup xmm3, [r10]       // xmm3 <- (ar, ar) := (fft2[k].re, fft2[k].re)
  movddup xmm4, [r10 + 8]   // xmm4 <- (ai, ai) := (fft2[k].im, fft2[k].im)
  mulpd xmm3, xmm2          // xmm3 <- (ar*w2r, ar*w2i)
  mulpd xmm4, xmm2          // xmm4 <- (ai*w2r, ai*w2i)
  pshufd xmm4, xmm4, $4e    // xmm4 <- (ai*w2i, ai*w2r)
  addsubpd xmm3, xmm4
  movapd xmm2, xmm3         // xmm2 <- fft2[k]*w2

  lea rsi, 2*rbx + rbx
  movupd xmm0, [rdx + rsi]  // xmm2 <- w3 := w[k]^3 = w[3*k]
  movddup xmm3, [r11]       // xmm3 <- (ar, ar) := (fft3[k].re, fft3[k].re)
  movddup xmm4, [r11 + 8]   // xmm4 <- (ai, ai) := (fft3[k].im, fft3[k].im)
  mulpd xmm3, xmm0          // xmm3 <- (ar*w3r, ar*w3i)
  mulpd xmm4, xmm0          // xmm4 <- (ai*w3r, ai*w3i)
  pshufd xmm4, xmm4, $4e    // xmm4 <- (ai*w3i, ai*w3r)
  addsubpd xmm3, xmm4       // xmm3 <- fft3[k]*w3

  movupd xmm0, [r8]         // xmm0 <- fft0[k]

  // (xmm0, xmm1, xmm2, xmm3) <- (fft0[k], fft1[k]*w1, fft2[k]*w2, fft3[k]*w3)
  movapd xmm5, xmm2
  movapd xmm2, xmm0
  addpd xmm0, xmm5          // xmm0 <- t0 := x0 + x2
  subpd xmm2, xmm5          // xmm2 <- t1 := x0 - x2
  movapd xmm5, xmm3
  movapd xmm3, xmm1
  addpd xmm1, xmm5          // xmm1 <- t2 := x1 + x3
  subpd xmm3, xmm5          // xmm3 <- t3 := x1 - x3
  pshufd xmm3, xmm3, $4e    // xmm3 <- (t3.im, t3.re)
  xorpd xmm3, xmm6          // xmm3 <- (t3.im, -t3.re)

  movapd xmm5, xmm0
  addpd xmm0, xmm1          // xmm0 <- t0 + t2
  movupd [r8], xmm0
  subpd xmm5, xmm1          // xmm5 <- t0 - t2
  movupd [r10], xmm5

  movapd xmm5, xmm2
  subpd xmm5, xmm3          // xmm5 <- t1 - t3
  movupd [r11], xmm5
  addpd xmm2, xmm3          // xmm2 <- t1 + t3
  movupd [r9], xmm2

  add r8, 16
  add r9, 16
  add r10, 16
  add r11, 16
  add rbx, rcx
  dec rax
  jnz @L

  mov rsi, [rsp + 24]
  mov rbx, [rsp + 16]
  movupd xmm6, [rsp]
  add rsp, 40
end;
{$else}
var
  k, N4: NativeInt;
  x0, x1, x2, x3: TCmplx128;
  t0, t1, t2, t3: TCmplx128;
  fft0, fft1, fft2, fft3: TVecC128;
begin
  N4 := w.Length;

  fft0 := data.Span(0, N4);
  fft1 := data.Span(N4, N4);
  fft2 := data.Span(2*N4, N4);
  fft3 := data.Span(3*N4, N4);

  for k := 0 to N4 - 1 do begin
    x0 := fft0[k];
    x1 := fft1[k] * w[k];
    x2 := fft2[k] * w[2*k];
    x3 := fft3[k] * w[3*k];

    t0 := x0 + x2;
    t1 := x0 - x2;
    t2 := x1 + x3;
    t3 := x1 - x3;

    fft0[k] := t0 + t2;
    fft2[k] := t0 - t2;

    t3.Init(t3.Im, -t3.Re); // multiplication by -i: (a+bi)*(-i) = b-ai

    fft1[k] := t1 + t3;
    fft3[k] := t1 - t3;
  end;
end;
{$endif}

type
  TC5s = record
    _1, _2, _3, _4: Single;
  end;

  TC5d = record
    _1, _2, _3, _4: Double;
  end;

const
  cC5s: TC5s = (
    _1:  0.3090169943749474241; // cos(2*pi/5)
    _2: -0.8090169943749474241; // cos(4*pi/5)
    _3:  0.9510565162951535721; // sin(2*pi/5)
    _4:  0.5877852522924731292  // sin(4*pi/5)
  );

  cC5si: TC5s = (
    _1:  0.3090169943749474241; // cos(-2*pi/5)
    _2: -0.8090169943749474241; // cos(-4*pi/5)
    _3: -0.9510565162951535721; // sin(-2*pi/5)
    _4: -0.5877852522924731292  // sin(-4*pi/5)
  );

  cC5d: TC5d = (
    _1:  0.3090169943749474241; // cos(2*pi/5)
    _2: -0.8090169943749474241; // cos(4*pi/5)
    _3:  0.9510565162951535721; // sin(2*pi/5)
    _4:  0.5877852522924731292  // sin(4*pi/5)
  );

  cC5di: TC5d = (
    _1:  0.3090169943749474241; // cos(-2*pi/5)
    _2: -0.8090169943749474241; // cos(-4*pi/5)
    _3: -0.9510565162951535721; // sin(-2*pi/5)
    _4: -0.5877852522924731292  // sin(-4*pi/5)
  );

procedure __fft5(const data: TVecC64; const c5: TC5s); overload;
{$if defined(ASMx64)}
// RCX <- @data, RDX <- @c5
asm
  sub rsp, 104 // 96 for xmm6..xmm11 backup + 8 for stack alignment
  movupd [rsp], xmm6
  movupd [rsp + 16], xmm7
  movupd [rsp + 32], xmm8
  movupd [rsp + 48], xmm9
  movupd [rsp + 64], xmm10
  movupd [rsp + 80], xmm11

  movss xmm8, [rdx]         // xmm8 <- c5_1
  movss xmm9, [rdx + 4]     // xmm9 <- c5_2
  movlhps xmm8, xmm9
  pshufd xmm8, xmm8, $a0    // xmm8 <- (c5_1, c5_1, c5_2, c5_2)
  pshufd xmm9, xmm8, $4e    // xmm9 <- (c5_2, c5_2, c5_1, c5_1)
  movss xmm10, [rdx + 8]    // xmm10 <- c5_3
  movss xmm11, [rdx + 12]   // xmm11 <- c5_4
  movlhps xmm10, xmm11
  pshufd xmm10, xmm10, $a0  // xmm10 <- (c5_3, c5_3, c5_4, c5_4)
  pshufd xmm11, xmm10, $4e  // xmm11 <- (c5_4, c5_4, c5_3, c5_3)
  movdqu xmm7, cHiC64SgnMask
  xorps xmm11, xmm7         // xmm11 <- (c5_4, c5_4, -c5_3, -c5_3)

  mov r8, [rcx]             // r8 <- @data[0]
  movq xmm0, [r8]           // xmm0 <- x0 := fft0[0]
  movq xmm1, [r8 + 8]       // xmm1 <- x1 := fft1[0]
  movq xmm2, [r8 + 16]      // xmm2 <- x2 := fft2[0]
  movq xmm3, [r8 + 24]      // xmm3 <- x3 := fft3[0]
  movq xmm4, [r8 + 32]      // xmm4 <- x4 := fft4[0]

  movq xmm5, xmm4
  movq xmm4, xmm1
  addps xmm1, xmm5          // xmm1 <- A := x1 + x4
  subps xmm4, xmm5          // xmm4 <- C := x1 - x4

  movq xmm5, xmm3
  movq xmm3, xmm2
  addps xmm2, xmm5          // xmm2 <- B <- x2 + x3
  subps xmm3, xmm5          // xmm3 <- D <- x2 - x3

  movq xmm5, xmm0           // xmm5 <- x0
  addps xmm5, xmm1
  addps xmm5, xmm2
  movq [r8], xmm5           // fft0[0] := x0 + A + B

  movlhps xmm1, xmm1        // xmm1 <- (A, A)
  movlhps xmm2, xmm2        // xmm2 <- (B, B)
  movlhps xmm3, xmm3        // xmm3 <- (D, D)
  movlhps xmm4, xmm4        // xmm4 <- (C, C)
  movlhps xmm0, xmm0        // xmm5 <- (x0, x0)

  mulps xmm1, xmm8          // xmm1 <- (c5_1*A, c5_2*A)
  mulps xmm2, xmm9          // xmm2 <- (c5_2*B, c5_1*B)
  addps xmm1, xmm2          // xmm1 <- U = (c5_1*A + c5_2*B, c5_2*A + c5_1*B)
  addps xmm0, xmm1          // xmm5 <- (x0 + U[0], x0 + U[1])

  mulps xmm4, xmm10         // xmm4 <- (c5_3*C, c5_4*C)
  mulps xmm3, xmm11         // xmm3 <- (c5_4*D, -c5_3*D)
  addps xmm3, xmm4          // xmm3 <- T = (c5_3*C + c5_4*D, c5_4*C - c5_3*D)
  pshufd xmm3, xmm3, $b1    // xmm3 <- (T[0].im, T[0].re, T[1].im, T[1].re)
  movdqu xmm7, cImSgnMaskF32
  xorps xmm3, xmm7          // xmm3 <- T' = (T[0].im, -T[0].re, T[1].im, -T[1].re)

  movaps xmm6, xmm0         // xmm6 <- x0 + U
  subps xmm6, xmm3          // xmm6 <- x0 + U - T'
  movq [r8 + 8], xmm6       // fft1[0] <- x0 + U[0] - T'[0]
  movhlps xmm6, xmm6
  movq [r8 + 16], xmm6      // fft2[0] <- x0 + U[1] - T'[1]

  addps xmm0, xmm3          // xmm5 <- x0 + U + T'
  movq [r8 + 32], xmm0      // fft4[0] <- x0 + U[0] + T'[0]
  movhlps xmm0, xmm0
  movq [r8 + 24], xmm0      // fft3[0] <- x0 + U[1] + T'[1]

  movupd xmm6, [rsp]
  movupd xmm7, [rsp + 16]
  movupd xmm8, [rsp + 32]
  movupd xmm9, [rsp + 48]
  movupd xmm10, [rsp + 64]
  movupd xmm11, [rsp + 80]
  add rsp, 104
end;
{$else}
var
  x0, x1, x2, x3, x4: TCmplx64;
  A, B, C, D, T, U, tmp: TCmplx64;
begin
  x0 := data[0];
  x1 := data[1];
  x2 := data[2];
  x3 := data[3];
  x4 := data[4];

  A := x1 + x4;
  B := x2 + x3;
  C := x1 - x4;
  D := x2 - x3;

  data[0] := x0 + A + B;

  U := A*c5._1 + B*c5._2;
  T := C*c5._3 + D*c5._4;
  tmp.Re := x0.Re + U.Re - T.Im;
  tmp.Im := x0.Im + U.Im + T.Re;
  data[1] := tmp;

  tmp.Re := x0.Re + U.Re + T.Im;
  tmp.Im := x0.Im + U.Im - T.Re;
  data[4] := tmp;

  U := A*c5._2 + B*c5._1;
  T := C*c5._4 - D*c5._3;
  tmp.Re := x0.Re + U.Re - T.Im;
  tmp.Im := x0.Im + U.Im + T.Re;
  data[2] := tmp;

  tmp.Re := x0.Re + U.Re + T.Im;
  tmp.Im := x0.Im + U.Im - T.Re;
  data[3] := tmp;
end;
{$endif}

procedure _fft5(const data: TVecC64);
begin
  __fft5(data, cC5s);
end;

procedure _ifft5(const data: TVecC64);
begin
  __fft5(data, cC5si);
end;

procedure __fft5(const data: TVecC128; const c5: TC5d); overload;
{$if defined(ASMx64)}
// RCX <- @data, RDX <- @c5
asm
  sub rsp, 104 // 96 for xmm6..xmm11 backup + 8 for stack alignment
  movupd [rsp], xmm6
  movupd [rsp + 16], xmm7
  movupd [rsp + 32], xmm8
  movupd [rsp + 48], xmm9
  movupd [rsp + 64], xmm10
  movupd [rsp + 80], xmm11

  movupd xmm8, [rdx]        // xmm8 <- c5._1
  movupd xmm9, [rdx + 8]    // xmm9 <- c5._2
  movupd xmm10, [rdx + 16]  // xmm10 <- c5._3
  movupd xmm11, [rdx + 24]  // xmm11 <- c5._4

  mov r8, [rcx]             // r8 <- @data[0]
  movupd xmm0, [r8]         // xmm0 <- x0 := fft0[0]
  movupd xmm1, [r8 + 16]    // xmm1 <- x1 := fft1[0]
  movupd xmm2, [r8 + 32]    // xmm2 <- x2 := fft2[0]
  movupd xmm3, [r8 + 48]    // xmm3 <- x3 := fft3[0]
  movupd xmm4, [r8 + 64]    // xmm4 <- x4 := fft4[0]

  movapd xmm5, xmm4
  movapd xmm4, xmm1
  addpd xmm1, xmm5          // xmm1 <- A := x1 + x4
  subpd xmm4, xmm5          // xmm4 <- C := x1 - x4

  movapd xmm5, xmm3
  movapd xmm3, xmm2
  addpd xmm2, xmm5          // xmm2 <- B <- x2 + x3
  subpd xmm3, xmm5          // xmm3 <- D <- x2 - x3

  movapd xmm5, xmm0         // xmm5 <- x0
  addpd xmm5, xmm1
  addpd xmm5, xmm2
  movupd [r8], xmm5         // fft0[0] := x0 + A + B

  movddup xmm5, xmm8
  movddup xmm6, xmm9
  mulpd xmm5, xmm1
  mulpd xmm6, xmm2
  addpd xmm5, xmm6          // xmm5 <- U := A*c5_1 + B*c5_2

  movddup xmm6, xmm10
  movddup xmm7, xmm11
  mulpd xmm6, xmm4
  mulpd xmm7, xmm3
  addpd xmm6, xmm7          // xmm6 <- T := C*c5_3 + D*c5_4
  pshufd xmm6, xmm6, $4e    // xmm6 <- (T.im, T.re)
  movdqu xmm7, cImSgnMaskF64
  xorpd xmm6, xmm7          // xmm6 <- T' := (T.im, -T.re)

  movapd xmm7, xmm0         // xmm7 <- x0
  addpd xmm7, xmm5
  subpd xmm7, xmm6
  movupd [r8 + 16], xmm7    // fft1[0] <- x0 + U - T'

  movapd xmm7, xmm0
  addpd xmm7, xmm5
  addpd xmm7, xmm6
  movupd [r8 + 64], xmm7    // fft4[0] := x0 + U + T'

  movddup xmm5, xmm9
  movddup xmm6, xmm8
  mulpd xmm5, xmm1
  mulpd xmm6, xmm2
  addpd xmm5, xmm6          // xmm5 <- U := A*c5_2 + B*c5_1

  movddup xmm6, xmm11
  movddup xmm7, xmm10
  mulpd xmm6, xmm4
  mulpd xmm7, xmm3
  subpd xmm6, xmm7          // xmm6 <- T := C*c5_4 - D*c5_3
  pshufd xmm6, xmm6, $4e
  movdqu xmm7, cImSgnMaskF64
  xorpd xmm6, xmm7          // xmm6 <- T' := (T.im, -T.re)

  movapd xmm7, xmm0
  addpd xmm7, xmm5
  subpd xmm7, xmm6
  movupd [r8 + 32], xmm7   // fft2[0] := x0 + U - T'

  movapd xmm7, xmm0
  addpd xmm7, xmm5
  addpd xmm7, xmm6
  movupd [r8 + 48], xmm7   // fft3[0] := x0 + U + T'

  movupd xmm6, [rsp]
  movupd xmm7, [rsp + 16]
  movupd xmm8, [rsp + 32]
  movupd xmm9, [rsp + 48]
  movupd xmm10, [rsp + 64]
  movupd xmm11, [rsp + 80]
  add rsp, 104
end;
{$else}
var
  x0, x1, x2, x3, x4: TCmplx128;
  A, B, C, D, T, U, tmp: TCmplx128;
begin
  x0 := data[0];
  x1 := data[1];
  x2 := data[2];
  x3 := data[3];
  x4 := data[4];

  A := x1 + x4;
  B := x2 + x3;
  C := x1 - x4;
  D := x2 - x3;

  data[0] := x0 + A + B;

  U := A*c5._1 + B*c5._2;
  T := C*c5._3 + D*c5._4;
  tmp.Re := x0.Re + U.Re - T.Im;
  tmp.Im := x0.Im + U.Im + T.Re;
  data[1] := tmp;

  tmp.Re := x0.Re + U.Re + T.Im;
  tmp.Im := x0.Im + U.Im - T.Re;
  data[4] := tmp;

  U := A*c5._2 + B*c5._1;
  T := C*c5._4 - D*c5._3;
  tmp.Re := x0.Re + U.Re - T.Im;
  tmp.Im := x0.Im + U.Im + T.Re;
  data[2] := tmp;

  tmp.Re := x0.Re + U.Re + T.Im;
  tmp.Im := x0.Im + U.Im - T.Re;
  data[3] := tmp;
end;
{$endif}

procedure _fft5(const data: TVecC128);
begin
  __fft5(data, cC5d);
end;

procedure _ifft5(const data: TVecC128);
begin
  __fft5(data, cC5di);
end;

procedure __fftcomb5(const data, w: TVecC64; const c5: TC5s); overload;
{$if defined(ASMx64)}
// RCX <- @data, RDX <- @w, R8 <- @c5
asm
  sub rsp, 136 // 112 for xmm6..xmm12 backup + 24 for RBX, RSI, R12
  movupd [rsp], xmm6
  movupd [rsp + 16], xmm7
  movupd [rsp + 32], xmm8
  movupd [rsp + 48], xmm9
  movupd [rsp + 64], xmm10
  movupd [rsp + 80], xmm11
  movupd [rsp + 96], xmm12
  mov [rsp + 112], rbx
  mov [rsp + 120], rsi
  mov [rsp + 128], r12

  movdqu xmm12, cImSgnMaskF32
  movss xmm8, [r8]          // xmm8 <- c5_1
  movss xmm9, [r8 + 4]      // xmm9 <- c5_2
  movlhps xmm8, xmm9
  pshufd xmm8, xmm8, $a0    // xmm8 <- (c5_1, c5_1, c5_2, c5_2)
  pshufd xmm9, xmm8, $4e    // xmm9 <- (c5_2, c5_2, c5_1, c5_1)
  movss xmm10, [r8 + 8]     // xmm10 <- c5_3
  movss xmm11, [r8 + 12]    // xmm11 <- c5_4
  movlhps xmm10, xmm11
  pshufd xmm10, xmm10, $a0  // xmm10 <- (c5_3, c5_3, c5_4, c5_4)
  pshufd xmm11, xmm10, $4e  // xmm11 <- (c5_4, c5_4, c5_3, c5_3)
  movdqu xmm7, cHiC64SgnMask
  xorps xmm11, xmm7         // xmm11 <- (c5_4, c5_4, -c5_3, -c5_3)

  mov rax, [rdx + 16] // RAX <- N/5
  shl rax, 3          // RAX <- (N/5)*SizeOf(TCmplx64)
  mov r8, [rcx]       // R8  <- @data[0]
  lea r9, r8 + rax    // R9  <- @data[N/5]
  lea r10, r9 + rax   // R10 <- @data[2N/5]
  lea r11, r10 + rax  // R11 <- @data[3N/5]
  lea r12, r11 + rax  // R12 <- @data[4N/5]
  shr rax, 3          // RAX <- N/5
  mov rcx, [rdx + 8]  // RCX <- w.stride
  mov rdx, [rdx]      // RDX <- @w
  xor rbx, rbx        // RBX <- k*w.stride
@L:
  movq xmm1, [rdx + rbx]    // xmm1 <- w1
  pshufd xmm1, xmm1, $14    // xmm1 <- (w1r, w1i, w1i, w1r)
  movq xmm2, [r9]           // xmm2 <- (ar, ai) := (fft1[k].re, fft1[k].im)
  pshufd xmm2, xmm2, $50    // xmm2 <- (ar, ar, ai, ai)
  mulps xmm1, xmm2          // xmm1 <- (w1r*ar, w1i*ar, w1i*ai, w1r*ai)
  movhlps xmm2, xmm1        // xmm2 <- (w1i*ai, w1r*ai)
  addsubps xmm1, xmm2       // xmm1 <- w*fft1[k]

  movq xmm3, [rdx + 2*rbx]  // xmm3 <- w2 = w^2 = w[2*k]
  pshufd xmm3, xmm3, $14    // xmm3 <- (w2r, w2i, w2i, w2r)
  movq xmm4, [r10]          // xmm4 <- (ar, ai) := (fft2[k].re, fft2[k].im)
  pshufd xmm4, xmm4, $50    // xmm4 <- (ar, ar, ai, ai)
  mulps xmm3, xmm4          // xmm3 <- (w2r*ar, w2i*ar, w2i*ai, w2r*ai)
  movhlps xmm2, xmm3        // xmm2 <- (w2i*ai, w2r*ai)
  addsubps xmm3, xmm2
  movq xmm2, xmm3           // xmm2 <- w2*fft2[k]

  lea rsi, 2*rbx + rbx
  movq xmm5, [rdx + rsi]    // xmm5 <- w3 := w[k]^3 = w[3*k]
  pshufd xmm5, xmm5, $14    // xmm5 <- (w3r, w3i, w3i, w3r)
  movq xmm6, [r11]          // xmm6 <- (ar, ai) := (fft3[k].re, fft3[k].im)
  pshufd xmm6, xmm6, $50    // xmm6 <- (ar, ar, ai, ai)
  mulps xmm5, xmm6          // xmm5 <- (w3r*ar, w3i*ar, w3i*ai, w3r*ai)
  movhlps xmm3, xmm5        // xmm3 <- (w2i*ai, w2r*ai)
  addsubps xmm5, xmm3
  movq xmm3, xmm5           // xmm3 <- w3*fft3[k]

  movq xmm4, [rdx + 4*rbx]  // xmm4 <- w4 = w^4 = w[4*k]
  pshufd xmm4, xmm4, $14    // xmm4 <- (w4r, w4i, w4i, w4r)
  movq xmm7, [r12]          // xmm7 <- (ar, ai) := (fft4[k].re, fft4[k].im)
  pshufd xmm7, xmm7, $50    // xmm7 <- (ar, ar, ai, ai)
  mulps xmm4, xmm7          // xmm4 <- (w4r*ar, w4i*ar, w4i*ai, w4r*ai)
  movhlps xmm7, xmm4        // xmm7 <- (w4i*ai, w4r*ai)
  addsubps xmm4, xmm7       // xmm4 <- w4*fft4[k]

  movupd xmm0, [r8]         // xmm0 <- x0 := fft0[k]

  // (xmm0, xmm1, xmm2, xmm3, xmm4) <- (x0, x1, x2, x3, x4)

  movaps xmm5, xmm4
  movaps xmm4, xmm1
  addps xmm1, xmm5          // xmm1 <- A := x1 + x4
  subps xmm4, xmm5          // xmm4 <- C := x1 - x4

  movaps xmm5, xmm3
  movaps xmm3, xmm2
  addps xmm2, xmm5          // xmm2 <- B <- x2 + x3
  subps xmm3, xmm5          // xmm3 <- D <- x2 - x3

  movaps xmm5, xmm0         // xmm5 <- x0
  addps xmm5, xmm1
  addps xmm5, xmm2
  movq [r8], xmm5           // fft0[k] := x0 + A + B

  movlhps xmm0, xmm0        // xmm5 <- (x0, x0)
  movlhps xmm1, xmm1        // xmm1 <- (A, A)
  movlhps xmm2, xmm2        // xmm2 <- (B, B)
  movlhps xmm3, xmm3        // xmm3 <- (D, D)
  movlhps xmm4, xmm4        // xmm4 <- (C, C)

  mulps xmm1, xmm8          // xmm1 <- (c5_1*A, c5_2*A)
  mulps xmm2, xmm9          // xmm2 <- (c5_2*B, c5_1*B)
  addps xmm1, xmm2          // xmm1 <- U = (c5_1*A + c5_2*B, c5_2*A + c5_1*B)
  addps xmm0, xmm1          // xmm5 <- (x0 + U[0], x0 + U[1])

  mulps xmm4, xmm10         // xmm4 <- (c5_3*C, c5_4*C)
  mulps xmm3, xmm11         // xmm3 <- (c5_4*D, -c5_3*D)
  addps xmm3, xmm4          // xmm3 <- T = (c5_3*C + c5_4*D, c5_4*C - c5_3*D)
  pshufd xmm3, xmm3, $b1    // xmm3 <- (T[0].im, T[0].re, T[1].im, T[1].re)
  xorps xmm3, xmm12          // xmm3 <- T' = (T[0].im, -T[0].re, T[1].im, -T[1].re)

  movaps xmm6, xmm0         // xmm6 <- x0 + U
  subps xmm6, xmm3          // xmm6 <- x0 + U - T'
  movq [r9], xmm6           // fft1[0] <- x0 + U[0] - T'[0]
  movhlps xmm6, xmm6
  movq [r10], xmm6          // fft2[0] <- x0 + U[1] - T'[1]

  addps xmm0, xmm3          // xmm5 <- x0 + U + T'
  movq [r12], xmm0          // fft4[0] <- x0 + U[0] + T'[0]
  movhlps xmm0, xmm0
  movq [r11], xmm0          // fft3[0] <- x0 + U[1] + T'[1]

  add r8, 8
  add r9, 8
  add r10, 8
  add r11, 8
  add r12, 8
  add rbx, rcx
  dec rax
  jnz @L

  movupd xmm6, [rsp]
  movupd xmm7, [rsp + 16]
  movupd xmm8, [rsp + 32]
  movupd xmm9, [rsp + 48]
  movupd xmm10, [rsp + 64]
  movupd xmm11, [rsp + 80]
  movupd xmm12, [rsp + 96]
  mov rbx, [rsp + 112]
  mov rsi, [rsp + 120]
  mov r12, [rsp + 128]
  add rsp, 136
end;
{$else}
var
  k, N5: NativeInt;
  x0, x1, x2, x3, x4: TCmplx64;
  A, B, C, D, T, U, tmp: TCmplx64;
  fft0, fft1, fft2, fft3, fft4: TVecC64;
begin
  N5 := w.Length;

  fft0 := data.Span(0, N5);
  fft1 := data.Span(N5, N5);
  fft2 := data.Span(2*N5, N5);
  fft3 := data.Span(3*N5, N5);
  fft4 := data.Span(4*N5, N5);

  for k := 0 to N5 - 1 do begin
    x0 := fft0[k];
    x1 := fft1[k] * w[k];
    x2 := fft2[k] * w[2*k];
    x3 := fft3[k] * w[3*k];
    x4 := fft4[k] * w[4*k];

    A := x1 + x4;
    B := x2 + x3;
    C := x1 - x4;
    D := x2 - x3;

    fft0[k] := x0 + A + B;

    U := A*c5._1 + B*c5._2;
    T := C*c5._3 + D*c5._4;
    tmp.Re := x0.Re + U.Re - T.Im;
    tmp.Im := x0.Im + U.Im + T.Re;
    fft1[k] := tmp;

    tmp.Re := x0.Re + U.Re + T.Im;
    tmp.Im := x0.Im + U.Im - T.Re;
    fft4[k] := tmp;

    U := A*c5._2 + B*c5._1;
    T := C*c5._4 - D*c5._3;
    tmp.Re := x0.Re + U.Re - T.Im;
    tmp.Im := x0.Im + U.Im + T.Re;
    fft2[k] := tmp;

    tmp.Re := x0.Re + U.Re + T.Im;
    tmp.Im := x0.Im + U.Im - T.Re;
    fft3[k] := tmp;
  end;
end;
{$endif}

procedure _fftcomb5(const data, w: TVecC64); overload;
begin
  __fftcomb5(data, w, cC5s);
end;

procedure _ifftcomb5(const data, w: TVecC64); overload;
begin
  __fftcomb5(data, w, cC5si);
end;

procedure __fftcomb5(const data, w: TVecC128; const c5: TC5d); overload;
{$if defined(ASMx64)}
// RCX <- @data, RDX <- @w, r8 <- @c5
asm
  sub rsp, 136 // 112 for xmm6..xmm12 backup + 24 for RBX, RSI, R12
  movupd [rsp], xmm6
  movupd [rsp + 16], xmm7
  movupd [rsp + 32], xmm8
  movupd [rsp + 48], xmm9
  movupd [rsp + 64], xmm10
  movupd [rsp + 80], xmm11
  movupd [rsp + 96], xmm12
  mov [rsp + 112], rbx
  mov [rsp + 120], rsi
  mov [rsp + 128], r12

  movddup xmm9, [r8]        // xmm9  <- (c5._1, c5._1)
  movddup xmm10, [r8 + 8]   // xmm10 <- (c5._2, c5._2)
  movddup xmm11, [r8 + 16]  // xmm11 <- (c5._3, c5._3)
  movddup xmm12, [r8 + 24]  // xmm12 <- (c5._4, c5._4)

  mov rax, [rdx + 16] // RAX <- N/5
  shl rax, 4          // RAX <- (N/5)*SizeOf(TCmplx128)
  mov r8, [rcx]       // R8  <- @data[0]
  lea r9, r8 + rax    // R9  <- @data[N/5]
  lea r10, r9 + rax   // R10 <- @data[2N/5]
  lea r11, r10 + rax  // R11 <- @data[3N/5]
  lea r12, r11 + rax  // R12 <- @data[4N/5]
  shr rax, 4          // RAX <- N/5
  mov rcx, [rdx + 8]  // RCX <- w.stride
  mov rdx, [rdx]      // RDX <- @w
  xor rbx, rbx        // RBX <- k*w.stride
  movdqu xmm8, cImSgnMaskF64
@L:
  movupd xmm0, [rdx + rbx]  // xmm0 <- w1
  movddup xmm1, [r9]        // xmm1 <- (ar, ar) := (fft1[k].re, fft1[k].re)
  movddup xmm2, [r9 + 8]    // xmm2 <- (ai, ai) := (fft1[k].im, fft1[k].im)
  mulpd xmm1, xmm0          // xmm1 <- (ar*wr, ar*wi)
  mulpd xmm2, xmm0          // xmm2 <- (ai*wr, ai*wi)
  pshufd xmm2, xmm2, $4e    // xmm2 <- (ai*wi, ai*wr)
  addsubpd xmm1, xmm2       // xmm1 <- x1 := fft1[k] * w1

  movupd xmm2, [rdx + 2*rbx]// xmm0 <- w2 = w^2 = w[2*k]
  movddup xmm3, [r10]       // xmm3 <- (ar, ar) := (fft2[k].re, fft2[k].re)
  movddup xmm4, [r10 + 8]   // xmm4 <- (ai, ai) := (fft2[k].im, fft2[k].im)
  mulpd xmm3, xmm2          // xmm3 <- (ar*w2r, ar*w2i)
  mulpd xmm4, xmm2          // xmm4 <- (ai*w2r, ai*w2i)
  pshufd xmm4, xmm4, $4e    // xmm4 <- (ai*w2i, ai*w2r)
  addsubpd xmm3, xmm4
  movapd xmm2, xmm3         // xmm2 <- x2 := fft2[k]*w2

  lea rsi, 2*rbx + rbx
  movupd xmm0, [rdx + rsi]  // xmm2 <- w3 := w[k]^3 = w[3*k]
  movddup xmm3, [r11]       // xmm3 <- (ar, ar) := (fft3[k].re, fft3[k].re)
  movddup xmm4, [r11 + 8]   // xmm4 <- (ai, ai) := (fft3[k].im, fft3[k].im)
  mulpd xmm3, xmm0          // xmm3 <- (ar*w3r, ar*w3i)
  mulpd xmm4, xmm0          // xmm4 <- (ai*w3r, ai*w3i)
  pshufd xmm4, xmm4, $4e    // xmm4 <- (ai*w3i, ai*w3r)
  addsubpd xmm3, xmm4       // xmm3 <- x3 := fft3[k]*w3

  movupd xmm0, [rdx + 4*rbx]// xmm0 <- w4 := w[k]^4 = w[4*k]
  movddup xmm4, [r12]       // xmm4 <- (ar, ar) := (fft4[k].re, fft4[k].re)
  movddup xmm5, [r12 + 8]   // xmm5 <- (ai, ai) := (fft4[k].im, fft4[k].im)
  mulpd xmm4, xmm0          // xmm4 <- (ar*w4r, ar*w4i)
  mulpd xmm5, xmm0          // xmm5 <- (ai*w4r, ai*w4i)
  pshufd xmm5, xmm5, $4e    // xmm5 <- (ai*w4i, ai*w4r)
  addsubpd xmm4, xmm5       // xmm4 <- x4 := fft4[k]*w4

  movupd xmm0, [r8]         // xmm0 <- x0 := fft0[k]

  // (xmm0, xmm1, xmm2, xmm3, xmm4) <- (x0, x1, x2, x3, x4)

  movapd xmm5, xmm4
  movapd xmm4, xmm1
  addpd xmm1, xmm5          // xmm1 <- A := x1 + x4
  subpd xmm4, xmm5          // xmm4 <- C := x1 - x4

  movapd xmm5, xmm3
  movapd xmm3, xmm2
  addpd xmm2, xmm5          // xmm2 <- B <- x2 + x3
  subpd xmm3, xmm5          // xmm3 <- D <- x2 - x3

  movapd xmm5, xmm0         // xmm5 <- x0
  addpd xmm5, xmm1
  addpd xmm5, xmm2
  movupd [r8], xmm5         // fft0[k] := x0 + A + B

  movddup xmm5, xmm9        // xmm5 <- (c5_1, c5_1)
  movddup xmm6, xmm10       // xmm6 <- (c5_2, c5_2)
  mulpd xmm5, xmm1
  mulpd xmm6, xmm2
  addpd xmm5, xmm6          // xmm5 <- U := A*c5_1 + B*c5_2

  movddup xmm6, xmm11       // xmm6 <- (c5_3, c5_3)
  movddup xmm7, xmm12       // xmm7 <- (c5_4, c5_4)
  mulpd xmm6, xmm4
  mulpd xmm7, xmm3
  addpd xmm6, xmm7          // xmm6 <- T := C*c5_3 + D*c5_4
  pshufd xmm6, xmm6, $4e    // xmm6 <- (T.im, T.re)
  xorpd xmm6, xmm8          // xmm6 <- T' := (T.im, -T.re)

  movapd xmm7, xmm0         // xmm7 <- x0
  addpd xmm7, xmm5
  subpd xmm7, xmm6
  movupd [r9], xmm7         // fft1[k] <- x0 + U - T'

  movapd xmm7, xmm0
  addpd xmm7, xmm5
  addpd xmm7, xmm6
  movupd [r12], xmm7        // fft4[k] := x0 + U + T'

  mulpd xmm1, xmm10         // xmm1 <- A*c5_2
  mulpd xmm2, xmm9          // xmm2 <- B*c5_1
  addpd xmm1, xmm2          // xmm1 <- U := A*c5_2 + B*c5_1

  mulpd xmm4, xmm12         // xmm4 <- C*c5_4
  mulpd xmm3, xmm11         // xmm3 <- D*c5_3
  subpd xmm4, xmm3          // xmm4 <- T := C*c5_4 - D*c5_3
  pshufd xmm4, xmm4, $4e
  xorpd xmm4, xmm8          // xmm4 <- T' := (T.im, -T.re)

  movapd xmm7, xmm0
  addpd xmm7, xmm1
  subpd xmm7, xmm4
  movupd [r10], xmm7        // fft2[k] := x0 + U - T'

  movapd xmm7, xmm0
  addpd xmm7, xmm1
  addpd xmm7, xmm4
  movupd [r11], xmm7        // fft3[k] := x0 + U + T'

  add r8, 16
  add r9, 16
  add r10, 16
  add r11, 16
  add r12, 16
  add rbx, rcx
  dec rax
  jnz @L

  movupd xmm6, [rsp]
  movupd xmm7, [rsp + 16]
  movupd xmm8, [rsp + 32]
  movupd xmm9, [rsp + 48]
  movupd xmm10, [rsp + 64]
  movupd xmm11, [rsp + 80]
  movupd xmm12, [rsp + 96]
  mov rbx, [rsp + 112]
  mov rsi, [rsp + 120]
  mov r12, [rsp + 128]
  add rsp, 136
end;
{$else}
var
  k, N5: NativeInt;
  x0, x1, x2, x3, x4: TCmplx128;
  A, B, C, D, T, U, tmp: TCmplx128;
  fft0, fft1, fft2, fft3, fft4: TVecC128;
begin
  N5 := w.Length;

  fft0 := data.Span(0, N5);
  fft1 := data.Span(N5, N5);
  fft2 := data.Span(2*N5, N5);
  fft3 := data.Span(3*N5, N5);
  fft4 := data.Span(4*N5, N5);

  for k := 0 to N5 - 1 do begin
    x0 := fft0[k];
    x1 := fft1[k] * w[k];
    x2 := fft2[k] * w[2*k];
    x3 := fft3[k] * w[3*k];
    x4 := fft4[k] * w[4*k];

    A := x1 + x4;
    B := x2 + x3;
    C := x1 - x4;
    D := x2 - x3;

    fft0[k] := x0 + A + B;

    U := A*c5._1 + B*c5._2;
    T := C*c5._3 + D*c5._4;
    tmp.Re := x0.Re + U.Re - T.Im;
    tmp.Im := x0.Im + U.Im + T.Re;
    fft1[k] := tmp;

    tmp.Re := x0.Re + U.Re + T.Im;
    tmp.Im := x0.Im + U.Im - T.Re;
    fft4[k] := tmp;

    U := A*c5._2 + B*c5._1;
    T := C*c5._4 - D*c5._3;
    tmp.Re := x0.Re + U.Re - T.Im;
    tmp.Im := x0.Im + U.Im + T.Re;
    fft2[k] := tmp;

    tmp.Re := x0.Re + U.Re + T.Im;
    tmp.Im := x0.Im + U.Im - T.Re;
    fft3[k] := tmp;
  end;
end;
{$endif}

procedure _fftcomb5(const data, w: TVecC128); overload;
begin
  __fftcomb5(data, w, cC5d);
end;

procedure _ifftcomb5(const data, w: TVecC128); overload;
begin
  __fftcomb5(data, w, cC5di);
end;

procedure RealFTRecombFull(const Z, W, X: TVecC64); overload;
var k, N: NativeInt;
    A, B, E, O, T: TCmplx64;
const I: TCmplx64 = (Re: 0.0; Im: 1.0);
begin
  N := Z.Length;
  X[0] := Z[0].Re + Z[0].Im;
  X[N] := Z[0].Re - Z[0].Im;

  for k := 1 to N - 1 do begin
    A := Z[k];
    B := Z[N - k].Conjugate;

    E :=  0.5*(A + B);
    O := -0.5*I*(A - B);

    T := W[k] * O;
    X[k]     := E + T;
    X[k + N] := E - T;
  end;
end;

// Z = FFT(z) = FFT(x[0::] + I*x[1::]), W[k] = Exp(2*Pi*I*k/N), X = FFT(X)
// Assumed: X.Length = Z.length && X.Stride = Z.Stride = SizeOf(TCmplx128)
// Procedure makes a full spectrum (including also X[k + N/2] values)
procedure RealFTRecombFull(const Z, W, X: TVecC128); overload;
{$if defined(ASMx64)}
// RCX <- @Z, RDX <- @W, R8 <- @X
const
  cHalf: Double = 0.5;
asm
  sub rsp, 24  // 16 for RSI, RDI + 8 for stack alignment
  mov [rsp], rsi
  mov [rsp + 8], rdi

  mov rsi, [rcx]            // RSI <- @Z[0]
  mov rdi, [r8]             // RDI <- @X[0]
  mov rax, [rdx]            // RAX <- @W[0]
  mov rdx, [rdx + 8]        // RDX <- w.Stride
  mov rcx, [rcx + 16]       // RCX <- N

  movddup xmm0, [rsi]       // xmm0 <- (Z[0].re, Z[0].re)
  movddup xmm1, [rsi + 8]   // xmm1 <- (Z[0].im, Z[0].im)
  addsubpd xmm0, xmm1       // xmm0 <- (Z[0].re - Z[0].im, Z[0].re + Z[0].im)
  xorpd xmm1, xmm1
  movhlps xmm1, xmm0
  movupd [rdi], xmm1        // X[0] <- (Z[0].re + Z[0].i, 0)
  movsd xmm1, xmm0
  mov r9, rcx
  shl r9, 4                 // R9 <- N * SizeOf(TCmplx128)
  movupd [rdi + r9], xmm1   // X[N] <- (Z[0].re - Z[0].im, 0)

  dec ecx                   // ECX <- N - 1
  mov r10, 16               // R10 <- k*SizeOf(TCmplx128), k := 1
  add rax, 16               // RAX <- @W[1]
  movupd xmm5, cImSgnMaskF64
  movddup xmm4, cHalf
@L:
  movupd xmm0, [rsi + r10]  // xmm0 <- A := Z[k]
  mov r11, r9
  sub r11, r10              // R11 <- (N - k) * SizeOf(TCmplx128)
  movupd xmm1, [rsi + r11]  // xmm1 <- Z[N - k]
  xorpd xmm1, xmm5          // xmm1 <- B := Z[N - k]*
  movapd xmm2, xmm1
  movapd xmm1, xmm0
  addpd xmm0, xmm2
  mulpd xmm0, xmm4          // xmm0 <- E := 1/2(A + B)
  subpd xmm1, xmm2
  mulpd xmm1, xmm4
  pshufd xmm1, xmm1, $4e
  xorpd xmm1, xmm5          // xmm1 <- O := 1/2*I*(A - B)

  movddup xmm2, [rax]       // xmm2 <- (wr, wr) := (W[k].re, W[k].re)
  movddup xmm3, [rax + 8]   // xmm3 <- (wi, wi) := (W[k].im, W[k].im)
  mulpd xmm2, xmm1          // xmm2 <- (wr*O.re, wr*O.im)
  mulpd xmm3, xmm1          // xmm3 <- (wi*O.re, wi*O.im)
  pshufd xmm3, xmm3, $4e    // xmm3 <- (wi*O.im, wi*O.re)
  addsubpd xmm2, xmm3       // xmm3 <- T := W[k]*O

  movapd xmm1, xmm0
  addpd xmm0, xmm2
  movupd [rdi + r10], xmm0  // X[k] <- E + T
  lea r11, r10 + r9         // R11 <- (N + k) * SizeOf(TCmplx128)
  subpd xmm1, xmm2
  movupd [rdi + r11], xmm1  // X[k + N] <- E - T

  add r10, 16               // k*SizeOf(TCmplx128)++
  add rax, rdx              // k*w.Stride++
  dec ecx
  jnz @L

  mov rsi, [rsp]
  mov rdi, [rsp + 8]
  add rsp, 24
end;
{$else}
var k, N: NativeInt;
    A, B, E, O, T: TCmplx128;
begin
  N := Z.Length;
  X[0] := Z[0].Re + Z[0].Im;
  X[N] := Z[0].Re - Z[0].Im;

  for k := 1 to N - 1 do begin
    A := Z[k];
    B := Z[N - k].Conjugate;

    E :=  0.5*(A + B);
    O := -0.5*I*(A - B);

    T := W[k] * O;
    X[k]     := E + T;
    X[k + N] := E - T;
  end;
end;
{$endif}

procedure RealFTRecombHalf(const Z, W, X: TVecC64); overload;
{$if defined(ASMx64)}
// RCX <- @Z, RDX <- @W, R8 <- @X
const
  cHalf: array [0..3] of Single = (0.5, 0.5, 0.5, 0.5);
asm
  sub rsp, 24  // 16 for RSI, RDI + 8 for stack alignment
  mov [rsp], rsi
  mov [rsp + 8], rdi

  mov rsi, [rcx]            // RSI <- @Z[0]
  mov rdi, [r8]             // RDI <- @X[0]
  mov rax, [rdx]            // RAX <- @W[0]
  mov rdx, [rdx + 8]        // RDX <- w.Stride
  mov rcx, [rcx + 16]       // RCX <- N

  xorps xmm0, xmm0
  movd xmm0, [rsi]          // xmm0 <- Z[0].re
  movaps xmm1, xmm0
  movd xmm2, [rsi + 4]      // xmm2 <- Z[0].im
  addss xmm0, xmm2          // xmm0 <- Z[0].re - Z[0].im
  movq [rdi], xmm0          // X[0] <- (Z[0].re + Z[0].im, 0)
  subss xmm1, xmm2          // zmm1 <- Z[0].re - Z[0].im
  mov r9, rcx
  shl r9, 3
  movq [rdi + r9], xmm1     // X[N] <- (Z[0].re - Z[0].im, 0)

  dec rcx                   // ECX <- N - 1
  mov r10, 8                // R10 <- k*SizeOf(TCmplx64), k := 1
  add rax, 8                // RAX <- @W[1]
  movups xmm5, cImSgnMaskF32
  movups xmm4, cHalf
@L:
  movq xmm0, [rsi + r10]    // xmm0 <- A := Z[k]
  mov r11, r9
  sub r11, r10              // R11 <- (N - k) * SizeOf(TCmplx64)
  movq xmm1, [rsi + r11]    // xmm1 <- Z[N - k]
  xorps xmm1, xmm5          // xmm1 <- B := Z[N - k]*

  movaps xmm2, xmm0
  addps xmm2, xmm1          // xmm2 <- A + B
  subps xmm0, xmm1          // xmm0 <- A - B
  movlhps xmm0, xmm2        // xmm0 <- (A - B, A + B)
  mulps xmm0, xmm4          // xmm0 <- (1/2(A - B), 1/2(A + B))
  movhlps xmm2, xmm0        // xmm2 <- E := 1/2(A + B)
  pshufd xmm0, xmm0, $1
  xorps xmm0, xmm5          // xmm0 <- O := 1/2*I*(A - B)

  movq xmm3, [rax]          // xmm3 <- (wr, wi)
  pshufd xmm3, xmm3, $14    // xmm3 <- (wr, wi, wi, wr)
  pshufd xmm1, xmm0, $50    // xmm1 <- (or, or, oi, oi)
  mulps xmm3, xmm1          // xmm3 <- (or*wr, or*wi, oi*wi, oi*wr)
  movhlps xmm0, xmm3        // xmm4 <- (oi*wi, oi*wr)
  addsubps xmm3, xmm0       // xmm3 <- T := W[k]*O

  addps xmm2, xmm3
  movq [rdi + r10], xmm2  // X[k] <- E + T

  add r10, 8               // k*SizeOf(TCmplx64)++
  add rax, rdx              // k*w.Stride++
  dec ecx
  jnz @L

  mov rsi, [rsp]
  mov rdi, [rsp + 8]
  add rsp, 24
end;
{$else}
var k, N: NativeInt;
    A, B, E, O, T: TCmplx64;
const I: TCmplx64 = (Re: 0.0; Im: 1.0);
begin
  N := Z.Length;
  X[0] := Z[0].Re + Z[0].Im;
  X[N] := Z[0].Re - Z[0].Im;

  for k := 1 to N - 1 do begin
    A := Z[k];
    B := Z[N - k].Conjugate;

    E :=  0.5*(A + B);
    O := -0.5*I*(A - B);

    T := W[k] * O;
    X[k] := E + T;
  end;
end;
{$endif}

// Z = FFT(z) = FFT(x[0::] + I*x[1::]), W[k] = Exp(2*Pi*I*k/N), X = FFT(X)
// Assumed: X.Length = Z.length && X.Stride = Z.Stride = SizeOf(TCmplx128)
// Procedure makes only a Hermitian half-spectrum (without X[k + N/2] values)
procedure RealFTRecombHalf(const Z, W, X: TVecC128); overload;
{$if defined(ASMx64)}
// RCX <- @Z, RDX <- @W, R8 <- @X
const
  cHalf: Double = 0.5;
asm
  sub rsp, 24  // 16 for RSI, RDI + 8 for stack alignment
  mov [rsp], rsi
  mov [rsp + 8], rdi

  mov rsi, [rcx]            // RSI <- @Z[0]
  mov rdi, [r8]             // RDI <- @X[0]
  mov rax, [rdx]            // RAX <- @W[0]
  mov rdx, [rdx + 8]        // RDX <- w.Stride
  mov rcx, [rcx + 16]       // RCX <- N

  movddup xmm0, [rsi]       // xmm0 <- (Z[0].re, Z[0].re)
  movddup xmm1, [rsi + 8]   // xmm1 <- (Z[0].im, Z[0].im)
  addsubpd xmm0, xmm1       // xmm0 <- (Z[0].re - Z[0].im, Z[0].re + Z[0].im)
  xorpd xmm1, xmm1
  movhlps xmm1, xmm0
  movupd [rdi], xmm1        // X[0] <- (Z[0].re + Z[0].im, 0)
  movsd xmm1, xmm0
  mov r9, rcx
  shl r9, 4                 // R9 <- N * SizeOf(TCmplx128)
  movupd [rdi + r9], xmm1   // X[N] <- (Z[0].re - Z[0].im, 0)

  dec rcx                   // ECX <- N - 1
  mov r10, 16               // R10 <- k*SizeOf(TCmplx128), k := 1
  add rax, 16               // RAX <- @W[1]
  movupd xmm5, cImSgnMaskF64
  movddup xmm4, cHalf
@L:
  movupd xmm0, [rsi + r10]  // xmm0 <- A := Z[k]
  mov r11, r9
  sub r11, r10              // R11 <- (N - k) * SizeOf(TCmplx128)
  movupd xmm1, [rsi + r11]  // xmm1 <- Z[N - k]
  xorpd xmm1, xmm5          // xmm1 <- B := Z[N - k]*
  movapd xmm2, xmm1
  movapd xmm1, xmm0
  addpd xmm0, xmm2
  mulpd xmm0, xmm4          // xmm0 <- E := 1/2(A + B)
  subpd xmm1, xmm2
  mulpd xmm1, xmm4
  pshufd xmm1, xmm1, $4e
  xorpd xmm1, xmm5          // xmm1 <- O := 1/2*I*(A - B)

  movddup xmm2, [rax]       // xmm2 <- (wr, wr) := (W[k].re, W[k].re)
  movddup xmm3, [rax + 8]   // xmm3 <- (wi, wi) := (W[k].im, W[k].im)
  mulpd xmm2, xmm1          // xmm2 <- (wr*O.re, wr*O.im)
  mulpd xmm3, xmm1          // xmm3 <- (wi*O.re, wi*O.im)
  pshufd xmm3, xmm3, $4e    // xmm3 <- (wi*O.im, wi*O.re)
  addsubpd xmm2, xmm3       // xmm3 <- T := W[k]*O

  addpd xmm0, xmm2
  movupd [rdi + r10], xmm0  // X[k] <- E + T

  add r10, 16               // k*SizeOf(TCmplx128)++
  add rax, rdx              // k*w.Stride++
  dec ecx
  jnz @L

  mov rsi, [rsp]
  mov rdi, [rsp + 8]
  add rsp, 24
end;
{$else}
var k, N: NativeInt;
    A, B, E, O: TCmplx128;
begin
  N := Z.Length;
  X[0] := Z[0].Re + Z[0].Im;
  X[N] := Z[0].Re - Z[0].Im;

  for k := 1 to N - 1 do begin
    A := Z[k];
    B := Z[N - k].Conjugate;

    E :=  0.5*(A + B);
    O := -0.5*I*(A - B);

    X[k] := E + W[k] * O;
  end;
end;
{$endif}

procedure RealFTReconstruct(const Z, W, X: TVecC64); overload;
{$if defined(ASMx64)}
// RCX <- @Z, RDX <- @W, R8 <- @X
asm
  sub rsp, 56        // 32 for xmm6, xmm7 + 16 for RSI, RDI + 8 for stack alignment
  mov [rsp], rsi
  mov [rsp + 8], rdi
  movupd [rsp + 16], xmm6
  movupd [rsp + 32], xmm7

  mov rsi, [rcx]          // RSI <- @Z[0]
  mov rdi, [r8]           // RDI <- @X[0]
  mov rax, [rdx]          // RAX <- @W[0]
  mov r9, [r8 + 16]       // R9 <- N := X.Length
  mov r8, r9
  shr r8, 1               // R8 <- N2 := N div 2
  mov r10, r9             // R10 <- N
  shl r10, 3              // R10 <- N * SizeOf(TCmplx64)
  lea r11, rdi + r10      // R11 <- @X[N]
  lea r10, rsi + r10      // R10 <- @Z[N]

  movd xmm0, [rsi]
  pshufd xmm0, xmm0, 0        // xmm0 <- (Z[0].Re, Z[0].Re)
  movd xmm1, [r10]
  pshufd xmm1, xmm1, 0        // xmm1 <- (Z[N].Re, Z[N].Re)
  addsubps xmm0, xmm1         // xmm0 <- (Z[0].Re - Z[N].Re, Z[0].Re + Z[N].Re)
  pshufd xmm0, xmm0, 1
  movq [rdi], xmm0            // X[0] <- (Z[0].Re + Z[N].Re, Z[0].Re - Z[N].Re)

  movups xmm6, cImSgnMaskF32
  movups xmm7, cReSgnMaskF32
  add rsi, 8                  // RSI <- @Z[1]
  sub r10, 8                  // R10 <- @Z[N - 1]
  add rdi, 8                  // RDI <- @X[1]
  sub r11, 8                  // R11 <- @X[N - 1]
  add rax, 8                  // RAX <_ @W[1]
  dec r8
@L:
  movq xmm0, [rsi]          // xmm0 <- A := Z[k]
  movq xmm1, xmm0
  movq xmm2, [r10]            // xmm1 <- Z[N - k]
  xorps xmm2, xmm6            // xmm1 <- B := Z[N - k]*
  addps xmm0, xmm2            // xmm0 <- S := A + B
  subps xmm1, xmm2            // xmm1 <- D := A - B

  movq xmm3, [rax]            // xmm3 <- (wr, wi)
  pshufd xmm3, xmm3, $14      // xmm3 <- (wr, wi, wi, wr)
  pshufd xmm1, xmm1, $50      // xmm1 <- (dr, dr, di, di)
  mulps xmm3, xmm1            // xmm3 <- (wr*dr, wi*dr, wi*di, wr*di)
  movhlps xmm1, xmm3          // xmm1 <- (wi*di, wr*di)
  addsubps xmm3, xmm1         // xmm3 <- W[k]*(A - B)

  pshufd xmm4, xmm3, 1
  xorps xmm4, xmm7            // xmm4 <- (-Di, Dr) == I*D
  addps xmm4, xmm0            // xmm4 <- S + I*D
  movq [rdi], xmm4

  pshufd xmm3, xmm3, 1        // xmm3 <- (Di, Dr) == I*D.conj
  xorps xmm0, xmm6            // xmm0 <- S.conj
  addps xmm3, xmm0            // xmm3 <- S.conj + I*D.conj
  movq [r11], xmm3

  add rdi, 8
  add rsi, 8
  add rax, 8
  sub r10, 8
  sub r11, 8
  dec r8
  jnz @L

  movd xmm0, [rsi]            // xmm0 <- A[N2].Re
  movd xmm1, [rsi + 4]        // xmm1 <- A[N2].Im
  pshufd xmm1, xmm1, 0        // xmm1 <- (A[N2].Im, A[N2].Im)
  movq xmm2, [rax]            // xmm2 <- W[N2]
  mulps xmm2, xmm1
  subps xmm0, xmm2            // (A[N2].Re, 0) - (A[N2].Im*wr, A[N2].Im*wi)
  addps xmm0, xmm0
  movq [rdi], xmm0

  mov rsi, [rsp]
  mov rdi, [rsp + 8]
  movupd xmm6, [rsp + 16]
  movupd xmm7, [rsp + 32]
  add rsp, 56
end;
{$else}
var k, N, N2: NativeInt;
    A, B, D, S, Wk: TCmplx64;
const I: TCmplx64 = (Re: 0.0; Im: 1.0);
begin
  N := X.Length;
  N2 := N div 2;
  A := Z[0];
  B := Z[N];
  D.Re := A.Re + B.Re;
  D.Im := A.Re - B.Re;
  X[0] := D;

  for k := 1 to N2 - 1 do begin
    A := Z[k];
    B := Z[N - k].Conjugate;
    Wk := W[k];
    S := A + B;
    D := Wk*(A - B);
    X[k] := S + I*D;
    X[N - k] := S.Conjugate + I*D.Conjugate;
  end;

  A := Z[N2];
  Wk := W[N2];
  X[N2] := 2*(A.Re - A.Im*Wk);
end;
{$endif}

procedure RealFTReconstruct(const Z, W, X: TVecC128); overload;
{$if defined(ASMx64)}
// RCX <- @Z, RDX <- @W, R8 <- @X
asm
  sub rsp, 56        // 32 for xmm6, xmm7 + 16 for RSI, RDI + 8 for stack alignment
  mov [rsp], rsi
  mov [rsp + 8], rdi
  movupd [rsp + 16], xmm6
  movupd [rsp + 32], xmm7

  mov rsi, [rcx]          // RSI <- @Z[0]
  mov rdi, [r8]           // RDI <- @X[0]
  mov rax, [rdx]          // RAX <- @W[0]
  mov r9, [r8 + 16]       // R9 <- N := X.Length
  mov r8, r9
  shr r8, 1               // R8 <- N2 := N div 2
  mov r10, r9             // R10 <- N
  shl r10, 4              // R10 <- N * SizeOf(TCmplx128)
  lea r11, rdi + r10      // R11 <- @X[N]
  lea r10, rsi + r10      // R10 <- @Z[N]

  movddup xmm0, [rsi]         // xmm0 <- (Z[0].Re, Z[0].Re)
  movddup xmm1, [r10]         // xmm1 <- (Z[N].Re, Z[N].Re)
  addsubpd xmm0, xmm1         // xmm0 <- (Z[0].Re - Z[N].Re, Z[0].Re + Z[N].Re)
  pshufd xmm0, xmm0, $4e
  movupd [rdi], xmm0          // X[0] <- (Z[0].Re + Z[N].Re, Z[0].Re - Z[N].Re)

  movupd xmm6, cImSgnMaskF64
  pshufd xmm7, xmm6, $4e      // xmm7 <- real signum mask
  add rsi, 16                 // RSI <- @Z[1]
  sub r10, 16                 // R10 <- @Z[N - 1]
  add rdi, 16                 // RDI <- @X[1]
  sub r11, 16                 // R11 <- @X[N - 1]
  add rax, 16                 // RAX <_ @W[1]
  dec r8
@L:
  movupd xmm0, [rsi]          // xmm0 <- A := Z[k]
  movapd xmm1, xmm0
  movupd xmm2, [r10]          // xmm1 <- Z[N - k]
  xorpd xmm2, xmm6            // xmm1 <- B := Z[N - k]*
  addpd xmm0, xmm2            // xmm0 <- S := A + B
  subpd xmm1, xmm2            // xmm1 <- D := A - B
  movddup xmm3, [rax]         // xmm3 <- (wr, wr)
  movddup xmm4, [rax + 8]     // xmm4 <- (wi, wi)
  mulpd xmm3, xmm1            // xmm3 <- (wr*dr, wr*di)
  mulpd xmm4, xmm1            // xmm4 <- (wi*dr, wi*di)
  pshufd xmm4, xmm4, $4e      // xmm4 <- (wi*di, wi*dr)
  addsubpd xmm3, xmm4         // xmm3 <- D := W[k]*(A - B)

  pshufd xmm4, xmm3, $4e
  xorpd xmm4, xmm7            // xmm4 <- (-Di, Dr) == I*D
  addpd xmm4, xmm0            // xmm4 <- S + I*D
  movupd [rdi], xmm4

  pshufd xmm3, xmm3, $4e      // xmm3 <- (Di, Dr) == I*D.conj
  xorps xmm0, xmm6            // xmm0 <- S.conj
  addpd xmm3, xmm0            // xmm3 <- S.conj + I*D.conj
  movupd [r11], xmm3

  add rdi, 16
  add rsi, 16
  add rax, 16
  sub r10, 16
  sub r11, 16
  dec r8
  jnz @L

  movq xmm0, [rsi]            // xmm0 <- A[N2].Re
  movddup xmm1, [rsi + 8]     // xmm1 <- (A[N2].Im, A[N2].Im)
  movupd xmm2, [rax]          // xmm2 <- W[N2]
  mulpd xmm2, xmm1
  subpd xmm0, xmm2            // (A[N2].Re, 0) - (A[N2].Im*wr, A[N2].Im*wi)
  addpd xmm0, xmm0
  movupd [rdi], xmm0

  mov rsi, [rsp]
  mov rdi, [rsp + 8]
  movupd xmm6, [rsp + 16]
  movupd xmm7, [rsp + 32]
  add rsp, 56
end;
{$else}
var k, N, N2: NativeInt;
    A, B, D, S, Wk: TCmplx128;
begin
  N := X.Length;
  N2 := N div 2;
  A := Z[0];
  B := Z[N];
  D.Re := A.Re + B.Re;
  D.Im := A.Re - B.Re;
  X[0] := D;

  for k := 1 to N2 - 1 do begin
    A := Z[k];
    B := Z[N - k].Conjugate;
    Wk := W[k];
    S := A + B;
    D := Wk*(A - B);
    X[k] := S + I*D;
    X[N - k] := S.Conjugate + I*D.Conjugate;
  end;

  A := Z[N2];
  Wk := W[N2];
  X[N2] := 2*(A.Re - A.Im*Wk);
end;
{$endif}

type
  TArrayF32x8 = array [0..7] of Single;
  PArrayF32x8 = ^TArrayF32x8;
  TArrayF64x8 = array [0..7] of Double;
  PArrayF64x8 = ^TArrayF64x8;

// Leaf radix-4 fft for Danielson-Lanzos ruotine
procedure fftN4(var aData: TArrayF32x8); overload; inline;
var y: TArrayF32x8;
begin
  y[0] := aData[0] + aData[2];
  y[1] := aData[1] + aData[3];
  y[2] := aData[4] + aData[6];
  y[3] := aData[5] + aData[7];

  y[4] := aData[0] - aData[2];
  y[5] := aData[1] - aData[3];
  y[6] := aData[4] - aData[6];
  y[7] := aData[5] - aData[7];

  aData[0] := y[0] + y[2];
  aData[1] := y[1] + y[3];
  aData[4] := y[0] - y[2];
  aData[5] := y[1] - y[3];

  aData[2] := y[4] - y[7];
  aData[3] := y[5] + y[6];
  aData[6] := y[4] + y[7];
  aData[7] := y[5] - y[6];
end;

procedure ifftN4(var aData: TArrayF32x8); overload; inline;
var y: TArrayF32x8;
begin
  y[0] := aData[0] + aData[2];
  y[1] := aData[1] + aData[3];
  y[2] := aData[4] + aData[6];
  y[3] := aData[5] + aData[7];

  y[4] := aData[0] - aData[2];
  y[5] := aData[1] - aData[3];
  y[6] := aData[4] - aData[6];
  y[7] := aData[5] - aData[7];

  aData[0] := y[0] + y[2];
  aData[1] := y[1] + y[3];
  aData[4] := y[0] - y[2];
  aData[5] := y[1] - y[3];

  aData[2] := y[4] + y[7];
  aData[3] := y[5] - y[6];
  aData[6] := y[4] - y[7];
  aData[7] := y[5] + y[6];
end;

procedure ifftN4(var aData: TArrayF64x8); overload; inline;
var y: TArrayF64x8;
begin
  y[0] := aData[0] + aData[2];
  y[1] := aData[1] + aData[3];
  y[2] := aData[4] + aData[6];
  y[3] := aData[5] + aData[7];

  y[4] := aData[0] - aData[2];
  y[5] := aData[1] - aData[3];
  y[6] := aData[4] - aData[6];
  y[7] := aData[5] - aData[7];

  aData[0] := y[0] + y[2];
  aData[1] := y[1] + y[3];
  aData[4] := y[0] - y[2];
  aData[5] := y[1] - y[3];

  aData[2] := y[4] + y[7];
  aData[3] := y[5] - y[6];
  aData[6] := y[4] - y[7];
  aData[7] := y[5] + y[6];
end;

procedure fftN4(var aData: TArrayF64x8); overload; inline;
var y: TArrayF64x8;
begin
  y[0] := aData[0] + aData[2];
  y[1] := aData[1] + aData[3];
  y[2] := aData[4] + aData[6];
  y[3] := aData[5] + aData[7];

  y[4] := aData[0] - aData[2];
  y[5] := aData[1] - aData[3];
  y[6] := aData[4] - aData[6];
  y[7] := aData[5] - aData[7];

  aData[0] := y[0] + y[2];
  aData[1] := y[1] + y[3];
  aData[4] := y[0] - y[2];
  aData[5] := y[1] - y[3];

  aData[2] := y[4] - y[7];
  aData[3] := y[5] + y[6];
  aData[6] := y[4] + y[7];
  aData[7] := y[5] - y[6];
end;

procedure DLW(const aData, aW: TVecC64);
{$if defined(ASMx64)}
// RCX <- @aData, RDX <- @aW
asm
  mov r9, [rcx + 16]      // R9 <- N = aData.Length
  mov rcx, [rcx]          // RCX <- @aData[0]
  mov rax, r9
  shr rax, 2              // RAX <- N div 4
  jnz @start
  movq xmm0, [rcx]        // xmm0  <- y[0]
  movq xmm1, xmm0
  movq xmm2, [rcx + 8]    // xmm2 <- y[1]
  addps xmm0, xmm2        // xmm0 <- y[0] + y[1]
  subps xmm1, xmm2        // xmm1 <- y[0] - y[1]
  movq [rcx], xmm0
  movq [rcx + 8], xmm1
  jmp @ExitN2

@start:
  push rbx
  push rsi
  push rdi
  push r12

  mov rdi, [rdx]          // RDI <- @aW[0]
  mov rsi, rcx            // RSI <- @aData[0]

  // 4-block fft is performed first
  mov rdx, r9             // RDX <- N
  shr rdx, 2              // RDX <- N div 4
  movups xmm5, cImSgnMaskF32

  mov eax, [rdi + 12]     // EAX <- W[1].Im
  test eax, $80000000
  jnz @ISTG1
@STG1:
  movups xmm0, [rcx]      // xmm0 <- (data[0], data[1])
  movups xmm2, [rcx + 16] // xmm2 <- (data[2], data[3])
  movq xmm1, xmm2         // xmm1 <- data[2]
  movhlps xmm2, xmm0      // xmm2 <- (x2, x3) := (data[1], data[3])
  movlhps xmm0, xmm1      // xmm1 <- (x0, x1) := (data[0], data[2])

  movaps xmm1, xmm0         // xmm1 <- (x0, x1)
  addps xmm0, xmm2          // xmm0 <- (t0, t2) := (x0 + x2, x1 + x3)
  subps xmm1, xmm2          // xmm1 <- (t1, t3) := (x0 - x2, x1 - x3)

  movhlps xmm2, xmm1        // xmm2 <- t3
  pshufd xmm2, xmm2, $11    // xmm2 <- (t3.im, t3.re, t3.im, t3.re)
  xorps xmm2, xmm5          // xmm2 <- (t3, t3) := (t3.im, -t3.re, t3.im, -t3.re)

  movhlps xmm2, xmm0        // xmm2 <- (t2, t3)
  movlhps xmm0, xmm1        // xmm1 <- (t0, t1)
  movaps xmm1, xmm0
  addps xmm0, xmm2          // xmm0 <- (t0 + t2, t1 + t3)
  subps xmm1, xmm2          // xmm1 <- (t0 - t2, t1 - t3)
  movhlps xmm3, xmm0        // xmm3 <- t1 + t3
  movhlps xmm4, xmm1        // xmm4 <- t1 - t3

  movq [rcx], xmm0         // data[0] <- t0 + t2
  movq [rcx + 24], xmm3    // data[3] <- t1 + t3
  movq [rcx + 16], xmm1    // data[2] <- t0 - t2
  movq [rcx + 8], xmm4     // data[1] <- t1 - t3

  add rcx, 32
  dec rdx
  jnz @STG1

  jmp @STG2Up

@ISTG1: // 4-block inverse fft loop
  movups xmm0, [rcx]      // xmm0 <- (data[0], data[1])
  movups xmm2, [rcx + 16] // xmm2 <- (data[2], data[3])
  movq xmm1, xmm2         // xmm1 <- data[2]
  movhlps xmm2, xmm0      // xmm2 <- (x2, x3) := (data[1], data[3])
  movlhps xmm0, xmm1      // xmm1 <- (x0, x1) := (data[0], data[2])

  movaps xmm1, xmm0         // xmm1 <- (x0, x1)
  addps xmm0, xmm2          // xmm0 <- (t0, t2) := (x0 + x2, x1 + x3)
  subps xmm1, xmm2          // xmm1 <- (t1, t3) := (x0 - x2, x1 - x3)

  movhlps xmm2, xmm1        // xmm2 <- t3
  pshufd xmm2, xmm2, $11    // xmm2 <- (t3.im, t3.re, t3.im, t3.re)
  xorps xmm2, xmm5          // xmm2 <- (t3, t3) := (t3.im, -t3.re, t3.im, -t3.re)

  movhlps xmm2, xmm0        // xmm2 <- (t2, t3)
  movlhps xmm0, xmm1        // xmm1 <- (t0, t1)
  movaps xmm1, xmm0
  addps xmm0, xmm2          // xmm0 <- (t0 + t2, t1 + t3)
  subps xmm1, xmm2          // xmm1 <- (t0 - t2, t1 - t3)
  movhlps xmm3, xmm0        // xmm3 <- t1 + t3
  movhlps xmm4, xmm1        // xmm4 <- t1 - t3

  movq [rcx], xmm0          // data[0] <- t0 + t2
  movq [rcx + 8], xmm3      // data[1] <- t1 + t3
  movq [rcx + 16], xmm1     // data[2] <- t0 - t2
  movq [rcx + 24], xmm4     // data[3] <- t1 - t3

  add rcx, 32
  dec rdx
  jnz @ISTG1

@STG2Up:
  mov rbx, 4                // RBX <- MMax := 4
  bsf rcx, r9
  sub rcx, 3                // RCX <- Log(s) := Log(N/8) = Log2(N) - Log2(8)
@L1:
  cmp rbx, r9
  jge @ExitL1               // if (Mmax >= N) goto @ExitL1

  mov r10, rbx              // R10 <- MMax
  shl r10, 1                // R10 <- IStep := MMax shl 1
  xor r12, r12              // R12 <- M := 0

@L2:
  cmp r12, rbx
  jge @ExitL2               // if (M >= Mmax) goto @ExitL2

  mov r8, r12               // R8 <- I := M
  mov r11, r12              // R11 <- M
  shl r11, cl               // R11 <- M * s
  shl r11, 3                // R11 <- M * SizeOf(TCmplx64)

  cmp r10, r9
  jne @L3Init               // if (IStep < N) goto @L3Init

  // IStep = N is the only case when odd sample count is processed in the L3 loop
  // the other cases can be unrolled
  movq xmm1, [rdi + r11]    // xmm1 <- w := W[M * s]
  pshufd xmm1, xmm1, $14    // xmm1 <- (w1r, w1i, w1i, w1r)

  // w * d[J]
  lea rax, r8 + rbx         // RAX <- J := I + Mmax
  lea rax, rsi + 8*rax      // RAX <- @d[J]
  movq xmm2, [rax]          // xmm2 <- (ar, ai) := d[J]
  pshufd xmm2, xmm2, $50    // xmm2 <- (ar, ar, ai, ai)
  mulps xmm2, xmm1          // xmm2 <- (w1r*ar, w1i*ar, w1i*ai, w1r*ai)
  movhlps xmm0, xmm2        // xmm3 <- (w1i*ai, w1r*ai)
  addsubps xmm2, xmm0       // xmm2 <- w*d[J]

  // d[J] := d[I] - w*d[J]
  // d[I] := d[I] + w*d[J]
  lea rdx, rsi + 8*r8
  movq xmm4, [rdx]          // xmm4 <- d[I]
  movq xmm3, xmm4           // xmm3 <- d[I]
  subps xmm3, xmm2
  addps xmm4, xmm2

  movq [rax], xmm3          // d[J] <- d[I] - w*d[J]
  movq [rdx], xmm4          // d[I] <- d[I] + w*d[J]
  jmp @ExitL3

@L3Init:
  movss xmm0, [rdi + r11]     // xmm0 <- wr := W[M * s].re
  movss xmm1, [rdi + r11 + 4] // xmm1 <- wi := W[M * s].im
  pshufd xmm0, xmm0, 0
  pshufd xmm1, xmm1, 0

@L3:
  cmp r8, r9
  jge @ExitL3               // if (I >= N) goto @ExitL3

  lea rax, r8 + rbx         // RAX <- J := I + Mmax
  lea rax, rsi + 8*rax      // RAX <- @d[J]

  movq xmm2, [rax]          // xmm2 <- (a1r, a1i) := d[J]
  movq xmm3, [rax + 8*r10]  // xmm3 <- (a2r, a2i) := d[J + IStep]

  movlhps xmm2, xmm3        // xmm2 <- (a1r, a1i, a2r, a2i)
  pshufd xmm3, xmm2, $b1    // xmm3 <- (a1i, a1r, a2i, a2r)
  mulps xmm2, xmm0          // xmm2 <- (a1r*wr, a1i*wr, a2r*wr, a2i*wr)
  mulps xmm3, xmm1          // xmm3 <- (a1i*wi, a1r*wi, a2i*wi, a2r*wi)
  addsubps xmm2, xmm3       // xmm2 <- (a1*w, a2*w) = (w*d[J], w*d[J + IStep])

  lea rdx, rsi + 8*r8
  movq xmm4, [rdx]          // xmm4 <- d[I]
  movq xmm5, [rdx + 8*r10]  // xmm5 <- d[I + IStep]
  movlhps xmm4, xmm5        // xmm4 <- (d[I], d[I + IStep])

  movaps xmm3, xmm4
  addps xmm4, xmm2          // xmm4 <- (d[I] + w*d[J], d[I + IStep] + w*d[J + IStep])
  subps xmm3, xmm2          // xmm3 <- (d[I] - w*d[J], d[I + IStep] + w*d[J + IStep])

  movq [rax], xmm3          // d[J] <- d[I] - w*d[J]
  movq [rdx], xmm4          // d[I] <- d[I] + w*d[J]
  movhlps xmm3, xmm3
  movhlps xmm4, xmm4
  movq [rax + 8*r10], xmm3  // d[J + IStep] <- d[I + IStep] - w*d[J + IStep]
  movq [rdx + 8*r10], xmm4  // d[I + IStep] <- d[I + IStep] + w*d[J + IStep]

  lea r8, r8 + 2*r10
  jmp @L3

@ExitL3:
  inc r12       // Inc(M)
  jmp @L2
  //End L2

@ExitL2:
  mov rbx, r10  // RBX <- MMax := IStep
  dec rcx       // RCX <- s := s-1
  jmp @L1
  // End L1

@ExitL1:
  pop r12
  pop rdi
  pop rsi
  pop rbx
@ExitN2:
end;
{$else}
var J, I, MMax: NativeInt;
    IStep, N, M, s: NativeInt;
    w, u, t: TCmplx64;
    pVecC4: PArrayF32x8;
begin
  N := aData.Length;
  if N = 2 then begin
    t := aData[0];
    aData[0] := t + aData[1];
    aData[1] := t - aData[1];
    exit;
  end;

  J := N shr 2;
  pVecC4 := PArrayF32x8(aData.Data);
  if aW[1].Im > 0 then begin
    for I := 0 to J - 1 do begin
      fftN4(pVecC4^);
      Inc(pVecC4);
    end;
  end else begin
    for I := 0 to J - 1 do begin
      ifftN4(pVecC4^);
      Inc(pVecC4);
    end;
  end;
  MMax := 4;
  s := N shr 3;

  while Mmax < N do begin //Outer loop executed log_2(N/2) times.
    Istep := Mmax shl 1;  // step between blocks
    M := 0;
    while M < Mmax do begin
      I := M;
      w := aW[M * s];
      while I < N do  begin       //This is the Danielson-Lanczos formula.
        J := I + Mmax;
        u := aData[I];
        t := w * aData[J];
        aData[J] := u - t;
        aData[I] := u + t;
        Inc(I, Istep);
      end;
      Inc(M);
    end;
    Mmax := Istep;
    s := s shr 1;
  end;
end;
{$endif}

procedure DLW(const aData, aW: TVecC128);
{$if defined(ASMx64)}
// RCX <- @aData, RDX <- @aW
asm
  mov r9, [rcx + 16]      // R9 <- N = aData.Length
  mov rcx, [rcx]          // RCX <- @aData[0]
  mov rax, r9
  shr rax, 2              // RAX <- N div 4
  jnz @start
  movupd xmm0, [rcx]      // xmm0  <- y[0]
  movapd xmm1, xmm0
  movupd xmm2, [rcx + 16] // xmm2 <- y[1]
  addpd xmm0, xmm2        // xmm0 <- y[0] + y[1]
  subpd xmm1, xmm2        // xmm1 <- y[0] - y[1]
  movupd [rcx], xmm0
  movupd [rcx + 16], xmm1
  jmp @ExitN2

@start:
  push rbx
  push rsi
  push rdi
  push r12

  mov rdi, [rdx]          // RDI <- @aW[0]
  mov rsi, rcx            // RSI <- @aData[0]

  // 4-block fft is performed first
  mov rdx, r9             // RDX <- N
  shr rdx, 2              // RDX <- N div 4
  movups xmm5, cImSgnMaskF64

  mov eax, [rdi + 28]     // EAX <- W[1].Im[4..7]
  test eax, $80000000
  jnz @ISTG1
@STG1:
  movupd xmm0, [rcx]
  movupd xmm1, xmm0
  movupd xmm2, [rcx + 16]
  addpd xmm0, xmm2 //xmm0 <- y[0..1]
  subpd xmm1, xmm2 //xmm1 <- y[4..5]

  movupd xmm2, [rcx + 32]
  movupd xmm3, xmm2
  movupd xmm4, [rcx + 48]
  addpd xmm2, xmm4 // xmm2 <- y[2..3]
  subpd xmm3, xmm4 // xmm3 <- y[6..7]

  movapd xmm5, xmm0
  addpd xmm5, xmm2
  movupd [rcx], xmm5

  movapd xmm5, xmm0
  subpd xmm5, xmm2
  movupd [rcx + 32], xmm5

  pshufd xmm3, xmm3, $4e // xmm3 <- (y[7],y[6])

  movapd xmm5, xmm1
  addsubpd xmm5, xmm3
  movupd [rcx + 16], xmm5

  xorpd xmm5, xmm5
  subpd xmm5, xmm3
  addsubpd xmm1, xmm5
  movupd [rcx + 48], xmm1
  add rcx, 64

  dec rdx
  jnz @STG1

  jmp @STG2Up

@ISTG1: // 4-block inverse fft loop
  movupd xmm0, [rcx]
  movupd xmm1, xmm0
  movupd xmm2, [rcx + 16]
  addpd xmm0, xmm2 //xmm0 <- y[0..1]
  subpd xmm1, xmm2 //xmm1 <- y[4..5]

  movupd xmm2, [rcx + 32]
  movupd xmm3, xmm2
  movupd xmm4, [rcx + 48]
  addpd xmm2, xmm4 // xmm2 <- y[2..3]
  subpd xmm3, xmm4 // xmm3 <- y[6..7]

  movapd xmm5, xmm0
  addpd xmm5, xmm2
  movupd [rcx], xmm5

  movapd xmm5, xmm0
  subpd xmm5, xmm2
  movupd [rcx + 32], xmm5

  pshufd xmm3, xmm3, $4e // xmm3 <- (y[7],y[6])
  movapd xmm5, xmm1
  addsubpd xmm5, xmm3
  movupd [rcx + 48], xmm5

  xorpd xmm5, xmm5
  subpd xmm5, xmm3
  addsubpd xmm1, xmm5
  movupd [rcx + 16], xmm1
  add rcx, 64

  dec rdx
  jnz @ISTG1


@STG2Up:
  mov rbx, 4                // RBX <- MMax := 4
  bsf rcx, r9
  sub rcx, 3                // RCX <- Log(s) := Log(N/8) = Log2(N) - Log2(8)
@L1:
  cmp rbx, r9
  jge @ExitL1               // if (Mmax >= N) goto @ExitL1

  mov r10, rbx              // R10 <- MMax
  shl r10, 1                // R10 <- IStep := MMax shl 1
  xor r12, r12              // R12 <- M := 0

@L2:
  cmp r12, rbx
  jge @ExitL2               // if (M >= Mmax) goto @ExitL2

  mov r8, r12               // R8 <- I := M
  mov r11, r12              // R11 <- M
  shl r11, cl               // R11 <- M * s
  shl r11, 4                // R11 <- M * SizeOf(TCmplx128)
  movupd xmm1, [rdi + r11]  // xmm1 <- W[M * s]

  cmp r10, r9
  jne @L3                   // if (IStep < N) goto @L3

  // IStep = N is the only case when odd sample count is processed in the L3 loop
  // the other cases can be unrolled
  mov rdx, r8               // RDX <- I
  shl rdx, 4                // RDX <- I * SizeOf(TCmplx128)
  add rdx, rsi              // RDX <- @d[I]
  lea rax, r8 + rbx         // RAX <- J := I + Mmax
  shl rax, 4                // RAX <- J * SizeOf(TCmplx128)
  add rax, rsi              // RAX <- @d[J]

  movupd xmm4, [rdx]        // xmm4 <- d[I]
  movddup xmm2, [rax]       // xmm2 <- (dr[J], dr[J])
  movddup xmm3, [rax + 8]   // xmm3 <- (di[J], di[J])

  // w * d[J]
  mulpd xmm2, xmm1          // xmm2 <- (wr*dr, wi*dr)
  mulpd xmm3, xmm1          // xmm3 <- (wr*di, wi*di)
  pshufd xmm3, xmm3, $4e    // xmm3 <- (wi*di, wr*di)
  addsubpd xmm2, xmm3       // xmm2 <- w * d[J]

  // d[J] := d[I] - w*d[J]
  // d[I] := d[I] + w*d[J]
  movapd xmm0, xmm4         // xmm4 <- d[I]
  subpd xmm0, xmm2
  addpd xmm4, xmm2
  movupd [rax], xmm0        // d[J] <- d[I] - w*d[J]
  movupd [rdx], xmm4        // d[I] <- d[I] + w*d[J]
  jmp @ExitL3

@L3:
  cmp r8, r9
  jge @ExitL3                // if (I >= N) goto @ExitL3

  mov rdx, r8               // RDX <- I
  shl rdx, 4                // RDX <- I * SizeOf(TCmplx128)
  add rdx, rsi              // RDX <- @d[I]
  lea rax, r8 + rbx         // RAX <- J := I + Mmax
  shl rax, 4                // RAX <- J * SizeOf(TCmplx128)
  add rax, rsi              // RAX <- @d[J]

  movupd xmm4, [rdx]        // xmm4 <- d[I]
  movddup xmm2, [rax]       // xmm2 <- (dr[J], dr[J])
  movddup xmm3, [rax + 8]   // xmm3 <- (di[J], di[J])

  // w * d[J]
  mulpd xmm2, xmm1          // xmm2 <- (wr*dr, wi*dr)
  mulpd xmm3, xmm1          // xmm3 <- (wr*di, wi*di)
  pshufd xmm3, xmm3, $4e    // xmm3 <- (wi*di, wr*di)
  addsubpd xmm2, xmm3       // xmm2 <- w * d[J]

  // d[J] := d[I] - w*d[J]
  // d[I] := d[I] + w*d[J]
  movapd xmm0, xmm4         // xmm4 <- d[I]
  subpd xmm0, xmm2
  addpd xmm4, xmm2
  movupd [rax], xmm0        // d[J] <- d[I] - w*d[J]
  movupd [rdx], xmm4        // d[I] <- d[I] + w*d[J]

  add r8, r10               // Inc(I, IStep)

  mov rdx, r8               // RDX <- I
  shl rdx, 4                // RDX <- I * SizeOf(TCmplx128)
  add rdx, rsi              // RDX <- @d[I]
  lea rax, r8 + rbx         // RAX <- J := I + Mmax
  shl rax, 4                // RAX <- J * SizeOf(TCmplx128)
  add rax, rsi              // RAX <- @d[J]

  movupd xmm4, [rdx]        // xmm4 <- d[I]
  movddup xmm2, [rax]       // xmm2 <- (dr[J], dr[J])
  movddup xmm3, [rax + 8]   // xmm3 <- (di[J], di[J])

  // w * d[J]
  mulpd xmm2, xmm1          // xmm2 <- (wr*dr, wi*dr)
  mulpd xmm3, xmm1          // xmm3 <- (wr*di, wi*di)
  pshufd xmm3, xmm3, $4e    // xmm3 <- (wi*di, wr*di)
  addsubpd xmm2, xmm3       // xmm2 <- w * d[J]

  // d[J] := d[I] - w*d[J]
  // d[I] := d[I] + w*d[J]
  movapd xmm0, xmm4         // xmm4 <- d[I]
  subpd xmm0, xmm2
  addpd xmm4, xmm2
  movupd [rax], xmm0        // d[J] <- d[I] - w*d[J]
  movupd [rdx], xmm4        // d[I] <- d[I] + w*d[J]

  add r8, r10               // Inc(I, IStep)

  jmp @L3
  //end L3

@ExitL3:
  inc r12       // Inc(M)
  jmp @L2
  //End L2

@ExitL2:
  mov rbx, r10  // RBX <- MMax := IStep
  dec rcx       // RCX <- s := s-1
  jmp @L1
  // End L1

@ExitL1:
  pop r12
  pop rdi
  pop rsi
  pop rbx
@ExitN2:
end;
{$else}
var J, I, MMax: NativeInt;
    IStep, N, M, s: NativeInt;
    w, t: TCmplx128;
    pVecC4: PArrayF64x8;
begin
  N := aData.Length;
  if N = 2 then begin
    t := aData[0];
    aData[0] := t + aData[1];
    aData[1] := t - aData[1];
    exit;
  end;

  J := N shr 2;
  pVecC4 := PArrayF64x8(aData.Data);
  if aW[1].Im > 0 then begin
    for I := 0 to J - 1 do begin
      fftN4(pVecC4^);
      Inc(pVecC4);
    end;
  end else begin
    for I := 0 to J - 1 do begin
      ifftN4(pVecC4^);
      Inc(pVecC4);
    end;
  end;
  MMax := 4;
  s := aData.Length shr 3;

  while Mmax < N do begin //Outer loop executed log_2(N/2) times.
    Istep := Mmax shl 1;  // step between blocks
    M := 0;
    while M < Mmax do begin
      I := M;
      w := aW[M * s];
      while I < N do  begin       //This is the Danielson-Lanczos formula.
        J := I + Mmax;
        t := w * aData[J];
        aData[J] := aData[I] - t;
        aData[I] := aData[I] + t;
        Inc(I, Istep);
      end;
      Inc(M);
    end;
    Mmax := Istep;
    s := s shr 1;
  end;
end;
{$endif}

{$region 'TFFTEvalBase'}

procedure TFFTEvalBase.AfterConstruction;
begin
  inherited;
  fDLThreshold := -1;
  fInitialized := False;
end;

procedure TFFTEvalBase.Init(N: NativeInt);
begin
  if N <= 0 then
    raise EFFTSizeError.Create(cPositiveSizeErrorMsg);

  fN := N;
  InitFunctions;
  InitBuffers(N);
  InitFactors(N);
  fInitialized := True;
end;

procedure TFFTEvalBase.SetRecursiveMethodThreshold(aValue: Integer);
begin
  if aValue = fDLThreshold then exit;

  if aValue < 0 then begin
    fDLThreshold := -1;
    exit;
  end;

  if aValue = 0 then begin
    fDLThreshold := 0;
    exit;
  end;

  fDLThreshold := NearestLowerPowerOfTwo(aValue);
end;

{$endregion}

{$region 'TFFTEvalBase<TF, TC>'}

procedure TFFTEvalBase<TF, TC>.InitBuffers(N: NativeInt);
begin
  fW := TNDABuffer<TC>.Create([N]);
  fWv.Init(fW.Data, N);
  fFnc_EvalTwiddleFactors(fWv);

  fBuff := TNDABuffer<TC>.Create([2*N]);
  fBuffv.Init(fBuff.Data, 2*N);
end;

procedure TFFTEvalBase<TF, TC>.InitFactors(N: NativeInt);
var p2, ws, pwr, pwr4: NativeInt;
    f: TFactorUI64;
begin
  fFactors := WheelFactorization(N);
  if fFactors[0].Value = 2 then begin
    p2 := 1 shl fFactors[0].Power;
    if fDLThreshold < 0 then
      ws := g_FFTProps.RecursiveMethodThreshold
    else
      ws := fDLThreshold;
    ws := Min(ws, p2);
    if ws > 1 then begin
      fBRPIdxs := BRPIndices(ws);
      fDLBlockSize := ws;
      p2 := p2 div ws;
    end else begin
      fBRPIdxs := nil;
      fDLBlockSize := 2;
      ws := 1;
    end;

    if ws < fWv.Length then begin
      fW2 := TNDAUt.Copy<TC>(fW[[NDISpan(0, -1, fWv.Length div ws)]]);
      fWv2.Init(fW2.Data, fW2.Shape[0])
    end else
      fWv2.Init(fW.Data, fN);

    if p2 > 1 then begin
      pwr := Round(Log2(p2));
      pwr4 := pwr shr 1;
      if pwr4 > 0 then begin
        f.Init(4, pwr4);
        Dec(fFactors[0].Power, pwr4 shl 1);
        if fFactors[0].Power > 0 then
          fFactors := TDynAUt.Insert<TFactorUI64>(fFactors, f, 1)
        else
          fFactors[0] := f;
      end;
    end;
  end;
end;

procedure TFFTEvalBase<TF, TC>.FFT(const aSrc, aDst: TVec<TC>);
var radix: Integer;
begin
  Dec(fFactPwr);
  if fFactPwr < 0 then begin
    Dec(fFactTop);
    fFactPwr := fFactors[fFactTop].Power - 1;
  end;
  radix := fFactors[fFactTop].Value;

  case radix of
    2: FFT2(aSrc, aDst);
    3: FFT3(aSrc, aDst);
    4: FFT4(aSrc, aDst);
    5: FFT5(aSrc, aDst);
  else
    raise ENotImplemented.CreateFmt('FFT does not support radix %d.', [radix]);
  end;

  Inc(fFactPwr);
  if fFactPwr >= fFactors[fFactTop].Power then begin
    Inc(fFactTop);
    fFactPwr := 0;
  end;
end;

procedure TFFTEvalBase<TF, TC>.FFT2(const aSrc, aDst: TVec<TC>);
var N, N2: NativeInt;
    v: TVec<TC>;
begin
  N := aSrc.Length;

  if N = fDLBlockSize then begin
    fFnc_pack(aSrc, aDst);
    v := aDst.Span(0, N);
    fFnc_BRP(fBRPIdxs, v);
    fFnc_DLW(v, fWv2);
    exit;
  end;

  N2 := N div 2;
  if N2 = fDLBlockSize then begin
    fFnc_split2(aSrc, aDst);
    v := aDst.Span(0, N2);
    fFnc_BRP(fBRPIdxs, v);
    fFnc_DLW(v, fWv2);
    v := aDst.Span(N2, N2);
    fFnc_BRP(fBRPIdxs, v);
    fFnc_DLW(v, fWv2);
  end else begin
    FFT(aSrc.Span(0, N2, 2), aDst.Span(0, N2));
    FFT(aSrc.Span(1, N2, 2), aDst.Span(N2, N2));
  end;

  // Result := [efft + w*offt, efft - w*offt];
  fFnc_fftcomb2(aDst.Span(0, N), fWv.Span(0, N2, aSrc.Stride div cCSz));
end;

procedure TFFTEvalBase<TF, TC>.FFT3(const aSrc, aDst: TVec<TC>);
var N, N3: NativeInt;
begin
  N := aSrc.Length;
  N3 := N div 3;
  if N3 = 1 then begin
    fFnc_pack(aSrc, aDst);
    fFnc_fftN3(aDst);
  end else begin
    FFT(aSrc.Span(0, N3, 3), aDst.Span(0, N3));
    FFT(aSrc.Span(1, N3, 3), aDst.Span(N3, N3));
    FFT(aSrc.Span(2, N3, 3), aDst.Span(2*N3, N3));
    fFnc_fftcomb3(aDst.Span(0, N), fWv.Span(0, N3, aSrc.Stride div cCSz));
  end;
end;

procedure TFFTEvalBase<TF, TC>.FFT4(const aSrc, aDst: TVec<TC>);
var N, N4: NativeInt;
    v: TVec<TC>;
begin
  N := aSrc.Length;
  if N = 4 then begin
    fFnc_pack(aSrc, aDst);
    fFnc_fftN4(aDst);
    exit;
  end;

  N4 := N div 4;
  if N4 = fDLBlockSize then begin
    fFnc_split4(aSrc, aDst);
    v := aDst.Span(0, N4);
    fFnc_BRP(fBRPIdxs, v);
    fFnc_DLW(v, fWv2);
    v := aDst.Span(N4, N4);
    fFnc_BRP(fBRPIdxs, v);
    fFnc_DLW(v, fWv2);
    v := aDst.Span(2*N4, N4);
    fFnc_BRP(fBRPIdxs, v);
    fFnc_DLW(v, fWv2);
    v := aDst.Span(3*N4, N4);
    fFnc_BRP(fBRPIdxs, v);
    fFnc_DLW(v, fWv2);
  end else begin
    FFT(aSrc.Span(0, N4, 4), aDst.Span(0, N4));
    FFT(aSrc.Span(1, N4, 4), aDst.Span(N4, N4));
    FFT(aSrc.Span(2, N4, 4), aDst.Span(2*N4, N4));
    FFT(aSrc.Span(3, N4, 4), aDst.Span(3*N4, N4));
  end;

  fFnc_fftcomb4(aDst.Span(0, N), fWv.Span(0, N4, aSrc.Stride div cCSz));
end;

procedure TFFTEvalBase<TF, TC>.FFT5(const aSrc, aDst: TVec<TC>);
var N, N5: NativeInt;
begin
  N := aSrc.Length;
  N5 := N div 5;
  if N5 = 1 then begin
    fFnc_pack(aSrc, aDst);
    fFnc_fftN5(aDst);
  end else begin
    FFT(aSrc.Span(0, N5, 5), aDst.Span(0, N5));
    FFT(aSrc.Span(1, N5, 5), aDst.Span(N5, N5));
    FFT(aSrc.Span(2, N5, 5), aDst.Span(2*N5, N5));
    FFT(aSrc.Span(3, N5, 5), aDst.Span(3*N5, N5));
    FFT(aSrc.Span(4, N5, 5), aDst.Span(4*N5, N5));
    fFnc_fftcomb5(aDst.Span(0, N), fWv.Span(0, N5, aSrc.Stride div cCSz));
  end;
end;

{$endregion}

{$region 'TFFTEval<TF, TC>'}

procedure TFFTEval<TF, TC>.AfterConstruction;
begin
  inherited;
  fDir := fdForward;
end;

procedure TFFTEval<TF, TC>.InitBuffers(N: NativeInt);
begin
  fW := TNDABuffer<TC>.Create([N]);
  fWv.Init(fW.Data, N);
  case fDir of
    fdForward: fFnc_EvalTwiddleFactors(fWv, 1);
    fdInverse: fFnc_EvalTwiddleFactors(fWv, -1);
  end;

  fBuff := TNDABuffer<TC>.Create([2*N]);
  fBuffv.Init(fBuff.Data, 2*N);
end;

procedure TFFTEval<TF, TC>.Execute(const aSrc: INDArray<TC>; var aDst: INDArray<TC>);
var srcv: TVec<TC>;
begin
  Assert(Assigned(aSrc) and (aSrc.NDim = 1) and (fN = aSrc.Shape[0]));

  if not Assigned(aDst) then
    aDst := TNDABuffer<TC>.Create([fN]);

  if fN = 1 then begin
    PC(aDst.Data)^ := PC(aSrc.Data)^;
    exit;
  end;

  srcv.Init(aSrc.Data, fN);
  fFactTop := High(fFactors);
  fFactPwr := fFactors[fFactTop].Power;
  FFT(srcv, fBuffv);
  if CContiguousQ(aDst) then
      Move(fBuff.Data^, aDst.Data^, fN * cCSz)
  else
    TNDAUt.Fill<TC>(aDst, fBuff[[NDISpan(0, fN - 1)]]);
end;

{$region 'Getters/Setters'}

procedure TFFTEval<TF, TC>.SetDir(aValue: TFFTDirection);
begin
  if not fInitialized then fDir := aValue;
end;

{$endregion}

{$endregion}

{$region 'TRealFFTEvalBase<TF, TC>'}

procedure TRealFFTEvalBase<TF, TC>.InitBuffers(N: NativeInt);
begin
  fW := TNDABuffer<TC>.Create([2*N]);
  fWv.Init(fW.Data, 2*N);
  if fDir = fdForward then
    fFnc_EvalTwiddleFactors(fWv, 1)
  else
    fFnc_EvalTwiddleFactors(fWv, -1);

  fBuff := TNDABuffer<TC>.Create([3*N]);
  fBuffv.Init(fBuff.Data, 2*N);
end;

procedure TRealFFTEvalBase<TF, TC>.Init(N: NativeInt);
begin
  if (N and 1) <> 0 then
    raise EFFTSizeError.Create(cEvenSizeErrorMsg);

  inherited Init(N div 2);

  fWv.Length := N;
  fWv.Stride := 2 * cCSz;
end;

{$endregion}

{$region 'TRealFFTEval<TF, TC>'}

procedure TRealFFTEval<TF, TC>.Init(N: NativeInt; aFullSpectrum: Boolean);
begin
  if N <= 0 then
    raise EArgumentOutOfRangeException.Create(cPositiveSizeErrorMsg);

  fFullSpectrum := aFullSpectrum;
  Init(N);
end;

procedure TRealFFTEval<TF, TC>.Execute(const aSrc: INDArray<TF>);
var srcv, res, w: TVec<TC>;
begin
  Assert(Assigned(aSrc) and (aSrc.NDim = 1) and (2*fN = aSrc.Shape[0]));

  if fN = 1 then begin
    with fBuff do begin
      // Re := PF(aSrc.Data)^, Im := 0
      PF(Data)^ := PF(aSrc.Data)^;
      PF(Data + cFSz)^ := Default(TF);
    end;
  end else begin
    srcv.Init(aSrc.Data, fN);
    fFactTop := High(fFactors);
    fFactPwr := fFactors[fFactTop].Power;
    FFT(srcv, fBuffv);
  end;
  w.Init(fW.Data, 2*fN);
  res := fBuffv.Span(fN, 2*fN);

  if fFullSpectrum then
    fFnc_FTRecombFull(fBuffv.Span(0, fN), w, res)
  else
    fFnc_FTRecombHalf(fBuffv.Span(0, fN), w, res);
end;

procedure TRealFFTEval<TF, TC>.Execute(const aSrc: INDArray<TF>; var aDst: INDArray<TC>);
begin
  Execute(aSrc);

  if fFullSpectrum then begin
    if not Assigned(aDst) then
      aDst := TNDABuffer<TC>.Create([2*fN])
    else
    if (aDst.NDim <> 1) or (aDst.Shape[0] <> 2*fN) then
      raise EFFTSizeError.CreateFmt(cInvSizeErrorMsg, [aDst.Shape[0], 2*fN]);

    if CContiguousQ(aDst) then
      Move(fBuffv.ItemPtr[fN]^, aDst.Data^, 2* fN*cCSz)
    else
      TNDAUt.Fill<TC>(aDst, fBuff[[NDISpan(fN, 3*fN - 1)]]);
  end else begin
    if not Assigned(aDst) then
      aDst := TNDABuffer<TC>.Create([fN + 1])
    else
    if (aDst.NDim <> 1) or (aDst.Shape[0] <> fN + 1) then
      raise EFFTSizeError.CreateFmt(cInvSizeErrorMsg, [aDst.Shape[0], fN + 1]);

    if CContiguousQ(aDst) then
      Move(fBuffv.ItemPtr[fN]^, aDst.Data^, (fN + 1)*cCSz)
    else
      TNDAUt.Fill<TC>(aDst, fBuff[[NDISpan(fN, 2*fN)]]);
  end;
end;

{$region 'Getters/Setters'}

procedure TRealFFTEval<TF, TC>.SetFullSpectrum(aValue: Boolean);
begin
  if not fInitialized then fFullSpectrum := aValue;
end;

function TRealFFTEval<TF, TC>.GetResult: TVec<TC>;
begin
  if fFullSpectrum then
    Result := fBuffv.Span(fN,  2*fN)
  else
    Result := fBuffv.Span(fN, fN + 1);
end;

{$endregion}

{$endregion}

{$region 'TRealIFFTEval<TF, TC>.'}

procedure TRealIFFTEval<TF, TC>.AfterConstruction;
begin
  inherited;
  fDir := fdInverse;
  fNormalize := False;
end;

procedure TRealIFFTEval<TF, TC>.Execute(const aSrc: INDArray<TC>);
var w, srcv, resv: TVec<TC>;
begin
  Assert(
    Assigned(aSrc) and (aSrc.NDim = 1) and (aSrc.Shape[0] = fN + 1)
  );

  w.Init(fW.Data, 2*fN);
  srcv.Init(aSrc.Data, fN + 1);
  fFnc_FTReconstruct(srcv, w, fBuffv.Span(0, fN));

  srcv.Init(fBuffv.Data, fN);
  resv := fBuffv.Span(fN, 2*fN);
  fFactTop := High(fFactors);
  fFactPwr := fFactors[fFactTop].Power;
  FFT(srcv, resv);

  if fNormalize then
    fFnc_Norm(resv.Data, resv.Data, resv.Length);
end;

procedure TRealIFFTEval<TF, TC>.Execute(const aSrc: INDArray<TC>; var aDst: INDArray<TF>);
var bNorm: Boolean;
    s: TF;
begin
  bNorm := fNormalize;
  try
    fNormalize := False;
    Execute(aSrc);
  finally
    fNormalize := bNorm;
  end;

  if not Assigned(aDst) then
    aDst := TNDABuffer<TF>.Create([2*fN]);

  if CContiguousQ(aDst) then begin
    if fNormalize then begin
      fFnc_Norm(fBuffv.ItemPtr[fN], aDst.Data, 2*fN);
    end else
      Move(fBuffv.ItemPtr[fN]^, aDst.Data^, fN*cCSz)
  end else begin
    if fNormalize then
      fFnc_Norm(fBuffv.ItemPtr[fN], fBuffv.ItemPtr[fN], 2*fN);
    TNDAUt.Fill<TF>(aDst, TNDAUt.AsType<TF>(fBuff[[NDISpan(fN, 2*fN - 1)]]));
  end;
end;

{$region 'Getters/Setters'}

function TRealIFFTEval<TF, TC>.GetResult: TVec<TF>;
begin
  Result.Init(fBuffv.ItemPtr[fN], 2*fN, SizeOf(TF));
end;

{$endregion}

{$endregion}

{$region 'TRealFFTFilter<TF, TC>'}

procedure TRealFFTFilter<TF, TC>.Execute(const aSrc: INDArray<TF>; var aDst: INDArray<TF>);
begin

end;

{$endregion}

{$region 'TFFTEvalBase2D<TF, TC>'}

{$region 'TRealFFTEval2D<TF, TC>.TRowPairFFT'}

procedure TFFTEvalBase2D<TF, TC>.TRowFFT.Evaluate(const aSrc: TVec<TC>);
begin
  Assert(aSrc.Length = fN);

  if fN = 1 then begin
    PC(fBuffv.Data)^ := PC(aSrc.Data)^;
    exit;
  end;

  fFactTop := High(fFactors);
  fFactPwr := fFactors[fFactTop].Power;
  FFT(aSrc, fBuffv);
end;

procedure TFFTEvalBase2D<TF, TC>.TRowFFT.Evaluate(const aSrc, aDst: TVec<TC>);
var prevBuff: PByte;
begin
  Assert((aDst.Length >= 2 * fN) and (aDst.Stride = cCSz));

  prevBuff := fBuffv.Data;
  try
    fBuffv.Data := aDst.Data;
    Evaluate(aSrc);
  finally
    fBuffv.Data := prevBuff;
  end;
end;

procedure TFFTEvalBase2D<TF, TC>.TRowFFT.InitFunctions;
begin
  // nothing to do here. Functions are alredy initialized by the owner
end;

{$endregion}

constructor TFFTEvalBase2D<TF, TC>.Create;
begin
  fRowFFT := TRowFFT.Create;
  fColFFT := TRowFFT.Create;
  fSpectrumLayout := slNormal;
end;

destructor TFFTEvalBase2D<TF, TC>.Destroy;
begin
  fRowFFT.Free;
  fColFFT.Free;
  inherited;
end;

procedure TFFTEvalBase2D<TF, TC>.Init(aNRows, aNCols: NativeInt);
begin
  InitFunctions;
  fNRows := aNRows;
  fNCols := aNCols;
  fRowFFT.Init(aNCols);
  fColFFT.Init(aNRows);
end;

{$region 'Getters/Setters'}

function TFFTEvalBase2D<TF, TC>.GetRecursiveMethodThreshold: Integer;
begin
  Result := fRowFFT.RecursiveMethodThreshold;
end;

procedure TFFTEvalBase2D<TF, TC>.SetRecursiveMethodThreshold(aValue: Integer);
begin
  fRowFFT.RecursiveMethodThreshold := aValue;
  fColFFT.RecursiveMethodThreshold := aValue;
end;

{$endregion}

{$endregion}

{$region 'TRealFFTEval2D<TF, TC>'}

procedure TRealFFTEval2D<TF, TC>.Init(aNRows, aNCols: NativeInt);
begin
  inherited;
  fBuff := TNDABuffer<TC>.Create([(fNCols div 2) + 1, fNRows]);
  fRowBuff := TNDABuffer<TC>.Create([fNCols]);
  fCBuff := TNDABuffer<TC>.Create([5, Max(fNRows, (fNCols div 2) + 1)]);
end;

procedure TRealFFTEval2D<TF, TC>.Init(aNRows, aNCols: NativeInt; aFullSpectrum: Boolean);
begin
  Init(aNRows, aNCols);
  fFullSpectrum := aFullSpectrum;
end;

procedure TRealFFTEval2D<TF, TC>.Execute(const aSrc: INDArray<TF>; var aDst: INDArray<TC>);
var I, N, rStep, cStep, bStep, NDstCols: NativeInt;
    x, y, r0, r1, c0, c1: TVec<TC>;
    pBuff: PByte;
begin
  Assert((aSrc.NDim = 2) and (aSrc.Shape[0] = fNRows) and (aSrc.Shape[1] = fNCols));

  NDstCols := (fNCols div 2) + 1;
  if not Assigned(aDst) then
    aDst := TNDABuffer<TC>.Create([fNRows, NDstCols]);

  // Rows FFT

  pBuff := fBuff.Data;
  rStep := aSrc.Strides[0];
  cStep := fCBuff.Strides[0];
  bStep := fBuff.Strides[0];
  r0.Init(aSrc.Data, fNCols, aSrc.Strides[1]);
  r1.Init(r0.Data + rStep, fNCols, r0.Stride);
  c0.Init(fCBuff.Data, NDstCols, cCSz);
  c1.Init(fCBuff.Data + cStep, NDstCols, cCSz);
  x.Init(fRowBuff.Data, fNCols);
  y.Init(fRowFFT.Buff.Data, fNCols);

  I := 0;
  N := (fNRows shr 2) shl 2;
  while I < N do begin
    c0.Data := fCBuff.Data;
    c1.Data := c0.Data + cStep;
    fFnc_InterleaveRows(r0.Data, r1.Data, x.Data, fNCols);
    fRowFFT.Evaluate(x);
    fFnc_RealRowPairFTRecomb(y, c0, c1);
    Inc(r0.Data, 2*rStep);
    Inc(r1.Data, 2*rStep);
    Inc(c0.Data, 2*cStep);
    Inc(c1.Data, 2*cStep);
    fFnc_InterleaveRows(r0.Data, r1.Data, x.Data, fNCols);
    fRowFFT.Evaluate(x);
    fFnc_RealRowPairFTRecomb(y, c0, c1);
    Inc(r0.Data, 2*rStep);
    Inc(r1.Data, 2*rStep);
    Inc(c0.Data, 2*cStep);
    Inc(c1.Data, 2*cStep);

    fFnc_Tr4(fCBuff.Data, pBuff, 4, NDstCols, cStep, bStep);
    Inc(pBuff, 4 * cCSz);

    Inc(I, 4);
  end;

  c0.Init(pBuff, NDstCols, fBuff.Strides[0]);
  c1.Init(c0.Data + cC64Sz, NDstCols, c0.Stride);

  if I < fNRows then begin
    fFnc_InterleaveRows(r0.Data, r1.Data, x.Data, fNCols);
    fRowFFT.Evaluate(x);
    fFnc_RealRowPairFTRecomb(y, c0, c1);
  end;

  // Colums FFT

  pBuff := aDst.Data;
  bStep := aDst.Strides[0];
  rStep := fBuff.Strides[0];
  cStep := fCBuff.Strides[0];
  r0.Init(fBuff.Data, fNRows);
  c0.Init(fCBuff.Data, 2 * fNRows, cCSz);

  I := 0;
  N := (NDstCols shr 2) shl 2;
  while I < N do begin
    c0.Data := fCBuff.Data;
    fColFFT.Evaluate(r0, c0);
    Inc(r0.Data, rStep);
    Inc(c0.Data, cStep);
    fColFFT.Evaluate(r0, c0);
    Inc(r0.Data, rStep);
    Inc(c0.Data, cStep);
    fColFFT.Evaluate(r0, c0);
    Inc(r0.Data, rStep);
    Inc(c0.Data, cStep);
    fColFFT.Evaluate(r0, c0);
    Inc(r0.Data, rStep);
    Inc(c0.Data, cStep);

    fFnc_Tr4(fCBuff.Data, pBuff, 4, fNRows, cStep, bStep);
    Inc(pBuff, 4 * cCSz);

    Inc(I, 4);
  end;

  cStep := aDst.Strides[1];
  c0.Init(pBuff, fNRows, bStep);
  r1.Init(fColFFT.Buff.Data, fNRows);

  while I < NDstCols do begin
    fColFFT.Evaluate(r0);
    fFnc_Copy(r1, c0);
    Inc(r0.Data, rStep);
    Inc(c0.Data, cStep);
    Inc(I);
  end;
end;

{$endregion}

{$region 'TRealIFFTEval2D<TF, TC>'}

procedure TRealIFFTEval2D<TF, TC>.AfterConstruction;
begin
  inherited;
  fRowFFT.Direction := fdInverse;
  fColFFT.Direction := fdInverse;
end;

procedure TRealIFFTEval2D<TF, TC>.ColFFTNative(const aSrc, aDst: INDArray<TC>);
var I, N, NSrcCols, bStep, rStep, cStep: NativeInt;
    c, r: TVec<TC>;
    pBuff: PByte;
begin
  NSrcCols := aSrc.Shape[0];

  pBuff := aDst.Data;
  bStep := aDst.Strides[0];
  rStep := fBuff.Strides[0];
  cStep := fCBuff.Strides[0];
  r.Init(aDst.Data, fNRows);
  c.Init(fCBuff.Data, 2 * fNRows);

  I := 0;
  N := (NSrcCols shr 2) shl 2;
  while I < N do begin
    c.Data := fCBuff.Data;
    fColFFT.Evaluate(r, c);
    Inc(r.Data, rStep);
    Inc(c.Data, cStep);
    fColFFT.Evaluate(r, c);
    Inc(r.Data, rStep);
    Inc(c.Data, cStep);
    fColFFT.Evaluate(r, c);
    Inc(r.Data, rStep);
    Inc(c.Data, cStep);
    fColFFT.Evaluate(r, c);
    Inc(r.Data, rStep);
    Inc(c.Data, cStep);

    fFnc_Tr4(fCBuff.Data, pBuff, 4, fNRows, cStep, bStep);
    Inc(pBuff, 4 * cCSz);

    Inc(I, 4);
  end;

  cStep := aDst.Strides[1];
  c.Init(pBuff, fNRows, bStep);
  r.Init(fColFFT.Buff.Data, fNRows);

  while I < NSrcCols do begin
    fColFFT.Evaluate(r);
    fFnc_Copy(r, c);
    Inc(r.Data, rStep);
    Inc(c.Data, cStep);
    Inc(I);
  end;
end;

procedure TRealIFFTEval2D<TF, TC>.Execute(const aSrc: INDArray<TC>; var aDst: INDArray<TF>);
var srcSh, dstSh: TNDAShape;
    w, h: NativeInt;
begin
  Assert(Assigned(aSrc) and (aSrc.NDim = 2));

  srcSh := aSrc.Shape;
  case fSpectrumLayout of
    slNormal: begin
      if not Assigned(aDst) then
        aDst := TNDABuffer<TF>.Create([srcSh[0], 2*(srcSh[1] - 1)]);


    end;

    slNative: begin
      if not Assigned(aDst) then
        aDst := TNDABuffer<TF>.Create([srcSh[1], 2*(srcSh[0] - 1)]);

      ColFFTNative(aSrc, fBuff);
    end;
  else
    raise EFFTError.Create('Unknown spectrum layout.');
  end;
end;

{$endregion}

{$region 'TRealFFTFilter2D<TF, TC>'}

procedure TRealFFTFilter2D<TF, TC>.Execute(const aSrc: INDArray<TF>; var aDst: INDArray<TF>);
begin

end;

{$endregion}

{$region 'TFFTEvalF32'}

procedure InitFFTFunctionsF32(aFFT: TFFTEvalBase<Single, TCmplx64>; aDir: TFFTDirection = fdForward);
begin
  with aFFT do begin
    fFnc_EvalTwiddleFactors := EvalTwiddleFactors;
    fFnc_pack := _pack;
    fFnc_split2 := _split2C64;
    fFnc_split4 := _split4C64;
    fFnc_BRP := _perm;
    fFnc_DLW := DLW;
    case aDir of
      fdForward: begin
        fFnc_fftN3 := _fft3;
        fFnc_fftN4 := _fft4;
        fFnc_fftN5 := _fft5;
        fFnc_fftcomb2 := _fftcomb2;
        fFnc_fftcomb3 := _fftcomb3;
        fFnc_fftcomb4 := _fftcomb4;
        fFnc_fftcomb5 := _fftcomb5;
      end;
      fdInverse: begin
        fFnc_fftN3 := _ifft3;
        fFnc_fftN4 := _ifft4;
        fFnc_fftN5 := _ifft5;
        fFnc_fftcomb2 := _fftcomb2;
        fFnc_fftcomb3 := _ifftcomb3;
        fFnc_fftcomb4 := _ifftcomb4;
        fFnc_fftcomb5 := _ifftcomb5;
      end;
    end;
  end;
end;

procedure TFFTEvalF32.InitFunctions;
begin
  InitFFTFunctionsF32(Self, fDir);
end;

{$endregion}

{$region 'TRealFFTEvalF32'}

procedure InitRealFFTFunctionsF32(aFFT: TRealFFTEval<Single, TCmplx64>);
begin
  InitFFTFunctionsF32(aFFT);

  with aFFT do begin
    fFnc_FTRecombFull := RealFTRecombFull;
    fFnc_FTRecombHalf := RealFTRecombHalf;
  end;
end;

procedure TRealFFTEvalF32.InitFunctions;
begin
  InitRealFFTFunctionsF32(Self);
end;

{$endregion}

{$region 'TRealIFFTEvalF32'}

procedure NormalizeF32(pSrc, pDst: PByte; aCount: NativeInt);
begin
  VecMul(PSingle(pSrc), 1/aCount, PSingle(pDst), aCount);
end;

procedure InitRealIFFTFunctionsF32(aFFT: TRealIFFTEval<Single, TCmplx64>);
begin
  InitFFTFunctionsF32(aFFT, fdInverse);

  with aFFT do begin
    fFnc_Norm := NormalizeF32;
    fFnc_FTReconstruct := RealFTReconstruct;
  end;
end;

procedure TRealIFFTEvalF32.InitFunctions;
begin
  InitRealIFFTFunctionsF32(Self);
end;

{$endregion}

{$region 'TRealFFTEval2DF32'}

procedure _interleaveRowsF32(X0, X1, Z: PByte; aCnt: NativeInt);
var pEnd: PByte;
begin
  pEnd := X0 + aCnt * cF32Sz;
  while X0 < pEnd do begin
    with PCmplx64(Z)^ do begin
      Re := PSingle(X0)^;
      Im := PSingle(X1)^;
    end;
    Inc(Z, cC64Sz);
    Inc(X0, cF32Sz);
    Inc(X1, cF32Sz);
  end;
end;

procedure RealRowPairFTRecomb(const y, c0, c1: TVecC64); overload;
{$if defined(ASMx64)}
const cHalf: array [0..3] of Single = (0.5, 0.5, 0.5, -0.5);
// RCX <- @y, RDX <- @c0, R8 <- @c1
asm
  mov r9, [rdx + 8]       // R9 <- c0.Stride
  mov rax, [rcx + 16]     // RAX <- N = y.Length
  mov rcx, [rcx]          // RCX <- @y[0]
  mov rdx, [rdx]          // RDX <- @c0[0]
  mov r8, [r8]            // R8 <- @c1[0]

  xorps xmm0, xmm0
  movss xmm0, [rcx]       // xmm0 <- y[0].re
  movq [rdx], xmm0        // c0[0] <- (y[0].re, 0)
  movss xmm0, [rcx + 4]   // xmm0 <- y[0].im
  movq [r8], xmm0         // c1[0] <- (y[0].im, 0)
  add rdx, r9
  add r8, r9
  dec rax                 // RAX <- N - 1

  movupd xmm5, cImSgnMaskF32
  movupd xmm4, cHalf
  lea r10, rcx + 8*rax    // R10 <- @y[N - 1]
  add rcx, 8              // RCX <- @y[1]
  inc rax
  shr rax, 1              // RAX <- N/2
@L:
  movq xmm0, [rcx]          // xmm0 <- z := y[k]
  movq xmm1, [r10]          // xmm1 <- y[N - k]
  xorps xmm1, xmm5          // xmm1 <- zc = y[N - k]*
  pshufd xmm0, xmm0, $14    // xmm0 <- (zr, zi, zi, zr)
  pshufd xmm1, xmm1, $14    // xmm1 <- (zcr, zci, zci, zcr)
  addsubps xmm0, xmm1       // xmm0 <- (zr - zcr, zi - zci, zi - zci, zr - zcr)
  pshufd xmm0, xmm0, $27    // xmm0 <- (zr + zcr, zi + zci, zi - zci, zr - zcr)
  mulps xmm0, xmm4          // xmm0 <- cHalf*xmm0
  movhlps xmm1, xmm0
  movq [rdx], xmm0          // c0[k] <- 1/2*(z + zc)
  movq [r8], xmm1           // c1[k] <- -1/2*I*(z - zc)

  add rcx, 8
  sub r10, 8
  add rdx, r9
  add r8, r9
  dec rax
  jnz @L
end;
{$else}
var k, N: NativeInt;
    z, zc: TCmplx64;
const I: TCmplx64 = (Re: 0.0; Im: 1.0);
begin
  Assert((c0.Stride = c1.Stride) and (y.Stride = cC64Sz));

  z := y[0];
  c0[0] := z.Re;
  c1[0] := z.Im;

  N := y.Length;
  for k := 1 to N div 2 do begin
    z := y[k];
    zc := y[N - k].Conjugate;
    c0[k] := 1/2*(z + zc);
    c1[k] := -1/2*I*(z - zc);
  end;
end;
{$endif}

procedure InitRealFFT2DFunctionsF32(aFFT: TRealFFTEval2D<Single, TCmplx64>);
begin
  with aFFT do begin
    fFnc_RealRowPairFTRecomb := RealRowPairFTRecomb;
    fFnc_InterleaveRows := _interleaveRowsF32;
    fFnc_Copy := _copyC64;
    fFnc_Tr4 := CTr_8B;
  end;
end;

procedure TRealFFTEval2DF32.InitFunctions;
begin
  InitRealFFT2DFunctionsF32(Self);
  InitFFTFunctionsF32(fRowFFT);
  InitFFTFunctionsF32(fColFFT);
end;

{$endregion}

{$region 'TFFTEvalF64'}

procedure InitFFTFunctionsF64(aFFT: TFFTEvalBase<Double, TCmplx128>; aDir: TFFTDirection = fdForward);
begin
  with aFFT do begin
    fFnc_EvalTwiddleFactors := EvalTwiddleFactors;
    fFnc_pack := _pack;
    fFnc_split2 := _split2C128;
    fFnc_split4 := _split4C128;
    fFnc_BRP := _perm;
    fFnc_DLW := DLW;
    case aDir of
      fdForward: begin
        fFnc_fftN3 := _fft3;
        fFnc_fftN4 := _fft4;
        fFnc_fftN5 := _fft5;
        fFnc_fftcomb2 := _fftcomb2;
        fFnc_fftcomb3 := _fftcomb3;
        fFnc_fftcomb4 := _fftcomb4;
        fFnc_fftcomb5 := _fftcomb5;
      end;
      fdInverse: begin
        fFnc_fftN3 := _ifft3;
        fFnc_fftN4 := _ifft4;
        fFnc_fftN5 := _ifft5;
        fFnc_fftcomb2 := _fftcomb2;
        fFnc_fftcomb3 := _ifftcomb3;
        fFnc_fftcomb4 := _ifftcomb4;
        fFnc_fftcomb5 := _ifftcomb5;
      end;
    end;
  end;
end;

procedure TFFTEvalF64.InitFunctions;
begin
  InitFFTFunctionsF64(Self, fDir);
end;

{$endregion}

{$region 'TRealFFTEvalF64'}

procedure InitRealFFTFunctionsF64(aFFT: TRealFFTEval<Double, TCmplx128>);
begin
  InitFFTFunctionsF64(aFFT);

  with aFFT do begin
    fFnc_FTRecombFull := RealFTRecombFull;
    fFnc_FTRecombHalf := RealFTRecombHalf;
  end;
end;

procedure TRealFFTEvalF64.InitFunctions;
begin
  InitRealFFTFunctionsF64(Self);
end;

{$endregion}

{$region 'TRealIFFTEvalF64'}

procedure NormalizeF64(pSrc, pDst: PByte; aCount: NativeInt);
begin
  VecMul(PDouble(pSrc), 1/aCount, PDouble(pDst), aCount);
end;

procedure InitRealIFFTFunctionsF64(aFFT: TRealIFFTEval<Double, TCmplx128>);
begin
  InitFFTFunctionsF64(aFFT, fdInverse);

  with aFFT do begin
    fFnc_Norm := NormalizeF64;
    fFnc_FTReconstruct := RealFTReconstruct;
  end;
end;

procedure TRealIFFTEvalF64.InitFunctions;
begin
  InitRealIFFTFunctionsF64(Self);
end;

{$endregion}

{$region 'TRealFFTEval2DF64'}

procedure _interleaveRowsF64(X0, X1, Z: PByte; aCnt: NativeInt);
{$if defined(zzASMx64)}
// RCX <- X0, RDX <- X1, R8 <- Z, R9 <- aCnt
asm
  mov r10, r9
  shr r9, 1
  jz @rest
@L:
  movupd xmm0, [rcx]        // xmm0 <- (Re[i], Re[i+1])
  movupd xmm1, [rdx]        // xmm1 <- (Im[i], Im[i+1])
  movsd xmm2, xmm0
  movlhps xmm2, xmm1        // xmm2 <- (Re[i], Im[i])
  movhlps xmm1, xmm0        // xmm1 <- (Re[i+1], Im[i+1])
  movupd [r8], xmm2
  movupd [r8 + 16], xmm1
  add r8, 32
  add rcx, 16
  add rdx, 16
  dec r9
  jnz @L
@rest:
  and r10, 1
  jz @end
  movsd xmm0, [rcx]         // xmm0 <- Re[aCnt-1]
  movsd xmm1, [rdx]         // xmm1 <- Im[aCnt-1]
  movq [r8], xmm0
  movq [r8 + 8], xmm1
@end:
end;
{$else}
var pEnd: PByte;
begin
  pEnd := X0 + aCnt * SizeOf(Double);
  while X0 < pEnd do begin
    with PCmplx128(Z)^ do begin
      Re := PDouble(X0)^;
      Im := PDouble(X1)^;
    end;
    Inc(X0, cF64Sz);
    Inc(X1, cF64Sz);
    Inc(Z, cC128Sz);
  end;
end;
{$endif}

procedure RealRowPairFTRecomb(const y, c0, c1: TVecC128); overload;
{$if defined(ASMx64)}
const cHalf: Double = 0.5;
// RCX <- @y, RDX <- @c0, R8 <- @c1
asm
  mov r9, [rdx + 8]       // R9 <- c0.Stride
  mov rax, [rcx + 16]     // RAX <- N = y.Length
  mov rcx, [rcx]          // RCX <- @y[0]
  mov rdx, [rdx]          // RDX <- @c0[0]
  mov r8, [r8]            // R8 <- @c1[0]

  movupd xmm0, [rcx]      // xmm0 <- z := y[0]
  xorpd xmm1, xmm1
  movsd xmm1, xmm0        // xmm1 <- (z.Re, 0)
  movupd [rdx], xmm1
  movhlps xmm1, xmm0      // xmm1 <- (z.Im, 0)
  movupd [r8], xmm1
  add rdx, r9
  add r8, r9
  dec rax                 // RAX <- N - 1

  movupd xmm5, cImSgnMaskF64
  movddup xmm4, cHalf
  mov r10, rax
  shl r10, 4              // R10 <- (N - 1) * SizeOf(TCmplx128)
  add r10, rcx            // R10 <- @y[N - 1]
  add rcx, 16             // RCX <- @y[1]
  inc rax
  shr rax, 1
@L:
  movupd xmm0, [rcx]      // xmm0 <- z := y[k]
  movupd xmm1, [r10]      // xmm1 <- y[N - k]
  xorps xmm1, xmm5        // xmm1 <- zc := y[N - k]*
  movapd xmm2, xmm0
  addpd xmm2, xmm1        // xmm2 <- z + zc
  mulpd xmm2, xmm4        // xmm2 <- 1/2*(z + zc)
  movupd [rdx], xmm2      // c0[k] <- 1/2*(z + zc)

  subpd xmm0, xmm1        // xmm0 <- z - zc
  pshufd xmm0, xmm0, $4e  // xmm0 <- ((z-zc).im, (z-zc).re)
  xorps xmm0, xmm5        // xmm0 <- ((z-zc).im, -(z-zc).re)
  mulpd xmm0, xmm4        // xmm0 <- -1/2*I*(z - zc)
  movupd [r8], xmm0       // c1[k] <- -1/2*I*(z - zc)

  add rcx, 16
  sub r10, 16
  add rdx, r9
  add r8, r9
  dec rax
  jnz @L
end;
{$else}
var k, N: NativeInt;
    z, zc: TCmplx128;
begin
  Assert((c0.Stride = c1.Stride) and (y.Stride = SizeOf(TCmplx128)));

  z := y[0];
  c0[0] := z.Re;
  c1[0] := z.Im;

  N := y.Length;
  for k := 1 to N div 2 do begin
    z := y[k];
    zc := y[N - k].Conjugate;
    c0[k] := 1/2*(z + zc);
    c1[k] := -1/2*I*(z - zc);
  end;
end;
{$endif}

procedure InitRealFFT2DFunctionsF64(aFFT: TRealFFTEval2D<Double, TCmplx128>);
begin
  with aFFT do begin
    fFnc_RealRowPairFTRecomb := RealRowPairFTRecomb;
    fFnc_InterleaveRows := _interleaveRowsF64;
    fFnc_Copy := _copyC128;
    fFnc_Tr4 := CTr_16B;
  end;
end;

procedure TRealFFTEval2DF64.InitFunctions;
begin
  InitRealFFT2DFunctionsF64(Self);
  InitFFTFunctionsF64(fRowFFT);
  InitFFTFunctionsF64(fColFFT);
end;

{$endregion}

{$region 'TRealIFFTEval2DF64'}

procedure _splitRowF64(pSrc, pRow1, pRow2: PByte; aCount: NativeInt);
var pEnd: PByte;
begin
  pEnd := pSrc + aCount * cC128Sz;
  while pSrc < pEnd do begin
    with PCmplx128(pSrc)^ do begin
      PDouble(pRow1)^ := Re;
      PDouble(pRow2)^ := Im;
    end;
    Inc(pSrc, cC128Sz);
    Inc(pRow1, cF64Sz);
    Inc(pRow2, cF64Sz);
  end;
end;

procedure RealRowPairReconstruct(const c0, c1, y: TVec<TCmplx128>);
var k, N: NativeInt;
    z, zc, u, v: TCmplx128;
begin
  Assert((c0.Stride = c1.Stride) and (y.Stride = SizeOf(TCmplx128)));

//  z := y[0];
//  c0[0] := z.Re;
//  c1[0] := z.Im;

//  N := y.Length;
//  for k := 1 to N div 2 do begin
//    z := y[k];
//    zc := y[N - k].Conjugate;
//    c0[k] := 1/2*(z + zc);
//    c1[k] := -1/2*I*(z - zc);
//  end;

  z.Init(c0[0].Re, c1[0].Re);
  y[0] := z;

  N := y.Length;
  for k := 1 to N div 2 do begin
    u := c0[k];
    v := c1[k];
    v.Init(-v.Im, v.Re);
    z  := u + v;
    zc := u - v;
    y[k] := z;
    y[N - k] := zc.Conjugate;
  end;
end;

procedure InitRealIFFT2DFunctionsF64(aFFT: TRealIFFTEval2D<Double, TCmplx128>);
begin
  with aFFT do begin
    fFnc_RealRowPairReconstruction := RealRowPairReconstruct;
    fFnc_SplitRows := _splitRowF64;
    fFnc_Copy := _copyC128;
    fFnc_Tr4 := CTr_16B;
  end;
end;

procedure TRealIFFTEval2DF64.InitFunctions;
begin
  InitRealIFFT2DFunctionsF64(Self)
end;

{$endregion}

{$region 'TFFTThresholds'}

{$region 'Getters/Setters'}

procedure TFFTProps.SetRMThreshold(aValue: Integer);
begin
  fRMThreshold := NearestLowerPowerOfTwo(aValue);
end;

{$endregion}

{$endregion}

initialization

  g_FFTProps.RecursiveMethodThreshold := 2048;

end.
