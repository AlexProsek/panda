unit panda.Arrays;

interface

{$ifdef FPC}
  {$mode delphiunicode}{$H+}
{$endif}

uses
    TypInfo
  , SysUtils
  , Math
  , Generics.Collections
  , panda.Nums
  , panda.Intfs
  , panda.Consts
  , panda.vCvt
  ;

type
  TNDIdx = class abstract(TInterfacedObject, INDIndex)
  public
    function IndexType: TNDIndexType; virtual; abstract;
    function RawIndexData: PByte; virtual; abstract;
  end;

  TNDIntIdx = class(TNDIdx, INDIntIndex)
  protected
    fValue: NativeInt;
  public
    constructor Create(aValue: NativeInt);
    function IndexType: TNDIndexType; override;
    function RawIndexData: PByte; override;
    function GetValue: NativeInt;
    function INDIntIndex.Value = GetValue;

    property Value: NativeInt read fValue write fValue;
  end;

  TNDSpanIdx = class(TNDIdx, INDSpanIndex)
  protected
    fLo, fHi, fStep: NativeInt;
  public
    constructor Create(aLo, aHi, aStep: NativeInt);
    function IndexType: TNDIndexType; override;
    function RawIndexData: PByte; override;
    function GetLow: NativeInt;
    function GetHigh: NativeInt;
    function GetStep: NativeInt;
    function INDSpanIndex.Low = GetLow;
    function INDSpanIndex.High = GetHigh;
    function INDSpanIndex.Step = GetStep;

    property Low: NativeInt read fLo write fLo;
    property High: NativeInt read fHi write fHi;
    property Step: NativeInt read fStep write fStep;
  end;

  TNDSetIdx = class(TNDIdx, INDSetIndex)
  protected
    fIdxs: TArray<NativeInt>;
  public
    constructor Create(const aIdxs: array of NativeInt);
    function IndexType: TNDIndexType; override;
    function RawIndexData: PByte; override;
    function GetItem(I: NativeInt): NativeInt;
    function GetCount: NativeInt;
    function Indices: TArray<NativeInt>;

    property Item[I: NativeInt]: NativeInt read GetItem; default;
  end;

  SNDIntIndex = record
  private
    fIndex: NativeInt;
  public
    class operator Implicit(aValue: NativeInt): SNDIntIndex;
    class operator Implicit(aValue: SNDIntIndex): NativeInt;
    class operator Implicit(aValue: SNDIntIndex): INDIndex;
    class operator Add(A: SNDIntIndex; B: NativeInt): SNDIntIndex;
  end;

  TNDIEnum = class;

  SNDIRange = record
    fLo, fHi, fStep: NativeInt;
  public
    function GetEnumerator: TNDIEnum;

    property Low: NativeInt read fLo;
    property High: NativeInt read fHi;
    property Step: NativeInt read fStep;
  end;

  TNDIEnum = class
  protected
    fRng: SNDIRange;
    fIdx: NativeInt;
  public
    constructor Create(const aRng: SNDIRange);
    function GetCurrent: NativeInt;
    function MoveNext: Boolean;

    property Current: NativeInt read GetCurrent;
  end;

  TNDScalar<T> = class(TInterfacedObject, INDArray, INDArray<T>, INDScalar<T>)
  protected
    fValue: T;
    function GetValue: T;
    procedure SetValue(const aValue: T);
  public
    constructor Create(const aValue: T); overload;
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
    function GetPart(const aIdx: INDIndexSeq): INDArray<T>;
    procedure SetPart(const aIdx: INDIndexSeq; const aValue: INDArray<T>);
    function GetItem(const aIdx: array of NativeInt): T;
    procedure SetItem(const aIdx: array of NativeInt; const aValue: T);
    function Reshape(const aShape: array of NativeInt): INDArray<T>;

    property Value: T read fValue write fValue;
  end;

  // This base class only provides INDArray interface.
  TNDABase = class abstract(TInterfacedObject, INDArray)
  protected
    fShape: TArray<NativeInt>;
    fStrides: TArray<NativeInt>;
    fFlags: Cardinal;
  public
    function GetItemType: PTypeInfo; virtual; abstract;
    function Data: PByte; virtual; abstract;
    function NDim: Integer;
    function Shape: TArray<NativeInt>;
    function Strides: TArray<NativeInt>;
    function ItemSize: Integer; virtual; abstract;
    function Size: NativeInt;
    function Flags: Cardinal;
    procedure SetFlags(aValue: Cardinal);
    procedure UnsetFlags(aValue: Cardinal);
  end;

  // This base class contains common type-independent methods for
  // data manipulation (indexing, slicing, ...). Its main reason is to
  // reduce code bloating caused by using generics
  TNDA = class abstract(TNDABase)
  protected
    function GetStrides(const aShape: TArray<NativeInt>): TArray<NativeInt>;
    function GetOffset(const aIdx: array of NativeInt): NativeInt;
    function GetFillRFunc: TIPProcVV; virtual; abstract;
    procedure SetPart_SetIdx(const aIdx: INDIndexSeq; const aValue: INDArray); virtual;
    procedure SetSlicePart(const aSlice: INDArray; const aIdx: INDIndexSeq; const aValue: INDArray); virtual; abstract;
    function PermuteAxes(const aPerm: array of Integer): INDArray; virtual; abstract;
    procedure InitPermuted(const aShape, aStrides: TArray<NativeInt>; const aAxesPerm: array of Integer);
  end;

  // This base class provides INDArray<T> interface.
  TNDA<T> = class abstract(TNDA, INDArray<T>)
  public type
    PT = ^T;
  protected
    function GetItemPart(const aIdx: array of INDIndex): T;
    procedure SetItemPart(const aIdx: array of INDIndex; const aValue: T);
    procedure SetSlicePart(const aSlice: INDArray; const aIdx: INDIndexSeq; const aValue: INDArray); override;
    function GetFillRFunc: TIPProcVV; override;
    function PermuteAxes(const aPerm: array of Integer): INDArray; override;
  public
    function GetItemType: PTypeInfo; override;
    function ItemSize: Integer; override;
    function GetItem(const aIdx: array of NativeInt): T;
    procedure SetItem(const aIdx: array of NativeInt; const aValue: T);
    function GetPart(const aIdx: INDIndexSeq): INDArray<T>;
    procedure SetPart(const aIdx: INDIndexSeq; const aValue: INDArray<T>);
    function Reshape(const aShape: array of NativeInt): INDArray<T>;

    property Item[const aIdx: array of NativeInt]: T read GetItem write SetItem;
    property Part[const aIdx: INDIndexSeq]: INDArray<T> read GetPart write SetPart; default;
  end;

  TNDArray<T> = class(TNDA<T>)
  protected
    fData: PByte;
  public
    constructor Create(aData: Pointer; const aShape, aStrides: TArray<NativeInt>);
    function Data: PByte; override;
  end;

  TNDPackedArray<T> = class(TNDA<T>)
  protected
    fData: PByte;
  public
    constructor Create(aData: Pointer; const aDims: array of NativeInt); overload;
    function Data: PByte; override;
  end;

  TNDABuffer<T> = class(TNDPackedArray<T>)
  protected
    fBuff: PByte;
    function InitBuffer(const aDims: array of NativeInt; aAlignment: Integer): PByte;
  public
    constructor Create(const aDims: array of NativeInt); overload;
    constructor Create(const aDims: array of NativeInt; aAlignment: Integer); overload;
    constructor Create(const aDims: array of NativeInt; const aValue: T;
      aAlignment: Integer); overload;
    destructor Destroy; override;
    procedure AfterConstruction; override;
  end;

  TNDArrayWrapper<T> = class(TNDA<T>)
  protected
    fArray: INDArray<T>;
  public
    constructor Create(const aArray: INDArray<T>); overload;
    constructor Create(const aArray: INDArray<T>; const aShape: array of NativeInt); overload;
    constructor CreatePermuted(const aArray: INDArray<T>; const aAxesPerm: array of Integer); overload;
    function Data: PByte; override;
  end;

  TDynArrWrapper<T> = class(TNDA<T>)
  protected
    fArray: TArray<T>;
  public
    constructor Create(const aArray: TArray<T>); overload;
    constructor Create(const aArray: TArray<T>; const aShape: array of NativeInt); overload;
    function Data: PByte; override;
  end;

  TNDArrayView = class(TNDABase)
  protected
    fArray: INDArray;
    fOffset: NativeInt;
  public
    constructor Create(const aArray: INDArray; const aIdx: INDIndexSeq;
      const aAxesPerm: TArray<Integer> = nil);
    function GetItemType: PTypeInfo; override;
    function Data: PByte; override;
    function ItemSize: Integer; override;
    class procedure GetViewProps(const aArray: INDArray; const aIdx: INDIndexSeq;
      out aShape, aStrides: TArray<NativeInt>;
      out aOffset: NativeInt; out aFlags: Cardinal;
      const aAxesPerm: TArray<Integer> = nil);
  end;

  TNDArrayView<T> = class(TNDA<T>)
  protected
    fArray: INDArray<T>;
    fOffset: NativeInt;
  public
    constructor Create(const aArray: INDArray<T>; const aIdx: INDIndexSeq);
    function Data: PByte; override;
  end;

  TVecView<T> = class(TNDA<T>, INDAVec<T>)
  protected
    fData: PByte;
    procedure Init(aData: PByte; aCount: NativeInt; aStride: NativeInt);
  public
    constructor Create(aData: PByte; aCount: NativeInt; aStride: NativeInt = 1);
    function Data: PByte; override;
    procedure SetData(aData: PByte);
    function Stride: NativeInt;
    function Length: NativeInt;
  end;

  TVecBuffer<T> = class(TVecView<T>)
  public
    constructor Create(aSize: NativeInt);
    destructor Destroy; override;
  end;

  TNDArrAxisView<T> = class(TVecView<T>)
  protected
    fArray: INDArray<T>;
  public
    constructor Create(const aArray: INDArray<T>; aAxis: Integer; aPos: TArray<NativeInt> = nil);
  end;

  TNDAIt = class
  protected type

    TNDAPos = record
      Offset: NativeInt;
      Base: NativeInt;
      Last: NativeInt;
      Step: NativeInt;
    end;
    PNDAPos = ^TNDAPos;

  protected
    fArr: INDArray;
    fData: PByte;
    fCurrent: PByte;
    fLoLvl: Integer;
    fHiLvl: Integer;
    fSt: TArray<TNDAPos>;
    fTopIt: PNDAPos;
    fBaseIt: PByte;
    fElSz: Integer;
  {$region 'Getters/Setters'}
    function GetLowLvl: Integer;
    function GetHighLvl: Integer;
  {$endregion}
  public
    constructor Create(const aArr: INDArray;
      const aIdxPerm: TArray<Integer> = nil); overload;
    constructor Create(const aArr: INDArray; aHiLvl: Integer;
      const aIdxPerm: TArray<Integer> = nil); overload;
    constructor Create(const aArr: INDArray; aLoLvl, aHiLvl: Integer;
      const aIdxPerm: TArray<Integer> = nil); overload; virtual;
    function MoveNext: Boolean; virtual;
    procedure Reset; virtual;
    procedure ResetData; virtual;

    property Current: PByte read fCurrent;
    property LowLevel: Integer read GetLowLvl;
    property HighLvl: Integer read GetHighLvl;
  end;

  TNDIdxIt = class
  protected type

    TNDIPos = record
      Base: NativeInt;
      Last: NativeInt;
      Step: NativeInt;
      Idx: PNativeInt;
    end;
    PNDIPos = ^TNDIPos;

  protected
    fSt: TArray<TNDIPos>;
    fTopIt: PNDIPos;
    fBaseIt: PByte;
    fIdx: TArray<NativeInt>;
  public
    constructor Create(const aShape: TArray<NativeInt>;
      const aRevAxis: TArray<Boolean> = nil; const aIdxPerm: TArray<Integer> = nil);
    function MoveNext: Boolean;

    property CurrentIndex: TArray<NativeInt> read fIdx;
  end;

  TNDASliceIt = class(TNDAIt)
  protected type

    TSliceView = class(TNDArrayView)
    protected
      fData: PByte;
      procedure SetData(aData: PByte);
    public
      constructor Create(const aArray: INDArray; const aAxes: TArray<Integer>;
        const aAxesPerm: TArray<Integer> = nil); overload;
      function Data: PByte; override;
    end;

  protected
    fSlice: INDArray;
    fSliceEntry: TSliceView;
    function CreateSliceView(const aArray: INDArray;
      const aAxes: TArray<Integer>): TSliceView; virtual;
  public
    constructor Create(const aArr: INDArray; aLoLvl, aHiLvl: Integer;
      const aIdxPerm: TArray<Integer> = nil); override;
    function MoveNext: Boolean; override;

    property CurrentSlice: INDArray read fSlice;
  end;

  /// <summary>
  ///  <c>TNDASliceSetIt</c> goes over the specified set of indices only at
  ///  the first level.
  /// </summary>
  TNDASliceSetIt = class(TNDASliceIt)
  protected
    fIdxs: TArray<NativeInt>;
    fPos: NativeInt;
    fStride: NativeInt;
  public
    constructor Create(const aArr: INDArray; const aIdxs: TArray<NativeInt>);
    function MoveNext: Boolean; override;
    procedure Reset; override;
  end;

  TNDAConstSliceIt = class(TNDASliceIt)
  protected
    fPos: Integer;
  public
    constructor Create(const aSlice: INDArray);
    function MoveNext: Boolean; override;
    procedure Reset; override;
  end;

  TNDASliceItChain = class
  protected
    fIts: TArray<TNDASliceIt>;
    fData, fSlice: INDArray;
  public
    constructor Create(const aArr: INDArray; const aIdx: INDIndexSeq);
    destructor Destroy; override;
    function MoveNext: Boolean;
    procedure Reset;

    property CurrentSlice: INDArray read fSlice;
  end;

  TNDAVecItems<T> = record
  private
    fArr: INDArray<T>;
    fData: PByte;
    fStride: NativeInt;
    fLength: NativeInt;
  public
    class operator Implicit(const aArr: INDArray<T>): TNDAVecItems<T>; overload;
    class operator Implicit(const aVec: TNDAVecItems<T>): INDArray<T>; overload;
    procedure Init(N: NativeInt); overload;
    procedure Init(const aItems: TArray<T>); overload;
    function GetItem(aIdx: NativeInt): T; inline;
    procedure SetItem(aIdx: NativeInt; const aValue: T); inline;

    property Item[aIdx: NativeInt]: T read GetItem write SetItem; default;
    property Length: NativeInt read fLength;
    property Base: INDArray<T> read fArr;
  end;

  TNDAMatItems<T> = record
  private
    fArr: INDArray<T>;
    fData: PByte;
    fStrides: array [0..1] of NativeInt;
    fRowCount, fColCount: NativeInt;
  public
    class operator Implicit(const aArr: INDArray<T>): TNDAMatItems<T>; overload;
    class operator Implicit(const aMat: TNDAMatItems<T>): INDArray<T>; overload;
    procedure Init(aRowCount, aColCount: NativeInt);
    function GetItem(I, J: NativeInt): T; inline;
    procedure SetItem(I, J: NativeInt; const aValue: T); inline;

    property Item[I, J: NativeInt]: T read GetItem write SetItem; default;
    property RowCount: NativeInt read fRowCount;
    property ColCount: NativeInt read fColCount;
    property Base: INDArray<T> read fArr;
  end;

  TNDAFactory = class abstract
  public
    function CreateNew(const aShape: array of NativeInt): INDArray; virtual; abstract;
  end;

  TNDABuffFactory<T> = class(TNDAFactory)
  public
    function CreateNew(const aShape: array of NativeInt): INDArray; override;
  end;

  TNDAUt = class
  public type
    TOArray<T> = array of T;  // open array
    TOArray2D<T> = array of TOArray<T>;
  protected class var
    fConstrs: TDictionary<PTypeInfo, TNDAFactory>;
    fCvtFuncs: TDictionary<TPair<PTypeInfo, PTypeInfo>, TIPProcVV>;
  protected
    class function TryGetCvtFunc<T, U>(out aCvtFunc: TIPProcVV): Boolean; overload; static;
    class function TryGetCvtFunc(T, U: PTypeInfo; out aCvtFunc: TIPProcVV): Boolean; overload; static;

    // Maps aFnc over two arrays with the same shape. The aFnc determines direction (L <- F(L,R) or R <- F(L,R) )
    class procedure MapSS(const L, R: INDArray; aFnc: TIPProcVV); overload; static;
    class procedure MapSS(const L, R: INDArray; aFnc: TIPProcVV; aCLvl: Integer; aCSz: NativeInt); overload; static;
    // L <- aRFnc(L, r)
    class procedure MapR(const L: INDArray; pR: PByte; aRFnc: TIPProcVV); overload; static;
    // R <- aLFnc(l, R)
    class procedure MapL(pL: PByte; const R: INDArray; aLFnc :TIPProcVV); overload; static;
    // L <- aRFnc(L, R)
    class procedure MapR(const L, R: INDArray; aRFnc: TIPProcVV); overload; static;
    // R <- aLFnc(L, R)
    class procedure MapL(const L, R: INDArray; aLFnc: TIPProcVV); overload; static;

    class procedure MapR<T>(const L: INDArray<T>; const R: T; aRFnc: TIPProcVV); overload; static;
    class procedure MapL<T>(const L: T; const R: INDArray<T>; aLFnc :TIPProcVV); overload; static;

    class procedure FillR<T>(N: NativeInt; L: PByte; IncL: NativeInt; R: PByte; IncR: NativeInt); static;
    class procedure Copy<T>(aSrc, aDst: PByte; aCount: NativeInt); overload; static;
    class procedure Copy<T>(aSrc, aDst: PByte; aCount, aStep: NativeInt); overload; static;
    class procedure Copy<T>(const aArr: INDArray; aDst: PByte); overload; static;
    class function Permute<T>(const aData: TArray<T>; const aIndices: array of Integer): TArray<T>;
  public
    class constructor Create;
    class destructor Destroy;
    class procedure AddArrayFactory(aT: PTypeInfo; aFactory: TNDAFactory);
    class procedure AddCvtFunc<T, U>(aFunc: TIPProcVV);
    /// <summary>
    ///   Returns a new array of given shape and type, without initializing entries.
    /// </summary>
    class function Empty(aT: PTypeInfo; aDims: array of NativeInt): INDArray; overload; static;
    class function Empty<T>(aDims: array of NativeInt): INDArray<T>; overload; static;
    /// <summary>
    ///   Return a new array of given shape and type, filled with <c>aValue</c>.
    /// </summary>
    class function Full<T>(const aDims: array of NativeInt; const aValue: T): INDArray<T>; overload; static;
    class function Full<T>(const aDims: array of NativeInt; const aValue: INDArray<T>): INDArray<T>; overload; static;
    /// <summary>
    ///   Returns the a square array with ones on the main diagonal.
    /// </summary>
    class function Identity<T>(aN: Integer): INDArray<T>; static;
    /// <summary>
    ///   Fills the main diagonal of the given array of any dimensionality.
    /// </summary>
    class procedure FillDiagonal<T>(const aArr: INDArray<T>; const aValue: T); static;
    class function AsArray<T>(const aItems: array of T; const aDims: array of NativeInt): INDArray<T>; overload; static;
    class function AsArray<T>(const aItems: array of T): INDArray<T>; overload; static;
    class function AsArray<T>(const aItems: TOArray2D<T>): INDArray<T>; overload; static;
    class function Scalar<T>(const aValue: T): INDScalar<T>; static;
    class function TryAsScalar<T>(const aArr: INDArray; out aValue: T): Boolean; static;
    class function TryAsDynArray<T>(const aArr: INDArray; out aValue: TArray<T>): Boolean; static;
    class function TryAsDynArray2D<T>(const aArr: INDArray; out aValue: TArray<TArray<T>>): Boolean; static;
    class function TryAsArray<T>(const aArr: INDArray; out aValue: INDArray<T>): Boolean; static;

    class procedure Map<T, U, V>(const L: INDArray<T>; const R: U; var aRes: INDArray<V>; aRFnc: TIPProcVV); overload; static;
    class procedure Map<T, U, V>(const L: T; const R: INDArray<U>; var aRes: INDArray<V>; aLFnc: TIPProcVV); overload; static;
    class procedure Map<T, U, V>(const L: INDArray<T>; const R: INDArray<U>; var aRes: INDArray<V>; aLFnc, aRFnc: TIPProcVV); overload; static;
    class procedure Map<T>(const L: INDArray<T>; const R: T; var aRes: INDArray<T>; aRFnc: TIPProcVV); overload; static;
    class procedure Map<T>(const L: T; const R: INDArray<T>; var aRes: INDArray<T>; aLFnc: TIPProcVV); overload; static;
    class procedure Map<T>(const L: INDArray<T>; const R: INDArray<T>; var aRes: INDArray<T>; aLFnc, aRFnc: TIPProcVV); overload; static;

    class procedure Fill<T>(const aArr: INDArray<T>; const aValue: T); overload; static;
    class procedure Fill<T>(const aArr: INDArray<T>; const aValue: INDArray<T>); overload; static;
    class function Copy<T>(const aArr: INDArray<T>): INDArray<T>; overload; static;
    class function TryAsType<T>(const aArr: INDArray; out aRes: INDArray<T>; aForceCopy: Boolean = False): Boolean; static;
    class function AsType<T>(const aArr: INDArray; aForceCopy: Boolean = False): INDArray<T>; overload; static;
    class function AsType<T>(const aArrays: array of INDArray; aForceCopy: Boolean = False): TArray<INDArray<T>>; overload; static;
    class function AsContiguousArray<T>(const aArr: INDArray<T>): INDArray<T>; overload; static;
  end;

  function SameQ(const aIdx1, aIdx2: array of NativeInt): Boolean; overload;
  function SameQ(const aIdx1, aIdx2: array of NativeInt; out aUpTo: Integer): Boolean; overload;
  function SameUpToQ(const aIdx1, aIdx2: array of NativeInt; aUpTo: Integer): Boolean; overload;
  function SameShapeQ(const A, B: INDArray): Boolean; inline;
  function ValidShapeQ(const aShape: TNDAShape): Boolean; inline;
  function AxesPermQ(const aAxes: array of Integer): Boolean;
  function IdPermQ(const aAxes: array of Integer): Boolean;
  function GetItemSize(aType: PTypeInfo): Integer;
  function GetSize(const aDims: array of NativeInt): NativeInt; overload;
  function GetSize(const aArr: INDArray): NativeInt; overload; inline;
  function NBytes(const aArr: INDArray): NativeInt; inline;
  function ScalarQ(const aArr: INDArray): Boolean;
  function VectorQ(const aArr: INDArray): Boolean;
  function MatrixQ(const aArr: INDArray): Boolean;
  function GetCContLvl(const aArr: INDArray; out aBlockSz: NativeInt): Integer;
  function GetCommonCContLvl(const A, B: INDArray; out aSz: NativeInt): Integer;
  function CheckCContLvl(const aArr: INDArray; aRequiredLvl: Integer): Boolean;
  function BroadcastLvl(A, B: INDArray): Integer;
  function GetPartShape(const A: INDArray; const aIdx: INDIndexSeq): TNDAShape;
  function CompatibleQ(const aShapes: array of TNDAShape; out aResShape: TNDAShape): Boolean; overload;
  function CompatibleQ(const aArrays: array of INDArray; out aResShape: TNDAShape; out aType: PTypeInfo): Boolean; overload;
  function GetCommonType(const aArrays: array of INDArray; out aType: PTypeInfo): Boolean;
  function ContiguousQ(aElemSz: Integer; const aShape, aStrides: array of NativeInt): Boolean; overload;

  function NDISpan(aLo: NativeInt = 0; aHi: NativeInt = -1; aStep: NativeInt = 1): INDSpanIndex; inline;
  function NDISet(const aIndices: array of NativeInt): INDSetIndex;
  function NDI(aIdx: NativeInt): INDIntIndex; overload; inline;
  function NDI(const aIndices: array of NativeInt): INDIndex; overload;
  function NDI(const aIndices: INDArray<NativeInt>): INDSetIndex; overload;
  function NDIAll(aStep: NativeInt = 1): INDSpanIndex; inline;
  function NDIAllTo(aTo: NativeInt; aStep: NativeInt = 1): INDSpanIndex; inline;
  function NDIAllFrom(aFrom: NativeInt; aStep: NativeInt = 1): INDSpanIndex; inline;
  function NDIAllSeq(aCount: Integer): INDIndexSeq;
  function NDIIntSeq(aCount: Integer): INDIndexSeq; overload;
  function NDIFirst: INDIntIndex; inline;
  function NDILast: INDIntIndex; inline;

  function NDIFreeQ(const aIndices: array of INDIndex; aType: TNDIndexType): Boolean; overload;
  function NDIFreeQ(const aIndices: array of INDIndex; const aIdxTypes: TNDIndexTypes): Boolean; overload;
  procedure NDITypes(const aIndices: array of INDIndex; out aIdxTypes: TNDIndexTypes);
  function NDISpanSize(const aItemCount: NativeInt; const aIdx: INDSpanIndex): NativeInt;
  function NDISpanLow(const aItemCount: NativeInt; const aIdx: INDSpanIndex): NativeInt; inline;
  function NDISpanIndices(const aItemCount: NativeInt; const aIdx: INDSpanIndex;
    out aLo, aHi, aStep: NativeInt): Boolean;
  procedure NDISplit(const aIdx: INDIndexSeq; out aSetIdx: INDIndexSeq; out aSpanIdx: INDIndexSeq;
    out aAxesPerm: TArray<Integer>);
  function NDIToStr(const aIndices: array of NativeInt): String; overload;
{$ifdef FPC}
  function NDIToStr(const aIndices: TArray<NativeInt>): String; overload;
{$endif}
  function NDICheckRange(const aIndices: INDIndexSeq; const aShape: TNDAShape): Boolean;
  function NDIRange(aLo, aHi: NativeInt; aStep: NativeInt = 1): SNDIRange;
  function ShapeToStr(const aArr: INDArray): String; inline;

  /// <summary>
  /// Moves array elements with step <c>aStep</c> into the packed array.
  /// </summary>
  /// <param Name="aSrc">Pointer of the first item of the source array.</param>
  /// <param Name="aDst">Pointer of the first item of the destination array.</param>
  /// <param Name="aCount">Number of source array elements.</param>
  /// <param Name="aStep">Step between two consecutive items of the source aray.</param>
  /// <param Name="aElSt">Item size in bytes.</param>
  procedure PackBytes(aSrc, aDst: PByte; aCount, aStep, aElSz: NativeInt);
  /// <summary>
  ///  Returns pointer to the packed array elements. If the <c>aArr</c> is contiguous then
  ///  the <c>aArr.Data</c> is returned else the <c>aBuff</c> is filled with the <c>aArr</c>
  ///  elements and the result points to this buffer.
  /// </summary>
  function GetPackedDataPtr(const aArr: INDArray; var aBuff: TBytes): PByte;

implementation

uses
    panda.cvArithmetic
  ;

function SameQ(const aIdx1, aIdx2: array of NativeInt): Boolean;
var I: Integer;
begin
  if Length(aIdx1) <> Length(aIdx2) then exit(False);

  for I := 0 to High(aIdx1) do
    if aIdx1[I] <> aIdx2[I] then exit(False);
  Result := True;
end;

function SameQ(const aIdx1, aIdx2: array of NativeInt; out aUpTo: Integer): Boolean;
var I, cnt: Integer;
begin
  cnt := Min(Length(aIdx1), Length(aIdx2));
  for I := 0 to cnt - 1 do
    if aIdx1[I] <> aIdx2[I] then begin
      aUpTo := I - 1;
      exit(False);
    end;

  aUpTo := cnt - 1;
  Result := (Length(aIdx1) = Length(aIdx2));
end;

function SameUpToQ(const aIdx1, aIdx2: array of NativeInt; aUpTo: Integer): Boolean;
var I, cnt: Integer;
begin
  cnt := Min(Length(aIdx1), Length(aIdx2));
  if cnt < aUpTo then exit(False);

  for I := 0 to aUpTo do
    if aIdx1[0] <> aIdx2[I] then exit(False);
  Result := True;
end;

function SameShapeQ(const A, B: INDArray): Boolean;
{$ifdef FPC}
var sA, sB: TArray<NativeInt>;
begin
  sA := A.Shape;
  sB := B.Shape;
  Result := SameQ(sA, sB);
end;
{$else}
begin
  Result := SameQ(A.Shape, B.Shape);
end;
{$endif}

function ValidShapeQ(const aShape: TNDAShape): Boolean;
var I: Integer;
begin
  for I := 0 to High(aShape) do
    if aShape[I] <= 0 then exit(False);
  Result := True;
end;

function AxesPermQ(const aAxes: array of Integer): Boolean;
var I, count: Integer;
    tmp: TArray<Integer>;
begin
  count := Length(aAxes);
  if count = 0 then exit(False);
  SetLength(tmp, count);
  Move(aAxes[0], tmp[0], count * SizeOf(Integer));
  TArray.Sort<Integer>(tmp);
  for I := 0 to count - 1 do
    if tmp[I] <> I then exit(False);
  Result := True;
end;

function IdPermQ(const aAxes: array of Integer): Boolean;
var I: Integer;
begin
  for I := 0 to High(aAxes) do
    if aAxes[I] <> I then exit(False);
  Result := True;
end;

function ScalarQ(const aArr: INDArray): Boolean;
begin
  Result := (Length(aArr.Shape) = 0);
end;

function VectorQ(const aArr: INDArray): Boolean;
begin
  Result := (Length(aArr.Shape) = 1);
end;

function MatrixQ(const aArr: INDArray): Boolean;
begin
  Result := (Length(aArr.Shape) = 2);
end;

function GetItemSize(aType: PTypeInfo): Integer;
begin
  if aType = nil then
    Exit(0);

  case aType^.Kind of
    tkInteger, tkChar, tkEnumeration, tkSet:
      case GetTypeData(aType)^.OrdType of
        otSByte, otUByte: Result := SizeOf(Byte);
        otSWord, otUWord: Result := SizeOf(Word);
        otSLong, otULong: Result := SizeOf(Integer);
      else
        Result := 0;
      end;

    tkInt64:
      Result := SizeOf(Int64);

    tkFloat:
      case GetTypeData(aType)^.FloatType of
        ftSingle: Result := SizeOf(Single);
        ftDouble: Result := SizeOf(Double);
        ftExtended: Result := SizeOf(Extended);
        ftComp: Result := SizeOf(Comp);
        ftCurr: Result := SizeOf(Currency);
      else
        Result := 0;
      end;

    tkArray:
      Result := GetTypeData(aType)^.ArrayData.Size;

    tkRecord:
      Result := GetTypeData(aType)^.RecSize;

    tkPointer, tkClass, tkLString, tkWString, tkUString, tkDynArray:
      Result := SizeOf(Pointer); // All pointer-based types
  else
    Result := 0; // Other types (interfaces, methods, etc.)
  end;
end;

function GetSize(const aDims: array of NativeInt): NativeInt;
var I: Integer;
begin
  Result := 1;
  for I := 0 to High(aDims) do begin
    Assert(aDims[I] > 0);
    Result := Result * aDims[I];
  end;
end;

function GetSize(const aArr: INDArray): NativeInt;
begin
  Result := GetSize(aArr.Shape);
end;

function NBytes(const aArr: INDArray): NativeInt;
begin
  Result := GetSize(aArr) * aArr.ItemSize;
end;

function GetCContLvl(const aArr: INDArray; out aBlockSz: NativeInt): Integer;
var shape, strides: TArray<NativeInt>;
    nDim: Integer;
begin
  nDim := aArr.NDim;
  shape := aArr.Shape;
  strides := aArr.Strides;
  aBlockSz := aArr.ItemSize;
  Result := nDim - 1;
  if strides[Result] <> aBlockSz then exit(nDim);
  while Result > 0 do begin
    aBlockSz := shape[Result] * aBlockSz;
    if strides[Result - 1] <> aBlockSz then break;
    Dec(Result);
  end;
  if Result = 0 then
    aBlockSz := shape[0] * aBlockSz;
end;

function GetCommonCContLvl(const A, B: INDArray; out aSz: NativeInt): Integer;
var lvlA, lvlB: Integer;
    szA, szB: NativeInt;
begin
  lvlA := GetCContLvl(A, szA);
  lvlB := GetCContLvl(B, szB);
  Result := Max(lvlA, lvlB);
  aSz := Min(szA, szB);
end;

function CheckCContLvl(const aArr: INDArray; aRequiredLvl: Integer): Boolean;
var bSz: NativeInt;
begin
  Result := (GetCContLvl(aArr, bSz) <= aRequiredLvl);
end;

function BroadcastLvl(A, B: INDArray): Integer;
var sA, sB: TArray<NativeInt>;
    hiLvlA, hiLvlB, I: Integer;
begin
  sA := A.Shape;
  sB := B.Shape;
  hiLvlA := High(sA);
  hiLvlB := High(sB);
  Assert(hiLvlA >= hiLvlB);
  Result := Min(hiLvlA, hiLvlB);
  for I := 0 to Result do
    if sA[hiLvlA - I] <> sB[hiLvlB - I] then exit(-1);
  Result := hiLvlA - Result;
end;

function GetPartShape(const A: INDArray; const aIdx: INDIndexSeq): TNDAShape;
var I, J, top, nDim, seqLen: Integer;
    shape: TNDAShape;
begin
  nDim := A.NDim;
  seqLen := Length(aIdx);
  if seqLen > nDim then
    raise ENDAIndexError.CreateFmt(csTooManyIndices, [nDim, seqlen]);

  shape := A.Shape;
  top := 0;
  SetLength(Result, nDim);
  for I := 0 to seqLen - 1 do
    case aIdx[I].IndexType of
      nditInt: ;
      nditSpan: begin
        Result[top] := NDISpanSize(shape[I], aIdx[I] as INDSpanIndex);
        Inc(top);
      end;
      nditSet: begin
        Result[top] := (aIdx[I] as INDSetIndex).Count;
        Inc(top);
      end;
    else
      raise EArgumentOutOfRangeException.CreateFmt(
        'NDIndexType(%d) is out of range.', [Ord(aIdx[I].IndexType)]
      );
    end;

  for J := seqLen to nDim - 1 do begin
    Result[top] := shape[J];
    Inc(top);
  end;
  SetLength(Result, top);
end;

function CompatibleQ(const aShapes: array of TNDAShape; out aResShape: TNDAShape): Boolean;
var I, J, K, count, nDim: Integer;
    nDims: TArray<NativeInt>;
    d, dma: NativeInt;
begin
  count := Length(aShapes);
  Assert(count > 0);
  if count = 1 then begin
    aResShape := aShapes[0];
    exit(True);
  end;

  nDim := 0;
  SetLength(nDims, count);
  for I := 0 to count - 1 do begin
    nDims[I] := Length(aShapes[I]);
    nDim := Max(nDim, nDims[I]);
  end;

  SetLength(aResShape, nDim);
  for J := 1 to nDim do begin
    dma := 1;
    for I := 0 to count - 1 do begin
      K := nDims[I] - J;
      if K < 0 then continue;
      d := aShapes[I, K];
      if d > 1 then begin
        if dma > 1 then begin
          if d <> dma then exit(False);
          continue;
        end;
        dma := d;
      end;
    end;
    aResShape[nDim - J] := dma;
  end;
  Result := True;
end;

function CompatibleQ(const aArrays: array of INDArray; out aResShape: TNDAShape;
  out aType: PTypeInfo): Boolean;
var shapes: TArray<TNDAShape>;
    I, count: Integer;
begin
  count := Length(aArrays);
  Assert(count > 0);
  SetLength(shapes, count);
  for I := 0 to count - 1 do
    shapes[I] := aArrays[I].Shape;

  Result := CompatibleQ(shapes, aResShape) and GetCommonType(aArrays, aType);
end;

function GetCommonType(const aArrays: array of INDArray; out aType: PTypeInfo): Boolean;
var I: Integer;
begin
  if Length(aArrays) = 0 then exit(False);

  aType := aArrays[0].GetItemType;
  for I := 1 to High(aArrays) do begin
    if not SameQ(aArrays[I].GetItemType, aType) then exit(False);

    // todo
  end;
  Result := True;
end;

function ContiguousQ(aElemSz: Integer; const aShape, aStrides: array of NativeInt): Boolean;
var hi, I: Integer;
    s: NativeInt;
begin
  Assert(Length(aShape) = Length(aStrides));
  s := aElemSz;
  hi := High(aShape);
  for I := hi downto 0 do begin
    if s <> aStrides[I] then exit(False);
    s := s * aShape[I];
  end;
  Result := True;
end;

{$region 'ND indices costructors'}

function NDISpan(aLo: NativeInt = 0; aHi: NativeInt = -1; aStep: NativeInt = 1): INDSpanIndex;
begin
  Result := TNDSpanIdx.Create(aLo, aHi, aStep);
end;

function NDISet(const aIndices: array of NativeInt): INDSetIndex;
begin
  Result := TNDSetIdx.Create(aIndices);
end;

function NDI(aIdx: NativeInt): INDIntIndex;
begin
  Result := TNDIntIdx.Create(aIdx);
end;

function NDI(const aIndices: array of NativeInt): INDIndex;
begin
  if Length(aIndices) = 1 then
    Result := TNDSpanIdx.Create(aIndices[0], aIndices[0], 1)
  else
    Result := TNDSetIdx.Create(aIndices);
end;

function NDI(const aIndices: INDArray<NativeInt>): INDSetIndex;
begin
  Assert(aIndices.NDim = 1);
  raise ENotImplemented.Create('NDI(NDArray<Int> is not implemented yet.');
end;

function NDIAll(aStep: NativeInt): INDSpanIndex;
begin
  Assert(aStep <> 0);
  if aStep > 0 then
    Result := TNDSpanIdx.Create(0, -1, aStep)
  else
    Result := TNDSpanIdx.Create(-1, 0, aStep);
end;

function NDIAllTo(aTo: NativeInt; aStep: NativeInt = 1): INDSpanIndex;
begin
  Assert(aStep <> 0);
  if aStep > 0 then
    Result := TNDSpanIdx.Create(0, aTo, aStep)
  else
    Result := TNDSpanIdx.Create(-1, aTo, aStep);
end;

function NDIAllFrom(aFrom: NativeInt; aStep: NativeInt = 1): INDSpanIndex;
begin
  Assert(aSTep <> 0);
  if aStep > 0 then
    Result := TNDSpanIdx.Create(aFrom, -1, aStep)
  else
    Result := TNDSpanIdx.Create(aFrom, 0, aStep);
end;

function NDIAllSeq(aCount: Integer): INDIndexSeq;
var I: Integer;
    idx: INDIndex;
begin
  SetLength(Result, aCount);
  idx := NDIAll();
  for I := 0 to aCount - 1 do
    Result[I] := idx;
end;

function NDIIntSeq(aCount: Integer): INDIndexSeq;
var I: Integer;
begin
  SetLength(Result, aCount);
  for I := 0 to aCount - 1 do
    Result[I] := NDI(0);
end;


function NDIFirst: INDIntIndex;
begin
  Result := TNDIntIdx.Create(0);
end;

function NDILast: INDIntIndex;
begin
  Result := TNDIntIdx.Create(-1);
end;

{$endregion}

{$region 'ND indices utils}

function NDIFreeQ(const aIndices: array of INDIndex; aType: TNDIndexType): Boolean;
var I: Integer;
begin
  for I := 0 to High(aIndices) do
    if aIndices[I].IndexType = aType then exit(False);
  Result := True;
end;

function NDIFreeQ(const aIndices: array of INDIndex; const aIdxTypes: TNDIndexTypes): Boolean;
var I: Integer;
begin
  for I := 0 to High(aIndices) do
    if aIndices[I].IndexType in aIdxTypes then exit(False);
  Result := True;
end;

procedure NDITypes(const aIndices: array of INDIndex; out aIdxTypes: TNDIndexTypes);
var I: Integer;
begin
  aIdxTypes := [];
  for I := 0 to High(aIndices) do
    aIdxTypes := aIdxTypes + [aIndices[I].IndexType];
end;

function NDISpanSize(const aItemCount: NativeInt; const aIdx: INDSpanIndex): NativeInt;
var lo, hi, step: NativeInt;
begin
  lo := aIdx.Low;
  if lo < 0 then lo := (lo + aItemCount) mod aItemCount;
  hi := aIdx.High;
  if hi < 0 then hi := (hi + aItemCount) mod aItemCount;
  step := aIdx.Step;
  Assert(step <> 0);
  Result := Max(0, 1 + ((hi - lo) div step));
end;

function NDISpanLow(const aItemCount: NativeInt; const aIdx: INDSpanIndex): NativeInt;
begin
  Result := aIdx.Low;
  if Result < 0 then Result := (Result + aItemCount) mod aItemCount;
end;

function NDISpanIndices(const aItemCount: NativeInt; const aIdx: INDSpanIndex;
  out aLo, aHi, aStep: NativeInt): Boolean;
begin
  aLo := aIdx.Low;
  aHi := aIdx.High;
  aStep := aIdx.Step;
  if aStep = 0 then exit(False);

  if aLo < 0 then aLo := (aLo + aItemCount) mod aItemCount;
  if aHi < 0 then aHi := (aHi + aItemCount) mod aItemCount;

  Result := InRange(aLo, 0, aItemCount - 1) and InRange(aHi, 0, aItemCount - 1) and
    ((aHi - aLo) * aStep > 0);
end;

procedure NDISplit(const aIdx: INDIndexSeq; out aSetIdx: INDIndexSeq; out aSpanIdx: INDIndexSeq;
  out aAxesPerm: TArray<Integer>);
var I, nDim, setTop, spanTop: Integer;
    idx: INDIndexSeq;
begin
  nDim := Length(aIdx);
  SetLength(idx, 2 * nDim);
  SetLength(aAxesPerm, 2 * nDim);
  setTop := 0;
  spanTop := nDim;
  for I := 0 to nDim - 1 do begin
    if aIdx[I].IndexType = nditSet then begin
      idx[setTop] := aIdx[I];
      aAxesPerm[setTop] := I;
      Inc(setTop);
    end else begin
      idx[spanTop] := aIdx[I];
      aAxesPerm[spanTop] := I;
      Inc(spanTop);
    end;
  end;
  SetLength(aSetIdx, setTop);
  for I := 0 to setTop - 1 do
    aSetIdx[I] := idx[I];
  Dec(spanTop, nDim);
  SetLength(aSpanIdx, spanTop);
  for I := 0 to spanTop - 1 do begin
    aSpanIdx[I] := idx[nDim + I];
    aAxesPerm[setTop + I] := aAxesPerm[nDim + I];
  end;
  SetLength(aAxesPerm, nDim);
end;

function NDIToStr(const aIndices: array of NativeInt): String;
var I: Integer;
begin
  if Length(aIndices) = 0 then exit('()');

  Result := Format('(%d', [aIndices[0]]);
  for I := 1 to High(aIndices) do
    Result := Format('%s, %d', [Result, aIndices[I]]);
  Result := Result + ')';
end;

{$ifdef FPC}
function NDIToStr(const aIndices: TArray<NativeInt>): String;
var I: Integer;
begin
  if Length(aIndices) = 0 then exit('()');

  Result := Format('(%d', [aIndices[0]]);
  for I := 1 to High(aIndices) do
    Result := Format('%s, %d', [Result, aIndices[I]]);
  Result := Result + ')';
end;
{$endif}

function NDICheckRange(const aIndices: INDIndexSeq; const aShape: TNDAShape): Boolean;
var I, J, N, d, lo, hi, s: NativeInt;
    idx: INDIndex;
    pIdx: PNativeInt;
    idxs: TArray<NativeInt>;
begin
  N := Length(aIndices);
  if N > Length(aShape) then exit(False);

  for I := 0 to N - 1 do begin
    d := aShape[I];
    idx := aIndices[I];
    pIdx := PNativeInt(idx.RawIndexData);
    case idx.IndexType of
      nditInt:
        if not InRange(pIdx^, 0, d - 1) then
          exit(False);

      nditSpan:
        if not NDISpanIndices(d, idx as INDSpanIndex, lo, hi, s) then
          exit(False);

      nditSet: begin
        idxs := (idx as INDSetIndex).Indices;
        d := aShape[I];
        for J := 0 to High(idxs) do
          if not InRange(idxs[J], 0, d - 1) then exit(False);
      end;
    else
      exit(False);
    end;
  end;

  Result := True;
end;

function NDIRange(aLo, aHi, aStep: NativeInt): SNDIRange;
begin
  Assert(aStep <> 0);
  Result.fLo := aLo;
  Result.fHi := aHi;
  Result.fStep := aStep;
end;

function ShapeToStr(const aArr: INDArray): String;
begin
  Result := NDIToStr(aArr.Shape);
end;

procedure pack1(aSrc, aDst: PByte; aCount, aStep: NativeInt);
var pEnd: PByte;
begin
  pEnd := aSrc + aCount * aStep;
  while aSrc < pEnd do begin
    aDst^ := aSrc^;
    Inc(aSrc, aStep);
    Inc(aDst);
  end;
end;

procedure pack2(aSrc, aDst: PByte; aCount, aStep: NativeInt);
var pEnd: PByte;
begin
  pEnd := aSrc + aCount * aStep;
  while aSrc < pEnd do begin
    PWord(aDst)^ := PWord(aSrc)^;
    Inc(aSrc, aStep);
    Inc(aDst, 2);
  end;
end;

procedure pack4(aSrc, aDst: PByte; aCount, aStep: NativeInt);
var pEnd: PByte;
begin
  pEnd := aSrc + aCount * aStep;
  while aSrc < pEnd do begin
    PInteger(aDst)^ := PInteger(aSrc)^;
    Inc(aSrc, aStep);
    Inc(aDst, 4);
  end;
end;

procedure pack8(aSrc, aDst: PByte; aCount, aStep: NativeInt);
var pEnd: PByte;
begin
  pEnd := aSrc + aCount * aStep;
  while aSrc < pEnd do begin
    PInt64(aDst)^ := PInt64(aSrc)^;
    Inc(aSrc, aStep);
    Inc(aDst, 8);
  end;
end;

procedure pack16(aSrc, aDst: PByte; aCount, aStep: NativeInt);
var pEnd: PByte;
begin
  pEnd := aSrc + aCount * aStep;
  while aSrc < pEnd do begin
    PCmplx128(aDst)^ := PCmplx128(aSrc)^;
    Inc(aSrc, aStep);
    Inc(aDst, 16);
  end;
end;

procedure packN(aSrc, aDst: PByte; aCount, aStep, aElSz: NativeInt);
var pEnd: PByte;
begin
  pEnd := aSrc + aCount * aStep;
  while aSrc < pEnd do begin
    Move(aSrc^, aDst^, aElSz);
    Inc(aSrc, aStep);
    Inc(aDst, aElSz);
  end;
end;

procedure PackBytes(aSrc, aDst: PByte; aCount, aStep, aElSz: NativeInt);
begin
  case aElSz of
    1: pack1(aSrc, aDst, aCount, aStep);
    2: pack2(aSrc, aDst, aCount, aStep);
    4: pack4(aSrc, aDst, aCount, aStep);
    8: pack8(aSrc, aDst, aCount, aStep);
    16: pack16(aSrc, aDst, aCount, aStep);
  else
    packN(aSrc, aDst, aCount, aStep, aElSz);
  end;
end;

function GetPackedDataPtr(const aArr: INDArray; var aBuff: TBytes): PByte;
var nDim, elSz, count, step: Integer;
    it: TNDAIt;
    p: PByte;
begin
  if CContiguousQ(aArr) then exit(aArr.Data);

  SetLength(aBuff, NBytes(aArr));
  Result := PByte(aBuff);
  elSz := aArr.ItemSize;
  nDim := aArr.NDim;  // nDim > 0
  count := aArr.Shape[nDim - 1];
  step := aArr.Strides[nDim - 1];
  if nDim = 1 then begin
    PackBytes(aArr.Data, PByte(aBuff), count, step, elSz);
    exit;
  end;

  it := TNDAIt.Create(aArr, 0, nDim - 2);
  try
    p := PByte(aBuff);
    while it.MoveNext do begin
      PackBytes(it.Current, p, count, step, elSz);
      Inc(p, count * elSz);
    end;
  finally
    it.Free;
  end;
end;

{$endregion}

{$region 'TNDIntIdx'}

constructor TNDIntIdx.Create(aValue: NativeInt);
begin
  fValue := aValue;
end;

function TNDIntIdx.IndexType: TNDIndexType;
begin
  Result := nditInt;
end;

function TNDIntIdx.RawIndexData: PByte;
begin
  Result := @fValue;
end;

function TNDIntIdx.GetValue: NativeInt;
begin
  Result := fValue;
end;

{$endregion}

{$region 'TNDSpanIdx'}

constructor TNDSpanIdx.Create(aLo, aHi, aStep: NativeInt);
begin
  fLo := aLo;
  fHi := aHi;
  fStep := aStep;
end;

function TNDSpanIdx.IndexType: TNDIndexType;
begin
  Result := nditSpan;
end;

function TNDSpanIdx.RawIndexData: PByte;
begin
  Result := @fLo;
end;

function TNDSpanIdx.GetLow: NativeInt;
begin
  Result := fLo;
end;

function TNDSpanIdx.GetHigh: NativeInt;
begin
  Result := fHi;
end;

function TNDSpanIdx.GetStep: NativeInt;
begin
  Result := fStep;
end;

{$endregion}

{$region 'TNDSetIdx'}

constructor TNDSetIdx.Create(const aIdxs: array of NativeInt);
begin
  Assert(Length(aIdxs) > 0);
  SetLength(fIdxs, Length(aIdxs));
  Move(aIdxs[0], fIdxs[0], Length(aIdxs) * cNISz);
end;

function TNDSetIdx.IndexType: TNDIndexType;
begin
  Result := nditSet;
end;

function TNDSetIdx.RawIndexData: PByte;
begin
  Result := PByte(fIdxs);
end;

function TNDSetIdx.GetItem(I: NativeInt): NativeInt;
begin
  Result := fIdxs[I];
end;

function TNDSetIdx.GetCount: NativeInt;
begin
  Result := Length(fIdxs);
end;

function TNDSetIdx.Indices: TArray<NativeInt>;
begin
  Result := fIdxs;
end;

{$endregion}

{$region 'SNDIntIndex'}

class operator SNDIntIndex.Implicit(aValue: NativeInt): SNDIntIndex;
begin
  Result.fIndex := aValue;
end;

class operator SNDIntIndex.Implicit(aValue: SNDIntIndex): NativeInt;
begin
  Result := aValue.fIndex;
end;

class operator SNDIntIndex.Implicit(aValue: SNDIntIndex): INDIndex;
begin
  Result := TNDIntIdx.Create(aValue.fIndex);
end;

class operator SNDIntIndex.Add(A: SNDIntIndex; B: NativeInt): SNDIntIndex;
begin
  Result.fIndex := A.fIndex + B;
end;

{$endregion}

{$region 'SNDIRange'}

function SNDIRange.GetEnumerator: TNDIEnum;
begin
  Result := TNDIEnum.Create(Self);
end;

{$endregion}

{$region 'TNDIEnum'}

constructor TNDIEnum.Create(const aRng: SNDIRange);
begin
  fRng := aRng;
  fIdx := fRng.Low - fRng.Step;
end;

function TNDIEnum.GetCurrent: NativeInt;
begin
  Result := fIdx;
end;

function TNDIEnum.MoveNext: Boolean;
begin
  with fRng do begin
    Result := (fIdx * Step < High * Step);
    if Result then Inc(fIdx, Step);
  end;
end;

{$endregion}

{$region 'TNDScalar<T>'}

constructor TNDScalar<T>.Create(const aValue: T);
begin
  fValue := aValue;
end;

function TNDScalar<T>.GetItemType: PTypeInfo;
begin
  Result := TypeInfo(T);
end;

function TNDScalar<T>.GetValue: T;
begin
  Result := fValue;
end;

procedure TNDScalar<T>.SetValue(const aValue: T);
begin
  fValue := aValue;
end;

function TNDScalar<T>.Data: PByte;
begin
  Result := @fValue;
end;

function TNDScalar<T>.NDim: Integer;
begin
  Result := 0;
end;

function TNDScalar<T>.Shape: TArray<NativeInt>;
begin
  Result := nil;
end;

function TNDScalar<T>.Strides: TArray<NativeInt>;
begin
  Result := nil;
end;

function TNDScalar<T>.ItemSize: Integer;
begin
  Result := SizeOf(T);
end;

function TNDScalar<T>.Size: NativeInt;
begin
  Result := 1;
end;

function TNDScalar<T>.Flags: Cardinal;
begin
  Result := NDAF_COW or NDAF_F_CONTIGUOUS;
end;

procedure TNDScalar<T>.SetFlags(aValue: Cardinal);
begin
  // not supported
end;

procedure TNDScalar<T>.UnsetFlags(aValue: Cardinal);
begin
  // not supported
end;

function TNDScalar<T>.GetPart(const aIdx: INDIndexSeq): INDArray<T>;
begin
  raise ENDAIndexError.Create(csInvIdxForScalar);
end;

procedure TNDScalar<T>.SetPart(const aIdx: INDIndexSeq; const aValue: INDArray<T>);
begin
  raise ENDAIndexError.Create(csInvIdxForScalar);
end;

function TNDScalar<T>.GetItem(const aIdx: array of NativeInt): T;
begin
  raise ENDAIndexError.Create(csInvIdxForScalar);
end;

procedure TNDScalar<T>.SetItem(const aIdx: array of NativeInt; const aValue: T);
begin
  raise ENDAIndexError.Create(csInvIdxForScalar);
end;

function TNDScalar<T>.Reshape(const aShape: array of NativeInt): INDArray<T>;
begin
  if Length(aShape) = 0 then exit(Self);
  if GetSize(aShape) <> 1 then
    raise ENDAPartError.CreateFmt(csInvArrayElem, [1, NDIToStr(aShape)]);

  Result := TNDABuffer<T>.Create(aShape);
  TNDA<T>.PT(Result.data)^ := Self.Value;
end;

{$endregion}

{$region 'TNDABase'}

function TNDABase.NDim: Integer;
begin
  Result := Length(fShape);
end;

function TNDABase.Shape: TArray<NativeInt>;
begin
  Result := fShape;
end;

function TNDABase.Strides: TArray<NativeInt>;
begin
  Result := fStrides;
end;

function TNDABase.Size: NativeInt;
begin
  Result := GetSize(fShape);
end;

function TNDABase.Flags: Cardinal;
begin
  Result := fFlags;
end;

procedure TNDABase.SetFlags(aValue: Cardinal);
begin
  fFlags := fFlags or aValue;
end;

procedure TNDABase.UnsetFlags(aValue: Cardinal);
begin
  fFlags := fFlags and (not aValue);
end;

{$endregion}

{$region 'TNDA'}

function TNDA.GetStrides(const aShape: TArray<NativeInt>): TArray<NativeInt>;
var I, count: Integer;
    step: NativeInt;
begin
  count := Length(aShape);
  SetLength(Result, count);
  step := ItemSize;
  Result[count - 1] := step;
  for I := count - 2 downto 0 do begin
    step := step * aShape[I + 1];
    Result[I] := step;
  end;
end;

function TNDA.GetOffset(const aIdx: array of NativeInt): NativeInt;
var I: Integer;
begin
  Result := 0;
  for I := 0 to High(fStrides) do
    Inc(Result, aIdx[I] * fStrides[I]);
end;

procedure TNDA.SetPart_SetIdx(const aIdx: INDIndexSeq; const aValue: INDArray);
var it: TNDASliceItChain;
    itv: TNDASliceIt;
    sh, shv, shs: TNDAShape;
    ff: TIPProcVV;
    setIdx, spanIdx: INDIndexSeq;
    axesPerm: TArray<Integer>;
    arr: INDArray;

  function GetPermutedArray(var aPerm: TArray<Integer>): INDArray;
  var I, count, nDim: Integer;
  begin
    if IdPermQ(aPerm) then exit(Self);

    nDim := Length(fShape);
    count := Length(aPerm);
    if count < NDim then begin
      SetLength(aPerm, nDim);
      for I := count to nDim - 1 do
        aPerm[I] := I;
    end;
    Result := PermuteAxes(aPerm);
  end;

begin
  NDISplit(aIdx, setIdx, spanIdx, axesPerm);
  arr := GetPermutedArray(axesPerm);
  sh := GetPartShape(arr, setIdx);
  shv := aValue.Shape;
  ff := GetFillRFunc();
  it := TNDASliceItChain.Create(arr, setIdx);
  try
    shs := it.CurrentSlice.Shape;
    if Length(spanIdx) = 0 then begin
      if SameQ(sh, shv) then begin
        itv := TNDASliceIt.Create(aValue, 0, Length(shs) - 1);
        try
          while it.MoveNext and itv.MoveNext do
           TNDAUt.MapR(it.CurrentSlice, itv.CurrentSlice, ff);
        finally
          itv.Free;
        end;
        exit;
      end;

      while it.MoveNext do begin
        TNDAut.MapR(it.CurrentSlice, aValue, ff);
      end;
    end else begin // Length(spanIdx) > 0
      while it.MoveNext do begin
        SetSlicePart(it.CurrentSlice, spanIdx, aValue);
      end;
    end;
  finally
    it.Free;
  end;
end;

procedure TNDA.InitPermuted(const aShape, aStrides: TArray<NativeInt>;
  const aAxesPerm: array of Integer);
var I, J, nDim: Integer;
begin
  nDim := Length(aAxesPerm);
  Assert(
    AxesPermQ(aAxesPerm) and
    (Length(aShape) = nDim) and (Length(aStrides) = nDim)
  );
  SetLength(fShape, nDim);
  SetLength(fStrides, nDim);
  for I := 0 to nDim - 1 do begin
    J := aAxesPerm[I];
    fShape[I] := aShape[J];
    fStrides[I] := aStrides[J];
  end;
end;

{$endregion}

{$region 'TNDA<T>'}

function TNDA<T>.GetItemType: PTypeInfo;
begin
  Result := TypeInfo(T);
end;

function TNDA<T>.ItemSize: Integer;
begin
  Result := SizeOf(T);
end;

function TNDA<T>.GetItemPart(const aIdx: array of INDIndex): T;
var idx, offset: NativeInt;
    I: Integer;
begin
  // integer index is expected!
  offset := 0;
  for I := 0 to High(aIdx) do begin
    idx := PNativeInt(aIdx[I].RawIndexData)^;
    Inc(offset, idx * Strides[I]);
  end;
  Result := PT(Data + offset)^;
end;

procedure TNDA<T>.SetItemPart(const aIdx: array of INDIndex; const aValue: T);
var idx, offset: NativeInt;
    I: Integer;
begin
  // integer index is expected!
  offset := 0;
  for I := 0 to High(aIdx) do begin
    idx := PNativeInt(aIdx[I].RawIndexData)^;
    Inc(offset, idx * Strides[I]);
  end;
  PT(Data + offset)^ := aValue;
end;

function TNDA<T>.GetItem(const aIdx: array of NativeInt): T;
begin
  Assert(Length(aIdx) = Length(fShape));
  Result := PT(Data + GetOffset(aIdx))^;
end;

procedure TNDA<T>.SetItem(const aIdx: array of NativeInt; const aValue: T);
begin
  if (fFlags and NDAF_WRITEABLE) = 0 then
    raise ENDAWriteError.CreateFmt('NDArray(%x) is not writeable.', [Data]);

  PT(Data + GetOffset(aIdx))^ := aValue;
end;

function TNDA<T>.GetPart(const aIdx: INDIndexSeq): INDArray<T>;
var its: TNDIndexTypes;
    it1: TNDASliceItChain;
    it2: TNDASliceIt;
    s: TNDAShape;
begin
  NDITypes(aIdx, its);
  if nditSet in its then begin
    s := GetPartShape(Self, aIdx);
    Result := TNDABuffer<T>.Create(s);
    it1 := TNDASliceItChain.Create(Self as INDArray, aIdx);
    it2 := TNDASliceIt.Create(Result, 0, Length(aIdx) - 1);
    try
      while it1.MoveNext do begin
        it2.MoveNext;
        TNDAUt.MapR(it2.CurrentSlice, it1.CurrentSlice, TNDAUt.FillR<T>);
      end;
    finally
      it1.Free;
      it2.Free;
    end;
  end else begin
    if (its = [nditInt]) and (Length(aIdx) = Length(fShape)) then
      Result := TNDScalar<T>.Create(GetItemPart(aIdx))
    else
      Result := TNDArrayView<T>.Create(Self as INDArray<T>, aIdx);
  end;
end;

procedure TNDA<T>.SetSlicePart(const aSlice: INDArray; const aIdx: INDIndexSeq; const aValue: INDArray);
var slice: INDArray<T>;
begin
  slice := TNDArray<T>.Create(aSlice.Data, aSlice.Shape, aSlice.Strides);
  slice[aIdx] := (aValue as INDArray<T>);
end;

function TNDA<T>.GetFillRFunc: TIPProcVV;
begin
  Result := TNDAUt.FillR<T>;
end;

function TNDA<T>.PermuteAxes(const aPerm: array of Integer): INDArray;
begin
  Result := TNDArrayWrapper<T>.CreatePermuted(Self, aPerm);
end;

procedure TNDA<T>.SetPart(const aIdx: INDIndexSeq; const aValue: INDArray<T>);
var view: INDArray<T>;
    its: TNDIndexTypes;
    it1, it2: TNDAIt;
    aLvl, vLvl: Integer;
    aSz, vSz: NativeInt;
    val: T;
begin
  NDITypes(aIdx, its);
  if nditSet in its then begin
    SetPart_SetIdx(aIdx, aValue);
    exit;
  end;

  if (its = [nditInt]) and (Length(aIdx) = Length(fShape)) then begin
    if not (TNDAUt.TryAsScalar<T>(aValue, val)) then
      raise ENDAPartError.Create(csInvArrayElem);
    SetItemPart(aIdx, val);
    exit;
  end;

  view := Self.GetPart(aIdx);
  if TNDAUt.TryAsScalar<T>(aValue, val) then begin
    TNDAUt.Fill<T>(view, val);
    exit;
  end;

  if not SameShapeQ(view, aValue) then
    raise ENDAPartError.CreateFmt(csInvBroadcastErr,
      [NDIToStr(view.Shape), NDIToStr(aValue.Shape)]
    );

  aLvl := GetCContLvl(aValue, aSz);
  vLvl := GetCContLvl(view, vSz);
  aLvl := Max(aLvl, vLvl);
  if aLvl = 0 then begin
    TNDAUt.Copy<T>(aValue.Data, view.Data, aSz div SizeOf(T));
    exit;
  end;
  aSz := Min(aSz, vSz);

  it1 := TNDAIt.Create(view, aLvl - 1);
  it2 := TNDAIt.Create(aValue, aLvl - 1);
  try
    if aSz = SizeOf(T) then begin
      while it1.MoveNext and it2.MoveNext do
        PT(it1.Current)^  := PT(it2.Current)^;
    end else begin
      aSz := aSz div SizeOf(T);
      while it1.MoveNext and it2.MoveNext do
        TNDAUt.Copy<T>(it2.Current, it1.Current, aSz);
    end;
  finally
    it1.Free;
    it2.Free;
  end;
end;

function TNDA<T>.Reshape(const aShape: array of NativeInt): INDArray<T>;
begin
  if (fFlags and NDAF_C_CONTIGUOUS) = 0 then begin
    Result := TNDABuffer<T>.Create(aShape);
    TNDAUt.Copy<T>(Self, Result.Data);
  end else
    Result := TNDArrayWrapper<T>.Create(Self, aShape);
end;

{$endregion}

{$region 'TNDArray<T>'}

constructor TNDArray<T>.Create(aData: Pointer; const aShape, aStrides: TArray<NativeInt>);
begin
  fData := PByte(aData);
  fShape := aShape;
  fStrides := aStrides;
  fFlags := 0;
  if ContiguousQ(SizeOf(T), fShape, fStrides) then
    fFlags := fFlags or NDAF_C_CONTIGUOUS;
end;

function TNDArray<T>.Data: PByte;
begin
  Result := fData;
end;

{$endregion}

{$region 'TNDPackedArray<T>'}

constructor TNDPackedArray<T>.Create(aData: Pointer; const aDims: array of NativeInt);
var count: Integer;
begin
  fData := PByte(aData);
  count := Length(aDims);
  SetLength(fShape, count);
  Move(aDims[0], fShape[0], count * cNISz);
  fStrides := GetStrides(fShape);
  fFlags := NDAF_C_CONTIGUOUS;
end;

function TNDPackedArray<T>.Data: PByte;
begin
  Result := fData;
end;

{$endregion}

{$region 'TNDABuffer<T>'}

constructor TNDABuffer<T>.Create(const aDims: array of NativeInt);
begin
  Create(aDims, 0);
end;

constructor TNDABuffer<T>.Create(const aDims: array of NativeInt; aAlignment: Integer);
var data: PByte;
begin
  data := InitBuffer(aDims, aAlignment);
  Create(data, aDims);
end;

constructor TNDABuffer<T>.Create(const aDims: array of NativeInt; const aValue: T;
  aAlignment: Integer);
var data: PByte;
begin
  data := InitBuffer(aDims, aAlignment);
  Create(data, aDims);
  TNDAUt.FillR<T>(Size, fData, SizeOf(T), @aValue, 0);
end;

function TNDABuffer<T>.InitBuffer(const aDims: array of NativeInt; aAlignment: Integer): PByte;
var sz: NativeInt;
begin
  sz := GetSize(aDims);
  GetMem(fBuff, sz * SizeOf(T));
  // todo: data alignment
  Result := fBuff;
end;

destructor TNDABuffer<T>.Destroy;
begin
  FreeMem(fBuff);
end;

procedure TNDABuffer<T>.AfterConstruction;
begin
  inherited;
  fFlags := fFlags or NDAF_OWNDATA or NDAF_WRITEABLE;
end;

{$endregion}

{$region 'TNDArrayWrapper<T>'}

constructor TNDArrayWrapper<T>.Create(const aArray: INDArray<T>);
begin
  fArray := aArray;
  fShape := fArray.Shape;
  fStrides := fArray.Strides;
  fFlags := fArray.Flags;
end;

constructor TNDArrayWrapper<T>.Create(const aArray: INDArray<T>; const aShape: array of NativeInt);
var nDim: Integer;
begin
  Assert(CContiguousQ(aArray) and (aArray.Size = GetSize(aShape)));
  fArray := aArray;
  nDim := Length(aShape);
  SetLength(fShape, nDim);
  Move(aShape[0], fShape[0], nDim * cNISz);
  fStrides := GetStrides(fShape);
  fFlags := fArray.Flags;
end;

constructor TNDArrayWrapper<T>.CreatePermuted(const aArray: INDArray<T>; const aAxesPerm: array of Integer);
begin
  fArray := aArray;
  InitPermuted(fArray.Shape, fArray.Strides, aAxesPerm);
  fFlags := fArray.Flags and (not NDAF_CONTIGUOUS);
  if ContiguousQ(ItemSize, fShape, fStrides) then
    fFlags := fFlags or NDAF_C_CONTIGUOUS;
end;

function TNDArrayWrapper<T>.Data: PByte;
begin
  Result := fArray.Data;
end;

{$endregion}

{$region 'TDynArrWrapper<T>'}

constructor TDynArrWrapper<T>.Create(const aArray: TArray<T>);
begin
  fArray := aArray;
  SetLength(fShape, 1);
  fShape[0] := Length(aArray);
  SetLength(fStrides, 1);
  fStrides[0] := SizeOf(T);
  fFlags := NDAF_COW or NDAF_F_CONTIGUOUS;
end;

constructor TDynArrWrapper<T>.Create(const aArray: TArray<T>; const aShape: array of NativeInt);
var nDim: Integer;
begin
  Assert(Length(aArray) = GetSize(aShape));
  fArray := aArray;
  nDim := Length(aShape);
  SetLength(fShape, nDim);
  Move(aShape[0], fShape[0], nDim * cNISz);
  fStrides := GetStrides(fShape);
  fFlags := NDAF_COW;
  if nDim = 1 then
    fFlags := NDAF_F_CONTIGUOUS;
end;

function TDynArrWrapper<T>.Data: PByte;
begin
  Result := PByte(fArray);
end;

{$endregion}

{$region 'TNDArrayView'}

constructor TNDArrayView.Create(const aArray: INDArray; const aIdx: INDIndexSeq;
  const aAxesPerm: TArray<Integer>);
begin
  fArray := aArray;
  GetViewProps(fArray, aIdx, fShape, fStrides, fOffset, fFlags, aAxesPerm);
end;

class procedure TNDArrayView.GetViewProps(const aArray: INDArray;
  const aIdx: INDIndexSeq; out aShape, aStrides: TArray<NativeInt>;
  out aOffset: NativeInt; out aFlags: Cardinal;
  const aAxesPerm: TArray<Integer>);
var I, J, dim, nDim: Integer;
    spanIdx: INDSpanIndex;
    arrShape, arrStrides: TArray<NativeInt>;
    bCCont, bSpan: Boolean;
begin
  aFlags := (aArray.Flags and NDAF_WRITEABLE);

  nDim := aArray.NDim;
  if Length(aIdx) > nDim then
    raise ENDAPartError.CreateFmt(csTooManyIndices, [nDim, Length(aIdx)]);

  arrShape := aArray.Shape;
  arrStrides := aArray.Strides;
  if Assigned(aAxesPerm) then begin
    arrShape := TNDAUt.Permute<NativeInt>(arrShape, aAxesPerm);
    arrStrides := TNDAUt.Permute<NativeInt>(arrStrides, aAxesPerm);
  end;
  bSpan := False;
  SetLength(aShape, nDim);
  SetLength(aStrides, nDim);
  dim := 0;
  aOffset := 0;
  J := Min(High(aIdx), nDim - 1);
  bCCont := (arrStrides[J] = aArray.ItemSize);
  for I := 0 to J do begin
    case aIdx[I].IndexType of
      nditSpan: begin
        spanIdx := (aIdx[I] as INDSpanIndex);
        aShape[dim] := NDISpanSize(arrShape[I], spanIdx);
        aStrides[dim] := arrStrides[I] * spanIdx.Step;
        Inc(aOffset, arrStrides[I] * NDISpanLow(arrShape[I], spanIdx));
        bCCont := bCCont and (spanIdx.Step = 1);
        bSpan := True;
        Inc(dim);
      end;

      nditInt: begin
        bCCont := bCCont and (not bSpan);
        Inc(aOffset, arrStrides[I] * (aIdx[I] as INDIntIndex).Value);
      end;
    else
      raise ENDAIndexError.Create('NDArrayView error: Set index is not allowed.');
    end;
  end;
  for I := J + 1 to nDim - 1 do begin
    aShape[dim] := arrShape[I];
    aStrides[dim] := arrStrides[I];
    Inc(dim);
  end;

  SetLength(aShape, dim);
  SetLength(aStrides, dim);

  if dim > 1 then
    bCCont := bCCont and (Abs(arrShape[nDim - 1]) = aShape[dim - 1]);
  if bCCont then
    aFlags := aFlags or NDAF_C_CONTIGUOUS;
end;

function TNDArrayView.GetItemType: PTypeInfo;
begin
  Result := fArray.GetItemType;
end;

function TNDArrayView.Data: PByte;
begin
  Result := fArray.Data + fOffset;
end;

function TNDArrayView.ItemSize: Integer;
begin
  Result := fArray.ItemSize;
end;

{$endregion}

{$region 'TNDArrayPart<T>'}

constructor TNDArrayView<T>.Create(const aArray: INDArray<T>; const aIdx: INDIndexSeq);
begin
  fArray := aArray;
  TNDArrayView.GetViewProps(aArray, aIdx, fShape, fStrides, fOffset, fFlags);
end;

function TNDArrayView<T>.Data: PByte;
begin
  Result := fArray.Data + fOffset;
end;

{$endregion}

{$region 'TVecView<T>'}

constructor TVecView<T>.Create(aData: PByte; aCount: NativeInt; aStride: NativeInt);
begin
  Init(aData, aCount, aStride);
end;

procedure TVecView<T>.Init(aData: PByte; aCount: NativeInt; aStride: NativeInt);
begin
  fData := aData;
  SetLength(fShape, 1);
  fShape[0] := aCount;
  SetLength(fStrides, 1);
  fStrides[0] := aStride;
  fFlags := 0;
  if SizeOf(T) = aStride then
    fFlags := fFlags or NDAF_CONTIGUOUS;
end;

function TVecView<T>.Data: PByte;
begin
  Result := fData;
end;

procedure TVecView<T>.SetData(aData: PByte);
begin
  fData := aData;
end;

function TVecView<T>.Stride: NativeInt;
begin
  Result := fStrides[0];
end;

function TVecView<T>.Length: NativeInt;
begin
  Result := fShape[0];
end;

{$endregion}

{$region 'TVecBuffer<T>'}

constructor TVecBuffer<T>.Create(aSize: NativeInt);
begin
  GetMem(fData, aSize * SizeOf(T));
  Init(fData, aSize, SizeOf(T));
end;

destructor TVecBuffer<T>.Destroy;
begin
  FreeMem(fData);
end;

{$endregion}

{$region 'TNDArrAxisView<T>'}

constructor TNDArrAxisView<T>.Create(const aArray: INDArray<T>; aAxis: Integer; aPos: TArray<NativeInt>);
var data: PByte;
    len: NativeInt;
begin
  fArray := aArray;
  if aAxis < 0 then
    aAxis := fArray.NDim + aAxis;
  data := fArray.Data;
  len := fArray.Shape[aAxis];
  if Assigned(aPos) then begin
    Inc(data, GetOffset(aPos));
    Dec(len, fArray.Shape[aAxis]);
  end;
  Init(data, len, fArray.Strides[aAxis]);
end;

{$endregion}

{$region 'TNDAIt'}

constructor TNDAIt.Create(const aArr: INDArray; const aIdxPerm: TArray<Integer>);
begin
  Create(aArr, 0, -1, aIdxPerm);
end;

constructor TNDAIt.Create(const aArr: INDArray; aHiLvl: Integer;
  const aIdxPerm: TArray<Integer>);
begin
  Create(aArr, 0, aHiLvl, aIdxPerm);
end;

constructor TNDAIt.Create(const aArr: INDArray; aLoLvl, aHiLvl: Integer;
  const aIdxPerm: TArray<Integer>);
var shape, strides: TArray<NativeInt>;
    I: Integer;
begin
  fArr := aArr;
  fData := fArr.Data;
  fLoLvl := aLoLvl;
  if fLoLvl < 0 then
    fLoLvl := fArr.NDim - fLoLvl;
  fHiLvl := aHiLvl;
  if fHiLvl < 0 then
    fHiLvl := fArr.NDim + fHiLvl;
  Assert(fHiLvl >= 0);
  shape := fArr.Shape;
  strides := fArr.Strides;
  SetLength(fSt, fHiLvl + 1);
  if Assigned(aIdxPerm) then begin
    Assert(Length(aIdxPerm) >= Length(fST));
    for I := 0 to fHiLvl do begin
      with fSt[I] do begin
        Offset := 0;
        Base := 0;
        Step := strides[aIdxPerm[I]];
        Last := Step * shape[aIdxPerm[I]];
      end;
    end;
  end else begin
    for I := 0 to fHiLvl do begin
      with fSt[I] do begin
        Offset := 0;
        Base := 0;
        Step := strides[I];
        Last := Step * shape[I];
      end;
    end;
  end;
  with fSt[fHiLvl] do
    Offset := -Step;
  fBaseIt := @fSt[fLoLvl];
  fTopIt := @fSt[fHiLvl];
  fElSz := aArr.ItemSize;
end;

function TNDAIt.MoveNext: Boolean;
var ppos: PNDAPos;
    b: NativeInt;
begin
  ppos := fTopIt;
  while PByte(ppos) >= fBaseIt do begin
    with ppos^ do begin
      Inc(Offset, Step);
      if Offset = Last then begin
        Dec(ppos);
        continue;
      end;
    end;

    while PByte(ppos) < PByte(fTopIt) do begin
      with ppos^ do
        b := Base + Offset;
      Inc(ppos);
      with ppos^ do begin
        Base := b;
        Offset := 0;
      end;
    end;

    with fTopIt^ do
      fCurrent := fData + Base + Offset;
    exit(True);
  end;

  Result := False;
end;

procedure TNDAIt.Reset;
var I: Integer;
begin
  for I := 0 to fHiLvl - 1 do begin
    with fSt[I] do begin
      Offset := 0;
      Base := 0;
    end;
  end;
  with fSt[fHiLvl] do begin
    Offset := -Step;
    Base := 0;
  end;
  fData := fArr.Data;
end;

procedure TNDAIt.ResetData;
begin
  fData := fArr.Data;
end;

{$region 'Getters/Setters'}

function TNDAIt.GetLowLvl: Integer;
begin
  Result := (fBaseIt - PByte(fSt)) div SizeOf(TNDAPos);
end;

function TNDAIt.GetHighLvl: Integer;
begin
  Result := Length(fSt);
end;

{$endregion}

{$endregion}

{$region 'TNDIdxIt'}

constructor TNDIdxIt.Create(const aShape: TArray<NativeInt>;
  const aRevAxis: TArray<Boolean>; const aIdxPerm: TArray<Integer>);
var dim: NativeInt;
    I, nDim: Integer;
begin
  Ndim := Length(aShape);
  SetLength(fSt, nDim);
  SetLength(fIdx, nDim);
  for I := 0 to nDim - 1 do begin
    with fSt[I] do begin
      dim := aShape[i];
      if Assigned(aRevAxis) and aRevAxis[I] then begin
        fIdx[I] := dim - 1;
        Base := dim - 1;
        Last := -1;
        Step := -1;
      end else begin
        fIdx[I] := 0;
        Base := 0;
        Last := dim;
        Step := 1;
      end;
    end;
  end;
  if Assigned(aIdxPerm) then begin
    for I := 0 to High(fSt) do
      fSt[I].Idx := @fIdx[aIdxPerm[I]];
  end else begin
    for I := 0 to High(fSt) do
      fSt[I].Idx := @fIdx[I];
  end;
  Dec(fSt[nDim - 1].Idx^, fSt[nDim - 1].Step);
  fBaseIt := PByte(fSt);
  fTopIt := @fSt[High(fSt)];
end;

function TNDIdxIt.MoveNext: Boolean;
var ppos: PNDIPos;
begin
  Result := False;
  ppos := fTopIt;
  while PByte(ppos) >= fBaseIt do begin
    with ppos^ do begin
      Inc(Idx^, Step);
      if Idx^ = Last then begin
        Dec(ppos);
        continue;
      end;
    end;

    while PByte(ppos) < PByte(fTopIt) do begin
      Inc(ppos);
      with ppos^ do
        Idx^ := Base;
    end;
    exit(True);
  end;
end;

{$endregion}

{$region 'TNDASliceIt'}

{$region 'TNDASliceIt.TSliceView'}

constructor TNDASliceIt.TSliceView.Create(const aArray: INDArray;
  const aAxes: TArray<Integer>; const aAxesPerm: TArray<Integer>);
var idx: INDIndexSeq;
    I: Integer;
begin
  idx := NDIIntSeq(aArray.NDim);
  for I := 0 to High(aAxes) do
    idx[aAxes[I]] := NDIAll();
  Create(aArray, idx, aAxesPerm);
  fData := fArray.Data;
end;

procedure TNDASliceIt.TSliceView.SetData(aData: PByte);
begin
  fData := aData;
end;

function TNDASliceIt.TSliceView.Data: PByte;
begin
  Result := fData;
end;

{$endregion}

constructor TNDASliceIt.Create(const aArr: INDArray; aLoLvl, aHiLvl: Integer;
  const aIdxPerm: TArray<Integer>);
var I, J, NDim: Integer;
    axes: TArray<Integer>;
begin
  inherited Create(aArr, aLoLvl, aHiLvl, aIdxPerm);

  if aHiLvl < 0 then
    aHiLvl := aArr.NDim + aHiLvl;
  NDim := aArr.NDim - (aHiLvl - aLoLvl + 1);
  SetLength(axes, NDim);
  for I := 0 to aLoLvl - 1 do
    axes[I] := I;
  J := aLoLvl;
  for I := aHiLvl + 1 to fArr.NDim - 1 do begin
    axes[J] := I;
    Inc(J);
  end;

  fSliceEntry := TSliceView.Create(aArr, axes, aIdxPerm);
  fSlice := fSliceEntry;
end;

function TNDASliceIt.CreateSliceView(const aArray: INDArray;
  const aAxes: TArray<Integer>): TSliceView;
begin
  Result := TSliceView.Create(aArray, aAxes);
end;

function TNDASliceIt.MoveNext: Boolean;
begin
  Result := inherited MoveNext;
  fSliceEntry.SetData(fCurrent);
end;

{$endregion}

{$region 'TNDASliceSetIt'}

constructor TNDASliceSetIt.Create(const aArr: INDArray; const aIdxs: TArray<NativeInt>);
begin
  inherited Create(aArr, 0, 0);
  fIdxs := aIdxs;
  fPos := -1;
  fStride := aArr.Strides[0];
end;

function TNDASliceSetIt.MoveNext: Boolean;
begin
  Inc(fPos);
  Result := (fPos < Length(fIdxs));
  if Result then begin
    fCurrent := fData + fIdxs[fPos] * fStride;
    fSliceEntry.SetData(fCurrent);
  end;
end;

procedure TNDASliceSetIt.Reset;
begin
  fPos := -1;
  fData := fArr.Data;
end;

{$endregion}

{$region 'TNDAConstSliceIt'}

constructor TNDAConstSliceIt.Create(const aSlice: INDArray);
begin
  fSlice := aSlice;
  fPos := -1;
end;

function TNDAConstSliceIt.MoveNext: Boolean;
begin
  Inc(fPos);
  Result := (fPos = 0);
end;

procedure TNDAConstSliceIt.Reset;
begin
  fPos := -1;
end;

{$endregion}

{$region 'TNDASliceItChain'}

constructor TNDASliceItChain.Create(const aArr: INDArray; const aIdx: INDIndexSeq);
var I, J, K, top: Integer;
    idx: INDIndexSeq;
    it: TNDASliceIt;
    arr: INDArray;
begin
  SetLength(fIts, Length(aIdx));
  top := 0;
  J := 0;
  for I := 0 to High(aIdx) do
    if aIdx[I].IndexType = nditSet then begin
      J := I;
      break;
    end;

  if J > 0 then begin
    SetLength(idx, J);
    for I := 0 to J - 1 do
      idx[I] := aIdx[I];
    fData := TNDArrayView.Create(aArr, idx);
    if aIdx[J - 1].IndexType = nditSpan then begin
      it := TNDASliceIt.Create(fData, 0, J - 1);
      arr := it.CurrentSlice;
      fIts[top] := it;
      Inc(top);
    end else
      arr := fData;
  end else begin
    fData := aArr;
    arr := aArr;
  end;

  for I := J to High(aIdx) do begin
    if aIdx[I].IndexType = nditSet then begin
      if I > J then begin
        SetLength(idx, I - J);
        for K := J to I - 1 do
          idx[K - J] := aIdx[K];
        arr := TNDArrayView.Create(arr, idx);
        it := TNDASliceIt.Create(arr, 0, Length(idx) - 1);
        arr := it.CurrentSlice;
        fIts[top] := it;
        Inc(top);
      end;

      it := TNDASliceSetIt.Create(arr, (aIdx[I] as INDSetIndex).Indices);
      arr := it.CurrentSlice;
      fIts[top] := it;
      Inc(top);
      J := I + 1;
      continue;
    end;
  end;

  if Length(aIdx) > J then begin
    SetLength(idx, Length(aIdx) - J);
    for K := J to High(aIdx) do
      idx[K - J] := aIdx[K];
    arr := TNDArrayView.Create(arr, idx);
    if aIdx[High(aIdx)].IndexType = nditSpan then
      it := TNDASliceIt.Create(arr, 0, Length(idx) - 1)
    else
      it := TNDAConstSliceIt.Create(arr);
    arr := it.CurrentSlice;
    fIts[top] := it;
    Inc(top);
  end;

  SetLength(fIts, top);
  if top > 0 then begin
    for I := 0 to top - 2 do
      fIts[I].MoveNext; // ? raise exception if not
    fSlice := fIts[top - 1].CurrentSlice;
  end else
    fSlice := fData;
end;

destructor TNDASliceItChain.Destroy;
var I: Integer;
begin
  for I := 0 to High(fIts) do
    fIts[I].Free;
  inherited;
end;

function TNDASliceItChain.MoveNext: Boolean;
var I, hi: Integer;
begin
  hi := High(fIts);
  if fIts[hi].MoveNext then exit(True);

  I := hi - 1;
  while I >= 0 do begin
    if fIts[I].MoveNext then begin
      for I := I + 1 to hi do begin
        fIts[I].Reset;
        if not fIts[I].MoveNext then exit(False);
      end;
      exit(True);
    end;
    Dec(I);
  end;

  Result := False;
end;

procedure TNDASliceItChain.Reset;
var I: Integer;
begin
  for I := 0 to High(fIts) do
    fIts[I].Reset;
end;

{$endregion}

{$region 'TNDAVecItems<T>'}

class operator TNDAVecItems<T>.Implicit(const aArr: INDArray<T>): TNDAVecItems<T>;
begin
  if not WriteableQ(aArr) then
    raise ENDAWriteError.Create(csNotWriteable);

  with Result do begin
    if aArr.NDim <> 1 then
      raise ENDACastError.CreateFmt(csInvCastToVec, [NDIToStr(aArr.Shape)]);
    fArr := aArr;
    fData := aArr.Data;
    fStride := aArr.Strides[0];
    fLength := aArr.Shape[0];
  end;
end;

class operator TNDAVecItems<T>.Implicit(const aVec: TNDAVecItems<T>): INDArray<T>;
begin
  Result := aVec.fArr;
end;

procedure TNDAVecItems<T>.Init(N: NativeInt);
begin
  Assert(N > 0);
  Self := TNDAUt.Empty<T>([N]);
end;

procedure TNDAVecItems<T>.Init(const aItems: TArray<T>);
begin
  Assert(System.Length(aItems) > 0);
  Self := TDynArrWrapper<T>.Create(aItems);
end;

function TNDAVecItems<T>.GetItem(aIdx: NativeInt): T;
begin
  Assert(aIdx < fLength);
  Result := TNDA<T>.PT(fData + fStride * aIdx)^
end;

procedure TNDAVecItems<T>.SetItem(aIdx: NativeInt; const aValue: T);
begin
  Assert(aIdx < fLength);
  TNDA<T>.PT(fData + fStride * aIdx)^ := aValue;
end;

{$endregion}

{$region 'TNDAMatItems<T>'}

class operator TNDAMatItems<T>.Implicit(const aArr: INDArray<T>): TNDAMatItems<T>;
var s: TArray<NativeInt>;
begin
  if not WriteableQ(aArr) then
    raise ENDAWriteError.Create(csNotWriteable);

  with Result do begin
    if aArr.NDim <> 2 then
      raise ENDACastError.CreateFmt(csInvCastToVec, [NDIToStr(aArr.Shape)]);
    fArr := aArr;
    fData := aArr.Data;
    s := aArr.Strides;
    fStrides[0] := s[0];
    fStrides[1] := s[1];
    s := aArr.Shape;
    fRowCount := s[0];
    fColCount := s[1];
  end;
end;

class operator TNDAMatItems<T>.Implicit(const aMat: TNDAMatItems<T>): INDArray<T>;
begin
  Result := aMat.fArr;
end;

procedure TNDAMatItems<T>.Init(aRowCount, aColCount: NativeInt);
begin
  Assert((aRowCount > 0) and (aColCount > 0));
  Self := TNDAUt.Empty<T>([aRowCount, aColCount]);
end;

function TNDAMatItems<T>.GetItem(I, J: NativeInt): T;
begin
  Assert((I < fRowCount) and (J < fColCount));
  Result := TNDA<T>.PT(fData + fStrides[0] * I + fStrides[1] * J)^
end;

procedure TNDAMatItems<T>.SetItem(I, J: NativeInt; const aValue: T);
begin
  Assert((I < fRowCount) and (J < fColCount));
  TNDA<T>.PT(fData + fStrides[0] * I + fStrides[1] * J)^ := aValue;
end;

{$endregion}

{$region 'TNDABuffFactory<T>'}

function TNDABuffFactory<T>.CreateNew(const aShape: array of NativeInt): INDArray;
begin
  Result := TNDABuffer<T>.Create(aShape);
end;

{$endregion}

{$region 'TNDAUt'}

class constructor TNDAUt.Create;
begin
  fCvtFuncs := TDictionary<TPair<PTypeInfo, PTypeInfo>, TIPProcVV>.Create;

  AddCvtFunc<Byte,    Single>(cvt_UI8F32);
  AddCvtFunc<Integer, Single>(cvt_I32F32);
  AddCvtFunc<Double,  Single>(cvt_F64F32);

  AddCvtFunc<Single,    Byte>(cvt_F32UI8);
  AddCvtFunc<Single,  Double>(cvt_F32F64);

  fConstrs := TObjectDictionary<PTypeInfo, TNDAFactory>.Create([doOwnsValues]);

  fConstrs.Add(TypeInfo(Integer),  TNDABuffFactory<Integer>.Create);
  fConstrs.Add(TypeInfo(Single),   TNDABuffFactory<Single>.Create);
  fConstrs.Add(TypeInfo(Double),   TNDABuffFactory<Double>.Create);
end;

class destructor TNDAUt.Destroy;
begin
  FreeAndNil(fCvtFuncs);
  FreeAndNil(fConstrs);
end;

class function TNDAUt.TryGetCvtFunc<T, U>(out aCvtFunc: TIPProcVV): Boolean;
begin
  Result := TryGetCvtFunc(TypeInfo(T), TypeInfo(U), aCvtFunc);
end;

class function TNDAUt.TryGetCvtFunc(T, U: PTypeInfo; out aCvtFunc: TIPProcVV): Boolean;
var p: TPair<PTypeInfo, PTypeInfo>;
begin
  p.Create(T, U);
  Result := fCvtFuncs.TryGetValue(p, aCvtFunc);
end;

class procedure TNDAUt.AddArrayFactory(aT: PTypeInfo; aFactory: TNDAFactory);
begin
  if not Assigned(aFactory) then begin
    fConstrs.Remove(aT);
    exit;
  end;
  fConstrs.AddOrSetValue(aT, aFactory);
end;

class procedure TNDAUt.AddCvtFunc<T, U>(aFunc: TIPProcVV);
var p: TPair<PTypeInfo, PTypeInfo>;
begin
  p.Create(TypeInfo(T), TypeInfo(U));
  fCvtFuncs.AddOrSetValue(p, aFunc);
end;

class procedure TNDAUt.FillR<T>(N: NativeInt; L: PByte; IncL: NativeInt;
  R: PByte; IncR: NativeInt);
var pEnd: PByte;
    s: T;
type PT = ^T;
begin
  pEnd := L + N * IncL;
  if IncR = 0 then begin
    s := TNDA<T>.PT(R)^;
    while L < pEnd do begin
      PT(L)^ := s;
      Inc(L, IncL);
    end;
  end else begin
    while L < pEnd do begin
      PT(L)^ := PT(R)^;
      Inc(L, IncL);
      Inc(R, IncR);
    end;
  end;
end;

class procedure TNDAUt.Copy<T>(aSrc, aDst: PByte; aCount: NativeInt);
var pEnd: PByte;
begin
  if IsManagedType(T) then begin
    pEnd := aSrc + aCount * SizeOf(T);
    while aSrc < pEnd do begin
      TNDA<T>.PT(aDst)^  := TNDA<T>.PT(aSrc)^;
      Inc(aSrc, SizeOf(T));
      Inc(aDst, SizeOf(T));
    end;
  end else
    Move(aSrc^, aDst^, aCount * SizeOf(T));
end;

class procedure TNDAUt.Copy<T>(aSrc, aDst: PByte; aCount, aStep: NativeInt);
var pEnd: PByte;
begin
  pEnd := aSrc + aCount * aStep;
  while aSrc < pEnd do begin
    TNDA<T>.PT(aDst)^ := TNDA<T>.PT(aSrc)^;
    Inc(aSrc, aStep);
    Inc(aDst, SizeOf(T));
  end;
end;

class function TNDAUt.Empty(aT: PTypeInfo; aDims: array of NativeInt): INDArray;
var fact: TNDAFactory;
begin
  if fConstrs.TryGetValue(aT, fact) then
    Result := fact.CreateNew(aDims);
end;

class function TNDAUt.Empty<T>(aDims: array of NativeInt): INDArray<T>;
begin
  if Length(aDims) > 0 then
    Result := TNDABuffer<T>.Create(aDims)
  else
    Result := TNDScalar<T>.Create;
end;

class function TNDAUt.Full<T>(const aDims: array of NativeInt; const aValue: T): INDArray<T>;
begin
  if Length(aDims) > 0 then
    Result := TNDABuffer<T>.Create(aDims, aValue, -1)
  else
    Result := TNDScalar<T>.Create(aValue);
end;

class function TNDAUt.Full<T>(const aDims: array of NativeInt; const aValue: INDArray<T>): INDArray<T>;
begin
  Result := TNDABuffer<T>.Create(aDims);
  Fill<T>(Result, aValue);
end;

class function TNDAUt.Identity<T>(aN: Integer): INDArray<T>;
var ti: PTypeInfo;
begin
  Result := Empty<T>([aN, aN]);
  Fill<T>(Result, Default(T));
  ti := TypeInfo(T);
  case ti^.Kind of
    tkInteger:
      case GetTypeData(ti)^.OrdType of
        otSByte, otUByte: FillDiagonal<Byte>(Result as INDArray<Byte>, 1);
        otSWord, otUWord: FillDiagonal<Word>(Result as INDArray<Word>, 1);
        otSLong, otULong: FillDiagonal<Integer>(Result as INDArray<Integer>, 1);
      end;

    tkFloat:
      case GetTypeData(ti)^.FloatType of
        ftSingle: FillDiagonal<Single>(Result as INDArray<Single>, 1);
        ftDouble: FillDiagonal<Double>(Result as INDArray<Double>, 1);
        ftExtended: FillDiagonal<Extended>(Result as INDArray<Extended>, 1);
      end;

    tkInt64:
      FillDiagonal<Int64>(Result as INDArray<Int64>, 1);
  else
    raise ENotImplemented.CreateFmt('TNDAUt.Identity<%s> is not implemented.', [ti^.Name]);
  end;
end;

class procedure TNDAUt.FillDiagonal<T>(const aArr: INDArray<T>; const aValue: T);
var s, idx: TNDAShape;
    mi: NativeInt;
    I, J, dim: Integer;
begin
  s := aArr.Shape;
  dim := Length(s);
  Assert(dim > 1);
  mi := s[0];
  for I := 1 to dim - 1 do
    if s[I] < mi then mi := s[I];

  SetLength(idx, Length(s));
  for I := 0 to mi - 1 do begin
    for J := 0 to dim - 1 do
      idx[J] := I;
    aArr.SetItem(idx, aValue);
  end;
end;

class function TNDAUt.AsArray<T>(const aItems: array of T; const aDims: array of NativeInt): INDArray<T>;
var sz: NativeInt;
begin
  sz := GetSize(aDims);
  Assert((sz > 0) and (Length(aItems) = sz));
  Result := TNDABuffer<T>.Create(aDims);
  Copy<T>(PByte(@aItems[0]), Result.Data, sz);
end;

class function TNDAUt.AsArray<T>(const aItems: array of T): INDArray<T>;
begin
  Assert(Length(aItems) > 0);
  Result := TNDABuffer<T>.Create([Length(aItems)]);
  Copy<T>(PByte(@aItems[0]), Result.Data, Length(aItems));
end;

class function TNDAUt.AsArray<T>(const aItems: TOArray2D<T>): INDArray<T>;
var I, w, h, ws: NativeInt;
    pRes: PByte;
begin
  h := Length(aItems);
  Assert(h > 0);
  w := Length(aItems[0]);
  Result := TNDABuffer<T>.Create([h, w]);
  pRes := Result.Data;
  ws := w * SizeOf(T);
  for I := 0 to h - 1 do begin
    Assert(Length(aItems[I]) = w);
    Copy<T>(PByte(@aItems[I, 0]), pRes, w);
    Inc(pRes, ws);
  end;
end;

class function TNDAUt.Scalar<T>(const aValue: T): INDScalar<T>;
begin
  Result := TNDScalar<T>.Create(aValue);
end;

class function TNDAUt.TryAsScalar<T>(const aArr: INDArray; out aValue: T): Boolean;
begin
  Result := SameQ(aArr.GetItemType, TypeInfo(T)) and (aArr.Shape = nil);
  if Result then aValue := TNDA<T>.PT(aArr.Data)^;
end;

class function TNDAUt.TryAsDynArray<T>(const aArr: INDArray; out aValue: TArray<T>): Boolean;
var I, count, step: NativeInt;
    shape: TArray<NativeInt>;
    p: PByte;
begin
  if not SameQ(aArr.GetItemType, TypeInfo(T)) then exit(False);

  shape := aArr.Shape;
  if (Length(shape) <> 1) or (shape[0] <= 0) then exit(False);

  count := shape[0];
  SetLength(aValue, count);
  if CContiguousQ(aArr) then begin
    Copy<T>(aArr.Data, PByte(aValue), count);
  end else begin
    p := aArr.Data;
    step := aArr.Strides[0];
    for I := 0 to count - 1 do begin
      aValue[I] := TNDA<T>.PT(p)^;
      Inc(p, step);
    end;
  end;
  Result := True;
end;

class function TNDAUt.TryAsDynArray2D<T>(const aArr: INDArray; out aValue: TArray<TArray<T>>): Boolean;
var I, J, w, h, ws, stepRow, stepCol: NativeInt;
    shape: TArray<NativeInt>;
    row: TArray<T>;
    pRow, p: PByte;
begin
  if not SameQ(aArr.GetItemType, TypeInfo(T)) then exit(False);

  shape := aArr.Shape;
  if (Length(shape) <> 2) or (shape[0] <= 0) or (shape[1] <= 0)  then exit(False);

  h := shape[0];
  w := shape[1];
  SetLength(aValue, h, w);
  p := aArr.Data;
  if CContiguousQ(aArr) then begin
    ws := w * SizeOf(T);
    for I := 0 to h - 1 do begin
      Copy<T>(p, PByte(aValue[I]), w);
      Inc(p, ws);
    end;
  end else begin
    stepRow := aArr.Strides[0];
    stepCol := aArr.Strides[1];
    pRow := p;
    for I := 0 to h - 1 do begin
      row := aValue[I];
      p := pRow;
      for J := 0 to w - 1 do begin
        row[J] := TNDA<T>.PT(p)^;
        Inc(p, stepCol);
      end;
      Inc(pRow, stepRow);
    end;
  end;
  Result := True;
end;

class function TNDAUt.TryAsArray<T>(const aArr: INDArray; out aValue: INDArray<T>): Boolean;
begin
  Result := SameQ(aArr.GetItemType, TypeInfo(T)) and Supports(aArr, INDArray<T>, aValue);
end;

class procedure TNDAUt.Fill<T>(const aArr: INDArray<T>; const aValue: T);
begin
  MapR(aArr, @aValue, FillR<T>);
end;

class procedure TNDAUt.Fill<T>(const aArr: INDArray<T>; const aValue: INDArray<T>);
begin
  if aValue.NDim > aArr.NDim then
    MapR(aArr, aValue[NDIIntSeq(aValue.NDim - aArr.NDim)], FillR<T>)
  else
    MapR(aArr, aValue, FillR<T>);
end;

class procedure TNDAUt.MapR(const L: INDArray; pR: PByte; aRFnc: TIPProcVV);
var cLvl: Integer;
    sz, count, lSz: NativeInt;
    it: TNDAIt;
begin
  if not WriteableQ(L) then
    raise ENDAWriteError.Create(csNotWriteable);

  lSz := L.ItemSize;
  if CContiguousQ(L) then begin
    aRFnc(L.Size, L.Data, lSz, pR, 0);
    exit;
  end;

  cLvl := GetCContLvl(L, sz);
  if sz = lSz then begin
    Dec(cLvl);
    sz := L.Strides[cLvl];
    if cLvl = 0 then begin
      aRFnc(L.Size, L.Data, sz, pR, 0);
      exit;
    end;

    count := L.Shape[cLvl];
    it := TNDAIt.Create(L, cLvl - 1);
    try
      while it.MoveNext do
        aRFnc(count, it.Current, sz, pR, 0);
    finally
      it.Free;
    end;
  end else begin
    count := sz div lSz;
    it := TNDAIt.Create(L, cLvl - 1);
    try
      while it.MoveNext do
        aRFnc(count, it.Current, lSz, pR, 0);
    finally
      it.Free;
    end;
  end;
end;

class procedure TNDAUt.MapL(pL: PByte; const R: INDArray; aLFnc :TIPProcVV);
var cLvl: Integer;
    sz, count, rSz: NativeInt;
    it: TNDAIt;
begin
  if not WriteableQ(R) then
    raise ENDAWriteError.Create(csNotWriteable);

  rSz := R.ItemSize;
  if CContiguousQ(R) then begin
    aLFnc(R.Size, pL, 0, R.Data, rSz);
    exit;
  end;

  cLvl := GetCContLvl(R, sz);
  if sz = rSz then begin
    Dec(cLvl);
    sz := R.Strides[cLvl];
    if cLvl = 0 then begin
      aLFnc(R.Size, pL, 0, R.Data, sz);
      exit;
    end;

    count := R.Shape[cLvl];
    it := TNDAIt.Create(R, cLvl - 1);
    try
      while it.MoveNext do
        aLFnc(count, pL, 0, it.Current, sz);
    finally
      it.Free;
    end;
  end else begin
    count := sz div rSz;
    it := TNDAIt.Create(R, cLvl - 1);
    try
      while it.MoveNext do
        aLFnc(count, pL, 0, it.Current, rSz);
    finally
      it.Free;
    end;
  end;
end;

class procedure TNDAUt.MapSS(const L, R: INDArray; aFnc: TIPProcVV);
var cLvl: Integer;
    cSz: NativeInt;
begin
  if not SameShapeQ(L, R) then
    raise ENDAMapError.CreateFmt(csBroadcastErr,
      [ShapeToStr(L), ShapeToStr(R)]);

  if (L.Flags and R.Flags and NDAF_C_CONTIGUOUS) <> 0 then begin
    aFnc(L.Size, L.Data, L.ItemSize, R.Data, R.ItemSize);
    exit;
  end;

  cLvl := GetCommonCContLvl(L, R, cSz);
  MapSS(L, R, aFnc, cLvl, cSz);
end;

class procedure TNDAUt.MapSS(const L, R: INDArray; aFnc: TIPProcVV; aCLvl: Integer; aCSz: NativeInt);
var itL, itR: TNDAIt;
    szL, szR, cnt: NativeInt;
begin
  if aCLvl = 0 then begin
    aFnc(L.Size, L.Data, L.ItemSize, R.Data, R.ItemSize);
    exit;
  end;

  if aCLvl = L.NDim then begin // there are no continuous blocks -> loop over vectors
    Dec(aCLvl);
    aCSz := L.Strides[aCLvl];
    szR := R.Strides[aCLvl];
    if aCLvl = 0 then begin // 1D case
      aFnc(L.Size, L.Data, aCSz, R.Data, szR);
      exit;
    end;

    itL := TNDAIt.Create(L, 0, aCLvl - 1);
    itR := TNDAIt.Create(R, 0, aCLvl - 1);
    try
      cnt := L.Shape[aCLvl];
      while itL.MoveNext do begin
        itR.MoveNext;
        aFnc(cnt, itL.Current, aCSz, itR.Current, szR);
      end;
    finally
      itL.Free;
      itR.Free;
    end;
  end else begin
    itL := TNDAIt.Create(L, 0, aCLvl - 1);
    itR := TNDAIt.Create(R, 0, aCLvl - 1);
    try
      szL := L.ItemSize;
      szR := R.ItemSize;
      cnt := aCSz div szL;
      while itL.MoveNext do begin
        itR.MoveNext;
        aFnc(cnt, itL.Current, szL, itR.Current, szR);
      end;
    finally
      itL.Free;
      itR.Free;
    end;
  end;
end;

class procedure TNDAUt.MapR(const L, R: INDArray; aRFnc: TIPProcVV);
var it: TNDAIt;
    dimL, dimR, lvl: Integer;
    sz: NativeInt;
    view: INDArray;
begin
  dimL := L.NDim;
  dimR := R.NDim;

  if dimR = 0 then begin
    MapR(L, R.Data, aRFnc);
    exit;
  end;

  if dimL = dimR then begin
    if not WriteableQ(L) then
      raise ENDAWriteError.Create(csNotWriteable);

    MapSS(L, R, aRFnc);
    exit;
  end;

  if dimL > dimR then begin
    lvl := BroadcastLvl(L, R);
    if lvl < 0 then
      raise ENDAMapError.CreateFmt(csBroadcastErr,
        [ShapeToStr(L), ShapeToStr(R)]);

    it := TNDASliceIt.Create(L, 0, lvl - 1);
    try
      view := TNDASliceIt(it).CurrentSlice;
      lvl := GetCommonCContLvl(view, R, sz);
      while it.MoveNext do
        MapSS(view, R, aRFnc, lvl, sz);
    finally
      it.Free;
    end;
    exit;
  end;

  // dimL < dimR
  raise ENDAMapError.CreateFmt(csBroadcastToErr,
    [ShapeToStr(L), ShapeToStr(R)]
  );
end;

class procedure TNDAUt.MapL(const L, R: INDArray; aLFnc: TIPProcVV);
var it: TNDAIt;
    dimL, dimR, lvl: Integer;
    sz: NativeInt;
    view: INDArray;
begin
  dimL := L.NDim;
  dimR := R.NDim;

  if dimL = 0 then begin
    MapL(L.Data, R, aLFnc);
    exit;
  end;

  if dimL = dimR then begin
    if not WriteableQ(R) then
      raise ENDAWriteError.Create(csNotWriteable);

    MapSS(L, R, aLFnc);
    exit;
  end;

  if dimR > dimL then begin
    lvl := BroadcastLvl(R, L);
    if lvl < 0 then
      raise ENDAMapError.CreateFmt(csBroadcastErr,
        [ShapeToStr(L), ShapeToStr(R)]);

    it := TNDASliceIt.Create(R, 0, lvl - 1);
    try
      view := TNDASliceIt(it).CurrentSlice;
      lvl := GetCommonCContLvl(L, view, sz);
      while it.MoveNext do
        MapSS(L, view, aLFnc, lvl, sz);
    finally
      it.Free;
    end;
    exit;
  end;

  // dimL < dimR
  raise ENDAMapError.CreateFmt(csBroadcastToErr,
    [ShapeToStr(L), ShapeToStr(R)]
  );
end;

class procedure TNDAUt.MapR<T>(const L: INDArray<T>; const R: T; aRFnc: TIPProcVV);
begin
  MapR(L, @R, aRFnc);
end;

class procedure TNDAUt.MapL<T>(const L: T; const R: INDArray<T>; aLFnc :TIPProcVV);
begin
  MapL(@L, R, aLFnc);
end;

class procedure TNDAUt.Map<T, U, V>(const L: INDArray<T>; const R: U;
  var aRes: INDArray<V>; aRFnc: TIPProcVV);
begin
  if not Assigned(aRes) then aRes := AsType<V>(L, True);
  MapR(aRes, @R, aRFnc);
end;

class procedure TNDAUt.Map<T, U, V>(const L: T; const R: INDArray<U>;
  var aRes: INDArray<V>; aLFnc: TIPProcVV);
begin
  if not Assigned(aRes) then aRes := AsType<V>(R, True);
  MapL(@L, aRes, aLFnc);
end;

class procedure TNDAUt.Map<T, U, V>(const L: INDArray<T>; const R: INDArray<U>;
  var aRes: INDArray<V>; aLFnc, aRFnc: TIPProcVV);
begin
  if L.NDim >= R.NDim then begin
    if not Assigned(aRes) then aRes := AsType<V>(L, True);
    MapR(aRes, R, aRFnc);
  end else begin
    if not Assigned(aRes) then aRes := AsType<V>(R, True);
    MapL(L, aRes, aLFnc);
  end;
end;

class procedure TNDAUt.Map<T>(const L: INDArray<T>; const R: T; var aRes: INDArray<T>; aRFnc: TIPProcVV);
begin
  Map<T, T, T>(L, R, aRes, aRFnc);
end;

class procedure TNDAUt.Map<T>(const L: T; const R: INDArray<T>; var aRes: INDArray<T>; aLFnc: TIPProcVV);
begin
  Map<T, T, T>(L, R, aRes, aLFnc);
end;

class procedure TNDAUt.Map<T>(const L: INDArray<T>; const R: INDArray<T>;
  var aRes: INDArray<T>; aLFnc, aRFnc: TIPProcVV);
begin
  Map<T, T, T>(L, R, aRes, aLFnc, aRFnc);
end;

class procedure TNDAUt.Copy<T>(const aArr: INDArray; aDst: PByte);
var sz, count: NativeInt;
    cLvl: Integer;
    it: TNDAIt;
begin
  if CContiguousQ(aArr) then begin
    Copy<T>(aArr.Data, aDst, aArr.Size);
    exit;
  end;

  cLvl := GetCContLvl(aArr, sz);
  it := TNDAIt.Create(aArr, cLvl - 1);
  try
    if sz = SizeOf(T) then begin
      while it.MoveNext do begin
        TNDA<T>.PT(aDst)^ := TNDA<T>.PT(it.Current)^;
        Inc(aDst, SizeOf(T));
      end;
    end else begin
      count := sz div SizeOf(T);
      while it.MoveNext do begin
        Copy<T>(it.Current, aDst, count);
        Inc(aDst, sz);
      end;
    end;
  finally
    it.Free;
  end;
end;

class function TNDAUt.Copy<T>(const aArr: INDArray<T>): INDArray<T>;
var shape: TArray<NativeInt>;
begin
  shape := aArr.Shape;
  if Length(shape) = 0 then begin
    Result := TNDScalar<T>.Create(TNDA<T>.PT(aArr.Data)^);
  end else begin
    Result := TNDABuffer<T>.Create(shape);
    Copy<T>(aArr, Result.Data);
  end;
end;

class function TNDAUt.TryAsType<T>(const aArr: INDArray; out aRes: INDArray<T>;
  aForceCopy: Boolean): Boolean;
var cvtFunc: TIPProcVV;
begin
  if SameQ(aArr.GetItemType, TypeInfo(T))  then begin
    aRes := (aArr as INDArray<T>);
    if aForceCopy then
      aRes := Copy<T>(aRes);
    exit(True);
  end;

  if TryGetCvtFunc(aArr.GetItemType, TypeInfo(T), cvtFunc) then begin
    aRes := Empty<T>(aArr.Shape);
    MapSS(aArr, aRes, cvtFunc);
    exit(True);
  end;

  Result := False;
end;

class function TNDAUt.AsType<T>(const aArr: INDArray; aForceCopy: Boolean): INDArray<T>;
begin
  if not TryAsType<T>(aArr, Result, aForceCopy) then
    raise ENotImplemented.CreateFmt('TNDAUt.AsType<%s>() is not implemented yet.',
      [GetTypeName(TypeInfo(T))]
    );
end;

class function TNDAUt.AsType<T>(const aArrays: array of INDArray;
  aForceCopy: Boolean): TArray<INDArray<T>>;
var I, count: Integer;
begin
  count := Length(aArrays);
  SetLength(Result, count);
  for I := 0 to count - 1 do
    if not TryAsType<T>(aArrays[I], Result[I], aForceCopy) then
      raise ENDACastError.Create('Invalid cast');
end;

class function TNDAUt.AsContiguousArray<T>(const aArr: INDArray<T>): INDArray<T>;
begin
  if CContiguousQ(aArr) then
    Result := aArr
  else
    Result := Copy<T>(aArr);
end;

class function TNDAUt.Permute<T>(const aData: TArray<T>; const aIndices: array of Integer): TArray<T>;
var I, count: NativeInt;
begin
  count := Length(aData);
  SetLength(Result, count);
  for I := 0 to High(Result) do
    Result[I] := aData[aIndices[I]];
end;

{$endregion}

end.
