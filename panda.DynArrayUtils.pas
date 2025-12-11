unit panda.DynArrayUtils;

interface

{$ifndef FPC}
  {$include DelphiVers.inc}
{$endif}

uses
    Math
  , SysUtils
  , TypInfo
  , Generics.Defaults
  , Generics.Collections
{$ifndef fPC}
  , System.RTTI
  , System.Classes
{$endif}
  , panda.Consts
  ;

type
  TMergeSort<T> = record
  private type
    PT = ^T;
  private
    fCmp: IComparer<T>;
    procedure Merge(pL, pR, pRes: PT; aLCnt, aRCnt: Integer);
  public
    constructor Create(const aComparer: IComparer<T>);
    function Sort(const aData: TArray<T>): TArray<T>;
  end;

  TDoubleArray = TArray<Double>;
  TDoubleArray2D = TArray<TDoubleArray>;

  TNativeIntArray = TArray<NativeInt>;
  TNativeIntArray2D = TArray<TNativeIntArray>;

  TNativeIntArrayLexComparer = class(TComparer<TNativeIntArray>)
  public
    function Compare({$ifdef FPC}constref{$else}const{$endif} Left, Right: TNativeIntArray): Integer; override;
  end;

  /// <summary>
  ///   Iterates over all possible permutations of the elements in <c>aSet</c>.
  /// </summary>
  /// <remarks>
  ///  <para>Heap's algorithm is used. </para>
  ///  <para>
  ///   Don't change <c>Current</c> array. This array is read only and it
  ///   is updated in the each iteration step.
  ///  </para>
  /// </remarks>
  TPermutationIterator<T> = class
  protected type
    TGenProc = function: Boolean of object;
  protected
    fIdx, fCount: NativeInt;
    fCurrent: TArray<T>;
    fStack: TArray<NativeInt>;
    fGenProc: TGenProc;
    class procedure Swap(var A, B: T);
    function GenerateNext: Boolean;
    function GenerateFirst: Boolean;
  public
    constructor Create(const aSet: TArray<T>);
    function MoveNext: Boolean;
    function Current: TArray<T>;

    property ItemCount: NativeInt read fCount;
  end;

  TSubsetIterator<T> = class
  protected type
    TGenProc = function: Boolean of object;
  protected
    fCount, fSubsetSize: NativeInt;
    fSet: TArray<T>;
    fCurrent: TArray<T>;
    fIdxs: TArray<NativeInt>;
    fGenProc: TGenProc;
    function GenerateFirst: Boolean;
    function GenerateNext: Boolean;
  public
    constructor Create(const aSet: TArray<T>; aSubsetSize: NativeInt);
    function MoveNext: Boolean;
    function Current: TArray<T>;
    function IncSubsetSize: Boolean;

    property SubsetSize: NativeInt read fSubsetSize;
    property ItemCount: NativeInt read fCount;
    property Items: TArray<T> read fSet;
  end;

{$ifdef FPC}
  TPredicateFunc<T> = function (const aArg: T): Boolean of object;

  TFnc<T, TResult> = function (const aValue: T): TResult of object;
  TFnc<T1, T2, TResult> = function (const aArg1: T1; const aArg2: T2): TResult of object;
{$else}
  TPredicateFunc<T> = reference to function (const aArg: T): Boolean;

  TFnc<T, TResult> = reference to function (const aValue: T): TResult;
  TFnc<T1, T2, TResult> = reference to function (const aArg1: T1; const aArg2: T2): TResult;
{$endif}

  TArrayIndex2D = record
    Row, Col: Integer;
  end;

  TIndexedValue<U> = record
    Value: U;
    Idx: NativeInt;
    procedure Init(const I: NativeInt; const aValue: U);
  end;

  TIndexedValueComparer<U> = class(TComparer<TIndexedValue<U>>)
  protected
    fComparer: IComparer<U>;
  public
    constructor Create(const aComparer: IComparer<U>);
    function Compare({$ifdef FPC}constref{$else}const{$endif} aLeft, aRight: TIndexedValue<U>): Integer; override;
	end;

  TIndexedValueList<U> = class(TList<TIndexedValue<U>>)
  end;

{$ifdef FPC}
  TIndices = TArray<NativeInt>;
{$else}
  TIndices = array of NativeInt;
{$endif}

  TTransformation<T, U> = class abstract
    function Execute(const aValue: T): U; virtual; abstract;
	end;

  TIdentity<T> = class(TTransformation<T, T>)
    function Execute(const aValue: T): T; override;
	end;

  TDynAUt = record
  private
    class function IsManaged<T>: Boolean; static;
    class procedure CheckArrays(Source, Destination: Pointer;
      SourceIndex, SourceLength, DestIndex, DestLength, Count: NativeInt); static;
  public
  class function Concat<T>(const A, B: TArray<T>): TArray<T>; overload; static;
{$ifndef FPC}
  class function Concat<T>(const aArrays: array of TArray<T>): TArray<T>; overload; static;
{$endif}
  class function Clone<T>(const A: TArray<T>): TArray<T>; overload; static;
{$ifndef FPC}
  class function Clone<T>(const A: TArray<TArray<T>>): TArray<TArray<T>>; overload; static;
  class function Clone<T>(const A: TArray<TArray<TArray<T>>>): TArray<TArray<TArray<T>>>; overload; static;
{$endif}
  /// <remarks>replaces the faulty function <c>TArray.Copy</c></remarks>
  class procedure Copy<T>(const Source, Destination: array of T; SourceIndex, DestIndex, Count: NativeInt); static;
  /// <summary>
  ///   Finds index <c>I</c> in a data array such that <c>data[I]</c> is the greatest smaller value than
  ///   <c>Value</c>. This method uses binary search algorithm so it's supposed
  ///   that the input data are successively sorted.
  /// </summary>
  /// <param name="Value"> value which is found in the data array</param>
  /// <param name="StartPos"> searching will beginning from this index</param>
  /// <returns> <c>True</c> if value was found</returns>
  /// <remarks> Example:
  ///   <code>
  ///     <para>data := TArray&lt;Double&gt;.Create(0, 1, 2, 3, 4, 5);</para>
  ///     <para>TSearchUtils.SearchPos&lt;Double&gt;(data, TComparer&lt;Double&gt;.Default, 4.55, out);</para>
  ///   </code>
  /// </remarks>
  class function SearchPos<T>(const data: array of T; const Value: T; out Pos: NativeInt;
    Comparer: IComparer<T> = nil; StartPos: NativeInt = 0; EndPos: NativeInt = -1): Boolean; overload; static;
  class function SearchPos(const data: array of Double; const Value: Double; out Pos: NativeInt;
    StartPos: NativeInt = 0; EndPos: NativeInt = -1) : boolean; overload; static;
  class function BinarySearch(const Values: array of Double; const Item: Double;
    out FoundIndex: Integer; Index, Count: Integer): Boolean; overload; static;
  class function BinarySearch(const Values: array of Double; const Item: Double;
    out FoundIndex: Integer): Boolean; overload; static;
  class procedure QuickSort(var Values: array of Double; L, R: Integer); static;
  /// <summary>
  ///   Finds index <c>I</c> in a data array such that <c>data[I]</c> is the greathest smaller value than
  ///   <c>Value</c>. This method goes through data item by item until required index is reached.
  /// </summary>
  /// <remarks>
  ///   <para>
  ///     This method can be better than <c>SearchPos</c> when I can go through close to all elements.
  ///   </para>
  /// </remarks>
  class function SearchPosSuccessively<T>(const data: array of T; const Comparer: IComparer<T>; const Value: T;
    out Pos: Integer; StartPos: Integer = 0; EndPos: Integer = -1) : boolean; static;
  /// <summary>
  ///   Serializes 2D array into the 1D array.
  /// </summary>
  class function Flatten<T>(const data: TArray<TArray<T>>): TArray<T>; static;
  /// <summary>
  ///    Partitions one dimensional array into nonoverlapping lists of arrays of length <c>aRowSize</c>.
  ///  </summary>
  /// <param name="Arr"> Original one-dimensional array </param>
  /// <param name="RowSize"> Number of elements in the row (it's equal to columns count)</param>
  /// <returns> two-dimensional array </returns>
  /// <remarks> This function doesn't work with managed types (<c>Move</c> is used)</remarks>
  class function Partition<T>(const Arr: TArray<T>; aRowSize: Integer; aOffset: Integer = 0): TArray<TArray<T>>; static;
  /// <summary>
  ///   Reverses the order of the elements in array.
  /// </summary>
  class function Reverse<T>(const Arr: TArray<T>): TArray<T>; static;
  class procedure ReverseInPlace<T>(var Arr: TArray<T>); static;
  class function Transpose<T>(const OriginalArray: TArray<TArray<T>>): TArray<TArray<T>>; static;
  class function Table<T>(const aFunc: TFnc<Double, T>; aX0, aX1, aXStep: Double): TArray<T>; overload; static;
  class function Table<T>(const aFunc: TFnc<Double, Double, T>;
    aX0, aX1, aXStep, aY0, aY1, aYStep: Double): TArray<TArray<T>>; overload; static;
  class function ElementsCount<T>(const aData: TArray<TArray<T>>): Integer; static;
  /// <summary>
  ///   Copy contens of original Array to the new array
  /// </summary>
  /// <param name = "data"> Orginal Array</param>
  /// <param name = "IndexFrom1/IndexFrom2"> Index of Original Array, where will begin copy Array </param>
  /// <param name = "Length1/Length2"> Length of copy Array </param>
  /// <remarks> If Length of copy Array is bigger then Original, other </remarks>
  /// <remarks> Don't use Original Array with zero length. </remarks>
  /// <returns> New Array </returns>
  class function SubArray<T>(const data: TArray<TArray<T>>;
      const IndexFrom1, IndexFrom2, Length1, Length2: Integer): TArray<TArray<T>>; overload; static;
  class function SubArray<T>(const aData: TArray<T>; aFrom, aTo: Integer): TArray<T>; overload; static;
  /// <summary>
  ///  Cycles the elements in array to the right.
  /// </summary>
  /// <param name="aData"> Array, which will be rotated. </param>
  /// <param name="RotateBy"> Number of how much be rotated. </param>
  /// <remarks> Don't use for managed types (<c>Move</c> function is used). </remarks>
  /// <remarks> Don't use negative <c>RotateBy</c> and use function RotateLeft. </remarks>
  /// <returns> Rotated Array. </returns>
  class function RotateRight<T>(const aData: TArray<T>; RotateBy: Integer): TArray<T>; static;
  /// <summary>
  ///   Cycles the elements in array to the left.
  /// </summary>
  /// <param name="aData"> Array which will be rotated. </param>
  /// <param name="RotateBy"> Number of how much be rotated. </param>
  /// <remarks> Don't use for managed types (<c>Move</c> function is used). </remarks>
  /// <remarks> Don't use negative <c>RotateBy</c> and use function <c>RotateRight</c>. </remarks>
  /// <returns> Rotated Array. </returns>
  class function RotateLeft<T>(const aData: TArray<T>; RotateBy: Integer): TArray<T>; static;
  class procedure MergeSort<T>(aData: TArray<T>; aComparer: IComparer<T> = nil); static;
  /// <summary>
  ///   Creates heap on the <c>aData</c> input.
  /// </summary>
  /// <param name="aData">
  ///   Input data which will be reordered into the heap structure (algorithm works in place!!)
  /// </param>
  /// <param name="aOrdering">determines wheather maximum or minimum will be on the top</param>
  /// <param name="aComparer">
  ///   Users comparer. Note that the default comparers (for basic types) are much faster.
  ///   You can obtain them with <c>TComparer&lt;T&gt;.Default</c> method
  /// </param>
  class procedure MakeHeap<T>(aData: TArray<T>; aComparer: IComparer<T> = nil;
    aCount: NativeInt = -1; aAscendingOrder: Boolean = True); static;
  /// <summary>
  ///   Sorts the elements in Heap.
  /// </summary>
  /// <param name="aData">
  ///    Input data. It's suposed that it is heap (heap can be created by <c>BuildHeap</c> method).
  /// </param>
  /// <param name="aOrderingType">
  ///   Determines an ordering of the heap. Ordering can be <c>otAscending</c> or
  ///   <c>otDescending</c>.
  /// </param>
  /// <param name="aComparer">comparer defined by user</param>
  class procedure HeapSort<T>(aData: TArray<T>; aComparer: IComparer<T> = nil;
    aAscendingOrder: Boolean = True); static;
  /// <summary>
  ///   Remove top (i.e. the smallest or the biggest) item from the heap
  ///   (heap structure will be preserved).
  /// </summary>
  /// <param name="aData">
  ///    Input data. It's suposed that it is heap (heap can be created by <c>BuildHeap</c> method).
  /// </param>
  /// <param name="aCount">
  ///    Number of the items in the heap. It can be different from the length of the input data.
  /// </param>
  /// <param name="aOrderingType">
  ///   Determines an ordering of the heap. Ordering can be <c>otAscending</c> or
  ///   <c>otDescending</c>.
  /// </param>
  /// <param name="aComparer">comparer defined by user</param>
  class function HeapPop<T>(aData: TArray<T>; aComparer: IComparer<T> = nil;
    aCount: NativeInt = -1; aAscendingOrder: Boolean = True): T; static;
  /// <summary> Adds item to the heap</summary>
  /// <param name="aData">
  ///    Input data. It's suposed that it is heap (heap can be created by <c>BuildHeap</c> method).
  /// </param>
  /// <param name="aCount">
  ///    Number of the items in the heap. It has to be less than length of the input data
  ///    (otherwise there is not free space for new item).
  /// </param>
  /// <param name="aValue">
  ///   new item which should be added into the heap.
  /// </param>
  /// <param name="aOrderingType">
  ///   Determines an ordering of the heap. Ordering can be <c>otAscending</c> or
  ///   <c>otDescending</c>. Implicitly is suposed that heap ordering is ascending.
  /// </param>
  /// <param name="aComparer">comparer defined by user</param>
  class procedure HeapPush<T>(var aData: TArray<T>; const aValue: T; aComparer: IComparer<T> = nil;
    aCount: NativeInt = -1; aAscendingOrder: Boolean = True); static;
  /// <summary>
  ///   Removes specified item from the heap.
  /// </summary>
  /// <remarks>
  ///   This method has <c>O(N)</c> complexity.
  /// </remarks>
  class function HeapRemove<T>(aData: TArray<T>; const aValue: T; aComparer: IComparer<T> = nil;
    aCount: NativeInt = -1; aAscendingOrder: Boolean = True): Boolean; static;
{$ifndef FPC}
  /// <summary>
  ///   Creates string representation of the <c>arr</c> as Mathematica list
  /// </summary>
  class function ToMListString<T>(const arr: TArray<T>): String; overload; static;
  class function ToMListString<T>(const arr: TArray<T>; const aFmt: TFormatSettings): String; overload; static;
  class function ToMListString<T>(const arr: TArray<TArray<T>>): String; overload; static;
  class function ToMListString<T>(const arr: TArray<TArray<T>>; const aFmt: TFormatSettings): String; overload; static;
  class procedure ExportMListString<T>(const aFileName: String; const aArr: TArray<T>); overload; static;
  class procedure ExportMListString<T>(const aFileName: String; const aArr: TArray<TArray<T>>); overload; static;
{$endif}
  /// <summary>
  ///   <para><c>Take(array, indices: array of Integer)</c> takes items form <c>array</c> that are
  ///     contained in <c>indices</c>.</para>
  ///   <para><c>Take(array, N)</c> gives the first <c>N</c> elements of the <c>array</c>.</para>
  ///   <para><c>Take(array, -N)</c> gives the last <c>N</c> elements of the <c>array</c>.</para>
  /// </summary>
  class function Take<T>(const aData: TArray<T>; const aIndices: TIndices): TArray<T>; overload; static;
  class function Take<T>(const aData: TArray<T>; N: NativeInt): TArray<T>; overload; static;
  /// <summary>
  ///   <para><c>Drop(array, indices: array of Integer)</c> drops items from <c>array</c> that are
  ///   contained in <c>indices</c>.</para>
  ///   <para><c>Drop(array, N)</c> gives <c>array</c> with its first <c>N</c> element dropped.</para>
  ///   <para><c>Drop(array, -N)</c> gives <c>array</c> with its last <c>N</c> elements dropped.</para>
  /// </summary>
  class function Drop<T>(const aData: TArray<T>; const idxs: TIndices): TArray<T>; overload; static;
  class function Drop<T>(const aData: TArray<T>; N: NativeInt): TArray<T>; overload; static;
  /// <summary>
  ///   <param><c>Insert(list, elem, n)</c> inserts <c>elem</c> at position <c>n</c> in <c>list</c></param>.
  /// </summary>
  class function Insert<T>(const aData: TArray<T>; const aItem: T; aIdx: Integer): TArray<T>; overload; static;
  class function InsertList<T>(const aData: TArray<T>; const aItems: array of T; const aIdxs: array of Integer): TArray<T>; overload; static;
  class function Select<T>(const aData: TArray<T>; const aPredicate: TPredicateFunc<T>): TArray<T>; static;
  /// <summary>
  ///   Gives a list of the positions at which objects satisfy condition <c>aPredicate</c>.
  ///  </summary>
  class function Position<T>(const aData: TArray<T>; const aPredicate: TPredicateFunc<T>): TArray<Integer>; static;
  /// <summary>
  ///   Gives the position of the first element in <c>aData</c> that is equal to <c>aItem</c>. If no such element is found
  ///   then function returns -1.
  /// </summary>
  class function FirstPosition<T>(const aData: TArray<T>; const aItem: T; aComparer: IEqualityComparer<T> = nil): Integer; static;
  /// <summary>
  ///   Return a permutation vector <c>I</c> that puts <c>v[I]</c> in sorted order.
  /// </summary>
  /// <remarks>
  ///   The permutation si quaranteed to be stable even if the sorting alogrith is unstable, meaning that
  ///   inidices of equal elements appear in ascending order.
  /// </remarks>
  class function SortPerm<T>(const aData: TArray<T>; aComparer: IComparer<T> = nil): TArray<Integer>; overload; static;
{$ifndef FPC}
  class function SortPerm<T>(const aData: TArray<T>; const aComparer: TComparison<T>): TArray<Integer>; overload; static;
{$endif}
  class procedure SortIndexedValues<T>(var aValues: TArray<TIndexedValue<T>>; aComparer: IComparer<T> = nil); static;
  class procedure SortIndexedValuesByIndex<T>(var aValues: TArray<TIndexedValue<T>>); static;
  /// <summary>
  ///   Maps a function <c>aFnc</c> on each element from input array <c>aData</c>.
  ///   At i-th position in output array is stored result by function <c>aFnc</c> which
  ///   was called with i-th element from input array <c>aData</c> as parameter.
  /// </summary>
  class function Map<TIn, TOut>(const aData: TArray<TIn>; const aFnc: TFnc<TIn, TOut>): TArray<TOut>; overload; static;
  class function Map2D<Tin, TOut>(const aData: TArray<TArray<TIn>>; const aFnc: TFnc<TIn, TOut>): TArray<TArray<TOut>>; overload; static;
  class function Map<T>(const aData: TArray<T>; const aFnc: TFnc<T, T>): TArray<T>; overload; static;
  class function Map2D<T>(const aData: TArray<TArray<T>>; const aFnc: TFnc<T, T>): TArray<TArray<T>>; overload; static;
  class function MapIndexed<TIn, TOut>(const aDAta: TArray<TIn>;
    const aFnc: TFnc<TIn, Integer, TOut>): TArray<TOut>; static;
  /// <sumamry>
  ///   Gives array <c>aData</c> with element <c>aValue</c> appended.
  /// </summary>
  class function Append<T>(const aData: TArray<T>; const aValue: T): TArray<T>; static;
  class function Prepend<T>(const aData: TArray<T>; const aValue: T): TArray<T>; static;
  class function AppendList<T>(const aData: TArray<T>; const aValues: array of T): TArray<T>; static;
  class function PrependList<T>(const aData: TArray<T>; const aValues: array of T): TArray<T>; static;
  /// <summary>
  ///   Determines whether array <c>aData</c> contains specified value.
  /// </summary>
  class function MemberQ<T>(const aData: TArray<T>; const aValue: T;
    const aComparer: IComparer<T> = nil): Boolean; static;
  class function FindMin<T>(const aData: TArray<T>; const aComparer: IComparer<T> = nil): T; static;
  class function FindMinPos<T>(const aData: TArray<T>; aComparer: IComparer<T> = nil): Integer; static;
  class function FindMax<T>(const aData: TArray<T>; const aComparer: IComparer<T> = nil): T; static;
  class function FindMaxPos<T>(const aData: TArray<T>; aComparer: IComparer<T> = nil): Integer; static;
  class procedure FindMinMax<T>(const aData: TArray<T>; var aMin, aMax: T; aComparer: IComparer<T> = nil); static;
  class function FindFirstPos<T>(const aData: TArray<T>; aPred: TPredicateFunc<T>): Integer; overload; static;
  class function FindFirstPos<T>(const aData: TArray<T>; const aValue: T; aComparer: IComparer<T> = nil): NativeInt; overload; static;
  class function Where<T>(const aData: TArray<T>; const aPredicate: TPredicateFunc<T>): TArray<Integer>; overload; static;
  class function Where<T>(const aData: TArray<TArray<T>>; const aPredicate: TPredicateFunc<T>): TArray<TArrayIndex2D>; overload; static;
  class function Median<T>(const aData: TArray<T>; aComparer: IComparer<T> = nil): T; static;
//{$ifndef FPC}
  /// <summary>
  ///  gathers into sublists each set of elements in list that gives the same value when <c>aFnc</c> is applied.
  /// </summary>
  class function GatherBy<T, U>(const aData: TArray<T>; const aFnc: TFnc<T, U>; aComparer: IComparer<U> = nil): TArray<TArray<T>>; overload; static;
  class function GatherBy<T>(const aData: TArray<T>; aComparer: IComparer<T> = nil): TArray<TArray<T>>; overload; static;
//{$endif}
  class function SplitBy<T>(const aData: TArray<T>; aComparer: IComparer<T> = nil): TArray<TArray<T>>; static;
  /// <summary>
  ///   Applies <c>aFnc</c> to size <c>aWin</c> windows in the specified <c>aData</c>.
  /// </summary>
  class function MovingMap<T, U>(const aData: TArray<T>; const aFnc: TFnc<TArray<T>, U>; aWin: Integer): TArray<U>; static;
  class function Riffle<T>(const A, B: TArray<T>): TArray<T>; static;
  class function ToArray<T>(const aData: array of T): TArray<T>; static;
  class function ConstantArray<T>(const aValue: T; aCount: Integer): TArray<T>; static;
  class function ItemCount<T>(const aArray: TArray<TArray<T>>): NativeInt; static;
  class function MaxLength<T>(const aArray: TArray<TArray<T>>): NativeInt; static;
  // predicates
  class function PositiveQ(const aArray: array of Integer): Boolean; static;

  // conversions
  class function Int2NInt(const aArray: array of Integer): TArray<NativeInt>; static;

  class function Range(const aLo, aHi: NativeInt; const aStep: NativeInt = 1): TArray<NativeInt>; overload; static;
  class function Range_I32(aLo, aHi: Integer; aStep: Integer = 1): TArray<Integer>; static;
  class function Range(const aLo, aHi: Double; const aStep: Double = 1): TArray<Double>; overload; static;
  class function Range_F32(const aLo, aHi: Single; const aStep: Single = 1): TArray<Single>; static;

{$ifndef FPC}
  // string array utils
  class function ToUpper(const aArray: array of String): TArray<String>; static;
  class function ToLower(const aArray: array of String): TArray<String>; static;
{$endif}
end;

{$ifndef FPC}

type
  TMStrGetter = function (const aValue: TValue; const aFmt: TFormatSettings): String;

function Real2MStr(const aValue: TValue; const aFmt: TFormatSettings): String;
function MStr(const aValue: TValue; const aFmt: TFormatSettings): String;

{$endif}

implementation

{$region 'TMergeSort<T>'}

constructor TMergeSort<T>.Create(const aComparer: IComparer<T>);
begin
  fCmp := aComparer;
  if not Assigned(fCmp) then
    fCmp := TComparer<T>.Default;
end;

procedure TMergeSort<T>.Merge(pL, pR, pRes: PT; aLCnt, aRCnt: Integer);
var pLEnd, pREnd: PByte;
begin
  pLEnd := PByte(pL) + aLCnt * SizeOf(T);
  pREnd := PByte(pR) + aRCnt * SizeOf(T);
  while (PByte(pL) < pLEnd) and (PByte(pR) < pREnd) do begin
    if fCmp.Compare(pL^, pR^) <= 0 then begin
      pRes^ := pL^;
      Inc(pL);
    end else begin
      pRes^ := pR^;
      Inc(pR);
    end;
    Inc(pRes);
  end;

  while PByte(pL) < pLEnd do begin
    pRes^ := pL^;
    Inc(pRes);
    Inc(pL);
  end;

  while PByte(pR) < pREnd do begin
    pRes^ := pR^;
    Inc(pRes);
    Inc(pR);
  end;
end;

function TMergeSort<T>.Sort(const aData: TArray<T>): TArray<T>;
var I, J, mid, aCount: Integer;
    l, r, lres, rres: TArray<T>;
begin
  aCount := Length(aData);
  if aCount <= 1 then
    exit(aData);

  mid := Round(aCount / 2);
  SetLength(l, mid);
  SetLength(r, aCount - mid);
  for I := 0 to mid - 1 do
    l[I] := aData[I];
  J := 0;
  for I := mid to aCount - 1 do begin
    r[J] := aData[I];
    Inc(J);
  end;

  lres := Sort(l);
  rres := Sort(r);
  SetLength(Result, aCount);
  merge(@lres[0], @rres[0], @Result[0], mid, aCount - mid);
end;

{$endregion}

{$region 'TNativeIntArrayLexComparer'}

function TNativeIntArrayLexComparer.Compare(
  {$ifdef FPC}constref{$else}const{$endif} Left, Right: TNativeIntArray): Integer;
var I, lenL, lenR, count: NativeInt;
    l, r: NativeInt;
begin
  Result := 0;
  lenL := Length(Left);
  lenR := Length(Right);
  if (lenL = 0) and (lenR = 0) then exit;

  count := Min(lenL, lenR);
  if count = 0 then begin
    if lenL = 0 then
      Result := -1
    else
      Result := 1;
    exit;
  end;

  for I := 0 to count - 1 do begin
    l := Left[I];
    r := Right[I];
    Result := l - r;
    if Result <> 0 then exit;
  end;

  if Result = 0 then
    Result := lenL - lenR;
end;

{$endregion}

{$region 'TPermutationIterator<T>'}

constructor TPermutationIterator<T>.Create(const aSet: TArray<T>);
begin
  fCurrent := aSet;
  fCount := Length(aSet);
  SetLength(fStack, fCount);
  fGenProc := GenerateFirst;
  fIdx := -1;
end;

class procedure TPermutationIterator<T>.Swap(var A, B: T);
var tmp: T;
begin
  tmp := A;
  A := B;
  B := tmp;
end;

function TPermutationIterator<T>.MoveNext: Boolean;
begin
  Result := fGenProc();
end;

function TPermutationIterator<T>.GenerateNext: Boolean;
begin
  if fIdx = fCount then exit(False);  

  if fStack[fIdx] < fIdx then begin
    if (fIdx and 1) = 0 then
      Swap(fCurrent[0], fCurrent[fIdx])
    else
      Swap(fCurrent[fStack[fIdx]], fCurrent[fIdx]);

    Inc(fStack[fIdx]);
    fIdx := 0;
    Result := True;
  end else begin
    fStack[fIdx] := 0;
    Inc(fIdx);
    Result := GenerateNext;
  end;
end;

function TPermutationIterator<T>.GenerateFirst: Boolean;
begin
  fIdx := 0;
  Result := (fIdx < fCount);
  fGenProc := GenerateNext;
end;

function TPermutationIterator<T>.Current: TArray<T>;
begin
  Result := fCurrent;
end;

{$endregion}

{$region 'TSubsetIterator<T>'}

constructor TSubsetIterator<T>.Create(const aSet: TArray<T>; aSubsetSize: NativeInt);
begin
  fSet := aSet;
  fCount := Length(fSet);
  Assert((0 <= aSubsetSize) and (aSubsetSize <= fCount));
  fSubsetSize := aSubsetSize;
  fGenProc := GenerateFirst;
end;

function TSubsetIterator<T>.IncSubsetSize: Boolean;
var size: NativeInt;
begin
  size := fSubsetSize + 1;
  if size > fCount then exit(False);
  fSubsetSize := size;
  fGenProc := GenerateFirst;
  Result := True;
end;

function TSubsetIterator<T>.GenerateFirst: Boolean;
var I, size: NativeInt;
begin
  size := fSubsetSize;
  if size = 0 then exit(False);

  SetLength(fIdxs, size);
  SetLength(fCurrent, size);
  for I := 0 to size - 1 do
    fIdxs[I] := I;

  fGenProc := GenerateNext;
  Result := True;
end;

function TSubsetIterator<T>.GenerateNext: Boolean;
var I, size, count: NativeInt;
begin
  count := fCount;
  size := fSubsetSize;
  I := size - 1;
  while (I >= 0) and (fIdxs[I] = (count - size + I)) do
    Dec(I);
  if I < 0 then exit(False);  

  Inc(fIdxs[I]);
  Inc(I);
  while I < size do begin
    fIdxs[I] := fIdxs[I - 1] + 1;
    Inc(I);
  end;

  Result := True;
end;

function TSubsetIterator<T>.MoveNext: Boolean;
begin
  Result := fGenProc();
end;

function TSubsetIterator<T>.Current: TArray<T>;
var I, size: NativeInt;
begin
  size := fSubsetSize;
  for I := 0 to size - 1 do
    fCurrent[I] := fSet[fIdxs[I]];
  Result := fCurrent;
end;

{$endregion}

{$region 'TIndexedValue<U>'}

procedure TIndexedValue<U>.Init(const I: NativeInt; const aValue: U);
begin
  Idx := I;
  Value := aValue;
end;

{$endregion}

{$region 'TIndexedValueComparer<U>'}

constructor TIndexedValueComparer<U>.Create(const aComparer: IComparer<U>);
begin
  fComparer := aComparer;
end;

function TIndexedValueComparer<U>.Compare(
  {$ifdef FPC}constref{$else}const{$endif} aLeft, aRight: TIndexedValue<U>): Integer;
begin
  Result := fComparer.Compare(aLeft.Value, aRight.Value);
end;

{$endregion}

{$region 'TIdentity<T>'}

function TIdentity<T>.Execute(const aValue: T): T;
begin
  Result := aValue;
end;

{$endregion}

{$region 'TArrayUtils'}

{$ifndef FPC}

{$ifndef DelphiXE7Up}

function IsManagedType(pType: PTypeInfo): Boolean;
var ctx: TRttiContext;
    t: TRttiType;
begin
  Result := False;
  if pType = nil then exit;
  ctx := TRttiContext.Create;
  try
    t := ctx.GetType(pType);
    Result := t.IsManaged;
  finally
    ctx.Free;
  end;
end;

{$endif}

function Real2MStr(const aValue: TValue; const aFmt: TFormatSettings): String;
begin
  Result := FloatToStr(aValue.AsExtended, aFmt);
end;

function MStr(const aValue: TValue; const aFmt: TFormatSettings): String;
begin
  TValue(aValue).ToString;
end;

{$endif}

class function TDynAUt.IsManaged<T>: Boolean;
begin
{$ifdef FPC}
  Result := IsManagedType(T);
{$else}
{$ifdef DelphiXE7Up}
  Result := System.IsManagedType(T);
{$else}
  Result := IsManagedType(TypeInfo(T));
{$endif}
{$endif}
end;

class function TDynAUt.Concat<T>(const A, B: TArray<T>): TArray<T>;
var I, l1, l2: Integer;
begin
  Result := nil;
  l1 := Length(A);
  l2 := Length(B);
  SetLength(Result, l1 + l2);
  for I := 0 to l1 - 1 do Result[I] := A[I];
  for I := 0 to l2 - 1 do begin
    Result[l1] := B[I];
    Inc(l1);
  end;
end;

{$ifndef FPC}

class function TDynAUt.Concat<T>(const aArrays: array of TArray<T>): TArray<T>;
var I, top, len: Integer;
begin
  len := 0;
  for I := 0 to High(aArrays) do
    Inc(len, Length(aArrays[I]));
  SetLength(Result, len);
  top := 0;
  for I := 0 to High(aArrays) do begin
    len := Length(aArrays[I]);
    Copy<T>(aArrays[I], Result, 0, top, len);
    Inc(top, len);
  end;
end;

{$endif}

class function TDynAUt.Clone<T>(const A: TArray<T>): TArray<T>;
begin
  Result := nil;
  SetLength(Result, Length(A));
  Copy<T>(A, Result, 0, 0, Length(A));
end;

{$ifndef FPC}

class function TDynAUt.Clone<T>(const A: TArray<TArray<T>>): TArray<TArray<T>>;
var I, len: Integer;
begin
  len := Length(A);
  SetLength(Result, len);
  for I := 0 to len - 1 do
    Result[I] := Clone<T>(A[I]);
end;

class function TDynAUt.Clone<T>(const A: TArray<TArray<TArray<T>>>): TArray<TArray<TArray<T>>>;
var I, len: Integer;
begin
  len := Length(A);
  SetLength(Result, len);
  for I := 0 to len - 1 do
    Result[I] := Clone<T>(A[I]);
end;

{$endif}

class procedure TDynAUt.CheckArrays(Source, Destination: Pointer;
  SourceIndex, SourceLength, DestIndex, DestLength, Count: NativeInt);
begin
  if (SourceIndex < 0) or (DestIndex < 0) or (SourceIndex >= SourceLength) or (DestIndex >= DestLength) or
     (SourceIndex + Count > SourceLength) or (DestIndex + Count > DestLength) then
    raise EArgumentOutOfRangeException.CreateRes(@csArgumentOutOfRange);
  if Source = Destination then
    raise EArgumentException.CreateRes(@csSameArrays);
end;

class procedure TDynAUt.Copy<T>(const Source, Destination: array of T; SourceIndex, DestIndex, Count: NativeInt);
begin
  if Count <= 0 then exit;

  CheckArrays(Pointer(@Source[0]), Pointer(@Destination[0]), SourceIndex, Length(Source), DestIndex, Length(Destination), Count);

  if IsManaged<T> then
    System.CopyArray(Pointer(@Destination[DestIndex]), Pointer(@Source[SourceIndex]), TypeInfo(T), Count)
  else
    System.Move(Pointer(@Source[SourceIndex])^, Pointer(@Destination[DestIndex])^, Count * SizeOf(T));
end;

class function TDynAUt.SearchPos<T>(const data: array of T; const Value: T; out Pos: NativeInt;
  Comparer: IComparer<T>; StartPos, EndPos: NativeInt): Boolean;
var I, J, K : NativeInt;
    LoLo, HiHi, MidLo, MidHi: T;
begin
  if not Assigned(Comparer) then
    Comparer := TComparer<T>.Default;

  if EndPos >= 0 then
    EndPos := Max(EndPos, High(data))
  else
    EndPos := High(data);

  if EndPos < StartPos then exit(False);

  if EndPos = StartPos then begin
    if Comparer.Compare(Value, data[StartPos]) = 0 then begin
      Pos := StartPos;
      exit(True);
    end else
      exit(False);
  end;

  I := StartPos;
  J := EndPos - 1;
  K := (I + J) div 2;
  LoLo := data[I];
  HiHi := data[J + 1];
  if (Comparer.Compare(LoLo, Value) >= 0) or (Comparer.Compare(Value, HiHi) >= 0) then exit(False);
  MidLo := data[K];
  MidHi := data[K + 1];
  // while not (MidLo <= Value < MidHi) do ...
  while (Comparer.Compare(MidLo, Value) > 0) or (Comparer.Compare(Value, MidHi) >= 0) do begin
    if Comparer.Compare(Value, MidLo) < 0 then
      J := K - 1
    else if Comparer.Compare(Value, MidHi) >= 0 then
      I := K + 1;
    K := (I + J) div 2;
    MidLo := data[K];
    MidHi := data[K + 1];
  end;
  Pos := K;
  Result := True;
end;

class function TDynAUt.SearchPos(const data: array of Double; const Value: Double;
  out Pos: NativeInt; StartPos, EndPos: NativeInt): Boolean;
var I, J, K : Integer;
    LoLo, HiHi, MidLo, MidHi: Double;
begin
  if EndPos >= 0 then
    EndPos := Max(EndPos, High(data))
  else
    EndPos := High(data);

  if EndPos < StartPos then exit(False);

  if EndPos = StartPos then begin
    if Value = data[StartPos] then begin
      Pos := StartPos;
      exit(True);
    end else
      exit(False);
  end;

  I := StartPos; J := EndPos - 1;
  K := (I + J) div 2;
  LoLo := data[I];
  HiHi := data[J + 1];
  if not InRange(Value, LoLo, HiHi) then exit(False);
  MidLo := data[K];
  MidHi := data[K + 1];
  // while not (MidLo <= Value < MidHi) do ...
  while (MidLo > Value) or (Value >= MidHi) do begin
    if Value < MidLo then
      J := K - 1
    else
    if Value >= MidHi then
      I := K + 1;
    K := (I + J) div 2;
    MidLo := data[K];
    MidHi := data[K + 1];
  end;
  Pos := K;
  Result := True;
end;

class function TDynAUt.BinarySearch(const Values: array of Double; const Item: Double;
  out FoundIndex: Integer; Index, Count: Integer): Boolean;
var L, H, mid: Integer;
begin
  if (Index < Low(Values)) or ((Index > High(Values)) and (Count > 0))
    or (Index + Count - 1 > High(Values)) or (Count < 0)
    or (Index + Count < 0) then
    raise EArgumentOutOfRangeException.CreateRes(@cSArgumentOutOfRange);
  if Count = 0 then
  begin
    FoundIndex := Index;
    Exit(False);
  end;

  Result := False;
  L := Index;
  H := Index + Count - 1;
  while L <= H do
  begin
    mid := L + (H - L) shr 1;
    if Values[mid] < Item then
      L := mid + 1
    else
    begin
      H := mid - 1;
      if Values[mid] = Item then
        Result := True;
    end;
  end;
  FoundIndex := L;
end;

class function TDynAUt.BinarySearch(const Values: array of Double; const Item: Double;
  out FoundIndex: Integer): Boolean;
begin
  Result := BinarySearch(Values, Item, FoundIndex, Low(Values), Length(Values));
end;

class procedure TDynAUt.QuickSort(var Values: array of Double; L, R: Integer);
var I, J: Integer;
    pivot, temp: Double;
begin
  if (Length(Values) = 0) or ((R - L) <= 0) then
    Exit;
  repeat
    I := L;
    J := R;
    pivot := Values[L + (R - L) shr 1];
    repeat
      while Values[I] < pivot do
        Inc(I);
      while Values[J] > pivot do
        Dec(J);
      if I <= J then
      begin
        if I <> J then
        begin
          temp := Values[I];
          Values[I] := Values[J];
          Values[J] := temp;
        end;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(Values, L, J);
    L := I;
  until I >= R;
end;

class function TDynAUt.SearchPosSuccessively<T>(const data: array of T; const Comparer: IComparer<T>; const Value: T;
  out Pos: Integer; StartPos: Integer = 0; EndPos: Integer = -1) : boolean;
var I, loRes, hiRes: Integer;
begin
  Result := False;
  if StartPos < 0 then StartPos := 0;
  if EndPos < 0 then EndPos := High(data)
  else EndPos := EnsureRange(EndPos, 0, High(data));
  if EndPos < StartPos then exit
  else if EndPos = StartPos then begin
    if Comparer.Compare(Value, data[StartPos]) = 0 then begin
      Pos := StartPos;
      exit(True);
    end else exit;
  end;

  loRes := Comparer.Compare(data[StartPos], Value);
  if loRes > 0 then exit;
  for I := StartPos to EndPos - 1 do begin
    hiRes := Comparer.Compare(Value, data[I + 1]);
    if (loRes <= 0) and (hiRes < 0) then begin
      Pos := I;
      exit(True);
    end;
    loRes := -hiRes;
  end;
end;

class function TDynAUt.Flatten<T>(const data: TArray<TArray<T>>) : TArray<T>;
var cBytes : TArray<Integer>;
    I, J, K, Count: Integer;
    iTmp: NativeInt;
    p : PByte;
begin
  Result := nil;
  if IsManaged<T> then begin
    Count := 0;
    for I := 0 to High(data) do
      Inc(Count, Length(data[I]));
    if Count = 0 then exit;
    SetLength(Result, Count);
    K := 0;
    for I := 0 to High(data) do
      for J := 0 to High(data[I]) do begin
        Result[K] := data[I, J];
        Inc(K);
      end;
  end else begin //there is no reference counting
    SetLength(cBytes, Length(data));
    Count := 0;
    for I := 0 to High(data) do begin
      iTmp := Length(data[I]);
      cBytes[I] := iTmp * SizeOf(T);
      Inc(Count, iTmp);
    end;
    if Count = 0 then exit;
    SetLength(Result, Count);
    p := @Result[0];
    for I := 0 to High(data) do begin
      iTmp := cBytes[I];
      Move(data[I, 0], p^, iTmp);
      p := p + iTmp;
    end;
  end;
end;

class function TDynAUt.Partition<T>(const Arr: TArray<T>;
  aRowSize: Integer; aOffset: Integer): TArray<TArray<T>>;
var I, cRowBytes, cSkipBytes, rowCount, restCount, len: Integer;
    tmpLen, tmpRowSize: Integer;
    pSrc: PByte;
begin
  Result := nil;
  len := Length(Arr);
  if len = 0 then exit;
  Assert((aRowSize > 0) and (aOffset >= 0) and (aOffset < aRowSize));
  tmpLen := len - aOffset;
  tmpRowSize := aRowSize - aOffset;
  rowCount := tmpLen div tmpRowSize;
  restCount := tmpLen mod tmpRowSize;
  if restCount = 0 then
    SetLength(Result, rowCount)
  else
    SetLength(Result, rowCount + 1);

  if IsManaged<T> then begin
    raise ENotImplemented.Create('TArrayUtils.Partition is not implemented for managed types.');
  end else begin
    pSrc := PByte(Arr);
    cRowBytes := aRowSize * SizeOf(T);
    cSkipBytes := tmpRowSize * SizeOf(T);
    for I := 0 to rowCount - 1 do begin
      SetLength(Result[I], aRowSize);
      Move(pSrc^, Result[I, 0], cRowBytes);
      Inc(pSrc, cSkipBytes);
    end;
    if restCount > 0 then begin
      I := High(Result);
      SetLength(Result[I], restCount + aOffset);
      Move(pSrc^, Result[I, 0], (restCount + aOffset) * SizeOf(T));
    end;
  end;
end;

class function TDynAUt.Reverse<T>(const Arr: TArray<T>): TArray<T>;
var I, J, Hi: Integer;
begin
  Result := nil;
  SetLength(Result, Length(Arr));
  Hi := High(Arr);
  J := Hi;
  for I := 0 to Hi do begin
    Result[J] := Arr[I];
    Dec(J);
  end;
end;

class procedure TDynAUt.ReverseInPlace<T>(var Arr: TArray<T>);
var I, J, Hi: Integer;
    tmp: T;
begin
  Hi := High(arr);
  J := Hi;
  Hi := Hi div 2;
  for I := 0 to Hi do begin
    tmp := Arr[I];
    Arr[I] := Arr[J];
    Arr[J] := tmp;
    Dec(J);
  end;
end;

class function TDynAUt.Transpose<T>(const OriginalArray : TArray<TArray<T>>): TArray<TArray<T>>;
var I, J : Integer;
begin
  if Length(OriginalArray) = 0 then exit;

  Result := nil;
  SetLength(Result, Length(OriginalArray[0]));
  for I := 0 to High(Result) do begin
    SetLength(Result[I], Length(OriginalArray));
    for J := 0 to High(OriginalArray) do
      Result[I][J] := OriginalArray[J][I];
  end;
end;

class function TDynAUt.Table<T>(const aFunc: TFnc<Double, T>; aX0, aX1, aXStep: Double): TArray<T>;
var I, count: Integer;
begin
  Assert(aXStep <> 0);
  if aXStep > 0 then begin
    if aX1 < aX0 then exit;
  end else begin
    if aX0 < aX1 then exit;
  end;
  count := Floor((aX1 - aX0 + aXStep) / aXStep);

  Result := nil;
  SetLength(Result, count);
  for I := 0 to count - 1 do
    Result[I] := aFunc(aX0 + I * aXStep);
end;

class function TDynAUt.Table<T>(const aFunc: TFnc<Double, Double, T>;
  aX0, aX1, aXStep, aY0, aY1, aYStep: Double): TArray<TArray<T>>;
var I, J, xCount, yCount: Integer;
begin
  Assert((aXStep <> 0) and (aYStep <> 0));
  if aXStep > 0 then begin
    if aX1 < aX0 then exit;
  end else begin
    if aX0 < aX1 then exit;
  end;
  xCount := Floor((aX1 - aX0 + 3/2*aXStep) / aXStep);
  if aYStep > 0 then begin
    if aY1 < aY0 then exit;
  end else begin
    if aY0 < aY1 then exit;
  end;
  yCount := Floor((aY1 - aY0 + 3/2*aYStep) / aYStep);

  Result := nil;
  SetLength(Result, xCount, yCount);
  for I := 0 to xCount - 1 do begin
    aX1 := aX0 + I * aXStep;
    for J := 0 to yCount - 1 do
      Result[I, J] := aFunc(aX1, aY0 + J * aYStep);
  end;
end;

class function TDynAUt.ElementsCount<T>(const aData: TArray<TArray<T>>): Integer;
var I: Integer;
begin
  Result := 0;
  for I := 0 to High(aData) do
    Inc(Result, Length(aData[I]));
end;

class function TDynAUt.SubArray<T>(const data: TArray<TArray<T>>;
  const IndexFrom1, IndexFrom2, Length1, Length2: Integer) : TArray<TArray<T>>;
var I, J, L1, L2: Integer;
begin
  Result := nil;
  SetLength(Result, Length1, Length2);
  L1 := Max(0, Min(Length1, Length(data) - IndexFrom1) - 1);
  if L1 = 0 then exit;
  L2 := Max(0, Min(Length2, Length(data[0]) - IndexFrom2) - 1);
  if L2 = 0 then exit;
  for I := 0 to L1 do
    for J := 0 to L2 do
      Result[I, J] := data[I + IndexFrom1, J + IndexFrom2];
end;

class function TDynAUt.SubArray<T>(const aData: TArray<T>; aFrom,
  aTo: Integer): TArray<T>;
var I, J: Integer;
begin
  Result := nil;
  EnsureRange(aFrom, 0, High(aData));
  EnsureRange(aTo, 0, High(aData));
  Assert(aFrom <= aTo);
  SetLength(Result, aTo - aFrom + 1);
  if IsManaged<T> then begin
    J := 0;
    for I := aTo to aFrom do begin
      Result[J] := aData[I];
      Inc(J);
    end;
  end else
    Move(aData[aFrom], Result[0], Length(Result) * SizeOf(T));
end;

class function TDynAUt.RotateRight<T>(const aData: TArray<T>; RotateBy: Integer): TArray<T>;
var iTmp: Integer;
begin
  iTmp := Length(aData);
  if iTmp = 0 then exit(nil);
  SetLength(Result, iTmp);
  RotateBy := (iTmp + (RotateBy mod iTmp)) mod iTmp;
  iTmp := Length(aData) - RotateBy;
  if IsManaged<T> then begin
    raise ENotImplemented.Create('TArrayUtils.RotateRigth<T> is not implemented for managed types.');
  end else begin
    Move(aData[0], Result[RotateBy], iTmp * SizeOf(T));
    if RotateBy = 0 then exit;
    Move(aData[Length(aData) - RotateBy], Result[0], RotateBy * SizeOf(T));
  end;
end;

class function TDynAUt.RotateLeft<T>(const aData: TArray<T>; RotateBy: Integer): TArray<T>;
var iTmp: Integer;
begin
  iTmp := Length(aData);
  if iTmp = 0 then exit(nil);
  SetLength(Result, iTmp);
  RotateBy := (iTmp + (RotateBy mod iTmp)) mod iTmp;
  iTmp := Length(aData) - RotateBy;
  if IsManaged<T> then begin
    raise ENotImplemented.Create('TArrayUtils.RotateLeft<T> is not implemented for managed types.');
  end else begin
    Move(aData[RotateBy], Result[0], iTmp * SizeOf(T));
    if RotateBy = 0 then exit;
    Move(aData[0], Result[Length(aData) - RotateBy], RotateBy * SizeOf(T));
  end;
end;

class procedure TDynAUt.MergeSort<T>(aData: TArray<T>; aComparer: IComparer<T>);
var me: TMergeSort<T>;
begin
  me.Create(aComparer);
  aData := me.Sort(aData);
end;

class procedure TDynAUt.MakeHeap<T>(aData: TArray<T>; aComparer: IComparer<T>;
  aCount: NativeInt; aAscendingOrder: Boolean);
var parentIdx, lastIdx, childIdx, tmpIdx: NativeInt;
    val: T;
begin
  if not Assigned(aData) then exit;

  if not Assigned(aComparer) then
    aComparer := TComparer<T>.Default;

  if aCount > 0 then
    lastIdx := Min(aCount, Length(aData))
  else
    lastIdx := Length(aData);
  parentIdx := lastIdx shr 1;

  if aAscendingOrder then begin
    while parentIdx > 0 do begin
      childIdx := parentIdx shl 1;
      if childIdx <= lastIdx then begin
        if
          (childIdx < lastIdx) and
          (aComparer.Compare(aData[childIdx - 1], aData[childIdx]) > 0)
        then
          Inc(childIdx);
        val := aData[parentIdx - 1];
        tmpIdx := parentIdx;

        while
          (childIdx <= lastIdx) and
          (aComparer.Compare(val, aData[childIdx - 1]) > 0)
        do begin
          aData[tmpIdx - 1] := aData[childIdx - 1];
          aData[childIdx - 1] := val;
          tmpIdx := childIdx;
          childIdx := tmpIdx shl 1;
          if
            (childIdx < LastIdx) and
            (aComparer.Compare(aData[childIdx - 1], aData[childIdx]) > 0)
          then
            Inc(childIdx);
        end;
      end;
      Dec(parentIdx);
    end;
  end else begin // descending order
    while parentIdx > 0 do begin
      childIdx := parentIdx shl 1;
      if childIdx <= lastIdx then begin
        if
          (childIdx < lastIdx) and
          (aComparer.Compare(aData[childIdx - 1], aData[childIdx]) < 0)
        then
          Inc(childIdx);
        val := aData[parentIdx - 1];
        tmpIdx := parentIdx;

        while
          (childIdx <= lastIdx) and
          (aComparer.Compare(val, aData[childIdx - 1]) < 0)
        do begin
          aData[tmpIdx - 1] := aData[childIdx - 1];
          aData[childIdx - 1] := val;
          tmpIdx := childIdx;
          childIdx := tmpIdx shl 1;
          if
            (childIdx < LastIdx) and
            (aComparer.Compare(aData[childIdx - 1], aData[childIdx]) < 0)
          then
            Inc(childIdx);
        end;
      end;
      Dec(parentIdx);
    end;
	end;
end;

class procedure TDynAUt.HeapSort<T>(aData: TArray<T>; aComparer: IComparer<T>;
  aAscendingOrder: Boolean);
var I, parentIdx, childIdx, heapSize: Integer;
    val: T;
begin
  if not Assigned(aData) then exit;

  if not Assigned(aComparer) then
    aComparer := TComparer<T>.Default;

  heapSize := Length(aData);
  MakeHeap<T>(aData, aComparer, -1, not aAscendingOrder);
  //note: the last leaf is ignored because this will be ordered in the next
  //extraction (due to can occur that last two elements have bad order, it has to
  //be solved on the end of the algorithm). This method saves one comparsion in
  //while loop.
  if aAscendingOrder then begin
    for I := High(aData) downto 2 do begin
      val := aData[I];
      aData[I] := aData[0];
      parentIdx := 1;
      childIdx := 3;
      Dec(heapSize);
      while childIdx <= heapSize do begin
        if aComparer.Compare(aData[childIdx - 2], aData[childIdx - 1]) > 0 then
          Dec(childIdx);
        if aComparer.Compare(aData[childIdx - 1], val) > 0 then begin //child > val -> swap
          aData[parentIdx - 1] := aData[childIdx - 1];
          parentIdx := childIdx;
          childIdx := parentIdx shl 1;
          Inc(childIdx);
        end else
          break;
      end;
      aData[parentIdx - 1] := val;
    end;
    if aComparer.Compare(aData[0], aData[1]) > 0 then begin
      val := aData[1];
      aData[1] := aData[0];
      aData[0] := val;
    end;
  end else begin // descending order
    for I := High(aData) downto 2 do begin
      val := aData[I];
      aData[I] := aData[0];
      parentIdx := 1;
      childIdx := 3;
      Dec(heapSize);
      while childIdx <= heapSize do begin
        if aComparer.Compare(aData[childIdx - 2], aData[childIdx - 1]) < 0 then
          Dec(childIdx);
        if aComparer.Compare(aData[childIdx - 1], val) < 0 then begin
          aData[parentIdx - 1] := aData[childIdx - 1];
          parentIdx := childIdx;
          childIdx := parentIdx shl 1;
          Inc(childIdx);
        end else
          break;
      end;
      aData[parentIdx - 1] := val;
    end;
	  if aComparer.Compare(aData[0], aData[1]) < 0 then begin
	    val := aData[1];
	    aData[1] := aData[0];
	    aData[0] := val;
	  end;
  end;
end;

class function TDynAUt.HeapPop<T>(aData: TArray<T>; aComparer: IComparer<T>;
  aCount: NativeInt; aAscendingOrder: Boolean): T;
var parentIdx, childIdx: NativeInt;
    val: T;
begin
  if aCount < 0 then aCount := Length(aData);
  if (aCount = 0) or (aCount > Length(aData)) then
    EArgumentOutOfRangeException.CreateRes(@csArgumentOutOfRange);

  if not Assigned(aComparer) then
    aComparer := TComparer<T>.Default;

  Result := aData[0];
  Dec(aCount);
  if aCount = 0 then exit;
  val := aData[aCount];
  aData[aCount] := Default(T);
  parentIdx := 1;
  childIdx := 2;
  if aAscendingOrder then begin
    while childIdx <= aCount do begin
      if
        (childIdx < aCount) and
        (aComparer.Compare(aData[childIdx - 1], aData[childIdx]) > 0)
      then
        Inc(childIdx);

      if aComparer.Compare(aData[childIdx - 1], val) < 0 then begin //child > val -> swap
        aData[parentIdx - 1] := aData[childIdx - 1];
        parentIdx := childIdx;
        childIdx := parentIdx shl 1;
      end else
        break;
    end;
    aData[parentIdx - 1] := val;
  end else begin
    while childIdx <= aCount do begin
      if
        (childIdx < aCount) and
        (aComparer.Compare(aData[childIdx - 1], aData[childIdx]) < 0)
      then
        Inc(childIdx);

      if aComparer.Compare(aData[childIdx - 1], val) > 0 then begin //child > val -> swap
        aData[parentIdx - 1] := aData[childIdx - 1];
        parentIdx := childIdx;
        childIdx := parentIdx shl 1;
      end else
        break;
    end;
    aData[parentIdx - 1] := val;
  end;
end;

class procedure TDynAUt.HeapPush<T>(var aData: TArray<T>; const aValue: T;
  aComparer: IComparer<T>; aCount: NativeInt; aAscendingOrder: Boolean);
var parentIdx, childIdx: NativeInt;
begin
  if aCount < 0 then aCount := High(aData);
  if (aCount < 0) or (aCount >= Length(aData)) then
    raise EArgumentOutOfRangeException.CreateRes(@csArgumentOutOfRange);

  if not Assigned(aComparer) then
    aComparer := TComparer<T>.Default;

  aData[aCount] := aValue;
  Inc(aCount);
  if aCount = 1 then exit;
  childIdx := aCount;
  parentIdx := childIdx shr 1;

  if aAscendingOrder then begin
    while parentIdx >= 1 do
      if aComparer.Compare(aData[parentIdx - 1], aValue) > 0 then begin //swap
        aData[childIdx - 1] := aData[parentIdx - 1];
        childIdx := parentIdx;
        parentIdx := childIdx shr 1;
      end else
        break;
    aData[childIdx - 1] := aValue;
  end else begin
    while parentIdx >= 1 do
      if aComparer.Compare(aData[parentIdx - 1], aValue) < 0 then begin //swap
        aData[childIdx - 1] := aData[parentIdx - 1];
        childIdx := parentIdx;
        parentIdx := childIdx shr 1;
      end else
        break;
    aData[childIdx - 1] := aValue;
  end;
end;

class function TDynAUt.HeapRemove<T>(aData: TArray<T>; const aValue: T;
  aComparer: IComparer<T>; aCount: NativeInt; aAscendingOrder: Boolean): Boolean;
var I, idx, parentIdx, childIdx: Integer;
    val: T;
begin
  if aCount < 0 then aCount := Length(aData);
  if aCount = 0 then exit(False);

  if not Assigned(aComparer) then
    aComparer := TComparer<T>.Default;

  idx := -1;
  for I := 0 to aCount - 1 do
    if aComparer.Compare(aData[I], aValue) = 0 then begin
      idx := I;
      break;
    end;

  if idx < 0 then exit(False);
  if idx = aCount - 1 then begin
    aData[idx] := Default(T);
    exit(True);
  end;

  val := aData[aCount - 1];
  aData[aCount - 1] := Default(T);
  aData[idx] := val;
  Dec(aCount);
  if aCount <= 1 then exit(True);
  parentIdx := idx + 1;
  childIdx := parentIdx shl 1;
  if aAscendingOrder then begin
    while childIdx <= aCount do begin
      if
        (childIdx < aCount) and
        (aComparer.Compare(aData[childIdx - 1], aData[childIdx]) > 0)
      then
        Inc(childIdx);

      if aComparer.Compare(aData[childIdx - 1], val) < 0 then begin //child > val -> swap
        aData[parentIdx - 1] := aData[childIdx - 1];
        parentIdx := childIdx;
        childIdx := parentIdx shl 1;
      end else
        break;
    end;
    aData[parentIdx - 1] := val;
  end else begin
    while childIdx <= aCount do begin
      if
        (childIdx < aCount) and
        (aComparer.Compare(aData[childIdx - 1], aData[childIdx]) < 0)
      then
        Inc(childIdx);

      if aComparer.Compare(aData[childIdx - 1], val) > 0 then begin //child > val -> swap
        aData[parentIdx - 1] := aData[childIdx - 1];
        parentIdx := childIdx;
        childIdx := parentIdx shl 1;
      end else
        break;
    end;
    aData[parentIdx - 1] := val;
  end;
  Result := True;
end;

class function TDynAUt.Take<T>(const aData: TArray<T>; const aIndices: TIndices): TArray<T>;
var I, count: NativeInt;
begin
  Result := nil;
  count := Length(aIndices);
  SetLength(Result, count);
  for I := 0 to count - 1 do
    Result[I] := aData[aIndices[I]];
end;

class function TDynAUt.Take<T>(const aData: TArray<T>; N: NativeInt): TArray<T>;
begin
  if N = 0 then exit(nil);
  Result := Drop<T>(aData, Sign(-N) * (Length(aData) - Abs(N)));
end;

class function TDynAUt.Drop<T>(const aData: TArray<T>; const idxs: TIndices): TArray<T>;
var I, tmp, hiData: NativeInt;
    dropped: TArray<Boolean>;
begin
  Setlength(dropped, Length(aData));
  tmp := 0;
  hiData := High(aData);
  for I := 0 to High(idxs) do
    if InRange(idxs[I], 0, hiData) then begin
      dropped[idxs[I]] := True;
      Inc(tmp);
    end;
  Result := nil;
  SetLength(Result, Length(aData) - tmp);
  tmp := 0;
  for I := 0 to hiData do
    if not dropped[I] then begin
      Result[tmp] := aData[I];
      Inc(tmp);
    end;
end;

class function TDynAUt.Drop<T>(const aData: TArray<T>; N: NativeInt): TArray<T>;
var I, count: NativeInt;
    bDropTail: Boolean;
begin
  if N = 0 then exit(aData);

  bDropTail := (N < 0);
  N := Abs(N);

  count := Length(aData);
  if N >= count then exit(nil);
  count := count - N;
  SetLength(Result, count);

  if bDropTail then begin
    for I := 0 to count - 1 do
      Result[I] := aData[I];
  end else begin
    for I := 0 to count - 1 do
      Result[I] := aData[N + I];
  end;
end;

class function TDynAUt.Insert<T>(const aData: TArray<T>; const aItem: T; aIdx: Integer): TArray<T>;
begin
  Result := nil;
  Assert(InRange(aIdx, 0, Length(aData)));
  SetLength(Result, Length(aData) + 1);
  Copy<T>(aData, Result, 0, 0, aIdx);
  Result[aIdx] := aItem;
  Copy<T>(aData, Result, aIdx, aIdx + 1, Length(aData) - aIdx);
end;

class function TDynAUt.InsertList<T>(const aData: TArray<T>; const aItems: array of T; const aIdxs: array of Integer): TArray<T>;
var newItems: TArray<TIndexedValue<T>>;
    I, count, lastIdx, srcIdx, dstIdx, seqCnt: Integer;
begin
  count := Length(aItems);
  Assert(count = Length(aIdxs));
  if count = 0 then begin
    Result := aData;
    exit;
  end else
  if count = 1 then begin
    Result := Insert<T>(aData, aItems[0], aIdxs[0]);
    exit;
  end;

  SetLength(newItems, count);
  for I := 0 to count - 1 do
    with newItems[I] do begin
      Idx := aIdxs[I];
      Value := aItems[I];
    end;

  //SortIndexedValuesByIndex<T>(newItems);
  SetLength(Result, Length(aData) + count);
  srcIdx := 0;
  dstIdx := 0;
  lastIdx := 0;
  for I := 0 to count - 1 do
    with newItems[I] do begin
      seqCnt := Idx - lastIdx;
      Copy<T>(aData, Result, srcIdx, dstIdx, seqCnt);
      Inc(srcIdx, seqCnt);
      Inc(dstIdx, seqCnt);
      Result[dstIdx] := Value;
      lastIdx := Idx;
      Inc(dstIdx);
    end;
  Copy<T>(aData, Result, srcIdx, dstIdx, Length(Result) - dstIdx);
end;

class function TDynAUt.Select<T>(const aData: TArray<T>; const aPredicate: TPredicateFunc<T>): TArray<T>;
var I, top, len: Integer;
    val: T;
begin
  Result := nil;
  len := Length(aData);
  SetLength(Result, len);
  top := 0;
  for I := 0 to len - 1 do begin
    val := aData[I];
    if aPredicate(val) then begin
      Result[top] := val;
      Inc(top);
    end;
  end;
  SetLength(Result, top);
end;

class function TDynAUt.Position<T>(const aData: TArray<T>; const aPredicate: TPredicateFunc<T>): TArray<Integer>;
var I, top, count: Integer;
begin
  Result := nil;
  count := Length(aData);
  SetLength(Result, count);
  top := 0;
  for I := 0 to count - 1 do
    if aPredicate(aData[I]) then begin
      Result[top] := I;
      Inc(top);
    end;
  SetLength(Result, top);
end;

class function TDynAUt.FirstPosition<T>(const aData: TArray<T>; const aItem: T; aComparer: IEqualityComparer<T>): Integer;
var I: Integer;
begin
  if not Assigned(aComparer) then aComparer := TEqualityComparer<T>.Default;
  for I := 0 to High(aData) do
    if aComparer.Equals(aData[I], aItem) then exit(I);
  Result := -1;
end;

class function TDynAUt.SortPerm<T>(const aData: TArray<T>; aComparer: IComparer<T>): TArray<Integer>;
var I, count: Integer;
begin
  if not Assigned(aComparer) then aComparer := TComparer<T>.Default;

  Result := nil;
  count := Length(aData);
  SetLength(Result, count);
  for I := 0 to count - 1 do
    Result[I] := I;
{$ifdef FPC}
  // todo
{$else}
  TArray.Sort<Integer>(Result,
    TComparer<Integer>.Construct(
      function (const aL, aR: Integer): Integer
      begin
        Result := aComparer.Compare(aData[aL], aData[aR]);
        if Result = 0 then
          Result := aL - aR;
      end
    )
  );
{$endif}
end;

{$ifndef FPC}

class function TDynAUt.SortPerm<T>(const aData: TArray<T>; const aComparer: TComparison<T>): TArray<Integer>;
begin
  Result := SortPerm<T>(aData, TComparer<T>.Construct(aComparer));
end;

{$endif}

class procedure TDynAUt.SortIndexedValues<T>(var aValues: TArray<TIndexedValue<T>>; aComparer: IComparer<T>);
begin
  if not Assigned(aComparer) then
    aComparer := TComparer<T>.Default;

{$ifdef FPC}
  // todo
{$else}
  TArray.Sort<TIndexedValue<T>>(aValues,
    TComparer<TIndexedValue<T>>.Construct(
      function(const Left, Right: TIndexedValue<T>): Integer
      begin
        Result := aComparer.Compare(Left.Value, Right.Value);
      end
    )
  );
{$endif}
end;

class procedure TDynAUt.SortIndexedValuesByIndex<T>(var aValues: TArray<TIndexedValue<T>>);
begin
{$ifdef FPC}
  // todo
{$else}
  TArray.Sort<TIndexedValue<T>>(aValues,
    TComparer<TIndexedValue<T>>.Construct(
      function (const aLeft, aRight: TIndexedValue<T>): Integer
      begin
        Result := (aLeft.Idx - aRight.Idx)
      end
    )
  );
{$endif}
end;

class function TDynAUt.Map<TIn, TOut>(const aData: TArray<TIn>;
  const aFnc: TFnc<TIn, TOut>): TArray<TOut>;
var I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(aData));
  for I := 0 to High(Result) do Result[I] := aFnc(aData[I]);
end;

class function TDynAUt.Map2D<Tin, TOut>(const aData: TArray<TArray<TIn>>;
  const aFnc: TFnc<TIn, TOut>): TArray<TArray<TOut>>;
var I, J: Integer;
    srcRow: TArray<TIn>;
    dstRow: TArray<TOut>;
begin
  Result := nil;
  SetLength(Result, Length(aData));
  for I := 0 to High(aData) do begin
    srcRow := aData[I];
    SetLength(dstRow, Length(srcRow));
    for J := 0 to High(srcRow) do
      dstRow[J] := aFnc(srcRow[J]);
    Result[I] := dstRow;
    dstRow := nil;
  end;
end;

class function TDynAUt.Map<T>(const aData: TArray<T>; const aFnc: TFnc<T, T>): TArray<T>;
begin
  Result := Map<T, T>(aData, aFnc);
end;

class function TDynAUt.Map2D<T>(const aData: TArray<TArray<T>>; const aFnc: TFnc<T, T>): TArray<TArray<T>>;
{$ifndef FPC}
begin
  Result := Map2D<T, T>(aData, aFnc);
end;
{$else}
// Map2D<T, T>(aData, aFnc) raises internal compiler exception
var I, J: NativeInt;
    srcRow, dstRow: TArray<T>;
begin
  Result := nil;
  SetLength(Result, Length(aData));
  for I := 0 to High(aData) do begin
    srcRow := aData[I];
    SetLength(dstRow, Length(srcRow));
    for J := 0 to High(srcRow) do
      dstRow[J] := aFnc(srcRow[J]);
    Result[I] := dstRow;
    dstRow := nil;
  end;
end;
{$endif}

class function TDynAUt.MapIndexed<TIn, TOut>(const aDAta: TArray<TIn>;
  const aFnc: TFnc<TIn, Integer, TOut>): TArray<TOut>;
var I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(aData));
  for I := 0 to High(Result) do
    Result[I] := aFnc(aData[I], I);
end;

class function TDynAUt.Append<T>(const aData: TArray<T>; const aValue: T): TArray<T>;
var count: Integer;
begin
  Result := nil;
  count := Length(aData);
  SetLength(Result, count + 1);
  Copy<T>(aData, Result, 0, 0, count);
  Result[count] := aValue;
end;

class function TDynAUt.Prepend<T>(const aData: TArray<T>; const aValue: T): TArray<T>;
var count: Integer;
begin
  Result := nil;
  count := Length(aData);
  SetLength(Result, count + 1);
  Result[0] := aValue;
  Copy<T>(aData, Result, 0, 1, count);
end;

class function TDynAUt.AppendList<T>(const aData: TArray<T>; const aValues: array of T): TArray<T>;
begin
  Result := nil;
  SetLength(Result, Length(aData) + Length(aValues));
  Copy<T>(aData, Result, 0, 0, Length(aData));
  Copy<T>(aValues, Result, 0, Length(aData), Length(aValues));
end;

class function TDynAUt.PrependList<T>(const aData: TArray<T>; const aValues: array of T): TArray<T>;
begin
  Result := nil;
  SetLength(Result, Length(aData) + Length(aValues));
  Copy<T>(aValues, Result, 0, 0, Length(aValues));
  Copy<T>(aData, Result, 0, Length(aValues), Length(aData));
end;

class function TDynAUt.MemberQ<T>(const aData: TArray<T>; const aValue: T;
  const aComparer: IComparer<T>): Boolean;
var elem: T;
    cmp: IComparer<T>;
begin
  Result := False;
  if not Assigned(aComparer) then cmp := TComparer<T>.Default
  else cmp := aComparer;
  for elem in aData do
    if cmp.Compare(elem, aValue) = 0 then exit(True);
end;

class function TDynAUt.FindMax<T>(const aData: TArray<T>;
  const aComparer: IComparer<T>): T;
var idx: Integer;
begin
  idx := FindMaxPos<T>(aData, aComparer);
  Result := aData[idx];
end;

class function TDynAUt.FindMaxPos<T>(const aData: TArray<T>;
  aComparer: IComparer<T>): Integer;
var I: Integer;
begin
  Assert(Length(aData) > 0);
  Result := 0;
  if not Assigned(aComparer) then aComparer := TComparer<T>.Default;
  for I := 1 to High(aData) do
    if aComparer.Compare(aData[I], aData[Result]) > 0 then Result := I;
end;

class function TDynAUt.FindMin<T>(const aData: TArray<T>;
  const aComparer: IComparer<T>): T;
var idx: Integer;
begin
  idx := FindMinPos<T>(aData, aComparer);
  Result := aData[idx];
end;

class function TDynAUt.FindMinPos<T>(const aData: TArray<T>;
  aComparer: IComparer<T>): Integer;
var I: Integer;
begin
  Assert(Length(aData) > 0);
  Result := 0;
  if not Assigned(aComparer) then aComparer := TComparer<T>.Default;
  for I := 1 to High(aData) do
    if aComparer.Compare(aData[I], aData[Result]) < 0 then Result := I;
end;

class procedure TDynAUt.FindMinMax<T>(const aData: TArray<T>; var aMin, aMax: T; aComparer: IComparer<T>);
var I: Integer;
    v: T;
begin
  Assert(Length(aData) > 0);
  aMin := aData[0];
  aMax := aData[1];
  if not Assigned(aComparer) then aComparer := TComparer<T>.Default;
  for I := 1 to High(aData) do begin
    v := aData[I];
    if aComparer.Compare(v, aMin) < 0 then
      aMin := v
    else if aComparer.Compare(v, aMax) > 0 then
      aMax := v;
  end;
end;

class function TDynAUt.FindFirstPos<T>(const aData: TArray<T>; aPred: TPredicateFunc<T>): Integer;
var I: Integer;
begin
  for I := 0 to High(aData) do
    if aPred(aData[I]) then exit(I);
  Result := -1;
end;

class function TDynAUt.FindFirstPos<T>(const aData: TArray<T>; const aValue: T; aComparer: IComparer<T>): NativeInt;
var I: NativeInt;
begin
  if not Assigned(aComparer) then aComparer := TComparer<T>.Default;
  for I := 0 to High(aData) do
    if aComparer.Compare(aData[I], aValue) = 0 then exit(I);
  Result := -1;
end;


class function TDynAUt.Where<T>(const aData: TArray<T>; const aPredicate: TPredicateFunc<T>): TArray<Integer>;
var I, count, top: Integer;
begin
  Result := nil;
  count := Length(aData);
  SetLength(Result, count);
  top := 0;
  for I := 0 to count - 1 do
    if aPredicate(aData[I]) then begin
      Result[top] := I;
      Inc(top);
    end;
  SetLength(Result, top);
end;

class function TDynAUt.Where<T>(const aData: TArray<TArray<T>>; const aPredicate: TPredicateFunc<T>): TArray<TArrayIndex2D>;
var I, J, w, h: Integer;
    res: TList<TArrayIndex2D>;
    rowData: TArray<T>;
    idx: TArrayIndex2D;
begin
  h := Length(aData);
  if h = 0 then exit(nil);
  w := Length(aData[0]);
  res := TList<TArrayIndex2D>.Create;
  try
    res.Capacity := h * Max(1, w);
    for I := 0 to h - 1 do begin
      rowData := aData[I];
      w := Length(rowData);
      for J := 0 to w - 1 do
        if aPredicate(rowData[J]) then begin
          with idx do begin
            Row := I;
            Col := J;
          end;
          res.Add(idx);
        end;
    end;
    Result := res.ToArray;
  finally
    res.Free;
  end;
end;

class function TDynAUt.Median<T>(const aData: TArray<T>; aComparer: IComparer<T>): T;
var tmp: TArray<T>;
    len: Integer;
begin
  len := Length(aData);
  Assert(len > 0);
  if len = 1 then exit(aData[0]);
  if not Assigned(aComparer) then aComparer := TComparer<T>.Default;
  tmp := Clone<T>(aData);
{$ifndef FPC}
  TArray.Sort<T>(tmp, aComparer);
{$else}
  TArrayHelper<T>.Sort(tmp);
{$endif}
  Result := tmp[len div 2];
end;

class function TDynAUt.GatherBy<T, U>(const aData: TArray<T>; const aFnc: TFnc<T, U>;
  aComparer: IComparer<U>): TArray<TArray<T>>;
var values: TArray<TIndexedValue<U>>;
    I, J, top: Integer;
    res: TArray<TIndexedValueList<U>>;
    cmp: IComparer<TIndexedValue<U>>;
{$ifdef FPC}
type TIdxValuesHelper = TArrayHelper<TIndexedValue<U>>;
{$endif}
begin
  if Length(aData) = 0 then exit;
  if not Assigned(aComparer) then aComparer := TComparer<U>.Default;
  SetLength(values, Length(aData));
  for I := 0 to High(values) do begin
    values[I].Value := aFnc(aData[I]);
    values[I].Idx := I;
  end;

  cmp := TIndexedValueComparer<U>.Create(aComparer);
{$ifdef FPC}
  TIdxValuesHelper.Sort(values, cmp);
{$else}
  TArray.Sort<TIndexedValue<U>>(values, cmp);
{$endif}
  SetLength(res, Length(aData));
  for I := 0 to High(values) do
    res[I] := TIndexedValueList<U>.Create;
  res[0].Add(values[0]);
  top := 0;
  for I := 1 to High(values) do begin
    if aComparer.Compare(res[top].First.Value, values[I].Value) = 0 then
      res[top].Add(values[I])
    else begin
      Inc(top);
      res[top].Add(values[I]);
    end;
  end;
  Result := nil;
  SetLength(Result, top + 1);
  for I := 0 to High(Result) do begin
    SetLength(Result[I], res[I].Count);
    for J := 0 to res[I].Count - 1 do
      Result[I, J] := aData[res[I][J].Idx];
  end;
  for I := 0 to High(res) do
    res[I].Free;
end;

class function TDynAUt.GatherBy<T>(const aData: TArray<T>; aComparer: IComparer<T>): TArray<TArray<T>>;
{$ifdef FPC}
var id: TIdentity<T>;
{$endif}
begin
{$ifdef FPC}
  id := TIdentity<T>.Create;
  try
    Result := GatherBy<T, T>(aData, id.Execute, aComparer);
	finally
    id.Free;
	end;
{$else}
  Result := GatherBy<T, T>(
    aData
    ,
    function (const aValue: T): T
    begin
      Result := aValue;
    end
    ,
    aComparer
  );
{$endif}
end;

class function TDynAUt.SplitBy<T>(const aData: TArray<T>; aComparer: IComparer<T>): TArray<TArray<T>>;
type TArrayList = TList<TArray<T>>;
var buff: TList<T>;
    res: TList<TArray<T>>;
    I: Integer;
    a, b: T;
begin
  if Length(aData) = 0 then exit(nil);
  if not Assigned(aComparer) then aComparer := TComparer<T>.Default;

  buff := TList<T>.Create;
  res := TArrayList.Create;
  try
    buff.Capacity := Length(aData);
    a := aData[0];
    buff.Add(a);
    for I := 1 to High(aData) do begin
      b := aData[I];
      if aComparer.Compare(a, b) = 0 then begin
        buff.Add(b);
      end else begin
        res.Add(buff.ToArray);
        buff.Clear;
        buff.Add(b);
      end;
      a := b;
    end;
    if buff.Count > 0 then
      res.Add(buff.ToArray);
    Result := res.ToArray;
  finally
    buff.Free;
    res.Free;
  end;
end;

class function TDynAUt.MovingMap<T, U>(const aData: TArray<T>; const aFnc: TFnc<TArray<T>, U>;
  aWin: Integer): TArray<U>;
var win: TArray<T>;
    I, J, K: Integer;
begin
  Assert(aWin > 0);
  if aWin > Length(aData) then
    raise EInvalidArgument.Create('TDynAUt.MovingMap error: Window size is greather than data size.');

  Result := nil;
  SetLength(win, aWin);
  SetLength(Result, Length(aData) - aWin + 1);
  for I := 0 to aWin - 1 do
    win[I] := aData[I];
  Result[0] := aFnc(win);
  K := 1;
  for I := aWin to High(aData) do begin
    for J := 1 to aWin - 1 do
      win[J - 1] := win[J];
    win[aWin - 1] := aData[I];
    Result[K] := aFnc(win);
    Inc(K);
  end;
end;

{$ifndef FPC}

class function TDynAUt.ToMListString<T>(const arr: TArray<T>): String;
var fmt: TFormatSettings;
begin
  fmt := TFormatSettings.Create;
  fmt.DecimalSeparator := '.';
  Result := ToMListString<T>(arr, fmt);
end;

class function TDynAUt.ToMListString<T>(const arr: TArray<T>;
  const aFmt: TFormatSettings): String;
var sb: TStringBuilder;
    strGetter: TMStrGetter;
    val: TValue;
    ti: PTypeInfo;
    I: Integer;
begin
  sb := TStringBuilder.Create;
  try
    ti := TypeInfo(T);
    case ti^.Kind of
      tkFloat: strGetter := Real2MStr;
    else
      strGetter := MStr;
    end;
    sb.Append('{');
    for I := 0 to High(arr) - 1 do begin
      TValue.Make(@arr[I], ti, val);
      sb.Append(StrGetter(val, aFmt)).Append(', ');
    end;
    TValue.Make(@arr[High(arr)], ti, val);
    sb.Append(StrGetter(val, aFmt));
    sb.Append('}');
  finally
    Result := sb.ToString;
    sb.Free;
  end;
end;

class function TDynAUt.ToMListString<T>(const arr: TArray<TArray<T>>): String;
var fmt: TFormatSettings;
begin
  fmt := TFormatSettings.Create;
  fmt.DecimalSeparator := '.';
  Result := ToMListString<T>(arr, fmt);
end;

class function TDynAUt.ToMListString<T>(const arr: TArray<TArray<T>>;
  const aFmt: TFormatSettings): String;
var I, J: Integer;
    sb: TStringBuilder;
    strGetter: TMStrGetter;
    val: TValue;
    ti: PTypeInfo;
begin
  sb := TStringBuilder.Create;
  try
    ti := TypeInfo(T);
    case ti^.Kind of
      tkFloat: strGetter := Real2MStr;
    else
      strGetter := MStr;
    end;

    sb.Append('{');
    for I := 0 to High(arr) do begin
      sb.Append('{');
      for J := 0 to High(arr[I]) do begin
        TValue.make(@arr[I, J], ti, val);
        sb.Append(StrGetter(val, aFmt)).Append(',');
      end;
      sb[sb.Length - 1] := '}';
      sb.Append(',');
    end;
    sb[sb.Length - 1] := '}';
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

class procedure TDynAUt.ExportMListString<T>(const aFileName: String; const aArr: TArray<T>);
var w: TStreamWriter;
begin
  w := TStreamWriter.Create(aFileName);
  try
    w.Write(ToMListString<T>(aArr));
  finally
    w.Free;
  end;
end;

class procedure TDynAUt.ExportMListString<T>(const aFileName: String; const aArr: TArray<TArray<T>>);
var w: TStreamWriter;
begin
  w := TStreamWriter.Create(aFileName);
  try
    w.Write(ToMListString<T>(aArr));
  finally
    w.Free;
  end;
end;

{$endif}

class function TDynAUt.Riffle<T>(const A, B: TArray<T>): TArray<T>;
var I, J, ca, cb: Integer;
begin
  ca := Length(A);
  cb := Length(B);
  if ca = cb then begin
    ca := 2 * ca;
    Result := nil;
    SetLength(Result, ca);
    I := 0;
    J := 0;
    while I < ca do begin
      Result[I] := A[J];
      Inc(I);
      Result[I] := B[J];
      Inc(I);
      Inc(J);
    end;
  end else
    raise ENotImplemented.Create('TDynAUt.Riffle not implemented yet.');
end;

class function TDynAUt.ToArray<T>(const aData: array of T): TArray<T>;
var I, count: Integer;
begin
  Result := nil;
  count := Length(aData);
  SetLength(Result, count);
  for I := 0 to count - 1 do
    Result[I] := aData[I];
end;

class function TDynAUt.ConstantArray<T>(const aValue: T; aCount: Integer): TArray<T>;
var I: Integer;
begin
  Assert(aCount > 0);
  Result := nil;
  SetLength(Result, aCount);
  for I := 0 to aCount - 1 do
    Result[I] := aValue;
end;

class function TDynAUt.ItemCount<T>(const aArray: TArray<TArray<T>>): NativeInt;
var I: Integer;
begin
  Result := 0;
  for I := 0 to High(aArray) do
    Inc(Result, Length(aArray[I]));
end;

class function TDynAUt.MaxLength<T>(const aArray: TArray<TArray<T>>): NativeInt;
var I, len: Integer;
begin
  Result := 0;
  for I := 0 to High(aArray) do begin
    len := Length(aArray[I]);
    if len > Result then
      Result := len;
  end;
end;

class function TDynAUt.PositiveQ(const aArray: array of Integer): Boolean;
var I: Integer;
begin
  for I := 0 to High(aArray) do
    if aArray[I] <= 0 then exit(False);
  Result := True;
end;

class function TDynAUt.Int2NInt(const aArray: array of Integer): TArray<NativeInt>;
var I, count: Integer;
begin
  Result := nil;
  count := Length(aArray);
  SetLength(Result, count);
  for I := 0 to count - 1 do
    Result[I] := aArray[I];
end;

class function TDynAUt.Range(const aLo, aHi: NativeInt; const aStep: NativeInt = 1): TArray<NativeInt>;
var I, count: NativeInt;
begin
  Assert(aStep <> 0);
  if aStep > 0 then begin
    if aHi < aLo then exit(nil);
  end else begin
    if aLo < aHi then exit(nil);
  end;
  count := Floor((aHi - aLo + aStep) / aStep);

  SetLength(Result, count);
  if aStep = 1 then begin
    if aLo = 0 then begin
      for I := 0 to count - 1 do
        Result[I] := I;
    end else begin
      for I := 0 to count - 1 do
        Result[I] := aLo + I;
    end;
  end else
  if aStep = -1 then begin
    if aLo = 0 then begin
      for I := 0 to count - 1 do
        Result[I] := -I;
    end else begin
      for I := 0 to count - 1 do
        Result[I] := aLo - I;
    end;
  end else begin
    if aLo = 0 then begin
      for I := 0 to count - 1 do
        Result[I] := I * aStep;
    end else begin
      for I := 0 to count - 1 do
        Result[I] := aLo + I * aStep;
    end;
  end;
end;

class function TDynAUt.Range_I32(aLo, aHi: Integer; aStep: Integer = 1): TArray<Integer>;
var I, count: Integer;
begin
  Assert(aStep <> 0);
  if aStep > 0 then begin
    if aHi < aLo then exit(nil);
  end else begin
    if aLo < aHi then exit(nil);
  end;
  count := Floor((aHi - aLo + aStep) / aStep);

  SetLength(Result, count);
  if aStep = 1 then begin
    if aLo = 0 then begin
      for I := 0 to count - 1 do
        Result[I] := I;
    end else begin
      for I := 0 to count - 1 do
        Result[I] := aLo + I;
    end;
  end else
  if aStep = -1 then begin
    if aLo = 0 then begin
      for I := 0 to count - 1 do
        Result[I] := -I;
    end else begin
      for I := 0 to count - 1 do
        Result[I] := aLo - I;
    end;
  end else begin
    if aLo = 0 then begin
      for I := 0 to count - 1 do
        Result[I] := I * aStep;
    end else begin
      for I := 0 to count - 1 do
        Result[I] := aLo + I * aStep;
    end;
  end;
end;

class function TDynAUt.Range(const aLo, aHi: Double; const aStep: Double): TArray<Double>;
var I, count: Integer;
begin
  Assert(aStep <> 0);
  if aStep > 0 then begin
    if aHi < aLo then exit(nil);
  end else begin
    if aLo < aHi then exit(nil);
  end;
  count := Floor((aHi - aLo + aStep) / aStep);

  SetLength(Result, count);
  if SameValue(aStep, 1) then begin
    if IsZero(aLo) then begin
      for I := 0 to count - 1 do
        Result[I] := I;
    end else begin
      for I := 0 to count - 1 do
        Result[I] := aLo + I;
    end;
  end else
  if SameValue(aStep, -1) then begin
    if IsZero(aLo) then begin
      for I := 0 to count - 1 do
        Result[I] := -I;
    end else begin
      for I := 0 to count - 1 do
        Result[I] := aLo - I;
    end;
  end else begin
    if IsZero(aLo) then begin
      for I := 0 to count - 1 do
        Result[I] := I * aStep;
    end else begin
      for I := 0 to count - 1 do
        Result[I] := aLo + I * aStep;
    end;
  end;
end;

class function TDynAUt.Range_F32(const aLo, aHi: Single; const aStep: Single): TArray<Single>;
var I, count: Integer;
begin
  Assert(aStep <> 0);
  if aStep > 0 then begin
    if aHi < aLo then exit(nil);
  end else begin
    if aLo < aHi then exit(nil);
  end;
  count := Floor((aHi - aLo + aStep) / aStep);

  SetLength(Result, count);
  if SameValue(aStep, 1) then begin
    if IsZero(aLo) then begin
      for I := 0 to count - 1 do
        Result[I] := I;
    end else begin
      for I := 0 to count - 1 do
        Result[I] := aLo + I;
    end;
  end else
  if SameValue(aStep, -1) then begin
    if IsZero(aLo) then begin
      for I := 0 to count - 1 do
        Result[I] := -I;
    end else begin
      for I := 0 to count - 1 do
        Result[I] := aLo - I;
    end;
  end else begin
    if IsZero(aLo) then begin
      for I := 0 to count - 1 do
        Result[I] := I * aStep;
    end else begin
      for I := 0 to count - 1 do
        Result[I] := aLo + I * aStep;
    end;
  end;
end;

{$ifndef FPC}

{$region 'string array utils'}

class function TDynAUt.ToUpper(const aArray: array of String): TArray<String>;
var I: Integer;
begin
  SetLength(Result, Length(aArray));
  for I := 0 to High(aArray) do
    Result[I] := aArray[I].ToUpper;
end;

class function TDynAUt.ToLower(const aArray: array of String): TArray<String>;
var I: Integer;
begin
  SetLength(Result, Length(aArray));
  for I := 0 to High(aArray) do
    Result[I] := aArray[I].ToLower;
end;

{$endregion}

{$endif}

{$endregion}


end.
