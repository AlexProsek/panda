unit pandalib;

interface

uses
    panda.Intfs
  , panda.Arrays
  , panda.ArrManip
  , panda.Arithmetic
  , panda.Math
  , panda.Formatter
  ;

type
  TPaNDALib = class
  public
    // Array construction
    class function Empty<T>(aDims: array of NativeInt): INDArray<T>;
    class function Scalar<T>(const aValue: T): INDScalar<T>;
    class function Full<T>(const aDims: array of NativeInt; const aValue: T): INDArray<T>;
    class function AsArray<T>(const aItems: array of T; const aDims: array of NativeInt): INDArray<T>; overload;
    class function AsArray<T>(const aItems: array of T): INDArray<T>; overload;
    class function AsArray<T>(const aItems: TNDAUt.TOArray2D<T>): INDArray<T>; overload;
    class function AsArray<T>(const aItems: TArray<T>): INDArray<T>; overload;

    class function Range(aHi: Integer): INDArray<Integer>; overload;
    class function Range(aLo, aHi: Integer; aStep: Integer = 1): INDArray<Integer>; overload;
    class function Range(aHi: Int64): INDArray<Int64>; overload;
    class function Range(aLo, aHi: Int64; aStep: Int64 = 1): INDArray<Int64>; overload;
    class function Range(aHi: Single): INDArray<Single>; overload;
    class function Range(aLo, aHi: Single; aStep: Single = 1): INDArray<Single>; overload;

    // Array manipulation
    class function Transpose<T>(const aArr: INDArray<T>): INDArray<T>; overload;
    class function Transpose<T>(const aArr: INDArray<T>; const aAxes: TArray<Integer>): INDArray<T>; overload;

    // Arithmetic
    class function Add(const aArrays: array of INDArray; var aRes: INDArray): Boolean; overload;
    class function Add(const aArrays: array of INDArray): INDArray; overload;
    class function Multiply(const aArrays: array of INDArray; var aRes: INDArray): Boolean; overload;
    class function Multiply(const aArrays: array of INDArray): INDArray; overload;

    // Math functions
    class function Exp(const aArr: INDArray<Single>): INDArray<Single>; overload;

    class function TotalAtLvl(const aArr: INDArray<Integer>; aLvl: Integer): INDArray<Integer>; overload;
    class function TotalAtLvl(const aArr: INDArray<Single>; aLvl: Integer): INDArray<Single>; overload;

    function Dot(const aA, aB: INDArray<Single>): INDArray<Single>; overload;
    function Dot(const aA, aB: INDArray<Double>): INDArray<Double>; overload;

    function Outer(const aA, aB: INDArray<Single>): INDArray<Single>; overload;
    function Outer(const aA, aB: INDArray<Double>): INDArray<Double>; overload;


    // Utils
    class function GetString(const aArr: INDArray): String; overload;
    class function TryAsScalar<T>(const aArr: INDArray; out aValue: T): Boolean;
    class function TryAsDynArray<T>(const aArr: INDArray; out aValue: TArray<T>): Boolean; static;
    class function TryAsDynArray2D<T>(const aArr: INDArray; out aValue: TArray<TArray<T>>): Boolean; static;
  end;

var nda: TPaNDALib;

implementation

{$region 'Array construction'}

class function TPaNDALib.Empty<T>(aDims: array of NativeInt): INDArray<T>;
begin
  Result := TNDAUt.Empty<T>(aDims);
end;

class function TPaNDALib.Scalar<T>(const aValue: T): INDScalar<T>;
begin
  Result := TNDAUt.Scalar<T>(aValue);
end;

class function TPaNDALib.Full<T>(const aDims: array of NativeInt; const aValue: T): INDArray<T>;
begin
  Result := TNDAUt.Full<T>(aDims, aValue);
end;

class function TPaNDALib.AsArray<T>(const aItems: array of T; const aDims: array of NativeInt): INDArray<T>;
begin
  Result := TNDAUt.AsArray<T>(aItems, aDims);
end;

class function TPaNDALib.AsArray<T>(const aItems: array of T): INDArray<T>;
begin
  Result := TNDAUt.AsArray(aItems);
end;

class function TPaNDALib.AsArray<T>(const aItems: TArray<T>): INDArray<T>;
begin
  Result := TDynArrWrapper<T>.Create(aItems);
end;

class function TPaNDALib.AsArray<T>(const aItems:TNDAUt.TOArray2D<T>): INDArray<T>;
begin
  Result := TNDAUt.AsArray<T>(aItems);
end;

class function TPaNDALib.Range(aHi: Integer): INDArray<Integer>;
begin
  Result := ndaRange(0, aHi);
end;

class function TPaNDALib.Range(aLo, aHi, aStep: Integer): INDArray<Integer>;
begin
  Result := ndaRange(aLo, aHi, aStep);
end;

class function TPaNDALib.Range(aHi: Int64): INDArray<Int64>;
begin
  Result := ndaRange(0, aHi);
end;

class function TPaNDALib.Range(aLo, aHi, aStep: Int64): INDArray<Int64>;
begin
  Result := ndaRange(aLo, aHi, aStep);
end;

class function TPaNDALib.Range(aHi: Single): INDArray<Single>;
begin
  Result := ndaRange(0, aHi);
end;

class function TPaNDALib.Range(aLo, aHi, aStep: Single): INDArray<Single>;
begin
  Result := ndaRange(aLo, aHi, aStep);
end;

{$endregion}

{$region 'Array Manipulation'}

class function TPaNDALib.Transpose<T>(const aArr: INDArray<T>): INDArray<T>;
begin
  Result := TNDAMan.Transpose<T>(aArr);
end;

class function TPaNDALib.Transpose<T>(const aArr: INDArray<T>; const aAxes: TArray<Integer>): INDArray<T>;
begin
  Result := TNDAMan.Transpose<T>(aArr, aAxes);
end;

{$endregion}

{$region 'Arithmetic'}

class function TPaNDALib.Add(const aArrays: array of INDArray; var aRes: INDArray): Boolean;
begin
  Result := ndaAdd(aArrays, aRes);
end;

class function TPaNDALib.Add(const aArrays: array of INDArray): INDArray;
begin
  if not ndaAdd(aArrays, Result) then
    Result := nil;
end;

class function TPaNDALib.Multiply(const aArrays: array of INDArray; var aRes: INDArray): Boolean;
begin
  Result := ndaMultiply(aArrays, aRes);
end;

class function TPaNDALib.Multiply(const aArrays: array of INDArray): INDArray;
begin
  if not ndaMultiply(aArrays, Result) then
    Result := nil;
end;

{$endregion}

{$region 'Math functions'}

class function TPaNDALib.Exp(const aArr: INDArray<Single>): INDArray<Single>;
begin
  Result := ndaExp(aArr);
end;

class function TPaNDALib.TotalAtLvl(const aArr: INDArray<Integer>; aLvl: Integer): INDArray<Integer>;
begin
  Result := ndaTotalAtLvl(aARr, aLvl);
end;

class function TPaNDALib.TotalAtLvl(const aArr: INDArray<Single>; aLvl: Integer): INDArray<Single>;
begin
  Result := ndaTotalAtLvl(aARr, aLvl);
end;

function TPaNDALib.Dot(const aA, aB: INDArray<Single>): INDArray<Single>;
begin
  Result := ndaDot(aA, aB);
end;

function TPaNDALib.Dot(const aA, aB: INDArray<Double>): INDArray<Double>;
begin
  Result := ndaDot(aA, aB);
end;

function TPaNDALib.Outer(const aA, aB: INDArray<Single>): INDArray<Single>;
begin
  Result := ndaOuter(aA, aB);
end;

function TPaNDALib.Outer(const aA, aB: INDArray<Double>): INDArray<Double>;
begin
  Result := ndaOuter(aA, aB);
end;

{$endregion}

{$region 'Utils'}

class function TPaNDALib.GetString(const aArr: INDArray): String;
var fmt: TNDAFormatter;
begin
  fmt := TNDAFormatter.Create;
  try
    Result := fmt.GetString(aArr);
  finally
    fmt.Free;
  end;
end;

class function TPaNDALib.TryAsScalar<T>(const aArr: INDArray; out aValue: T): Boolean;
begin
  Result := TNDAUt.TryAsScalar<T>(aArr, aValue);
end;

class function TPaNDALib.TryAsDynArray<T>(const aArr: INDArray; out aValue: TArray<T>): Boolean;
begin
  Result := TNDAUt.TryAsDynArray<T>(aArr, aValue);
end;

class function TPaNDALib.TryAsDynArray2D<T>(const aArr: INDArray; out aValue: TArray<TArray<T>>): Boolean;
begin
  Result := TNDAUt.TryAsDynArray2D<T>(aArr, aValue);
end;

{$endregion}

initialization

  nda := TPaNDALib.Create;

finalization

  nda.Free;

end.
