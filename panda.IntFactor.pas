unit panda.IntFactor;

interface

uses
    System.Math
  , System.SysUtils
  , System.Generics.Collections
  , panda.NumsQP
  ;

{$I primes.inc}

type
  TFactorUI64 = record
    Value: UInt64;
    Power: Word;
    procedure Init(const aValue: UInt64; aPower: Word); inline;
  end;

  TPrimePredicate = function (const aValue: UInt64): Boolean;

function WheelFactorization(aValue: UInt64; aPrimePred: TPrimePredicate = nil): TArray<TFactorUI64>;
/// <summary>
///   Returns <c>True</c> if <c>aValue</c> is a prime number and <c>False</c> otherwise.
/// </summary>
/// </remarks>
///   For numbers less than 2^16 searches in a prime table else uses Miller-Rapin pseudoprime test.
/// </remarks>
function PrimeQ(const aValue: UInt64): Boolean;

type
  // Linear congruential generator
  TLCG64 = class
  private class var
    seed, K, r, m: TUInt128;
  public
    class constructor Create;
    class procedure SetSeed(const aSeed: UInt64);
    class function Random: UInt64; overload;
    class function Random(const aRange: UInt64): UInt64; overload;
    class function RandomRange(const aFrom, aTo: UInt64): UInt64;
  end;

implementation

{$region 'TFactorUI64'}

procedure TFactorUI64.Init(const aValue: UInt64; aPower: Word);
begin
  Value := aValue;
  Power := aPower;
end;

{$endregion}

function FindTabeledPrime(const aValue: UInt64; out aPos: Integer): Boolean;
var L, R, M: Integer;
    p: Int64;
begin
  L := 0;
  R := cPrimeCount - 1;
  while L < R do begin
    M := (L + R) div 2;
    p := cPrimes[M];
    if cPrimes[M] < aValue then
      L := M + 1
    else
    if p > aValue then
      R := M - 1
    else begin
      aPos := M;
      exit(True);
    end;
  end;
  Result := False;
end;

function FindPrime(const aValue: UInt64): Boolean;
var I: Integer;
begin
  Result := (aValue = 1) or FindTabeledPrime(aValue, I);
end;

function PowerMod(const aX, aExponent, aModulus: UInt64): UInt64;
var Z, Y, m: TUInt128;
    N: UInt64;
    t: Integer;
begin
  Assert(aExponent > 0);

  Y := 1;
  Z := aX;
  m := aModulus;
  N := aExponent;
  while True do begin
    t := N and 1;
    N := N shr 1;
    if t = 1 then Y := (Z * Y) mod m;
    if N = 0 then break;
    Z := (Z * Z) mod m;
  end;
  Result := UInt64(Y);
end;

function MillerRabinTest(const aValue: UInt64): Boolean;
var x, a, d: UInt64;
    I, s: Integer;
    m, x2: TUInt128;
begin
  Assert((aValue > 3) and ((aValue and 1) <> 0));

  s := 0;
  a := aValue - 1;
  while (a and 1) = 0 do begin
    a := a shr 1;
    Inc(s);
  end;
  d := (aValue - 1) div (1 shl s);
  // a = 2^s * d + 1

  m := aValue;
  a := TLCG64.RandomRange(2, aValue - 2);
  x := PowerMod(a, d, aValue);
  if (x = 1) or (x = aValue - 1) then
    exit(True);
  for I := 0 to s - 1 do begin
    x2 := x;
    x2 := (x2 * x2) mod m;
    x := UInt64(x2);
    if x = aValue - 1 then
      exit(True);
  end;
  Result := False;
end;

function PrimeQ(const aValue: UInt64): Boolean;
var I: Integer;
begin
  if (aValue and $FFFFFFFFFFFF0000) = 0 then begin
    Result := FindPrime(aValue);
    exit;
  end;

  if (aValue and 1) = 0 then exit(False);

  I := 30;
  while I > 0 do begin
    if not MillerRabinTest(aValue) then exit(False);
    Dec(I);
  end;

  Result := True;
end;

function WheelFactorization(aValue: UInt64; aPrimePred: TPrimePredicate): TArray<TFactorUI64>;
const incr: array [0..7] of Cardinal = (4, 2, 4, 2, 4, 6, 2, 6);
var factors: TList<TFactorUI64>;
    I, pwr: Integer;
    k, q, r: UInt64;
    f: TFactorUI64;
begin
  if not Assigned(aPrimePred) then aPrimePred := PrimeQ;

  factors := TList<TFactorUI64>.Create;
  try
    pwr := 0;
    while (aValue and 1) = 0 do begin
      aValue := aValue shr 1;
      Inc(pwr);
    end;
    if pwr > 0 then begin
      f.Init(2, pwr);
      factors.Add(f)
    end;

    pwr := 0;
    while True do begin
      DivMod(aValue, 3, q, r);
      if r = 0 then begin
        aValue := q;
        Inc(pwr);
      end else
        break;
    end;
    if pwr > 0 then begin
      f.Init(3, pwr);
      factors.Add(f);
    end;

    pwr := 0;
    while True do begin
      DivMod(aValue, 5, q, r);
      if r = 0 then begin
        aValue := q;
        Inc(pwr);
      end else
        break;
    end;
    if pwr > 0 then begin
      f.Init(5, pwr);
      factors.Add(f);
    end;

    k := 7;
    i := 0;
    while k * k <= aValue do begin
      pwr := 0;
      while True do begin
        DivMod(aValue, k, q, r);
        if r = 0 then begin
          aValue := q;
          Inc(pwr);
        end else
          break;
      end;
      if pwr > 0 then begin
        f.Init(k, pwr);
        factors.Add(f);
        if aPrimePred(aValue) then break;
      end;
      Inc(k, incr[I]);
      if I < 7 then
        Inc(I)
      else
        I := 0;
    end;

    if aValue > 1 then begin
      f.Init(aValue, 1);
      factors.Add(f);
    end;

    Result := factors.ToArray;
  finally
    factors.Free;
  end;
end;

{$region 'TLCG64'}

class constructor TLCG64.Create;
var s: UInt64;
    sarr: array [0..1] of Cardinal absolute s;
begin
  sarr[0] := System.Random(Integer.MaxValue);
  sarr[1] := System.Random(Integer.MaxValue);

  seed := s;
  K := 6364136223846793005;
  r := 1442695040888963407;
  m := $FFFFFFFFFFFFFFFF;
end;

class procedure TLCG64.SetSeed(const aSeed: UInt64);
begin
  seed := aSeed;
end;

class function TLCG64.Random: UInt64;
begin
  seed := (seed * k + r) and m;
  Move(seed, Result, SizeOf(UInt64));
end;

class function TLCG64.Random(const aRange: UInt64): UInt64;
var tmp: TUInt128;
begin
  seed := (seed * k + r) and m;
  tmp := (aRange * seed) shr 64;
  Move(tmp, Result, SizeOf(UInt64));
end;

class function TLCG64.RandomRange(const aFrom, aTo: UInt64): UInt64;
begin
  if aFrom > aTo then
    Result := Random(aFrom - aTo) + ATo
  else
    Result := Random(aTo - aFrom) + AFrom;
end;

{$endregion}

end.
