unit panda.Sorting;

interface

uses
    panda.Intfs
  , panda.Arrays
  , panda.Consts
  , System.SysUtils
  , System.Math
  ;

type
  // some specialized method for sorting double arrays without using a comparer
  TSortUtF64 = record
  public
    class function BinarySearch(const Values: array of Double; const Item: Double;
      out FoundIndex: NativeInt; Index, Count: NativeInt): Boolean; overload; static;
    class function BinarySearch(const Values: array of Double; const Item: Double;
      out FoundIndex: NativeInt): Boolean; overload; static;
    class function SearchPos(const data: array of Double; const Value: Double; out Pos: NativeInt;
      StartPos: NativeInt = 0; EndPos: NativeInt = -1) : boolean; static;

    class procedure QuickSort(var Values: array of Double; L, R: NativeInt); overload; static;
    class procedure QuickSort(var Values: array of Double); overload; static;
    class function MergeSort(const Values: array of Double): TArray<Double>; static;
  end;

implementation

class function TSortUtF64.BinarySearch(const Values: array of Double; const Item: Double;
  out FoundIndex: NativeInt; Index, Count: NativeInt): Boolean;
var L, H, mid: NativeInt;
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

class function TSortUtF64.BinarySearch(const Values: array of Double; const Item: Double;
  out FoundIndex: NativeInt): Boolean;
begin
  Result := BinarySearch(Values, Item, FoundIndex, Low(Values), Length(Values));
end;

class function TSortUtF64.SearchPos(const data: array of Double; const Value: Double;
  out Pos: NativeInt; StartPos, EndPos: NativeInt): Boolean;
var I, J, K : NativeInt;
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

class procedure TSortUtF64.QuickSort(var Values: array of Double; L, R: NativeInt);
var I, J: NativeInt;
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

class procedure TSortUtF64.QuickSort(var Values: array of Double);
begin
  QuickSort(Values, 0, High(Values));
end;

procedure Merge(pL, pR, pRes: PDouble; aLCnt, aRCnt: NativeInt);
var pLEnd, pREnd: PByte;
begin
  pLEnd := PByte(pL) + aLCnt * SizeOf(Double);
  pREnd := PByte(pR) + aRCnt * SizeOf(Double);
  while (PByte(pL) < pLEnd) and (PByte(pR) < pREnd) do begin
    if pL^ <= pR^ then begin
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

class function TSortUtF64.MergeSort(const Values: array of Double): TArray<Double>;
var A, B, tmp: TArray<Double>;
    I, step, step2, count, cnt, rCnt: NativeInt;
    pL, pR, pRes: PDouble;
    pEnd: PByte;
begin
  count := Length(Values);
  if count = 1 then begin
    SetLength(Result, 1);
    Result[0] := Values[0];
    exit;
  end;

  SetLength(A, count);
  SetLength(B, count);
  step := 2;
  cnt := (count div step) * step;
  I := 0;
  while I < cnt do begin
    if Values[I] <= Values[I + 1] then begin
      A[I] := Values[I];
      A[I + 1] := Values[I + 1];
    end else begin
      A[I] := Values[I + 1];
      A[I + 1] := Values[I];
    end;
    Inc(I, 2);
  end;
  if cnt < count then
    A[cnt] := Values[cnt];

  while step < count do begin
    pL := PDouble(@A[0]);
    pR := PDouble(@A[step]);
    pRes := PDouble(B);
    step2 := 2 * step;
    cnt := (count div step2) * step2;
    pEnd := PByte(pL) + cnt * SizeOf(Double);
    while PByte(pL) < pEnd do begin
      Merge(pL, pR, pRes, step, step);
      Inc(pRes, step2);
      Inc(pL, step2);
      Inc(pR, step2);
    end;
    rCnt := count - cnt;
    if rCnt > step then
      Merge(pL, pR, pRes, step, rCnt - step)
    else
    if rCnt > 0 then    
      Move(A[cnt], B[cnt], rCnt * SizeOf(Double));
    step := step2;
    tmp := A;
    A := B;
    B := tmp;
  end;
  Result := A;
end;

end.
