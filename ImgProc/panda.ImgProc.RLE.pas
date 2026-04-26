unit panda.ImgProc.RLE;

interface

uses
    System.SysUtils
  ;

const
  RLE_END_OF_LINE       = $0000;
  RLE_END_OF_BITMAP     = $0001;
  RLE_DELTA             = $0002;

  RLEF_EOL              = $01;
  RLEF_EOB              = $02;

function RLE8Encode(pSrc: PByte; pDst: PByte; aCount: Integer; aFlags: Integer = 0): Integer;

type
  TRLE8ImageWriter = record
  private
    fW: Integer;
    fBuffer: TBytes;
    fPos: NativeInt;
    function GetBytes: TBytes;
    function GetData: PByte;
    procedure CheckBufferSize(aNeeded: Integer);
  public
    procedure Init(aW, aH: Integer; aDefault: Byte = 0);
    procedure WriteRow(pRow: PByte; aCount: Integer = -1; aX: Integer = -1);
    procedure WriteEndOfImage;

    property Bytes: TBytes read GetBytes;
    property Data: PByte read GetData;
    property ByteCount: NativeInt read fPos;
  end;


implementation

function RLE8Encode(pSrc: PByte; pDst: PByte; aCount: Integer; aFlags: Integer): Integer;
 var pDstOrig, pEnd: PByte;
    absMode: Boolean;
    count, value, b: Byte;
begin
  Assert(aCount > 0);

  pDstOrig := pDst;
  pEnd := pSrc + aCount;
  absMode := False;
  count := 0;
  value := pSrc^;
  while pSrc < pEnd do begin
    b := pSrc^;
    if value = b then begin
      if absMode then begin
        if count > 3 then begin
          PWord(pDst)^ := (count - 1) shl 8;
          Inc(pDst, 2);
          Move((pSrc - count)^, pDst^, count - 1);
          Inc(pDst, count - 1);
          if (count and 1) = 0 then begin
            pDst^ := 0;
            Inc(pDst);
          end;
        end else begin
          if count = 3 then begin
            PInteger(pDst)^ := $010001 or ((pSrc - 3)^ shl 8) or ((pSrc - 2)^ shl 24);
            Inc(pDst, 4);
          end else begin
            PWord(pDst)^ := $1 or ((pSrc - 2)^ shl 8);
            inc(pDst, 2);
          end;
        end;
        absMode := False;
        count := 2;
      end else begin
        Inc(count);
        if count = $FF then begin
          PWord(pDst)^ := $FF or (value shl 8);
          Inc(pDst, 2);
          count := 0;
        end;
      end;
    end
    else begin // c.Value <> b
      if absMode then begin
        Inc(count);
        if count = $FF then begin
          PWord(pDst)^ := $FF00;
          Inc(pDst, 2);
          Move((pSrc - $FE)^, pDst^, $FF);
          Inc(pDst, $FF);
          pDst^ := 0;
          Inc(pDst);
          absMode := False;
          count := 0;
        end;
        value := b;
      end else begin // not absolute mode
        if count > 1 then begin
          PWord(pDst)^ := count or (value shl 8);
          Inc(pDst, 2);
          count := 1;
          value := b;
        end else begin
          absMode := True;
          Inc(count);
          value := b;
        end;
      end;
    end;
    Inc(pSrc);
  end;

  if count > 0 then begin
    if absMode then begin
      if count > 2 then begin
        PWord(pDst)^ := (count shl 8);
        Inc(pDst, 2);
        Move((pSrc - count)^, pDst^, count);
        Inc(pDst, count);
        if (count and 1) <> 0 then begin
          pDst^ := 0;
          Inc(pDst);
        end;
      end else begin
        if count = 2 then begin
          PInteger(pDst)^ := $010001 or ((pSrc - 2)^ shl 8) or ((pSrc - 1)^ shl 24);
          Inc(pDst, 4);
        end else begin
          PWord(pDst)^ := 1 or ((pSrc - 1)^ shl 8);
          Inc(pDst, 2);
        end;
      end;
    end else begin
      PWord(pDst)^ := count or (value shl 8);
      Inc(pDst, 2);
    end;
  end;

  if (aFlags and RLEF_EOL) <> 0 then begin
    PWord(pDst)^ := RLE_END_OF_LINE;
    Inc(pDst, 2);
  end;
  if (aFlags and RLEF_EOB) <> 0 then begin
    PWord(pDst)^ := RLE_END_OF_BITMAP;
    Inc(pDst, 2);
  end;

  Result := pDst - pDstOrig;
end;

{$region 'TRLE8ImageWriter'}

procedure TRLE8ImageWriter.Init(aW, aH: Integer; aDefault: Byte);
begin
  Assert(aW > 0);
  if aH <= 0 then
    SetLength(fBuffer, 2 * aW)
  else
    SetLength(fBuffer, aW * aH);

  fW := aW;
  fPos := 0;
end;

procedure TRLE8ImageWriter.CheckBufferSize(aNeeded: Integer);
begin
  if (Length(fBuffer) - fPos) < aNeeded then
    SetLength(fBuffer, 2 * Length(fBuffer));
end;

procedure TRLE8ImageWriter.WriteRow(pRow: PByte; aCount, aX: Integer);
var bCnt: Integer;
begin
  Assert(aCount < fW);
  if aCount < 0 then aCount := fW;

  CheckBufferSize(2 * aCount);
  bCnt := RLE8Encode(pRow, @fBuffer[fPos], aCount, RLEF_EOL);
  Inc(fPos, bCnt);
end;

procedure TRLE8ImageWriter.WriteEndOfImage;
begin
  CheckBufferSize(2);
  fBuffer[fPos] := 0;
  fBuffer[fPos + 1] := 1;
  Inc(fPos, 2);
end;

function TRLE8ImageWriter.GetBytes: TBytes;
begin
  SetLength(Result, fPos);
  Move(fBuffer[0], Result[0], fPos);
end;

function TRLE8ImageWriter.GetData: PByte;
begin
  Result := PByte(fBuffer);
end;

{$endregion}

end.
