unit panda.ImgProc.Tests.RLE;

interface

uses
    TestFramework
  , System.SysUtils
  , panda.ImgProc.RLE
  , panda.Tests.NDATestCase
  ;

type
  TRLETests = class(TNDATestCase)
  published
    procedure Encode_ShortSequences;
    procedure Encode_AbsMode_OddLen;
    procedure Encode_AbsMode_EvenLen;
    procedure Encode_TwoDiffItems;
    procedure Encode_ShortAbsMode;
    procedure Encode_SeqAbs;
    procedure Encode_AbsSeq;
    procedure Encode_SeqAbsSeq;
    procedure Encode_LongSeq;
    procedure Encode_LongSeqSeq;
    procedure Encode_LongAbsSeq;
  end;

implementation

{$region 'TRLETest'}

procedure TRLETests.Encode_ShortSequences;
var src, dst: TBytes;
    count: Integer;
begin
  src := TBytes.Create(3, 3, 3, 3, 3, 5, 5, 5);
  SetLength(dst, 2 * SizeOf(src));
  count := RLE8Encode(PByte(src), PByte(dst), Length(src));
  CheckEquals(4, count);
  CheckEquals(5, dst[0]);
  CheckEquals(3, dst[1]);
  CheckEquals(3, dst[2]);
  CheckEquals(5, dst[3]);
end;

procedure TRLETests.Encode_AbsMode_OddLen;
var src, dst: TBytes;
    count: Integer;
begin
  src := TBytes.Create(1, 2, 3);
  SetLength(dst, 2 * Length(src));
  count := RLE8Encode(PByte(src), PByte(dst), Length(src));
  CheckEquals(6, count);
  CheckEquals(0, dst[0]);
  CheckEquals(3, dst[1]);
  CheckEquals(1, dst[2]);
  CheckEquals(2, dst[3]);
  CheckEquals(3, dst[4]);
  CheckEquals(0, dst[5]);
end;

procedure TRLETests.Encode_AbsMode_EvenLen;
var src, dst: TBytes;
    count: Integer;
begin
  src := TBytes.Create(1, 2, 3, 4);
  SetLength(dst, 2 * Length(src));
  count := RLE8Encode(PByte(src), PByte(dst), Length(src));
  CheckEquals(6, count);
  CheckEquals(0, dst[0]);
  CheckEquals(4, dst[1]);
  CheckEquals(1, dst[2]);
  CheckEquals(2, dst[3]);
  CheckEquals(3, dst[4]);
  CheckEquals(4, dst[5]);
end;

procedure TRLETests.Encode_TwoDiffItems;
var src, dst: TBytes;
    count: Integer;
begin
  src := TBytes.Create(5, 6);
  SetLength(dst, 2 * Length(src));
  count := RLE8Encode(PByte(src), PByte(dst), Length(src));
  CheckEquals(4, count);
  CheckEquals(1, dst[0]);
  CheckEquals(5, dst[1]);
  CheckEquals(1, dst[2]);
  CheckEquals(6, dst[3]);
end;

procedure TRLETests.Encode_ShortAbsMode;
var src, dst: TBytes;
    count: Integer;
begin
  src := TBytes.Create(5, 6, 2, 2);
  SetLength(dst, 6);
  count := RLE8Encode(PByte(src), PByte(dst), Length(src));
  CheckEquals(6, count);
  CheckEquals(1, dst[0]);
  CheckEquals(5, dst[1]);
  CheckEquals(1, dst[2]);
  CheckEquals(6, dst[3]);
  CheckEquals(2, dst[4]);
  CheckEquals(2, dst[5]);
end;

procedure TRLETests.Encode_SeqAbs;
var src, dst: TBytes;
    count: Integer;
begin
  src := TBytes.Create(2, 2, 3, 4, 5);
  SetLength(dst, 2 * Length(src));
  count := RLE8Encode(PByte(src), PByte(dst), Length(src));
  CheckEquals(8, count);
  CheckEquals(2, dst[0]);
  CheckEquals(2, dst[1]);
  CheckEquals(0, dst[2]);
  CheckEquals(3, dst[3]);
  CheckEquals(3, dst[4]);
  CheckEquals(4, dst[5]);
  CheckEquals(5, dst[6]);
  CheckEquals(0, dst[7]);
end;

procedure TRLETests.Encode_AbsSeq;
var src, dst: TBytes;
    count: Integer;
begin
  src := TBytes.Create(1, 2, 3, 4, 5, 5);
  SetLength(dst, 2 * Length(src));
  count := RLE8Encode(PByte(src), PByte(dst), Length(src));
  CheckEquals(8, count);
  CheckEquals(0, dst[0]);
  CheckEquals(4, dst[1]);
  CheckEquals(1, dst[2]);
  CheckEquals(2, dst[3]);
  CheckEquals(3, dst[4]);
  CheckEquals(4, dst[5]);
  CheckEquals(2, dst[6]);
  CheckEquals(5, dst[7]);

  src := TBytes.Create(1, 2, 3, 5, 5);
  SetLength(dst, 2 * Length(src));
  count := RLE8Encode(PByte(src), PByte(dst), Length(src));
  CheckEquals(8, count);
  CheckEquals(0, dst[0]);
  CheckEquals(3, dst[1]);
  CheckEquals(1, dst[2]);
  CheckEquals(2, dst[3]);
  CheckEquals(3, dst[4]);
  CheckEquals(0, dst[5]);
  CheckEquals(2, dst[6]);
  CheckEquals(5, dst[7]);
end;

procedure TRLETests.Encode_SeqAbsSeq;
var src, dst: TBytes;
    count: Integer;
begin
  src := TBytes.Create(1, 1, 2, 3, 4, 5, 5);
  SetLength(dst, 2 * Length(src));
  count := RLE8Encode(PByte(src), PByte(dst), Length(src));
  CheckEquals(10, count);
  CheckEquals(2, dst[0]);
  CheckEquals(1, dst[1]);
  CheckEquals(0, dst[2]);
  CheckEquals(3, dst[3]);
  CheckEquals(2, dst[4]);
  CheckEquals(3, dst[5]);
  CheckEquals(4, dst[6]);
  CheckEquals(0, dst[7]);
  CheckEquals(2, dst[8]);
  CheckEquals(5, dst[9]);
end;

procedure TRLETests.Encode_LongSeq;
var src, dst: TBytes;
    count: Integer;
begin
  SetLength(src, 256);
  FillChar(src[0], Length(src), 3);
  SetLength(dst, 8);
  count := RLE8Encode(PByte(src), PByte(dst), Length(src));
  CheckEquals(4, count);
  CheckEquals(255, dst[0]);
  CheckEquals(3, dst[1]);
  CheckEquals(1, dst[2]);
  CheckEquals(3, dst[3]);
end;

procedure TRLETests.Encode_LongSeqSeq;
var src, dst: TBytes;
    count: Integer;
begin
  SetLength(src, 258);
  FillChar(src[0], 256, 3);
  FillChar(src[256], 2, 5);
  SetLength(dst, 8);
  count := RLE8Encode(PByte(src), PByte(dst), Length(src));
  CheckEquals(6, count);
  CheckEquals(255, dst[0]);
  CheckEquals(3, dst[1]);
  CheckEquals(1, dst[2]);
  CheckEquals(3, dst[3]);
  CheckEquals(2, dst[4]);
  CheckEquals(5, dst[5]);
end;

procedure TRLETests.Encode_LongAbsSeq;
var src, dst: TBytes;
    I, J, count: Integer;
begin
  SetLength(src, 256);
  for I := 0 to High(src) do
    src[I] := 1 + (I mod 3);
  SetLength(dst, 270);

  count := RLE8Encode(PByte(src), PByte(dst), 255);
  CheckEquals(258, count);
  CheckEquals(0, dst[0]);
  CheckEquals(255, dst[1]);
  J := 2;
  for I := 0 to 254 do begin
    CheckEquals(src[I], dst[J]);
    Inc(J);
  end;
  CheckEquals(0, dst[257]);

  FillChar(dst[0], Length(dst), 0);
  count := RLE8Encode(PByte(src), PByte(dst), Length(src));
  CheckEquals(260, count);
  CheckEquals(0, dst[0]);
  CheckEquals(255, dst[1]);
  J := 2;
  for I := 0 to 254 do begin
    CheckEquals(src[I], dst[J]);
    Inc(J);
  end;
  CheckEquals(0, dst[257]);
  CheckEquals(1, dst[258]);
  CheckEquals(src[High(src)], dst[259]);
end;

{$endregion}

initialization

  RegisterTest(TRLETests.Suite);

end.
