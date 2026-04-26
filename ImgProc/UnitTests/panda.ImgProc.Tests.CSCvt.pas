unit panda.ImgProc.Tests.CSCvt;

interface

uses
    TestFramework
  , System.Math
  , panda.Intfs
  , panda.Arrays
  , panda.ArrManip
  , panda.ImgProc.Types
  , panda.ImgProc.CSCvt
  , panda.ImgProc.Images
  , panda.Tests.NDATestCase
  ;

type
  TCSCvtTests = class(TNDATestCase)
  protected const
    sTol = 1e-6;
  published
    procedure TestRGB24ToUI8_2;
    procedure TestRGB24ToUI8_4;
    procedure TestRGB24ToF32_2;
    procedure TestRGB24ToF32_4;
    procedure TestUI8ToRGB24_16;
    procedure TestUI8ToRGB24_18;
    procedure TestUI8ToF32_8;
    procedure TestUI8ToF32_10;
    procedure TestF32ToUI8_3;
    procedure TestF32ToUI8_8;
    procedure TestF32ToUI8_10;
  end;

implementation

{$region 'TCSCvtTests'}

procedure TCSCvtTests.TestRGB24ToUI8_2;
var src: TArray<TRGB24>;
    dst: TArray<Byte>;
    I: Integer;
begin
  src := TArray<TRGB24>.Create(RGB24(1, 2, 3), RGB24(4, 5, 6));
  SetLength(dst, Length(src));

  cscvtRGB24ToUI8(PByte(src), PByte(dst), Length(src));

  for I := 0 to High(dst) do
    CheckEquals(Round(0.299 * src[I].R + 0.587 * src[I].G + 0.114 * src[I].B), dst[I]);
end;

procedure TCSCvtTests.TestRGB24ToUI8_4;
var src: TArray<TRGB24>;
    dst: TArray<Byte>;
    I: Integer;
begin
  src := TArray<TRGB24>.Create(RGB24(1, 2, 3), RGB24(4, 5, 6), RGB24(7, 8, 9), RGB24(10, 11, 12));
  SetLength(dst, Length(src));

  cscvtRGB24ToUI8(PByte(src), PByte(dst), Length(src));

  for I := 0 to High(dst) do
    CheckEquals(Round(0.299 * src[I].R + 0.587 * src[I].G + 0.114 * src[I].B), dst[I]);
end;

procedure TCSCvtTests.TestRGB24ToF32_2;
var src: TArray<TRGB24>;
    dst: TArray<Single>;
    I: Integer;
begin
  src := TArray<TRGB24>.Create(RGB24(1, 2, 3), RGB24(4, 5, 6));
  SetLength(dst, Length(src));

  cscvtRGB24ToF32(PByte(src), PByte(dst), Length(src));

  for I := 0 to High(dst) do
    CheckEquals((0.299 * src[I].R + 0.587 * src[I].G + 0.114 * src[I].B) / 255, dst[I], sTol);
end;

procedure TCSCvtTests.TestRGB24ToF32_4;
var src: TArray<TRGB24>;
    dst: TArray<Single>;
    I: Integer;
begin
  src := TArray<TRGB24>.Create(RGB24(1, 2, 3), RGB24(4, 5, 6), RGB24(7, 8, 9), RGB24(10, 11, 12));
  SetLength(dst, Length(src));

  cscvtRGB24ToF32(PByte(src), PByte(dst), Length(src));

  for I := 0 to High(dst) do
    CheckEquals((0.299 * src[I].R + 0.587 * src[I].G + 0.114 * src[I].B) / 255, dst[I], sTol);
end;

procedure TCSCvtTests.TestUI8ToRGB24_16;
var src: TArray<Byte>;
    dst: TArray<TRGB24>;
    I: Integer;
begin
  src := TArray<Byte>.Create(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
  SetLength(dst, Length(src));

  cscvtUI8ToRGB24(PByte(src), PByte(dst), Length(src));

  for I := 0 to High(dst) do begin
    CheckEquals(I, dst[I].R);
    CheckEquals(I, dst[I].G);
    CheckEquals(I, dst[I].B);
  end;
end;

procedure TCSCvtTests.TestUI8ToRGB24_18;
var src: TArray<Byte>;
    dst: TArray<TRGB24>;
    I: Integer;
begin
  src := TArray<Byte>.Create(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17);
  SetLength(dst, Length(src));

  cscvtUI8ToRGB24(PByte(src), PByte(dst), Length(src));

  for I := 0 to High(dst) do begin
    CheckEquals(I, dst[I].R);
    CheckEquals(I, dst[I].G);
    CheckEquals(I, dst[I].B);
  end;
end;

procedure TCSCvtTests.TestUI8ToF32_8;
var src: TArray<Byte>;
    dst: TArray<Single>;
    I: Integer;
begin
  src := TArray<Byte>.Create(0, 1, 2, 3, 4, 5, 6, 7);
  SetLength(dst, Length(src));

  cscvtUI8ToF32(PByte(src), PByte(dst), Length(src));

  for I := 0 to High(dst) do
    CheckEquals(I/255, dst[I], sTol);
end;

procedure TCSCvtTests.TestUI8ToF32_10;
var src: TArray<Byte>;
    dst: TArray<Single>;
    I: Integer;
begin
  src := TArray<Byte>.Create(0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
  SetLength(dst, Length(src));

  cscvtUI8ToF32(PByte(src), PByte(dst), Length(src));

  for I := 0 to High(dst) do
    CheckEquals(I/255, dst[I], sTol);
end;

procedure TCSCvtTests.TestF32ToUI8_3;
var src: TArray<Single>;
    dst: TArray<Byte>;
    I: Integer;
begin
  src := TArray<Single>.Create(-1, 0.5, 2);
  SetLength(dst, Length(src));

  cscvtF32ToUI8(PByte(src), PByte(dst), Length(src));

  for I := 0 to High(dst) do
    CheckEquals(Round(Max(0, Min(255, 255*src[I]))), dst[I]);
end;

procedure TCSCvtTests.TestF32ToUI8_8;
var src: TArray<Single>;
    dst: TArray<Byte>;
    I: Integer;
begin
  src := TArray<Single>.Create(-1, 0, 0.5, 1, 2, 1, 0.5, 0);
  SetLength(dst, Length(src));

  cscvtF32ToUI8(PByte(src), PByte(dst), Length(src));

  for I := 0 to High(dst) do
    CheckEquals(Round(Max(0, Min(255, 255*src[I]))), dst[I]);
end;

procedure TCSCvtTests.TestF32ToUI8_10;
var src: TArray<Single>;
    dst: TArray<Byte>;
    I: Integer;
begin
  src := TArray<Single>.Create(-1, 0, 0.5, 1, 2, 1, 0.5, 0, 0.5, 4);
  SetLength(dst, Length(src));

  cscvtF32ToUI8(PByte(src), PByte(dst), Length(src));

  for I := 0 to High(dst) do
    CheckEquals(Round(Max(0, Min(255, 255*src[I]))), dst[I]);
end;

{$endregion}

initialization

  RegisterTest(TCSCvtTests.Suite);

end.
