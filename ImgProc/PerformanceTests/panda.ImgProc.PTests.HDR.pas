unit panda.ImgProc.PTests.HDR;

interface

uses
    TestFramework
  , panda.Intfs
  , panda.Arrays
  , panda.ImgProc.Types
  , panda.ImgProc.Images
  , panda.ImgProc.HDR
  , panda.MAT4io
  , panda.Tests.NDATestCase
  , System.IOUtils
  ;

type
  THDRTests = class(TNDAPerformanceTestCase)
  published
    procedure DebevecResponse_Mono8;
    procedure LowLvlRowRMCombine2;
    procedure LowLvlRowRMCombine3;
    procedure LowLvlRowRMCombine;
  end;

implementation

{$region 'THDRTests'}

procedure THDRTests.DebevecResponse_Mono8;
var importer: TMAT4Importer;
    arr: INDArray<Double>;
    darr: TArray<TArray<Double>>;
    intensity: TArray<TArray<Byte>>;
    logExp: TArray<Double>;
    I, J: Integer;
begin
  importer := TMAT4Importer.Create(GetTestDataPath('DebevecHDR.mat'));
  try
    if not (
      importer.TryRead<Double>(0, arr) and
      TNDAUt.TryAsDynArray2D<Double>(arr, darr))
    then
      Fail('Data import failed.');

    SetLength(intensity, Length(darr));
    for I := 0 to High(darr) do begin
      SetLength(intensity[I], Length(darr[I]));
      for J := 0 to High(darr[I]) do
        intensity[I, J] := Round(darr[I, J]);
    end;

    if not (
      importer.TryRead<Double>(1, arr) and
      TNDAUt.TryAsDynArray2D<Double>(arr, darr) and (Length(darr) = 1))
    then
      Fail('Data import failed.');

    logExp := darr[0];
  finally
    importer.Free;
  end;

  SWStart;
  DebevecResponseCurve(intensity, logExp, 100, HatWeight);
  SWStop;
end;

procedure THDRTests.LowLvlRowRMCombine2;
var src: TArray<PByte>;
    srcbuff: TArray<Byte>;
    dstbuff: TArray<Single>;
    g, logTs, w: TArray<Single>;
    I: Integer;
const Cnt = 1000000;
begin
  SetLength(srcbuff, 2 * Cnt);
  SetLength(dstbuff, Cnt);
  SetLength(src, 2);
  src[0] := @srcbuff[0];
  src[1] := @srcbuff[Cnt];
  SetLength(g, 256);
  SetLength(w, 256);
  for I := 0 to High(g) do begin
    g[I] := I;
    w[I] := 1;
  end;
  logTs := TArray<Single>.Create(1, 2);

  SWStart;
  _RMCombine2(src, PSingle(dstbuff), g, logTs, w, Cnt);
  SWStop;
end;

procedure THDRTests.LowLvlRowRMCombine3;
var src: TArray<PByte>;
    srcbuff: TArray<Byte>;
    dstbuff: TArray<Single>;
    g, logTs, w: TArray<Single>;
    I: Integer;
const Cnt = 1000000;
begin
  SetLength(srcbuff, 3 * Cnt);
  SetLength(dstbuff, Cnt);
  SetLength(src, 3);
  src[0] := @srcbuff[0];
  src[1] := @srcbuff[Cnt];
  src[2] := @srcbuff[2*Cnt];
  SetLength(g, 256);
  SetLength(w, 256);
  for I := 0 to High(g) do begin
    g[I] := I;
    w[I] := 1;
  end;
  logTs := TArray<Single>.Create(1, 2, 4);

  SWStart;
  _RMCombine3(src, PSingle(dstbuff), g, logTs, w, Cnt);
  SWStop;
end;

procedure THDRTests.LowLvlRowRMCombine;
var src: TArray<PByte>;
    srcbuff: TArray<Byte>;
    dstbuff: TArray<Single>;
    g, logTs, w: TArray<Single>;
    I: Integer;
const Cnt = 1000000;
begin
  SetLength(srcbuff, 3 * Cnt);
  SetLength(dstbuff, Cnt);
  SetLength(src, 3);
  src[0] := @srcbuff[0];
  src[1] := @srcbuff[Cnt];
  src[2] := @srcbuff[2*Cnt];
  SetLength(g, 256);
  SetLength(w, 256);
  for I := 0 to High(g) do begin
    g[I] := I;
    w[I] := 1;
  end;
  logTs := TArray<Single>.Create(1, 2, 4);

  SWStart;
  _RMCombine(src, PSingle(dstbuff), g, logTs, w, Cnt);
  SWStop;
end;

{$endregion}

initialization

  RegisterTest(THDRTests.Suite);

end.
