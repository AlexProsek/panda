unit panda.ImgProc.Tests.HDR;

interface

uses
    TestFramework
  , panda.Intfs
  , panda.Arrays
  , panda.ArrManip
  , panda.ImgProc.Types
  , panda.ImgProc.Images
  , panda.ImgProc.HDR
  , panda.Tests.NDATestCase
  ;

type
  THDRTests = class(TNDATestCase)
  protected const
    sTol = 1e-5;
  protected
    procedure CheckInvalidResponse(const aSamples: TArray<TArray<Byte>>;
      const aExposures: TArray<Double>; aWeight: TWeightingFunction);
  published
    procedure DebevecLinearResponse;
    procedure DebevecEmptySamples;
    procedure DebevecEmptyExposures;
    procedure DebevecNilWeight;
    procedure DebevecMismatchedSamples;
    procedure DebevecInsufficientEquations;
    procedure DebevecRankDeficient;
    procedure DebevecZeroSmoothing;
    procedure LowLvlRMCombine3_2;
    procedure LowLvlRMCombine3_4;
    procedure LowLvlRMCombine3_10;
    procedure LowLvlRMCombine_2;
    procedure LowLvlRMCombine_4;
    procedure LowLvlRMCombine_10;
  end;

implementation

uses
    System.SysUtils
  ;

function UniformWeight(const aValue: Double): Double;
begin
  Result := 1;
end;

{$region 'THDRTests'}

procedure THDRTests.CheckInvalidResponse(const aSamples: TArray<TArray<Byte>>;
  const aExposures: TArray<Double>; aWeight: TWeightingFunction);
var raised: Boolean;
begin
  raised := False;

  try
    DebevecResponseCurve(aSamples, aExposures, 1, aWeight);
  except
    on E: EAssertionFailed do raised := True;
  end;

  CheckTrue(raised, 'Expected EAssertionFailed');
end;

procedure THDRTests.DebevecLinearResponse;
var samples: TArray<TArray<Byte>>;
    exposures: TArray<Double>;
    curve: TArray<Single>;
    I: Integer;
begin
  samples := TArray<TArray<Byte>>.Create(
    TArray<Byte>.Create(64, 192),
    TArray<Byte>.Create(96, 224)
  );
  exposures := TArray<Double>.Create(-1, 1);

  curve := DebevecResponseCurve(samples, exposures, 1, UniformWeight);

  CheckEquals(256, Length(curve));
  CheckEquals(0, curve[128], sTol);
  for I := 0 to 255 do
    CheckEquals((I - 128) / 64, curve[I], sTol);
  CheckEquals(64, samples[0, 0]);
  CheckEquals(192, samples[0, 1]);
  CheckEquals(96, samples[1, 0]);
  CheckEquals(224, samples[1, 1]);
  CheckEquals(-1, exposures[0], 0);
  CheckEquals(1, exposures[1], 0);
end;

procedure THDRTests.DebevecEmptySamples;
begin
  CheckInvalidResponse(nil, TArray<Double>.Create(-1, 1), UniformWeight);
end;

procedure THDRTests.DebevecEmptyExposures;
begin
  CheckInvalidResponse(TArray<TArray<Byte>>.Create(
    TArray<Byte>.Create(64, 192)), nil, UniformWeight);
end;

procedure THDRTests.DebevecNilWeight;
begin
  CheckInvalidResponse(TArray<TArray<Byte>>.Create(
    TArray<Byte>.Create(64, 192)), TArray<Double>.Create(-1, 1), nil);
end;

procedure THDRTests.DebevecMismatchedSamples;
begin
  CheckInvalidResponse(
    TArray<TArray<Byte>>.Create(
      TArray<Byte>.Create(64, 192),
      TArray<Byte>.Create(96)
    ),
    TArray<Double>.Create(-1, 1), UniformWeight
  );
end;

procedure THDRTests.DebevecInsufficientEquations;
begin
  CheckInvalidResponse(TArray<TArray<Byte>>.Create(
    TArray<Byte>.Create(64)), TArray<Double>.Create(-1), UniformWeight);
end;

procedure THDRTests.DebevecRankDeficient;
var samples: TArray<TArray<Byte>>;
    exposures: TArray<Double>;
    raised: Boolean;
begin
  samples := TArray<TArray<Byte>>.Create(TArray<Byte>.Create(64, 64));
  exposures := TArray<Double>.Create(-1, 1);
  raised := False;

  try
    DebevecResponseCurve(samples, exposures, 1, UniformWeight);
  except
    on E: EAssertionFailed do raised := True;
  end;

  CheckTrue(raised, 'Expected EAssertionFailed');
end;

procedure THDRTests.DebevecZeroSmoothing;
var samples: TArray<TArray<Byte>>;
    exposures: TArray<Double>;
    raised: Boolean;
begin
  samples := TArray<TArray<Byte>>.Create(TArray<Byte>.Create(64, 192));
  exposures := TArray<Double>.Create(-1, 1);
  raised := False;

  try
    DebevecResponseCurve(samples, exposures, 0, UniformWeight);
  except
    on E: EAssertionFailed do raised := True;
  end;

  CheckTrue(raised, 'Expected EAssertionFailed');
end;

procedure THDRTests.LowLvlRMCombine3_2;
var src: TArray<PByte>;
    srcbuff: TArray<Byte>;
    dstbuff: TArray<Single>;
    g, logTs, w: TArray<Single>;
    I: Integer;
const Cnt = 2;
begin
  srcbuff := TArray<byte>.Create(1, 2, 3, 4, 5, 6);
  SetLength(dstbuff, Cnt);
  SetLength(src, 3);
  src[0] := @srcbuff[0];
  src[1] := @srcbuff[Cnt];
  src[2] := @srcbuff[2*Cnt];
  SetLength(g, 256);
  SetLength(w, 256);
  for I := 0 to High(g) do begin
    g[I] := I;
    w[I] := I/255;
  end;
  logTs := TArray<Single>.Create(1, 2, 4);

  _RMCombine3(src, PSingle(dstbuff), g, logTs, w, Cnt);

  CheckEquals([0.8888889, 1.83333337], dstbuff, sTol);
end;

procedure THDRTests.LowLvlRMCombine3_4;
var src: TArray<PByte>;
    srcbuff: TArray<Byte>;
    dstbuff: TArray<Single>;
    g, logTs, w: TArray<Single>;
    I: Integer;
const Cnt = 4;
begin
  srcbuff := TArray<byte>.Create(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12);
  SetLength(dstbuff, Cnt);
  SetLength(src, 3);
  src[0] := @srcbuff[0];
  src[1] := @srcbuff[Cnt];
  src[2] := @srcbuff[2*Cnt];
  SetLength(g, 256);
  SetLength(w, 256);
  for I := 0 to High(g) do begin
    g[I] := I;
    w[I] := I/255;
  end;
  logTs := TArray<Single>.Create(1, 2, 4);

  _RMCombine3(src, PSingle(dstbuff), g, logTs, w, Cnt);

  CheckEquals([4, 4.77777767, 5.61904764, 6.5], dstbuff, sTol);
end;

procedure THDRTests.LowLvlRMCombine3_10;
var src: TArray<PByte>;
    srcbuff: TArray<Byte>;
    dstbuff: TArray<Single>;
    g, logTs, w: TArray<Single>;
    I: Integer;
const Cnt = 10;
begin
  srcbuff := TArray<byte>.Create(
     1,  2,  3,  4,  5,  6,  7,  8,  9, 10,
    11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
    21, 22, 23, 24, 25, 26, 27, 28, 29, 30
  );
  SetLength(dstbuff, Cnt);
  SetLength(src, 3);
  src[0] := @srcbuff[0];
  src[1] := @srcbuff[Cnt];
  src[2] := @srcbuff[2*Cnt];
  SetLength(g, 256);
  SetLength(w, 256);
  for I := 0 to High(g) do begin
    g[I] := I;
    w[I] := I/255;
  end;
  logTs := TArray<Single>.Create(1, 2, 4);

  _RMCombine3(src, PSingle(dstbuff), g, logTs, w, Cnt);

  CheckEquals([
    13.81818199, 14.38888931, 15.02564144, 15.71428585,
    16.44444466, 17.20833397, 18, 18.81481552, 19.649122238, 20.5
  ], dstbuff, sTol);
end;

procedure THDRTests.LowLvlRMCombine_2;
var src: TArray<PByte>;
    srcbuff: TArray<Byte>;
    dstbuff: TArray<Single>;
    g, logTs, w: TArray<Single>;
    I: Integer;
const Cnt = 2;
begin
  srcbuff := TArray<byte>.Create(1, 2, 3, 4, 5, 6);
  SetLength(dstbuff, Cnt);
  SetLength(src, 3);
  src[0] := @srcbuff[0];
  src[1] := @srcbuff[Cnt];
  src[2] := @srcbuff[2*Cnt];
  SetLength(g, 256);
  SetLength(w, 256);
  for I := 0 to High(g) do begin
    g[I] := I;
    w[I] := I/255;
  end;
  logTs := TArray<Single>.Create(1, 2, 4);

  _RMCombine(src, PSingle(dstbuff), g, logTs, w, Cnt);

  CheckEquals([0.8888889, 1.83333337], dstbuff, sTol);
end;

procedure THDRTests.LowLvlRMCombine_4;
var src: TArray<PByte>;
    srcbuff: TArray<Byte>;
    dstbuff: TArray<Single>;
    g, logTs, w: TArray<Single>;
    I: Integer;
const Cnt = 4;
begin
  srcbuff := TArray<byte>.Create(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12);
  SetLength(dstbuff, Cnt);
  SetLength(src, 3);
  src[0] := @srcbuff[0];
  src[1] := @srcbuff[Cnt];
  src[2] := @srcbuff[2*Cnt];
  SetLength(g, 256);
  SetLength(w, 256);
  for I := 0 to High(g) do begin
    g[I] := I;
    w[I] := I/255;
  end;
  logTs := TArray<Single>.Create(1, 2, 4);

  _RMCombine(src, PSingle(dstbuff), g, logTs, w, Cnt);

  CheckEquals([4, 4.77777767, 5.61904764, 6.5], dstbuff, sTol);
end;

procedure THDRTests.LowLvlRMCombine_10;
var src: TArray<PByte>;
    srcbuff: TArray<Byte>;
    dstbuff: TArray<Single>;
    g, logTs, w: TArray<Single>;
    I: Integer;
const Cnt = 10;
begin
  srcbuff := TArray<byte>.Create(
     1,  2,  3,  4,  5,  6,  7,  8,  9, 10,
    11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
    21, 22, 23, 24, 25, 26, 27, 28, 29, 30
  );
  SetLength(dstbuff, Cnt);
  SetLength(src, 3);
  src[0] := @srcbuff[0];
  src[1] := @srcbuff[Cnt];
  src[2] := @srcbuff[2*Cnt];
  SetLength(g, 256);
  SetLength(w, 256);
  for I := 0 to High(g) do begin
    g[I] := I;
    w[I] := I/255;
  end;
  logTs := TArray<Single>.Create(1, 2, 4);

  _RMCombine(src, PSingle(dstbuff), g, logTs, w, Cnt);

  CheckEquals([
    13.81818199, 14.38888931, 15.02564144, 15.71428585,
    16.44444466, 17.20833397, 18, 18.81481552, 19.649122238, 20.5
  ], dstbuff, sTol);
end;

{$endregion}

initialization

  RegisterTest(THDRTests.Suite);

end.
