unit panda.LinAlg.PTests.LUP;

interface

uses
    panda.LinAlg.Base
  , TestFramework
  , panda.Tests.NDATestCase
  ;

type
  TLUPF64PTests = class(TNDAPerformanceTestCase)
  protected
    procedure Compare(aN, aBlockSize: Integer);
  published
    procedure N128_Block16;
    procedure N128_Block32;
    procedure N128_Block64;
    procedure N512_Block16;
    procedure N512_Block32;
    procedure N512_Block64;
    procedure N1024_Block16;
    procedure N1024_Block32;
    procedure N1024_Block64;
    procedure N1024_Block128;
    procedure N1024_Block256;
    procedure N1024_Block512;
    procedure N1024_Block1024;
    procedure N2048_Block128;
    procedure N2048_Block256;
    procedure N2048_Block512;
    procedure N2048_Block1024;
  end;

implementation

uses
    System.SysUtils
  , System.Math
  , panda.Intfs
  , panda.Arrays
  , panda.LinAlg.LUPF64
  , panda.LinAlg.LUP
  ;

type
  TTimings = array[0..2] of Double;

  TSolveVariant = (svCurrent, svVectors, svTransposed);

  // Alternative solve layouts use the same cached factors as production Solve.
  TLUPBenchmarkSolver = class(TLUPDecompositionF64)
  public
    procedure SolveColumns(const aB: INDArray<Double>;
      out aRes: INDArray<Double>; aTransposed: Boolean);
    function Permutation: TArray<NativeInt>;
  end;

  TLUPSolveF64PTests = class(TNDAPerformanceTestCase)
  private
    fSolver: TLUPBenchmarkSolver;
    fA, fLU, fB, fExpected, fLUSnapshot, fBSnapshot: INDArray<Double>;
    fP, fPSnapshot: TArray<NativeInt>;
    fN, fK: Integer;
    fNormA: Double;
    procedure PrepareMatrix(aN: Integer);
    procedure PrepareRHS(aK: Integer);
    procedure RunVariant(aVariant: TSolveVariant; out aX: INDArray<Double>);
    function TimeBatch(aVariant: TSolveVariant; aCount: Integer;
      out aX: INDArray<Double>): Double;
    procedure CheckSolution(const aX: INDArray<Double>);
    procedure CheckInputs;
    procedure CompareRHS;
    procedure Sweep(aN: Integer);
    procedure WriteResult(const aText: String);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure N128;
    procedure N512;
    procedure N1024;
    procedure N2048;
    procedure StridedRHS;
  end;

{$POINTERMATH ON}

procedure TLUPBenchmarkSolver.SolveColumns(const aB: INDArray<Double>;
  out aRes: INDArray<Double>; aTransposed: Boolean);
var bSpec: TMatSpec;
    rows: TArray<PT>;
    colB, colX, Y: TArray<Double>;
    N, K, I, J: NativeInt;
    pCol: PT;
    tmp: Double;
begin
  aRes := nil;
  if not Initialized then exit;
  N := Length(fRows);
  if not NDMatrix(aB, bSpec) then exit;
  if bSpec.NRows <> N then exit;
  K := bSpec.NCols;

  aRes := TNDAUt.Empty<Double>([N, K]);
  rows := fRows;
  SetLength(colB, N);
  SetLength(colX, N);
  if not aTransposed then begin
    for J := 0 to K - 1 do begin
      Gather(bSpec.Data + J * bSpec.CStep, bSpec.RStep, N, colB);
      SolveVec(colB, colX);
      Store(aRes, J, colX, False);
    end;
    exit;
  end;

  if (N <= 0) or (K <= 0) then exit;
  SetLength(Y, N * K);
  for J := 0 to K - 1 do begin
    Gather(bSpec.Data + J * bSpec.CStep, bSpec.RStep, N, colB);
    for I := 0 to N - 1 do
      Y[J * N + I] := colB[fP[I]];
  end;

  for I := 0 to N - 1 do
    for J := 0 to K - 1 do begin
      pCol := @Y[J * N];
      fNumeric.Dot(rows[I], pCol, I, tmp);
      pCol[I] := fNumeric.Subtract(pCol[I], tmp);
    end;

  for J := 0 to K - 1 do
    Y[J * N + N - 1] := fNumeric.Divide(Y[J * N + N - 1], rows[N - 1][N - 1]);
  for I := N - 2 downto 0 do
    for J := 0 to K - 1 do begin
      pCol := @Y[J * N];
      fNumeric.Dot(rows[I] + I + 1, pCol + I + 1, N - I - 1, tmp);
      pCol[I] := fNumeric.SubDiv(pCol[I], tmp, rows[I][I]);
    end;

  for J := 0 to K - 1 do begin
    Gather(PByte(@Y[J * N]), SizeOf(Double), N, colX);
    Store(aRes, J, colX, False);
  end;
end;

function TLUPBenchmarkSolver.Permutation: TArray<NativeInt>;
begin
  Result := Copy(fP);
end;

function Median(const aTimes: TTimings): Double;
begin
  Result := Max(Min(aTimes[0], aTimes[1]),
    Min(Max(aTimes[0], aTimes[1]), aTimes[2]));
end;

procedure TLUPSolveF64PTests.SetUp;
begin
  inherited;
  fSolver := TLUPBenchmarkSolver.Create;
end;

procedure TLUPSolveF64PTests.TearDown;
begin
  fSolver.Free;
  fA := nil;
  fLU := nil;
  fB := nil;
  fExpected := nil;
  fLUSnapshot := nil;
  fBSnapshot := nil;
  fP := nil;
  fPSnapshot := nil;
  inherited;
end;

procedure TLUPSolveF64PTests.PrepareMatrix(aN: Integer);
var I, J, R: Integer;
    data: PDouble;
    value, rowNorm: Double;
begin
  fN := aN;
  fA := TNDAUt.Empty<Double>([fN, fN]);
  data := PDouble(fA.Data);
  fNormA := 0;
  for I := 0 to fN - 1 do begin
    R := fN - I - 1;
    rowNorm := 0;
    for J := 0 to fN - 1 do begin
      value := ((R * 17 + J * 13) mod 19 - 9) / 19;
      if R = J then value := value + fN;
      data[I * fN + J] := value;
      rowNorm := rowNorm + Abs(value);
    end;
    fNormA := Max(fNormA, rowNorm);
  end;
  fLU := TNDAUt.Copy<Double>(fA);

  CheckTrue(fSolver.InitializeWithoutCopy(fLU) = lsisInitialized);
  fP := fSolver.Permutation;

  fLUSnapshot := TNDAUt.Copy<Double>(fLU);
  fPSnapshot := Copy(fP);
  CheckEquals(fN - 1, Integer(fP[0]), 'Fixture must exercise row pivoting');
end;

procedure TLUPSolveF64PTests.PrepareRHS(aK: Integer);
var I, J, C: Integer;
    aData, xData, bData: PDouble;
    value: Double;
begin
  fK := aK;
  fExpected := TNDAUt.Empty<Double>([fN, fK]);
  fB := TNDAUt.Empty<Double>([fN, fK]);
  aData := PDouble(fA.Data);
  xData := PDouble(fExpected.Data);
  bData := PDouble(fB.Data);
  for I := 0 to fN - 1 do
    for C := 0 to fK - 1 do
      xData[I * fK + C] := ((I * 7 + C * 11) mod 23 - 11) / 11 + C / (fK + 1);

  // Row-wise accumulation avoids expensive GetItem calls during fixture setup.
  for I := 0 to fN - 1 do begin
    for C := 0 to fK - 1 do
      bData[I * fK + C] := 0;
    for J := 0 to fN - 1 do begin
      value := aData[I * fN + J];
      for C := 0 to fK - 1 do
        bData[I * fK + C] := bData[I * fK + C] + value * xData[J * fK + C];
    end;
  end;
  fBSnapshot := TNDAUt.Copy<Double>(fB);
end;

procedure TLUPSolveF64PTests.RunVariant(aVariant: TSolveVariant;
  out aX: INDArray<Double>);
begin
  case aVariant of
    svCurrent: fSolver.Solve(fB, aX);
    svVectors: fSolver.SolveColumns(fB, aX, False);
    svTransposed: fSolver.SolveColumns(fB, aX, True);
  end;
end;

function TLUPSolveF64PTests.TimeBatch(aVariant: TSolveVariant; aCount: Integer;
  out aX: INDArray<Double>): Double;
var I: Integer;
begin
  SW.Start;
  for I := 1 to aCount do
    RunVariant(aVariant, aX);
  SW.Stop;
  Result := SW.ElapsedMilisecondsEx;
end;

procedure TLUPSolveF64PTests.CheckSolution(const aX: INDArray<Double>);
var I, J, C: Integer;
    aData, xData, refData, bData: PDouble;
    sums, normX, normB, normRef, errors, residuals: TArray<Double>;
    value, maxError, maxResidual: Double;
begin
  CheckTrue(aX <> nil);
  CheckEquals(2, aX.NDim);
  CheckEquals(fN, Integer(aX.Shape[0]));
  CheckEquals(fK, Integer(aX.Shape[1]));

  aData := PDouble(fA.Data);
  xData := PDouble(aX.Data);
  refData := PDouble(fExpected.Data);
  bData := PDouble(fBSnapshot.Data);
  SetLength(sums, fK);
  SetLength(normX, fK);
  SetLength(normB, fK);
  SetLength(normRef, fK);
  SetLength(errors, fK);
  SetLength(residuals, fK);
  for I := 0 to fN - 1 do
    for C := 0 to fK - 1 do begin
      value := xData[I * fK + C];
      CheckFalse(IsNan(value) or IsInfinite(value), 'Nonfinite solution');
      normX[C] := Max(normX[C], Abs(value));
      normB[C] := Max(normB[C], Abs(bData[I * fK + C]));
      normRef[C] := Max(normRef[C], Abs(refData[I * fK + C]));
      errors[C] := Max(errors[C], Abs(value - refData[I * fK + C]));
    end;

  for I := 0 to fN - 1 do begin
    for C := 0 to fK - 1 do
      sums[C] := 0;
    for J := 0 to fN - 1 do begin
      value := aData[I * fN + J];
      for C := 0 to fK - 1 do
        sums[C] := sums[C] + value * xData[J * fK + C];
    end;
    for C := 0 to fK - 1 do
      residuals[C] := Max(residuals[C], Abs(sums[C] - bData[I * fK + C]));
  end;
  maxError := 0;
  maxResidual := 0;
  for C := 0 to fK - 1 do begin
    maxError := Max(maxError, errors[C] / Max(1, normRef[C]));
    maxResidual := Max(maxResidual, residuals[C] / (fNormA * normX[C] + normB[C]));
  end;

  CheckTrue(maxError <= 1e-9, Format('Relative solution error %.3e', [maxError]));
  CheckTrue(maxResidual <= 1e-12, Format('Normalized residual %.3e', [maxResidual]));
end;

procedure TLUPSolveF64PTests.CheckInputs;
var bCopy: INDArray<Double>;
    P: TArray<NativeInt>;
begin
  P := fSolver.Permutation;
  CheckTrue(CompareMem(fLU.Data, fLUSnapshot.Data, fN * fN * SizeOf(Double)),
    'Factors changed');
  CheckEquals(Length(fPSnapshot), Length(P));
  CheckTrue(CompareMem(@P[0], @fPSnapshot[0], Length(P) * SizeOf(NativeInt)),
    'Permutation changed');

  // Copy also checks logical values for the untimed strided fixture.
  bCopy := TNDAUt.Copy<Double>(fB);
  CheckTrue(CompareMem(bCopy.Data, fBSnapshot.Data, fN * fK * SizeOf(Double)),
    'RHS changed');
end;

procedure TLUPSolveF64PTests.WriteResult(const aText: String);
begin
  fWriter.WriteLine(aText);
{$ifdef CONSOLE_TESTRUNNER}
  Writeln(aText);
{$else}
  Status(aText);
{$endif}
end;

procedure TLUPSolveF64PTests.CompareRHS;
const names: array[TSolveVariant] of String = ('current', 'vectors', 'transposed');
var variant: TSolveVariant;
    counts: array[TSolveVariant] of Integer;
    times: array[TSolveVariant] of TTimings;
    X: INDArray<Double>;
    elapsed, baseline: Double;
    sample, order: Integer;
    labelText: String;
begin
  for variant := Low(TSolveVariant) to High(TSolveVariant) do begin
    RunVariant(variant, X);

    CheckSolution(X);
    CheckInputs;
  end;

  for variant := Low(TSolveVariant) to High(TSolveVariant) do begin
    counts[variant] := 1;
    repeat
      elapsed := TimeBatch(variant, counts[variant], X);
      if (elapsed >= 100) or (counts[variant] >= 256) then break;
      counts[variant] := counts[variant] * 2;
    until False;

    CheckSolution(X);
    CheckInputs;
  end;

  for sample := 0 to 2 do
    for order := 0 to 2 do begin
      variant := TSolveVariant((sample + order) mod 3);
      elapsed := TimeBatch(variant, counts[variant], X);
      times[variant][sample] := elapsed / counts[variant];

      CheckSolution(X);
      CheckInputs;
    end;

  baseline := Median(times[svCurrent]);
  labelText := '';
  if fK = 1 then labelText := ' baseline=SolveVec';
  for variant := Low(TSolveVariant) to High(TSolveVariant) do
    WriteResult(Format('LUP solve N=%d K=%d variant=%s repeats=%d '
      + 'samples_ms=[%.6f,%.6f,%.6f] median_ms=%.6f speedup=%.3f%s',
      [fN, fK, names[variant], counts[variant], times[variant][0],
      times[variant][1], times[variant][2], Median(times[variant]),
      baseline / Median(times[variant]), labelText], TFormatSettings.Invariant));
end;

procedure TLUPSolveF64PTests.Sweep(aN: Integer);
const rhsCounts: array[0..6] of Integer = (1, 2, 4, 8, 16, 32, 64);
var K: Integer;
begin
  PrepareMatrix(aN);
  for K in rhsCounts do begin
    PrepareRHS(K);

    CompareRHS;
  end;

  if aN <= 512 then begin
    PrepareRHS(aN);

    CompareRHS;
  end;
end;

procedure TLUPSolveF64PTests.N128;
begin
  Sweep(128);
end;

procedure TLUPSolveF64PTests.N512;
begin
  Sweep(512);
end;

procedure TLUPSolveF64PTests.N1024;
begin
  Sweep(1024);
end;

procedure TLUPSolveF64PTests.N2048;
begin
  Sweep(2048);
end;

procedure TLUPSolveF64PTests.StridedRHS;
var wide, wideSnapshot, X: INDArray<Double>;
    variant: TSolveVariant;
    I, J: Integer;
begin
  PrepareMatrix(7);
  PrepareRHS(3);
  wide := TNDAUt.Empty<Double>([2 * fN, 2 * fK]);
  for I := 0 to 2 * fN - 1 do
    for J := 0 to 2 * fK - 1 do
      wide.SetItem([I, J], 99);
  for I := 0 to fN - 1 do
    for J := 0 to fK - 1 do
      wide.SetItem([2 * I, 2 * J], fB.GetItem([I, J]));
  wideSnapshot := TNDAUt.Copy<Double>(wide);
  fB := wide[[NDISpan(0, 2 * fN - 1, 2), NDISpan(0, 2 * fK - 1, 2)]];

  for variant := Low(TSolveVariant) to High(TSolveVariant) do begin
    RunVariant(variant, X);

    CheckSolution(X);
    CheckInputs;
    CheckTrue(CompareMem(wide.Data, wideSnapshot.Data,
      4 * fN * fK * SizeOf(Double)), 'Strided RHS backing storage changed');
  end;
end;

procedure TLUPF64PTests.Compare(aN, aBlockSize: Integer);
const solveCount = 8;
var ordinary: TLUPDecompositionF64;
    blocked: TLUPDecompositionF64;
    A, B, BM, LU, blockLU, X, XM: INDArray<Double>;
    I, J, K, R, sample, repeatIdx: Integer;
    value, total: Double;
    ok: Boolean;
    plainInit, blockInit, plainVec, blockVec, plainMat, blockMat: TTimings;
    elapsed: Double;
    msg: String;

  procedure TimeOrdinaryInitialize;
  begin
    SW.Start;
    LU := TNDAUt.Copy<Double>(A);
    ok := (ordinary.InitializeWithoutCopy(LU) = lsisInitialized);
    SW.Stop;
    elapsed := SW.ElapsedMilisecondsEx;

    CheckTrue(ok);
    if sample > 0 then plainInit[sample - 1] := elapsed;
  end;

  procedure TimeBlockedInitialize;
  begin
    SW.Start;
    blockLU := TNDAUt.Copy<Double>(A);
    ok := (blocked.InitializeWithoutCopy(blockLU) = lsisInitialized);
    SW.Stop;
    elapsed := SW.ElapsedMilisecondsEx;

    CheckTrue(ok);
    if sample > 0 then blockInit[sample - 1] := elapsed;
  end;

  procedure CheckVector;
  var idx: Integer;
  begin
    CheckTrue(X <> nil);
    for idx := 0 to aN - 1 do
      CheckEquals(1, X.GetItem([idx]), 1e-10);
  end;

  procedure CheckMatrix;
  var row, col: Integer;
  begin
    CheckTrue(XM <> nil);
    for row := 0 to aN - 1 do
      for col := 0 to 7 do
        CheckEquals(col + 1, XM.GetItem([row, col]), 1e-9);
  end;

begin
  // Deterministic dense, diagonally dominant coefficients with reversed rows
  // exercise global pivoting. Exact solutions are 1 and RHS column index + 1.
  A := TNDAUt.Empty<Double>([aN, aN]);
  B := TNDAUt.Empty<Double>([aN]);
  BM := TNDAUt.Empty<Double>([aN, 8]);
  for I := 0 to aN - 1 do begin
    R := aN - I - 1;
    total := 0;
    for J := 0 to aN - 1 do begin
      value := ((R * 17 + J * 13) mod 19 - 9) / 19;
      if R = J then value := value + aN;
      A.SetItem([I, J], value);
      total := total + value;
    end;
    B.SetItem([I], total);
    for K := 0 to 7 do
      BM.SetItem([I, K], total * (K + 1));
  end;
  ordinary := TLUPDecompositionF64.Create;
  try
    blocked := TLUPDecompositionF64.Create;
    try
      blocked.FactorizationMethod := lupBlocked;
      blocked.BlockSize := aBlockSize;
      // Both paths include a private matrix copy and automatic tolerance
      // estimation. Neither path scans numerical inputs for finite values.
      // Sample zero warms up every operation; the next three form the median.
      // Alternate initialization order to reduce systematic order effects.
      for sample := 0 to 3 do begin
        if Odd(sample) then begin
          TimeBlockedInitialize;
          TimeOrdinaryInitialize;
        end else begin
          TimeOrdinaryInitialize;
          TimeBlockedInitialize;
        end;

        SW.Start;
        for repeatIdx := 1 to solveCount do
          ordinary.Solve(B, X);
        SW.Stop;
        elapsed := SW.ElapsedMilisecondsEx / solveCount;

        CheckVector;
        if sample > 0 then plainVec[sample - 1] := elapsed;

        SW.Start;
        for repeatIdx := 1 to solveCount do
          blocked.Solve(B, X);
        SW.Stop;
        elapsed := SW.ElapsedMilisecondsEx / solveCount;

        CheckVector;
        if sample > 0 then blockVec[sample - 1] := elapsed;

        SW.Start;
        for repeatIdx := 1 to solveCount do
          ordinary.Solve(BM, XM);
        SW.Stop;
        elapsed := SW.ElapsedMilisecondsEx / solveCount;

        CheckMatrix;
        if sample > 0 then plainMat[sample - 1] := elapsed;

        SW.Start;
        for repeatIdx := 1 to solveCount do
          blocked.Solve(BM, XM);
        SW.Stop;
        elapsed := SW.ElapsedMilisecondsEx / solveCount;

        CheckMatrix;
        if sample > 0 then blockMat[sample - 1] := elapsed;
      end;
      msg := Format('N=%d block=%d median ms; init ordinary=%.6f blocked=%.6f; '
        + 'solve vector ordinary=%.6f blocked=%.6f; '
        + 'solve 8 RHS ordinary=%.6f blocked=%.6f',
        [aN, aBlockSize, Median(plainInit), Median(blockInit),
        Median(plainVec), Median(blockVec), Median(plainMat), Median(blockMat)]);
      fWriter.WriteLine(msg);
    {$ifdef CONSOLE_RUNNER}
      Writeln(msg);
    {$else}
      Status(msg);
    {$endif}
    finally
      blocked.Free;
    end;
  finally
    ordinary.Free;
  end;
end;

procedure TLUPF64PTests.N128_Block16;
begin
  Compare(128, 16);
end;

procedure TLUPF64PTests.N128_Block32;
begin
  Compare(128, 32);
end;

procedure TLUPF64PTests.N128_Block64;
begin
  Compare(128, 64);
end;

procedure TLUPF64PTests.N512_Block16;
begin
  Compare(512, 16);
end;

procedure TLUPF64PTests.N512_Block32;
begin
  Compare(512, 32);
end;

procedure TLUPF64PTests.N512_Block64;
begin
  Compare(512, 64);
end;

procedure TLUPF64PTests.N1024_Block16;
begin
  Compare(1024, 16);
end;

procedure TLUPF64PTests.N1024_Block32;
begin
  Compare(1024, 32);
end;

procedure TLUPF64PTests.N1024_Block64;
begin
  Compare(1024, 64);
end;

procedure TLUPF64PTests.N1024_Block128;
begin
  Compare(1024, 128);
end;

procedure TLUPF64PTests.N1024_Block256;
begin
  Compare(1024, 256);
end;

procedure TLUPF64PTests.N1024_Block512;
begin
  Compare(1024, 512);
end;

procedure TLUPF64PTests.N1024_Block1024;
begin
  Compare(1024, 1024);
end;

procedure TLUPF64PTests.N2048_Block128;
begin
  Compare(2048, 128);
end;

procedure TLUPF64PTests.N2048_Block256;
begin
  Compare(2048, 256);
end;

procedure TLUPF64PTests.N2048_Block512;
begin
  Compare(2048, 512);
end;

procedure TLUPF64PTests.N2048_Block1024;
begin
  Compare(2048, 1024);
end;

initialization

  RegisterTest(TLUPF64PTests.Suite);
  RegisterTest(TLUPSolveF64PTests.Suite);

end.
