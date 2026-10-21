unit panda.LinAlg.PTests.CholeskyF64;

interface

uses
    TestFramework
  , panda.Tests.NDATestCase
  ;

type
  /// <summary>Compares ordinary LUP and Cholesky on the same dense SPD systems.</summary>
  TLUPCholeskyF64PTests = class(TNDAPerformanceTestCase)
  protected
    procedure Compare(aN: Integer);
  published
    procedure LUCholesky_N512;
    procedure LUCholesky_N1024;
  end;

implementation

uses
    System.SysUtils
  , System.Math
  , panda.Intfs
  , panda.Arrays
  , panda.LinAlg.Base
  , panda.LinAlg.LUPF64
  , panda.LinAlg.CholeskyF64
  ;

type
  TSolverKind = (skLU, skCholesky);
  TRHSKind = (rkVector, rkMatrix);
  TTimings = array[0..2] of Double;

function Median(const aTimes: TTimings): Double;
begin
  Result := Max(Min(aTimes[0], aTimes[1]),
    Min(Max(aTimes[0], aTimes[1]), aTimes[2]));
end;

procedure TLUPCholeskyF64PTests.Compare(aN: Integer);
const rhsCount = 8;
      minBatchMS = 100;
var solvers: array[TSolverKind] of TLinearSolver<Double>;
    A, B, BM: INDArray<Double>;
    initTimes: array[TSolverKind] of TTimings;
    solveTimes: array[TSolverKind, TRHSKind] of TTimings;
    counts: array[TSolverKind, TRHSKind] of Integer;
    sample, I, J, K: Integer;
    value, total: Double;
    kind: TSolverKind;
    rhs: TRHSKind;

  procedure CheckSolution(const aX: INDArray<Double>; aRHS: TRHSKind);
  var row, col: Integer;
      actual: Double;
  begin
    CheckTrue(aX <> nil);
    CheckEquals(aN, aX.Shape[0]);
    if aRHS = rkVector then begin
      CheckEquals(1, aX.NDim);
      for row := 0 to aN - 1 do begin
        actual := aX.GetItem([row]);
        CheckFalse(IsNan(actual) or IsInfinite(actual));
        CheckEquals(1, actual, 1e-10);
      end;
    end else begin
      CheckEquals(2, aX.NDim);
      CheckEquals(rhsCount, aX.Shape[1]);
      for row := 0 to aN - 1 do
        for col := 0 to rhsCount - 1 do begin
          actual := aX.GetItem([row, col]);
          CheckFalse(IsNan(actual) or IsInfinite(actual));
          CheckEquals(col + 1, actual, 1e-9);
        end;
    end;
  end;

  procedure TimeInitialize(aKind: TSolverKind);
  var status: TLinearSolverInitStatus;
      elapsed: Double;
  begin
    // Initialize includes the private copy and automatic tolerance preparation.
    SW.Start;
    status := solvers[aKind].Initialize(A);
    SW.Stop;
    elapsed := SW.ElapsedMilisecondsEx;

    CheckTrue(status = lsisInitialized);
    if sample > 0 then initTimes[aKind][sample - 1] := elapsed;
  end;

  function SolveBatch(aKind: TSolverKind; aRHS: TRHSKind; aCount: Integer): Double;
  var input, X: INDArray<Double>;
      repeatIdx: Integer;
      ok: Boolean;
  begin
    if aRHS = rkVector then input := B else input := BM;
    ok := True;

    SW.Start;
    for repeatIdx := 1 to aCount do
      ok := (solvers[aKind].Solve(input, X) = lsssSolved) and ok;
    SW.Stop;
    Result := SW.ElapsedMilisecondsEx;

    CheckTrue(ok);
    CheckSolution(X, aRHS);
  end;

  procedure TimeSolve(aKind: TSolverKind; aRHS: TRHSKind);
  var elapsed: Double;
  begin
    if sample = 0 then begin
      // Calibrate each backend/workload independently during warm-up.
      counts[aKind, aRHS] := 1;
      repeat
        elapsed := SolveBatch(aKind, aRHS, counts[aKind, aRHS]);
        if elapsed >= minBatchMS then break;
        counts[aKind, aRHS] := counts[aKind, aRHS] * 2;
      until False;
    end else begin
      elapsed := SolveBatch(aKind, aRHS, counts[aKind, aRHS]);
      solveTimes[aKind, aRHS][sample - 1] := elapsed / counts[aKind, aRHS];
    end;
  end;

  procedure Report(const aOperation: String; const aLU, aCholesky: TTimings;
    aLUCount, aCholeskyCount: Integer);
  var msg: String;
  begin
    msg := Format('LUP/Cholesky N=%d operation=%s '
      + 'LU_samples_ms=[%.6f,%.6f,%.6f] Cholesky_samples_ms=[%.6f,%.6f,%.6f] '
      + 'LU_median_ms=%.6f Cholesky_median_ms=%.6f speedup=%.3f repeats=[%d,%d]',
      [aN, aOperation, aLU[0], aLU[1], aLU[2],
      aCholesky[0], aCholesky[1], aCholesky[2],
      Median(aLU), Median(aCholesky), Median(aLU) / Median(aCholesky),
      aLUCount, aCholeskyCount], TFormatSettings.Invariant);
    fWriter.WriteLine(msg);
  {$ifdef CONSOLE_TESTRUNNER}
    Writeln(msg);
  {$else}
    Status(msg);
  {$endif}
  end;

begin
  // Symmetric off-diagonal entries have magnitude below 0.5, while every
  // diagonal is N. Strict diagonal dominance makes this matrix positive definite.
  // B is the row sum, giving X=1; BM columns give X=column index + 1.
  A := TNDAUt.Empty<Double>([aN, aN]);
  B := TNDAUt.Empty<Double>([aN]);
  BM := TNDAUt.Empty<Double>([aN, rhsCount]);
  for I := 0 to aN - 1 do begin
    total := 0;
    for J := 0 to aN - 1 do begin
      if I = J then
        value := aN
      else
        value := ((Min(I, J) * 17 + Max(I, J) * 13) mod 19 - 9) / 19;
      A.SetItem([I, J], value);
      total := total + value;
    end;
    B.SetItem([I], total);
    for K := 0 to rhsCount - 1 do BM.SetItem([I, K], total * (K + 1));
  end;

  solvers[skLU] := nil;
  solvers[skCholesky] := nil;
  try
    solvers[skLU] := TLUPDecompositionF64.Create;
    solvers[skCholesky] := TCholeskyDecompositionF64.Create;
    // Sample zero warms up; the next three are measured. Alternate backend
    // order for every operation. No checks or matrix generation are timed.
    for sample := 0 to 3 do begin
      if Odd(sample) then begin
        TimeInitialize(skCholesky);
        TimeInitialize(skLU);
      end else begin
        TimeInitialize(skLU);
        TimeInitialize(skCholesky);
      end;

      for rhs := Low(TRHSKind) to High(TRHSKind) do
        if Odd(sample) then begin
          TimeSolve(skCholesky, rhs);
          TimeSolve(skLU, rhs);
        end else begin
          TimeSolve(skLU, rhs);
          TimeSolve(skCholesky, rhs);
        end;
    end;

    Report('initialize', initTimes[skLU], initTimes[skCholesky], 1, 1);
    Report('vector', solveTimes[skLU, rkVector], solveTimes[skCholesky, rkVector],
      counts[skLU, rkVector], counts[skCholesky, rkVector]);
    Report('8RHS', solveTimes[skLU, rkMatrix], solveTimes[skCholesky, rkMatrix],
      counts[skLU, rkMatrix], counts[skCholesky, rkMatrix]);
  finally
    for kind := Low(TSolverKind) to High(TSolverKind) do solvers[kind].Free;
  end;
end;

procedure TLUPCholeskyF64PTests.LUCholesky_N512;
begin
  Compare(512);
end;

procedure TLUPCholeskyF64PTests.LUCholesky_N1024;
begin
  Compare(1024);
end;

initialization

  RegisterTest(TLUPCholeskyF64PTests.Suite);

end.
