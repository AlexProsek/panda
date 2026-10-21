unit panda.LinAlg.PTests.GaussF64;

interface

uses
    TestFramework
  , panda.Tests.NDATestCase
  , panda.LinAlg.GaussF64
  ;

type
  TGaussElimF64Tests = class(TNDAPerformanceTestCase)
  protected
    fSolver: TGaussElimF64;
    procedure TimeSolve(aWidth: Integer; aBanded: Boolean);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Solve_N500;
    procedure SolveTridiagonalDense_N500;
    procedure SolveTridiagonalBand_N500;
    procedure SolveWidth4Dense_N500;
    procedure SolveWidth4Band_N500;
  end;

implementation

uses
    System.SysUtils
  , System.Math
  ;

function TestMatLEx(aN: Integer; const aValue: Double = 1): TArray<TArray<Double>>;
var I, J: Integer;
begin
  SetLength(Result, aN, aN + 1);
  for I := 0 to aN - 1 do begin
    for J := 0 to I do
      Result[I, J] := aValue;
    Result[I, aN] := aValue;
  end;
end;

function TestMatBandEx(aN, aWidth: Integer): TArray<TArray<Double>>;
var I, J: Integer;
begin
  Result := nil;
  SetLength(Result, aN, aN + 1);
  for I := 0 to aN - 1 do
    for J := 0 to aN - 1 do begin
      if I = J then
        Result[I, J] := 4 * aWidth + 1
      else if Abs(I - J) <= aWidth then
        Result[I, J] := -1;
      // The reference solution has every component equal to one.
      Result[I, aN] := Result[I, aN] + Result[I, J];
    end;
end;

{$region 'TGaussElimF64Tests'}

procedure TGaussElimF64Tests.SetUp;
begin
  inherited;
  fSolver := TGaussElimF64.Create;
end;

procedure TGaussElimF64Tests.TearDown;
begin
  fSolver.Free;
  inherited;
end;

procedure TGaussElimF64Tests.Solve_N500;
begin
  TimeSolve(0, False);
end;

procedure TGaussElimF64Tests.TimeSolve(aWidth: Integer; aBanded: Boolean);
var X: TArray<Double>;
    H: TArray<TArray<Double>>;
    elapsed: Double;
    times: array[0..2] of Double;
    counts: array[0..2] of Integer;
    sample, count: Integer;
    msg: String;

  procedure SolveOnce(aTimed: Boolean);
  var I: Integer;
  begin
    // Each solve consumes its augmented matrix. Prepare fresh storage outside timing.
    if aWidth = 0 then
      CheckTrue(fSolver.InitWithoutCopy(TestMatLEx(500)))
    else
      CheckTrue(fSolver.InitWithoutCopy(TestMatBandEx(500, aWidth)));

    if aTimed then SW.Start;
    fSolver.Solve(X, H);
    if aTimed then begin
      SW.Stop;
      elapsed := elapsed + SW.ElapsedMilisecondsEx;
    end;

    CheckEquals(500, Length(X));
    CheckEquals(0, Length(H));
    for I := 0 to High(X) do
      if aWidth = 0 then
        CheckEquals(Ord(I = 0), X[I], 1e-12)
      else
        CheckEquals(1, X[I], 1e-12);
  end;

begin
  if aBanded then begin
    fSolver.LowerBandWidth := aWidth;
    fSolver.UpperBandWidth := aWidth;
  end else begin
    fSolver.LowerBandWidth := -1;
    fSolver.UpperBandWidth := -1;
  end;

  SolveOnce(False);
  for sample := 0 to 2 do begin
    elapsed := 0;
    count := 0;
    repeat
      SolveOnce(True);
      Inc(count);
    until elapsed >= 100;
    times[sample] := elapsed / count;
    counts[sample] := count;
  end;

  msg := System.SysUtils.Format('Gauss N=500 width=%d banded=%s samples_ms=[%.6f,%.6f,%.6f] '
    + 'median_ms=%.6f repeats=[%d,%d,%d]; excludes initialization, includes tolerance and substitution',
    [aWidth, System.SysUtils.BoolToStr(aBanded, True), times[0], times[1], times[2],
    Max(Min(times[0], times[1]), Min(Max(times[0], times[1]), times[2])),
    counts[0], counts[1], counts[2]], TFormatSettings.Invariant);
  fWriter.WriteLine(msg);
  Status(msg);
end;

procedure TGaussElimF64Tests.SolveTridiagonalDense_N500;
begin
  TimeSolve(1, False);
end;

procedure TGaussElimF64Tests.SolveTridiagonalBand_N500;
begin
  TimeSolve(1, True);
end;

procedure TGaussElimF64Tests.SolveWidth4Dense_N500;
begin
  TimeSolve(4, False);
end;

procedure TGaussElimF64Tests.SolveWidth4Band_N500;
begin
  TimeSolve(4, True);
end;

{$endregion}

initialization

  RegisterTest(TGaussElimF64Tests.Suite);

end.
