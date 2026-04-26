unit panda.Filters.Tests.TVFilter;

interface

uses
    TestFramework
  , panda.Intfs
  , panda.Arrays
  , panda.ArrManip
  , panda.Arithmetic
  , panda.DynArrayUtils
  , panda.Filters.TVFilter
  , panda.Tests.NDATestCase
  , panda.ImgProc.Types
  , panda.ImgProc.Images
  , panda.ImgProc.io
  , panda.ImgProc.CSCvt
  , System.Generics.Collections
  ;

type
  TStepMonitor = class
  protected
    fValues: TList<Double>;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Notify(const aValue: Double);
    function ObjectiveValues: TArray<Double>;
  end;

  TTestTVFilter1DF32 = class(TTVFilter1DF32)
  end;

  TTVFilter1DF32Tests = class(TNDATestCase)
  protected
    fFilter: TTestTVFilter1DF32;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure NoisyStep;
  end;

  TTestTVFilter2DF32 = class(TTVFilter2DF32)
  end;

  TTVFilter2DF32Tests = class(TNDATestCase)
  protected const
    tol = 1e-12;
  protected
    fFilter: TTestTVFilter2DF32;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure EvalGrad;
    procedure EvalDiv;
    procedure EvalDivH7;
    procedure Normalization2;
    procedure Normalization4;
    procedure NormalizationSmallNorm;
    procedure SigNorm4;
    procedure NoisyLenaObjFunc;
    procedure NoisyLena_OctaveCmp;
  end;

implementation

{$region 'TStepMonitor'}

procedure TStepMonitor.AfterConstruction;
begin
  inherited;
  fValues := TList<Double>.Create;
end;

procedure TStepMonitor.BeforeDestruction;
begin
  fValues.Free;
  inherited;
end;

procedure TStepMonitor.Notify(const aValue: Double);
begin
  fValues.Add(aValue);
end;

function TStepMonitor.ObjectiveValues: TArray<Double>;
begin
  Result := fValues.ToArray;
end;

{$endregion}

{$region 'TTVFilter1DF32Tests'}

procedure TTVFilter1DF32Tests.SetUp;
begin
  inherited;
  fFilter := TTestTVFilter1DF32.Create;
end;

procedure TTVFilter1DF32Tests.TearDown;
begin
  inherited;
  fFilter.Free;
end;

procedure TTVFilter1DF32Tests.NoisyStep;
var src, dst: INDArray<Single>;
begin
  src := TNDAMan.Concat<Single>([
    TNDAUt.Full<Single>([100], 1/4), TNDAUt.Full<Single>([100], 3/4)
  ]);
//  src := TTensorF32(src) + TRndUt.Random<Single>([200], -1/4, 1/4);

  fFilter.Execute(src, dst);
end;

{$endregion}

{$region 'TTVFilter2DF32Tests'}

procedure TTVFilter2DF32Tests.SetUp;
begin
  fFilter := TTestTVFilter2DF32.Create;
end;

procedure TTVFilter2DF32Tests.TearDown;
begin
 fFilter.Free;
end;

procedure TTVFilter2DF32Tests.EvalGrad;
var p, g: TArray<Single>;
    g2: TArray<TArray<Single>>;
    I, w, h: Integer;
begin
  w := 5; h := 10;
  fFilter.Init(w, h);

  p := TDynAUt.Range_F32(1.0, w*h);
  g := TDynAUt.ConstantArray<Single>(100, 2*Length(p));
  fFilter.EvalGrad(p, g);

  g2 := TDynAUt.Partition<Single>(g, w);
  CheckEquals(2*h, Length(g2));
  for I := 0 to h - 1 do
    CheckEquals([1, 1, 1, 1, 0], g2[I], tol);
  for I := h to 2*h - 2 do
    CheckEquals([5, 5, 5, 5, 5], g2[I], tol);
  CheckEquals([0, 0, 0, 0, 0], g2[2*h - 1], tol);
end;

procedure TTVFilter2DF32Tests.EvalDiv;
var p, g: TArray<Single>;
    w, h: Integer;
begin
  w := 2; h := 3;
  fFilter.Init(w, h);
  p := TDynAUt.Range_F32(1.0, 2*w*h);
  SetLength(g, w * h);
  fFilter.EvalDiv(p, g);

  CheckEquals([8, 7, 5, -1, -4, -15], g, tol);
end;

procedure TTVFilter2DF32Tests.EvalDivH7;
var p, g: TArray<Single>;
    w, h: Integer;
begin
  w := 2; h := 7;
  fFilter.Init(w, h);
  p := TDynAUt.Range_F32(1.0, 2*w*h);
  SetLength(g, w * h);
  fFilter.EvalDiv(p, g);

  CheckEquals([16, 15, 5, -1, 7, -3, 9, -5, 11, -7, 13, -9, -12, -39], g, tol);
end;

procedure TTVFilter2DF32Tests.Normalization2;
var x, y: TArray<Single>;
    n: Single;
const tol = 1e-6;
begin
  x := TArray<Single>.Create(1, 2);
  y := TArray<Single>.Create(3, 4);
  fFilter.Normalize(PSingle(x), PSingle(y), 2);

  n := Sqrt(10);
  CheckEquals(1/n, x[0], tol);
  CheckEquals(3/n, y[0], tol);
  n := Sqrt(20);
  CheckEquals(2/n, x[1], tol);
  CheckEquals(4/n, y[1], tol);
end;

procedure TTVFilter2DF32Tests.Normalization4;
var x, y: TArray<Single>;
    n: Single;
const tol = 1e-6;
begin
  x := TArray<Single>.Create(1, 2, 3, 4);
  y := TArray<Single>.Create(3, 4, 5, 6);
  fFilter.Normalize(PSingle(x), PSingle(y), 4);

  n := Sqrt(10);
  CheckEquals(1/n, x[0], tol);
  CheckEquals(3/n, y[0], tol);
  n := Sqrt(20);
  CheckEquals(2/n, x[1], tol);
  CheckEquals(4/n, y[1], tol);
  n := Sqrt(34);
  CheckEquals(3/n, x[2], tol);
  CheckEquals(5/n, y[2], tol);
  n := Sqrt(52);
  CheckEquals(4/n, x[3], tol);
  CheckEquals(6/n, y[3], tol);
end;

procedure TTVFilter2DF32Tests.NormalizationSmallNorm;
var x, y: TArray<Single>;
const tol = 1e-6;
begin
  x := TArray<Single>.Create(0.25, 0.50);
  y := TArray<Single>.Create(0.50, 0.75);
  fFilter.Normalize(PSingle(x), PSingle(y), 2);

  CheckEquals(0.25, x[0], tol);
  CheckEquals(0.50, y[0], tol);
  CheckEquals(0.50, x[1], tol);
  CheckEquals(0.75, y[1], tol);
end;

procedure TTVFilter2DF32Tests.SigNorm4;
var px, py, uxy: TArray<Single>;
begin
  px := TArray<Single>.Create(2, 4, 6, 8);
  py := TArray<Single>.Create(1, 2, 3, 4);
  uxy := TArray<Single>.Create(11, 12, 11, 13, 12, 14, 13, 15);

  fFilter.ProjNorm(2, PSingle(uxy), PSingle(px), PSingle(py), 4);

  CheckEquals([0.69253, 0.68232, 0.694595, 0.70711], px, 1e-3);
  CheckEquals([0.72139, 0.73106, 0.719401, 0.70711], py, 1e-3);
end;

procedure TTVFilter2DF32Tests.NoisyLenaObjFunc;
//var src, dst: IImage;
//    sm: TStepMonitor;
begin
//  src := GetTestImage('noisyLena.png', CS_Float);
//  dst := TImg.Create(src.Width, src.Height, CS_Float);
//
//  sm := TStepMonitor.Create;
//  try
//    fFilter.StepMonitor := sm.Notify;
//    fFilter.Init(src.Width, src.Height);
//    CheckTrue(fFilter.Execute(src, dst));
//
//  finally
//    sm.Free;
//  end;
end;

procedure TTVFilter2DF32Tests.NoisyLena_OctaveCmp;
var src, dst, octaveRes: INDArray<Single>;
    arrui8: INDArray<Byte>;
    i: IImage;
begin
  i := ImportImage(GetTestDataPath('FilteredNoisyLena_octave.bmp'));
  CheckTrue(TImgUt.TryAsArray<Byte>(i, arrui8));
  octaveRes := TNDAUt.AsType<Single>(arrui8);

  i := ImportImage(GetTestDataPath('noisyLena.png'));
  CheckTrue(TImgUt.TryAsArray<Byte>(i, arrui8));
  src := TNDAUt.AsType<Single>(arrui8);
  TTensorF32(src).DivideBy(255);
  dst := TNDAUt.Empty<Single>(src.Shape);

  TotalVariationFilter(src, dst, 1, 100);

  TTensorF32(dst).MultiplyBy(255);
  CheckTrue(ndaAllClose(dst, octaveRes, 1));
end;

{$endregion}

initialization

  RegisterTest(TTVFilter1DF32Tests.Suite);
  RegisterTest(TTVFilter2DF32Tests.Suite);

end.
