unit panda.Tests.Formatter;

interface

uses
    TestFramework
  , panda.Tests.NDATestCase
  , panda.Intfs
  , panda.Arrays
  , panda.Formatter
  ;

type
  TFormatterTests = class(TNDATestCase)
  protected
    fFmt: TNDAFormatter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure FormatScalar;
    procedure Format1D;
    procedure Format2D;
    procedure Format3D;
    procedure Format1DStepLv;
    procedure Format2DStepLvl1;
    procedure PyLikeFormat;
  end;

  TParserTests = class(TNDATestCase)
  protected
    fParser: TNDAParser;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ParseScalar;
    procedure Parse1D;
    procedure Parse2D;
    procedure Parse3D;
  end;

implementation

{$region 'TFormatterTests'}

procedure TFormatterTests.SetUp;
begin
  inherited;
  fFmt := TNDAFormatter.Create;
end;

procedure TFormatterTests.TearDown;
begin
  inherited;
  fFmt.Free;
end;

procedure TFormatterTests.FormatScalar;
var a: INDArray;
    s: String;
begin
  a := TNDAUt.Scalar<Integer>(10);
  s := fFmt.GetString(a);
  CheckEquals('10', s);
end;

procedure TFormatterTests.Format1D;
var a: INDArray;
    s: String;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3]);
  s := fFmt.GetString(a);
  CheckEquals('{1, 2, 3}', s);
end;

procedure TFormatterTests.Format2D;
var a: INDArray;
    s: String;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3, 4, 5, 6], [2, 3]);
  s := fFmt.GetString(a);
  CheckEquals('{{1, 2, 3}, {4, 5, 6}}', s);
end;

procedure TFormatterTests.Format3D;
var a: INDArray;
    s: String;
begin
  a := iRng2NDA([2, 3, 2]);
  s := fFmt.GetString(a);
  CheckEquals('{{{1, 2}, {3, 4}, {5, 6}}, {{7, 8}, {9, 10}, {11, 12}}}', s);
end;

procedure TFormatterTests.Format1DStepLv;
var a: INDArray<Integer>;
    s: String;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3, 4, 5, 6]);
  s := fFmt.GetString(a[[NDISpan(0, -1, 2)]]);
  CheckEquals('{1, 3, 5}', s);
end;

procedure TFormatterTests.Format2DStepLvl1;
var a: INDArray<Integer>;
    s: String;
begin
  a := TNDAUt.AsArray<Integer>([1, 2, 3, 4, 5, 6], [2, 3]);
  s := fFmt.GetString(a[[NDIAll, NDISpan(0, -1, 2)]]);
  CheckEquals('{{1, 3}, {4, 6}}', s);
end;

procedure TFormatterTests.PyLikeFormat;
var a: INDArray<Integer>;
    s: String;
begin
  fFmt.Prefix := 'array(';
  fFmt.Suffix := ')';
  fFmt.BeginBracket := '[';
  fFmt.EndBracket := ']';

  a := TNDAUt.AsArray<Integer>([1, 2, 3, 4, 5, 6], [2, 3]);
  s := fFmt.GetString(a);
  CheckEquals('array([[1, 2, 3], [4, 5, 6]])', s);
end;

{$endregion}

{$region 'TParserTests'}

procedure TParserTests.SetUp;
begin
  inherited;
  fParser := TNDAParser.Create;
end;

procedure TParserTests.TearDown;
begin
  fParser.Free;
  inherited;
end;

procedure TParserTests.ParseScalar;
var a: INDArray;
    v: Integer;
begin
  fParser.ElementType := TypeInfo(Integer);

  CheckTrue(fParser.Parse('3', a));
  CheckTrue(TNDAUt.TryAsScalar<Integer>(a, v));
  CheckEquals(3, v);
end;

procedure TParserTests.Parse1D;
var a: INDArray;
    v: TArray<Integer>;
begin
  fParser.ElementType := TypeInfo(Integer);

  CheckTrue(fParser.Parse('{1, 2, 3}', a));
  CheckTrue(TNDAUt.TryAsDynArray<Integer>(a, v));
  CheckEquals(3, Length(v));
  CheckEquals(1, v[0]);
  CheckEquals(2, v[1]);
  CheckEquals(3, v[2]);
end;

procedure TParserTests.Parse2D;
var a: INDArray;
    m: TArray<TArray<Integer>>;
begin
  fParser.ElementType := TypeInfo(Integer);

  CheckTrue(fParser.Parse('{{1, 2, 3}, {4, 5, 6}}', a));
  CheckTrue(TNDAUt.TryAsDynArray2D<Integer>(a, m));
  CheckEquals(2, Length(m));
  CheckEquals(3, Length(m[0]));
  CheckEquals(3, Length(m[1]));

  CheckEquals(1, m[0, 0]);
  CheckEquals(2, m[0, 1]);
  CheckEquals(3, m[0, 2]);

  CheckEquals(4, m[1, 0]);
  CheckEquals(5, m[1, 1]);
  CheckEquals(6, m[1, 2]);
end;

procedure TParserTests.Parse3D;
var a: INDArray;
    ai: TNDAItems<Integer>;
begin
  fParser.ElementType := TypeInfo(Integer);

  CheckTrue(fParser.Parse('{{{1, 2}, {3, 4}}, {{5, 6}, {7, 8}}}', a));
  CheckTrue(SameQ(a.GetItemType, TypeInfo(Integer)));
  CheckEquals(3, a.NDim);
  CheckEquals(2, a.Shape[0]);
  CheckEquals(2, a.Shape[1]);
  CheckEquals(2, a.Shape[2]);

  ai := a as INDArray<Integer>;

  CheckEquals(1, ai[[0, 0, 0]]);
  CheckEquals(2, ai[[0, 0, 1]]);

  CheckEquals(3, ai[[0, 1, 0]]);
  CheckEquals(4, ai[[0, 1, 1]]);


  CheckEquals(5, ai[[1, 0, 0]]);
  CheckEquals(6, ai[[1, 0, 1]]);

  CheckEquals(7, ai[[1, 1, 0]]);
  CheckEquals(8, ai[[1, 1, 1]]);
end;


{$endregion}

initialization

  RegisterTest(TFormatterTests.Suite);
  RegisterTest(TParserTests.Suite);

end.
