unit panda.Tests.MAT4io;

interface

uses
    TestFramework
  , panda.Tests.NDATestCase
  , panda.Intfs
  , panda.Nums
  , panda.Arrays
  , panda.MAT4io
  , System.SysUtils
  , System.Classes
  , System.TypInfo
  ;

type
  TMAT4TestCase = class abstract(TNDATestCase)
  public const
    cHSz = SizeOf(TMAT4Header);
  protected
    fStream: TBytesStream;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TMAT4ExportTests = class(TMAT4TestCase)
  protected
    fExporter: TMAT4Exporter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure WriteNamedArray;
    procedure WriteByteVec;
    procedure WriteByteMat;
    procedure WriteTwoVec;
  end;

  TMAT4ImportTests = class(TMAT4TestCase)
  protected
    fExporter: TMAT4Exporter;
    fImporter: TMAT4Importer;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ReadArrayLabel;
    procedure ReadByteVec;
    procedure ReadByteMat;
    procedure ReadTwoVec;
  end;

  {$define Scilab}

  TScilabMatImportTests = class(TNDATestCase)
  published
    procedure CheckNumMatrices;
    procedure ReadUInt8Vec;
    procedure ReadInt16Vec;
    procedure ReadUInt16Vec;
    procedure ReadInt32Vec;
    procedure ReadDoubleVec;
    procedure ReadUInt8Mat;
    procedure ReadComplexVec;
    procedure ReadComplexMat;
    procedure ReadStrVec;
  end;

implementation

{$region 'TMAT4TestCase'}

procedure TMAT4TestCase.SetUp;
begin
  inherited;
  fStream := TBytesStream.Create;
end;

procedure TMAT4TestCase.TearDown;
begin
  fStream.Free;
  inherited;
end;

{$endregion}

{$region 'TMAT4ExportTests'}

procedure TMAT4ExportTests.SetUp;
begin
  inherited;
  fExporter := TMAT4Exporter.Create(fStream);
end;

procedure TMAT4ExportTests.TearDown;
begin
  fExporter.Free;
  inherited;
end;

procedure TMAT4ExportTests.WriteNamedArray;
var arr: INDArray<Byte>;
    pH: PMAT4Header;
begin
  arr := TNDAUt.AsArray<Byte>([1, 2, 3]);
  fExporter.WriteMatrix(arr, 'x');

  CheckEquals(cHSz + 2 (* name size *) + 3, fStream.Size);
  pH := PMAT4Header(fStream.Bytes);
  CheckEquals(2, pH^.NameLen);
  CheckEquals(Ord('x'), fStream.Bytes[cHSz]);
  CheckEquals(0, fStream.Bytes[cHSz + 1]);
end;

procedure TMAT4ExportTests.WriteByteVec;
var arr: INDArray<Byte>;
    pH: PMAT4Header;
begin
  arr := TNDAUt.AsArray<Byte>([1, 2, 3]);
  fExporter.WriteMatrix(arr);

  CheckEquals(cHSz + 1 (* name size *) + 3, fStream.Size);
  pH := PMAT4Header(fStream.Bytes);
  CheckEquals(1, pH^.MRows);
  CheckEquals(3, pH^.NCols);
  CheckEquals(0, pH^.Imagf);
  CheckEquals(1, pH^.NameLen);
  CheckEquals(0, fStream.Bytes[cHSz]);
  CheckEquals(1, fStream.Bytes[cHSz + 1]);
  CheckEquals(2, fStream.Bytes[cHSz + 2]);
  CheckEquals(3, fStream.Bytes[cHSz + 3]);
end;

procedure TMAT4ExportTests.WriteByteMat;
var arr: INDArray<Byte>;
    pH: PMAT4Header;
begin
  arr := TNDAUt.AsArray<Byte>([[1, 2, 3], [4, 5, 6]]);
  fExporter.WriteMatrix(arr);

  CheckEquals(cHSz + 1 + 6, fStream.Size);
  pH := PMAT4Header(fStream.Bytes);
  CheckEquals(2, pH^.MRows);
  CheckEquals(3, pH^.NCols);
  CheckEquals(0, pH^.Imagf);
  CheckEquals(1, pH^.NameLen);
  CheckEquals(0, fStream.Bytes[cHSz]);
  CheckEquals(1, fStream.Bytes[cHSz + 1]);
  CheckEquals(4, fStream.Bytes[cHSz + 2]);
  CheckEquals(2, fStream.Bytes[cHSz + 3]);
  CheckEquals(5, fStream.Bytes[cHSz + 4]);
  CheckEquals(3, fStream.Bytes[cHSz + 5]);
  CheckEquals(6, fStream.Bytes[cHSz + 6]);
end;

procedure TMAT4ExportTests.WriteTwoVec;
var arr: INDArray<Byte>;
    pH: PMAT4Header;
begin
  arr := TNDAUt.AsArray<Byte>([1, 2, 3]);
  fExporter.WriteMatrix(arr);
  arr := TNDAUt.AsArray<Byte>([1, 2]);
  fExporter.WriteMatrix(arr);

  CheckEquals(2*cHSz + 2 + 5, fStream.Size);
  pH := PMAT4Header(fStream.Bytes);
  CheckEquals(1, pH^.MRows);
  CheckEquals(3, pH^.NCols);
  pH := PMAT4Header(@fStream.Bytes[cHSz + 4]);
  CheckEquals(1, pH^.MRows);
  CheckEquals(2, pH^.NCols);
end;

{$endregion}

{$region 'TMAT4ImportTests'}

procedure TMAT4ImportTests.SetUp;
begin
  inherited;
  fExporter := TMAT4Exporter.Create(fStream);
  fImporter := TMAT4Importer.Create(fStream);
end;

procedure TMAT4ImportTests.TearDown;
begin
  fExporter.Free;
  fImporter.Free;
  inherited;
end;

procedure TMAT4ImportTests.ReadArrayLabel;
var labels: TArray<String>;
begin
  fExporter.WriteMatrix(TNDAUt.AsArray<Byte>([1, 2, 3]), 'x');
  fStream.Position := 0;

  labels := fImporter.ReadLabels;
  CheckEquals(1, Length(labels));
  CheckEquals('x', labels[0]);
end;

procedure TMAT4ImportTests.ReadByteVec;
var arr: INDArray<Byte>;
    v: TArray<Byte>;
    I: Integer;
begin
  fExporter.WriteMatrix(TNDAUt.AsArray<Byte>([1, 2, 3]), 'x');
  fStream.Position := 0;

  CheckTrue(fImporter.TryRead<Byte>(0, arr));
  CheckTrue(TNDAUt.TryAsDynArray<Byte>(arr, v));
  CheckEquals(3, Length(v));
  for I := 0 to 2 do
    CheckEquals(I + 1, v[I]);
end;

procedure TMAT4ImportTests.ReadByteMat;
var arr: INDArray<Byte>;
    m: TArray<TArray<Byte>>;
    I, J: Integer;
begin
  fExporter.WriteMatrix(TNDAUt.AsArray<Byte>([[1, 2, 3], [4, 5, 6]]), 'x');
  fStream.Position := 0;

  CheckTrue(fImporter.TryRead<Byte>(0, arr));
  CheckTrue(TNDAUt.TryAsDynArray2D<Byte>(arr, m));
  CheckEquals(2, Length(m));
  CheckEquals(3, Length(m[0]));
  CheckEquals(3, Length(m[1]));
  for I := 0 to 1 do
    for J := 0 to 2 do
      CheckEquals(I*3 + J + 1, m[I, J]);
end;

procedure TMAT4ImportTests.ReadTwoVec;
var data: TArray<INDArray>;
    v: TArray<Byte>;
    I: Integer;
begin
  fExporter.WriteMatrix(TNDAUt.AsArray<Byte>([1, 2, 3]), 'x');
  fExporter.WriteMatrix(TNDAUt.AsArray<Byte>([4, 5, 6]), 'y');
  fStream.Position := 0;

  data := fImporter.ReadAll;

  CheckTrue(TNDAUt.TryAsDynArray<Byte>(data[0], v));
  CheckEquals(3, Length(v));
  for I := 0 to 2 do
    CheckEquals(I + 1, v[I]);

  CheckTrue(TNDAUt.TryAsDynArray<Byte>(data[1], v));
  CheckEquals(3, Length(v));
  for I := 0 to 2 do
    CheckEquals(I + 4, v[I]);
end;

{$endregion}

{$region 'TScilabMatImportTests'}

procedure TScilabMatImportTests.CheckNumMatrices;
var fn: String;
    importer: TMAT4Importer;
    labels: TArray<String>;
    I: Integer;
begin
  fn := GetTestDataPath('numMat.mat');
  if not FileExists(fn) then
    Fail(Format('File ''%s not found.', [fn]));

  importer := TMat4Importer.Create(fn);
  try
    labels := importer.ReadLabels;
    CheckEquals(['vui8', 'vi16', 'vui16', 'vi32', 'vd', 'mui8'], labels);

    for I := 0 to High(labels) do
      CheckTrue(mtNumeric = importer.MatType(I));

     CheckTrue(etUInt8  = importer.MatElementType(0));
     CheckTrue(etInt16  = importer.MatElementType(1));
     CheckTrue(etUInt16 = importer.MatElementType(2));
     CheckTrue(etInt32  = importer.MatElementType(3));
     CheckTrue(etDouble = importer.MatElementType(4));
     CheckTrue(etUInt8  = importer.MatElementType(5));
  finally
    importer.Free;
  end;
end;

procedure TScilabMatImportTests.ReadUInt8Vec;
var fn: String;
    importer: TMAT4Importer;
    arr: INDArray<Byte>;
    v: TArray<Byte>;
begin
  fn := GetTestDataPath('numMat.mat');
  if not FileExists(fn) then
    Fail(Format('File ''%s not found.', [fn]));

  importer := TMat4Importer.Create(fn);
  try
    CheckTrue(importer.TryRead<Byte>(0, arr));
    CheckTrue(TNDAUt.TryAsDynArray<Byte>(arr, v));
    CheckEquals(3, Length(v));
    CheckEquals(1, v[0]);
    CheckEquals(2, v[1]);
    CheckEquals(3, v[2]);
  finally
    importer.Free;
  end;
end;

procedure TScilabMatImportTests.ReadInt16Vec;
var fn: String;
    importer: TMAT4Importer;
    arr: INDArray<Int16>;
    v: TArray<Int16>;
begin
  fn := GetTestDataPath('numMat.mat');
  if not FileExists(fn) then
    Fail(Format('File ''%s not found.', [fn]));

  importer := TMat4Importer.Create(fn);
  try
    CheckTrue(importer.TryRead<Int16>(1, arr));
    CheckTrue(TNDAUt.TryAsDynArray<Int16>(arr, v));
    CheckEquals(5, Length(v));
    CheckEquals(-2000, v[0]);
    CheckEquals(-1000, v[1]);
    CheckEquals(0,     v[2]);
    CheckEquals(1000,  v[3]);
    CheckEquals(2000,  v[4]);
  finally
    importer.Free;
  end;
end;

procedure TScilabMatImportTests.ReadUInt16Vec;
var fn: String;
    importer: TMAT4Importer;
    arr: INDArray<UInt16>;
    v: TArray<UInt16>;
begin
  fn := GetTestDataPath('numMat.mat');
  if not FileExists(fn) then
    Fail(Format('File ''%s not found.', [fn]));

  importer := TMat4Importer.Create(fn);
  try
    CheckTrue(importer.TryRead<UInt16>(2, arr));
    CheckTrue(TNDAUt.TryAsDynArray<UInt16>(arr, v));
    CheckEquals(3, Length(v));
    CheckEquals(0,    v[0]);
    CheckEquals(1000, v[1]);
    CheckEquals(2000, v[2]);
  finally
    importer.Free;
  end;
end;

procedure TScilabMatImportTests.ReadInt32Vec;
var fn: String;
    importer: TMAT4Importer;
    arr: INDArray<Integer>;
    v: TArray<Integer>;
begin
  fn := GetTestDataPath('numMat.mat');
  if not FileExists(fn) then
    Fail(Format('File ''%s not found.', [fn]));

  importer := TMat4Importer.Create(fn);
  try
    CheckTrue(importer.TryRead<Integer>(3, arr));
    CheckTrue(TNDAUt.TryAsDynArray<Integer>(arr, v));
    CheckEquals(5, Length(v));
    CheckEquals(-2000000, v[0]);
    CheckEquals(-1000000, v[1]);
    CheckEquals(0,        v[2]);
    CheckEquals(1000000,  v[3]);
    CheckEquals(2000000,  v[4]);
  finally
    importer.Free;
  end;
end;

procedure TScilabMatImportTests.ReadDoubleVec;
var fn: String;
    importer: TMAT4Importer;
    arr: INDArray<Double>;
    v: TArray<Double>;
begin
  fn := GetTestDataPath('numMat.mat');
  if not FileExists(fn) then
    Fail(Format('File ''%s not found.', [fn]));

  importer := TMat4Importer.Create(fn);
  try
    CheckTrue(importer.TryRead<Double>(4, arr));
    CheckTrue(TNDAUt.TryAsDynArray<Double>(arr, v));
    CheckEquals(3, Length(v));
    CheckEquals(1, v[0]);
    CheckEquals(2, v[1]);
    CheckEquals(3, v[2]);
  finally
    importer.Free;
  end;
end;

procedure TScilabMatImportTests.ReadUInt8Mat;
var fn: String;
    importer: TMAT4Importer;
    arr: INDArray<Byte>;
    m: TArray<TArray<Byte>>;
begin
  fn := GetTestDataPath('numMat.mat');
  if not FileExists(fn) then
    Fail(Format('File ''%s not found.', [fn]));

  importer := TMat4Importer.Create(fn);
  try
    CheckTrue(importer.TryRead<Byte>(5, arr));
    CheckTrue(TNDAUt.TryAsDynArray2D<Byte>(arr, m));
    CheckEquals(2, Length(m));
    CheckEquals(3, Length(m[0]));
    CheckEquals(3, Length(m[1]));
    CheckEquals(1, m[0, 0]);
    CheckEquals(2, m[0, 1]);
    CheckEquals(3, m[0, 2]);
    CheckEquals(4, m[1, 0]);
    CheckEquals(5, m[1, 1]);
    CheckEquals(6, m[1, 2]);
  finally
    importer.Free;
  end;
end;

procedure TScilabMatImportTests.ReadComplexVec;
var fn: String;
    importer: TMAT4Importer;
    labels: TArray<String>;
    I: Integer;
    arr: INDArray<TCmplx128>;
    v: TArray<TCmplx128>;
begin
  fn := GetTestDataPath('cmplxMat.mat');
  if not FileExists(fn) then
    Fail(Format('File ''%s not found.', [fn]));

  importer := TMat4Importer.Create(fn);
  try
    labels := importer.ReadLabels;
    CheckEquals(['v', 'm'], labels);

    for I := 0 to High(labels) do begin
      CheckTrue(mtNumeric = importer.MatType(I));
      CheckTrue(etCmplx128 = importer.MatElementType(I));
    end;

    CheckTrue(importer.TryRead<TCmplx128>(0, arr));
    CheckTrue(TNDAUt.TryAsDynArray<TCmplx128>(arr, v));
    CheckEquals(2, Length(v));
    CheckEquals(Cmplx(1, 2), v[0]);
    CheckEquals(Cmplx(3, 4), v[1]);
  finally
    importer.Free;
  end;
end;

procedure TScilabMatImportTests.ReadComplexMat;
var fn: String;
    importer: TMAT4Importer;
    labels: TArray<String>;
    I: Integer;
    arr: INDArray<TCmplx128>;
    m: TArray<TArray<TCmplx128>>;
begin
  fn := GetTestDataPath('cmplxMat.mat');
  if not FileExists(fn) then
    Fail(Format('File ''%s not found.', [fn]));

  importer := TMat4Importer.Create(fn);
  try
    labels := importer.ReadLabels;
    CheckEquals(['v', 'm'], labels);

    for I := 0 to High(labels) do begin
      CheckTrue(mtNumeric = importer.MatType(I));
      CheckTrue(etCmplx128 = importer.MatElementType(I));
    end;

    CheckTrue(importer.TryRead<TCmplx128>(1, arr));
    TNDAUt.TryAsDynArray2D<TCmplx128>(arr, m);
    CheckEquals(2, Length(m));
    CheckEquals(3, Length(m[0]));
    CheckEquals(Cmplx(1, 2), m[0, 0]);
    CheckEquals(Cmplx(3, 4), m[0, 1]);
    CheckEquals(Cmplx(5, 6), m[0, 2]);
    CheckEquals(3, Length(m[1]));
    CheckEquals(Cmplx(7, 8),   m[1, 0]);
    CheckEquals(Cmplx(9, 10),  m[1, 1]);
    CheckEquals(Cmplx(11, 12), m[1, 2]);
  finally
    importer.Free;
  end;
end;

procedure TScilabMatImportTests.ReadStrVec;
var fn: String;
    importer: TMAT4Importer;
    labels: TArray<String>;
    arr: TArray<String>;
begin
  fn := GetTestDataPath('strMat.mat');
  if not FileExists(fn) then
    Fail(Format('File ''%s not found.', [fn]));

  importer := TMat4Importer.Create(fn);
  try
    labels := importer.ReadLabels;
    CheckEquals(['v'], labels);
    CheckTrue(mtTextMat = importer.MatType(0));

    CheckTrue(importer.TryReadTextArray(0, arr));
    CheckEquals(3, Length(arr));
    CheckEquals('bubu', arr[0]);
    CheckEquals('haha', arr[1]);
    CheckEquals('c   ', arr[2]);
  finally
    importer.Free;
  end;
end;

{$endregion}

initialization

  RegisterTest(TMAT4ExportTests.Suite);
  RegisterTest(TMAT4ImportTests.Suite);
{$ifdef Scilab}
  RegisterTest(TScilabMatImportTests.Suite);
{$endif}

end.
