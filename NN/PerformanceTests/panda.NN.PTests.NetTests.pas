unit panda.NN.PTests.NetTests;

interface

uses
    TestFramework
  , panda.Tests.NDATestCase
  , panda.Intfs
  , panda.NN
  , panda.NN.Importer
  , panda.DynArrayUtils
  , panda.ImgProc.Types
  , panda.ImgProc.io
  , panda.ImgProc.Images
  , System.SysUtils
  , System.Math
  , System.IOUtils
  ;

type
  TNetTests = class(TNDAPerformanceTestCase)
  published
    procedure MnistNetTest;
  end;

implementation

function NNTestDataFile(const aFileName: string): string;
begin
  Result := TPath.GetFullPath(TPath.Combine(ExtractFilePath(ParamStr(0)),
    '..\..\..\UnitTests\TestData\' + aFileName));
end;

function ReadNNMAT4Fixture(const aFileName: string): TNNetChain;
var importer: TNNMAT4Importer;
begin
  importer := TNNMAT4Importer.Create(NNTestDataFile(aFileName));
  try
    Result := importer.ReadNetwork;
  finally
    importer.Free;
  end;
end;

function ReadImage(const aFileName: String): IImage;
begin
  Result := ImportImage(NNTestDataFile(aFileName));
end;

procedure TNetTests.MnistNetTest;
var network: TNNetChain;
    img: IImage;
    I: Integer;
const N = 100;
begin
  network := ReadNNMAT4Fixture('nn-mnist.mat');
  try
    img := ReadImage('mnist_char_3.jpg');

    SWStart;
    for I := 0 to N - 1 do
      network.Execute(img);
    SWStop;
  finally
    network.Free;
  end;
end;

initialization

  RegisterTest(TNetTests.Suite);

end.
