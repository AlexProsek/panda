unit panda.Filters.PTests.TVFilter;

interface

uses
    TestFramework
  , panda.Intfs
  , panda.Arrays
  , panda.Filters.TVFilter
  , panda.Tests.NDATestCase
  ;

type
  TTVFilterTests = class(TNDAPerformanceTestCase)
  published
    procedure TVFilter2D_500x500_F32;
  end;

implementation

{$region 'TTVFilterTests'}

procedure TTVFilterTests.TVFilter2D_500x500_F32;
var f: TTVFilter2DF32;
    src, dst: INDArray<Single>;
begin
  f := TTVFilter2DF32.Create;
  try
    f.Init(500, 500);
    f.Lambda := 1;
    f.IterationCount := 100;
    src := TNDAUt.Full<Single>([500, 500], 0);
    dst := TNDAUt.Full<Single>([500, 500], 0);

    SWStart;
    f.Execute(src, dst);
    SWStop;
  finally
    f.Free;
  end;
end;

{$endregion}

initialization

  RegisterTest(TTVFilterTests.Suite);

end.
