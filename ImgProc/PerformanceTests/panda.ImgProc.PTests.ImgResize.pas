unit panda.ImgProc.PTests.ImgResize;

interface

uses
    TestFramework
  , panda.ImgProc.Types
  , panda.ImgProc.Images
  , panda.ImgProc.ImgResize
  , panda.Tests.NDATestCase
  ;

type
  TImgResizeTests = class(TNDAPerformanceTestCase)
  published
    procedure ResizeBL_UI8;
    procedure ResizeBL_RGB24;
  end;

implementation

{$region 'TImgResizeTests'}

procedure TImgResizeTests.ResizeBL_UI8;
var src, dst: IImage<Byte>;
begin
  src := TImgUt.ConstantImage<Byte>(1000, 1000, 0);
  dst := TImgUt.ConstantImage<Byte>(2000, 2000, 0);

  SWStart;
  ImageResize(src, dst);
  SWStop;
end;

procedure TImgResizeTests.ResizeBL_RGB24;
var src, dst: IImage<TRGB24>;
begin
  src := TImgUt.ConstantImage<TRGB24>(1000, 1000, RGB24(0, 0, 0));
  dst := TImgUt.ConstantImage<TRGB24>(2000, 2000, RGB24(0, 0, 0));

  SWStart;
  ImageResize(src, dst);
  SWStop;
end;

{$endregion}

initialization

  RegisterTest(TImgResizeTests.Suite);

end.
