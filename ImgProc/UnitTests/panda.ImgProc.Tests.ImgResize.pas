unit panda.ImgProc.Tests.ImgResize;

interface

uses
    TestFramework
  , panda.Intfs
  , panda.Arrays
  , panda.ArrManip
  , panda.ImgProc.Types
  , panda.ImgProc.ImgResize
  , panda.ImgProc.Images
  , panda.Tests.NDATestCase
  ;

type
  TImgResizeTests = class(TNDATestCase)
  published
    procedure ResizeNN_UI8;
    procedure ResizeNN_RGB24;

    procedure ResizeBL_UI8;
    procedure ResizeBL_RGB24;
  end;

implementation

{$region 'TImgResizeTests'}

procedure TImgResizeTests.ResizeNN_UI8;
var src, dst: IImage<Byte>;
    m: TArray<TArray<Byte>>;
begin
  src := TImgUt.AsImage<Byte>([[1, 3], [3, 5]]);
  dst := TNDAImg<Byte>.Create(3, 3);

  ImageResize(src, dst, imNN);

  m := TImgUt.AsDynArray<Byte>(dst);
  CheckEquals(3, Length(m));
  CheckEquals([1, 1, 3], m[0]);
  CheckEquals([1, 1, 3], m[1]);
  CheckEquals([3, 3, 5], m[2]);
end;

procedure TImgResizeTests.ResizeNN_RGB24;
var src, dst: IImage<TRGB24>;
    ch: INDArray<Byte>;
    m: TArray<TArray<Byte>>;
begin
  src := TImgUt.AsImage<TRGB24>([
    [RGB24(1,2,3), RGB24(3,4,5)],
    [RGB24(3,4,5), RGB24(5,6,7)]
  ]);
  dst := TNDAImg<TRGB24>.Create(3, 3);

  ImageResize(src, dst, imNN);

  ch := TImgUt.AsArray<TRGB24, Byte>(dst);
  ch := TNDAMan.Transpose<Byte>(ch, [2, 0, 1]);
  CheckTrue(TNDAUt.TryAsDynArray2D<Byte>(ch[[NDI(2)]], m)); //2 - Red channel

  CheckEquals(3, Length(m));
  CheckEquals([1, 1, 3], m[0]);
  CheckEquals([1, 1, 3], m[1]);
  CheckEquals([3, 3, 5], m[2]);

  CheckTrue(TNDAUt.TryAsDynArray2D<Byte>(ch[[NDI(1)]], m)); //1 - Green channel

  CheckEquals(3, Length(m));
  CheckEquals([2, 2, 4], m[0]);
  CheckEquals([2, 2, 4], m[1]);
  CheckEquals([4, 4, 6], m[2]);

  CheckTrue(TNDAUt.TryAsDynArray2D<Byte>(ch[[NDI(0)]], m)); //0 - Blue channel

  CheckEquals(3, Length(m));
  CheckEquals([3, 3, 5], m[0]);
  CheckEquals([3, 3, 5], m[1]);
  CheckEquals([5, 5, 7], m[2]);
end;

procedure TImgResizeTests.ResizeBL_UI8;
var src, dst: IImage<Byte>;
    m: TArray<TArray<Byte>>;
begin
  src := TImgUt.AsImage<Byte>([[1, 3], [3, 5]]);
  dst := TNDAImg<Byte>.Create(3, 3);

  ImageResize(src, dst);

  m := TImgUt.AsDynArray<Byte>(dst);
  CheckEquals(3, Length(m));
  CheckEquals([1, 2, 3], m[0]);
  CheckEquals([2, 3, 4], m[1]);
  CheckEquals([3, 4, 5], m[2]);
end;

procedure TImgResizeTests.ResizeBL_RGB24;
var src, dst: IImage<TRGB24>;
    ch: INDArray<Byte>;
    m: TArray<TArray<Byte>>;
begin
  src := TImgUt.AsImage<TRGB24>([
    [RGB24(1,2,3), RGB24(3,4,5)],
    [RGB24(3,4,5), RGB24(5,6,7)]
  ]);
  dst := TNDAImg<TRGB24>.Create(3, 3);

  ImageResize(src, dst);

  ch := TImgUt.AsArray<TRGB24, Byte>(dst);
  ch := TNDAMan.Transpose<Byte>(ch, [2, 0, 1]);
  CheckTrue(TNDAUt.TryAsDynArray2D<Byte>(ch[[NDI(2)]], m));

  CheckEquals(3, Length(m));
  CheckEquals([1, 2, 3], m[0]);
  CheckEquals([2, 3, 4], m[1]);
  CheckEquals([3, 4, 5], m[2]);

  CheckTrue(TNDAUt.TryAsDynArray2D<Byte>(ch[[NDI(1)]], m));

  CheckEquals(3, Length(m));
  CheckEquals([2, 3, 4], m[0]);
  CheckEquals([3, 4, 5], m[1]);
  CheckEquals([4, 5, 6], m[2]);

  CheckTrue(TNDAUt.TryAsDynArray2D<Byte>(ch[[NDI(0)]], m));

  CheckEquals(3, Length(m));
  CheckEquals([3, 4, 5], m[0]);
  CheckEquals([4, 5, 6], m[1]);
  CheckEquals([5, 6, 7], m[2]);
end;

{$endregion}


initialization

  RegisterTest(TImgResizeTests.Suite);

end.
