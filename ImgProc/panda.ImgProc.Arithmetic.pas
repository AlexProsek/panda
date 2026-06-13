unit panda.ImgProc.Arithmetic;

interface

uses
    panda.ImgProc.Types
  , panda.ImgProc.Images
  , panda.cvArithmetic
  , panda.Nums
  ;

// aRes <- A - B
procedure ImageSubtract(const A, B: IImage<Byte>; var aRes: IImage<Byte>);

implementation

procedure ImageSubtract(const A, B: IImage<Byte>; var aRes: IImage<Byte>);
var I, w, h: NativeInt;
    pA, pB, pRes: PByte;
    AWs, BWs, ResWs: NativeInt;
begin
  Assert(Assigned(A) and Assigned(B));

  w := A.Width;
  h := A.Height;
  if not Assigned(aRes) then
    aRes := TNDAImg<Byte>.Create(w, h);
  Assert((w = B.Width) and (h = B.Height) and (w = aRes.Width) and (h = aRes.Height));

  pA := A.Data;
  pB := B.Data;
  pRes := aRes.Data;
  AWs := A.WidthStep;
  BWs := B.WidthStep;
  ResWs := aRes.WidthStep;
  for I := 0 to h - 1 do begin
    VecSubWithSat(PUInt8(pA), PUInt8(pB), PUInt8(pRes), w);
    Inc(pRes, ResWs);
    Inc(pA, AWs);
    Inc(pB, AWs);
  end;
end;

end.
