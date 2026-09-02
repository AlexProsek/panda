unit panda.ImgProc.Morph;

interface

uses
    panda.ImgProc.Types
  , panda.ImgProc.Images
  , panda.ImgProc.Arithmetic
  , panda.ImgProc.Filters
  , panda.Filters.OrderStatFilters
  ;

procedure Dilation(const aSrc: IImage<Byte>; var aDst: IImage<Byte>; aRadius: Integer; aFlags: Cardinal = 0); overload;
procedure Erosion(const aSrc: IImage<Byte>; var aDst: IImage<Byte>; aRadius: Integer; aFlags: Cardinal = 0); overload;
procedure Opening(const aSrc: IImage<Byte>; var aDst: IImage<Byte>; aRadius: Integer; aFlags: Cardinal = 0); overload;
procedure Closing(const aSrc: IImage<Byte>; var aDst: IImage<Byte>; aRadius: Integer; aFlags: Cardinal = 0); overload;
procedure MorphGradient(const aSrc: IImage<Byte>; var aDst: IImage<Byte>; aRadius: Integer; aFlags: Cardinal = 0); overload;
procedure TopHat(const aSrc: IImage<Byte>; var aDst: IImage<Byte>; aRadius: Integer; aFlags: Cardinal = 0); overload;
procedure BottomHat(const aSrc: IImage<Byte>; var aDst: IImage<Byte>; aRadius: Integer; aFlags: Cardinal = 0); overload;

implementation

procedure Dilation(const aSrc: IImage<Byte>; var aDst: IImage<Byte>; aRadius: Integer; aFlags: Cardinal);
begin
  MaxFilter(aSrc, aDst, aRadius, aRadius, aFlags);
end;

procedure Erosion(const aSrc: IImage<Byte>; var aDst: IImage<Byte>; aRadius: Integer; aFlags: Cardinal);
begin
  MinFilter(aSrc, aDst, aRadius, aRadius, aFlags);
end;

procedure Opening(const aSrc: IImage<Byte>; var aDst: IImage<Byte>; aRadius: Integer; aFlags: Cardinal);
var tmp: IImage<Byte>;
begin
  Erosion(aSrc, tmp, aRadius);
  Dilation(tmp, aDst, aRadius);
end;

procedure Closing(const aSrc: IImage<Byte>; var aDst: IImage<Byte>; aRadius: Integer; aFlags: Cardinal);
var tmp: IImage<Byte>;
begin
  Dilation(aSrc, tmp, aRadius);
  Erosion(tmp, aDst, aRadius);
end;

procedure MorphGradient(const aSrc: IImage<Byte>; var aDst: IImage<Byte>; aRadius: Integer; aFlags: Cardinal);
var tmp: IImage<Byte>;
begin
  Dilation(aSrc, aDst, aRadius);
  Erosion(aSrc, tmp, aRadius);
  ImageSubtract(aDst, tmp, aDst);
end;

procedure TopHat(const aSrc: IImage<Byte>; var aDst: IImage<Byte>; aRadius: Integer; aFlags: Cardinal);
var tmp: IImage<Byte>;
begin
  Assert(Assigned(aSrc));

  if not Assigned(aDst) then
    aDst := TNDAImg<Byte>.Create(aSrc.Width, aSrc.Height);
  Assert((aSrc.Width = aDst.Width) and (aSrc.Height = aDst.Height));

  TImgUt.CopyTo<Byte>(aSrc, aDst);
  Opening(aSrc, tmp, aRadius);
  ImageSubtract(aDst, tmp, aDst);
end;

procedure BottomHat(const aSrc: IImage<Byte>; var aDst: IImage<Byte>; aRadius: Integer; aFlags: Cardinal);
var tmp: IImage<Byte>;
begin
  Assert(Assigned(aSrc));

  if not Assigned(aDst) then
    aDst := TNDAImg<Byte>.Create(aSrc.Width, aSrc.Height);
  Assert((aSrc.Width = aDst.Width) and (aSrc.Height = aDst.Height));

  Closing(aSrc, aDst, aRadius);
  ImageSubtract(aDst, aSrc, aDst);
end;

end.
