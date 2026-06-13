unit panda.ImgProc.Morph;

interface

uses
    panda.ImgProc.Types
  , panda.ImgProc.Images
  , panda.ImgProc.Arithmetic
  , panda.Filters.OrderStatFilters
  ;

procedure Dilation(const aSrc: IImage<Byte>; var aDst: IImage<Byte>; aRadius: Integer); overload;
procedure Erosion(const aSrc: IImage<Byte>; var aDst: IImage<Byte>; aRadius: Integer); overload;
procedure Opening(const aSrc: IImage<Byte>; var aDst: IImage<Byte>; aRadius: Integer); overload;
procedure Closing(const aSrc: IImage<Byte>; var aDst: IImage<Byte>; aRadius: Integer); overload;
procedure MorphGradient(const aSrc: IImage<Byte>; var aDst: IImage<Byte>; aRadius: Integer); overload;
procedure TopHat(const aSrc: IImage<Byte>; var aDst: IImage<Byte>; aRadius: Integer); overload;
procedure BottomHat(const aSrc: IImage<Byte>; var aDst: IImage<Byte>; aRadius: Integer); overload;

implementation

procedure Dilation(const aSrc: IImage<Byte>; var aDst: IImage<Byte>; aRadius: Integer);
var f: TBoxMaxFilter2DUI8;
begin
  Assert(Assigned(aSrc));

  if not Assigned(aDst) then
    aDst := TNDAImg<Byte>.Create(aSrc.Width, aSrc.Height);
  Assert((aDst.Width = aSrc.Width) and (aDst.Height = aSrc.Height));

  f := TBoxMaxFilter2DUI8.Create;
  try
    f.HRadius := aRadius;
    f.VRadius := aRadius;
    f.Execute(aSrc.Data, aDst.Data, aSrc.WidthStep, aDst.WidthStep, aSrc.Width, aSrc.Height);
  finally
    f.Free;
  end;
end;

procedure Erosion(const aSrc: IImage<Byte>; var aDst: IImage<Byte>; aRadius: Integer);
var f: TBoxMinFilter2DUI8;
begin
  Assert(Assigned(aSrc));

  if not Assigned(aDst) then
    aDst := TNDAImg<Byte>.Create(aSrc.Width, aSrc.Height);
  Assert((aDst.Width = aSrc.Width) and (aDst.Height = aSrc.Height));

  f := TBoxMinFilter2DUI8.Create;
  try
    f.HRadius := aRadius;
    f.VRadius := aRadius;
    f.Execute(aSrc.Data, aDst.Data, aSrc.WidthStep, aDst.WidthStep, aSrc.Width, aSrc.Height);
  finally
    f.Free;
  end;
end;

procedure Opening(const aSrc: IImage<Byte>; var aDst: IImage<Byte>; aRadius: Integer);
var tmp: IImage<Byte>;
begin
  Erosion(aSrc, tmp, aRadius);
  Dilation(tmp, aDst, aRadius);
end;

procedure Closing(const aSrc: IImage<Byte>; var aDst: IImage<Byte>; aRadius: Integer);
var tmp: IImage<Byte>;
begin
  Dilation(aSrc, tmp, aRadius);
  Erosion(tmp, aDst, aRadius);
end;

procedure MorphGradient(const aSrc: IImage<Byte>; var aDst: IImage<Byte>; aRadius: Integer);
var tmp: IImage<Byte>;
begin
  Dilation(aSrc, aDst, aRadius);
  Erosion(aSrc, tmp, aRadius);
  ImageSubtract(aDst, tmp, aDst);
end;

procedure TopHat(const aSrc: IImage<Byte>; var aDst: IImage<Byte>; aRadius: Integer);
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

procedure BottomHat(const aSrc: IImage<Byte>; var aDst: IImage<Byte>; aRadius: Integer);
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
