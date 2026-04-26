unit panda.ImgProc.Filters;

interface

uses
    panda.ImgProc.Types
  , panda.ImgProc.Images
  , panda.Filters.OrderStatFilters
  , panda.Filters.TVFilter
  , System.SysUtils
  ;

const
  MFF_PARALLELIZE     = $10000;
  MFF_BOX_KERNEL      = $00001;
  MFF_DIAMOND_KERNEL  = $00002;
  MFF_CIRCLE_KERNEL   = $00003;

procedure MinFilter(const aSrc: IImage<Byte>; var aDst: IImage<Byte>;
  aHRadius: Integer; aVRadius: Integer = -1; const aFlags: Cardinal = 0); overload;
procedure MinFilter(const aSrc: IImage<Byte>; var aDst: IImage<Byte>;
  const aKernel: IImage<Byte>; const aFlags: Cardinal = 0); overload;

procedure MaxFilter(const aSrc: IImage<Byte>; var aDst: IImage<Byte>;
  aHRadius: Integer; aVRadius: Integer = -1; const aFlags: Cardinal = 0); overload;
procedure MaxFilter(const aSrc: IImage<Byte>; var aDst: IImage<Byte>;
  const aKernel: IImage<Byte>; const aFlags: Cardinal = 0); overload;

procedure MedianFilter(const aSrc: IImage<Byte>; var aDst: IImage<Byte>;
  aHRadius: Integer; aVRadius: Integer = -1; aFlags: Cardinal = 0); overload;

procedure TotalVariationFilter(const aSrc: IImage<Single>; var aDst: IImage<Single>;
  aIterCount: Integer; aLambda: Single);

{$region 'low-level functions'}

procedure _ApplyMFilter(aFilterCls: TMFilter2DClass; const aSrc: IImage<Byte>; var aDst: IImage<Byte>;
  const aKernel: IImage<Byte>; const aFlags: Cardinal);

{$endregion}

implementation

const
  MFF_KERNEL_MASK     = 3;
  MFF_KERNEL_DEFAULT  = 0;

{$region 'Min/Max filter'}

procedure _ApplyMFilter(aFilterCls: TMFilter2DClass; const aSrc: IImage<Byte>; var aDst: IImage<Byte>;
  const aKernel: IImage<Byte>; const aFlags: Cardinal);
var f: TMFilter2D;
begin
  Assert(Assigned(aSrc) and Assigned(aKernel));

  if not Assigned(aDst) then
    aDst := TNDAImg<Byte>.Create(aSrc.Width, aSrc.Height);

  f := aFilterCls.Create;
  try
    f.Parallelize := ((aFlags and MFF_PARALLELIZE) <> 0);
    f.Kernel := TNDAImgView<Byte>.Create(aKernel);
    f.Execute(aSrc.Data, aDst.Data, aSrc.WidthStep, aDst.WidthStep, aSrc.Width, aSrc.Height);
  finally
    f.Free;
  end;
end;

procedure MinFilter(const aSrc: IImage<Byte>; var aDst: IImage<Byte>;
  aHRadius, aVRadius: Integer; const aFlags: Cardinal);
var f: TGMFilter2D;
begin
  Assert(Assigned(aSrc) and (aHRadius > 0));

  if not Assigned(aDst) then
    aDst := TNDAImg<Byte>.Create(aSrc.Width, aSrc.Height);
  if aVRadius < 0 then
    aVRadius := aHRadius;

  case aFlags and MFF_KERNEL_MASK of
    MFF_KERNEL_DEFAULT, MFF_BOX_KERNEL: begin
      f := TBoxMinFilter2DUI8.Create;
      with TBoxMinFilter2DUI8(f) do begin
        HRadius := aHRadius;
        VRadius := aVRadius;
      end;
    end;

    MFF_DIAMOND_KERNEL: begin
      f := TMinDCFilter2DUI8.Create;
      with TMinDCFilter2DUI8(f) do begin
        Radius := aHRadius;
        KernelType := ktDiamond;
      end;
    end;

    MFF_CIRCLE_KERNEL: begin
      f := TMinDCFilter2DUI8.Create;
      with TMinDCFilter2DUI8(f) do begin
        Radius := aHRadius;
        KernelType := ktCircle;
      end;
    end
  else
    raise ENotImplemented.CreateFmt('Unknown kernel type %d', [aFlags and MFF_KERNEL_MASK]);
  end;

  try
    f.Parallelize := ((aFlags and MFF_PARALLELIZE) > 0);
    f.Execute(aSrc.Data, aDst.Data, aSrc.WidthStep, aDst.WidthStep, aSrc.Width, aSrc.Height);
  finally
    f.Free;
  end;
end;

procedure MinFilter(const aSrc: IImage<Byte>; var aDst: IImage<Byte>;
  const aKernel: IImage<Byte>; const aFlags: Cardinal);
begin
  _ApplyMFilter(TMinFilter2DUI8, aSrc, aDst, aKernel, aFlags);
end;

procedure MaxFilter(const aSrc: IImage<Byte>; var aDst: IImage<Byte>;
  aHRadius, aVRadius: Integer; const aFlags: Cardinal);
var f: TBoxMaxFilter2DUI8;
begin
  Assert(Assigned(aSrc) and (aHRadius > 0));

  if not Assigned(aDst) then
    aDst := TNDAImg<Byte>.Create(aSrc.Width, aSrc.Height);
  if aVRadius < 0 then
    aVRadius := aHRadius;

  f := TBoxMaxFilter2DUI8.Create;
  try
    f.HRadius := aHRadius;
    f.VRadius := aVRadius;
    f.Execute(aSrc.Data, aDst.Data, aSrc.WidthStep, aDst.WidthStep, aSrc.Width, aSrc.Height);
  finally
    f.Free;
  end;
end;

procedure MaxFilter(const aSrc: IImage<Byte>; var aDst: IImage<Byte>;
  const aKernel: IImage<Byte>; const aFlags: Cardinal);
begin
  _ApplyMFilter(TMaxFilter2DUI8, aSrc, aDst, aKernel, aFlags);
end;

{$endregion}

procedure MedianFilter(const aSrc: IImage<Byte>; var aDst: IImage<Byte>;
  aHRadius, aVRadius: Integer; aFlags: Cardinal);
var f: TMedianFilter2DUI8;
begin
  Assert(Assigned(aSrc) and (aHRadius > 0));

  if not Assigned(aDst) then
    aDst := TNDAImg<Byte>.Create(aSrc.Width, aDst.Width);
  if aVRadius < 0 then
    aVRadius := aHRadius;

  f := TMedianFilter2DUI8.Create;
  try
    f.HRadius := aHRadius;
    f.VRadius := aVRadius;
    f.Execute(aSrc.Data, aDst.Data, aSrc.WidthStep, aDst.WidthStep, aSrc.Width, aSrc.Height);
  finally
    f.Free;
  end;
end;

procedure TotalVariationFilter(const aSrc: IImage<Single>; var aDst: IImage<Single>;
  aIterCount: Integer; aLambda: Single);
var f: TTVFilter2DF32;
begin
  Assert(Assigned(aSrc) and (aIterCount > 0) and (aLambda > 0));

  if not Assigned(aDst) then
    aDst := TNDAImg<Single>.Create(aSrc.Width, aSrc.Height);

  f := TTVFilter2DF32.Create;
  try
    f.IterationCount := aIterCount;
    f.Lambda := aLambda;
    f.Init(aSrc.Width, aSrc.Height);
    f.Execute(TImgUt.AsArray<Single>(aSrc), TImgUt.AsArray<Single>(aDst));
  finally
    f.Free;
  end;
end;

end.
