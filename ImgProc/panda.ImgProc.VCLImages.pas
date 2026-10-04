unit panda.ImgProc.VCLImages;

interface

uses
    panda.Intfs
  , panda.Arrays
  , panda.ImgProc.Types
  , panda.ImgProc.Images
  , VCL.Graphics
  , System.TypInfo
  ;

type
  IBitmapImage = interface
  ['{B1CF6406-DC3E-4E7F-A9D6-F903C3E50F02}']
    function GetBitmap: TBitmap;

    property Bitmap: TBitmap read GetBitmap;
  end;

  TBmp = class abstract(TNDAImg, IImage, IBitmapImage)
  protected
    fBmp: TBitmap;
    fOwnBmp: Boolean;
    procedure Init(aElSz: Integer); virtual;
  public
    destructor Destroy; override;
    function Data: PByte; override;
    function GetBitmap: TBitmap;
    class function PixelFormat: TPixelFormat; virtual; abstract;
  end;

  TBmp<T> = class(TBmp, IImage<T>)
  protected
    function GetItemType: PTypeInfo; override;
  public
    constructor Create(const aBmp: TBitmap; aOwnBmp: Boolean = True); overload;
    constructor Create(aW, aH: Integer); overload;
  end;

  TBmpUI8 = class(TBmp<Byte>)
  public
    class function PixelFormat: TPixelFormat; override;
  end;

  TBmpRGB24 = class(TBmp<TRGB24>)
  public
    class function PixelFormat: TPixelFormat; override;
  end;

implementation

{$region 'TBmp'}

destructor TBmp.Destroy;
begin
  if fOwnBmp then
    fBmp.Free;
  inherited;
end;

procedure TBmp.Init(aElSz: Integer);
begin
  fW := fBmp.Width;
  fH := fBmp.Height;
  fWStep := ((aElSz * fW + 3) div 4) * 4;

  if fWStep = fW * aElSz then
    fFlags := fFlags or NDAF_C_CONTIGUOUS;
end;

function TBmp.Data: PByte;
begin
  Result := fBmp.ScanLine[fH - 1];
end;

function TBmp.GetBitmap: TBitmap;
begin
  Result := fBmp;
end;

{$endregion}

{$region 'TBmp<T>'}

function TBmp<T>.GetItemType: PTypeInfo;
begin
  Result := TypeInfo(T);
end;

{$endregion}

{$region 'TBmp<T>'}

constructor TBmp<T>.Create(const aBmp: TBitmap; aOwnBmp: Boolean);
begin
  Assert(aBmp.PixelFormat = PixelFormat);
  fBmp := aBmp;
  fOwnBmp := aOwnBmp;
  Init(SizeOf(T));
end;

constructor TBmp<T>.Create(aW, aH: Integer);
begin
  fBmp := TBitmap.Create(aW, aH);
  fBmp.PixelFormat := PixelFormat;
  fBmp.SetSize(aW, aH);
  Init(SizeOf(T));
  fFlags := fFlags or NDAF_WRITEABLE;
end;

{$endregion}

{$region 'TBmpUI8'}

class function TBmpUI8.PixelFormat: TPixelFormat;
begin
  Result := pf8bit;
end;

{$endregion}

{$region 'TBmpRGB24'}

class function TBmpRGB24.PixelFormat: TPixelFormat;
begin
  Result := pf24bit;
end;

{$endregion}

end.
