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
    procedure Init(aElSz: Integer); virtual;
  public
    destructor Destroy; override;
    function Data: PByte; override;
    function GetBitmap: TBitmap;
  end;

  TBmp<T> = class(TBmp, IImage<T>)
  protected
    function GetItemType: PTypeInfo; override;
  end;

  TBmpUI8 = class(TBmp<Byte>)
  public
    constructor Create(const aBmp: TBitmap); overload;
    constructor Create(aW, aH: Integer); overload;
  end;

  TBmpRGB24 = class(TBmp<TRGB24>)
  public
    constructor Create(const aBmp: TBitmap); overload;
    constructor Create(aW, aH: Integer); overload;
  end;

implementation

{$region 'TBmp'}

destructor TBmp.Destroy;
begin
  fBmp.Free;
  inherited;
end;

procedure TBmp.Init(aElSz: Integer);
begin
  fW := fBmp.Width;
  fH := fBmp.Height;
  fWStep := ((aElSz * fW + 3) div 4) * 4;
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

{$region 'TBmpUI8'}

constructor TBmpUI8.Create(const aBmp: TBitmap);
begin
  Assert(fBmp.PixelFormat = pf8bit);
  fBmp := aBmp;
  Init(SizeOf(Byte));
end;

constructor TBmpUI8.Create(aW, aH: Integer);
begin
  fBmp := TBitmap.Create(aW, aH);
  fBmp.PixelFormat := pf8bit;
  Init(SizeOf(Byte));
end;

{$endregion}

{$region 'TBmpRGB24'}

constructor TBmpRGB24.Create(const aBmp: TBitmap);
begin
  Assert(aBmp.PixelFormat = pf24bit);
  fBmp := aBmp;
  Init(SizeOf(TRGB24));
end;

constructor TBmpRGB24.Create(aW, aH: Integer);
begin
  fBmp := TBitmap.Create(aW, aH);
  fBmp.PixelFormat := pf24bit;
  fBmp.SetSize(aW, aH);
  Init(SizeOf(TRGB24));
  fFlags := NDAF_WRITEABLE;
end;

{$endregion}

end.
