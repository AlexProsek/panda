unit panda.Demos.Utils;

interface

uses
    Vcl.Graphics
  , panda.Intfs
  , panda.Arrays
  ;

type
  TRGB24 = packed record
    R, G, B: Byte;
  end;

  TImageRGB24 = class(TNDA<TRGB24>)
  protected
    fBmp: TBitmap;
    procedure Init;
  public
    constructor Create(const aBmp: TBitmap); overload;
    constructor Create(aW, aH: Integer); overload;
    destructor Destroy; override;
    function Data: PByte; override;
    function RowWidth: Integer;

    property Bitmap: TBitmap read fBmp;
  end;

implementation

{$region 'TImageRGB24'}

constructor TImageRGB24.Create(const aBmp: TBitmap);
begin
  fBmp := aBmp;
  Init;
end;

constructor TImageRGB24.Create(aW, aH: Integer);
begin
  fBmp := TBitmap.Create(aW, aH);
  fBmp.PixelFormat := pf24bit;
  Init;
end;

procedure TImageRGB24.Init;
begin
  fShape := TArray<NativeInt>.Create(fBmp.Height, fBmp.Width);
  SetLength(fStrides, 2);
  fStrides[0] := RowWidth;
  fStrides[1] := SizeOf(TRGB24);
end;

destructor TImageRGB24.Destroy;
begin
  fBmp.Free;
  inherited;
end;

function TImageRGB24.Data: PByte;
begin
  Result := fBmp.ScanLine[fBmp.Height - 1];
end;

function TImageRGB24.RowWidth: Integer;
begin
  Result := ((3 * fBmp.Width + 3) div 4) * 4;
end;

{$endregion}

end.
