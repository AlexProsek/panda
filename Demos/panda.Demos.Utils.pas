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

  TRGB24Channels = class(TNDA<Byte>)
  protected
    fImg: INDArray<TRGB24>;
  public
    constructor Create(const aImg: INDArray<TRGB24>); overload;
    function Data: PByte; override;
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

{$region 'TRGB24Channels'}

constructor TRGB24Channels.Create(const aImg: INDArray<TRGB24>);
var count: Integer;
    imgSh, imgStr: TArray<NativeInt>;
begin
  Assert(aImg.NDim = 2);
  fImg := aImg;
  count := fImg.NDim;
  SetLength(fShape, count + 1);
  SetLength(fStrides, count + 1);
  imgSh := aImg.Shape;
  imgStr := aImg.Strides;
  Move(imgSh[0], fShape[0], count * SizeOf(NativeInt));
  Move(imgStr[0], fStrides[0], count * SizeOf(NativeInt));
  fShape[count] := 3;
  fStrides[count] := 1;
  fFlags := NDAF_WRITEABLE;
  if (fShape[1] mod 4) = 0 then
    fFlags := fFlags or NDAF_C_CONTIGUOUS;
end;

function TRGB24Channels.Data: PByte;
begin
  Result := fImg.Data;
end;

{$endregion}

end.
