unit panda.ImgProc.Images;

interface

uses
    panda.Intfs
  , panda.Arrays
  , panda.ImgProc.Types
  , System.TypInfo
  ;

type
  TNDAImg = class abstract (TInterfacedObject, IImage)
  protected
    fFlags: Cardinal;
    fW, fH, fWStep: NativeInt;
    function GetWidth: NativeInt;
    function GetHeight: NativeInt;
    function GetWidthStep: NativeInt;
    function Data: PByte; virtual; abstract;
    function GetItemType: PTypeInfo; virtual; abstract;
    procedure SetFlags(aValue: Cardinal);
    function GetFlags: Cardinal;
  end;

  TNDAImg<T> = class(TNDAImg, IImage<T>)
  protected
    fData: PByte;
    function GetItemType: PTypeInfo; override;
    function Data: PByte; override;
  public
    constructor Create(const aWidth, aHeight: NativeInt);
    destructor Destroy; override;
  end;

  TNDAImgView<T> = class(TNDA<T>)
  protected
    fImg: IImage<T>;
    fData: PByte;
  public
    constructor Create(const aImg: IImage<T>);
    function Data: PByte; override;
  end;

  TNDArrAsImg<T> = class(TNDAImg<T>)
  protected
    fArr: INDArray<T>;
    fData: PByte;
  public
    constructor Create(const aArr: INDArray<T>);
    function Data: PByte; override;
  end;

  TNDAImgChannels<T, U> = class(TNDA<U>)
  protected
    fImg: IImage<T>;
    fData: PByte;
  public
    constructor Create(const aImg: IImage<T>);
    function Data: PByte; override;
  end;


//  TNDAImgRoi<T> = class(TNDA<T>, IImage, IImage<T>)
//  end;

type
  TImgUt = class
  public
    class function AsArray<T>(const aImg: IImage<T>): INDArray<T>; overload; inline;
    class function AsArray<T, U>(const aImg: IImage<T>): INDArray<U>; overload; inline;
    class function TryAsArray<T>(const aImg: IImage; out aArr: INDArray<T>): Boolean; inline;
    class function AsImage<T>(const aItems: TNDAUt.TOArray2D<T>): IImage<T>; overload; inline;
    class function AsImage<T>(const aArr: INDArray<T>): IImage<T>; overload; inline;
    class function AsDynArray<T>(const aImg: IImage<T>): TArray<TArray<T>>; inline;
    class function ConstantImage<T>(aW, aH: NativeInt; const aValue: T): IImage<T>; inline;
    class procedure CopyTo<T>(const aSrc: IImage<T>; var aDst: IImage<T>); inline;
  end;

function SameSizeQ(const aImg0, aImg1: IImage): Boolean; inline;

implementation

function SameSizeQ(const aImg0, aImg1: IImage): Boolean;
begin
  Result := (aImg0.Width = aImg1.Width) and (aImg0.Height = aImg1.Height);
end;

{$region 'TNDAImg'}

function TNDAImg.GetWidth: NativeInt;
begin
  Result := fW;
end;

function TNDAImg.GetHeight: NativeInt;
begin
  Result := fH;
end;

function TNDAImg.GetWidthStep: NativeInt;
begin
  Result := fWStep;
end;

procedure TNDAImg.SetFlags(aValue: Cardinal);
begin
  fFlags := aValue;
end;

function TNDAImg.GetFlags: Cardinal;
begin
  Result := fFlags;
end;

{$endregion}

{$region 'TNDAImg<T>'}

constructor TNDAImg<T>.Create(const aWidth, aHeight: NativeInt);
begin
  fW := aWidth;
  fH := aHeight;
  fWStep := fW * SizeOf(T);
  GetMem(fData, aHeight * fWStep);
  fFlags := NDAF_COW;
end;

destructor TNDAImg<T>.Destroy;
begin
  FreeMem(fData);
  inherited;
end;

function TNDAImg<T>.GetItemType: PTypeInfo;
begin
  Result := TypeInfo(T);
end;

function TNDAImg<T>.Data: PByte;
begin
  Result := fData;
end;

{$endregion}

{$region 'TNDAImgView<T>'}

constructor TNDAImgView<T>.Create(const aImg: IImage<T>);
begin
  fImg := aImg;
  fData := aImg.Data;
  fShape := TNDAShape.Create(aImg.Height, aImg.Width);
  SetLength(fStrides, 2);
  fStrides[0] := aImg.WidthStep;
  fStrides[1] := SizeOf(T);
  fFlags := aImg.GetFlags and NDAF_MASK;
  if ContiguousQ(SizeOf(T), fShape, fStrides) then
    fFlags := fFlags or NDAF_C_CONTIGUOUS;
end;

function TNDAImgView<T>.Data: PByte;
begin
  Result := fData;
end;

{$endregion}

{$region 'TNDArrAsImg<T>'}

constructor TNDArrAsImg<T>.Create(const aArr: INDArray<T>);
var sh: TNDAShape;
begin
  Assert(aArr.NDim = 2);
  fArr := aArr;
  fData := fArr.Data;
  sh := fArr.Shape;
  fW := sh[1];
  fH := sh[0];
  fWStep := aArr.Strides[0];
  fFlags := aArr.Flags and NDAF_WRITEABLE;
end;

function TNDArrAsImg<T>.Data: PByte;
begin
  Result := fData;
end;

{$endregion}

{$region 'TNDAImgChannels<T, U>'}

constructor TNDAImgChannels<T, U>.Create(const aImg: IImage<T>);
var chCnt: Integer;
begin
  Assert((SizeOf(T) mod SizeOf(U)) = 0);
  fImg := aImg;
  fData := aImg.Data;
  chCnt := SizeOf(T) div SizeOf(U);
  fShape := TNDAShape.Create(aImg.Height, aImg.Width, chCnt);
  SetLength(fStrides, 3);
  fStrides[0] := aImg.WidthStep;
  fStrides[1] := SizeOf(T);
  fStrides[2] := SizeOf(U);
  fFlags := aImg.GetFlags and NDAF_MASK;
  if ContiguousQ(SizeOf(U), fShape, fStrides) then
    fFlags := fFlags or NDAF_C_CONTIGUOUS;
end;

function TNDAImgChannels<T, U>.Data: PByte;
begin
  Result := fData;
end;

{$endregion}

{$region 'TImgUt'}

class function TImgUt.AsArray<T>(const aImg: IImage<T>): INDArray<T>;
begin
  Result := TNDAImgView<T>.Create(aImg);
end;

class function TImgUt.AsArray<T, U>(const aImg: IImage<T>): INDArray<U>;
begin
  Result := TNDAImgChannels<T, U>.Create(aImg);
end;

class function TImgUt.TryAsArray<T>(const aImg: IImage; out aArr: INDArray<T>): Boolean;
begin
  Result := SameQ(aImg.GetItemType, TypeInfo(T));
  if Result then
    aArr := TNDAImgView<T>.Create(aImg as IImage<T>);
end;

class function TImgUt.AsImage<T>(const aItems: TNDAUt.TOArray2D<T>): IImage<T>;
begin
  Result := AsImage<T>(TNDAUt.AsArray<T>(aItems));
end;

class function TImgUt.AsImage<T>(const aArr: INDArray<T>): IImage<T>;
begin
  if aArr.NDim <> 2 then
    raise ENDAShapeError.Create('An image must have dimension 2.');
  Result := TNDArrAsImg<T>.Create(TNDAUt.AsContiguousArray<T>(aArr));
end;

class function TImgUt.AsDynArray<T>(const aImg: IImage<T>): TArray<TArray<T>>;
begin
  if not TNDAUt.TryAsDynArray2D<T>(AsArray<T>(aImg), Result) then
    raise ENDAError.Create('Image conversion to a dynamic array failed.');
end;

class function TImgUt.ConstantImage<T>(aW, aH: NativeInt; const aValue: T): IImage<T>;
begin
  Result := TNDAImg<T>.Create(aW, aH);
  TNDAUt.Fill<T>(AsArray<T>(Result), aValue);
end;

class procedure TImgUt.CopyTo<T>(const aSrc: IImage<T>; var aDst: IImage<T>);
begin
  Assert(Assigned(aSrc) and Assigned(aDst) and SameSizeQ(aSrc, aDst));
  TNDAUt.Fill<T>(AsArray<T>(aSrc), AsArray<T>(aDst));
end;

{$endregion}

end.
