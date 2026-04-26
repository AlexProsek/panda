unit panda.ImgProc.Types;

interface

uses
    panda.Intfs
  , System.TypInfo
  ;

type
  // TRGB24 is compatible with Windows.Winapi.TRGBTriple
  TRGB24 = packed record
    B, G, R: Byte;
  end;
  PRGB24 = ^TRGB24;

  TRGB48 = packed record
    B, G, R: Word;
  end;
  PRGB48 = ^TRGB48;

  TRGB32 = packed record
    B, G, R, A: Byte;
    function IsGray: Boolean; inline;
  end;
  PRGB32 = ^TRGB32;

  IImage = interface
  ['{04B438CC-D398-4467-9978-C5DCDB87D91E}']
    function GetWidth: NativeInt;
    function GetHeight: NativeInt;
    function GetWidthStep: NativeInt;
    function Data: PByte;
    function GetItemType: PTypeInfo;
    procedure SetFlags(aValue: Cardinal);
    function GetFlags: Cardinal;

    property Width: NativeInt read GetWidth;
    property Height: NativeInt read GetHeight;
    property WidthStep: NativeInt read GetWidthStep;
    property Flags: Cardinal read GetFlags write SetFlags;
  end;

  IImage<T> = interface(IImage)
  ['{4D630977-E5FD-40B4-B22A-AE5F77CB2FD1}']
  end;

function RGB24(aR, aG, aB: Byte): TRGB24; inline;

implementation

function RGB24(aR, aG, aB: Byte): TRGB24;
begin
  with Result do begin
    R := aR;
    G := aG;
    B := aB;
  end;
end;

{$region 'TRGB32'}

function TRGB32.IsGray: Boolean;
begin
  Result := (R = G) and (G = B);
end;

{$endregion}

end.
