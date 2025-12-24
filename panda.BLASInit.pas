unit panda.BLASInit;

interface

implementation

{$ifdef BLAS}

uses
    LibCBLAS
  , System.SysUtils
  , System.IOUtils
  ;

procedure LoadPandaBLAS;
var s: String;
begin
  s := GetEnvironmentVariable('PANDA_BLAS');
{$if defined(Win32)}
  s := TPath.Combine(s, 'Win32\libopenblas.dll');
{$elseif defined(Win64)}
  s := TPath.Combine(s, 'Win64\libopenblas.dll');
{$else}
   s := '';
{$endif}
  if not TFile.Exists(s) then
    raise Exception.Create('BLAS not found.'#13#10 +
      'Please set PANDA_BLAS environment variable.'
    );

  LoadLibCBLAS(s);
end;

initialization

  LoadPandaBLAS;

finalization

  FreeLibCBLAS;

{$endif}

end.
