program Demo05;

uses
  Vcl.Forms,
  Unit05 in 'Unit05.pas' {Form5},
  panda.ImgProc.VCLImages in '..\..\ImgProc\panda.ImgProc.VCLImages.pas';

{$R *.res}

begin
{$ifdef MSWINDOWS}
{$WARN SYMBOL_PLATFORM OFF}
  if DebugHook <> 0 then ReportMemoryLeaksOnShutDown := True;
{$endif}

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm5, Form5);
  Application.Run;
end.
