program Demo01;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
{$ifdef MSWINDOWS}
{$WARN SYMBOL_PLATFORM OFF}
  if DebugHook <> 0 then ReportMemoryLeaksOnShutDown := True;
{$endif}

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
