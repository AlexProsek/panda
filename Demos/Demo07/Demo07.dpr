program Demo07;

uses
  Vcl.Forms,
  Unit07 in 'Unit07.pas' {Form7},
  panda.ImgProc.Filters in '..\..\ImgProc\panda.ImgProc.Filters.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm7, Form7);
  Application.Run;
end.
