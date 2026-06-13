program Demo08;

uses
  Vcl.Forms,
  Unit08 in 'Unit08.pas' {Form8},
  panda.ImgProc.Morph in '..\..\ImgProc\panda.ImgProc.Morph.pas',
  panda.ImgProc.Arithmetic in '..\..\ImgProc\panda.ImgProc.Arithmetic.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm8, Form8);
  Application.Run;
end.
