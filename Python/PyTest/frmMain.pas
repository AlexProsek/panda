unit frmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, SynEditHighlighter, SynEditCodeFolding,
  SynHighlighterPython, SynEdit, Vcl.ExtCtrls, Vcl.StdCtrls, PythonEngine,
  Vcl.PythonGUIInputOutput
  , SynEditTypes
  ;

type
  TForm1 = class(TForm)
    edInput: TSynEdit;
    SynPythonSyn1: TSynPythonSyn;
    PythonEngine1: TPythonEngine;
    mmOutput: TMemo;
    Splitter1: TSplitter;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    procedure edInputKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.edInputKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (ssShift in Shift) then begin
    if mmOutput.Lines.Count > 0 then mmOutput.Lines.Add('');
    PythonEngine1.ExecStrings(edInput.Lines);
    edInput.Clear;
    Key := 0;
  end;
end;

type
  TJupyterThread = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TJupyterThread.Execute;
begin
  Form1.PythonEngine1.PyGILstate_Ensure();
  // Run the Python script in a separate thread
  Form1.PythonEngine1.ExecString(
  '''
    import __main__
    __main__.b = 2

    from ipykernel.kernelapp import IPKernelApp
    app = IPKernelApp.instance()
    app.initialize()
    app.start()
    '''
//    'import __main__' + sLineBreak
//    '__main__.b = 2' + sLineBreak +
//    'from ipykernel.kernelapp import IPKernelApp' + sLineBreak +
//    'app = IPKernelApp.instance()' + sLineBreak +
//    'app.initialize()' + sLineBreak +
//    'app.start()'  // This will run in a background thread
  );
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
//  TJupyterThread.Create(False);
end;

end.
