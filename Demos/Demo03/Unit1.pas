unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, SynEdit, Vcl.StdCtrls,
  PythonEngine, SynEditHighlighter, SynEditCodeFolding, SynHighlighterPython,
  Vcl.PythonGUIInputOutput
  , panda.Intfs
  , panda.Demos.Utils
  , pynda.Arrays
  ;

type
  TForm3 = class(TForm)
    edInput: TSynEdit;
    Splitter1: TSplitter;
    PythonEngine1: TPythonEngine;
    SynPythonSyn1: TSynPythonSyn;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    FileOpenDialog1: TFileOpenDialog;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    pnTop: TPanel;
    pnCentre: TPanel;
    btLoadImg: TButton;
    Panel1: TPanel;
    btExec: TButton;
    procedure btExecClick(Sender: TObject);
    procedure btLoadImgClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    fImg: INDArray<TRGB24>;
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

procedure TForm3.btExecClick(Sender: TObject);
var bmp: TBitmap;
begin
  PythonEngine1.ExecStrings(edInput.Lines);
  bmp := TImageRGB24(fImg).Bitmap;
  Image1.SetBounds(0, 0, bmp.Width, bmp.Height);
  Image1.Picture.Assign(bmp);
end;

procedure TForm3.btLoadImgClick(Sender: TObject);
var bmp: TBitmap;
    pyArr, dict: PPyObject;
begin
  if FileOpenDialog1.Execute then begin
    bmp := TBitmap.Create;
    bmp.LoadFromFile(FileOpenDialog1.FileName);
    bmp.PixelFormat := pf24bit;
    Image1.SetBounds(0, 0, bmp.Width, bmp.Height);
    Image1.Picture.Assign(bmp);
    fImg := TImageRGB24.Create(bmp);
    fImg.SetFlags(NDAF_WRITEABLE);

    pyArr := TPyAPI.CreateNumpyArray(fImg);
    with PythonEngine1 do begin
      dict := PyModule_GetDict(GetMainModule);
      PyDict_SetItemString(dict, 'img', pyArr);
      Py_DECREF(pyArr);
    end;

    btExec.Enabled := True;
  end;
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  TPyAPI.Initialize;
end;

procedure TForm3.FormDestroy(Sender: TObject);
begin
  TPyAPI.Finalize;
end;

end.
