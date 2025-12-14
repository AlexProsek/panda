unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs
  , panda.Intfs
  , panda.Arrays
  , panda.Arithmetic
  , panda.Math
  , panda.Formatter, Vcl.StdCtrls, Vcl.ExtCtrls
  ;

type
  TForm2 = class(TForm)
    btExecute: TButton;
    mmInput: TMemo;
    Splitter1: TSplitter;
    mmOutput: TMemo;
    Panel1: TPanel;
    ComboBox1: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btExecuteClick(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
  private
    { Private declarations }
    fFmt: TNDAFormatter;
    fParser: TNDAParser;
    procedure ExecuteLUDecomp;
    procedure ExecuteUTLSolve;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure LUDecomp(const A: INDArray<Double>; out aL, aU: INDArray<Double>);
var m, n: NativeInt;
    L, U: TTensorF64;
    k: SNDIntIndex;
begin
  Assert(A.NDim = 2);
  m := A.Shape[0];
  n := A.Shape[1];
  L := TNDAUt.Identity<Double>(n);
  U := TNDAUt.Copy<Double>(A);

  Dec(n);
  for k in NDIRange(0, n - 1) do begin
    L[[NDISpan(k, n), k]] := U[[NDISpan(k, n), k]] / U[[k, k]];
    U[[NDISpan(k + 1, n), NDISpan(k, n)]] := U[[NDISpan(k + 1, n), NDISpan(k, n)]] -
      ndaDot(L[[NDISpan(k + 1, n), NDI([k])]], U[[NDI([k]), NDISpan(k, n)]]);
  end;

  aL := L;
  aU := U;
end;

function UpperTriangulationLinearSolve(const aU, aV: INDArray<Double>): INDArray<Double>;
var i: SNDIntIndex;
    m, n: NativeInt;
    U, v, x: TTensorF64;
begin
  Assert((aU.NDim = 2) and (aV.NDim = 1) and (aU.Shape[1] = aV.Shape[0]));
  m := aU.Shape[0];
  n := aU.Shape[1];
  U := TNDAUt.Copy<Double>(aU);
  v := TNDAUt.Copy<Double>(aV);
  x := ndaRange(0.0, n);

  Dec(n);
  for i in NDIRange(n, 0, -1) do
    x[[i]] := (v[[i]] - ndaDot(U[[i, NDISpan(i + 1, n)]],  x[[NDISpan(i + 1, n)]])) / U[[i, i]];
  Result := x;
end;

procedure TForm2.ExecuteLUDecomp;
var l, u: INDArray<Double>;
    arr: INDArray;
    s: String;
begin
  s := mmInput.Text;
  if not fParser.Parse(s, arr) then begin
    MessageDlg('Invalid array specification.', mtError, [mbOk], 0);
    exit;
  end;
  LUDecomp(arr as INDArray<Double>, l, u);

  mmOutput.Lines.Add(Format('L = %s', [fFmt.GetString(l)]));
  mmOutput.Lines.Add(Format('U = %s', [fFmt.GetString(u)]));
end;

procedure TForm2.ExecuteUTLSolve;
var arrM, arrB: INDArray;
    r: INDArray<Double>;
begin
  if mmInput.Lines.Count <> 2 then begin
    MessageDlg('Input has to contain two lines (matrix and right side vector).',
      mtError, [mbOk], 0);
    exit;
  end;

  if not (
    fParser.Parse(mmInput.Lines[0], arrM) and
    fParser.parse(mmInput.Lines[1], arrB))
  then begin
    MessageDlg('Invalid input specification.', mtError, [mbOk], 0);
    exit;
  end;

  r := UpperTriangulationLinearSolve(arrM as INDArray<Double>, arrB as INDArray<Double>);

  mmOutput.Lines.Add(fFmt.GetString(r));
end;

procedure TForm2.btExecuteClick(Sender: TObject);
begin
  mmOutput.Clear;
  case ComboBox1.ItemIndex of
    0: ExecuteLUDecomp;
    1: ExecuteUTLSolve;
  end;
end;

procedure TForm2.ComboBox1Change(Sender: TObject);
begin
  mmInput.Clear;
  case TComboBox(Sender).ItemIndex of
    0: mmInput.Lines.Add('{{1, 2, 3}, {4, 5, 6}, {7, 8, 9}}');
    1: begin
      mmInput.Lines.Add('{{1, 2, 3}, {0, 4, 5}, {0, 0, 6}}');
      mmInput.Lines.Add('{1, 2, 3}');
    end;
  end;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  fFmt := TNDAFormatter.Create;
  fParser := TNDAParser.Create;
  fParser.ElementType := TypeInfo(Double);
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  fFmt.Free;
  fParser.Free;
end;

end.
