unit Unit09;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  VCLTee.TeEngine, VCLTee.TeeProcs, VCLTee.Chart, VCLTee.Series, Vcl.ComCtrls

  , panda.Intfs
  , panda.Arrays
  , panda.MAT4io
  , panda.Nums
  , panda.fft, Vcl.Samples.Spin
  ;

type
  TForm9 = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    FileOpenDialog1: TFileOpenDialog;
    Chart1: TChart;
    Splitter1: TSplitter;
    Chart2: TChart;
    Button2: TButton;
    Series1: TLineSeries;
    Series2: TLineSeries;
    StatusBar1: TStatusBar;
    Panel2: TPanel;
    Splitter2: TSplitter;
    Chart3: TChart;
    Series3: TLineSeries;
    Button3: TButton;
    Button4: TButton;
    Label1: TLabel;
    StaticText1: TStaticText;
    Label2: TLabel;
    edLoFreq: TSpinEdit;
    Label3: TLabel;
    edHiFreq: TSpinEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
    fSrc: TArray<Double>;
    fSpectrum: INDArray<TCmplx128>;
    procedure ShowFFT;
  public
    { Public declarations }
  end;

var
  Form9: TForm9;

implementation

{$R *.dfm}

procedure TForm9.Button1Click(Sender: TObject);
var data: TArray<INDArray>;
begin
  if FileOpenDialog1.Execute then begin
    data := MAT4Load(FileOpenDialog1.FileName);

  end;
end;

procedure TForm9.Button2Click(Sender: TObject);
var fft: TRealFFTEvalF64;
    src: INDArray<Double>;
begin
  fft := TRealFFTEvalF64.Create;
  try
    src := TDynArrWrapper<Double>.Create(fSrc);
    fft.Init(src.Shape[0]);
    fft.Execute(src, fSpectrum);

    ShowFFT;
  finally
    fft.Free;
  end;
end;

procedure TForm9.Button3Click(Sender: TObject);
var I: Integer;
begin
  for I := 0 to High(fSrc) do
    fSrc[I] := fSrc[I] + Random();

  Chart1.Series[0].Clear;
  Chart1.Series[0].AddArray(fSrc);
end;

procedure TForm9.Button4Click(Sender: TObject);
var s: INDArray<TCmplx128>;
    ifft: TRealIFFTEvalF64;
    dst: INDArray<Double>;
    arr: TArray<Double>;
    lof, hif: Integer;
begin
  lof := edLoFreq.Value;
  hif := edHiFreq.Value;
  if not ((0 <= lof) and (lof < hif) and (hif < fSpectrum.Shape[0])) then begin
    MessageDlg('Low and high frequencies don''t have valid values.', mtError, [mbOk], 0);
    exit;
  end;

  s := TNDAUt.Copy<TCmplx128>(fSpectrum);
  s[[NDISpan(0, lof)]] := TNDAUt.Scalar<TCmplx128>(0);
  s[[NDISpan(hif, -1)]] := TNDAUt.Scalar<TCmplx128>(0);

  ifft := TRealIFFTEvalF64.Create;
  try
    ifft.Init(Length(fSrc));
    ifft.Execute(s, dst);
  finally
    ifft.Free;
  end;

  if TNDAUt.TryAsDynArray<Double>(dst, arr) then begin
    Chart3.Series[0].Clear;
    Chart3.Series[0].AddArray(arr);
  end;
end;

procedure TForm9.FormCreate(Sender: TObject);
var I: Integer;
const N = 3840;  // 2^8*3*5
begin
  SetLength(fSrc, N);
  for I := 0 to N - 1 do begin
    fSrc[I] := Cos(2*Pi*I/N) + 1/2*Sin(16*Pi*I/N) + 1/4*Cos(64*Pi*I/N) +
      1/8*Sin(256*Pi*I/N) + 1/16*Cos(512*Pi*I/N);
  end;

  Chart1.Series[0].Clear;
  Chart1.Series[0].AddArray(fSrc);

  Chart3.Series[0].Clear;

  Button2Click(nil);
end;

procedure TForm9.ShowFFT;
var ffti: TNDAVecItems<TCmplx128>;
    I: Integer;
begin
  with Chart2.Series[0] do begin
    Clear;
    if not Assigned(fSpectrum) then exit;
    
    BeginUpdate;
    try
      ffti := fSpectrum;
      for I := 0 to ffti.Length - 1 do
        Add(ffti[I].Abs);
    finally
      EndUpdate;
    end;
  end;
end;

end.
