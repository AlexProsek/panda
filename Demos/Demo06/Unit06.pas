unit Unit06;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCLTee.TeEngine, VCLTee.TeeProcs,
  VCLTee.Chart, Vcl.ExtCtrls, VCLTee.Series, Vcl.StdCtrls, Vcl.ComCtrls
  , System.Diagnostics, Vcl.Samples.Spin
  ;

type
  TForm6 = class(TForm)
    pnTop: TPanel;
    Chart1: TChart;
    Series1: TFastLineSeries;
    btApply: TButton;
    Series2: TFastLineSeries;
    StatusBar1: TStatusBar;
    cbFilter: TComboBox;
    Label1: TLabel;
    lbR: TLabel;
    edRadius: TSpinEdit;
    lbLambda: TLabel;
    edLambda: TEdit;
    Label2: TLabel;
    cbSignal: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure btApplyClick(Sender: TObject);
    procedure cbFilterChange(Sender: TObject);
    procedure cbSignalChange(Sender: TObject);
  private
    { Private declarations }
    fX, fY: TArray<Double>;
    fSW: TStopwatch;
    procedure SWStart;
    procedure SWStop;
    procedure ExecuteMinFilter;
    procedure ExecuteMaxFilter;
    procedure ExecuteMedianFilter;
    procedure ExecuteTVFilter;
    procedure MakeTrigSig;
    procedure MakeSqrSig;
    procedure UpdateSourceSeries;
    procedure ShowResult(const aData: TArray<Double>);
  public
    { Public declarations }
  end;

var
  Form6: TForm6;

implementation

uses
    System.Math
  , panda.Intfs
  , panda.Arrays
  , panda.Filters.OrderStatFilters
  , panda.Filters.TVFilter
  ;

{$R *.dfm}

const
  // filter types
  FIDX_MIN    = 0;
  FIDX_MAX    = 1;
  FIDX_MEDIAN = 2;
  FIDX_TV     = 3;

  // signal types
  SIDX_TRIG   = 0;
  SIDX_SQR    = 1;

procedure TForm6.SWStart;
begin
  fSW.Reset;
  fSW.Start;
end;

procedure TForm6.SWStop;
begin
  fSW.Stop;
  StatusBar1.Panels[0].Text := Format('Elapsed time: %f [ms]',
    [fSW.Elapsed.TotalMilliseconds]
  );
end;

procedure TForm6.ShowResult(const aData: TArray<Double>);
begin
  Series2.BeginUpdate;
  try
    Series2.Clear;
    Series2.AddArray(fX, aData);
  finally
    Series2.EndUpdate;
  end;
end;

procedure TForm6.ExecuteMinFilter;
var f: TMinFilter1DF64;
    y: TArray<Double>;
begin
  f := TMinFilter1DF64.Create;
  try
    SetLength(y, Length(fY));
    f.Radius := edRadius.Value;

    SWStart;
    f.Execute(PByte(fY), PByte(y), Length(fY));
    SWStop;

    ShowResult(y);
  finally
    f.Free;
  end;
end;

procedure TForm6.cbFilterChange(Sender: TObject);
begin
  case cbFilter.ItemIndex of
    FIDX_MIN, FIDX_MAX, FIDX_MEDIAN: begin
      lbR.Caption := 'Radius:';
      lbLambda.Visible := False;
      edLambda.Visible := False;
    end;
    FIDX_TV: begin
      lbR.Caption := 'Iterations:';
      lbLambda.Visible := True;
      edLambda.Visible := True;
    end;
  end;
end;

procedure TForm6.cbSignalChange(Sender: TObject);
begin
  case cbSignal.ItemIndex of
    SIDX_TRIG: MakeTrigSig;
    SIDX_SQR:  MakeSqrSig;
  end;
end;

procedure TForm6.ExecuteMaxFilter;
var f: TMaxFilter1DF64;
    y: TArray<Double>;
    sw: TStopWatch;
begin
  f := TMaxFilter1DF64.Create;
  try
    SetLength(y, Length(fY));
    f.Radius := edRadius.Value;

    SWStart;
    f.Execute(PByte(fY), PByte(y), Length(fY));
    SWStop;

    ShowResult(y);
  finally
    f.Free;
  end;
end;

procedure TForm6.ExecuteMedianFilter;
var f: TMedianFilter1DF64;
    y: TArray<Double>;
    sw: TStopWatch;
begin
  f := TMedianFilter1DF64.Create;
  try
    SetLength(y, Length(fY));
    f.Radius := edRadius.Value;

    SWStart;
    f.Execute(PByte(fY), PByte(y), Length(fY));
    SWStop;

    ShowResult(y);
  finally
    f.Free;
  end;
end;

procedure TForm6.ExecuteTVFilter;
var f: TTVFilter1DF32;
    src, dst: INDArray<Single>;
    y: TArray<Double>;
    lambda: Double;
begin
  src := TNDAUt.Cvt<Double, Single>(fY);
  dst := TNDAUt.Empty<Single>([Length(fY)]);

  f := TTVFilter1DF32.Create;
  try
    f.IterationCount := edRadius.Value;
    if not (TryStrToFloat(edLambda.Text, lambda) and (lambda > 0)) then begin
      MessageDlg('Lambda has to be a positive real value.', mtError, [mbOk], 0);
      exit;
    end;
    f.Lambda := lambda;
    f.Init(Length(fY));

    SWStart;
    f.Execute(src, dst);
    SWStop;

    TNDAUt.TryAsDynArray<Double>(TNDAUt.AsType<Double>(dst), y);
    ShowResult(y);
  finally
    f.Free;
  end;
end;

procedure TForm6.btApplyClick(Sender: TObject);
begin
  case cbFilter.ItemIndex of
    FIDX_MIN:     ExecuteMinFilter;
    FIDX_MAX:     ExecuteMaxFilter;
    FIDX_MEDIAN:  ExecuteMedianFilter;
    FIDX_TV:      ExecuteTVFilter;
  end;
end;

procedure TForm6.MakeTrigSig;
var u: Double;
    I: Integer;
const N = 30000;
begin
  SetLength(fX, N);
  SetLength(fY, N);
  for I := 0 to High(fX) do begin
    u := 2*Pi*I/(N - 1);
    fX[I] := u;
    fY[I] := Sin(u) + 1/2*Cos(2*u) + 1/8*Sin(8*u) + RandG(0, 0.1);
  end;

  UpdateSourceSeries;
end;

procedure TForm6.MakeSqrSig;
var u: Double;
    I: Integer;
const N = 30000;
begin
  SetLength(fX, N);
  SetLength(fY, N);
  for I := 0 to High(fX) do begin
    u := 2*Pi*I/(N - 1);
    fX[I] := u;
    fY[I] := 0.5*Sign(Sin(10*u)) + RandG(0, 0.25);
  end;

  UpdateSourceSeries;
end;

procedure TForm6.UpdateSourceSeries;
begin
  Series1.BeginUpdate;
  try
    Series1.Clear;
    Series1.AddArray(fX, fY);
  finally
    Series1.EndUpdate;
  end;
end;

procedure TForm6.FormCreate(Sender: TObject);
begin
  pnTop.Height := 90;
  cbSignalChange(nil);
end;

end.
