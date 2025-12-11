unit panda.Utils.StopWatch;
// inspired by .Net and http://delphi.about.com/od/windowsshellapi/a/delphi-high-performance-timer-tstopwatch.htm

interface

uses
    SysUtils
  , DateUtils
  , Windows
  , Generics.Collections
  ;

type 
  TStopWatch = class
  private type
    TCountStack = TStack<TLargeInteger>;
  {$ifndef FPC}
    TMsgProc = TProc<Double>;
  {$else}
    TMsgProc = procedure (const aElapsedTime: Double) of object;
  {$endif}
  {$ifdef FPC}
  private const
    MSecsPerSec = 1000;
  {$endif}
  private
    fFrequency : TLargeInteger;
    fIsRunning: boolean;
    fIsHighResolution: boolean;
    fStartCount, fStopCount : TLargeInteger;
    fCountsStack : TCountStack;
    fMsgProv: TMsgProc;
    fPrefix: String;
    procedure SetTickStamp(var lInt : TLargeInteger) ;
    function GetElapsedTicks: TLargeInteger;
    function GetElapsedMiliseconds: TLargeInteger;
    function GetElapsedMilisecondsEx: Extended;
    function GetCurrentElapsedEx: Extended;
    function GetStackDepth: Integer;
{$ifdef FPC}
    procedure OutputMessage(const aElapsedTime: Double);
{$endif}
  public
    constructor Create(const startOnCreate : boolean = false) ;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    procedure Reset;
    procedure ElapsedTimeMsg(const aPrefix: String = '');

    property IsHighResolution : boolean read fIsHighResolution;
    property ElapsedTicks : TLargeInteger read GetElapsedTicks;
    property ElapsedMiliseconds : TLargeInteger read GetElapsedMiliseconds;
    property ElapsedMilisecondsEx : Extended read GetElapsedMilisecondsEx;
    property CurrentElapsedEx: Extended read GetCurrentElapsedEx;
    property IsRunning : boolean read fIsRunning;
    /// <summary>
    ///   <c>MsgProvider</c> is an anonymous function. Its parameter is
    ///   elapsed time in miliseconds.
    /// </summary>
    property MsgProvider: TMsgProc read fMsgProv write fMsgProv;
    property StackDepth: Integer read GetStackDepth;
  end;

implementation

constructor TStopWatch.Create(const startOnCreate : boolean = false) ;
begin
  inherited Create;
  fCountsStack := TCountStack.Create;
  fCountsStack.Capacity := 20;

  fIsRunning := false;

  fIsHighResolution := QueryPerformanceFrequency(fFrequency) ;
  if not fIsHighResolution then fFrequency := MSecsPerSec;

{$ifndef FPC}
  fMsgProv :=
    procedure (aElapsedTime: Double)
    var s: String;
    begin
      s := '';
      if fPrefix <> '' then s := fPrefix + ': ';
      s := s + 'Elapsed time = %f [ms].';
      OutputDebugString(PChar(Format(s, [aElapsedTime])));
    end;
{$else}
   fMsgProv := OutputMessage;
{$endif}

  if startOnCreate then Start;
end;

destructor TStopWatch.Destroy;
begin
  inherited;
  fCountsStack.Free;
end;

{$ifdef FPC}

procedure TStopWatch.OutputMessage(const aElapsedTime: Double);
var s: String;
begin
  s := '';
  if fPrefix <> '' then s := fPrefix + ': ';
  s := s + 'Elapsed time = %f [ms].';
{$ifdef FPC}
  OutputDebugString(PAnsiChar(UnicodeFormat(s, [aElapsedTime])));
{$else}
  OutputDebugString(PChar(Format(s, [aElapsedTime])));
{$endif}
end;

{$endif}

function TStopWatch.GetElapsedTicks: TLargeInteger;
begin
  result := fStopCount - fStartCount;
end;

procedure TStopWatch.SetTickStamp(var lInt : TLargeInteger) ;
begin
  if fIsHighResolution then
    QueryPerformanceCounter(lInt)
  else
    lInt := MilliSecondOf(Now) ;
end;

function TStopWatch.GetElapsedMilisecondsEx: Extended;
begin
  result := (MSecsPerSec * (fStopCount - fStartCount)) / fFrequency;
end;

function TStopWatch.GetCurrentElapsedEx: Extended;
var count: TLargeInteger;
begin
  SetTickStamp(count) ;
  fStartCount := fCountsStack.Peek;
  result := (MSecsPerSec * (count - fStartCount)) / fFrequency;
end;

function TStopWatch.GetElapsedMiliseconds: TLargeInteger;
begin
  result := (MSecsPerSec * (fStopCount - fStartCount)) div fFrequency;
end;

procedure TStopWatch.Start;
var count : TLargeInteger;
begin
  SetTickStamp(count);
  fCountsStack.Push(count);
  fIsRunning := true;
end;

procedure TStopWatch.Stop;
begin
  SetTickStamp(fStopCount);
  fStartCount := fCountsStack.Pop;
  fIsRunning := (fCountsStack.Count > 0);
end;

procedure TStopWatch.Reset;
var count: TLargeInteger;
begin
  if fIsRunning then begin
    fCountsStack.Pop;
    SetTickStamp(count);
    fCountsStack.Push(count);
  end;
end;

procedure TStopWatch.ElapsedTimeMsg(const aPrefix: String);
begin
  fPrefix := aPrefix;
  if Assigned(fMsgProv) then fMsgProv(ElapsedMilisecondsEx);
end;

function TStopWatch.GetStackDepth: Integer;
begin
  Result := fCountsStack.Count;
end;

end.
