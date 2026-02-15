unit panda.Tests.NDATestCase;

interface

{$ifdef FPC}
  {$mode delphiunicode}{$H+}
{$endif}

uses
{$ifndef FPC}
    TestFramework
{$else}
    fpcunit
  , testutils
  , testregistry
{$endif}
  , SysUtils
  , Classes
  , IOUtils
  , Forms
  , Windows
  , panda.Nums
  , panda.Intfs
  , panda.Arrays
  , panda.DynArrayUtils
  , panda.Utils.StopWatch
  ;

type
  TNDATestCase = class(TTestCase)
  protected
  {$ifdef FPC}
    procedure Status(const aValue: String);
    function QueryInterface(constref IID: TGUID; out Obj): HResult; virtual; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _AddRef: Integer; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release: Integer; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  {$endif}
    function GetTestDataPath(const aFileName: String): String;
  public
    procedure CheckEquals(const aExpected, aValue: array of NativeInt); overload;
  {$ifdef CPUX64}
    procedure CheckEquals(const aExpected, aValue: array of Integer); overload;
  {$endif}
    procedure CheckEquals(const aExpected, aValue: array of Double; const aTol: Double = 0); overload;
    procedure CheckEquals(const aExpected: array of String; const aValue: array of String); overload;
    procedure CheckEquals(const aExpected, aValue: TCmplx128; const aTol: Double = 0); overload;

    function iRng2NDA(const aShape: array of NativeInt;
      aLo: Integer = 1; aStep: Integer = 1): INDArray<Integer>; overload;
    function sRng2NDA(const aShape: array of NativeInt;
      aLo: Single = 1; aStep: Single = 1): INDArray<Single>; overload;
  {$ifdef FPC}
    class function Suite: TTestCaseClass;
  {$endif}

  {$ifndef FPC}
    property TestName: String read fTestName;
  {$endif}
  end;

  TNDAPerformanceTestCase = class(TNDATestCase)
  protected
    fSW: TStopWatch;
    fLoopCount: Integer;
  protected class var
    fWriter: TStreamWriter;
  public
    class constructor Create;
    class destructor Destroy;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure SWStart;
    procedure SWStop(const aAppend: String = ''; aTimeDelta: Double = 0);
    procedure DoTestLoop(const aTest: TProc; aCount: Integer); overload;
    procedure DoTestLoop(aTest: TTestMethod; aCount: Integer); overload;

    property SW: TStopWatch read fSW;
  end;

implementation

{$region 'TNDATestCase'}

{$ifdef FPC}

procedure TNDATestCase.Status(const aValue: String);
begin
end;

class function TNDATestCase.Suite: TTestCaseClass;
begin
  Result := TTestCaseClass(ClassType);
end;

function TNDATestCase.QueryInterface(constref IID: TGUID; out Obj): HResult; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
const NO_SUCH_INTFARCE: HRESULT = $80004002;
begin
  if GetInterface(IID, Obj) then Result := 0
    else Result := NO_SUCH_INTFARCE;
end;

function TNDATestCase._AddRef: Integer;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := -1;
end;

function TNDATestCase._Release: Integer;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := -1;
end;

{$endif}

function TNDATestCase.GetTestDataPath(const aFileName: String): String;
begin
  Result := TPath.GetDirectoryName(Application.ExeName);
  Result := TPath.Combine(Result, Format('..\..\TestData\%s', [aFileName]));
end;

procedure TNDATestCase.CheckEquals(const aExpected, aValue: array of NativeInt);
var I: Integer;
begin
  CheckEquals(Length(aExpected), Length(aValue));
  for I := 0 to High(aExpected) do
    CheckEquals(aExpected[I], aValue[I]);
end;

{$ifdef CPUX64}
procedure TNDATestCase.CheckEquals(const aExpected, aValue: array of Integer);
var I: Integer;
begin
  CheckEquals(Length(aExpected), Length(aValue));
  for I := 0 to High(aExpected) do
    CheckEquals(aExpected[I], aValue[I]);
end;
{$endif}

procedure TNDATestCase.CheckEquals(const aExpected, aValue: array of Double; const aTol: Double);
var I: Integer;
begin
  CheckEquals(Length(aExpected), Length(aValue));
  for I := 0 to High(aExpected) do
    CheckEquals(Extended(aExpected[I]), Extended(aValue[I]), aTol);
end;

procedure TNDATestCase.CheckEquals(const aExpected: array of String; const aValue: array of String);
var I: Integer;
begin
  CheckEquals(Length(aExpected), Length(aValue));
  for I := 0 to High(aExpected) do
    CheckEquals(aExpected[I], aValue[I]);
end;

procedure TNDATestCase.CheckEquals(const aExpected, aValue: TCmplx128; const aTol: Double);
begin
  CheckEquals(Extended(aExpected.Re), Extended(aValue.Re), aTol);
  CheckEquals(Extended(aExpected.Im), Extended(aValue.Im), aTol);
end;

function TNDATestCase.iRng2NDA(const aShape: array of NativeInt; aLo, aStep: Integer): INDArray<Integer>;
var items: TArray<Integer>;
    count: NativeInt;
begin
  count := GetSize(aShape);
  items := TDynAUt.Range_I32(aLo, aLo + count * aStep - 1, aStep);
  Result := TDynArrWrapper<Integer>.Create(items, aShape);
end;

function TNDATestCase.sRng2NDA(const aShape: array of NativeInt; aLo, aStep: Single): INDArray<Single>;
var items: TArray<Single>;
    count: NativeInt;
begin
  count := GetSize(aShape);
  items := TDynAUt.Range_F32(aLo, aLo + count * aStep - 1, aStep);
  Result := TDynArrWrapper<Single>.Create(items, aShape);
end;

{$endregion}

{$region 'TNDAPerformanceTestCase'}

class constructor TNDAPerformanceTestCase.Create;
var s: String;
begin
  s := TPath.GetDirectoryName(Application.ExeName);
  s := TPath.Combine(s, Format('%s.txt', [ClassName]));
  fWriter := TStreamWriter.Create(s);

{$if defined(NoASM)}
  fWriter.WriteLine('ASM: False');
{$else}
  fWriter.WriteLine('ASM: True');
{$endif}
{$if defined(CPUx86)}
  fWriter.WriteLine('Arch: x86');
{$elseif defined(CPUx64)}
  fWriter.WriteLine('Arch: x64');
{$endif}
  fWriter.WriteLine;
end;

class destructor TNDAPerformanceTestCase.Destroy;
begin
  fWriter.Free;
end;

procedure TNDAPerformanceTestCase.AfterConstruction;
begin
  inherited;
  fSW := TStopWatch.Create;
end;

procedure TNDAPerformanceTestCase.BeforeDestruction;
begin
  fSW.Free;
end;

procedure TNDAPerformanceTestCase.SWStart;
begin
  fSW.Start;
  fLoopCount := 1;
end;

procedure TNDAPerformanceTestCase.SWStop(const aAppend: String; aTimeDelta: Double);
var msg: String;
begin
  fSW.Stop;

  msg := Format('%s.%s elapsed time: %.6f [ms]', [
    {$ifdef FPC}
      AnsiString(ClassName).Substring(1),
    {$else}
      ClassName.Substring(1),
    {$endif}
      TestName,
      (fSW.ElapsedMilisecondsEx / fLoopCount) + aTimeDelta
    ]);
  if aAppend <> '' then
    msg := Format('%s (%s)', [msg, aAppend]);

  fWriter.WriteLine(msg);
{$ifndef FPC}
   OutputDebugString(PChar(msg));
{$else}
   OutputDebugString(PAnsiChar(msg));
{$endif}
  Status(msg);
end;

procedure TNDAPerformanceTestCase.DoTestLoop(const aTest: TProc; aCount: Integer);
var I: Integer;
begin
  for I := 1 to aCount do begin
    aTest();
    Inc(fLoopCount);
  end;
  Dec(fLoopCount);
end;

procedure TNDAPerformanceTestCase.DoTestLoop(aTest: TTestMethod; aCount: Integer);
var I: Integer;
begin
  for I := 1 to aCount do begin
    aTest();
    Inc(fLoopCount);
  end;
  Dec(fLoopCount);
end;

{$endregion}

end.
