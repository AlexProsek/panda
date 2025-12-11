{
VER80 - Delphi 1
VER90 - Delphi 2
VER100 - Delphi 3
VER120 - Delphi 4
VER130 - Delphi 5
VER140 - Delphi 6
VER150 - Delphi 7
VER160 - Delphi 8
VER170 - Delphi 2005
VER180 - Delphi 2006
VER180 - Delphi 2007
VER185 - Delphi 2007
VER200 - Delphi 2009 (Tiburon)
VER210 - Delphi 2010
VER220 - Delphi XE
VER230 - Delphi XE2
VER240 - Delphi XE3
VER250 - Delphi XE4
VER260 - Delphi XE5 (Zephyr)
VER270 - Delphi XE6
VER280 - Delphi XE7
VER290 - Delphi XE8
VER300 - Delphi Seattle
VER310 - Delphi Berlin
VER320 - Delphi Tokyo
VER330 - Delphi Rio
VER340 - Delphi Sydney
VER350 - Delphi Alexandria
VER360 - Delphi Athens
VER370 - Delphi Florence
WIN32 - Indicates that the operating environment is the Win32 API.
WIN64 - Indicates that the operating environment is the Win64 API.
LINUX - Indicates that the operating environment is Linux
MSWINDOWS - Indicates that the operating environment is the MS Windows/li]
CONSOLE - Indicates that an application is being compiled as a console application
NEXTGEN - NextGen compiler using LLVM
ANDROID
IOS
MACOS
MACOS32
POSIX
POSIX32
AUTOREFCOUNT - Automatic reference counting (ARC) enabled for objects (part of nextgen)
NATIVECODE - Always on for DCC, only .net may not use this
EXTERNALLINKER - Defined for compilers that have an external linker and the LLVM code generator; the Delphi mobile compilers have the external ld linker and use LLVM as code generator. *New* at XE4/iOS
CPUARM
CPU386
CPUX86 - CPU is an Intel 386 or later on any platform. *New* at XE2/x64
CPUX64
UNICODE - UNICODE is defined as the default string type.
}

{$IFNDEF FPC}

{$IFDEF VER370}
  {$DEFINE DelphiFlorenceUp}
  {$DEFINE DelphiAthensUp}
  {$DEFINE DelphiAlexandriaUp}
  {$DEFINE DelphiSydneyUp}
  {$DEFINE DelphiRioUp}
  {$DEFINE DelphiTokyoUp}
  {$DEFINE DelphiBerlinUp}
  {$DEFINE DelphiSeattleUp}
  {$DEFINE DelphiXE8Up}
  {$DEFINE DelphiXE7Up}
  {$DEFINE DelphiXE6Up}
  {$DEFINE DelphiXE5Up}
  {$DEFINE DelphiXE4Up}
  {$DEFINE DelphiXE3Up}
  {$DEFINE DelphiXE2Up}
  {$DEFINE DelphiXEUp}
  {$DEFINE Delphi2010Up}
  {$DEFINE Delphi2009Up}
  {$DEFINE DelphiTiburonUp}
  {$DEFINE Delphi2007Up}
  {$DEFINE Delphi2006Up}
  {$DEFINE Delphi2005Up}
  {$DEFINE Delphi7Up}
  {$DEFINE Delphi6Up}
  {$DEFINE Delphi5Up}
{$ENDIF}

{$IFDEF VER360}
  {$DEFINE DelphiAthensUp}
  {$DEFINE DelphiAlexandriaUp}
  {$DEFINE DelphiSydneyUp}
  {$DEFINE DelphiRioUp}
  {$DEFINE DelphiTokyoUp}
  {$DEFINE DelphiBerlinUp}
  {$DEFINE DelphiSeattleUp}
  {$DEFINE DelphiXE8Up}
  {$DEFINE DelphiXE7Up}
  {$DEFINE DelphiXE6Up}
  {$DEFINE DelphiXE5Up}
  {$DEFINE DelphiXE4Up}
  {$DEFINE DelphiXE3Up}
  {$DEFINE DelphiXE2Up}
  {$DEFINE DelphiXEUp}
  {$DEFINE Delphi2010Up}
  {$DEFINE Delphi2009Up}
  {$DEFINE DelphiTiburonUp}
  {$DEFINE Delphi2007Up}
  {$DEFINE Delphi2006Up}
  {$DEFINE Delphi2005Up}
  {$DEFINE Delphi7Up}
  {$DEFINE Delphi6Up}
  {$DEFINE Delphi5Up}
{$ENDIF}

{$IFDEF VER350}
  {$DEFINE DelphiAlexandriaUp}
  {$DEFINE DelphiSydneyUp}
  {$DEFINE DelphiRioUp}
  {$DEFINE DelphiTokyoUp}
  {$DEFINE DelphiBerlinUp}
  {$DEFINE DelphiSeattleUp}
  {$DEFINE DelphiXE8Up}
  {$DEFINE DelphiXE7Up}
  {$DEFINE DelphiXE6Up}
  {$DEFINE DelphiXE5Up}
  {$DEFINE DelphiXE4Up}
  {$DEFINE DelphiXE3Up}
  {$DEFINE DelphiXE2Up}
  {$DEFINE DelphiXEUp}
  {$DEFINE Delphi2010Up}
  {$DEFINE Delphi2009Up}
  {$DEFINE DelphiTiburonUp}
  {$DEFINE Delphi2007Up}
  {$DEFINE Delphi2006Up}
  {$DEFINE Delphi2005Up}
  {$DEFINE Delphi7Up}
  {$DEFINE Delphi6Up}
  {$DEFINE Delphi5Up}
{$ENDIF}

{$IFDEF VER350}
  {$DEFINE DelphiAlexandriaUp}
  {$DEFINE DelphiSydneyUp}
  {$DEFINE DelphiRioUp}
  {$DEFINE DelphiTokyoUp}
  {$DEFINE DelphiBerlinUp}
  {$DEFINE DelphiSeattleUp}
  {$DEFINE DelphiXE8Up}
  {$DEFINE DelphiXE7Up}
  {$DEFINE DelphiXE6Up}
  {$DEFINE DelphiXE5Up}
  {$DEFINE DelphiXE4Up}
  {$DEFINE DelphiXE3Up}
  {$DEFINE DelphiXE2Up}
  {$DEFINE DelphiXEUp}
  {$DEFINE Delphi2010Up}
  {$DEFINE Delphi2009Up}
  {$DEFINE DelphiTiburonUp}
  {$DEFINE Delphi2007Up}
  {$DEFINE Delphi2006Up}
  {$DEFINE Delphi2005Up}
  {$DEFINE Delphi7Up}
  {$DEFINE Delphi6Up}
  {$DEFINE Delphi5Up}
{$ENDIF}

{$IFDEF VER340}
  {$DEFINE DelphiSydneyUp}
  {$DEFINE DelphiRioUp}
  {$DEFINE DelphiTokyoUp}
  {$DEFINE DelphiBerlinUp}
  {$DEFINE DelphiSeattleUp}
  {$DEFINE DelphiXE8Up}
  {$DEFINE DelphiXE7Up}
  {$DEFINE DelphiXE6Up}
  {$DEFINE DelphiXE5Up}
  {$DEFINE DelphiXE4Up}
  {$DEFINE DelphiXE3Up}
  {$DEFINE DelphiXE2Up}
  {$DEFINE DelphiXEUp}
  {$DEFINE Delphi2010Up}
  {$DEFINE Delphi2009Up}
  {$DEFINE DelphiTiburonUp}
  {$DEFINE Delphi2007Up}
  {$DEFINE Delphi2006Up}
  {$DEFINE Delphi2005Up}
  {$DEFINE Delphi7Up}
  {$DEFINE Delphi6Up}
  {$DEFINE Delphi5Up}
{$ENDIF}

{$IFDEF VER330}
  {$DEFINE DelphiRioUp}
  {$DEFINE DelphiTokyoUp}
  {$DEFINE DelphiBerlinUp}
  {$DEFINE DelphiSeattleUp}
  {$DEFINE DelphiXE8Up}
  {$DEFINE DelphiXE7Up}
  {$DEFINE DelphiXE6Up}
  {$DEFINE DelphiXE5Up}
  {$DEFINE DelphiXE4Up}
  {$DEFINE DelphiXE3Up}
  {$DEFINE DelphiXE2Up}
  {$DEFINE DelphiXEUp}
  {$DEFINE Delphi2010Up}
  {$DEFINE Delphi2009Up}
  {$DEFINE DelphiTiburonUp}
  {$DEFINE Delphi2007Up}
  {$DEFINE Delphi2006Up}
  {$DEFINE Delphi2005Up}
  {$DEFINE Delphi7Up}
  {$DEFINE Delphi6Up}
  {$DEFINE Delphi5Up}
{$ENDIF}

{$IFDEF VER320}
  {$DEFINE DelphiTokyoUp}
  {$DEFINE DelphiBerlinUp}
  {$DEFINE DelphiSeattleUp}
  {$DEFINE DelphiXE8Up}
  {$DEFINE DelphiXE7Up}
  {$DEFINE DelphiXE6Up}
  {$DEFINE DelphiXE5Up}
  {$DEFINE DelphiXE4Up}
  {$DEFINE DelphiXE3Up}
  {$DEFINE DelphiXE2Up}
  {$DEFINE DelphiXEUp}
  {$DEFINE Delphi2010Up}
  {$DEFINE Delphi2009Up}
  {$DEFINE DelphiTiburonUp}
  {$DEFINE Delphi2007Up}
  {$DEFINE Delphi2006Up}
  {$DEFINE Delphi2005Up}
  {$DEFINE Delphi7Up}
  {$DEFINE Delphi6Up}
  {$DEFINE Delphi5Up}
{$ENDIF}

{$IFDEF VER310}
  {$DEFINE DelphiBerlinUp}
  {$DEFINE DelphiSeattleUp}
  {$DEFINE DelphiXE8Up}
  {$DEFINE DelphiXE7Up}
  {$DEFINE DelphiXE6Up}
  {$DEFINE DelphiXE5Up}
  {$DEFINE DelphiXE4Up}
  {$DEFINE DelphiXE3Up}
  {$DEFINE DelphiXE2Up}
  {$DEFINE DelphiXEUp}
  {$DEFINE Delphi2010Up}
  {$DEFINE Delphi2009Up}
  {$DEFINE DelphiTiburonUp}
  {$DEFINE Delphi2007Up}
  {$DEFINE Delphi2006Up}
  {$DEFINE Delphi2005Up}
  {$DEFINE Delphi7Up}
  {$DEFINE Delphi6Up}
  {$DEFINE Delphi5Up}
{$ENDIF}

{$IFDEF VER300}
  {$DEFINE DelphiSeattleUp}
  {$DEFINE DelphiXE8Up}
  {$DEFINE DelphiXE7Up}
  {$DEFINE DelphiXE6Up}
  {$DEFINE DelphiXE5Up}
  {$DEFINE DelphiXE4Up}
  {$DEFINE DelphiXE3Up}
  {$DEFINE DelphiXE2Up}
  {$DEFINE DelphiXEUp}
  {$DEFINE Delphi2010Up}
  {$DEFINE Delphi2009Up}
  {$DEFINE DelphiTiburonUp}
  {$DEFINE Delphi2007Up}
  {$DEFINE Delphi2006Up}
  {$DEFINE Delphi2005Up}
  {$DEFINE Delphi7Up}
  {$DEFINE Delphi6Up}
  {$DEFINE Delphi5Up}
{$ENDIF}

{$IFDEF VER290}
  {$DEFINE DelphiXE8Up}
  {$DEFINE DelphiXE7Up}
  {$DEFINE DelphiXE6Up}
  {$DEFINE DelphiXE5Up}
  {$DEFINE DelphiXE4Up}
  {$DEFINE DelphiXE3Up}
  {$DEFINE DelphiXE2Up}
  {$DEFINE DelphiXEUp}
  {$DEFINE Delphi2010Up}
  {$DEFINE Delphi2009Up}
  {$DEFINE DelphiTiburonUp}
  {$DEFINE Delphi2007Up}
  {$DEFINE Delphi2006Up}
  {$DEFINE Delphi2005Up}
  {$DEFINE Delphi7Up}
  {$DEFINE Delphi6Up}
  {$DEFINE Delphi5Up}
{$ENDIF}

{$IFDEF VER280}
  {$DEFINE DelphiXE7Up}
  {$DEFINE DelphiXE6Up}
  {$DEFINE DelphiXE5Up}
  {$DEFINE DelphiXE4Up}
  {$DEFINE DelphiXE3Up}
  {$DEFINE DelphiXE2Up}
  {$DEFINE DelphiXEUp}
  {$DEFINE Delphi2010Up}
  {$DEFINE Delphi2009Up}
  {$DEFINE DelphiTiburonUp}
  {$DEFINE Delphi2007Up}
  {$DEFINE Delphi2006Up}
  {$DEFINE Delphi2005Up}
  {$DEFINE Delphi7Up}
  {$DEFINE Delphi6Up}
  {$DEFINE Delphi5Up}
{$ENDIF}

{$IFDEF VER270}
  {$DEFINE DelphiXE6Up}
  {$DEFINE DelphiXE5Up}
  {$DEFINE DelphiXE4Up}
  {$DEFINE DelphiXE3Up}
  {$DEFINE DelphiXE2Up}
  {$DEFINE DelphiXEUp}
  {$DEFINE Delphi2010Up}
  {$DEFINE Delphi2009Up}
  {$DEFINE DelphiTiburonUp}
  {$DEFINE Delphi2007Up}
  {$DEFINE Delphi2006Up}
  {$DEFINE Delphi2005Up}
  {$DEFINE Delphi7Up}
  {$DEFINE Delphi6Up}
  {$DEFINE Delphi5Up}
{$ENDIF}

{$IFDEF VER260}
  {$DEFINE DelphiXE5Up}
  {$DEFINE DelphiXE4Up}
  {$DEFINE DelphiXE3Up}
  {$DEFINE DelphiXE2Up}
  {$DEFINE DelphiXEUp}
  {$DEFINE Delphi2010Up}
  {$DEFINE Delphi2009Up}
  {$DEFINE DelphiTiburonUp}
  {$DEFINE Delphi2007Up}
  {$DEFINE Delphi2006Up}
  {$DEFINE Delphi2005Up}
  {$DEFINE Delphi7Up}
  {$DEFINE Delphi6Up}
  {$DEFINE Delphi5Up}
{$ENDIF}

{$IFDEF VER250}
  {$DEFINE DelphiXE4Up}
  {$DEFINE DelphiXE3Up}
  {$DEFINE DelphiXE2Up}
  {$DEFINE DelphiXEUp}
  {$DEFINE Delphi2010Up}
  {$DEFINE Delphi2009Up}
  {$DEFINE DelphiTiburonUp}
  {$DEFINE Delphi2007Up}
  {$DEFINE Delphi2006Up}
  {$DEFINE Delphi2005Up}
  {$DEFINE Delphi7Up}
  {$DEFINE Delphi6Up}
  {$DEFINE Delphi5Up}
{$ENDIF}

{$IFDEF VER240}
  {$DEFINE DelphiXE3Up}
  {$DEFINE DelphiXE2Up}
  {$DEFINE DelphiXEUp}
  {$DEFINE Delphi2010Up}
  {$DEFINE Delphi2009Up}
  {$DEFINE DelphiTiburonUp}
  {$DEFINE Delphi2007Up}
  {$DEFINE Delphi2006Up}
  {$DEFINE Delphi2005Up}
  {$DEFINE Delphi7Up}
  {$DEFINE Delphi6Up}
  {$DEFINE Delphi5Up}
{$ENDIF}

{$IFDEF VER230}
  {$DEFINE DelphiXE2Up}
  {$DEFINE DelphiXEUp}
  {$DEFINE Delphi2010Up}
  {$DEFINE Delphi2009Up}
  {$DEFINE DelphiTiburonUp}
  {$DEFINE Delphi2007Up}
  {$DEFINE Delphi2006Up}
  {$DEFINE Delphi2005Up}
  {$DEFINE Delphi7Up}
  {$DEFINE Delphi6Up}
  {$DEFINE Delphi5Up}
{$ENDIF}

{$IFDEF VER220}
  {$DEFINE DelphiXEUp}
  {$DEFINE Delphi2010Up}
  {$DEFINE Delphi2009Up}
  {$DEFINE DelphiTiburonUp}
  {$DEFINE Delphi2007Up}
  {$DEFINE Delphi2006Up}
  {$DEFINE Delphi2005Up}
  {$DEFINE Delphi7Up}
  {$DEFINE Delphi6Up}
  {$DEFINE Delphi5Up}
{$ENDIF}

{$IFDEF VER210}
  {$DEFINE Delphi2010Up}
  {$DEFINE Delphi2009Up}
  {$DEFINE DelphiTiburonUp}
  {$DEFINE Delphi2007Up}
  {$DEFINE Delphi2006Up}
  {$DEFINE Delphi2005Up}
  {$DEFINE Delphi7Up}
  {$DEFINE Delphi6Up}
  {$DEFINE Delphi5Up}
{$ENDIF}

{$IFDEF VER200}
  {$DEFINE Delphi2009Up}
  {$DEFINE DelphiTiburonUp}
  {$DEFINE Delphi2007Up}
  {$DEFINE Delphi2006Up}
  {$DEFINE Delphi2005Up}
  {$DEFINE Delphi7Up}
  {$DEFINE Delphi6Up}
  {$DEFINE Delphi5Up}
{$ENDIF}

{$IFDEF VER180}
  {$IFDEF VER185}
    {$DEFINE Delphi2007Up}
  {$ENDIF}
  {$DEFINE Delphi2006Up}
  {$DEFINE Delphi2005Up}
  {$DEFINE Delphi7Up}
  {$DEFINE Delphi6Up}
  {$DEFINE Delphi5Up}
{$ENDIF}

{$IFDEF VER170}
  {$DEFINE Delphi2005Up}
  {$DEFINE Delphi7Up}
  {$DEFINE Delphi6Up}
  {$DEFINE Delphi5Up}
{$ENDIF}

{$IFDEF VER150}
  {$DEFINE Delphi7Up}
  {$DEFINE Delphi6Up}
  {$DEFINE Delphi5Up}
{$ENDIF}

{$IFDEF VER140}
  {$DEFINE Delphi6Up}
  {$DEFINE Delphi5Up}
{$ENDIF}

{$IFDEF VER130}
  {$DEFINE Delphi5Up}
{$ENDIF}

{$IFDEF Delphi2005Up}
  {$DEFINE USE_INLINE}
{$ENDIF}

{$IFDEF Delphi2007Up}
  {$DEFINE HAS_HELPERS}
{$ENDIF}

{$IFDEF Delphi2009Up}
  {$DEFINE HAS_GENERICS}
  {$DEFINE HAS_ANONYMOUSMETHODS}
{$ENDIF}

{$IFDEF Delphi2010Up}
  {$DEFINE HAS_ATTRIBUTES}
{$ENDIF}

{$ELSE !FPC}

{ FPC Defines }
{$I 'FPCVers.inc'}

{$ENDIF FPC}


