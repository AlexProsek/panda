object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'PaNDA test'
  ClientHeight = 467
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object Splitter1: TSplitter
    Left = 0
    Top = 265
    Width = 624
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitWidth = 202
  end
  object edInput: TSynEdit
    Left = 0
    Top = 0
    Width = 624
    Height = 265
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Consolas'
    Font.Style = []
    Font.Quality = fqClearTypeNatural
    TabOrder = 0
    OnKeyDown = edInputKeyDown
    UseCodeFolding = False
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -13
    Gutter.Font.Name = 'Consolas'
    Gutter.Font.Style = []
    Gutter.Font.Quality = fqClearTypeNatural
    Gutter.Bands = <
      item
        Kind = gbkMarks
        Width = 13
      end
      item
        Kind = gbkLineNumbers
      end
      item
        Kind = gbkFold
      end
      item
        Kind = gbkTrackChanges
      end
      item
        Kind = gbkMargin
        Width = 3
      end>
    Highlighter = SynPythonSyn1
    Lines.Strings = (
      'import sys'
      'print (sys.version_info)'
      'import pandapy as nda'
      'print (dir(nda))')
    SelectedColor.Alpha = 0.400000005960464500
    ExplicitWidth = 622
  end
  object mmOutput: TMemo
    Left = 0
    Top = 268
    Width = 624
    Height = 199
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object SynPythonSyn1: TSynPythonSyn
    Left = 496
    Top = 40
  end
  object PythonEngine1: TPythonEngine
    DllName = 'python312.dll'
    APIVersion = 1013
    RegVersion = '3.12'
    UseLastKnownVersion = False
    IO = PythonGUIInputOutput1
    Left = 352
    Top = 120
  end
  object PythonGUIInputOutput1: TPythonGUIInputOutput
    UnicodeIO = True
    RawOutput = False
    Output = mmOutput
    Left = 496
    Top = 120
  end
end
