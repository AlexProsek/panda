object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'Form3'
  ClientHeight = 593
  ClientWidth = 1016
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object Splitter1: TSplitter
    Left = 405
    Top = 41
    Height = 552
    Align = alRight
    ExplicitLeft = 0
    ExplicitTop = 0
    ExplicitHeight = 1016
  end
  object ScrollBox1: TScrollBox
    Left = 408
    Top = 41
    Width = 608
    Height = 552
    Align = alRight
    TabOrder = 0
    object Image1: TImage
      Left = 3
      Top = -2
      Width = 166
      Height = 179
    end
  end
  object pnTop: TPanel
    Left = 0
    Top = 0
    Width = 1016
    Height = 41
    Align = alTop
    TabOrder = 1
    ExplicitWidth = 1014
    object btLoadImg: TButton
      Left = 9
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Load Image'
      TabOrder = 0
      OnClick = btLoadImgClick
    end
  end
  object pnCentre: TPanel
    Left = 0
    Top = 41
    Width = 405
    Height = 552
    Align = alClient
    TabOrder = 2
    ExplicitWidth = 717
    object edInput: TSynEdit
      Left = 1
      Top = 1
      Width = 403
      Height = 509
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Consolas'
      Font.Style = []
      Font.Quality = fqClearTypeNatural
      TabOrder = 0
      UseCodeFolding = False
      Gutter.Font.Charset = DEFAULT_CHARSET
      Gutter.Font.Color = clWindowText
      Gutter.Font.Height = -13
      Gutter.Font.Name = 'Consolas'
      Gutter.Font.Style = []
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
        '# variable img contains the loaded image'
        'tmp = img["R"].copy()'
        'img["R"] = img["G"]'
        'img["G"] = tmp')
      SelectedColor.Alpha = 0.400000005960464500
      ExplicitWidth = 713
      ExplicitHeight = 241
    end
    object Panel1: TPanel
      Left = 1
      Top = 510
      Width = 403
      Height = 41
      Align = alBottom
      TabOrder = 1
      ExplicitLeft = 144
      ExplicitTop = 472
      ExplicitWidth = 185
      DesignSize = (
        403
        41)
      object btExec: TButton
        Left = 318
        Top = 9
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Execute'
        Enabled = False
        TabOrder = 0
        OnClick = btExecClick
        ExplicitLeft = 630
      end
    end
  end
  object PythonEngine1: TPythonEngine
    DllName = 'python312.dll'
    APIVersion = 1013
    RegVersion = '3.12'
    UseLastKnownVersion = False
    IO = PythonGUIInputOutput1
    Left = 368
    Top = 152
  end
  object SynPythonSyn1: TSynPythonSyn
    Left = 432
    Top = 64
  end
  object PythonGUIInputOutput1: TPythonGUIInputOutput
    UnicodeIO = True
    RawOutput = False
    Left = 488
    Top = 144
  end
  object FileOpenDialog1: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'Bitmap image'
        FileMask = '*.bmp'
      end>
    Options = []
    Left = 288
    Top = 64
  end
end
