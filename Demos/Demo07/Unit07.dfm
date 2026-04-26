object Form7: TForm7
  Left = 0
  Top = 0
  Caption = 'Form7'
  ClientHeight = 546
  ClientWidth = 865
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object pnTop: TPanel
    Left = 0
    Top = 0
    Width = 865
    Height = 121
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 863
    DesignSize = (
      865
      121)
    object Label1: TLabel
      Left = 14
      Top = 47
      Width = 29
      Height = 15
      Caption = 'Filter:'
    end
    object lbRadius: TLabel
      Left = 214
      Top = 47
      Width = 38
      Height = 15
      Caption = 'Radius:'
    end
    object lbKerType: TLabel
      Left = 366
      Top = 47
      Width = 62
      Height = 15
      Caption = 'Kernel type:'
    end
    object lbIterCnt: TLabel
      Left = 214
      Top = 89
      Width = 52
      Height = 15
      Caption = 'Iterations:'
    end
    object lbLambda: TLabel
      Left = 382
      Top = 89
      Width = 46
      Height = 15
      Caption = 'Lambda:'
    end
    object Button1: TButton
      Left = 16
      Top = 11
      Width = 81
      Height = 25
      Caption = 'Load image'
      TabOrder = 0
      OnClick = Button1Click
    end
    object cbFilter: TComboBox
      Left = 53
      Top = 43
      Width = 145
      Height = 23
      Style = csDropDownList
      TabOrder = 1
      OnSelect = cbFilterSelect
    end
    object btApply: TButton
      Left = 777
      Top = 42
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Apply'
      Enabled = False
      TabOrder = 2
      OnClick = btApplyClick
      ExplicitLeft = 775
    end
    object edRadius: TSpinEdit
      Left = 262
      Top = 43
      Width = 93
      Height = 24
      MaxValue = 1000
      MinValue = 1
      TabOrder = 3
      Value = 10
    end
    object cbKerType: TComboBox
      Left = 436
      Top = 43
      Width = 118
      Height = 23
      Style = csDropDownList
      TabOrder = 4
    end
    object cbParallelize: TCheckBox
      Left = 560
      Top = 46
      Width = 90
      Height = 17
      Caption = 'Parallelize'
      TabOrder = 5
    end
    object edIterCnt: TSpinEdit
      Left = 278
      Top = 86
      Width = 77
      Height = 24
      MaxValue = 10000
      MinValue = 1
      TabOrder = 6
      Value = 100
    end
    object edLambda: TEdit
      Left = 438
      Top = 86
      Width = 116
      Height = 23
      TabOrder = 7
      Text = '1'
    end
    object btAddNoise: TButton
      Left = 109
      Top = 11
      Width = 89
      Height = 25
      Caption = 'Add noise'
      Enabled = False
      TabOrder = 8
      OnClick = btAddNoiseClick
    end
  end
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 121
    Width = 865
    Height = 406
    Align = alClient
    TabOrder = 1
    ExplicitWidth = 863
    ExplicitHeight = 398
    object Image1: TImage
      Left = 0
      Top = 0
      Width = 313
      Height = 217
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 527
    Width = 865
    Height = 19
    Panels = <
      item
        Text = 'Elapsed time:'
        Width = 50
      end>
    ExplicitTop = 519
    ExplicitWidth = 863
  end
  object FileOpenDialog1: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'Image file'
        FileMask = '*.bmp;*.jpg;*.jpeg;*.png;*.gif'
      end>
    Options = []
    Left = 632
    Top = 152
  end
end
