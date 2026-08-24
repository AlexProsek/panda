object Form10: TForm10
  Left = 0
  Top = 0
  Caption = 'Form10'
  ClientHeight = 551
  ClientWidth = 815
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 815
    Height = 41
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 813
    object Label1: TLabel
      Left = 152
      Top = 13
      Width = 71
      Height = 15
      Caption = 'Kernel radius:'
    end
    object btLoadImg: TButton
      Left = 7
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Load image'
      TabOrder = 0
      OnClick = btLoadImgClick
    end
    object edRadius: TSpinEdit
      Left = 229
      Top = 7
      Width = 121
      Height = 24
      MaxValue = 1000
      MinValue = 1
      TabOrder = 1
      Value = 10
    end
    object btApply: TButton
      Left = 512
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Apply'
      Enabled = False
      TabOrder = 2
      OnClick = btApplyClick
    end
    object cbFFTConv: TCheckBox
      Left = 358
      Top = 10
      Width = 135
      Height = 17
      Caption = 'Use FFT convolution'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = cbFFTConvClick
    end
    object cbUseSepKer: TCheckBox
      Left = 624
      Top = 10
      Width = 137
      Height = 17
      Caption = 'Use separated kernel'
      Enabled = False
      TabOrder = 4
      Visible = False
      WordWrap = True
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 532
    Width = 815
    Height = 19
    Panels = <
      item
        Text = 'Elapsed time:'
        Width = 100
      end>
    ExplicitTop = 524
    ExplicitWidth = 813
  end
  object Panel2: TPanel
    Left = 0
    Top = 41
    Width = 815
    Height = 491
    Align = alClient
    TabOrder = 2
    ExplicitWidth = 813
    ExplicitHeight = 483
    object Image1: TImage
      Left = 0
      Top = 0
      Width = 105
      Height = 105
    end
  end
  object FileOpenDialog1: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = []
    Left = 264
    Top = 73
  end
end
