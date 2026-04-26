object Form4: TForm4
  Left = 0
  Top = 0
  Caption = 'Form4'
  ClientHeight = 541
  ClientWidth = 827
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
    Width = 827
    Height = 49
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 825
    object Label1: TLabel
      Left = 115
      Top = 17
      Width = 58
      Height = 15
      Caption = 'Kernel size:'
    end
    object Label2: TLabel
      Left = 328
      Top = 17
      Width = 55
      Height = 15
      Caption = 'Filter type:'
    end
    object Button1: TButton
      Left = 16
      Top = 13
      Width = 75
      Height = 25
      Caption = 'Load Image'
      TabOrder = 0
      OnClick = Button1Click
    end
    object edKernelSize: TSpinEdit
      Left = 192
      Top = 14
      Width = 121
      Height = 24
      Increment = 2
      MaxValue = 200
      MinValue = 3
      TabOrder = 1
      Value = 11
    end
    object btExecute: TButton
      Left = 663
      Top = 13
      Width = 75
      Height = 25
      Caption = 'Execute'
      Enabled = False
      TabOrder = 2
      OnClick = btExecuteClick
    end
    object cbFilterType: TComboBox
      Left = 397
      Top = 14
      Width = 117
      Height = 23
      Style = csDropDownList
      ItemIndex = 1
      TabOrder = 3
      Text = 'Gaussian'
      Items.Strings = (
        'Mean'
        'Gaussian')
    end
    object cbUseSepKer: TCheckBox
      Left = 520
      Top = 17
      Width = 137
      Height = 17
      Caption = 'Use separated kernel'
      TabOrder = 4
    end
  end
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 49
    Width = 827
    Height = 473
    Align = alClient
    TabOrder = 1
    ExplicitWidth = 825
    ExplicitHeight = 465
    object Image1: TImage
      Left = 0
      Top = 0
      Width = 377
      Height = 257
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 522
    Width = 827
    Height = 19
    Panels = <
      item
        Text = 'Elapsed Time:'
        Width = 150
      end>
    ExplicitTop = 514
    ExplicitWidth = 825
  end
  object FileOpenDialog1: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'Image file'
        FileMask = '*.bmp;*.jpg;*.jpeg;*.png;*.gif'
      end>
    Options = []
    Left = 400
    Top = 89
  end
end
