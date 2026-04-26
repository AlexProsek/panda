object Form5: TForm5
  Left = 0
  Top = 0
  Caption = 'Form5'
  ClientHeight = 590
  ClientWidth = 783
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
    Width = 783
    Height = 65
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 781
    object lbScale: TLabel
      Left = 347
      Top = 12
      Width = 28
      Height = 15
      Caption = '100%'
    end
    object Label1: TLabel
      Left = 148
      Top = 12
      Width = 30
      Height = 15
      Caption = 'Scale:'
    end
    object Button1: TButton
      Left = 12
      Top = 8
      Width = 85
      Height = 25
      Caption = 'Load Image'
      TabOrder = 0
      OnClick = Button1Click
    end
    object cbGrayscale: TCheckBox
      Left = 384
      Top = 39
      Width = 97
      Height = 17
      Caption = 'Grayscale'
      TabOrder = 1
      OnClick = cbGrayscaleClick
    end
    object TrackBar1: TTrackBar
      Left = 194
      Top = 8
      Width = 150
      Height = 45
      Position = 5
      TabOrder = 2
      OnChange = TrackBar1Change
    end
    object cbInterpMethod: TComboBox
      Left = 384
      Top = 9
      Width = 145
      Height = 23
      Style = csDropDownList
      ItemIndex = 1
      TabOrder = 3
      Text = 'bilinear interpolation'
      Items.Strings = (
        'nearest neighbor'
        'bilinear interpolation')
    end
  end
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 65
    Width = 783
    Height = 506
    Align = alClient
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 1
    ExplicitWidth = 781
    ExplicitHeight = 498
    object Image1: TImage
      Left = 0
      Top = 0
      Width = 265
      Height = 161
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 571
    Width = 783
    Height = 19
    Panels = <
      item
        Text = 'Elapsed time: '
        Width = 50
      end>
    ExplicitTop = 563
    ExplicitWidth = 781
  end
  object FileOpenDialog1: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'Image file'
        FileMask = '*.bmp;*.jpg;*.jpeg;*.png;*.gif'
      end>
    Options = []
    Left = 184
    Top = 88
  end
end
