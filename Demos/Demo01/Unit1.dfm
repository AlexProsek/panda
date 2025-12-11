object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 441
  ClientWidth = 624
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
    Width = 624
    Height = 41
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 622
    DesignSize = (
      624
      41)
    object btFlipH: TSpeedButton
      Left = 471
      Top = 9
      Width = 60
      Height = 22
      AllowAllUp = True
      Anchors = [akTop, akRight]
      GroupIndex = 10
      Caption = 'Flip H'
      Enabled = False
      OnClick = btFlipChanged
      ExplicitLeft = 477
    end
    object btFlipV: TSpeedButton
      Left = 538
      Top = 9
      Width = 60
      Height = 22
      AllowAllUp = True
      Anchors = [akTop, akRight]
      GroupIndex = 11
      Caption = 'Flip V'
      Enabled = False
      OnClick = btFlipChanged
      ExplicitLeft = 544
    end
    object Button1: TButton
      Left = 16
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Load Image'
      TabOrder = 0
      OnClick = Button1Click
    end
    object btSplitRGB: TButton
      Left = 390
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Spit RGB'
      Enabled = False
      TabOrder = 1
      OnClick = btSplitRGBClick
      ExplicitLeft = 388
    end
  end
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 41
    Width = 624
    Height = 400
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    TabOrder = 1
    ExplicitWidth = 622
    ExplicitHeight = 392
    object Image1: TImage
      Left = 0
      Top = 0
      Width = 361
      Height = 273
    end
  end
  object FileOpenDialog1: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'Bitmap image'
        FileMask = '*.bmp'
      end>
    Options = []
    Left = 256
    Top = 8
  end
end
