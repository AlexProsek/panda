object Form11: TForm11
  Left = 0
  Top = 0
  Caption = 'Form11'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 624
    Height = 41
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 622
    object lbClass: TLabel
      Left = 189
      Top = 11
      Width = 30
      Height = 15
      Caption = 'Class:'
    end
    object btClassify: TButton
      Left = 100
      Top = 7
      Width = 75
      Height = 25
      Caption = 'Classify'
      Enabled = False
      TabOrder = 0
      OnClick = btClassifyClick
    end
    object btLoadImg: TButton
      Left = 11
      Top = 7
      Width = 75
      Height = 25
      Caption = 'Load Image'
      TabOrder = 1
      OnClick = btLoadImgClick
    end
  end
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 41
    Width = 624
    Height = 400
    Align = alClient
    TabOrder = 1
    ExplicitWidth = 622
    ExplicitHeight = 392
    object Image1: TImage
      Left = 0
      Top = 0
      Width = 297
      Height = 281
    end
  end
  object FileOpenDialog1: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'Image files'
        FileMask = '*.jpg;*.jpeg;*.bmp;*.png;*.gif'
      end>
    Options = []
    Left = 352
    Top = 113
  end
end
