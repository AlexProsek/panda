object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Demo2'
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
  object Splitter1: TSplitter
    Left = 0
    Top = 145
    Width = 624
    Height = 5
    Cursor = crVSplit
    Align = alTop
  end
  object mmInput: TMemo
    Left = 0
    Top = 0
    Width = 624
    Height = 145
    Align = alTop
    Lines.Strings = (
      '{{1, 2, 3}, {4, 5, 6}, {7, 8, 9}}')
    TabOrder = 0
  end
  object mmOutput: TMemo
    Left = 0
    Top = 150
    Width = 624
    Height = 250
    Align = alClient
    ReadOnly = True
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 400
    Width = 624
    Height = 41
    Align = alBottom
    TabOrder = 2
    DesignSize = (
      624
      41)
    object btExecute: TButton
      Left = 532
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Execute'
      TabOrder = 0
      OnClick = btExecuteClick
      ExplicitLeft = 536
    end
    object ComboBox1: TComboBox
      Left = 15
      Top = 8
      Width = 249
      Height = 23
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 1
      Text = 'LU decomposition'
      OnChange = ComboBox1Change
      Items.Strings = (
        'LU decomposition'
        'Upper triangulation linear solve')
    end
  end
end
