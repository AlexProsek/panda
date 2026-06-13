object Form8: TForm8
  Left = 0
  Top = 0
  Caption = 'Form8'
  ClientHeight = 529
  ClientWidth = 806
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 121
    Width = 806
    Height = 389
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 804
    ExplicitHeight = 381
    object Image1: TImage
      Left = 0
      Top = 0
      Width = 329
      Height = 201
      OnMouseLeave = Image1MouseLeave
      OnMouseMove = Image1MouseMove
    end
  end
  object pnTop: TPanel
    Left = 0
    Top = 0
    Width = 806
    Height = 121
    Align = alTop
    TabOrder = 1
    ExplicitWidth = 804
    object Label1: TLabel
      Left = 352
      Top = 20
      Width = 56
      Height = 15
      Caption = 'Threshold:'
    end
    object Label2: TLabel
      Left = 133
      Top = 20
      Width = 45
      Height = 15
      Caption = 'Method:'
    end
    object lbParams: TLabel
      Left = 133
      Top = 46
      Width = 42
      Height = 15
      Caption = 'Params:'
    end
    object Label3: TLabel
      Left = 352
      Top = 46
      Width = 38
      Height = 15
      Caption = 'Radius:'
    end
    object Label4: TLabel
      Left = 133
      Top = 90
      Width = 45
      Height = 15
      Caption = 'Method:'
    end
    object Label5: TLabel
      Left = 354
      Top = 90
      Width = 52
      Height = 15
      Caption = 'Iterations:'
    end
    object Label6: TLabel
      Left = 12
      Top = 72
      Width = 66
      Height = 15
      Caption = 'Morphology'
    end
    object btLoadImage: TButton
      Left = 12
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Load Image'
      TabOrder = 0
      OnClick = btLoadImageClick
    end
    object cbMethod: TComboBox
      Left = 189
      Top = 17
      Width = 145
      Height = 23
      Style = csDropDownList
      TabOrder = 1
      OnChange = cbMethodChange
    end
    object edThreshold: TSpinEdit
      Left = 424
      Top = 17
      Width = 121
      Height = 24
      MaxValue = 255
      MinValue = 0
      TabOrder = 2
      Value = 127
    end
    object btExecute: TButton
      Left = 572
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Execute'
      Enabled = False
      TabOrder = 3
      OnClick = btExecuteClick
    end
    object edParams: TEdit
      Left = 189
      Top = 43
      Width = 145
      Height = 23
      TabOrder = 4
      Text = '1.0, 0.0, 0.0'
    end
    object edRadius: TSpinEdit
      Left = 424
      Top = 43
      Width = 121
      Height = 24
      MaxValue = 1000
      MinValue = 1
      TabOrder = 5
      Value = 2
    end
    object cbMMethod: TComboBox
      Left = 189
      Top = 87
      Width = 145
      Height = 23
      Style = csDropDownList
      Enabled = False
      TabOrder = 6
    end
    object StaticText1: TStaticText
      Left = 84
      Top = 80
      Width = 563
      Height = 2
      AutoSize = False
      BorderStyle = sbsSunken
      Caption = 'StaticText1'
      TabOrder = 7
    end
    object edNIter: TSpinEdit
      Left = 424
      Top = 87
      Width = 121
      Height = 24
      Enabled = False
      MaxValue = 1000
      MinValue = 1
      TabOrder = 8
      Value = 1
    end
    object btMExecute: TButton
      Left = 572
      Top = 86
      Width = 75
      Height = 25
      Caption = 'Execute'
      Enabled = False
      TabOrder = 9
      OnClick = btMExecuteClick
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 510
    Width = 806
    Height = 19
    Panels = <
      item
        Text = 'Elapsed time:'
        Width = 150
      end
      item
        Text = 'Pixel value:'
        Width = 50
      end>
    ExplicitTop = 502
    ExplicitWidth = 804
  end
  object FileOpenDialog1: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'Image file'
        FileMask = '*.bmp;*.jpg;*.jpeg;*.png;*.gif'
      end>
    Options = []
    Left = 504
    Top = 272
  end
end
