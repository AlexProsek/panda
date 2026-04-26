object Form6: TForm6
  Left = 0
  Top = 0
  Caption = 'Form6'
  ClientHeight = 659
  ClientWidth = 912
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
    Width = 912
    Height = 153
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 910
    DesignSize = (
      912
      153)
    object Label1: TLabel
      Left = 16
      Top = 44
      Width = 29
      Height = 15
      Caption = 'Filter:'
    end
    object lbR: TLabel
      Left = 240
      Top = 44
      Width = 38
      Height = 15
      Caption = 'Radius:'
    end
    object lbLambda: TLabel
      Left = 392
      Top = 44
      Width = 46
      Height = 15
      Caption = 'Lambda:'
      Visible = False
    end
    object Label2: TLabel
      Left = 16
      Top = 13
      Width = 35
      Height = 15
      Caption = 'Signal:'
    end
    object btApply: TButton
      Left = 798
      Top = 40
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Apply'
      TabOrder = 0
      OnClick = btApplyClick
      ExplicitLeft = 796
    end
    object cbFilter: TComboBox
      Left = 64
      Top = 41
      Width = 142
      Height = 23
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 1
      Text = 'Min filter'
      OnChange = cbFilterChange
      Items.Strings = (
        'Min filter'
        'Max filter'
        'Median filter'
        'Total variation filter')
    end
    object edRadius: TSpinEdit
      Left = 296
      Top = 41
      Width = 77
      Height = 24
      MaxValue = 2000
      MinValue = 1
      TabOrder = 2
      Value = 80
    end
    object edLambda: TEdit
      Left = 453
      Top = 41
      Width = 84
      Height = 23
      TabOrder = 3
      Text = '1'
      Visible = False
    end
    object cbSignal: TComboBox
      Left = 64
      Top = 10
      Width = 142
      Height = 23
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 4
      Text = 'Trigonometric'
      OnChange = cbSignalChange
      Items.Strings = (
        'Trigonometric'
        'Square wave')
    end
  end
  object Chart1: TChart
    Left = 0
    Top = 153
    Width = 912
    Height = 487
    BackWall.Brush.Gradient.Direction = gdBottomTop
    BackWall.Brush.Gradient.EndColor = clWhite
    BackWall.Brush.Gradient.StartColor = 15395562
    BackWall.Brush.Gradient.Visible = True
    BackWall.Transparent = False
    Foot.Font.Color = clBlue
    Foot.Font.Name = 'Verdana'
    Gradient.Direction = gdBottomTop
    Gradient.EndColor = clWhite
    Gradient.MidColor = 15395562
    Gradient.StartColor = 15395562
    Gradient.Visible = True
    LeftWall.Color = clLightyellow
    Legend.Font.Name = 'Verdana'
    Legend.Shadow.Transparency = 0
    Legend.Visible = False
    RightWall.Color = clLightyellow
    Title.Font.Name = 'Verdana'
    Title.Text.Strings = (
      'TChart')
    Title.Visible = False
    BottomAxis.Axis.Color = 4210752
    BottomAxis.Grid.Color = clDarkgray
    BottomAxis.LabelsFormat.Font.Name = 'Verdana'
    BottomAxis.TicksInner.Color = clDarkgray
    BottomAxis.Title.Font.Name = 'Verdana'
    DepthAxis.Axis.Color = 4210752
    DepthAxis.Grid.Color = clDarkgray
    DepthAxis.LabelsFormat.Font.Name = 'Verdana'
    DepthAxis.TicksInner.Color = clDarkgray
    DepthAxis.Title.Font.Name = 'Verdana'
    DepthTopAxis.Axis.Color = 4210752
    DepthTopAxis.Grid.Color = clDarkgray
    DepthTopAxis.LabelsFormat.Font.Name = 'Verdana'
    DepthTopAxis.TicksInner.Color = clDarkgray
    DepthTopAxis.Title.Font.Name = 'Verdana'
    LeftAxis.Axis.Color = 4210752
    LeftAxis.Grid.Color = clDarkgray
    LeftAxis.LabelsFormat.Font.Name = 'Verdana'
    LeftAxis.TicksInner.Color = clDarkgray
    LeftAxis.Title.Font.Name = 'Verdana'
    RightAxis.Axis.Color = 4210752
    RightAxis.Grid.Color = clDarkgray
    RightAxis.LabelsFormat.Font.Name = 'Verdana'
    RightAxis.TicksInner.Color = clDarkgray
    RightAxis.Title.Font.Name = 'Verdana'
    TopAxis.Axis.Color = 4210752
    TopAxis.Grid.Color = clDarkgray
    TopAxis.LabelsFormat.Font.Name = 'Verdana'
    TopAxis.TicksInner.Color = clDarkgray
    TopAxis.Title.Font.Name = 'Verdana'
    View3D = False
    Align = alClient
    TabOrder = 1
    ExplicitWidth = 910
    ExplicitHeight = 479
    DefaultCanvas = 'TGDIPlusCanvas'
    ColorPaletteIndex = 13
    object Series1: TFastLineSeries
      Selected.Hover.Visible = True
      LinePen.Color = 10708548
      LinePen.EndStyle = esRound
      TreatNulls = tnDontPaint
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object Series2: TFastLineSeries
      Selected.Hover.Visible = True
      LinePen.Color = 3513587
      LinePen.EndStyle = esRound
      TreatNulls = tnDontPaint
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 640
    Width = 912
    Height = 19
    Panels = <
      item
        Text = 'Elapsed time: '
        Width = 50
      end>
    ExplicitTop = 632
    ExplicitWidth = 910
  end
end
