object Form9: TForm9
  Left = 0
  Top = 0
  Caption = 'Form9'
  ClientHeight = 708
  ClientWidth = 737
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object Splitter1: TSplitter
    Left = 0
    Top = 233
    Width = 737
    Height = 4
    Cursor = crVSplit
    Align = alTop
    ExplicitTop = 313
  end
  object Splitter2: TSplitter
    Left = 0
    Top = 477
    Width = 737
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitWidth = 203
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 737
    Height = 41
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 735
    object Button1: TButton
      Left = 16
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Load data'
      Enabled = False
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 104
      Top = 9
      Width = 75
      Height = 25
      Caption = 'FFT'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 200
      Top = 9
      Width = 81
      Height = 25
      Caption = 'Add noise'
      TabOrder = 2
      OnClick = Button3Click
    end
  end
  object Chart1: TChart
    Left = 0
    Top = 41
    Width = 737
    Height = 192
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
    RightWall.Color = clLightyellow
    Title.Font.Name = 'Verdana'
    Title.Text.Strings = (
      'Sample data')
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
    Align = alTop
    TabOrder = 1
    ExplicitWidth = 735
    DefaultCanvas = 'TGDIPlusCanvas'
    ColorPaletteIndex = 13
    object Series1: TLineSeries
      Legend.Visible = False
      ShowInLegend = False
      Brush.BackColor = clDefault
      LinePen.Color = 10708548
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 680
    Width = 737
    Height = 28
    Panels = <>
    ExplicitTop = 672
    ExplicitWidth = 735
  end
  object Panel2: TPanel
    Left = 0
    Top = 237
    Width = 737
    Height = 240
    Align = alTop
    Caption = 'Panel2'
    TabOrder = 3
    ExplicitWidth = 735
    DesignSize = (
      737
      240)
    object Label1: TLabel
      Left = 16
      Top = 190
      Width = 82
      Height = 15
      Caption = 'Filter properties'
    end
    object Label2: TLabel
      Left = 16
      Top = 212
      Width = 49
      Height = 15
      Caption = 'Low freq:'
    end
    object Label3: TLabel
      Left = 224
      Top = 212
      Width = 53
      Height = 15
      Caption = 'High freq:'
    end
    object Chart2: TChart
      Left = 1
      Top = 1
      Width = 735
      Height = 184
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
        'FFT')
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
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      Anchors = [akLeft, akTop, akRight, akBottom]
      ExplicitWidth = 733
      DefaultCanvas = 'TGDIPlusCanvas'
      ColorPaletteIndex = 13
      object Series2: TLineSeries
        Brush.BackColor = clDefault
        LinePen.Color = 10708548
        Pointer.InflateMargins = True
        Pointer.Style = psVisual
        Pointer.Visible = True
        XValues.Name = 'X'
        XValues.Order = loAscending
        YValues.Name = 'Y'
        YValues.Order = loNone
      end
    end
    object Button4: TButton
      Left = 639
      Top = 208
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Execute'
      TabOrder = 1
      OnClick = Button4Click
      ExplicitLeft = 637
    end
    object StaticText1: TStaticText
      Left = 112
      Top = 198
      Width = 602
      Height = 2
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      BorderStyle = sbsSunken
      Caption = 'StaticText1'
      TabOrder = 2
      ExplicitWidth = 600
    end
    object edLoFreq: TSpinEdit
      Left = 88
      Top = 208
      Width = 121
      Height = 24
      MaxValue = 0
      MinValue = 0
      TabOrder = 3
      Value = 0
    end
    object edHiFreq: TSpinEdit
      Left = 288
      Top = 208
      Width = 121
      Height = 24
      MaxValue = 0
      MinValue = 0
      TabOrder = 4
      Value = 100
    end
  end
  object Chart3: TChart
    Left = 0
    Top = 480
    Width = 737
    Height = 200
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
      'Reconstructed signal')
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
    TabOrder = 4
    ExplicitWidth = 735
    ExplicitHeight = 192
    DefaultCanvas = 'TGDIPlusCanvas'
    ColorPaletteIndex = 13
    object Series3: TLineSeries
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
  end
  object FileOpenDialog1: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'vector data file'
        FileMask = '*.mat'
      end>
    Options = []
    Left = 432
    Top = 56
  end
end
