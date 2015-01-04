object Form1: TForm1
  Left = 253
  Top = 78
  AutoScroll = False
  Caption = 'Haxe 3D Model Viewer By Luis Santos AKA DJOKER'
  ClientHeight = 549
  ClientWidth = 790
  Color = clBlack
  Constraints.MinHeight = 240
  Constraints.MinWidth = 320
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Microsoft Sans Serif'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnMouseDown = FormMouseDown
  OnMouseMove = FormMouseMove
  OnMouseUp = FormMouseUp
  OnPaint = FormPaint
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 530
    Width = 790
    Height = 19
    Panels = <
      item
        Width = 80
      end
      item
        Width = 100
      end
      item
        Width = 50
      end>
  end
  object Panel1: TPanel
    Left = 0
    Top = 459
    Width = 790
    Height = 71
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      790
      71)
    object Label1: TLabel
      Left = 703
      Top = 20
      Width = 3
      Height = 13
    end
    object SpeedButton: TSpeedButton
      Left = 16
      Top = 16
      Width = 33
      Height = 22
      AllowAllUp = True
      GroupIndex = 1
      Caption = '4'
      Flat = True
      Font.Charset = SYMBOL_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Webdings'
      Font.Style = []
      ParentFont = False
    end
    object JvLinkLabel: TJvLinkLabel
      Left = 608
      Top = 56
      Width = 177
      Height = 13
      Caption = 'https://djokergames.wordpress.com'
      Text.Strings = (
        'https://djokergames.wordpress.com')
    end
    object TrackBar1: TTrackBar
      Left = 72
      Top = 16
      Width = 627
      Height = 30
      Anchors = [akLeft, akTop, akRight]
      Max = 0
      TabOrder = 0
      OnChange = TrackBar1Change
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 637
    Height = 459
    Align = alClient
    Caption = 'Panel2'
    TabOrder = 2
    OnMouseDown = Panel2MouseDown
    OnMouseMove = Panel2MouseMove
    OnMouseUp = Panel2MouseUp
  end
  object Panel3: TPanel
    Left = 637
    Top = 0
    Width = 153
    Height = 459
    Align = alRight
    TabOrder = 3
    object Label2: TLabel
      Left = 16
      Top = 432
      Width = 32
      Height = 13
      Caption = 'Label2'
    end
    object GroupBox1: TGroupBox
      Left = 11
      Top = 16
      Width = 154
      Height = 190
      Caption = 'Rotation'
      TabOrder = 0
      object TrackBar2: TTrackBar
        Left = 15
        Top = 21
        Width = 45
        Height = 142
        Max = 180
        Min = -180
        Orientation = trVertical
        TabOrder = 0
      end
      object TrackBar3: TTrackBar
        Left = 54
        Top = 19
        Width = 45
        Height = 143
        Max = 180
        Min = -180
        Orientation = trVertical
        TabOrder = 1
      end
      object TrackBar4: TTrackBar
        Left = 95
        Top = 22
        Width = 46
        Height = 140
        Max = 180
        Min = -180
        Orientation = trVertical
        TabOrder = 2
      end
      object Button1: TButton
        Left = 41
        Top = 166
        Width = 75
        Height = 18
        Caption = 'Reset'
        TabOrder = 3
        OnClick = Button1Click
      end
    end
    object GroupBox2: TGroupBox
      Left = 10
      Top = 213
      Width = 154
      Height = 164
      Caption = 'Light'
      TabOrder = 1
      object TrackBar5: TTrackBar
        Left = 9
        Top = 18
        Width = 38
        Height = 142
        Max = 255
        Orientation = trVertical
        Position = 200
        TabOrder = 0
      end
      object TrackBar6: TTrackBar
        Left = 53
        Top = 16
        Width = 36
        Height = 143
        Max = 255
        Orientation = trVertical
        Position = 200
        TabOrder = 1
      end
      object TrackBar7: TTrackBar
        Left = 106
        Top = 16
        Width = 33
        Height = 140
        Max = 255
        Orientation = trVertical
        Position = 200
        TabOrder = 2
      end
    end
    object ScrollBar: TScrollBar
      Left = 16
      Top = 408
      Width = 121
      Height = 17
      Min = 1
      PageSize = 0
      Position = 100
      TabOrder = 2
      OnChange = ScrollBarChange
    end
  end
  object MainMenu1: TMainMenu
    Left = 8
    Top = 8
    object Model1: TMenuItem
      Caption = 'Model'
      object Load1: TMenuItem
        Caption = 'Load'
        ShortCut = 16460
        OnClick = Load1Click
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object N2: TMenuItem
        Caption = 'Load Texture'
        OnClick = N2Click
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object N4: TMenuItem
        Caption = 'Import'
        object B3d1: TMenuItem
          Caption = 'B3d'
          OnClick = B3d1Click
        end
        object Ms3d1: TMenuItem
          Caption = 'Ms3d'
          OnClick = Ms3d1Click
        end
        object Assimp1: TMenuItem
          Caption = '-'
        end
        object AssimpMesh1: TMenuItem
          Caption = 'Assimp Mesh'
          OnClick = AssimpMesh1Click
        end
        object AssimpAnimation1: TMenuItem
          Caption = 'Assimp Animation'
          OnClick = AssimpAnimation1Click
        end
        object AssimpMergeAnimation1: TMenuItem
          Caption = 'Assimp Merge Animation'
          OnClick = AssimpMergeAnimation1Click
        end
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        ShortCut = 16465
        OnClick = Exit1Click
      end
    end
    object View1: TMenuItem
      Caption = 'View'
      object Center1: TMenuItem
        Caption = 'Center'
        OnClick = Center1Click
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object view_1: TMenuItem
        AutoCheck = True
        Caption = 'Textures'
        Checked = True
      end
      object view_2: TMenuItem
        AutoCheck = True
        Caption = 'Wireframe'
      end
      object view_3: TMenuItem
        AutoCheck = True
        Caption = 'Lighting'
        Checked = True
      end
      object view_4: TMenuItem
        AutoCheck = True
        Caption = 'Cull Face'
        Checked = True
      end
      object view_5: TMenuItem
        AutoCheck = True
        Caption = 'Grid'
        Checked = True
      end
      object Depth1: TMenuItem
        AutoCheck = True
        Caption = 'Depth'
        Checked = True
      end
      object view_6: TMenuItem
        AutoCheck = True
        Caption = 'Axis'
        Checked = True
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object Vetices1: TMenuItem
        Caption = 'Vetices Trace'
        OnClick = Vetices1Click
      end
      object Log1: TMenuItem
        Caption = 'Log'
        OnClick = Log1Click
      end
    end
    object about1: TMenuItem
      Caption = 'About'
      ShortCut = 112
      OnClick = about1Click
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '*.h3d'
    Filter = 'Haxe 3D Model File (*.h3d)|*.h3d'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 8
    Top = 40
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '*.qmd'
    Filter = 'Quatro Model File (*.qmd)|*.qmd'
    Left = 8
    Top = 72
  end
  object OpenDialog2: TOpenDialog
    DefaultExt = '*.qmt'
    Filter = 'Quatro Text Model File (*.qmt)|*.qmt'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 8
    Top = 104
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 41
    Top = 8
  end
  object XPManifest1: TXPManifest
    Left = 270
    Top = 66
  end
  object OpenDialog3: TOpenDialog
    DefaultExt = '*.*'
    Filter = 'all images|*.*|Targa|*.tga|bmp|*.bmp|png|*.png|jpg|*.jpg'
    Left = 156
    Top = 70
  end
  object Timer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = TimerTimer
    Left = 24
    Top = 507
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'b3d'
    Filter = 'Blitz 3d|*.b3d'
    Left = 80
    Top = 168
  end
  object OpenDialog4: TOpenDialog
    DefaultExt = '*.ms3d'
    FileName = 'MilshShape'
    Filter = 'Milkshape|*.ms3d'
    Left = 168
    Top = 184
  end
  object OpenDialog5: TOpenDialog
    DefaultExt = '*.*'
    Filter = 'All Assimp files|*.*'
    Left = 128
    Top = 32
  end
end
