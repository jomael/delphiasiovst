object FmVT: TFmVT
  Left = 394
  Top = 220
  BorderStyle = bsNone
  Caption = 'Valueable'
  ClientHeight = 395
  ClientWidth = 359
  Color = 329145
  Font.Charset = DEFAULT_CHARSET
  Font.Color = 103128
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LbTrebleFLat: TGuiLabel
    Left = 65
    Top = 16
    Width = 37
    Height = 13
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = 'FLAT'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 103128
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
  end
  object LbBassFLat: TGuiLabel
    Left = 65
    Top = 204
    Width = 37
    Height = 12
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = 'FLAT'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 103128
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
  end
  object DialLowGain: TGuiDial
    Left = 20
    Top = 218
    Width = 128
    Height = 128
    LineColor = 103128
    Max = 12.000000000000000000
    Min = -12.000000000000000000
    NumGlyphs = 65
    PointerAngles.Start = 225
    PointerAngles.Range = 270
    PointerAngles.Resolution = 270.000000000000000000
    ScrollRange_Pixel = 200.000000000000000000
    StitchKind = skHorizontal
    OnChange = DialLowGainChange
  end
  object DialHiGain: TGuiDial
    Left = 20
    Top = 31
    Width = 128
    Height = 128
    LineColor = 103128
    Max = 12.000000000000000000
    Min = -12.000000000000000000
    NumGlyphs = 65
    PointerAngles.Start = 225
    PointerAngles.Range = 270
    PointerAngles.Resolution = 270.000000000000000000
    ScrollRange_Pixel = 200.000000000000000000
    StitchKind = skHorizontal
    OnChange = DialHiGainChange
  end
  object DialSelector: TGuiDial
    Left = 162
    Top = 142
    Width = 84
    Height = 84
    LineColor = 103128
    DefaultPosition = 1.000000000000000000
    Max = 4.000000000000000000
    Min = 1.000000000000000000
    NumGlyphs = 4
    PointerAngles.Start = 45
    PointerAngles.Range = 90
    PointerAngles.Resolution = 90.000000000000000000
    Position = 1.000000000000000000
    ScrollRange_Pixel = 200.000000000000000000
    StitchKind = skHorizontal
    OnChange = DialSelectorChange
  end
  object DialLowBypass: TGuiDial
    Left = 29
    Top = 349
    Width = 32
    Height = 32
    LineColor = 103128
    Max = 1.000000000000000000
    NumGlyphs = 2
    PointerAngles.Start = 180
    PointerAngles.Range = 180
    PointerAngles.Resolution = 180.000000000000000000
    ScrollRange_Pixel = 20.000000000000000000
    StitchKind = skHorizontal
    OnChange = DialLowBypassChange
  end
  object DialHiBypass: TGuiDial
    Left = 18
    Top = 163
    Width = 32
    Height = 32
    LineColor = 103128
    Max = 1.000000000000000000
    NumGlyphs = 2
    PointerAngles.Start = 180
    PointerAngles.Range = 180
    PointerAngles.Resolution = 180.000000000000000000
    ScrollRange_Pixel = 20.000000000000000000
    StitchKind = skHorizontal
    OnChange = DialHiBypassChange
  end
  object LbTitle: TGuiLabel
    Left = 162
    Top = 21
    Width = 179
    Height = 39
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = 'Valueable'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 103128
    Font.Height = -32
    Font.Name = 'Verdana'
    Font.Style = [fsBold, fsItalic]
  end
  object LbBass: TGuiLabel
    Left = 67
    Top = 352
    Width = 64
    Height = 23
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = 'BASS'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 103128
    Font.Height = -24
    Font.Name = 'Arial'
    Font.Style = [fsBold]
  end
  object LbTreble: TGuiLabel
    Left = 52
    Top = 167
    Width = 94
    Height = 25
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = 'TREBLE'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 103128
    Font.Height = -24
    Font.Name = 'Arial'
    Font.Style = [fsBold]
  end
  object LbSubTitle: TGuiLabel
    Left = 199
    Top = 65
    Width = 92
    Height = 39
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = '1963'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 103128
    Font.Height = -32
    Font.Name = 'Verdana'
    Font.Style = [fsBold, fsItalic]
  end
  object LbDrive: TGuiLabel
    Left = 202
    Top = 241
    Width = 74
    Height = 25
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = 'DRIVE'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 103128
    Font.Height = -24
    Font.Name = 'Arial'
    Font.Style = [fsBold]
  end
  object LbRoasty1: TGuiLabel
    Left = 256
    Top = 161
    Width = 65
    Height = 18
    AntiAlias = gaaLinear4x
    Caption = 'Roasty I'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 103128
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    OnClick = LbRoasty1Click
  end
  object LbRoasty2: TGuiLabel
    Left = 247
    Top = 136
    Width = 70
    Height = 18
    AntiAlias = gaaLinear4x
    Caption = 'Roasty II'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 103128
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    OnClick = LbRoasty2Click
  end
  object LbSteamin1: TGuiLabel
    Left = 256
    Top = 188
    Width = 82
    Height = 18
    AntiAlias = gaaLinear4x
    Caption = 'Steamin'#39' I'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 103128
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    OnClick = LbSteamin1Click
  end
  object LbSteamin2: TGuiLabel
    Left = 247
    Top = 213
    Width = 82
    Height = 19
    AntiAlias = gaaLinear4x
    Caption = 'Steamin'#39' II'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 103128
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    OnClick = LbSteamin2Click
  end
  object GuiLabel1: TGuiLabel
    Left = 14
    Top = 144
    Width = 20
    Height = 13
    Alignment = taRightJustify
    AntiAlias = gaaLinear4x
    Caption = '-12'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 103128
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
  end
  object GuiLabel2: TGuiLabel
    Left = 14
    Top = 330
    Width = 20
    Height = 13
    Alignment = taRightJustify
    AntiAlias = gaaLinear4x
    Caption = '-12'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 103128
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
  end
  object GuiLabel3: TGuiLabel
    Left = 132
    Top = 144
    Width = 14
    Height = 13
    AntiAlias = gaaLinear4x
    Caption = '12'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 103128
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
  end
  object GuiLabel4: TGuiLabel
    Left = 132
    Top = 330
    Width = 14
    Height = 13
    AntiAlias = gaaLinear4x
    Caption = '12'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 103128
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
  end
  object GuiPanel: TGuiPanel
    Left = 163
    Top = 275
    Width = 178
    Height = 106
    AntiAlias = gaaLinear4x
    Caption = 'GuiPanel'
    LineColor = 329145
    Linewidth = 3
    PanelColor = 103128
    ParentColor = True
    Radius = 8
    TabOrder = 0
    UseDockManager = True
    Transparent = True
    object DialOutputGain: TGuiDial
      Left = 71
      Top = 10
      Width = 32
      Height = 32
      Color = 103128
      LineColor = 329145
      Max = 12.000000000000000000
      Min = -12.000000000000000000
      NumGlyphs = 31
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      ScrollRange_Pixel = 100.000000000000000000
      StitchKind = skHorizontal
      OnChange = DialOutputGainChange
    end
    object DialChannel: TGuiDial
      Left = 114
      Top = 55
      Width = 32
      Height = 32
      Color = 103128
      LineColor = 329145
      DefaultPosition = 1.000000000000000000
      Max = 2.000000000000000000
      Min = 1.000000000000000000
      NumGlyphs = 2
      PointerAngles.Start = 180
      PointerAngles.Range = 180
      PointerAngles.Resolution = 180.000000000000000000
      Position = 1.000000000000000000
      ScrollRange_Pixel = 20.000000000000000000
      StitchKind = skHorizontal
      OnChange = DialChannelChange
    end
    object LbGain: TGuiLabel
      Left = 13
      Top = 15
      Width = 54
      Height = 25
      AntiAlias = gaaLinear4x
      Caption = 'GAIN:'
      Color = 103128
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 329145
      Font.Height = -19
      Font.Name = 'Arial'
      Font.Style = [fsBold]
    end
    object LbChannel: TGuiLabel
      Left = 13
      Top = 62
      Width = 100
      Height = 23
      AntiAlias = gaaLinear4x
      Caption = 'CHANNEL:'
      Color = 103128
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 329145
      Font.Height = -19
      Font.Name = 'Arial'
      Font.Style = [fsBold]
    end
    object LbGainValue: TGuiLabel
      Left = 109
      Top = 15
      Width = 66
      Height = 22
      AntiAlias = gaaLinear4x
      Caption = '0 dB'
      Color = 103128
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 329145
      Font.Height = -19
      Font.Name = 'Arial'
      Font.Style = [fsBold]
    end
    object LbMono: TGuiLabel
      Left = 112
      Top = 45
      Width = 37
      Height = 10
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'MONO'
      Color = 103128
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 329145
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = [fsBold]
    end
    object LbStereo: TGuiLabel
      Left = 111
      Top = 88
      Width = 42
      Height = 10
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'STEREO'
      Color = 103128
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 329145
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = [fsBold]
    end
  end
end