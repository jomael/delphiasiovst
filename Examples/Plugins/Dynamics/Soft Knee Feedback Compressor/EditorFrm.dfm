object EditorForm: TEditorForm
  Left = 218
  Top = 81
  BorderStyle = bsNone
  Caption = 'EditorForm'
  ClientHeight = 128
  ClientWidth = 402
  Color = clSilver
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LbThresholdValue: TLabel
    Left = 8
    Top = 102
    Width = 64
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'dB'
    Transparent = False
  end
  object LbRatioValue: TLabel
    Left = 88
    Top = 102
    Width = 64
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Transparent = False
  end
  object LbAttackValue: TLabel
    Left = 166
    Top = 102
    Width = 64
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Transparent = False
  end
  object LbReleaseValue: TLabel
    Left = 246
    Top = 102
    Width = 64
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Transparent = False
  end
  object DialThreshold: TGuiDial
    Left = 8
    Top = 32
    Width = 64
    Height = 64
    CircleColor = clSilver
    DefaultPosition = -0.000000000100000001
    DialImageIndex = -1
    LineColor = clGray
    LineWidth = 2
    Max = -0.000000000100000001
    Min = -96.000000000000000000
    NumGlyphs = 31
    OnChange = DialThresholdChange
    PointerAngles.Start = 235
    PointerAngles.Range = 270
    PointerAngles.Resolution = 270.000000000000000000
    Position = -0.000000000100000001
    ScrollRange_Pixel = 400.000000000000000000
    StitchKind = skVertical
    WheelStep = 1.000000000000000000
  end
  object DialRatio: TGuiDial
    Left = 88
    Top = 32
    Width = 64
    Height = 64
    CircleColor = clSilver
    DialImageIndex = -1
    LineColor = clGray
    LineWidth = 2
    Max = 200.000000000000000000
    NumGlyphs = 31
    OnChange = DialRatioChange
    PointerAngles.Start = 235
    PointerAngles.Range = 270
    PointerAngles.Resolution = 270.000000000000000000
    ScrollRange_Pixel = 400.000000000000000000
    StitchKind = skVertical
    WheelStep = 1.000000000000000000
  end
  object DialAttack: TGuiDial
    Left = 166
    Top = 32
    Width = 64
    Height = 64
    CircleColor = clSilver
    DialImageIndex = -1
    LineColor = clGray
    LineWidth = 2
    Max = 300.000000000000000000
    Min = -200.000000000000000000
    NumGlyphs = 31
    OnChange = DialAttackChange
    PointerAngles.Start = 235
    PointerAngles.Range = 270
    PointerAngles.Resolution = 270.000000000000000000
    ScrollRange_Pixel = 400.000000000000000000
    StitchKind = skVertical
    WheelStep = 1.000000000000000000
  end
  object DialRelease: TGuiDial
    Left = 246
    Top = 32
    Width = 64
    Height = 64
    CircleColor = clSilver
    DefaultPosition = 699.000000000000000000
    DialImageIndex = -1
    LineColor = clGray
    LineWidth = 2
    Max = 3699.000000000000000000
    Min = 699.000000000000000000
    NumGlyphs = 31
    OnChange = DialReleaseChange
    PointerAngles.Start = 235
    PointerAngles.Range = 270
    PointerAngles.Resolution = 270.000000000000000000
    Position = 699.000000000000000000
    ScrollRange_Pixel = 400.000000000000000000
    StitchKind = skVertical
    WheelStep = 1.000000000000000000
  end
  object LbRatio: TGuiLabel
    Left = 88
    Top = 8
    Width = 64
    Height = 13
    Margins.Bottom = 0
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    AutoSize = True
    Caption = 'Ratio'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
    Shadow.Color = clBlack
  end
  object LbAttack: TGuiLabel
    Left = 166
    Top = 8
    Width = 64
    Height = 13
    Margins.Bottom = 0
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    AutoSize = True
    Caption = 'Attack'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
    Shadow.Color = clBlack
  end
  object LbRelease: TGuiLabel
    Left = 246
    Top = 8
    Width = 64
    Height = 13
    Margins.Bottom = 0
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    AutoSize = True
    Caption = 'Release'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
    Shadow.Color = clBlack
  end
  object LbThreshold: TGuiLabel
    Left = 8
    Top = 8
    Width = 64
    Height = 13
    Margins.Bottom = 0
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    AutoSize = True
    Caption = 'Threshold'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
    Shadow.Color = clBlack
  end
  object LbMakeUpValue: TLabel
    Left = 324
    Top = 102
    Width = 64
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Transparent = False
  end
  object DialMakeUp: TGuiDial
    Left = 324
    Top = 32
    Width = 64
    Height = 64
    CircleColor = clSilver
    DialImageIndex = -1
    LineColor = clGray
    LineWidth = 2
    Max = 40.000000000000000000
    NumGlyphs = 31
    OnChange = DialMakeUpChange
    PointerAngles.Start = 235
    PointerAngles.Range = 270
    PointerAngles.Resolution = 270.000000000000000000
    ScrollRange_Pixel = 400.000000000000000000
    StitchKind = skVertical
    WheelStep = 1.000000000000000000
  end
  object LbMakeUp: TGuiLabel
    Left = 324
    Top = 8
    Width = 64
    Height = 13
    Margins.Bottom = 0
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    AutoSize = True
    Caption = 'Makeup'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
    Shadow.Color = clBlack
  end
end