object FmSimpleVibrato: TFmSimpleVibrato
  Left = 274
  Top = 80
  BorderStyle = bsNone
  Caption = 'Simple Vibrato'
  ClientHeight = 92
  ClientWidth = 134
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object DialSpeed: TGuiDial
    Left = 16
    Top = 33
    Width = 36
    Height = 36
    AntiAlias = gaaLinear3x
    CurveMapping = -1.799999952316284000
    DefaultPosition = 1.000000000000000000
    DialImageIndex = -1
    LineColor = 14277598
    LineWidth = 2
    Max = 10.000000000000000000
    Min = 0.009999999776482582
    GlyphCount = 65
    OnChange = DialSpeedChange
    PointerAngles.Start = 225
    PointerAngles.Range = 270
    PointerAngles.Resolution = 270.000000000000000000
    Position = 1.000000000000000000
    ScrollRange_Pixel = 400.000000000000000000
    StitchKind = skHorizontal
    WheelStep = 1.000000000000000000
  end
  object DialDepth: TGuiDial
    Left = 80
    Top = 33
    Width = 36
    Height = 36
    AntiAlias = gaaLinear3x
    DefaultPosition = 10.000000000000000000
    DialImageIndex = -1
    LineColor = 14277598
    LineWidth = 2
    Max = 100.000000000000000000
    GlyphCount = 65
    OnChange = DialDepthChange
    PointerAngles.Start = 225
    PointerAngles.Range = 270
    PointerAngles.Resolution = 270.000000000000000000
    Position = 10.000000000000000000
    ScrollRange_Pixel = 400.000000000000000000
    StitchKind = skHorizontal
    WheelStep = 1.000000000000000000
  end
  object LbSpeed: TGuiLabel
    Left = 8
    Top = 8
    Width = 50
    Height = 25
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = 'Speed'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14277598
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LbDepth: TGuiLabel
    Left = 72
    Top = 8
    Width = 50
    Height = 25
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = 'Depth'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14277598
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LbSpeedValue: TGuiLabel
    Left = 3
    Top = 68
    Width = 60
    Height = 20
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14277598
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LbDepthValue: TGuiLabel
    Left = 67
    Top = 68
    Width = 60
    Height = 20
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14277598
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
  end
end
