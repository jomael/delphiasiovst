object FmRingModulator: TFmRingModulator
  Left = 286
  Top = 81
  BorderStyle = bsNone
  Caption = 'Ring Modulator'
  ClientHeight = 129
  ClientWidth = 147
  Color = 7373965
  Font.Charset = DEFAULT_CHARSET
  Font.Color = 7373965
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnPaint = FormPaint
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GpFrequency: TGuiGroup
    Left = 8
    Top = 8
    Width = 131
    Height = 113
    AntiAlias = gaaLinear4x
    Caption = 'Frequency'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 7373965
    Font.Height = -16
    Font.Name = 'Times New Roman'
    Font.Style = [fsBold]
    GroupColor = 15659506
    LineColor = 15659506
    OutlineWidth = 3
    PanelColor = 7373965
    ParentFont = False
    Radius = 5
    TabOrder = 0
    object DialFrequency: TGuiDial
      Left = 40
      Top = 32
      Width = 48
      Height = 48
      CircleColor = 3226174
      CurveMapping = -2.069999933242798000
      DefaultPosition = 20.000000000000000000
      DialImageIndex = -1
      LineColor = 15659506
      LineWidth = 2
      Max = 1000.000000000000000000
      Min = 0.009999999776482582
      NumGlyphs = 65
      OnChange = DialFrequencyChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 10.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object PnDisplay: TGuiPanel
      Left = 8
      Top = 86
      Width = 114
      Height = 17
      AntiAlias = gaaLinear4x
      Caption = 'PnDisplay'
      LineColor = 5398887
      Linewidth = 1
      PanelColor = 3226174
      ParentColor = True
      Radius = 5
      TabOrder = 0
      UseDockManager = True
      Transparent = True
      object LbDisplay: TGuiLabel
        Left = 5
        Top = 2
        Width = 105
        Height = 12
        Alignment = taCenter
        AntiAlias = gaaLinear4x
        Caption = 'Ring Modulator'
        Color = 3226174
        Font.Charset = ANSI_CHARSET
        Font.Color = 15659506
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Shadow.Color = clBlack
      end
    end
  end
end
