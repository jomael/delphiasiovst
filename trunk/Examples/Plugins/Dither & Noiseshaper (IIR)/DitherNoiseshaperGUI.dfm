object FmDitherNoiseshaper: TFmDitherNoiseshaper
  Left = 264
  Top = 81
  BorderStyle = bsNone
  Caption = 'Dither & Noiseshaper Example'
  ClientHeight = 56
  ClientWidth = 241
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LbNoiseshaperType: TLabel
    Left = 8
    Top = 62
    Width = 117
    Height = 13
    Caption = 'Noiseshaper Frequency:'
  end
  object LbFinalBitDepth: TLabel
    Left = 4
    Top = 7
    Width = 73
    Height = 13
    Caption = 'Final Bit Depth:'
  end
  object LbBit: TLabel
    Left = 156
    Top = 7
    Width = 12
    Height = 13
    Caption = 'Bit'
  end
  object LbDitherType: TLabel
    Left = 8
    Top = 36
    Width = 60
    Height = 13
    Caption = 'Dither Type:'
  end
  object LbDitherAmp: TLabel
    Left = 182
    Top = 36
    Width = 25
    Height = 13
    Caption = 'Amp:'
  end
  object DialAmplitude: TGuiDial
    Left = 210
    Top = 29
    Width = 24
    Height = 24
    CircleColor = clBtnShadow
    CurveMapping = -1.000000000000000000
    DefaultPosition = 1.000000000000000000
    DialImageIndex = -1
    LineColor = clBtnHighlight
    LineWidth = 2
    Max = 4.000000000000000000
    OnChange = DialAmplitudeChange
    PointerAngles.Start = 225
    PointerAngles.Range = 270
    PointerAngles.Resolution = 270.000000000000000000
    Position = 1.000000000000000000
    ScrollRange_Pixel = 400.000000000000000000
    StitchKind = skHorizontal
    WheelStep = 1.000000000000000000
  end
  object SeBitDepth: TSpinEdit
    Left = 100
    Top = 4
    Width = 49
    Height = 22
    MaxValue = 32
    MinValue = 1
    TabOrder = 0
    Value = 16
    OnChange = SeBitDepthChange
  end
  object CbLimit: TCheckBox
    Left = 187
    Top = 6
    Width = 42
    Height = 17
    Caption = 'Limit'
    TabOrder = 1
    OnClick = CbLimitClick
  end
  object CbDitherType: TComboBox
    Left = 74
    Top = 32
    Width = 99
    Height = 21
    Style = csDropDownList
    ItemIndex = 4
    TabOrder = 2
    Text = 'Fast Gauss'
    OnChange = CbDitherTypeChange
    Items.Strings = (
      'none'
      'Rectangle'
      'Triangular'
      'Gauss'
      'Fast Gauss')
  end
  object SbFrequency: TScrollBar
    Left = 131
    Top = 61
    Width = 103
    Height = 16
    LargeChange = 100
    Max = 1000
    PageSize = 0
    SmallChange = 10
    TabOrder = 3
    OnChange = SbFrequencyChange
  end
end
