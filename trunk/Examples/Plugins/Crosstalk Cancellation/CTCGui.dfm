object FmCTC: TFmCTC
  Left = 339
  Top = 199
  BorderStyle = bsNone
  Caption = 'Crosstalk Cancellation'
  ClientHeight = 281
  ClientWidth = 665
  Color = 13160660
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    665
    281)
  PixelsPerInch = 96
  TextHeight = 13
  object LbSpeakerDistance: TLabel
    Left = 183
    Top = 51
    Width = 102
    Height = 13
    Caption = 'Speaker Distance:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
  object LbListenerDistance: TLabel
    Left = 183
    Top = 75
    Width = 101
    Height = 13
    Caption = 'Listener Distance:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
  object LbSpeakerDistanceValue: TLabel
    Left = 438
    Top = 51
    Width = 14
    Height = 13
    Caption = '1m'
    Transparent = True
  end
  object LbListenerDistanceValue: TLabel
    Left = 438
    Top = 75
    Width = 14
    Height = 13
    Caption = '1m'
    Transparent = True
  end
  object LbAttenuation: TLabel
    Left = 183
    Top = 121
    Width = 71
    Height = 13
    Caption = 'Attenuation:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
  object LbAttenuationValue: TLabel
    Left = 438
    Top = 121
    Width = 22
    Height = 13
    Caption = '-3dB'
    Transparent = True
  end
  object LbRecursionSteps: TLabel
    Left = 183
    Top = 98
    Width = 94
    Height = 13
    Caption = 'Recursion Steps:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
  object LbRecursionStepsValue: TLabel
    Left = 438
    Top = 98
    Width = 6
    Height = 13
    Caption = '3'
    Transparent = True
  end
  object LbFilterType: TLabel
    Left = 183
    Top = 146
    Width = 63
    Height = 13
    Caption = 'Filter Type:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
  object LbFilterFrequencyValue: TLabel
    Left = 438
    Top = 170
    Width = 39
    Height = 13
    Caption = '1400 Hz'
    Transparent = True
  end
  object Image1: TImage
    Left = 8
    Top = 8
    Width = 169
    Height = 265
  end
  object Image2: TImage
    Left = 490
    Top = 8
    Width = 169
    Height = 265
    Anchors = [akTop, akRight]
  end
  object LbFilterTypeValue: TLabel
    Left = 291
    Top = 146
    Width = 76
    Height = 13
    Caption = 'Simple Highpass'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object LbFilterFrequency: TLabel
    Left = 183
    Top = 170
    Width = 94
    Height = 13
    Caption = 'Filter Frequency:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
  object LbFilterGainValue: TLabel
    Left = 438
    Top = 194
    Width = 31
    Height = 13
    Caption = '-10 dB'
    Transparent = True
  end
  object LbFilterGain: TLabel
    Left = 183
    Top = 194
    Width = 60
    Height = 13
    Caption = 'Filter Gain:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
  object LbOutputGain: TLabel
    Left = 183
    Top = 233
    Width = 70
    Height = 13
    Caption = 'Output Gain:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
  object LbOutputGainValue: TLabel
    Left = 438
    Top = 233
    Width = 25
    Height = 13
    Caption = '-6 dB'
    Transparent = True
  end
  object LbTitle: TGuiLabel
    Left = 183
    Top = 8
    Width = 301
    Height = 30
    Alignment = taCenter
    Caption = 'Crosstalk Cancellation'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    FontOversampling = fo4x
    ParentFont = False
    Shadow.Blur = 4.000000000000000000
    Transparent = True
  end
  object LbSwitches: TLabel
    Left = 183
    Top = 258
    Width = 53
    Height = 13
    Caption = 'Switches:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
  object CBBypass: TGuiControlsCheckBox
    Left = 290
    Top = 256
    Width = 60
    Height = 17
    Caption = 'Bypass'
    Color = 11121848
    ParentColor = False
    TabOrder = 0
    OnClick = CBBypassClick
    Transparent = True
    Native = False
  end
  object CBAGC: TGuiControlsCheckBox
    Left = 356
    Top = 256
    Width = 128
    Height = 17
    Caption = 'Automatic Gain Control'
    Color = 11121848
    Enabled = False
    ParentColor = False
    TabOrder = 1
    Transparent = True
    Native = False
  end
  object SbSpeakerDistance: TGuiSlider
    Left = 291
    Top = 50
    Width = 141
    Height = 17
    BorderRadius = 4.000000000000000000
    BorderWidth = 1.500000000000000000
    Color = 11121848
    DefaultValue = 50.000000000000000000
    Max = 5000.000000000000000000
    Min = 10.000000000000000000
    ParentColor = False
    Value = 1000.000000000000000000
    SlideColor = 6316128
    OnChange = SbSpeakerDistanceChange
  end
  object SbListenerDistance: TGuiSlider
    Left = 291
    Top = 73
    Width = 141
    Height = 17
    BorderRadius = 4.000000000000000000
    BorderWidth = 1.500000000000000000
    Color = 11121848
    DefaultValue = 50.000000000000000000
    Max = 5000.000000000000000000
    Min = 10.000000000000000000
    ParentColor = False
    Value = 1000.000000000000000000
    SlideColor = 6316128
    OnChange = SbListenerDistanceChange
  end
  object SbAttenuation: TGuiSlider
    Left = 291
    Top = 119
    Width = 141
    Height = 17
    BorderRadius = 4.000000000000000000
    BorderWidth = 1.500000000000000000
    Color = 11121848
    Min = -150.000000000000000000
    ParentColor = False
    Value = -30.000000000000000000
    SlideColor = 6316128
    OnChange = SbAttenuationChange
    DefaultValue = 0.000000000000000000
    Max = 0.000000000000000000
  end
  object SbRecursionSteps: TGuiSlider
    Left = 291
    Top = 96
    Width = 141
    Height = 17
    BorderRadius = 4.000000000000000000
    BorderWidth = 1.500000000000000000
    Color = 11121848
    DefaultValue = 16.000000000000000000
    Max = 16.000000000000000000
    Min = 1.000000000000000000
    ParentColor = False
    Value = 3.000000000000000000
    SlideColor = 6316128
    OnChange = SbRecursionStepsChange
  end
  object SbFilterFrequency: TGuiSlider
    Left = 291
    Top = 169
    Width = 141
    Height = 17
    BorderRadius = 4.000000000000000000
    BorderWidth = 1.500000000000000000
    Color = 11121848
    DefaultValue = 50.000000000000000000
    Max = 10000.000000000000000000
    ParentColor = False
    Value = 1400.000000000000000000
    SlideColor = 6316128
    OnChange = SbFilterFrequencyChange
  end
  object SbFilterGain: TGuiSlider
    Left = 291
    Top = 193
    Width = 141
    Height = 17
    BorderRadius = 4.000000000000000000
    BorderWidth = 1.500000000000000000
    Color = 11121848
    Min = -150.000000000000000000
    ParentColor = False
    Value = -10.000000000000000000
    SlideColor = 6316128
    OnChange = SbFilterGainChange
    DefaultValue = 0.000000000000000000
    Max = 0.000000000000000000
  end
  object SbOutputGain: TGuiSlider
    Left = 291
    Top = 231
    Width = 141
    Height = 17
    BorderRadius = 4.000000000000000000
    BorderWidth = 1.500000000000000000
    Color = 11121848
    Min = -150.000000000000000000
    ParentColor = False
    Value = -6.000000000000000000
    SlideColor = 6316128
    OnChange = SbOutputGainChange
    DefaultValue = 0.000000000000000000
    Max = 0.000000000000000000
  end
end
