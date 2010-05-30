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
    AntiAlias = gaaLinear4x
    Caption = 'Crosstalk Cancellation'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
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
  object SbSpeakerDistance: TScrollBar
    Left = 291
    Top = 50
    Width = 141
    Height = 17
    Max = 5000
    Min = 10
    PageSize = 0
    Position = 100
    TabOrder = 0
    OnChange = SbSpeakerDistanceChange
  end
  object SbListenerDistance: TScrollBar
    Left = 291
    Top = 73
    Width = 141
    Height = 17
    Max = 5000
    Min = 10
    PageSize = 0
    Position = 100
    TabOrder = 1
    OnChange = SbListenerDistanceChange
  end
  object SbAttenuation: TScrollBar
    Left = 291
    Top = 119
    Width = 141
    Height = 17
    Max = 0
    Min = -150
    PageSize = 0
    Position = -60
    TabOrder = 2
    OnChange = SbAttenuationChange
  end
  object SbRecursionSteps: TScrollBar
    Left = 291
    Top = 96
    Width = 141
    Height = 17
    Max = 16
    Min = 1
    PageSize = 0
    Position = 1
    TabOrder = 3
    OnChange = SbRecursionStepsChange
  end
  object SbFilterFrequency: TScrollBar
    Left = 291
    Top = 169
    Width = 141
    Height = 17
    Max = 10000
    PageSize = 0
    TabOrder = 4
    OnChange = SbFilterFrequencyChange
  end
  object SbFilterGain: TScrollBar
    Left = 291
    Top = 193
    Width = 141
    Height = 17
    Max = 0
    Min = -150
    PageSize = 0
    Position = -60
    TabOrder = 5
    OnChange = SbFilterGainChange
  end
  object SbOutputGain: TScrollBar
    Left = 291
    Top = 231
    Width = 141
    Height = 17
    Max = 0
    Min = -150
    PageSize = 0
    Position = -60
    TabOrder = 6
    OnChange = SbOutputGainChange
  end
  object CBBypass: TCheckBox
    Left = 290
    Top = 256
    Width = 60
    Height = 17
    Caption = 'Bypass'
    Color = 11121848
    ParentColor = False
    TabOrder = 7
    OnClick = CBBypassClick
  end
  object CBAGC: TCheckBox
    Left = 356
    Top = 256
    Width = 128
    Height = 17
    Caption = 'Automatic Gain Control'
    Color = 11121848
    Enabled = False
    ParentColor = False
    TabOrder = 8
  end
end
