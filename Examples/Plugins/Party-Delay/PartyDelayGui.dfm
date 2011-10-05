object FmPartyDelay: TFmPartyDelay
  Left = 321
  Top = 236
  BorderStyle = bsNone
  Caption = 'Party Delay'
  ClientHeight = 358
  ClientWidth = 222
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
  object LbMix: TLabel
    Left = 8
    Top = 6
    Width = 20
    Height = 13
    Caption = 'Mix:'
  end
  object LbDry: TLabel
    Left = 39
    Top = 6
    Width = 17
    Height = 13
    Caption = 'Dry'
  end
  object LbWet: TLabel
    Left = 194
    Top = 6
    Width = 20
    Height = 13
    Caption = 'Wet'
  end
  object TC: TTabControl
    Left = 0
    Top = 27
    Width = 222
    Height = 312
    Align = alBottom
    TabOrder = 0
    Tabs.Strings = (
      'Band 1'
      'Band 2'
      'Band 3'
      'Band 4')
    TabIndex = 0
    OnChange = TCChange
    object LbPan: TLabel
      Left = 69
      Top = 30
      Width = 22
      Height = 13
      Caption = 'Pan:'
    end
    object LbLevel: TLabel
      Left = 62
      Top = 52
      Width = 29
      Height = 13
      Caption = 'Level:'
    end
    object LbDelay: TLabel
      Left = 25
      Top = 81
      Width = 31
      Height = 13
      Caption = 'Delay:'
    end
    object LbFeedback: TLabel
      Left = 6
      Top = 102
      Width = 50
      Height = 13
      Caption = 'Feedback:'
    end
    object LbFilterType: TLabel
      Left = 6
      Top = 134
      Width = 55
      Height = 13
      Caption = 'Filter Type:'
    end
    object LbFrequency: TLabel
      Left = 6
      Top = 158
      Width = 55
      Height = 13
      Caption = 'Frequency:'
    end
    object LbGain: TLabel
      Left = 36
      Top = 180
      Width = 25
      Height = 13
      Caption = 'Gain:'
    end
    object LbBandwidth: TLabel
      Left = 7
      Top = 202
      Width = 54
      Height = 13
      Caption = 'Bandwidth:'
    end
    object LbFreqShift: TLabel
      Left = 6
      Top = 234
      Width = 90
      Height = 13
      Caption = 'Frequency Shifter:'
    end
    object DialFreqShift: TGuiDial
      Left = 102
      Top = 228
      Width = 24
      Height = 24
      CircleColor = clBtnShadow
      DialImageIndex = -1
      LineColor = clBtnHighlight
      LineWidth = 2
      Max = 1.000000000000000000
      OnChange = DialFreqShiftChange
      PointerAngles.Start = 180
      PointerAngles.Resolution = 360.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbDrive: TLabel
      Left = 32
      Top = 266
      Width = 29
      Height = 13
      Caption = 'Drive:'
    end
    object LbBalance: TLabel
      Left = 20
      Top = 288
      Width = 41
      Height = 13
      Caption = 'Balance:'
    end
    object CbActive: TCheckBox
      Left = 6
      Top = 29
      Width = 52
      Height = 17
      Caption = 'Active'
      TabOrder = 0
      OnClick = CbActiveClick
    end
    object SbPan: TScrollBar
      Left = 97
      Top = 29
      Width = 118
      Height = 16
      Max = 1000
      Min = -1000
      PageSize = 0
      TabOrder = 1
      OnChange = SbPanChange
    end
    object SbLevel: TScrollBar
      Left = 97
      Top = 50
      Width = 118
      Height = 16
      Max = 60
      Min = -540
      PageSize = 0
      TabOrder = 2
      OnChange = SbLevelChange
    end
    object CbInvert: TCheckBox
      Left = 6
      Top = 50
      Width = 43
      Height = 17
      Caption = 'Inv.'
      TabOrder = 3
      OnClick = CbInvertClick
    end
    object SbDelay: TScrollBar
      Left = 62
      Top = 80
      Width = 153
      Height = 16
      Max = 10000
      PageSize = 0
      TabOrder = 4
      OnChange = SbDelayChange
    end
    object SbFeedback: TScrollBar
      Left = 62
      Top = 101
      Width = 153
      Height = 16
      Max = 1000
      PageSize = 0
      TabOrder = 5
      OnChange = SbFeedbackChange
    end
    object CbFilterType: TComboBox
      Left = 67
      Top = 131
      Width = 148
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 6
      Text = 'None'
      OnChange = CbFilterTypeChange
      Items.Strings = (
        'None'
        'Lowcut'
        'Lowshelf'
        'Peak'
        'Bandpass'
        'Allpass'
        'Notch'
        'Highshelf'
        'Highcut')
    end
    object SbFrequency: TScrollBar
      Left = 67
      Top = 157
      Width = 148
      Height = 16
      Max = 10000
      PageSize = 0
      TabOrder = 7
      OnChange = SbFrequencyChange
    end
    object SbFilterGain: TScrollBar
      Left = 67
      Top = 179
      Width = 148
      Height = 16
      Max = 150
      Min = -150
      PageSize = 0
      TabOrder = 8
      OnChange = SbFilterGainChange
    end
    object SbFilterBW: TScrollBar
      Left = 67
      Top = 201
      Width = 148
      Height = 16
      Max = 10000
      PageSize = 0
      TabOrder = 9
      OnChange = SbFilterBWChange
    end
    object SbFreqShift: TScrollBar
      Left = 132
      Top = 233
      Width = 83
      Height = 16
      Max = 10000
      PageSize = 0
      TabOrder = 10
      OnChange = SbFreqShiftChange
    end
    object SbDrive: TScrollBar
      Left = 67
      Top = 265
      Width = 148
      Height = 16
      Max = 1000
      PageSize = 0
      TabOrder = 11
      OnChange = SbDriveChange
    end
    object SbBalance: TScrollBar
      Left = 67
      Top = 287
      Width = 148
      Height = 16
      Max = 1000
      Min = -1000
      PageSize = 0
      TabOrder = 12
      OnChange = SbBalanceChange
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 339
    Width = 222
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object SbMix: TScrollBar
    Left = 62
    Top = 5
    Width = 126
    Height = 16
    Max = 1000
    PageSize = 0
    TabOrder = 2
    OnChange = SbMixChange
  end
end
