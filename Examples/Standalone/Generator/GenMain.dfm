object FmGenerator: TFmGenerator
  Left = 379
  Top = 372
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Generator'
  ClientHeight = 239
  ClientWidth = 609
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    609
    239)
  PixelsPerInch = 96
  TextHeight = 13
  object LbChannelCount: TLabel
    Left = 203
    Top = 216
    Width = 75
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Channel Count:'
  end
  object LbVolume: TLabel
    Left = 359
    Top = 13
    Width = 41
    Height = 16
    Alignment = taCenter
    Caption = '-0.0 dB'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object LbP30: TLabel
    Left = 398
    Top = 34
    Width = 20
    Height = 13
    Alignment = taCenter
    Caption = '+30'
  end
  object LbM30: TLabel
    Left = 398
    Top = 128
    Width = 16
    Height = 13
    Alignment = taCenter
    Caption = '-30'
  end
  object LbZero: TLabel
    Left = 401
    Top = 81
    Width = 6
    Height = 13
    Alignment = taCenter
    Caption = '0'
  end
  object LED6: TGuiLED
    Left = 344
    Top = 34
    Width = 19
    Height = 19
    BorderStrength_Percent = 50.000000000000000000
    Brightness_Percent = 10.000000000000000000
    LEDColor = clRed
    LineWidth = 2.000000000000000000
    Uniformity_Percent = 36.754447937011720000
    Transparent = True
  end
  object LED5: TGuiLED
    Left = 344
    Top = 52
    Width = 19
    Height = 19
    BorderStrength_Percent = 50.000000000000000000
    Brightness_Percent = 10.000000000000000000
    LEDColor = clRed
    LineWidth = 2.000000000000000000
    Uniformity_Percent = 36.754447937011720000
    Transparent = True
  end
  object LED4: TGuiLED
    Left = 344
    Top = 70
    Width = 19
    Height = 19
    BorderStrength_Percent = 50.000000000000000000
    Brightness_Percent = 10.000000000000000000
    LEDColor = clRed
    LineWidth = 2.000000000000000000
    Uniformity_Percent = 36.754447937011720000
    Transparent = True
  end
  object LED3: TGuiLED
    Left = 344
    Top = 88
    Width = 19
    Height = 19
    BorderStrength_Percent = 50.000000000000000000
    Brightness_Percent = 10.000000000000000000
    LEDColor = clRed
    LineWidth = 2.000000000000000000
    Uniformity_Percent = 36.754447937011720000
    Transparent = True
  end
  object LED2: TGuiLED
    Left = 344
    Top = 106
    Width = 19
    Height = 19
    BorderStrength_Percent = 50.000000000000000000
    Brightness_Percent = 10.000000000000000000
    LEDColor = clRed
    LineWidth = 2.000000000000000000
    Uniformity_Percent = 36.754447937011720000
    Transparent = True
  end
  object LED1: TGuiLED
    Left = 344
    Top = 124
    Width = 19
    Height = 19
    BorderStrength_Percent = 50.000000000000000000
    Brightness_Percent = 10.000000000000000000
    LEDColor = clRed
    LineWidth = 2.000000000000000000
    Uniformity_Percent = 36.754447937011720000
    Transparent = True
  end
  object BtPlay: TSpeedButton
    Left = 441
    Top = 211
    Width = 48
    Height = 24
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000000000000868686008E8E
      8E00959595009F9F9F00A4A4A400ACACAC00B4B4B400BCBCBC00C4C4C400CCCC
      CC00D4D4D400DCDCDC00E3E3E300EBEBEB00F4F4F400FFFFFF00FFFFFFFFFFFF
      FFFFF127AFFFFFFFFFFFF69447AFFFFFFFFFF6BB85579FFFFFFFF6CBBA85569F
      FFFFF6CCBBBA8646AFFFF6CCCBBBA98536AFF7DCCCBBBAA9851FF7DDCCCBBBA9
      724FF7DDDCCCBA7449FFF7EDDDCA8558FFFFF7EEDB7658FFFFFFF8ED8568FFFF
      FFFFF79469AFFFFFFFFFF059FFFFFFFFFFFFFFFFFFFFFFFFFFFF}
    OnClick = BtPlayClick
  end
  object BtStop: TSpeedButton
    Left = 552
    Top = 211
    Width = 49
    Height = 24
    Glyph.Data = {
      36050000424D3605000000000000360400002800000010000000100000000100
      08000000000000010000000000000000000000010000000000007F7F7F00B5B5
      B500BABABA00BDBDBD00C0C0C000C2C2C200C3C3C300C4C4C400C5C5C500C8C8
      C800CBCBCB00CDCDCD00CECECE00D0D0D000D1D1D100D3D3D300D5D5D500D8D8
      D800DBDBDB00DDDDDD00DEDEDE00E0E0E000E1E1E100E3E3E300E4E4E400E6E6
      E600E9E9E900EBEBEB00EFEFEF00F2F2F200F4F4F400F7F7F700FAFAFA00FFFF
      FF000033330000336600003399000033CC000033FF0000660000006633000066
      6600006699000066CC000066FF00009900000099330000996600009999000099
      CC000099FF0000CC000000CC330000CC660000CC990000CCCC0000CCFF0000FF
      660000FF990000FFCC0033FF0000FF00330033006600330099003300CC003300
      FF00FF3300003333330033336600333399003333CC003333FF00336600003366
      330033666600336699003366CC003366FF003399000033993300339966003399
      99003399CC003399FF0033CC000033CC330033CC660033CC990033CCCC0033CC
      FF0033FF330033FF660033FF990033FFCC0033FFFF0066000000660033006600
      6600660099006600CC006600FF00663300006633330066336600663399006633
      CC006633FF00666600006666330066666600666699006666CC00669900006699
      330066996600669999006699CC006699FF0066CC000066CC330066CC990066CC
      CC0066CCFF0066FF000066FF330066FF990066FFCC00CC00FF00FF00CC009999
      000099339900990099009900CC009900000099333300990066009933CC009900
      FF00996600009966330099336600996699009966CC009933FF00999933009999
      6600999999009999CC009999FF0099CC000099CC330066CC660099CC990099CC
      CC0099CCFF0099FF000099FF330099CC660099FF990099FFCC0099FFFF00CC00
      000099003300CC006600CC009900CC00CC0099330000CC333300CC336600CC33
      9900CC33CC00CC33FF00CC660000CC66330099666600CC669900CC66CC009966
      FF00CC990000CC993300CC996600CC999900CC99CC00CC99FF00CCCC0000CCCC
      3300CCCC6600CCCC9900CCCCCC00CCCCFF00CCFF0000CCFF330099FF6600CCFF
      9900CCFFCC00CCFFFF00CC003300FF006600FF009900CC330000FF333300FF33
      6600FF339900FF33CC00FF33FF00FF660000FF663300CC666600FF669900FF66
      CC00CC66FF00FF990000FF993300FF996600FF999900FF99CC00FF99FF00FFCC
      0000FFCC3300FFCC6600FFCC9900FFCCCC00FFCCFF00FFFF3300CCFF6600FFFF
      9900FFFFCC006666FF0066FF660066FFFF00FF666600FF66FF00FFFF66002100
      A5005F5F5F00777777008686860096969600CBCBCB00B2B2B200D7D7D700DDDD
      DD00E3E3E300EAEAEA00F1F1F100F8F8F800F0FBFF00A4A0A000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00202020202020
      20202020202020202020200000000000000000000000000000202000120F0FBC
      BCBC070707030301002020001212120F0F0FBCBCBC0707030020200018121212
      0F0F0FBCBCBC07030020200018181212120F0F0FBCBCBC070020200018181812
      12120F0F0FBCBC07002020001B1818181212120F0F0FBC07002020001B1B1818
      181212120F0F0FBC002020001B1B1B1818181212120F0FBC002020001E1B1B1B
      1818181212120FBC002020001E1E1B1B1B1818181212120F002020001E1E1E1B
      1B1B18181812120F00202000201E1E1E1B1B1B18181812120020200000000000
      0000000000000000002020202020202020202020202020202020}
    OnClick = BtStopClick
  end
  object BtPause: TSpeedButton
    Left = 495
    Top = 211
    Width = 51
    Height = 24
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      04000000000080000000000000000000000010000000000000007F7F7F00D7D7
      D700D8D8D800D9D9D900DADADA00DBDBDB00DCDCDC00DDDDDD00DEDEDE00DFDF
      DF00FFFFFF0000FFFF00FF000000FF00FF00FFFF0000FFFFFF00AAAAAAAAAAAA
      AAAAA000000AA000000AA054440AA022210AA055440AA032220AA055540AA033
      220AA055550AA043320AA065550AA044330AA066550AA044430AA076650AA044
      440AA087660AA044440AA088760AA054440AA088870AA055440AA088880AA055
      540AA098880AA055550AA000000AA000000AAAAAAAAAAAAAAAAA}
    OnClick = BtPauseClick
  end
  object PcSelect: TPageControl
    Left = 8
    Top = 8
    Width = 313
    Height = 199
    ActivePage = TsSweep
    Anchors = [akLeft, akTop, akBottom]
    TabOrder = 0
    OnChange = PcSelectChange
    object TsWhiteNoise: TTabSheet
      Caption = 'White Noise'
      object LbWhiteNoiseGain: TLabel
        Left = 16
        Top = 24
        Width = 48
        Height = 13
        Caption = 'Gain [dB]:'
      end
      object LbDistribution: TLabel
        Left = 16
        Top = 61
        Width = 58
        Height = 13
        Caption = 'Distribution:'
      end
      object SeWhiteNoiseGain: TSpinEdit
        Left = 86
        Top = 21
        Width = 67
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 0
        Value = 0
        OnChange = SeWhiteNoiseGainChange
      end
      object RbRectangle: TRadioButton
        Left = 86
        Top = 60
        Width = 65
        Height = 17
        Caption = 'Rectangle'
        Checked = True
        TabOrder = 1
        TabStop = True
      end
      object RbTriangle: TRadioButton
        Left = 168
        Top = 60
        Width = 65
        Height = 17
        Caption = 'Triangle'
        TabOrder = 2
      end
    end
    object TsPinkNoise: TTabSheet
      Caption = 'Pink Noise'
      ImageIndex = 1
      object LbPinkNoiseGain: TLabel
        Left = 16
        Top = 24
        Width = 48
        Height = 13
        Caption = 'Gain [dB]:'
      end
      object SePinkNoiseGain: TSpinEdit
        Left = 86
        Top = 21
        Width = 67
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 0
        Value = 0
        OnChange = SePinkNoiseGainChange
      end
    end
    object TsSine: TTabSheet
      Caption = 'Sine'
      ImageIndex = 2
      DesignSize = (
        305
        171)
      object LbSineCount: TLabel
        Left = 16
        Top = 49
        Width = 117
        Height = 13
        Caption = 'Sine Count per Channel:'
      end
      object LbNoChannelSine: TLabel
        Left = 139
        Top = 3
        Width = 59
        Height = 13
        Caption = 'No Channel:'
        Visible = False
      end
      object LbSineNo: TLabel
        Left = 224
        Top = 31
        Width = 44
        Height = 13
        Caption = 'Sine No.:'
        Visible = False
      end
      object SeSineCount: TSpinEdit
        Left = 139
        Top = 46
        Width = 59
        Height = 22
        MaxValue = 99
        MinValue = 1
        TabOrder = 0
        Value = 1
        OnChange = SeSineCountChange
      end
      object GbParameterSine: TGroupBox
        Left = 3
        Top = 74
        Width = 299
        Height = 94
        Anchors = [akLeft, akTop, akBottom]
        Caption = ' Parameter Sine Voice '
        TabOrder = 1
        object LbFrequency: TLabel
          Left = 13
          Top = 25
          Width = 78
          Height = 13
          Caption = 'Frequency [Hz]:'
        end
        object LbInitialPhase: TLabel
          Left = 13
          Top = 52
          Width = 82
          Height = 13
          Caption = 'Initiap Phase ['#176']:'
        end
        object LbGain: TLabel
          Left = 167
          Top = 25
          Width = 48
          Height = 13
          Caption = 'Gain [dB]:'
        end
        object EdFrequency: TEdit
          Left = 101
          Top = 22
          Width = 60
          Height = 21
          TabOrder = 0
          Text = '1000'
          OnChange = EdFrequencyChange
        end
        object EdInitialPhase: TEdit
          Left = 101
          Top = 49
          Width = 60
          Height = 21
          TabOrder = 1
          Text = '0'
          OnChange = EdInitialPhaseChange
        end
        object EdSineGain: TEdit
          Left = 221
          Top = 22
          Width = 59
          Height = 21
          TabOrder = 2
          Text = '0'
          OnChange = EdSineGainChange
        end
      end
      object CbIdenticalChannelsSine: TCheckBox
        Left = 16
        Top = 22
        Width = 105
        Height = 17
        Caption = 'Identical Channels'
        TabOrder = 2
        Visible = False
        OnClick = CbIdenticalChannelsSineClick
      end
      object CbChannelSine: TComboBox
        Left = 139
        Top = 19
        Width = 59
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 3
        Text = '1'
        Visible = False
        OnChange = CbSineChannelChange
        Items.Strings = (
          '1'
          '2'
          '3'
          '4')
      end
      object CbSine: TComboBox
        Left = 224
        Top = 47
        Width = 59
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 4
        Text = '1'
        Visible = False
        OnChange = CbSineChannelChange
        Items.Strings = (
          '1')
      end
    end
    object TsSweep: TTabSheet
      Caption = 'Sweep'
      ImageIndex = 3
      DesignSize = (
        305
        171)
      object LbNoChannelSweep: TLabel
        Left = 139
        Top = 3
        Width = 59
        Height = 13
        Caption = 'No Channel:'
        Visible = False
      end
      object LbModulationTime: TLabel
        Left = 16
        Top = 49
        Width = 103
        Height = 13
        Caption = 'Modulation time [ms]:'
      end
      object CbIdenticalChannelsSweep: TCheckBox
        Left = 16
        Top = 22
        Width = 105
        Height = 17
        Caption = 'Identical Channels'
        TabOrder = 0
        Visible = False
        OnClick = CbIdenticalChannelsSweepClick
      end
      object CbChannelSweep: TComboBox
        Left = 139
        Top = 19
        Width = 59
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 1
        Text = '1'
        Visible = False
        OnChange = CbChannelSweepChange
        Items.Strings = (
          '1'
          '2'
          '3'
          '4')
      end
      object EdSweepModulationTime: TEdit
        Left = 139
        Top = 46
        Width = 59
        Height = 21
        TabOrder = 2
        Text = '1000'
        OnChange = EdSweepModulationTimeChange
      end
      object GbSweep: TGroupBox
        Left = 3
        Top = 74
        Width = 299
        Height = 94
        Anchors = [akLeft, akTop, akBottom]
        Caption = ' Parameter Sweep Channel '
        TabOrder = 3
        object LbStartFreq: TLabel
          Left = 13
          Top = 25
          Width = 80
          Height = 13
          Caption = 'Start Freq. [Hz]:'
        end
        object LbEndFreq: TLabel
          Left = 13
          Top = 52
          Width = 74
          Height = 13
          Caption = 'End Freq. [Hz]:'
        end
        object LbGainSweep: TLabel
          Left = 167
          Top = 25
          Width = 48
          Height = 13
          Caption = 'Gain [dB]:'
        end
        object EdStartFreq: TEdit
          Left = 101
          Top = 22
          Width = 60
          Height = 21
          TabOrder = 0
          Text = '20'
          OnChange = EdStartFreqChange
        end
        object EdEndFreq: TEdit
          Left = 101
          Top = 49
          Width = 60
          Height = 21
          TabOrder = 1
          Text = '20000'
          OnChange = EdEndFreqChange
        end
        object EdGainSweep: TEdit
          Left = 221
          Top = 22
          Width = 59
          Height = 21
          TabOrder = 2
          Text = '0'
          OnChange = EdGainSweepChange
        end
        object RbRisingSweep: TRadioButton
          Left = 13
          Top = 73
          Width = 113
          Height = 17
          Caption = 'Rising Sweep'
          Checked = True
          TabOrder = 3
          TabStop = True
          OnClick = RbRisingSweepClick
        end
        object RbFallingSweep: TRadioButton
          Left = 132
          Top = 73
          Width = 113
          Height = 17
          Caption = 'Falling Sweep'
          Enabled = False
          TabOrder = 4
          OnClick = RbFallingSweepClick
        end
      end
    end
    object TsWavFile: TTabSheet
      Caption = 'WAV-File'
      ImageIndex = 4
      object BtSelectWavFile: TButton
        Left = 64
        Top = 37
        Width = 177
        Height = 25
        Caption = 'Select WAV-File...'
        Default = True
        TabOrder = 0
        OnClick = BtSelectWavFileClick
      end
      object EdWavFile: TEdit
        Left = 16
        Top = 78
        Width = 273
        Height = 21
        TabOrder = 1
        OnChange = EdWavFileChange
      end
    end
  end
  object DriverCombo: TComboBox
    Left = 8
    Top = 213
    Width = 189
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    TabOrder = 1
    OnChange = DriverComboChange
  end
  object SeChannelCount: TSpinEdit
    Left = 284
    Top = 213
    Width = 37
    Height = 22
    Anchors = [akLeft, akBottom]
    MaxValue = 99
    MinValue = 1
    TabOrder = 2
    Value = 1
    OnChange = SeChannelCountChange
  end
  object TbVolume: TTrackBar
    Left = 367
    Top = 32
    Width = 25
    Height = 113
    Max = 300
    Min = -300
    Orientation = trVertical
    Frequency = 300
    ShowSelRange = False
    TabOrder = 3
    ThumbLength = 14
    OnChange = TbVolumeChange
  end
  object CbTimeLimit: TCheckBox
    Left = 344
    Top = 151
    Width = 65
    Height = 17
    Caption = 'Time Limit'
    TabOrder = 4
  end
  object PnTimeDomain: TPanel
    Left = 440
    Top = 8
    Width = 161
    Height = 160
    BevelOuter = bvLowered
    Color = clBlack
    TabOrder = 5
    object AdTimeDomain: TGuiAudioDataDisplay
      Left = 1
      Top = 1
      Width = 159
      Height = 158
      Align = alClient
      AudioDataCollection = ADC
      DisplayChannels = <>
      LineColor = clYellow
      LineWidth = 0
      Normalize = False
      XAxis.SampleUpper = 255
      XAxis.FractionalLower = -0.500000000000000000
      XAxis.FractionalUpper = 0.500000000000000000
    end
  end
  object PnTime: TPanel
    Left = 480
    Top = 174
    Width = 121
    Height = 31
    BevelOuter = bvLowered
    Color = clBlack
    TabOrder = 6
    DesignSize = (
      121
      31)
    object LbTime: TLabel
      Left = 1
      Top = 4
      Width = 118
      Height = 23
      Alignment = taCenter
      Anchors = [akLeft, akTop, akRight, akBottom]
      AutoSize = False
      Caption = '0:00:00:000'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clLime
      Font.Height = -21
      Font.Name = 'Times New Roman'
      Font.Style = []
      ParentFont = False
    end
  end
  object ASIOHost: TAsioHost
    AsioTime.Speed = 1.000000000000000000
    AsioTime.SampleRate = 44100.000000000000000000
    AsioTime.Flags = [atSystemTimeValid, atSamplePositionValid, atSampleRateValid, atSpeedValid]
    PreventClipping = pcDigital
    SampleRate = 44100.000000000000000000
    OnBufferSwitch32 = BufferSwitchWhiteNoise32
    OnSampleRateChanged = ASIOHostSampleRateChanged
    Left = 344
    Top = 176
  end
  object OpenWavDialog: TOpenDialog
    DefaultExt = '.wav'
    Filter = 
      'WAVE File (*.wav)|*.wav|AIFF File (*.aiff)|*aif?|AU file (*.au)|' +
      '*.au'
    Title = 'Select an audio file...'
    Left = 240
    Top = 40
  end
  object GuiTimer: TTimer
    Interval = 30
    OnTimer = GuiTimerTimer
    Left = 448
    Top = 176
  end
  object ADC: TAudioDataCollection32
    Channels = <>
    SampleFrames = 256
    SampleRate = 44100.000000000000000000
    Left = 464
    Top = 72
  end
end
