object FmSettings: TFmSettings
  Left = 299
  Top = 51
  BorderStyle = bsDialog
  Caption = 'Settings'
  ClientHeight = 308
  ClientWidth = 385
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnShow = FormShow
  DesignSize = (
    385
    308)
  PixelsPerInch = 96
  TextHeight = 13
  object GbTrials: TGroupBox
    Left = 8
    Top = 8
    Width = 237
    Height = 81
    Caption = 'Trials'
    TabOrder = 0
    object LbTrialsPerCircle: TLabel
      Left = 8
      Top = 27
      Width = 75
      Height = 13
      Caption = 'Trials per circle:'
    end
    object LbUpdateTrials: TLabel
      Left = 8
      Top = 55
      Width = 132
      Height = 13
      Caption = 'Trials between GUI update:'
    end
    object SeTrialsPerCircle: TSpinEdit
      Left = 162
      Top = 24
      Width = 67
      Height = 22
      MaxValue = 9999999
      MinValue = 1
      TabOrder = 0
      Value = 3000
      OnKeyPress = SeSettingsPress
    end
    object SeUpdateTrials: TSpinEdit
      Left = 162
      Top = 52
      Width = 67
      Height = 22
      MaxValue = 9999999
      MinValue = 1
      TabOrder = 1
      Value = 30
      OnKeyPress = SeSettingsPress
    end
    object CbAutoTrials: TCheckBox
      Left = 100
      Top = 26
      Width = 43
      Height = 17
      Caption = 'auto'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
  end
  object GbOptimizer: TGroupBox
    Left = 8
    Top = 95
    Width = 237
    Height = 174
    Caption = 'Optimizer'
    TabOrder = 1
    object LbInitialSeed: TLabel
      Left = 8
      Top = 27
      Width = 56
      Height = 13
      Caption = 'Initial seed:'
    end
    object LbCrossover: TLabel
      Left = 8
      Top = 55
      Width = 86
      Height = 13
      Caption = 'Crossover (in %):'
    end
    object Label1: TLabel
      Left = 8
      Top = 83
      Width = 71
      Height = 13
      Caption = 'Weight (in %):'
    end
    object LbBest: TLabel
      Left = 8
      Top = 111
      Width = 99
      Height = 13
      Caption = 'Consider best (in %)'
    end
    object LbAdditional: TLabel
      Left = 8
      Top = 139
      Width = 133
      Height = 13
      Caption = 'Additional Population (in %)'
    end
    object SeInitialSeed: TSpinEdit
      Left = 162
      Top = 24
      Width = 67
      Height = 22
      MaxValue = 9999999
      MinValue = 5
      TabOrder = 0
      Value = 100
      OnKeyPress = SeSettingsPress
    end
    object SeCrossover: TSpinEdit
      Left = 162
      Top = 52
      Width = 67
      Height = 22
      MaxValue = 100
      MinValue = 0
      TabOrder = 1
      Value = 90
      OnKeyPress = SeSettingsPress
    end
    object SeWeight: TSpinEdit
      Left = 162
      Top = 80
      Width = 67
      Height = 22
      MaxValue = 100
      MinValue = 0
      TabOrder = 2
      Value = 70
      OnKeyPress = SeSettingsPress
    end
    object CbWeightDither: TCheckBox
      Left = 100
      Top = 82
      Width = 56
      Height = 17
      Caption = 'dither'
      TabOrder = 3
      OnKeyPress = SeSettingsPress
    end
    object SeBest: TSpinEdit
      Left = 162
      Top = 108
      Width = 67
      Height = 22
      MaxValue = 100
      MinValue = 0
      TabOrder = 4
      Value = 0
      OnKeyPress = SeSettingsPress
    end
    object SeAdditional: TSpinEdit
      Left = 162
      Top = 136
      Width = 67
      Height = 22
      MaxValue = 100
      MinValue = 0
      TabOrder = 5
      Value = 0
      OnKeyPress = SeSettingsPress
    end
    object CbAutoInitialSeed: TCheckBox
      Left = 100
      Top = 26
      Width = 43
      Height = 17
      Caption = 'auto'
      TabOrder = 6
      OnClick = CbAutoInitialSeedClick
      OnKeyPress = SeSettingsPress
    end
  end
  object BtOK: TButton
    Left = 140
    Top = 275
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = BtOKClick
  end
  object BtCancel: TButton
    Left = 221
    Top = 275
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object BtApply: TButton
    Left = 302
    Top = 275
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Apply'
    TabOrder = 4
    OnClick = BtApplyClick
  end
  object GbModifications: TGroupBox
    Left = 251
    Top = 95
    Width = 126
    Height = 174
    Caption = 'Modifications'
    TabOrder = 5
    object CbCorrectColor: TCheckBox
      Left = 12
      Top = 26
      Width = 97
      Height = 17
      Caption = 'Correct Color'
      TabOrder = 0
      OnKeyPress = SeSettingsPress
    end
    object CbCorrectPosition: TCheckBox
      Left = 12
      Top = 49
      Width = 97
      Height = 17
      Caption = 'Correct Position'
      TabOrder = 1
      OnKeyPress = SeSettingsPress
    end
    object CbCorrectRadius: TCheckBox
      Left = 12
      Top = 72
      Width = 97
      Height = 17
      Caption = 'Correct Radius'
      TabOrder = 2
      OnKeyPress = SeSettingsPress
    end
    object CbCorrectInvisible: TCheckBox
      Left = 12
      Top = 95
      Width = 97
      Height = 17
      Caption = 'Correct Invisible'
      TabOrder = 3
      OnKeyPress = SeSettingsPress
    end
    object CbRandomCircle: TCheckBox
      Left = 12
      Top = 118
      Width = 97
      Height = 17
      Caption = 'Random Circles'
      TabOrder = 4
      OnKeyPress = SeSettingsPress
    end
    object CbChangeOrder: TCheckBox
      Left = 12
      Top = 141
      Width = 97
      Height = 17
      Caption = 'Change Order'
      TabOrder = 5
      OnKeyPress = SeSettingsPress
    end
  end
  object GbPrimitives: TGroupBox
    Left = 251
    Top = 8
    Width = 126
    Height = 81
    Caption = 'Primitives'
    TabOrder = 6
    object LbCircleCount: TLabel
      Left = 12
      Top = 27
      Width = 33
      Height = 13
      Caption = 'Count:'
    end
    object SePrimitiveCount: TSpinEdit
      Left = 51
      Top = 24
      Width = 65
      Height = 22
      MaxValue = 999
      MinValue = 1
      TabOrder = 0
      Value = 1
      OnChange = SePrimitiveCountChange
      OnKeyPress = SeSettingsPress
    end
    object CbRandomOrder: TCheckBox
      Left = 12
      Top = 54
      Width = 97
      Height = 17
      Caption = 'Random Order'
      TabOrder = 1
      OnKeyPress = SeSettingsPress
    end
  end
end
