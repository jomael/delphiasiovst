object FmVOL: TFmVOL
  Left = 317
  Height = 277
  Top = 85
  Width = 374
  BorderStyle = bsNone
  Caption = 'VST Opcode Logger'
  ClientHeight = 277
  ClientWidth = 374
  Font.Height = -11
  Font.Name = 'Tahoma'
  OnShow = FormShow
  LCLVersion = '0.9.29'
  object LbParameter: TLabel
    Left = 8
    Height = 14
    Top = 30
    Width = 64
    Caption = 'Parameter 1:'
    ParentColor = False
  end
  object LbParameterValue: TLabel
    Left = 287
    Height = 14
    Top = 30
    Width = 80
    Caption = 'Parameter Value'
    ParentColor = False
  end
  object BtClear: TButton
    Left = 299
    Height = 25
    Top = 0
    Width = 75
    Anchors = [akTop, akRight]
    Caption = '&Clear'
    OnClick = BtClearClick
    TabOrder = 1
  end
  object MOpcodeLog: TMemo
    Left = 0
    Height = 229
    Top = 48
    Width = 374
    Align = alBottom
    Anchors = [akTop, akLeft, akRight, akBottom]
    Lines.Strings = (
      'MOpcodeLog'
    )
    ReadOnly = True
    TabOrder = 0
  end
  object BtUpdate: TButton
    Left = 224
    Height = 25
    Top = 0
    Width = 75
    Anchors = [akTop, akRight]
    Caption = '&Update'
    OnClick = BtUpdateClick
    TabOrder = 2
  end
  object CBAutoUpdates: TCheckBox
    Left = 4
    Height = 17
    Top = 3
    Width = 111
    Caption = 'Automatic Updates'
    TabOrder = 3
  end
  object BtSaveAs: TButton
    Left = 149
    Height = 25
    Top = 0
    Width = 75
    Anchors = [akTop, akRight]
    Caption = 'Save &As...'
    OnClick = BtSaveAsClick
    TabOrder = 4
  end
  object Sb1: TScrollBar
    Left = 77
    Height = 15
    Top = 29
    Width = 204
    Max = 2000
    Min = 1000
    PageSize = 0
    Position = 1000
    TabOrder = 5
    OnChange = Sb1Change
  end
  object SaveDialog: TSaveDialog
    Title = 'Save As...'
    DefaultExt = '.log'
    Filter = 'Logfile (*.log)|*.log'
    left = 128
    top = 40
  end
end