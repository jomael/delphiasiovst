object FmLabelTest: TFmLabelTest
  Left = 218
  Top = 77
  Caption = 'Label Test'
  ClientHeight = 99
  ClientWidth = 135
  Color = 7373965
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object LabelA: TGuiLabel
    Left = 8
    Top = 8
    Width = 57
    Height = 25
    Caption = 'Test'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LabelC: TGuiLabel
    Left = 8
    Top = 39
    Width = 57
    Height = 25
    AntiAlias = gaaLinear3x
    Caption = 'Test'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LabelB: TGuiLabel
    Left = 71
    Top = 8
    Width = 57
    Height = 25
    AntiAlias = gaaLinear2x
    Caption = 'Test'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clLime
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LabelD: TGuiLabel
    Left = 71
    Top = 39
    Width = 57
    Height = 25
    AntiAlias = gaaLinear4x
    Caption = 'Test'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clYellow
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object CbTransparent: TCheckBox
    Left = 25
    Top = 70
    Width = 79
    Height = 17
    Caption = 'Transparent'
    Color = 7373965
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    TabOrder = 0
    OnClick = CbTransparentClick
  end
end
