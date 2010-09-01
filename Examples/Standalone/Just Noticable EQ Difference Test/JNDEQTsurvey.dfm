object FmSurvey: TFmSurvey
  Left = 395
  Top = 240
  BorderStyle = bsDialog
  Caption = 'Survey'
  ClientHeight = 66
  ClientWidth = 319
  Color = 8620693
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnResize = FormResize
  DesignSize = (
    319
    66)
  PixelsPerInch = 96
  TextHeight = 14
  object LbSetup: TGuiLabel
    Left = 8
    Top = 13
    Width = 141
    Height = 15
    AntiAlias = gaaLinear4x
    Caption = 'Speaker/Headphone Setup:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
    Shadow.Color = 10333885
    Shadow.Visible = True
  end
  object LbGender: TGuiLabel
    Left = 8
    Top = 41
    Width = 44
    Height = 12
    AntiAlias = gaaLinear4x
    Caption = 'Gender:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
    Shadow.Color = 10333885
    Shadow.Visible = True
  end
  object LEDGenderMale: TGuiLED
    Left = 54
    Top = 38
    Width = 18
    Height = 18
    BorderStrength_Percent = 70.000000000000000000
    Brightness_Percent = 10.000000000000000000
    LEDColor = 10333885
    LineWidth = 2.200000047683716000
    Uniformity_Percent = 40.000000000000000000
    Transparent = True
    OnClick = LbGenderMaleClick
  end
  object LEDGenderFemale: TGuiLED
    Left = 102
    Top = 38
    Width = 18
    Height = 18
    BorderStrength_Percent = 70.000000000000000000
    Brightness_Percent = 10.000000000000000000
    LEDColor = 10333885
    LineWidth = 2.200000047683716000
    Uniformity_Percent = 40.000000000000000000
    Transparent = True
    OnClick = LbGenderFemaleClick
  end
  object LbGenderMale: TGuiLabel
    Left = 72
    Top = 41
    Width = 27
    Height = 12
    AntiAlias = gaaLinear4x
    Caption = 'Male'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
    Shadow.Color = 10333885
    Shadow.Visible = True
    OnClick = LbGenderMaleClick
  end
  object LbGenderFemale: TGuiLabel
    Left = 123
    Top = 41
    Width = 38
    Height = 12
    AntiAlias = gaaLinear4x
    Caption = 'Female'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
    Shadow.Color = 10333885
    Shadow.Visible = True
    OnClick = LbGenderFemaleClick
  end
  object LbAge: TGuiLabel
    Left = 179
    Top = 41
    Width = 25
    Height = 12
    AntiAlias = gaaLinear4x
    Caption = 'Age:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
    Shadow.Color = 10333885
    Shadow.Visible = True
  end
  object PnSetup: TGuiPanel
    Left = 155
    Top = 8
    Width = 156
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    AntiAlias = gaaLinear4x
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -64
    Font.Name = 'Tahoma'
    Font.Style = []
    LineColor = clBlack
    PanelColor = 10333885
    ParentColor = True
    Radius = 4
    TabOrder = 0
    UseDockManager = True
    DesignSize = (
      156
      22)
    object EdSetup: TEdit
      Left = 5
      Top = 4
      Width = 146
      Height = 14
      Anchors = [akLeft, akTop, akRight]
      BorderStyle = bsNone
      Color = 10333885
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
  end
  object PnAge: TGuiPanel
    Left = 210
    Top = 36
    Width = 47
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    AntiAlias = gaaLinear4x
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -64
    Font.Name = 'Tahoma'
    Font.Style = []
    LineColor = clBlack
    PanelColor = 10333885
    ParentColor = True
    Radius = 4
    TabOrder = 1
    UseDockManager = True
    DesignSize = (
      47
      22)
    object EdAge: TEdit
      Left = 5
      Top = 4
      Width = 37
      Height = 14
      Anchors = [akLeft, akTop, akRight]
      BorderStyle = bsNone
      Color = 10333885
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
  end
  object PnOK: TGuiPanel
    Left = 263
    Top = 36
    Width = 48
    Height = 22
    AntiAlias = gaaLinear4x
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -64
    Font.Name = 'Tahoma'
    Font.Style = []
    LineColor = clBlack
    PanelColor = 10333885
    ParentColor = True
    Radius = 5
    TabOrder = 2
    UseDockManager = True
    OnClick = PnOKClick
    DesignSize = (
      48
      22)
    object LbOK: TGuiLabel
      Left = 12
      Top = 3
      Width = 23
      Height = 16
      Alignment = taCenter
      Anchors = [akLeft, akTop, akRight]
      AntiAlias = gaaLinear4x
      Caption = 'OK'
      Color = 10333885
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Shadow.Color = 5663873
      Shadow.Visible = True
      OnClick = PnOKClick
    end
  end
end
