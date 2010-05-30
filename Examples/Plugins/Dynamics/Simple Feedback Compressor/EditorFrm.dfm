object EditorForm: TEditorForm
  Left = 355
  Top = 330
  BorderStyle = bsNone
  Caption = 'EditorForm'
  ClientHeight = 150
  ClientWidth = 344
  Color = 5983816
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel: TGuiPanel
    Left = 8
    Top = 8
    Width = 328
    Height = 133
    AntiAlias = gaaLinear4x
    LineColor = 14602441
    PanelColor = 14602441
    ParentColor = True
    Radius = 15
    TabOrder = 0
    UseDockManager = True
    Transparent = True
    object LbRatioValue: TLabel
      Left = 65
      Top = 94
      Width = 96
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Color = 14602441
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 5983816
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object LbReleaseValue: TLabel
      Left = 223
      Top = 94
      Width = 100
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Color = 14602441
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 5983816
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object DialRatio: TGuiDial
      Left = 11
      Top = 75
      Width = 48
      Height = 48
      Color = 14602441
      LineColor = 12097673
      CircleColor = 5983816
      DefaultPosition = 100.000000000000000000
      Max = 200.000000000000000000
      NumGlyphs = 31
      PointerAngles.Start = 235
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skVertical
      OnChange = DialRatioChange
    end
    object DialRelease: TGuiDial
      Left = 169
      Top = 75
      Width = 48
      Height = 48
      Color = 14602441
      LineColor = 12097673
      CircleColor = 5983816
      DefaultPosition = 699.000000000000000000
      Max = 3699.000000000000000000
      Min = 699.000000000000000000
      NumGlyphs = 31
      PointerAngles.Start = 235
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 699.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skVertical
      OnChange = DialReleaseChange
    end
    object LbThresholdValue: TLabel
      Left = 65
      Top = 32
      Width = 96
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'dB'
      Color = 14602441
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 5983816
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object DialThreshold: TGuiDial
      Left = 11
      Top = 13
      Width = 48
      Height = 48
      Color = 14602441
      LineColor = 12097673
      CircleColor = 5983816
      DefaultPosition = -20.000000000000000000
      Min = -96.000000000000000000
      NumGlyphs = 31
      PointerAngles.Start = 235
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skVertical
      OnChange = DialThresholdChange
    end
    object LbRatio: TGuiLabel
      Left = 65
      Top = 75
      Width = 41
      Height = 16
      Margins.Bottom = 0
      AntiAlias = gaaLinear4x
      AutoSize = True
      Caption = 'Ratio'
      Color = 14602441
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 5983816
      Font.Height = -13
      Font.Name = 'Verdana'
      Font.Style = [fsBold, fsUnderline]
    end
    object LbRelease: TGuiLabel
      Left = 223
      Top = 75
      Width = 58
      Height = 16
      Margins.Bottom = 0
      AntiAlias = gaaLinear4x
      AutoSize = True
      Caption = 'Release'
      Color = 14602441
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 5983816
      Font.Height = -13
      Font.Name = 'Verdana'
      Font.Style = [fsBold, fsUnderline]
    end
    object LbThreshold: TGuiLabel
      Left = 65
      Top = 13
      Width = 75
      Height = 16
      Margins.Bottom = 0
      AntiAlias = gaaLinear4x
      AutoSize = True
      Caption = 'Threshold'
      Color = 14602441
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 5983816
      Font.Height = -13
      Font.Name = 'Verdana'
      Font.Style = [fsBold, fsUnderline]
    end
    object LbAttack: TGuiLabel
      Left = 223
      Top = 13
      Width = 48
      Height = 16
      Margins.Bottom = 0
      AntiAlias = gaaLinear4x
      AutoSize = True
      Caption = 'Attack'
      Color = 14602441
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 5983816
      Font.Height = -13
      Font.Name = 'Verdana'
      Font.Style = [fsBold, fsUnderline]
    end
    object LbAttackValue: TLabel
      Left = 223
      Top = 32
      Width = 96
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Color = 14602441
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 5983816
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object DialAttack: TGuiDial
      Left = 169
      Top = 13
      Width = 48
      Height = 48
      Color = 14602441
      LineColor = 12097673
      CircleColor = 5983816
      Max = 300.000000000000000000
      Min = -200.000000000000000000
      NumGlyphs = 31
      PointerAngles.Start = 235
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skVertical
      OnChange = DialAttackChange
    end
  end
end
