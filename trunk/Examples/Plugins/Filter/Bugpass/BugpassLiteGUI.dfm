object FmBugpassLite: TFmBugpassLite
  Left = 425
  Top = 147
  BorderStyle = bsNone
  Caption = 'Bugpass Lite'
  ClientHeight = 173
  ClientWidth = 325
  Color = 14733494
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
    325
    173)
  PixelsPerInch = 96
  TextHeight = 13
  object LbSubtitleShadow: TGuiLabel
    Left = 258
    Top = 87
    Width = 60
    Height = 34
    Alignment = taCenter
    Oversampling = fo4x
    Caption = 'Lite'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 3616034
    Font.Height = -27
    Font.Name = 'Comic Sans MS'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
    Shadow.Color = clBlack
  end
  object LbTitleShadow: TGuiLabel
    Left = 10
    Top = 10
    Width = 309
    Height = 78
    Alignment = taCenter
    Oversampling = fo4x
    Caption = 'BUGPASS'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 3616034
    Font.Height = -64
    Font.Name = 'Comic Sans MS'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
    Shadow.Color = clBlack
  end
  object LbTitle: TGuiLabel
    Left = 8
    Top = 8
    Width = 309
    Height = 78
    Alignment = taCenter
    Oversampling = fo4x
    Caption = 'BUGPASS'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 12693147
    Font.Height = -64
    Font.Name = 'Comic Sans MS'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
    Shadow.Color = clBlack
  end
  object LbFreqLowValue: TGuiLabel
    Left = 8
    Top = 144
    Width = 81
    Height = 21
    Alignment = taCenter
    Anchors = [akLeft, akBottom]
    Oversampling = fo4x
    Caption = '1 kHz'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 12693147
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
    Shadow.Color = clBlack
  end
  object FrequencyBar: TPaintBox
    Left = 8
    Top = 127
    Width = 309
    Height = 16
    Anchors = [akLeft, akTop, akRight, akBottom]
    OnMouseDown = FrequencyBarMouseDown
    OnMouseMove = FrequencyBarMouseMove
    OnMouseUp = FrequencyBarMouseUp
    OnPaint = PaintBoxPaint
  end
  object LbSubTitle: TGuiLabel
    Left = 257
    Top = 86
    Width = 60
    Height = 34
    Alignment = taCenter
    Oversampling = fo4x
    Caption = 'Lite'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 12693147
    Font.Height = -27
    Font.Name = 'Comic Sans MS'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
    Shadow.Color = clBlack
  end
  object LbFreqHighValue: TGuiLabel
    Left = 236
    Top = 144
    Width = 81
    Height = 21
    Alignment = taCenter
    Anchors = [akLeft, akBottom]
    Oversampling = fo4x
    Caption = '1 kHz'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 12693147
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
    Shadow.Color = clBlack
  end
  object GuiLabel1: TGuiLabel
    Left = 8
    Top = 87
    Width = 61
    Height = 34
    Alignment = taCenter
    Oversampling = fo4x
    Caption = 'VST'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 3616034
    Font.Height = -27
    Font.Name = 'Comic Sans MS'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
    Shadow.Color = clBlack
  end
  object GuiLabel2: TGuiLabel
    Left = 7
    Top = 86
    Width = 61
    Height = 34
    Alignment = taCenter
    Oversampling = fo4x
    Caption = 'VST'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 12693147
    Font.Height = -27
    Font.Name = 'Comic Sans MS'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
    Shadow.Color = clBlack
  end
end
