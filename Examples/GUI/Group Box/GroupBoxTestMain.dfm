object FmGroupBoxTest: TFmGroupBoxTest
  Left = 218
  Top = 77
  Caption = 'GroupBox Test'
  ClientHeight = 220
  ClientWidth = 278
  Color = clBtnFace
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
  object LbOutlineWidth: TLabel
    Left = 8
    Top = 149
    Width = 69
    Height = 13
    Caption = 'Outline Width:'
  end
  object LbRoundRadius: TLabel
    Left = 8
    Top = 173
    Width = 70
    Height = 13
    Caption = 'Round Radius:'
  end
  object ShGroupColor: TShape
    Left = 129
    Top = 196
    Width = 17
    Height = 17
    Brush.Color = clBtnShadow
    OnMouseDown = ShGroupColorMouseDown
  end
  object LbColor: TLabel
    Left = 94
    Top = 197
    Width = 29
    Height = 13
    Caption = 'Color:'
  end
  object GroupA: TGuiGroup
    Left = 8
    Top = 8
    Width = 128
    Height = 64
    Caption = 'Group A'
    TabOrder = 0
  end
  object GroupB: TGuiGroup
    Left = 142
    Top = 8
    Width = 128
    Height = 64
    AntiAlias = gaaLinear2x
    Caption = 'Group B'
    TabOrder = 1
  end
  object GroupC: TGuiGroup
    Left = 8
    Top = 78
    Width = 128
    Height = 64
    AntiAlias = gaaLinear3x
    Caption = 'Group C'
    TabOrder = 2
  end
  object GroupD: TGuiGroup
    Left = 142
    Top = 78
    Width = 128
    Height = 64
    AntiAlias = gaaLinear4x
    Caption = 'Group D'
    TabOrder = 3
  end
  object TbOutlineWidth: TTrackBar
    Left = 84
    Top = 148
    Width = 186
    Height = 18
    TabOrder = 4
    ThumbLength = 12
    OnChange = TbOutlineWidthChange
  end
  object TbRoundRadius: TTrackBar
    Left = 84
    Top = 172
    Width = 186
    Height = 18
    TabOrder = 5
    ThumbLength = 12
    OnChange = TbRoundRadiusChange
  end
  object CbTransparent: TCheckBox
    Left = 8
    Top = 196
    Width = 80
    Height = 17
    Caption = 'Transparent'
    TabOrder = 6
    OnClick = CbTransparentClick
  end
  object ColorDialog: TColorDialog
    Left = 168
    Top = 184
  end
end
