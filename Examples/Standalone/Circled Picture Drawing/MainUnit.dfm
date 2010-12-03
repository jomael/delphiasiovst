object FmPrimitivePictureEvolution: TFmPrimitivePictureEvolution
  Left = 300
  Top = 56
  Caption = 'Primitive Picture Evolution'
  ClientHeight = 234
  ClientWidth = 424
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBoxRef: TPaintBox
    Left = 8
    Top = 8
    Width = 201
    Height = 201
    OnPaint = PaintBoxRefPaint
  end
  object PaintBoxDraw: TPaintBox
    Left = 215
    Top = 8
    Width = 201
    Height = 201
    OnPaint = PaintBoxDrawPaint
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 215
    Width = 424
    Height = 19
    Panels = <
      item
        Text = 'No Reference'
        Width = 72
      end
      item
        Text = 'Circles:'
        Width = 64
      end
      item
        Text = 'Trials:'
        Width = 72
      end
      item
        Text = 'Cost:'
        Width = 80
      end
      item
        Text = 'Global Costs:'
        Width = 100
      end>
  end
  object MainMenu: TMainMenu
    Left = 24
    Top = 16
    object MiFile: TMenuItem
      Caption = '&File'
      object MiOpenReference: TMenuItem
        Caption = '&Open Reference...'
        OnClick = MiOpenReferenceClick
      end
      object MiSaveResult: TMenuItem
        Caption = '&Save Result...'
        OnClick = MiSaveResultClick
      end
      object MiSaveHighResolution: TMenuItem
        Caption = 'Save &High Resolution...'
        OnClick = MiSaveHighResolutionClick
      end
      object MiSaveFramed: TMenuItem
        Caption = 'Save &Framed...'
        OnClick = MiSaveFramedClick
      end
      object MiSaveAnimation: TMenuItem
        Caption = 'Save &Animation...'
        OnClick = MiSaveAnimationClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object MiOpenDrawing: TMenuItem
        Caption = 'Open &Drawing...'
        OnClick = MiOpenDrawingClick
      end
      object MiOpenBest: TMenuItem
        Caption = 'Open &Best...'
        OnClick = MiOpenBestClick
      end
      object MiCopyReference: TMenuItem
        Caption = '&Copy Reference'
        Visible = False
        OnClick = MiCopyReferenceClick
      end
      object MiSaveDrawing: TMenuItem
        Caption = 'Sa&ve Drawing...'
        Enabled = False
        OnClick = MiSaveDrawingClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MiExit: TMenuItem
        Caption = 'E&xit'
        OnClick = MiExitClick
      end
    end
    object MiEvolve: TMenuItem
      Caption = '&Optimizer'
      object MiStart: TMenuItem
        Action = AcStart
      end
      object MiStopContinue: TMenuItem
        Caption = 'St&op'
        Enabled = False
        OnClick = MiStopContinueClick
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object MiSavePopulation: TMenuItem
        Caption = 'Save &Population'
        Enabled = False
        OnClick = MiSavePopulationClick
      end
      object MiLoadPopulation: TMenuItem
        Caption = 'Load Population...'
        OnClick = MiLoadPopulationClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object MiNext: TMenuItem
        Action = AcNext
        Enabled = False
      end
      object MiBack: TMenuItem
        Action = AcBack
        Enabled = False
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object MiSettings: TMenuItem
        Action = AcSettings
      end
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.bmp'
    Filter = 'Bitmap (*.bmp)|*.bmp|Portable Network Graphics (*.png)|*.png'
    Left = 112
    Top = 16
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '.bmp'
    Filter = 'Bitmap (*.bmp)|*.bmp|Portable Network Graphics (*.png)|*.png'
    Left = 200
    Top = 16
  end
  object OpenDialogPrimitives: TOpenDialog
    Filter = 
      'Circles (*.Circles)|*.Circles|Rounded Rectangles (*.RoundRects)|' +
      '*.RoundRects'
    Left = 112
    Top = 72
  end
  object SaveDialogPrimitives: TSaveDialog
    Filter = 
      'Circles (*.Circles)|*.Circles|Rounded Rectangles (*.RoundRects)|' +
      '*.RoundRects'
    Left = 200
    Top = 72
  end
  object ActionList: TActionList
    Left = 24
    Top = 72
    object AcStart: TAction
      Category = 'Optimizer'
      Caption = '&Start'
      ShortCut = 120
      OnExecute = AcStartExecute
    end
    object AcNext: TAction
      Category = 'Optimizer'
      Caption = '&Next'
      ShortCut = 119
      OnExecute = AcNextExecute
    end
    object AcBack: TAction
      Category = 'Optimizer'
      Caption = '&Back'
      OnExecute = AcBackExecute
    end
    object AcSettings: TAction
      Category = 'Optimizer'
      Caption = 'S&ettings'
      ShortCut = 121
      OnExecute = AcSettingsExecute
    end
  end
end
