unit DAV_GuiRegister;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, TypInfo,
  {$IFDEF FPC}
  LCLIntf, LazIDEIntf, PropEdits, ComponentEditors
  {$ELSE}
  {$IFDEF COMPILER6_UP}
  DesignIntf
  {$ELSE}
  DsgnIntf
  {$ENDIF}
  {$ENDIF};

procedure Register;

implementation

{$IFNDEF FPC}{$R ..\..\Resources\DAV_GuiRegister.res}{$ENDIF}

uses
  {$IFDEF FPC} LResources, {$ELSE} DAV_GuiGroup, DAV_GuiLevelMeter,
  DAV_GuiModular, DAV_GuiBackgrounds,{$ENDIF} DAV_GuiADSRGraph,
  DAV_GuiAudioDataDisplay, DAV_GuiButton, DAV_GuiCorrelationMeter,
  DAV_GuiDial, DAV_GuiDialDesign, DAV_GuiDialRenderer, DAV_GuiDynamicWaveform,
  DAV_GuiEQGraph, DAV_GuiEQSlide, DAV_GuiFont, DAV_GuiInscription,
  DAV_GuiGraphXY, DAV_GuiGraphXYDesign, DAV_GuiLabel, DAV_GuiLED,
  DAV_GuiMediaButton, DAV_GuiMidiKeys, DAV_GuiPaintBox, DAV_GuiPanel,
  DAV_GuiPixelMap, DAV_GuiPixelMapDesign, DAV_GuiPng, DAV_GuiPngDesign,
  DAV_GuiSelectBox, DAV_GuiSlider, DAV_GuiStaticWaveform,
  DAV_GuiStitchedButton, DAV_GuiStitchedControls, DAV_GuiStitchedDial,
  DAV_GuiStitchedDisplay, DAV_GuiStitchedPngList, DAV_GuiStitchedSwitch,
  DAV_GuiVUMeter;

procedure Register;
begin
  RegisterComponents('ASIO/VST GUI', [
    TGuiADSRGraph, TGuiAudioDataDisplay, TGuiButton, TGuiCorrelationMeter,
    TGuiDial, TGuiDialEx, TGuiDialImageList, TGuiDialImageRenderer,
    TGuiDialMetal, TGuiDynamicWaveform, TGuiEQGraph, TGuiEQSlide, TGuiGraphXY,
    TGuiInscription, TGuiLabel, TGuiLED, TGuiMediaButton, TGuiMidiKeys,
    TGuiOversampledGDIFont, TGuiPaintBox, TGuiPanel, TGuiSelectBox,
    TGuiSimpleGDIFont, TGuiSlider, TGuiStaticWaveform, TGuiStitchedButton,
    TGuiStitchedDial, TGuiStitchedDisplay, TGuiStitchedImageList,
    TGuiStitchedPNGList, TGuiStitchedSwitch, TGuiSwitch, TGuiVUMeter
    {$IFNDEF FPC}, TGuiGroupA, TGuiGroupB, TGuiGroup, TGuiLevelMeter,
    TGuiColorLevelMeter, TGuiModular, TGuiBackground {$ENDIF}]);

  RegisterPropertyEditor(TypeInfo(TGuiCustomPixelMap), nil, '', TPixelMapProperty);
  RegisterPropertyEditor(TypeInfo(TPortableNetworkGraphicPixel32), nil, '', TPngProperty);

  RegisterPropertyEditor(TypeInfo(string), TGuiDialLayerCollectionItem, 'PrimitiveClassName', nil);
  RegisterPropertyEditor(TypeInfo(TCustomGuiDialPrimitive), TGuiDialLayerCollectionItem, 'Primitive', TPrimitiveClassProperty);

  RegisterPropertyEditor(TypeInfo(string), TGuiGraphXYSeriesCollectionItem, 'SeriesClassName', nil);
  RegisterPropertyEditor(TypeInfo(TCustomGuiGraphXYSeries), TGuiGraphXYSeriesCollectionItem, 'Series', TSeriesClassProperty);
end;

{$IFDEF FPC}
initialization
  {$i ..\..\Resources\DAV_GuiRegister.lrs}
{$ENDIF}

end.
