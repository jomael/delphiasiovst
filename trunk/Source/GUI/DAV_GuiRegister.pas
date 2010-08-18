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
  {$IFDEF FPC} LResources, {$ELSE} DAV_GuiGroup, DAV_GuiLevelMeter, {$ENDIF}
  DAV_GuiADSRGraph, DAV_GuiStaticWaveform, DAV_GuiDynamicWaveform,
  DAV_GuiAudioDataDisplay, DAV_GuiLED, DAV_GuiPanel, DAV_GuiDial, DAV_GuiLabel,
  DAV_GuiButton, DAV_GuiMidiKeys, DAV_GuiSelectBox, DAV_GuiCorrelationMeter,
  DAV_GuiVUMeter, DAV_GuiGraphXY, DAV_GuiGraphXYDesign, DAV_GuiDialDesign,
  DAV_GuiDialRenderer, {$IFNDEF FPC} DAV_GuiModular, DAV_GuiBackgrounds,
  {$ENDIF} DAV_GuiEQGraph, DAV_GuiEQSlide, DAV_GuiSlider, DAV_GuiMediaButton;

procedure Register;
begin
  RegisterComponents('ASIO/VST GUI', [TGuiDynamicWaveform, TGuiStaticWaveform,
    TGuiADSRGraph, TGuiAudioDataDisplay, TGuiLabel, TGuiPanel, TGuiLED,
    TGuiVUMeter, TGuiDial, TGuiDialMetal, TGuiCorrelationMeter, TGuiSelectBox,
    TGuiMidiKeys, TGuiButton, {$IFNDEF FPC} TGuiGroupA, TGuiGroupB, TGuiGroup,
    TGuiLevelMeter, TGuiColorLevelMeter, {$ENDIF} TGuiGraphXY, TGuiDialEx,
    TGuiDialImageList, TGuiDialImageRenderer, TGuiEQGraph, TGuiEQSlide,
    {$IFNDEF FPC} TGuiModular, TGuiBackground, {$ENDIF} TGuiSwitch,
    TGuiSlider, TGuiMediaButton]);

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
