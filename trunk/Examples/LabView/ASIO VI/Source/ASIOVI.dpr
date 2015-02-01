library ASIOVI;

{%TogetherDiagram 'ModelSupport_ASIOVI\default.txaPackage'}

uses
  FastMM4,
//  FastMove,
  ASIOVIObject in 'ASIOVIObject.pas',
  Registry in 'Registry.pas';

exports ASIOControlPanel;
exports ASIOGetNumDevices;
exports ASIOInitDriver;
exports ASIOSetDriverIndex;
exports ASIOGetDriverName;
exports ASIOGetDriverNames;
exports ASIOCanSampleRate;
exports ASIOSetSampleRate;
exports ASIODriverStart;
exports ASIODriverStop;
exports ASIOGetBufferSize;
exports ASIOGetChannels;
exports ASIOOutputType;
exports ASIOGetOutputLevel;
exports ASIOGetInputLevel;
exports ASIOSetOutputVolume;
exports ASIOSetOutputVolumedB;
exports ASIOSetSineFrequency;
exports ASIOReadWriteSize;
exports ASIOReadWriteSizeFixed;
exports ASIOReadWrite;
exports ASIOReadWriteX;
exports ASIOAutopilot;
exports ASIOSetExtraBufferSize;
exports ASIOBufferUnderrun;
exports ASIOResetBufferUnderruns;
exports ASIOGetLoopCounts;
exports ASIOSetLoopCounts;
exports ASIOSetClipFunction;
exports ASIOCalcMeters;

begin
end.

