library PhaseAdjustment;

{$I DAV_Compiler.inc}

uses
  Interfaces,
  Forms,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  PhaseAdjustmentDSP in 'PhaseAdjustmentDSP.pas' {PhaseAdjustmentModule: TVSTModule};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  Result := VstModuleMain(AudioMasterCallback, TPhaseAdjustmentModule);
end;

exports
{$IFDEF DARWIN}  {OS X entry points}
  VSTPluginMain name '_main',
  VSTPluginMain name '_main_macho',
  VSTPluginMain name '_VSTPluginMain';
{$ELSE}
  VSTPluginMain name 'main',
  VSTPluginMain name 'main_plugin',
  VSTPluginMain name 'VSTPluginMain';
{$ENDIF}

end.
