library PhaseRotator;

{$I DAV_Compiler.inc}

uses
  Interfaces,
  Forms,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  PhaseRotatorDSP in 'PhaseRotatorDSP.pas' {PhaseRotatorModule: TVSTModule};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TPhaseRotatorModule);
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
