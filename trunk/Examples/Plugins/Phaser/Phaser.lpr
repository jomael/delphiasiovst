{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library Phaser;

{$I DAV_Compiler.inc}

uses
  Interfaces,
  Forms,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  {$IFDEF MSWINDOWS}
  DAV_WinAmp,
  {$ENDIF}
  PhaserDM in 'PhaserDM.pas' {PhaserModule: TPhaserModule},
  PhaserFrm in 'PhaserFrm.pas' {PhaserForm};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TPhaserModule);
end;

{$IFDEF MSWINDOWS}
function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
  Result := WinampDSPModuleHeader(TPhaserModule);
end;
{$ENDIF}

exports
{$IFDEF DARWIN}  {OS X entry points}
  VSTPluginMain name '_main',
  VSTPluginMain name '_main_macho',
  VSTPluginMain name '_VSTPluginMain';
{$ELSE}
  VSTPluginMain name 'main',
  VSTPluginMain name 'main_plugin',
  VSTPluginMain name 'VSTPluginMain',
{$IFDEF MSWINDOWS}
  WinampDSPGetHeader name 'winampDSPGetHeader2';
{$ENDIF}
{$ENDIF}

begin
 Application.Initialize;
end.
