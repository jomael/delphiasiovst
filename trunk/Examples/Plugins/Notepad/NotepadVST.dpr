{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library NotepadVST;

uses
  FastMM4, // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  madExcept, // either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  PluginDM in 'PluginDM.pas' {PluginDataModule: TVSTModule},
  Editor in 'Editor.pas' {FmNotepad};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TPluginDataModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
 Result := WinampDSPModuleHeader(TPluginDataModule);
end;

exports VstPluginMain name 'main';
exports VstPluginMain name 'VSTPluginMain';
exports WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
end.