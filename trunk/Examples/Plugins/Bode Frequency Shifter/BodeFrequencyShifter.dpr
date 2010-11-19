{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library BodeFrequencyShifter;

uses
  FastMM4, // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  madExcept, // either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  BodeFrequencyShifterDM in 'BodeFrequencyShifterDM.pas' {BodeFrequencyShifterDataModule: TVSTModule},
  BodeFrequencyShifterGUI in 'BodeFrequencyShifterGUI.pas' {FmBodeFrequencyShifter};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  Result := VstModuleMain(AudioMasterCallback, TBodeFrequencyShifterDataModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
  Result := WinampDSPModuleHeader(TBodeFrequencyShifterDataModule);
end;

exports
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain',
  WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
end.