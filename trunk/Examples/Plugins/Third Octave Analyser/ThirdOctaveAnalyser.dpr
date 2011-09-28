{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library ThirdOctaveAnalyser;

{$I DAV_Compiler.inc}

uses
  FastMM4,
  FastMove,
  {$IFDEF UseMadExcept}
  madExcept, // either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  {$ENDIF}
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  ThirdOctaveAnalyserDM in 'ThirdOctaveAnalyserDM.pas' {ThirdOctaveAnalyserModule: TVSTModule},
  ThirdOctaveAnalyserGUI in 'ThirdOctaveAnalyserGUI.pas' {FmThirdOctaveAnalyser};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  Result := VstModuleMain(AudioMasterCallback, TThirdOctaveAnalyserModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
  Result := WinampDSPModuleHeader(TThirdOctaveAnalyserModule);
end;

exports
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain',
  WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
end.
