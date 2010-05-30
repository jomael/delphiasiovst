library dsp_vst;

{.$R 'EmbeddedVSTPlugin.res' 'EmbeddedVSTPlugin.rc'}

uses
  FastMM4,
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  FastMove,
  WinAmpDspVst in 'WinAmpDspVst.pas',
  WinAmpDspVstGui in 'WinAmpDspVstGui.pas' {FmWinAmpVST};

exports winampDSPGetHeader2 name 'winampDSPGetHeader2';
exports winampDSPGetHeader2;

end.
