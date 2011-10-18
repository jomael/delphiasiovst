library SEVoiceSynth;

{$I DAV_Compiler.inc}

uses
  {$IFDEF UseFastMove}
  FastMove, // either download the library or comment if there is an error here
  {$ENDIF}
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SEVoiceSynthModule in 'SEVoiceSynthModule.pas';

{$E sem}
{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 Result := True;
 case Index of 
  0: TSEVoiceSynthStaticModule.GetModuleProperties(Properties);
  1: TSEVoiceSynthControllableModule.GetModuleProperties(Properties);
  else Result := False; // host will ask for module 0,1,2,3 etc. return false to signal when done
 end;;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
begin
 Result := nil;
 if (ProcessType = 1) then
  case Index of
   0: Result := TSEVoiceSynthStaticModule.Create(SEAudioMaster, Reserved).Effect;
   1: Result := TSEVoiceSynthControllableModule.Create(SEAudioMaster, Reserved).Effect;
  end;
end;

exports makeModule name 'makeModule',
  getModuleProperties name 'getModuleProperties';

end.
