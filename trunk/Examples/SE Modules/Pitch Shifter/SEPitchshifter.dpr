library SEPitchshifter;

uses
  FastMove,
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SEPitchshifterModule in 'SEPitchshifterModule.pas';

{$E sem}
{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 result := True;
 case Index of
  0: TSEStkPitchshifterModule.GetModuleProperties(Properties);
  else result := False;
 end;;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
begin
 result := nil;
 if (ProcessType = 1) then
  case Index of
   0: result := TSEStkPitchshifterModule.Create(SEAudioMaster, Reserved).Effect;
  end;
end;

exports makeModule name 'makeModule';
exports getModuleProperties name 'getModuleProperties';

end.
