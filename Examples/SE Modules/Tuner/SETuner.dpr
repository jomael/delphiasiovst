library SETuner;

uses
  FastMove,
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SETunerModule in 'SETunerModule.pas',
  SEAdvancedTunerModule in 'SEAdvancedTunerModule.pas';

{$E sem}
{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 result := True;
 case Index of // !!TODO!! list your in / out plugs
  0: TSETunerStaticModule.GetModuleProperties(Properties);
  1: TSETunerControllableModule.GetModuleProperties(Properties);
  2: TSEAdvancedTunerStaticModule.GetModuleProperties(Properties);
  3: TSEAdvancedTunerControllableModule.GetModuleProperties(Properties);
  else result := False; // host will ask for module 0,1,2,3 etc. return false to signal when done
 end;;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
begin
 result := nil;
 if (ProcessType = 1) then
  case Index of
   0: result := TSETunerStaticModule.Create(SEAudioMaster, Reserved).Effect;
   1: result := TSETunerControllableModule.Create(SEAudioMaster, Reserved).Effect;
   2: result := TSEAdvancedTunerStaticModule.Create(SEAudioMaster, Reserved).Effect;
   3: result := TSEAdvancedTunerControllableModule.Create(SEAudioMaster, Reserved).Effect;
  end;
end;

exports makeModule name 'makeModule';
exports getModuleProperties name 'getModuleProperties';

end.
