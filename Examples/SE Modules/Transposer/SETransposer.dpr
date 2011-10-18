library SETransposer;

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SETransposerModule in 'SETransposerModule.pas';

{$E sem}
{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 Result := True;
 case Index of // !!TODO!! list your in / out plugs
  0: TSETransposerModule.GetModuleProperties(Properties);
  else Result := False; // host will ask for module 0,1,2,3 etc. return false to signal when done
 end;;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; p_resvd1: Pointer): Pointer; cdecl; export;
var
  SEModuleBase: TSEModuleBase;
begin
 Result := nil;
 case Index of // !!TODO!! list your in / out plugs
  0: begin
      if (ProcessType = 1) then// Audio Processing Object
       begin
        SEModuleBase := TSETransposerModule.Create(SEAudioMaster, p_resvd1);
        if Assigned(SEModuleBase)
         then Result := SEModuleBase.Effect;
       end;
     end;
 end;
end;

exports 
  makeModule name 'makeModule',
  getModuleProperties name 'getModuleProperties';

end.