{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaCombo;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  ComboDM in 'ComboDM.pas' {ComboDataModule: TVSTModule},
  ComboGUI in 'ComboGUI.pas' {FmCombo};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 try
  with TComboDataModule.Create(Application) do
   begin
    AudioMaster := AudioMasterCallback;
    Result := Effect;
   end;
 except
  Result := nil;
 end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.