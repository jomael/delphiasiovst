{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library ButterworthSplitterStereo;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  ButterworthSplitterDM in 'ButterworthSplitterDM.pas' {ButterworthSplitterModule: TVSTModule};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  try
    with TButterworthSplitterModule.Create(Application) do
     begin
      AudioMaster := AudioMasterCallback;
      Result := Effect;
      numInputs := 2;
      numOutputs := 2 * numInputs;
      CanDos := [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in4out];
     end;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.