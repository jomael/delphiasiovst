program MiniHost;

{.$R 'EmbeddedPlugin.res' 'EmbeddedPlugin.rc'}

uses
  FastMM4, // either download the library or comment if there is an error here
{$IFDEF UseFastMove}
  FastMove, // either download the library or comment if there is an error here
{$ENDIF}
  Forms,
  MiniHostForm in 'MiniHostForm.pas' {FmMiniHost},
  OptionsForm in 'OptionsForm.pas' {FmOptions},
  PlayerForm in 'PlayerForm.pas' {Player},
  AboutForm in 'AboutForm.pas' {FmAbout};

{$R *.RES}
begin
 Application.Initialize;
 Application.Title := 'Tobybear MiniHost';
 Application.CreateForm(TFmMiniHost, FmMiniHost);
 Application.Run;
end.
