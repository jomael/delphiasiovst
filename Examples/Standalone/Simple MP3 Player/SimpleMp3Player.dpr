program SimpleMp3Player;

uses
  FastMM4, // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  Forms,
  SmpMain in 'SmpMain.pas' {FmSimpleMp3Player},
  SmpSetup in 'SmpSetup.pas' {FmSetup};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Buffered ASIO MP3 Player';
  Application.CreateForm(TFmSimpleMp3Player, FmSimpleMp3Player);
  Application.CreateForm(TFmSetup, FmSetup);
  Application.Run;
end.
