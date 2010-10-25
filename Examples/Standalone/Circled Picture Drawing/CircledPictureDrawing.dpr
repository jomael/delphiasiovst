program CircledPictureDrawing;

uses
  FastMM4,
  FastMove,
  madExcept,
  madLinkDisAsm,
  madListModules,
  Forms,
  MainUnit in 'MainUnit.pas' {FmCircledPictureDialog},
  SettingsUnit in 'SettingsUnit.pas' {FmSettings},
  ProgressBarUnit in 'ProgressBarUnit.pas' {FmProgressBar},
  AdditionalChunks in 'AdditionalChunks.pas';

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Circled Picture Optimizer';
  Application.CreateForm(TFmCircledPictureDialog, FmCircledPictureDialog);
  Application.Run;
end.

