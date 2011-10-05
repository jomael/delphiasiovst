program CalculateWorstCaseDifference;

{$I DAV_Compiler.inc}

uses
  FastMM4, // either download the library or comment if there is an error here
  {$IFDEF UseFastMove}
  FastMove, // either download the library or comment if there is an error here
  {$ENDIF}
  {$IFDEF UseMadExcept}
  madExcept, // either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  {$ENDIF}
  Vcl.Forms,
  MainUnit in 'MainUnit.pas' {FmCalculateWorstCaseDifference};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFmCalculateWorstCaseDifference, FmCalculateWorstCaseDifference);
  Application.Run;
end.

