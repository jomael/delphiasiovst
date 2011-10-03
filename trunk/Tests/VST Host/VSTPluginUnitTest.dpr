program VSTPluginUnitTest;

{$I DAV_Compiler.inc}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  FastMM4,
  {$IFDEF UseFastMove}
  FastMove,
  {$ENDIF}
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
{$IFNDEF CONSOLE_TESTRUNNER}
  SplashScreen in 'SplashScreen.pas' {FmSplashScreen},
{$ENDIF}
  DAV_TestVSTHost in 'DAV_TestVSTHost.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'VST Plugin Unit Test';
  if IsConsole
   then TextTestRunner.RunRegisteredTests
   else GUITestRunner.RunRegisteredTests;
end.

