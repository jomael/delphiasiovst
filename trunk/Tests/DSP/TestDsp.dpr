program TestDsp;
{

  Delphi DUnit-Testprojekt
  -------------------------
  Dieses Projekt enth�lt das DUnit-Test-Framework und die GUI/Konsolen-Test-Runner.
  Zum Verwenden des Konsolen-Test-Runners f�gen Sie den konditinalen Definitionen  
  in den Projektoptionen "CONSOLE_TESTRUNNER" hinzu. Ansonsten wird standardm��ig 
  der GUI-Test-Runner verwendet.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  DAV_DspAmbience in '..\..\Source\DSP\DAV_DspAmbience.pas',
  DAV_DspAudioToMidiTrigger in '..\..\Source\DSP\DAV_DspAudioToMidiTrigger.pas',
  DAV_DspBarberpole in '..\..\Source\DSP\DAV_DspBarberpole.pas',
  DAV_DspBarberpoleTuner in '..\..\Source\DSP\DAV_DspBarberpoleTuner.pas',
  DAV_DspBesselFilter in '..\..\Source\DSP\DAV_DspBesselFilter.pas',
  DAV_DspBuildingBlocks in '..\..\Source\DSP\DAV_DspBuildingBlocks.pas' {$R *.RES},
  DAV_DspChorus in '..\..\Source\DSP\DAV_DspChorus.pas',
  DAV_DspCircularBuffer in '..\..\Source\DSP\DAV_DspCircularBuffer.pas',
  DAV_DspConvolution in '..\..\Source\DSP\DAV_DspConvolution.pas',
  DAV_DspCrosstalkCancellation in '..\..\Source\DSP\DAV_DspCrosstalkCancellation.pas',
  DAV_DspCrosstalkSimulator in '..\..\Source\DSP\DAV_DspCrosstalkSimulator.pas',
  DAV_DspDelayLines in '..\..\Source\DSP\DAV_DspDelayLines.pas',
  DAV_DspDitherNoiseShaper in '..\..\Source\DSP\DAV_DspDitherNoiseShaper.pas',
  DAV_DspDynamics in '..\..\Source\DSP\DAV_DspDynamics.pas',
  DAV_DspExciter in '..\..\Source\DSP\DAV_DspExciter.pas',
  DAV_DspFilterBasics in '..\..\Source\DSP\DAV_DspFilterBasics.pas',
  DAV_DspFilterBasicsAutomatable in '..\..\Source\DSP\DAV_DspFilterBasicsAutomatable.pas',
  DAV_DspFilterButterworth in '..\..\Source\DSP\DAV_DspFilterButterworth.pas',
  DAV_DspFilterChebyshev in '..\..\Source\DSP\DAV_DspFilterChebyshev.pas',
  DAV_DspFilterLinearPhaseCrossover in '..\..\Source\DSP\DAV_DspFilterLinearPhaseCrossover.pas',
  DAV_DspFilterLinkwitzRiley in '..\..\Source\DSP\DAV_DspFilterLinkwitzRiley.pas',
  DAV_DspFilterSpectralDelay in '..\..\Source\DSP\DAV_DspFilterSpectralDelay.pas',
  DAV_DspLeslie in '..\..\Source\DSP\DAV_DspLeslie.pas',
  DAV_DspSoundTouch in '..\..\Source\DSP\DAV_DspSoundTouch.pas',
  DAV_DspWaveshaper in '..\..\Source\DSP\DAV_DspWaveshaper.pas',
  TestDAV_DspAmbience in 'TestDAV_DspAmbience.pas',
  TestDAV_DspAudioToMidiTrigger in 'TestDAV_DspAudioToMidiTrigger.pas',
  TestDAV_DspBarberpole in 'TestDAV_DspBarberpole.pas',
  TestDAV_DspBarberpoleTuner in 'TestDAV_DspBarberpoleTuner.pas',
  TestDAV_DspBesselFilter in 'TestDAV_DspBesselFilter.pas',
  TestDAV_DspBuildingBlocks in 'TestDAV_DspBuildingBlocks.pas',
  TestDAV_DspChorus in 'TestDAV_DspChorus.pas',
  TestDAV_DspCircularBuffer in 'TestDAV_DspCircularBuffer.pas',
  TestDAV_DspConvolution in 'TestDAV_DspConvolution.pas',
  TestDAV_DspCrosstalkCancellation in 'TestDAV_DspCrosstalkCancellation.pas',
  TestDAV_DspCrosstalkSimulator in 'TestDAV_DspCrosstalkSimulator.pas',
  TestDAV_DspDelayLines in 'TestDAV_DspDelayLines.pas',
  TestDAV_DspDitherNoiseShaper in 'TestDAV_DspDitherNoiseShaper.pas',
  TestDAV_DspDynamics in 'TestDAV_DspDynamics.pas',
  TestDAV_DspExciter in 'TestDAV_DspExciter.pas',
  TestDAV_DspFilterBasics in 'TestDAV_DspFilterBasics.pas',
  TestDAV_DspFilterBasicsAutomatable in 'TestDAV_DspFilterBasicsAutomatable.pas',
  TestDAV_DspFilterButterworth in 'TestDAV_DspFilterButterworth.pas',
  TestDAV_DspFilterChebyshev in 'TestDAV_DspFilterChebyshev.pas',
  TestDAV_DspFilterLinearPhaseCrossover in 'TestDAV_DspFilterLinearPhaseCrossover.pas',
  TestDAV_DspFilterLinkwitzRiley in 'TestDAV_DspFilterLinkwitzRiley.pas',
  TestDAV_DspFilterSpectralDelay in 'TestDAV_DspFilterSpectralDelay.pas',
  TestDAV_DspLeslie in 'TestDAV_DspLeslie.pas',
  TestDAV_DspSoundTouch in 'TestDAV_DspSoundTouch.pas',
  TestDAV_DspWaveshaper in 'TestDAV_DspWaveshaper.pas' {$R *.RES},
  TestDAV_BlockRoutines in 'TestDAV_BlockRoutines.pas';

{$R *.RES}

begin
  Application.Initialize;
  if IsConsole
   then TextTestRunner.RunRegisteredTests
   else GUITestRunner.RunRegisteredTests;
end.

