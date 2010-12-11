unit PhaseAdjustmentDSP;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Version: MPL 1.1 or LGPL 2.1 with linking exception                       //
//                                                                            //
//  The contents of this file are subject to the Mozilla Public License       //
//  Version 1.1 (the "License"); you may not use this file except in          //
//  compliance with the License. You may obtain a copy of the License at      //
//  http://www.mozilla.org/MPL/                                               //
//                                                                            //
//  Software distributed under the License is distributed on an "AS IS"       //
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the   //
//  License for the specific language governing rights and limitations under  //
//  the License.                                                              //
//                                                                            //
//  Alternatively, the contents of this file may be used under the terms of   //
//  the Free Pascal modified version of the GNU Lesser General Public         //
//  License Version 2.1 (the "FPC modified LGPL License"), in which case the  //
//  provisions of this license are applicable instead of those above.         //
//  Please see the file LICENSE.txt for additional information concerning     //
//  this license.                                                             //
//                                                                            //
//  The code is part of the Delphi ASIO & VST Project                         //
//                                                                            //
//  The initial developer of this code is Christian-W. Budde                  //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2010        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.INC}

uses
  {$IFDEF FPC}LCLIntf, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, Forms,
  SyncObjs, DAV_Types, DAV_Complex, DAV_MemoryUtils,
  DAV_DspFftReal2Complex, {$IFDEF Use_IPPS}DAV_DspFftReal2ComplexIPPS, {$ENDIF}
  {$IFDEF Use_CUDA}DAV_DspFftReal2ComplexCUDA, {$ENDIF}
  DAV_DspConvolution, DAV_VSTModule;

type
  TPhaseAdjustmentModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure ParameterPhaseChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FCriticalSection   : TCriticalSection;
    FIRSize            : Integer;
    FCurrentPhase      : Single;
    FDesiredPhase      : Single;
    FImpulseResponse   : PDAVSingleFixedArray;
    FFilterFreq        : PDAVComplexSingleFixedArray;
    {$IFDEF Use_IPPS}
    FFft               : TFftReal2ComplexIPPSFloat32;
    {$ELSE} {$IFDEF Use_CUDA}
    FFft               : TFftReal2ComplexCUDA32;
    {$ELSE}
    FFft               : TFftReal2ComplexNativeFloat32;
    {$ENDIF}{$ENDIF}

    FStereoConvolution : TLowLatencyConvolutionStereo32;

    procedure CalculateFilterKernel;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, DAV_Common, DAV_Math, DAV_DspWindowing, PhaseAdjustmentGUI;

procedure TPhaseAdjustmentModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
 FImpulseResponse := nil;
 FFilterFreq      := nil;
 FIRSize          := 1024;
end;

procedure TPhaseAdjustmentModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TPhaseAdjustmentModule.VSTModuleOpen(Sender: TObject);
begin
 // create stereo convolution processor
 FStereoConvolution := TLowLatencyConvolutionStereo32.Create;

 // set initial delay
 InitialDelay := FStereoConvolution.Latency + FIRSize div 2;

 {$IFDEF Use_IPPS}
 FFft := TFftReal2ComplexIPPSFloat32.Create(Round(Log2(2 * FIRSize)));

 ReallocateAlignedMemory(Pointer(FFilterFreq), (FIRSize + 1) * SizeOf(TComplexSingle));
 FillChar(FFilterFreq^[0], (FIRSize + 1) * SizeOf(TComplexSingle), 0);
 {$ELSE} {$IFDEF Use_CUDA}
 FFft := TFftReal2ComplexCUDA32.Create(Round(Log2(FIRSize)));

 ReallocateAlignedMemory(FFilterFreq, FIRSize * SizeOf(Single));
 FillChar(FFilterFreq^[0], FIRSize * SizeOf(Single), 0);
 {$ELSE}
 FFft := TFftReal2ComplexNativeFloat32.Create(Round(Log2(FIRSize)));

 ReallocateAlignedMemory(FFilterFreq, FIRSize * SizeOf(Single));
 FillChar(FFilterFreq^[0], FIRSize * SizeOf(Single), 0);
 {$ENDIF}{$ENDIF}

 // allocate memory for impulse response
 ReallocateAlignedMemory(FImpulseResponse, 2 * FIRSize * SizeOf(Single));
 FillChar(FImpulseResponse^[0], 2 * FIRSize * SizeOf(Single), 0);

 FFft.AutoScaleType := astDivideInvByN;
 FFft.DataOrder := doPackedComplex;

 // set editor class
 EditorFormClass := TFmPhaseAdjustment;

 // Parameters and Programs
 Parameter[0] := 45;
 Programs[1].CopyParameters(0);
 Programs[2].CopyParameters(0);
 Programs[3].CopyParameters(0);
 Programs[4].CopyParameters(0);
 Programs[5].CopyParameters(0);

 CalculateFilterKernel;
end;

procedure TPhaseAdjustmentModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FStereoConvolution);

 FreeAlignedMemory(FImpulseResponse);
 FreeAlignedMemory(FFilterFreq);
 FreeAndNil(FFft);
end;

procedure TPhaseAdjustmentModule.ParameterPhaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FDesiredPhase := Value;

 // update GUI
 if EditorForm is TFmPhaseAdjustment then
  with TFmPhaseAdjustment(EditorForm)
   do UpdatePhaseDial;
end;

procedure TPhaseAdjustmentModule.CalculateFilterKernel;
var
  Index : Integer;
const
  CDegToPi : Single = 180 / Pi;
begin
 if Assigned(FImpulseResponse) and Assigned(FFilterFreq) and Assigned(FFft) then
  begin
   FFilterFreq[0] := ComplexPolar32(1, 0);
   for Index := 1 to FIRSize - 1
    do FFilterFreq[Index] := ComplexPolar32(1, FDesiredPhase * CDegToPi);
   FFilterFreq[FIRSize] := ComplexPolar32(1, 0);

   {$IFDEF Use_IPPS}
   FFft.PerformiFFTCCS(FFilterFreq, FImpulseResponse);
   {$ELSE}{$IFDEF Use_CUDA}
   FFft.PerformiFFT(FFilterFreq, FImpulseResponse);
   {$ELSE}
   // calculate frequency
   case FFft.DataOrder of
    doPackedRealImaginary : FFft.PerformiFFTPackedReIm(PDAVSingleFixedArray(FFilterFreq), FImpulseResponse);
          doPackedComplex : FFft.PerformiFFTPackedComplex(FFilterFreq, FImpulseResponse);
    else raise Exception.Create('not supported');
   end;
   {$ENDIF}{$ENDIF}

//   FillChar(FImpulseResponse^[FIRSize], FIRSize * SizeOf(Single), 0);
//   ApplyBlackmanWindow(FImpulseResponse, FIRSize);

   FStereoConvolution.LoadImpulseResponse(FImpulseResponse, FIRSize);
   FDesiredPhase := FCurrentPhase;
  end;
end;

procedure TPhaseAdjustmentModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
begin
 FCriticalSection.Enter;
 try
  Move(Inputs[0, 0], Outputs[0, 0], SampleFrames * SizeOf(Single));
  Move(Inputs[1, 0], Outputs[1, 0], SampleFrames * SizeOf(Single));
  if FDesiredPhase <> FCurrentPhase then
   begin
(*
    FStereoConvolution.ProcessBlock(PDAVSingleFixedArray(@Outputs[0, 0]),
      PDAVSingleFixedArray(@Outputs[1, 0]), SampleFrames);
    *)

    CalculateFilterKernel;
    // FStereoConvolution.Reset
   end;

  FStereoConvolution.ProcessBlock(PDAVSingleFixedArray(@Outputs[0, 0]),
    PDAVSingleFixedArray(@Outputs[1, 0]), SampleFrames);
 finally
  FCriticalSection.Leave;
 end;
end;

end.
