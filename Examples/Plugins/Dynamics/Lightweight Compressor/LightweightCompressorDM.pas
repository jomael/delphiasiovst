unit LightweightCompressorDM;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2009-2011        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes,
  Forms, DAV_Types, DAV_VSTModule, DAV_DspDynamics, DAV_DspLightweightDynamics;

type
  TLightweightCompressorDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcessMono(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessStereo(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessMonoSoftClip(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessStereoSoftClip(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterKneeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterMakeUpGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterThresholdDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterRatioDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterKneeDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterOnOffDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterStereoChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterLimitChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterAutoMakeUpGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterMakeUpGainDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterTimeDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterTimeLabel(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterMixChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FCompressor : array [0..1] of TCustomKneeCompressor;
    function GetLightweightCompressor(Index: Integer): TCustomKneeCompressor;
    procedure ChooseProcess;
  public
    function EvaluateCharacteristic(const Input: Single): Single;
    property LightweightCompressor[Index: Integer]: TCustomKneeCompressor read GetLightweightCompressor;
  end;

implementation

{$IFDEF FPC}
{$R *.LFM}
{$ELSE}
{$R *.DFM}
{$ENDIF}

uses
  Math, DAV_Common, DAV_Approximations, DAV_VSTModuleWithPrograms,
  LightweightCompressorGUI;

procedure TLightweightCompressorDataModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
const
  CPresets : array [1..10, 0..9] of Single = (
    (50, 500, -10, 3, 4, 3, 0, 1, 0, 100),
    (20, 100, -12, 4, 2.5, 6, 0, 0, 0, 100),
    (20,  80, -15, 8, 2, 8, 0, 1, 0, 100),
    (5, 60, -20, 7, 3, 13, 1, 0, 0, 100),
    (1, 50, -30, 6, 2, 18, 0, 0, 0, 100),
    (8, 64, -30, 12, 5, 17, 0, 0, 0, 100),
    (16, 78, -24, 15, 1.8, 19, 0, 1, 0, 100),
    (1, 20, -14, 5, 3, 8, 0, 1, 0, 100),
    (3, 44, -17, 7, 1, 9, 1, 0, 0, 100),
    (8, 56, -11, 9, 4, 5, 1, 1, 0, 100));
begin
 for Channel := 0 to Length(FCompressor) - 1 do
  begin
   FCompressor[Channel] := TLightweightSoftKneeCompressor.Create;
   FCompressor[Channel].SampleRate := SampleRate;
  end;

 Parameter[0] := 15;
 Parameter[1] := 75;
 Parameter[2] := -10;
 Parameter[3] := 5;
 Parameter[4] := 2;
 Parameter[5] := 6;
 Parameter[6] := 0;
 Parameter[7] := 0;
 Parameter[8] := 0;
 Parameter[9] := 100;

 Programs[0].SetParameters(FParameter);
 for Channel := 1 to numPrograms - 1
  do Programs[Channel].SetParameters(CPresets[Channel]);
end;

procedure TLightweightCompressorDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FCompressor[0]);
 FreeAndNil(FCompressor[1]);
end;

procedure TLightweightCompressorDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmLightweightCompressor.Create(Self);
end;

function TLightweightCompressorDataModule.EvaluateCharacteristic(
  const Input: Single): Single;
begin
 Result := FCompressor[0].CharacteristicCurve_dB(Input);
end;

procedure TLightweightCompressorDataModule.ChooseProcess;
begin
 case round(Parameter[7]) of
  0 : case round(Parameter[6]) of
       0 : OnProcess := VSTModuleProcessMono;
       1 : OnProcess := VSTModuleProcessStereo;
      end;
  1 : case round(Parameter[6]) of
       0 : OnProcess := VSTModuleProcessMonoSoftClip;
       1 : OnProcess := VSTModuleProcessStereoSoftClip;
      end;
 end;
 OnProcess32Replacing := OnProcess;
end;

function TLightweightCompressorDataModule.GetLightweightCompressor(Index: Integer): TCustomKneeCompressor;
begin
 if Index in [0..Length(FCompressor) - 1]
  then Result := FCompressor[Index]
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

procedure TLightweightCompressorDataModule.ParameterAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCompressor[0]) then
  begin
   FCompressor[0].Attack := Value;
   if Assigned(FCompressor[1])
    then FCompressor[1].Attack := FCompressor[0].Attack;
  end;

 // update GUI
 if EditorForm is TFmLightweightCompressor
  then TFmLightweightCompressor(EditorForm).UpdateAttack;
end;

procedure TLightweightCompressorDataModule.ParameterReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCompressor[0]) then
  begin
   FCompressor[0].Release := Value;
   if Assigned(FCompressor[1])
    then FCompressor[1].Release := FCompressor[0].Release;
  end;

 // update GUI
 if EditorForm is TFmLightweightCompressor
  then TFmLightweightCompressor(EditorForm).UpdateRelease;
end;

procedure TLightweightCompressorDataModule.ParameterThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCompressor[0]) then
  begin
   FCompressor[0].Threshold_dB := Value;
   if Assigned(FCompressor[1])
    then FCompressor[1].Threshold_dB := Value;
  end;

 // update GUI
 if EditorForm is TFmLightweightCompressor
  then TFmLightweightCompressor(EditorForm).UpdateThreshold;
end;

procedure TLightweightCompressorDataModule.ParameterRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCompressor[0]) then
  begin
   FCompressor[0].Ratio := Value;
   if Assigned(FCompressor[1])
    then FCompressor[1].Ratio := Value;
  end;

 // update GUI
 if EditorForm is TFmLightweightCompressor
  then TFmLightweightCompressor(EditorForm).UpdateRatio;
end;

procedure TLightweightCompressorDataModule.ParameterKneeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCompressor[0]) then
  begin
   FCompressor[0].Knee_dB := Value;
   if Assigned(FCompressor[1])
    then FCompressor[1].Knee_dB := Value;
  end;

 // update GUI
 if EditorForm is TFmLightweightCompressor
  then TFmLightweightCompressor(EditorForm).UpdateKnee;
end;

procedure TLightweightCompressorDataModule.ParameterAutoMakeUpGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCompressor[0]) then
  begin
   FCompressor[0].AutoMakeUp := Boolean(round(Value));
   FCompressor[1].AutoMakeUp := FCompressor[0].AutoMakeUp;
  end;

 // update GUI
 if EditorForm is TFmLightweightCompressor
  then TFmLightweightCompressor(EditorForm).UpdateAutoMakeUpGain;
end;

procedure TLightweightCompressorDataModule.ParameterMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 Value := 100;
end;

procedure TLightweightCompressorDataModule.ParameterTimeLabel(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
var
  Val : Single;
begin
 Val := Parameter[Index];
 if Val < 1
  then PreDefined := '�s' else
 if Val >= 1000
  then PreDefined := 's';
end;

procedure TLightweightCompressorDataModule.ParameterTimeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
var
  Val : Single;
begin
 Val := Parameter[Index];
 if Val < 1
  then PreDefined := FloatToStrF(RoundTo(1E3 * Val, -2), ffGeneral, 3, 3) else
 if Val < 1000
  then PreDefined := FloatToStrF(RoundTo(Val, -2), ffGeneral, 3, 3)
  else PreDefined := FloatToStrF(RoundTo(1E-3 * Val, -2), ffGeneral, 3, 3);
end;

procedure TLightweightCompressorDataModule.ParameterMakeUpGainDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TLightweightCompressorDataModule.ParameterMakeUpGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCompressor[0]) then
  begin
   FCompressor[0].MakeUpGain_dB := Value;
   FCompressor[1].MakeUpGain_dB := FCompressor[0].MakeUpGain_dB;
  end;

 // update GUI
 if EditorForm is TFmLightweightCompressor
  then TFmLightweightCompressor(EditorForm).UpdateMakeUp;
end;

procedure TLightweightCompressorDataModule.ParameterThresholdDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TLightweightCompressorDataModule.ParameterRatioDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TLightweightCompressorDataModule.ParameterKneeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TLightweightCompressorDataModule.ParameterOnOffDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case round(Parameter[Index]) of
  0 : PreDefined := 'Off';
  1 : PreDefined := 'On';
 end;
end;

procedure TLightweightCompressorDataModule.ParameterStereoChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 ChooseProcess;

 // update GUI
 if EditorForm is TFmLightweightCompressor
  then TFmLightweightCompressor(EditorForm).UpdateStereo;
end;

procedure TLightweightCompressorDataModule.ParameterLimitChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 ChooseProcess;

 // update GUI
 if EditorForm is TFmLightweightCompressor
  then TFmLightweightCompressor(EditorForm).UpdateLimit;
end;

procedure TLightweightCompressorDataModule.VSTModuleProcessStereo(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := FCompressor[0].ProcessSample64(Inputs[0, Sample]);
   Outputs[1, Sample] := FCompressor[1].ProcessSample64(Inputs[1, Sample]);
  end;
end;

procedure TLightweightCompressorDataModule.VSTModuleProcessMono(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
  Temp   : Single;
begin
 for Sample := 0 to SampleFrames - 1 do
  with FCompressor[0] do
   begin
    InputSample(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample]));
    Temp := MakeUpGain * GainReductionFactor;
    Outputs[0, Sample] := Temp * Inputs[0, Sample];
    Outputs[1, Sample] := Temp * Inputs[1, Sample];
   end;
end;

procedure TLightweightCompressorDataModule.VSTModuleProcessStereoSoftClip(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := FastTanhContinousError4(FCompressor[0].ProcessSample64(Inputs[0, Sample]));
   Outputs[1, Sample] := FastTanhContinousError4(FCompressor[1].ProcessSample64(Inputs[1, Sample]));
  end;
end;

procedure TLightweightCompressorDataModule.VSTModuleProcessMonoSoftClip(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
  Temp   : Single;
begin
 for Sample := 0 to SampleFrames - 1 do
  with FCompressor[0] do
   begin
    InputSample(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample]));
    Temp := MakeUpGain * GainReductionFactor;
    Outputs[0, Sample] := FastTanhContinousError4(Temp * Inputs[0, Sample]);
    Outputs[1, Sample] := FastTanhContinousError4(Temp * Inputs[1, Sample]);
   end;
end;

procedure TLightweightCompressorDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if Abs(SampleRate) <> 0 then
  begin
   if Assigned(FCompressor[0]) then FCompressor[0].SampleRate := SampleRate;
   if Assigned(FCompressor[1]) then FCompressor[1].SampleRate := SampleRate;
  end;
end;

end.
