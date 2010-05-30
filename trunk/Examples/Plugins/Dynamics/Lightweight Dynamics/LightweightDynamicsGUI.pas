unit LightweightDynamicsGUI;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2009             //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule,
  DAV_GuiLabel, Controls, DAV_GuiBaseControl, DAV_GuiDial, DAV_GuiGraphXY,
  DAV_GuiLED, StdCtrls, DAV_GuiGroup, DAV_GuiLevelMeter;

type
  TFmLightweightDynamics = class(TForm)
    DialCompressorAttack: TGuiDial;
    DialGateAttack: TGuiDial;
    DialGateRatio: TGuiDial;
    DialGateRelease: TGuiDial;
    DialGateKnee: TGuiDial;
    DialGateThreshold: TGuiDial;
    DialCompressorKnee: TGuiDial;
    DialCompressorMakeUpGain: TGuiDial;
    DialCompressorRatio: TGuiDial;
    DialCompressorRelease: TGuiDial;
    DialCompressorThreshold: TGuiDial;
    GbGate: TGuiGroup;
    GuiDialImageList: TGuiDialImageList;
    GbCompressor: TGuiGroup;
    LbInputGainLeft: TGuiLabel;
    LbInputGainRight: TGuiLabel;
    LbAutomaticMakeupGain: TGuiLabel;
    LbCompressorAttack: TGuiLabel;
    LbCompressorAttackValue: TGuiLabel;
    LbGateAttack: TGuiLabel;
    LbGateAttackValue: TGuiLabel;
    LbGateRatio: TGuiLabel;
    LbGateRatioValue: TGuiLabel;
    LbGateRelease: TGuiLabel;
    LbGateReleaseValue: TGuiLabel;
    LbGateKnee: TGuiLabel;
    LbGateKneeValue: TGuiLabel;
    LbGateThreshold: TGuiLabel;
    LbGateThresholdValue: TGuiLabel;
    LbCompressorKnee: TGuiLabel;
    LbCompressorKneeValue: TGuiLabel;
    LbCompressorMakeUpGain: TGuiLabel;
    LbCompressorMakeUpGainValue: TGuiLabel;
    LbCompressorRatio: TGuiLabel;
    LbCompressorRatioValue: TGuiLabel;
    LbCompressorRelease: TGuiLabel;
    LbCompressorReleaseValue: TGuiLabel;
    LbCompressorThreshold: TGuiLabel;
    LbCompressorThresholdValue: TGuiLabel;
    LEDAutoGain: TGuiLED;
    LmLeft: TGuiColorLevelMeter;
    LmRight: TGuiColorLevelMeter;
    GbCharacteristics: TGuiGroup;
    GuiGraphXY: TGuiGraphXY;
    GbLimiter: TGuiGroup;
    DialLimiterAttack: TGuiDial;
    LbLimiterAttack: TGuiLabel;
    LbLimiterAttackValue: TGuiLabel;
    DialLimiterRelease: TGuiDial;
    LbLimiterRelease: TGuiLabel;
    LbLimiterReleaseValue: TGuiLabel;
    DialLimiterThreshold: TGuiDial;
    LbLimiterThreshold: TGuiLabel;
    LbLimiterThresholdValue: TGuiLabel;
    DialLimiterKnee: TGuiDial;
    LbLimiterKnee: TGuiLabel;
    LbLimiterKneeValue: TGuiLabel;
    LbSoftClip: TGuiLabel;
    LEDSoftClip: TGuiLED;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DialGateAttackChange(Sender: TObject);
    procedure DialGateReleaseChange(Sender: TObject);
    procedure DialGateThresholdChange(Sender: TObject);
    procedure DialGateRatioChange(Sender: TObject);
    procedure DialGateKneeChange(Sender: TObject);
    procedure DialCompressorAttackChange(Sender: TObject);
    procedure DialCompressorMakeUpGainChange(Sender: TObject);
    procedure LEDAutoGainClick(Sender: TObject);
    procedure DialCompressorReleaseChange(Sender: TObject);
    procedure DialCompressorThresholdChange(Sender: TObject);
    procedure DialCompressorRatioChange(Sender: TObject);
    procedure DialCompressorKneeChange(Sender: TObject);
    procedure DialLimiterAttackChange(Sender: TObject);
    procedure DialLimiterReleaseChange(Sender: TObject);
    procedure DialLimiterThresholdChange(Sender: TObject);
    procedure DialLimiterKneeChange(Sender: TObject);
    procedure LbSoftClipClick(Sender: TObject);
  public
    // gate
    procedure UpdateGateAttack;
    procedure UpdateGateRelease;
    procedure UpdateGateThreshold;
    procedure UpdateGateRatio;
    procedure UpdateGateKnee;

    // compressor
    procedure UpdateCompressorAttack;
    procedure UpdateCompressorRelease;
    procedure UpdateCompressorThreshold;
    procedure UpdateCompressorRatio;
    procedure UpdateCompressorKnee;
    procedure UpdateCompressorMakeUp;
    procedure UpdateCompressorAutoMakeUpGain;

    // limiter
    procedure UpdateLimiterAttack;
    procedure UpdateLimiterRelease;
    procedure UpdateLimiterThreshold;
    procedure UpdateLimiterKnee;
    procedure UpdateLimiterSoftClip;

    function EvaluateCharacteristic(Sender: TObject; X: Double): Double;
  end;

implementation

uses
  LightweightDynamicsDM, PngImage, DAV_VSTModuleWithPrograms;

{$R *.DFM}

procedure TFmLightweightDynamics.FormCreate(Sender: TObject);
var
  RS     : TResourceStream;
  PngBmp : TPngObject;
begin
 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(hInstance, 'DynamicsKnob', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   with GuiDialImageList[0].DialBitmap do
    begin
     Canvas.FillRect(Canvas.ClipRect);
     Assign(PngBmp);
    end;
   DialGateAttack.DialImageIndex := 0;
   DialGateRelease.DialImageIndex := 0;
   DialGateThreshold.DialImageIndex := 0;
   DialGateKnee.DialImageIndex := 0;
   DialGateRatio.DialImageIndex := 0;
   DialCompressorAttack.DialImageIndex := 0;
   DialCompressorRelease.DialImageIndex := 0;
   DialCompressorThreshold.DialImageIndex := 0;
   DialCompressorKnee.DialImageIndex := 0;
   DialCompressorRatio.DialImageIndex := 0;
   DialCompressorMakeUpGain.DialImageIndex := 0;
   DialLimiterAttack.DialImageIndex := 0;
   DialLimiterRelease.DialImageIndex := 0;
   DialLimiterThreshold.DialImageIndex := 0;
   DialLimiterKnee.DialImageIndex := 0;
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;
 with TGuiGraphXYFunctionSeries(GuiGraphXY[0].Series) do
  begin
   OnEvaluate := EvaluateCharacteristic;
  end;
end;

procedure TFmLightweightDynamics.FormShow(Sender: TObject);
begin
 UpdateGateAttack;
 UpdateGateRelease;
 UpdateGateThreshold;
 UpdateGateRatio;
 UpdateGateKnee;
 UpdateCompressorAttack;
 UpdateCompressorRelease;
 UpdateCompressorThreshold;
 UpdateCompressorRatio;
 UpdateCompressorKnee;
 UpdateCompressorMakeUp;
 UpdateCompressorAutoMakeUpGain;
 UpdateLimiterAttack;
 UpdateLimiterRelease;
 UpdateLimiterThreshold;
 UpdateLimiterKnee;
 UpdateLimiterSoftClip;
end;

procedure TFmLightweightDynamics.LbSoftClipClick(Sender: TObject);
begin
 with TLightweightDynamicsDataModule(Owner) do
  begin
   Parameter[17] := Integer(LEDSoftClip.Brightness_Percent < 50);
  end;
end;

procedure TFmLightweightDynamics.LEDAutoGainClick(Sender: TObject);
begin
 with TLightweightDynamicsDataModule(Owner) do
  begin
   Parameter[11] := Integer(LEDAutoGain.Brightness_Percent < 50);
   if not DialCompressorMakeUpGain.Enabled then UpdateCompressorMakeUp;
  end;
end;

procedure TFmLightweightDynamics.DialGateAttackChange(Sender: TObject);
begin
 with TLightweightDynamicsDataModule(Owner) do
  begin
   Parameter[0] := DialGateAttack.Position;
  end;
end;

procedure TFmLightweightDynamics.DialGateReleaseChange(Sender: TObject);
begin
 with TLightweightDynamicsDataModule(Owner) do
  begin
   Parameter[1] := DialGateRelease.Position;
  end;
end;

procedure TFmLightweightDynamics.DialGateThresholdChange(Sender: TObject);
begin
 with TLightweightDynamicsDataModule(Owner) do
  begin
   Parameter[2] := DialGateThreshold.Position;
  end;
end;

function TFmLightweightDynamics.EvaluateCharacteristic(Sender: TObject;
  X: Double): Double;
begin
 result := TLightweightDynamicsDataModule(Owner).EvaluateCharacteristic(X);
end;

procedure TFmLightweightDynamics.DialGateRatioChange(Sender: TObject);
begin
 with TLightweightDynamicsDataModule(Owner) do
  begin
   Parameter[3] := DialGateRatio.Position;
  end;
end;

procedure TFmLightweightDynamics.DialGateKneeChange(Sender: TObject);
begin
 with TLightweightDynamicsDataModule(Owner) do
  begin
   Parameter[4] := DialGateKnee.Position;
  end;
end;

procedure TFmLightweightDynamics.DialCompressorAttackChange(Sender: TObject);
begin
 with TLightweightDynamicsDataModule(Owner) do
  begin
   Parameter[5] := DialCompressorAttack.Position;
  end;
end;

procedure TFmLightweightDynamics.DialCompressorReleaseChange(Sender: TObject);
begin
 with TLightweightDynamicsDataModule(Owner) do
  begin
   Parameter[6] := DialCompressorRelease.Position;
  end;
end;

procedure TFmLightweightDynamics.DialCompressorThresholdChange(Sender: TObject);
begin
 with TLightweightDynamicsDataModule(Owner) do
  begin
   Parameter[7] := DialCompressorThreshold.Position;
  end;
end;

procedure TFmLightweightDynamics.DialCompressorRatioChange(Sender: TObject);
begin
 with TLightweightDynamicsDataModule(Owner) do
  begin
   Parameter[8] := DialCompressorRatio.Position;
  end;
end;

procedure TFmLightweightDynamics.DialCompressorKneeChange(Sender: TObject);
begin
 with TLightweightDynamicsDataModule(Owner) do
  begin
   Parameter[9] := DialCompressorKnee.Position;
  end;
end;

procedure TFmLightweightDynamics.DialCompressorMakeUpGainChange(Sender: TObject);
begin
 with TLightweightDynamicsDataModule(Owner) do
  begin
   Parameter[10] := DialCompressorMakeUpGain.Position;
  end;
end;

procedure TFmLightweightDynamics.DialLimiterAttackChange(Sender: TObject);
begin
 with TLightweightDynamicsDataModule(Owner) do
  begin
   Parameter[13] := DialLimiterAttack.Position;
  end;
end;

procedure TFmLightweightDynamics.DialLimiterReleaseChange(Sender: TObject);
begin
 with TLightweightDynamicsDataModule(Owner) do
  begin
   Parameter[14] := DialLimiterRelease.Position;
  end;
end;

procedure TFmLightweightDynamics.DialLimiterThresholdChange(Sender: TObject);
begin
 with TLightweightDynamicsDataModule(Owner) do
  begin
   Parameter[15] := DialLimiterThreshold.Position;
  end;
end;

procedure TFmLightweightDynamics.DialLimiterKneeChange(Sender: TObject);
begin
 with TLightweightDynamicsDataModule(Owner) do
  begin
   Parameter[16] := DialLimiterKnee.Position;
  end;
end;


// public update procedures

procedure TFmLightweightDynamics.UpdateGateAttack;
var
  Attack : Single;
begin
 with TLightweightDynamicsDataModule(Owner) do
  begin
   Attack := Parameter[0];
   if Attack <> DialGateAttack.Position
    then DialGateAttack.Position := Attack;
   LbGateAttackValue.Caption := ParameterDisplay[0] + ' ' + ParameterLabel[0];
  end;
end;

procedure TFmLightweightDynamics.UpdateGateRelease;
var
  Release : Single;
begin
 with TLightweightDynamicsDataModule(Owner) do
  begin
   Release := Parameter[1];
   if Release <> DialGateRelease.Position
    then DialGateRelease.Position := Release;
   LbGateReleaseValue.Caption := ParameterDisplay[1] + ' ' + ParameterLabel[1];
  end;
end;

procedure TFmLightweightDynamics.UpdateGateThreshold;
var
  Threshold : Single;
begin
 with TLightweightDynamicsDataModule(Owner) do
  begin
   Threshold := Parameter[2];
   if Threshold <> DialGateThreshold.Position
    then DialGateThreshold.Position := Threshold;
   LbGateThresholdValue.Caption := ParameterDisplay[2] + ' ' + ParameterLabel[2];
   GuiGraphXY.UpdateGraph;
  end;
end;

procedure TFmLightweightDynamics.UpdateGateRatio;
var
  Ratio : Single;
begin
 with TLightweightDynamicsDataModule(Owner) do
  begin
   Ratio := Parameter[3];
   if Ratio <> DialGateRatio.Position
    then DialGateRatio.Position := Ratio;
   LbGateRatioValue.Caption := ParameterDisplay[3];
   GuiGraphXY.UpdateGraph;
  end;
end;

procedure TFmLightweightDynamics.UpdateGateKnee;
var
  Knee : Single;
begin
 with TLightweightDynamicsDataModule(Owner) do
  begin
   Knee := Parameter[4];
   if Knee <> DialGateKnee.Position
    then DialGateKnee.Position := Knee;
   LbGateKneeValue.Caption := ParameterDisplay[4] + ' ' + ParameterLabel[4];
   GuiGraphXY.UpdateGraph;
  end;
end;

procedure TFmLightweightDynamics.UpdateCompressorAttack;
var
  Attack : Single;
begin
 with TLightweightDynamicsDataModule(Owner) do
  begin
   Attack := Parameter[5];
   if Attack <> DialCompressorAttack.Position
    then DialCompressorAttack.Position := Attack;
   LbCompressorAttackValue.Caption := ParameterDisplay[5] + ' ' + ParameterLabel[5];
  end;
end;

procedure TFmLightweightDynamics.UpdateCompressorRelease;
var
  Release : Single;
begin
 with TLightweightDynamicsDataModule(Owner) do
  begin
   Release := Parameter[6];
   if Release <> DialCompressorRelease.Position
    then DialCompressorRelease.Position := Release;
   LbCompressorReleaseValue.Caption := ParameterDisplay[6] + ' ' + ParameterLabel[6];
  end;
end;

procedure TFmLightweightDynamics.UpdateCompressorThreshold;
var
  Threshold : Single;
begin
 with TLightweightDynamicsDataModule(Owner) do
  begin
   Threshold := Parameter[7];
   if Threshold <> DialCompressorThreshold.Position
    then DialCompressorThreshold.Position := Threshold;
   LbCompressorThresholdValue.Caption := ParameterDisplay[7] + ' ' + ParameterLabel[7];
   GuiGraphXY.UpdateGraph;
  end;
end;

procedure TFmLightweightDynamics.UpdateCompressorRatio;
var
  Ratio : Single;
begin
 with TLightweightDynamicsDataModule(Owner) do
  begin
   Ratio := Parameter[8];
   if Ratio <> DialCompressorRatio.Position
    then DialCompressorRatio.Position := Ratio;
   LbCompressorRatioValue.Caption := ParameterDisplay[8];
   GuiGraphXY.UpdateGraph;
  end;
end;

procedure TFmLightweightDynamics.UpdateCompressorKnee;
var
  Knee : Single;
begin
 with TLightweightDynamicsDataModule(Owner) do
  begin
   Knee := Parameter[9];
   if Knee <> DialCompressorKnee.Position
    then DialCompressorKnee.Position := Knee;
   LbCompressorKneeValue.Caption := ParameterDisplay[9] + ' ' + ParameterLabel[9];
   GuiGraphXY.UpdateGraph;
  end;
end;

procedure TFmLightweightDynamics.UpdateCompressorMakeUp;
var
  MakeUp : Single;
begin
 with TLightweightDynamicsDataModule(Owner) do
  begin
   MakeUp := Compressor.MakeUpGain_dB;
   if MakeUp <> DialCompressorMakeUpGain.Position
    then DialCompressorMakeUpGain.Position := MakeUp;
   LbCompressorMakeUpGainValue.Caption := ParameterDisplay[10] + ' ' + ParameterLabel[10];
   GuiGraphXY.UpdateGraph;
  end;
end;

procedure TFmLightweightDynamics.UpdateCompressorAutoMakeUpGain;
var
  Brightness : Single;
begin
 with TLightweightDynamicsDataModule(Owner) do
  begin
   Brightness := 100 * (0.1 + 0.9 * Parameter[11]);
   if Brightness <> LEDAutoGain.Brightness_Percent
    then LEDAutoGain.Brightness_Percent := Brightness;
   DialCompressorMakeUpGain.Enabled := Brightness < 50;
   GuiGraphXY.UpdateGraph;
  end;
end;

procedure TFmLightweightDynamics.UpdateLimiterAttack;
var
  Attack : Single;
begin
 with TLightweightDynamicsDataModule(Owner) do
  begin
   Attack := Parameter[13];
   if Attack <> DialLimiterAttack.Position
    then DialLimiterAttack.Position := Attack;
   LbLimiterAttackValue.Caption := ParameterDisplay[13] + ' ' + ParameterLabel[13];
  end;
end;

procedure TFmLightweightDynamics.UpdateLimiterRelease;
var
  Release : Single;
begin
 with TLightweightDynamicsDataModule(Owner) do
  begin
   Release := Parameter[14];
   if Release <> DialLimiterRelease.Position
    then DialLimiterRelease.Position := Release;
   LbLimiterReleaseValue.Caption := ParameterDisplay[14] + ' ' + ParameterLabel[14];
  end;
end;

procedure TFmLightweightDynamics.UpdateLimiterThreshold;
var
  Threshold : Single;
begin
 with TLightweightDynamicsDataModule(Owner) do
  begin
   Threshold := Parameter[15];
   if Threshold <> DialLimiterThreshold.Position
    then DialLimiterThreshold.Position := Threshold;
   LbLimiterThresholdValue.Caption := ParameterDisplay[15] + ' ' + ParameterLabel[15];
   GuiGraphXY.UpdateGraph;
  end;
end;

procedure TFmLightweightDynamics.UpdateLimiterKnee;
var
  Knee : Single;
begin
 with TLightweightDynamicsDataModule(Owner) do
  begin
   Knee := Parameter[16];
   if Knee <> DialLimiterKnee.Position
    then DialLimiterKnee.Position := Knee;
   LbLimiterKneeValue.Caption := ParameterDisplay[16] + ' ' + ParameterLabel[16];
   GuiGraphXY.UpdateGraph;
  end;
end;

procedure TFmLightweightDynamics.UpdateLimiterSoftClip;
var
  Brightness : Single;
begin
 with TLightweightDynamicsDataModule(Owner) do
  begin
   Brightness := 100 * (0.1 + 0.9 * Parameter[17]);
   if Brightness <> LEDSoftClip.Brightness_Percent
    then LEDSoftClip.Brightness_Percent := Brightness;
  end;
end;

end.
