unit LightweightFeedbackCompressorGUI;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2009-2010        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule,
  DAV_GuiLabel, Controls, DAV_GuiBaseControl, DAV_GuiDial, DAV_GuiGraphXY,
  DAV_GuiLED;

type
  TFmLightweightFeedbackCompressor = class(TForm)
    DialAttack: TGuiDial;
    DialKnee: TGuiDial;
    DialMakeUpGain: TGuiDial;
    DialRatio: TGuiDial;
    DialRelease: TGuiDial;
    DialThreshold: TGuiDial;
    GuiDialImageList: TGuiDialImageList;
    GuiGraphXY: TGuiGraphXY;
    GuiLabel2: TGuiLabel;
    GuiLabel3: TGuiLabel;
    LbAttack: TGuiLabel;
    LbAttackValue: TGuiLabel;
    LbKnee: TGuiLabel;
    LbKneeValue: TGuiLabel;
    LbMakeUpGain: TGuiLabel;
    LbMakeUpGainValue: TGuiLabel;
    LbOversample: TGuiLabel;
    LbRatio: TGuiLabel;
    LbRatioValue: TGuiLabel;
    LbRelease: TGuiLabel;
    LbReleaseValue: TGuiLabel;
    LbStereo: TGuiLabel;
    LbThreshold: TGuiLabel;
    LbThresholdValue: TGuiLabel;
    LEDAutoGain: TGuiLED;
    LEDLimit: TGuiLED;
    LEDOversample: TGuiLED;
    LEDStereo: TGuiLED;
    procedure FormCreate(Sender: TObject);
    procedure DialAttackChange(Sender: TObject);
    procedure DialReleaseChange(Sender: TObject);
    procedure DialThresholdChange(Sender: TObject);
    procedure DialRatioChange(Sender: TObject);
    procedure DialKneeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DialMakeUpGainChange(Sender: TObject);
    procedure LEDStereoClick(Sender: TObject);
    procedure LEDLimitClick(Sender: TObject);
    procedure LEDAutoGainClick(Sender: TObject);
  public
    procedure UpdateAttack;
    procedure UpdateRelease;
    procedure UpdateThreshold;
    procedure UpdateRatio;
    procedure UpdateKnee;
    procedure UpdateMakeUp;
    procedure UpdateStereo;
    procedure UpdateLimit;
    procedure UpdateAutoMakeUpGain;
    function EvaluateCharacteristic(Sender: TObject; X: Double): Double;
  end;

implementation

uses
  Math, PngImage, DAV_VSTModuleWithPrograms, LightweightFeedbackCompressorDM;

{$R *.DFM}

procedure TFmLightweightFeedbackCompressor.FormCreate(Sender: TObject);
var
  RS     : TResourceStream;
  PngBmp : TPngObject;
begin
 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(hInstance, 'FeedbackCompressorKnob', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   with GuiDialImageList[0].DialBitmap do
    begin
     Canvas.FillRect(Canvas.ClipRect);
     Assign(PngBmp);
    end;
   DialThreshold.DialImageIndex := 0;
   DialKnee.DialImageIndex := 0;
   DialRatio.DialImageIndex := 0;
   DialAttack.DialImageIndex := 0;
   DialRelease.DialImageIndex := 0;
   DialMakeUpGain.DialImageIndex := 0;
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

procedure TFmLightweightFeedbackCompressor.FormShow(Sender: TObject);
begin
 with TLightweightFeedbackCompressorDataModule(Owner) do
  begin
   DialAttack.Min := max(0.01, 2000 / SampleRate);
   DialRelease.Min := max(0.1, 2000 / SampleRate);
  end;
 UpdateAttack;
 UpdateRelease;
 UpdateThreshold;
 UpdateRatio;
 UpdateKnee;
 UpdateMakeUp;
 UpdateStereo;
 UpdateLimit;
 UpdateAutoMakeUpGain;
end;

procedure TFmLightweightFeedbackCompressor.LEDAutoGainClick(Sender: TObject);
begin
 with TLightweightFeedbackCompressorDataModule(Owner) do
  begin
   Parameter[8] := Integer(LEDAutoGain.Brightness_Percent < 50);
   if not DialMakeUpGain.Enabled then UpdateMakeUp;
  end;
end;

procedure TFmLightweightFeedbackCompressor.LEDLimitClick(Sender: TObject);
begin
 with TLightweightFeedbackCompressorDataModule(Owner) do
  begin
   Parameter[7] := Integer(LEDLimit.Brightness_Percent < 50);
  end;
end;

procedure TFmLightweightFeedbackCompressor.LEDStereoClick(Sender: TObject);
begin
 with TLightweightFeedbackCompressorDataModule(Owner) do
  begin
   Parameter[6] := Integer(LEDStereo.Brightness_Percent < 50);
  end;
end;

procedure TFmLightweightFeedbackCompressor.DialAttackChange(Sender: TObject);
begin
 with TLightweightFeedbackCompressorDataModule(Owner) do
  begin
   Parameter[0] := DialAttack.Position;
  end;
end;

procedure TFmLightweightFeedbackCompressor.DialReleaseChange(Sender: TObject);
begin
 with TLightweightFeedbackCompressorDataModule(Owner) do
  begin
   Parameter[1] := DialRelease.Position;
  end;
end;

procedure TFmLightweightFeedbackCompressor.DialThresholdChange(Sender: TObject);
begin
 with TLightweightFeedbackCompressorDataModule(Owner) do
  begin
   Parameter[2] := DialThreshold.Position;
  end;
end;

function TFmLightweightFeedbackCompressor.EvaluateCharacteristic(Sender: TObject;
  X: Double): Double;
begin
 result := TLightweightFeedbackCompressorDataModule(Owner).EvaluateCharacteristic(X);
end;

procedure TFmLightweightFeedbackCompressor.DialRatioChange(Sender: TObject);
begin
 with TLightweightFeedbackCompressorDataModule(Owner) do
  begin
   Parameter[3] := DialRatio.Position;
  end;
end;

procedure TFmLightweightFeedbackCompressor.DialKneeChange(Sender: TObject);
begin
 with TLightweightFeedbackCompressorDataModule(Owner) do
  begin
   Parameter[4] := DialKnee.Position;
  end;
end;

procedure TFmLightweightFeedbackCompressor.DialMakeUpGainChange(Sender: TObject);
begin
 with TLightweightFeedbackCompressorDataModule(Owner) do
  begin
   Parameter[5] := DialMakeUpGain.Position;
  end;
end;

procedure TFmLightweightFeedbackCompressor.UpdateAttack;
var
  Attack : Single;
begin
 with TLightweightFeedbackCompressorDataModule(Owner) do
  begin
   Attack := Parameter[0];
   if Attack <> DialAttack.Position
    then DialAttack.Position := Attack;
   LbAttackValue.Caption := ParameterDisplay[0] + ' ' + ParameterLabel[0];
  end;
end;

procedure TFmLightweightFeedbackCompressor.UpdateRelease;
var
  Release : Single;
begin
 with TLightweightFeedbackCompressorDataModule(Owner) do
  begin
   Release := Parameter[1];
   if Release <> DialRelease.Position
    then DialRelease.Position := Release;
   LbReleaseValue.Caption := ParameterDisplay[1] + ' ' + ParameterLabel[1];
  end;
end;

procedure TFmLightweightFeedbackCompressor.UpdateKnee;
var
  Knee : Single;
begin
 with TLightweightFeedbackCompressorDataModule(Owner) do
  begin
   Knee := Parameter[4];
   if Knee <> DialKnee.Position
    then DialKnee.Position := Knee;
   LbKneeValue.Caption := ParameterDisplay[4] + ' ' + ParameterLabel[4];
   GuiGraphXY.UpdateGraph;
  end;
end;

procedure TFmLightweightFeedbackCompressor.UpdateMakeUp;
var
  MakeUp : Single;
begin
 with TLightweightFeedbackCompressorDataModule(Owner) do
  begin
   MakeUp := LightweightFeedbackCompressor[0].MakeUpGain_dB;
   if MakeUp <> DialMakeUpGain.Position
    then DialMakeUpGain.Position := MakeUp;
   LbMakeUpGainValue.Caption := ParameterDisplay[5] + ' ' + ParameterLabel[5];
   GuiGraphXY.UpdateGraph;
  end;
end;

procedure TFmLightweightFeedbackCompressor.UpdateRatio;
var
  Ratio : Single;
begin
 with TLightweightFeedbackCompressorDataModule(Owner) do
  begin
   Ratio := Parameter[3];
   if Ratio <> DialRatio.Position
    then DialRatio.Position := Ratio;
   LbRatioValue.Caption := ParameterDisplay[3] + ' : 1';
   GuiGraphXY.UpdateGraph;
  end;
end;

procedure TFmLightweightFeedbackCompressor.UpdateThreshold;
var
  Threshold : Single;
begin
 with TLightweightFeedbackCompressorDataModule(Owner) do
  begin
   Threshold := Parameter[2];
   if Threshold <> DialThreshold.Position
    then DialThreshold.Position := Threshold;
   LbThresholdValue.Caption := ParameterDisplay[2] + ' ' + ParameterLabel[2];
   GuiGraphXY.UpdateGraph;
  end;
end;

procedure TFmLightweightFeedbackCompressor.UpdateStereo;
var
  Brightness : Single;
begin
 with TLightweightFeedbackCompressorDataModule(Owner) do
  begin
   Brightness := 100 * (0.1 + 0.9 * Parameter[6]);
   if Brightness <> LEDStereo.Brightness_Percent
    then LEDStereo.Brightness_Percent := Brightness;
  end;
end;

procedure TFmLightweightFeedbackCompressor.UpdateAutoMakeUpGain;
var
  Brightness : Single;
begin
 with TLightweightFeedbackCompressorDataModule(Owner) do
  begin
   Brightness := 100 * (0.1 + 0.9 * Parameter[8]);
   if Brightness <> LEDAutoGain.Brightness_Percent
    then LEDAutoGain.Brightness_Percent := Brightness;
   DialMakeUpGain.Enabled := Brightness < 50;
   GuiGraphXY.UpdateGraph;
  end;
end;

procedure TFmLightweightFeedbackCompressor.UpdateLimit;
var
  Brightness : Single;
begin
 with TLightweightFeedbackCompressorDataModule(Owner) do
  begin
   Brightness := 100 * (0.1 + 0.9 * Parameter[7]);
   if Brightness <> LEDLimit.Brightness_Percent
    then LEDLimit.Brightness_Percent := Brightness;
  end;
end;

end.
