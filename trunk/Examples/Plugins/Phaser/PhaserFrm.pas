unit PhaserFrm;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2006-2010        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, Forms, Controls, StdCtrls, ExtCtrls,
  Graphics, DAV_Types, DAV_VSTModule;

type
  TPhaserForm = class(TForm)
    LbDepthValue: TLabel;
    LbDepth: TLabel;
    LbFeedback: TLabel;
    LbMinimum: TLabel;
    LbMaximum: TLabel;
    LbRate: TLabel;
    LbStages: TLabel;
    LbStagesValue: TLabel;
    LbFeedbackValue: TLabel;
    LbMinimumValue: TLabel;
    LbMaximumValue: TLabel;
    LbRateValue: TLabel;
    SBFeedback: TScrollBar;
    SBMinimum: TScrollBar;
    SBMaximum: TScrollBar;
    SBRate: TScrollBar;
    SBDepth: TScrollBar;
    SBStages: TScrollBar;
    BackgroundImage: TImage;
    procedure SBDepthChange(Sender: TObject);
    procedure SBFeedbackChange(Sender: TObject);
    procedure SBMinimumChange(Sender: TObject);
    procedure SBMaximumChange(Sender: TObject);
    procedure SBRateChange(Sender: TObject);
    procedure SBStagesChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    procedure UpdateDepth;
    procedure UpdateFeedback;
    procedure UpdateMinimum;
    procedure UpdateMaximum;
    procedure UpdateRate;
    procedure UpdateStages;
  end;

implementation

{$R *.DFM}

uses
  DAV_Common, PhaserDM;

procedure TPhaserForm.FormShow(Sender: TObject);
begin
 UpdateDepth;
 UpdateFeedback;
 UpdateMinimum;
 UpdateMaximum;
 UpdateRate;
 UpdateStages;
end;

procedure TPhaserForm.SBDepthChange(Sender: TObject);
begin
  with TPhaserModule(Owner) do
   begin
    Parameter[0] := SBDepth.Position * 0.1;
   end;
end;

procedure TPhaserForm.SBFeedbackChange(Sender: TObject);
begin
  with TPhaserModule(Owner) do
   begin
    Parameter[1] := SBFeedback.Position * 0.1;
   end;
end;

procedure TPhaserForm.SBMinimumChange(Sender: TObject);
begin
  with TPhaserModule(Owner) do
   begin
    if Parameter[2] <> FreqLinearToLog(SBMinimum.Position * 1E-3)
     then Parameter[2] := FreqLinearToLog(SBMinimum.Position * 1E-3);
   end;
end;

procedure TPhaserForm.SBMaximumChange(Sender: TObject);
begin
  with TPhaserModule(Owner) do
   begin
    if Parameter[3] <> FreqLinearToLog(SBMaximum.Position * 1E-3)
     then Parameter[3] := FreqLinearToLog(SBMaximum.Position * 1E-3);
   end;
end;

procedure TPhaserForm.SBRateChange(Sender: TObject);
begin
  with TPhaserModule(Owner) do
   begin
    Parameter[4] := SBRate.Position * 1E-3;
   end;
end;

procedure TPhaserForm.SBStagesChange(Sender: TObject);
begin
  with TPhaserModule(Owner) do
   begin
    Parameter[5] := SBStages.Position;
   end;
end;

procedure TPhaserForm.UpdateDepth;
var
  Depth : Integer;
begin
 with TPhaserModule(Owner) do
  begin
   Depth := round(10 * Parameter[0]);
   if SBDepth.Position <> Depth
    then SBDepth.Position := Depth;
   LbDepthValue.Caption := FloatToStrF(Parameter[0], ffGeneral, 4, 4) + '%';
  end;
end;

procedure TPhaserForm.UpdateFeedback;
var
  Feedback : Integer;
begin
 with TPhaserModule(Owner) do
  begin
   Feedback := round(10 * Parameter[1]);
   if SBFeedback.Position <> Feedback
    then SBFeedback.Position := Feedback;
   LbFeedbackValue.Caption := FloatToStrF(Parameter[1], ffFixed, 3, 1) + '%';
  end;
end;

procedure TPhaserForm.UpdateMinimum;
var
  Minimum : Integer;
begin
 with TPhaserModule(Owner) do
  begin
   Minimum := round(1000 * FreqLogToLinear(Parameter[2]));
   if SBMinimum.Position <> Minimum
    then SBMinimum.Position := Minimum;
   LbMinimumValue.Caption := FloatToStrF(Parameter[2], ffFixed, 6, 0) + 'Hz';
  end;
end;

procedure TPhaserForm.UpdateMaximum;
var
  Maximum : Integer;
begin
 with TPhaserModule(Owner) do
  begin
   Maximum := round(1000 * FreqLogToLinear(Parameter[3]));
   if SBMaximum.Position <> Maximum
    then SBMaximum.Position := Maximum;
   LbMaximumValue.Caption := FloatToStrF(Parameter[3], ffFixed, 6, 0) + 'Hz';
  end;
end;

procedure TPhaserForm.UpdateRate;
var
  Rate : Integer;
begin
 with TPhaserModule(Owner) do
  begin
   Rate := round(1000 * Parameter[4]);
   if SBRate.Position <> Rate
    then SBRate.Position := Rate;
   LbRateValue.Caption := FloatToStrF(Parameter[4], ffFixed, 2, 2) + 'Hz';
  end;
end;

procedure TPhaserForm.UpdateStages;
var
  Stages : Integer;
begin
 with TPhaserModule(Owner) do
  begin
   Stages := round(Parameter[5]);
   if SBStages.Position <> Stages
    then SBStages.Position := Stages;
   LbStagesValue.Caption := IntToStr(Stages);
  end;
end;

end.