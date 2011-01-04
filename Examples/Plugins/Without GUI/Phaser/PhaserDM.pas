unit PhaserDM;

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
  Windows, SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule, DAV_DspPhaser;

type
  TPhaserModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const inputs, outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure PMDepthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure PMFeedbackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure PMMinimumChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure PMMaximumChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure PMRateChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure PMStagesChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FPhaser : array [0..1] of TPhaser;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  DAV_Common;

procedure TPhaserModule.VSTModuleOpen(Sender: TObject);
begin
 FPhaser[0] := TPhaser.Create;
 FPhaser[1] := TPhaser.Create;

 Parameter[0] := 30;
 Parameter[1] := 30;
 Parameter[2] := 300;
 Parameter[3] := 1000;
 Parameter[4] := 0.1;
 Parameter[5] := 5;
end;

procedure TPhaserModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FPhaser[0]);
 FreeAndNil(FPhaser[1]);
end;

procedure TPhaserModule.PMDepthChange(Sender: TObject; const Index: Integer;
  var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to 1 do
  if Assigned(FPhaser[Channel])
   then FPhaser[Channel].Depth := 0.01 * Value;
end;

procedure TPhaserModule.PMFeedbackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to 1 do
  if Assigned(FPhaser[Channel])
   then FPhaser[Channel].Feedback := 0.01 * Value;
end;

procedure TPhaserModule.PMMinimumChange(Sender: TObject; const Index: Integer;
  var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to 1 do
  if Assigned(FPhaser[Channel])
   then FPhaser[Channel].Minimum := Limit(Value, 20, 20000);
end;

procedure TPhaserModule.PMMaximumChange(Sender: TObject; const Index: Integer;
  var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to 1 do
  if Assigned(FPhaser[Channel])
   then FPhaser[Channel].Maximum := Limit(Value, 20, 20000);
end;

procedure TPhaserModule.PMRateChange(Sender: TObject; const Index: Integer;
  var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to 1 do
  if Assigned(FPhaser[Channel])
   then FPhaser[Channel].Rate := Value;
end;

procedure TPhaserModule.PMStagesChange(Sender: TObject; const Index: Integer;
  var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to 1 do
  if Assigned(FPhaser[Channel])
   then FPhaser[Channel].Stages := round(Value);
end;

procedure TPhaserModule.VSTModuleProcess(const inputs, outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := FPhaser[0].ProcessSample32(Inputs[0, Sample]);
   Outputs[1, Sample] := FPhaser[1].ProcessSample32(Inputs[1, Sample]);
  end;
end;

procedure TPhaserModule.VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := FPhaser[0].ProcessSample32(Inputs[0, Sample]);
   Outputs[1, Sample] := FPhaser[1].ProcessSample32(Inputs[1, Sample]);
  end;
end;

end.
