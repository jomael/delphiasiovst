unit Chebyshev2GUI;

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
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms, ExtCtrls, Controls, StdCtrls, DAV_Types, DAV_VSTModule, 
  DAV_GuiBaseControl, DAV_GuiLabel, DAV_GuiPanel, DAV_GuiStitchedControls, 
  DAV_GuiStitchedDial, DAV_GuiStitchedPngList;

type
  TFmChebyshev2 = class(TForm)
    LbChebyshev2FilterDemo: TGuiLabel;
    LbChebyshev2FilterDemoShaddow: TGuiLabel;
    LbFrequency: TGuiLabel;
    LbFrequencyValue: TGuiLabel;
    LbOrder: TGuiLabel;
    LbOrderValue: TGuiLabel;
    LbStopband: TGuiLabel;
    LbStopbandValue: TGuiLabel;
    PnControls: TGuiPanel;
    GSPL: TGuiStitchedPNGList;
    DialFrequency: TGuiStitchedDial;
    DialStopband: TGuiStitchedDial;
    DialOrder: TGuiStitchedDial;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure DialFrequencyChange(Sender: TObject);
    procedure DialStopbandChange(Sender: TObject);
    procedure DialOrderChange(Sender: TObject);
    procedure DialFrequencyDblClick(Sender: TObject);
    procedure DialStopbandDblClick(Sender: TObject);
    procedure DialOrderDblClick(Sender: TObject);
    procedure EdValueKeyPress(Sender: TObject; var Key: Char);
    procedure PnControlsClick(Sender: TObject);
  private
    FEdValue: TEdit;
  public
    procedure UpdateFrequency;
    procedure UpdateStopband;
    procedure UpdateOrder;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  DAV_VSTModuleWithPrograms, Chebyshev2DM;

procedure TFmChebyshev2.FormDestroy(Sender: TObject);
begin
 if Assigned(FEdValue)
  then FreeAndNil(FEdValue);
end;

procedure TFmChebyshev2.FormShow(Sender: TObject);
begin
 UpdateFrequency;
 UpdateStopband;
 UpdateOrder;
(*
 with TChebyshevLPModule(Owner) do
  begin
   Resizer.SetEditorHwnd(Self.Handle);
  end;
*)
end;

procedure TFmChebyshev2.PnControlsClick(Sender: TObject);
begin
 if Assigned(FEdValue)
  then FreeAndNil(FEdValue);
end;

procedure TFmChebyshev2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 with TChebyshev2HPModule(Owner) do
  begin
   Resizer.SetEditorHwnd(0);
  end;
end;

procedure TFmChebyshev2.DialFrequencyChange(Sender: TObject);
begin
 with TChebyshev2HPModule(Owner) do
  begin
   if ParameterByName['Frequency'] <> DialFrequency.Value
    then ParameterByName['Frequency'] := DialFrequency.Value;
  end;
end;

procedure TFmChebyshev2.DialFrequencyDblClick(Sender: TObject);
begin
 if not Assigned(FEdValue)
  then FEdValue := TEdit.Create(Self);

 with FEdValue do
  begin
   Parent := PnControls;
   Left := LbFrequencyValue.Left;
   Top := LbFrequencyValue.Top;
   Width := LbFrequencyValue.Width;
   Height := LbFrequencyValue.Height;
   BorderStyle := bsNone;
   Color := PnControls.PanelColor;
   Text := LbFrequencyValue.Caption;
   Tag := 0;
   OnKeyPress := EdValueKeyPress;
   SetFocus;
  end;
end;

procedure TFmChebyshev2.DialOrderChange(Sender: TObject);
begin
 with TChebyshev2HPModule(Owner) do
  begin
   if ParameterByName['Order'] <> DialOrder.Value
    then ParameterByName['Order'] := DialOrder.Value;
  end;
end;

procedure TFmChebyshev2.DialOrderDblClick(Sender: TObject);
begin
 if not Assigned(FEdValue)
  then FEdValue := TEdit.Create(Self);

 with FEdValue do
  begin
   Parent := PnControls;
   Left := LbOrderValue.Left;
   Top := LbOrderValue.Top;
   Width := LbOrderValue.Width;
   Height := LbOrderValue.Height;
   BorderStyle := bsNone;
   Color := PnControls.PanelColor;
   Text := LbOrderValue.Caption;
   Tag := 2;
   OnKeyPress := EdValueKeyPress;
   SetFocus;
  end;
end;

procedure TFmChebyshev2.DialStopbandChange(Sender: TObject);
begin
 with TChebyshev2HPModule(Owner) do
  begin
   if ParameterByName['Stopband'] <> DialStopband.Value
    then ParameterByName['Stopband'] := DialStopband.Value;
  end;
end;

procedure TFmChebyshev2.DialStopbandDblClick(Sender: TObject);
begin
 if not Assigned(FEdValue)
  then FEdValue := TEdit.Create(Self);

 with FEdValue do
  begin
   Parent := PnControls;
   Left := LbStopbandValue.Left;
   Top := LbStopbandValue.Top;
   Width := LbStopbandValue.Width;
   Height := LbStopbandValue.Height;
   BorderStyle := bsNone;
   Color := PnControls.PanelColor;
   Text := LbStopbandValue.Caption;
   Tag := 1;
   OnKeyPress := EdValueKeyPress;
   SetFocus;
  end;
end;

procedure TFmChebyshev2.EdValueKeyPress(Sender: TObject; var Key: Char);
begin
 with TChebyshev2HPModule(Owner) do
  if (Key = #13) and Assigned(FEdValue) then
   try
    StringToParameter(FEdValue.Tag, FEdValue.Text);
    FreeAndNil(FEdValue);
   except
   end;
end;

procedure TFmChebyshev2.UpdateFrequency;
var
  Freq : Single;
begin
 with TChebyshev2HPModule(Owner) do
  begin
   Freq := ParameterByName['Frequency'];
   if DialFrequency.Value <> Freq
    then DialFrequency.Value := Freq;
   if Freq < 1000
    then LbFrequencyValue.Caption := FloatToStrF(Freq, ffGeneral, 5, 5) + ' Hz'
    else LbFrequencyValue.Caption := FloatToStrF(Freq * 1E-3, ffGeneral, 5, 5) + ' kHz'
  end;
end;

procedure TFmChebyshev2.UpdateOrder;
var
  Order : Single;
begin
 with TChebyshev2HPModule(Owner) do
  begin
   Order := ParameterByName['Order'];
   if DialOrder.Value <> Order
    then DialOrder.Value := Order;
   LbOrderValue.Caption := IntToStr(round(Order));
  end;
end;

procedure TFmChebyshev2.UpdateStopband;
var
  Stopband : Single;
begin
 with TChebyshev2HPModule(Owner) do
  begin
   Stopband := ParameterByName['Stopband'];
   if DialStopband.Value <> Stopband
    then DialStopband.Value := Stopband;
   LbStopbandValue.Caption := FloatToStrF(Stopband, ffGeneral, 3, 3) + ' dB';
  end;
end;

end.
