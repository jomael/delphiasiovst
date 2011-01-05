unit LinkwitzRileySeparatedGui;

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
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls,
  StdCtrls, DAV_GuiPanel, DAV_GuiLabel, DAV_GuiBaseControl,
  DAV_GuiGroup, DAV_GuiEQGraph, DAV_GuiStitchedControls, DAV_GuiStitchedDial,
  DAV_GuiStitchedPngList, DAV_GuiPixelMap;

type
  TFmLinkwitzRiley = class(TForm)
    GbFrequencyResponse: TGuiGroup;
    GpLinkwitzRiley: TGuiGroup;
    GuiEQGraph: TGuiEQGraph;
    LbDisplay: TGuiLabel;
    LbIFrequency: TGuiLabel;
    LbSlope: TGuiLabel;
    LbType: TGuiLabel;
    PnDisplay: TGuiPanel;
    GSPL: TGuiStitchedPNGList;
    DialFrequency: TGuiStitchedDial;
    DialSlope: TGuiStitchedDial;
    DialType: TGuiStitchedDial;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DialTypeChange(Sender: TObject);
    procedure DialSlopeChange(Sender: TObject);
    procedure DialFrequencyChange(Sender: TObject);
    function GuiEQGraphGetFilterGain(Sender: TObject;
      const Frequency: Single): Single;
  private
    FBackground : TGuiCustomPixelMap;
  public
    procedure UpdateFrequency;
    procedure UpdateSlope;
    procedure UpdateType;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  DAV_GuiCommon, DAV_VSTModuleWithPrograms, LinkwitzRileySeparatedDM;

procedure TFmLinkwitzRiley.FormCreate(Sender: TObject);
begin
 FBackground := TGuiPixelMapMemory.Create;
end;

procedure TFmLinkwitzRiley.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackground);
end;

procedure TFmLinkwitzRiley.FormPaint(Sender: TObject);
begin
 if Assigned(FBackground)
  then FBackground.PaintTo(Canvas);
end;

procedure TFmLinkwitzRiley.FormResize(Sender: TObject);
var
  x, y   : Integer;
  s      : array [0..1] of Single;
  h, hr  : Single;
  ScnLn  : PPixel32Array;
begin
 with FBackground do
  begin
   SetSize(ClientWidth, ClientHeight);
   s[0] := 0;
   s[1] := 0;
   hr   := 1 / Height;
   for y := 0 to Height - 1 do
    begin
     ScnLn := Scanline[y];
     h    := 0.1 * (1 - Sqr(2 * (y - Height div 2) * hr));
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.97 * s[0] + 0.03 * Random;
       s[0] := s[1];

       ScnLn[x].B := Round($70 - $34 * (s[1] - h));
       ScnLn[x].G := Round($84 - $48 * (s[1] - h));
       ScnLn[x].R := Round($8D - $50 * (s[1] - h));
       ScnLn[x].A := $FF;
      end;
    end;
  end;
end;

procedure TFmLinkwitzRiley.FormShow(Sender: TObject);
begin
 UpdateFrequency;
 UpdateSlope;
 UpdateType;
 LbDisplay.Caption := 'Linkwitz-Riley';
end;

function TFmLinkwitzRiley.GuiEQGraphGetFilterGain(Sender: TObject;
  const Frequency: Single): Single;
begin
 with Owner as TLinkwitzRileySeparatedModule do
  begin
   Result := -5;
  end;
end;

procedure TFmLinkwitzRiley.DialFrequencyChange(Sender: TObject);
begin
 with Owner as TLinkwitzRileySeparatedModule do
  begin
   if Parameter[0] <> DialFrequency.Value
    then Parameter[0] := DialFrequency.Value;
  end;
end;

procedure TFmLinkwitzRiley.DialSlopeChange(Sender: TObject);
begin
 with Owner as TLinkwitzRileySeparatedModule do
  begin
   if Parameter[1] <> DialSlope.Value
    then Parameter[1] := DialSlope.Value;
  end;
end;

procedure TFmLinkwitzRiley.DialTypeChange(Sender: TObject);
begin
 with Owner as TLinkwitzRileySeparatedModule do
  begin
   if Parameter[2] <> DialType.Value
    then Parameter[2] := DialType.Value;
  end;
end;

procedure TFmLinkwitzRiley.UpdateFrequency;
begin
 with Owner as TLinkwitzRileySeparatedModule do
  begin
   if DialFrequency.Value <> Parameter[0]
    then DialFrequency.Value := Parameter[0];
   LbDisplay.Caption := ParameterDisplay[0] + ' ' + ParameterLabel[0];
  end;
end;

procedure TFmLinkwitzRiley.UpdateSlope;
begin
 with Owner as TLinkwitzRileySeparatedModule do
  begin
   if DialSlope.Value <> Parameter[1]
    then DialSlope.Value := Parameter[1];
   LbDisplay.Caption := 'Slope: ' + ParameterDisplay[1] + 'dB/Oct';
  end;
end;

procedure TFmLinkwitzRiley.UpdateType;
begin
 with Owner as TLinkwitzRileySeparatedModule do
  begin
   if DialType.Value <> Parameter[2]
    then DialType.Value := Parameter[2];
   LbDisplay.Caption := ParameterDisplay[2];
  end;
end;

end.
