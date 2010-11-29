unit ChebyshevWaveshaperGUI;

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
  Forms, Controls, Graphics, DAV_Types, DAV_VSTModule, DAV_GuiLabel,
  DAV_GuiStitchedControls, DAV_GuiStitchedDial, DAV_GuiStitchedPngList,
  DAV_GuiPng, DAV_GuiFont;

type
  TFmChebyshevWaveshaper = class(TForm)
    GSPL: TGuiStitchedPNGList;
    procedure GuiDialChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FDials  : array of TGuiStitchedDial;
    FLabels : array of TGuiLabel;
  public
    procedure UpdateHarmonic(Index: Integer);  
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, ChebyshevWaveshaperDM, DAV_VSTModuleWithPrograms;

procedure TFmChebyshevWaveshaper.FormCreate(Sender: TObject);
var
  i  : Integer;
begin
 ClientWidth := 8 + 6 * 56;
 ClientHeight := 8 + 4 * 72;

 with TChebyshevWaveshaperDataModule(Owner) do
  begin
   SetLength(FDials, numParams);
   SetLength(FLabels, numParams);
   for i := 0 to numParams - 2 do
    begin
     FDials[i] := TGuiStitchedDial.Create(Self);
     with FDials[i] do
      begin
       Parent   := Self;
       Anchors  := [];
       Width    := 48;
       Height   := 48;
       Left     := 8 + (i mod 6 * (Width + 8));
       Top      := 8 + (i div 6) * 72;
       StitchedImageList  := GSPL;
       StitchedImageIndex := 0;
       Min                := -1;
       Max                := 1;
       Tag                := i;
       Value              := Sign(Parameter[i]) * Sqr(Parameter[i]);
       OnChange           := GuiDialChange;
      end;
     FLabels[i] := TGuiLabel.Create(Self);
     with FLabels[i] do
      begin
       Parent           := Self;
       Anchors          := [];
       Width            := 48;
       Height           := 16;
       Left             := 8 + (i mod 6 * (Width + 8));
       Top              := 56 + (i div 6) * 72;
       Alignment        := taCenter;
       FontOversampling := fo3x;
       if i = 0
        then Caption    := 'Fun.'
        else Caption    := 'H' + IntToStr(i + 1);
       Font.Color       := 13877402;
       Font.Height      := -13;
       Font.Name        := 'Verdana';
       Font.Style       := [fsBold];
       Tag              := i;
      end;
    end;
  end;
end;

procedure TFmChebyshevWaveshaper.FormDestroy(Sender: TObject);
var
  i : Integer;
begin
 with TChebyshevWaveshaperDataModule(Owner) do
  begin
   for i := 0 to numParams - 1 do
    begin
     FreeAndNil(FDials[i]);
     FreeAndNil(FLabels[i]);
    end;
   SetLength(FDials, 0);
   SetLength(FLabels, 0);
  end;
end;

procedure TFmChebyshevWaveshaper.GuiDialChange(Sender: TObject);
var
  NewValue : Single;
begin
 with TChebyshevWaveshaperDataModule(Owner), (Sender as TGuiStitchedDial) do
  begin
   NewValue := Sign(Value) * Sqrt(Abs(Value));
   if Parameter[Tag] <> NewValue
    then Parameter[Tag] := NewValue;
  end;
end;

procedure TFmChebyshevWaveshaper.UpdateHarmonic(Index: Integer);
begin
 with TChebyshevWaveshaperDataModule(Owner), FDials[Index] do
  begin
   Value := Sign(Parameter[Index]) * Sqr(Parameter[Index]);
  end;
end;

end.