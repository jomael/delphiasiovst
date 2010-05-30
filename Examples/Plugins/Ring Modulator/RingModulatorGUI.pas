unit RingModulatorGUI;

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
  Windows, Messages, SysUtils, Classes, Forms, Graphics, Controls, StdCtrls,
  ExtCtrls, DAV_Types, DAV_VSTModule, DAV_GuiBaseControl, DAV_GuiDial,
  DAV_GuiLabel, DAV_GuiPanel, DAV_GuiGroup;

type
  TFmRingModulator = class(TForm)
    GpFrequency: TGuiGroup;
    DialFrequency: TGuiDial;
    PnDisplay: TGuiPanel;
    LbDisplay: TGuiLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DialFrequencyChange(Sender: TObject);
  private
    FBackgrounBitmap : TBitmap;
  public
    procedure UpdateFrequency;
  end;

implementation

{$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}

uses
  PngImage, DAV_GuiCommon, RingModulatorDM, DAV_VSTModuleWithPrograms,
  DAV_VSTParameters;

procedure TFmRingModulator.FormCreate(Sender: TObject);
var
  RS     : TResourceStream;
  PngBmp : TPngObject;

begin
 // Create BackgRound Image
 FBackgrounBitmap := TBitmap.Create;
 FBackgrounBitmap.PixelFormat := pf24bit;

 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(hInstance, 'RingModulator', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   DialFrequency.DialBitmap.Assign(PngBmp);
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;
end;

procedure TFmRingModulator.FormShow(Sender: TObject);
begin
 with TRingModulatorDataModule(Owner)
  do DialFrequency.Max := ParameterProperties[0].Max;
 UpdateFrequency;
end;

procedure TFmRingModulator.FormResize(Sender: TObject);
var
  x, y   : Integer;
  s      : array[0..1] of Single;
  h, hr  : Single;
  Line   : PRGB24Array;
begin
 with FBackgrounBitmap do
  begin
   PixelFormat := pf24bit;
   Width := Self.Width;
   Height := Self.Height;
   s[0] := 0;
   s[1] := 0;
   hr   := 1 / Height;
   for y := 0 to Height - 1 do
    begin
     Line := Scanline[y];
     h    := 0.1 * (1 - sqr(2 * (y - Height div 2) * hr));
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.97 * s[0] + 0.03 * random;
       s[0] := s[1];

       Line[x].B := Round($70 - $34 * (s[1] - h));
       Line[x].G := Round($84 - $48 * (s[1] - h));
       Line[x].R := Round($8D - $50 * (s[1] - h));
      end;
    end;
  end;
end;

procedure TFmRingModulator.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, FBackgrounBitmap);
end;

procedure TFmRingModulator.DialFrequencyChange(Sender: TObject);
begin
 with TRingModulatorDataModule(Owner) do
  begin
   if Parameter[0] <> DialFrequency.Position
    then Parameter[0] := DialFrequency.Position;
  end;
end;

procedure TFmRingModulator.UpdateFrequency;
begin
 with TRingModulatorDataModule(Owner) do
  begin
   if DialFrequency.Position <> Parameter[0]
    then DialFrequency.Position := Parameter[0];
   LbDisplay.Caption := ParameterDisplay[0] + ' ' + ParameterLabel[0];
  end;
end;

{$IFDEF FPC}
initialization

{$ENDIF}

end.
