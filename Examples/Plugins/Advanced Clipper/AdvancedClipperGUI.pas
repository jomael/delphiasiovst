unit AdvancedClipperGUI;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2009        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses 
  Windows, Messages, SysUtils, Classes, Forms, Graphics, Controls, StdCtrls,
  ExtCtrls, DAV_Types, DAV_VSTModule, DAV_GuiGroup, DAV_GuiPanel, DAV_GuiLabel,
  DAV_GuiBaseControl, DAV_GuiDial, DAV_GuiLED;

type
  TFmAdvancedClipper = class(TForm)
    ClipLEDInput: TGuiLED;
    ClipLEDStage1: TGuiLED;
    ClipLEDStage2: TGuiLED;
    DialFilterOrder1: TGuiDial;
    DialFilterOrder2: TGuiDial;
    DialInputGain: TGuiDial;
    DialOSFactor1: TGuiDial;
    DialOSFactor2: TGuiDial;
    DialOutputGain: TGuiDial;
    DIL: TGuiDialImageList;
    GpStage1: TGuiGroup;
    GpStage2: TGuiGroup;
    PnHardClipping: TGuiPanel;
    PnClipping: TGuiPanel;
    LbClipInput: TGuiLabel;
    LbClipStage1: TGuiLabel;
    LbClipStage2: TGuiLabel;
    LbDisplay: TGuiLabel;
    LbFilterOrder: TGuiLabel;
    LbFilterOrder2: TGuiLabel;
    LbHardClip: TGuiLabel;
    LbInputGain: TGuiLabel;
    LbOSFactor: TGuiLabel;
    LbOSFactor2: TGuiLabel;
    LbOutputGain: TGuiLabel;
    LEDHardClip: TGuiLED;
    PnDisplay: TGuiPanel;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure DialFilterOrder1Change(Sender: TObject);
    procedure DialFilterOrder2Change(Sender: TObject);
    procedure DialInputGainChange(Sender: TObject);
    procedure DialOSFactor1Change(Sender: TObject);
    procedure DialOSFactor2Change(Sender: TObject);
    procedure DialOutputGainChange(Sender: TObject);
    procedure LbHardClipClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ClipLEDClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FBackgroundBitmap : TBitmap;
  public
    procedure UpdateInputGain;
    procedure UpdateOSFactor1;
    procedure UpdateOSFactor2;
    procedure UpdateOrder1;
    procedure UpdateOrder2;
    procedure UpdateOutputGain;
    procedure UpdateHardClip;
  end;

implementation

{$R *.DFM}

uses
  Math, {$IFDEF FPC} LazPNG, {$ELSE} PNGImage, {$ENDIF}
  DAV_Common, DAV_GuiCommon, AdvancedClipperDM;

procedure TFmAdvancedClipper.FormCreate(Sender: TObject);
var
  x, y   : Integer;
  s      : array[0..1] of Single;
  h, hr  : Single;
  Line   : PBGR24Array;
  {$IFDEF FPC}
  PngBmp : TPNGImage;
  {$ELSE}
  RS     : TResourceStream;
  {$IFDEF DELPHI2010_UP}
  PngBmp : TPngImage;
  {$ELSE}
  PngBmp : TPngObject;
  {$ENDIF}
  {$ENDIF}

begin
 // Create Background Image
 FBackgroundBitmap := TBitmap.Create;
 with FBackgroundBitmap do
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

 {$IFDEF FPC}
 PngBmp := TPNGImage.Create;
 try
  PngBmp.LoadFromLazarusResource('TwoBandDistortion');

  // yet todo!

 finally
  FreeAndNil(PngBmp);
 end;
 {$ELSE}
 {$IFDEF DELPHI2010_UP}
 PngBmp := TPngImage.Create;
 {$ELSE}
 PngBmp := TPngObject.Create;
 {$ENDIF}
 try
  RS := TResourceStream.Create(hInstance, 'ClipperKnob', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   with DIL.DialImages.Add do
    begin
     DialBitmap.Canvas.Brush.Color := $70848D;
     {$IFDEF DELPHI2010_UP}
     DialBitmap.SetSize(PngBmp.Width, PngBmp.Height);
     PngBmp.DrawUsingPixelInformation(DialBitmap.Canvas, Point(0, 0));
     {$ELSE}
     DialBitmap.Assign(PngBmp);
     {$ENDIF}
     GlyphCount := 65;
    end;
   DialFilterOrder1.DialImageIndex := 0;
   DialFilterOrder2.DialImageIndex := 0;
   DialInputGain.DialImageIndex := 0;
   DialOSFactor1.DialImageIndex := 0;
   DialOSFactor2.DialImageIndex := 0;
   DialOutputGain.DialImageIndex := 0;
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;
 {$ENDIF}
end;

procedure TFmAdvancedClipper.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackgroundBitmap);
end;

procedure TFmAdvancedClipper.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, FBackgroundBitmap);
end;

procedure TFmAdvancedClipper.FormShow(Sender: TObject);
begin
 UpdateInputGain;
 UpdateOSFactor1;
 UpdateOSFactor2;
 UpdateOrder1;
 UpdateOrder2;
 UpdateOutputGain;
 UpdateHardClip;
 LbDisplay.Caption := 'Advanced Clipper';
end;

procedure TFmAdvancedClipper.DialFilterOrder1Change(Sender: TObject);
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   ParameterByName['Stage 1: Filter Order'] := DialFilterOrder1.Position;
  end;
end;

procedure TFmAdvancedClipper.DialFilterOrder2Change(Sender: TObject);
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   ParameterByName['Stage 2: Filter Order'] := DialFilterOrder2.Position;
  end;
end;

procedure TFmAdvancedClipper.DialInputGainChange(Sender: TObject);
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   ParameterByName['Input Gain'] := DialInputGain.Position;
  end;
end;

procedure TFmAdvancedClipper.DialOutputGainChange(Sender: TObject);
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   ParameterByName['Output Gain'] := DialOutputGain.Position;
  end;
end;

procedure TFmAdvancedClipper.DialOSFactor1Change(Sender: TObject);
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   ParameterByName['Stage 1: Oversampling Factor'] := DialOSFactor1.Position;
  end;
end;

procedure TFmAdvancedClipper.DialOSFactor2Change(Sender: TObject);
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   ParameterByName['Stage 2: Oversampling Factor'] := DialOSFactor2.Position;
  end;
end;

procedure TFmAdvancedClipper.ClipLEDClick(Sender: TObject);
begin
 (Sender As TGuiLED).Brightness_Percent := 0;
end;

procedure TFmAdvancedClipper.LbHardClipClick(Sender: TObject);
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   ParameterByName['Hard Clip'] := 1 - ParameterByName['Hard Clip'];
  end;
end;

procedure TFmAdvancedClipper.TimerTimer(Sender: TObject);
begin
 with TAdvancedClipperDataModule(Owner) do
  begin
   ClipLEDInput.Brightness_Percent := Power(Limit(PeakInput - 1, 0, 1), 0.01) * 100;
   ClipLEDStage1.Brightness_Percent := Power(Limit(PeakStage1 - 1, 0, 1), 0.01) * 100;
   ClipLEDStage2.Brightness_Percent := Power(Limit(PeakStage2 - 1, 0, 1), 0.01) * 100;
  end;
end;

procedure TFmAdvancedClipper.UpdateHardClip;
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   LEDHardClip.Brightness_Percent := 10 + 80 * ParameterByName['Hard Clip'];
  end;
end;

procedure TFmAdvancedClipper.UpdateInputGain;
var
  value : Single;
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   Value := ParameterByName['Input Gain'];
   if DialInputGain.Position <> Value
    then DialInputGain.Position := Value;
   LbDisplay.Caption := 'Input Gain: ' + FloatToStrF(Value, ffGeneral, 2, 2) + 'dB';
  end;
end;

procedure TFmAdvancedClipper.UpdateOrder1;
var
  value : Single;
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   Value := ParameterByName['Stage 1: Filter Order'];
   if DialFilterOrder1.Position <> Value
    then DialFilterOrder1.Position := Value;
   LbDisplay.Caption := 'Filter Order: ' + IntToStr(round(Value));
  end;
end;

procedure TFmAdvancedClipper.UpdateOrder2;
var
  value : Single;
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   Value := ParameterByName['Stage 2: Filter Order'];
   if DialFilterOrder2.Position <> Value
    then DialFilterOrder2.Position := Value;
   LbDisplay.Caption := 'Filter Order: ' + IntToStr(round(Value));
  end;
end;

procedure TFmAdvancedClipper.UpdateOSFactor1;
var
  value : Single;
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   Value := ParameterByName['Stage 1: Oversampling Factor'];
   if DialOSFactor1.Position <> Value
    then DialOSFactor1.Position := Value;
   LbDisplay.Caption := 'Oversampling: ' + IntToStr(round(Value)) + 'x';
  end;
end;

procedure TFmAdvancedClipper.UpdateOSFactor2;
var
  value : Single;
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   Value := ParameterByName['Stage 2: Oversampling Factor'];
   if DialOSFactor2.Position <> Value
    then DialOSFactor2.Position := Value;
   LbDisplay.Caption := 'Oversampling: ' + IntToStr(round(Value)) + 'x';
  end;
end;

procedure TFmAdvancedClipper.UpdateOutputGain;
var
  value : Single;
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   Value := ParameterByName['Output Gain'];
   if DialOutputGain.Position <> Value
    then DialOutputGain.Position := Value;
   LbDisplay.Caption := 'Output Gain: ' + FloatToStrF(Value, ffGeneral, 2, 2) + 'dB';
  end;
end;

end.