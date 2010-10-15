unit DAV_GuiBackgrounds;

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

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, LMessages,
  {$ELSE} Windows, Messages, {$ENDIF}
  Graphics, Classes, Controls, ExtCtrls, DAV_GuiCommon;

type
  TCustomGuiBackground = class(TComponent)
  private
    FPrevParentWndProc : Pointer;
    FParentHandle      : THandle;
    FActive            : Boolean;
    FOffset            : TPoint;
    FParentWinControl  : TWinControl;
    FColor             : TColor;
    FAmount            : Byte;
    procedure NewParentWndProc(var Msg: TMessage);
    procedure SetActive(const Value: Boolean);
    procedure SetColor(const Value: TColor);
    procedure SetAmount(const Value: Byte);
  protected
    FBitmap : TBitmap;
    procedure ActiveChanged; virtual;
    procedure AmountChanged; virtual;
    procedure ColorChanged; virtual;
    procedure DrawBackground; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Active: Boolean read FActive write SetActive default True;
    property Amount: Byte read FAmount write SetAmount default $20;
    property Color: TColor read FColor write SetColor default $C8D0D4;
  end;

  TGuiBackground = class(TCustomGuiBackground)
  published
    property Active;
    property Amount;
    property Color;
  end;

implementation

uses
  Forms, Math, SysUtils;

resourcestring
  RCStrParentInvalid = 'Parent is not a TWinControl descendant';

{ TCustomGuiBackground }

constructor TCustomGuiBackground.Create(AOwner: TComponent);
var
  P : Pointer;
begin
 if not (AOwner is TWinControl)
  then raise Exception.Create(RCStrParentInvalid);

 inherited Create(AOwner);
 FParentWinControl := TWinControl(Owner);
 FParentHandle := FParentWinControl.Handle;
 FActive := True;
 FAmount := $20;

 if AOwner is TCustomForm
  then FColor := ColorToRGB(TCustomForm(AOwner).Color)
  else FColor := $C8D0D4;

 FBitmap := TBitmap.Create;
 FBitmap.PixelFormat := pf24bit;

 FPrevParentWndProc := Pointer(GetWindowLong(FParentHandle, GWL_WNDPROC));
 P := MakeObjectInstance(NewParentWndProc);
 SetWindowLong(FParentHandle, GWL_WNDPROC, LongInt(P));
end;

destructor TCustomGuiBackground.Destroy;
begin
 SetWindowLong(FParentHandle, GWL_WNDPROC, LongInt(FPrevParentWndProc));
 inherited;
end;

procedure TCustomGuiBackground.NewParentWndProc(var Msg: TMessage);
var
  DC : hDC;
begin
 with Msg do
  begin
   Result := CallWindowProc(FPrevParentWndProc, FParentHandle, Msg,
     WParam, LParam);
   if FActive then
    case Msg of
     WM_PAINT :
      begin
       DC := GetWindowDC(FParentHandle);
       with TControlCanvas.Create do
        begin
         Handle := DC;
         Draw(FOffset.X, FOffset.Y, FBitmap);
        end;
      end;
     WM_MOVE :
      begin
       FOffset.X := LParamLo - FParentWinControl.Left;
       FOffset.Y := LParamHi - FParentWinControl.Top;
      end;
     WM_SIZE :
      with FBitmap do
       begin
        Width := LParamLo;
        Height := LParamHi;
        DrawBackground;
       end;
    end;
  end;
end;

procedure TCustomGuiBackground.SetActive(const Value: Boolean);
begin
 if FActive <> Value then
  begin
   FActive := Value;
   ActiveChanged;
  end;
end;

procedure TCustomGuiBackground.SetAmount(const Value: Byte);
begin
 if FAmount <> Value then
  begin
   FAmount := Value;
   AmountChanged;
  end;
end;

procedure TCustomGuiBackground.SetColor(const Value: TColor);
begin
 if FColor <> Value then
  begin
   FColor := Value;
   ColorChanged;
  end;
end;

procedure TCustomGuiBackground.ActiveChanged;
begin
 FParentWinControl.Invalidate;
end;

procedure TCustomGuiBackground.AmountChanged;
begin
 FParentWinControl.Invalidate;
end;

procedure TCustomGuiBackground.ColorChanged;
begin
 FParentWinControl.Invalidate;
end;

procedure TCustomGuiBackground.DrawBackground;
var
  x, y : Integer;
  s    : array[0..1] of Single;
  Val  : Integer;
  Line : PRGB24Array;
begin
 with FBitmap do
  begin
   s[0] := 0;
   s[1] := 0;
   for y := 0 to Height - 1 do
    begin
     Line := Scanline[y];
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.97 * s[0] + 0.03 * (2 * random - 1);

       Val := (FColor shr 16) and $FF;
       Val := Round(Val + FAmount * s[1]);
       if Val <= 0 then Line[x].B := 0 else
       if Val >= 255 then Line[x].B := 255
        else Line[x].B := Val;

       Val := (FColor shr 8) and $FF;
       Val := Round(Val + FAmount * s[1]);
       if Val <= 0 then Line[x].G := 0 else
       if Val >= 255 then Line[x].G := 255
        else Line[x].G := Val;

       Val := FColor and $FF;
       Val := Round(Val + FAmount * s[1]);
       if Val <= 0 then Line[x].R := 0 else
       if Val >= 255 then Line[x].R := 255
        else Line[x].R := Val;

       s[0] := s[1];
      end;
    end;
  end;
end;

end.