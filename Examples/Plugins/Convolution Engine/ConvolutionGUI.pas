unit ConvolutionGUI;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2010        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses 
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms, Controls, StdCtrls, DAV_Types, DAV_VSTModule, DAV_GuiLabel, 
  DAV_GuiBaseControl;

type
  TFmConvolution = class(TForm)
    EdFileName: TEdit;
    LbFileName: TLabel;
    BtLoad: TButton;
    procedure BtLoadClick(Sender: TObject);
    procedure EdFileNameChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Dialogs, ConvolutionDM, DAV_VSTModuleWithPrograms;

procedure TFmConvolution.BtLoadClick(Sender: TObject);
begin
 with TOpenDialog.Create(Self) do
  try
   DefaultExt := 'wav';
   Filter := 'All known files|*.wav;*.aif*;*.au;*.snd|Wave (*.wav)|*.wav|' +
     'AIFF files (*.aiff)|*.aiff|AU files (*.au)|*.au;*.snd';
   Options := [ofHideReadOnly, ofFileMustExist, ofEnableSizing];
   Title := 'Select an Impulse Response';
   if Execute then EdFileName.Text := FileName;
  finally
   Free;
  end;
end;

procedure TFmConvolution.EdFileNameChange(Sender: TObject);
begin
 if FileExists(EdFileName.Text)
  then TConvolutionDataModule(Owner).LoadIR(EdFileName.Text);
end;

procedure TFmConvolution.FormShow(Sender: TObject);
begin
 EdFileName.Text := '';
end;

end.
