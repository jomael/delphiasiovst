unit ConvolutionDM;

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
  Forms, DAV_Types, DAV_VSTModule, DAV_DspConvolution, DAV_AudioFileWAV, 
  DAV_AudioFileAIFF, DAV_AudioFileAU, DAV_AudioData;

type
  TConvolutionDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleCreate(Sender: TObject);
    procedure ParameterMaximumIROrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterLatencyChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FConvolutionClassic    : TConvolution32;
    FConvolutionLowLatency : TLowLatencyConvolution32;
    FSemaphore    : Integer;
  public
    procedure LoadIR(FileName: TFileName);
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, DAV_Common, DAV_Math, ConvolutionGUI;

procedure TConvolutionDataModule.VSTModuleCreate(Sender: TObject);
begin
 FSemaphore := 0;
end;

procedure TConvolutionDataModule.VSTModuleOpen(Sender: TObject);
begin
 FConvolutionClassic := TConvolution32.Create;
 FConvolutionLowLatency := TLowLatencyConvolution32.Create;
 FConvolutionClassic.FFTOrder := max(7, CeilLog2(InitialDelay)) + 1;
 FConvolutionLowLatency.MinimumIRBlockOrder := max(7, CeilLog2(InitialDelay));
 FConvolutionLowLatency.MaximumIRBlockOrder := 17;
end;

procedure TConvolutionDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FConvolutionLowLatency);
 FreeAndNil(FConvolutionClassic);
end;

procedure TConvolutionDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmConvolution.Create(Self);
end;

procedure TConvolutionDataModule.ParameterMaximumIROrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 while FSemaphore > 0 do;
 inc(FSemaphore);
 try
  if Value >= FConvolutionLowLatency.MinimumIRBlockOrder
   then FConvolutionLowLatency.MaximumIRBlockOrder := round(Limit(Value, 7, 20))
   else Value := FConvolutionLowLatency.MinimumIRBlockOrder;
 finally
  dec(FSemaphore);
 end;
end;

procedure TConvolutionDataModule.ParameterLatencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 while FSemaphore > 0 do;
 inc(FSemaphore);
 try
  if Value > FConvolutionLowLatency.MaximumIRBlockOrder
   then Value := FConvolutionLowLatency.MaximumIRBlockOrder;
  FConvolutionLowLatency.MinimumIRBlockOrder := round(Value);
  FConvolutionClassic.FFTOrder := round(Value) + 1;
 finally
  dec(FSemaphore);
 end;
end;

procedure TConvolutionDataModule.LoadIR(FileName: TFileName);
var
  ADC : TAudioDataCollection32;
begin
 while FSemaphore > 0 do;
 inc(FSemaphore);
 try
  ADC := TAudioDataCollection32.Create(Self);
  with ADC do
   try
    LoadFromFile(FileName);

    FConvolutionLowLatency.LoadImpulseResponse(ADC[0].ChannelDataPointer, ADC.SampleFrames);
    if ADC.ChannelCount > 1
     then FConvolutionClassic.LoadImpulseResponse(ADC[1].ChannelDataPointer, ADC.SampleFrames)
     else FConvolutionClassic.LoadImpulseResponse(ADC[0].ChannelDataPointer, ADC.SampleFrames);
   finally
    FreeAndNil(ADC);
   end;
 finally
  dec(FSemaphore);
 end;
end;

procedure TConvolutionDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
begin
 // lock processing
 while FSemaphore > 0 do;
 Inc(FSemaphore);
 try
  FConvolutionClassic.ProcessBlock(@Inputs[0, 0], @Outputs[0, 0], SampleFrames);
  FConvolutionLowLatency.ProcessBlock(@Inputs[1, 0], @Outputs[1, 0], SampleFrames);
 finally
  Dec(FSemaphore);
 end;
end;

end.
