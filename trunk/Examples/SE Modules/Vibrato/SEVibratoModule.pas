unit SEVibratoModule;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2011        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
//  SynthEdit is witten by Jef McClintock (see http://www.synthedit.com/      //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  DAV_Types, DAV_SECommon, DAV_SEModule, DAV_DspVibrato;

type
  // define some constants to make referencing in/outs clearer
  TSEVibratoPins = (pinInput, pinOutput, pinDepth, pinSpeed);

  TCustomSEVibratoModule = class(TSEModuleBase)
  private
    FInputBuffer  : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer : PDAVSingleFixedArray;
    FStaticCount  : Integer;
    FDepth        : Single;
    FSpeed        : Single;
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FVibrato      : TDspVibrato32;
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    procedure SampleRateChanged; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); virtual; abstract;
  end;

  TSEVibratoStaticModule = class(TCustomSEVibratoModule)
  private
    FDepth : Single;
    FSpeed : Single;
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSEVibratoControllableModule = class(TSEVibratoStaticModule)
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

implementation

uses
  SysUtils;

{ TCustomSEVibratoModule }

constructor TCustomSEVibratoModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FVibrato := TDspVibrato32.Create
end;

destructor TCustomSEVibratoModule.Destroy;
begin
 FreeAndNil(FVibrato);
 inherited;
end;

procedure TCustomSEVibratoModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TCustomSEVibratoModule.SampleRateChanged;
begin
 inherited;
 FVibrato.SampleRate := SampleRate;
end;

procedure TCustomSEVibratoModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TCustomSEVibratoModule.ChooseProcess;
begin
 if Pin[Integer(pinInput)].Status = stRun
  then OnProcess := SubProcess
  else
   begin
    FStaticCount := BlockSize;
    OnProcess := SubProcessStatic;
   end;
end;

// describe the pins (plugs)
function TCustomSEVibratoModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case TSEVibratoPins(index) of
  pinInput:
   with Properties^ do
    begin
     Name            := 'Input';
     VariableAddress := @FInputBuffer;
     Direction       := drIn;
     Flags           := [iofLinearInput];
     Datatype        := dtFSample;
     DefaultValue    := '0';
    end;
  pinOutput:
   with Properties^ do
    begin
     Name            := 'Output';
     VariableAddress := @FOutputBuffer;
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  pinDepth:
   with Properties^ do
    begin
     Name            := 'Depth';
     VariableAddress := @FDepth;
     Direction       := drIn;
     Datatype        := dtSingle;
     DefaultValue    := '0.2';
    end;
  pinSpeed:
   with Properties^ do
    begin
     Name            := 'Speed';
     VariableAddress := @FSpeed;
     Direction       := drIn;
     Datatype        := dtSingle;
     DefaultValue    := '0.2';
    end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

// An input plug has changed value
procedure TCustomSEVibratoModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSEVibratoPins(CurrentPin.PinID) of
  pinInput: begin
             ChooseProcess;
             Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
            end;
 end;
end;


{ TSEVibratoStaticModule }

// describe your module
class procedure TSEVibratoStaticModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Vibrato (static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Vibrato (static)';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

function TSEVibratoStaticModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case TSEVibratoPins(index) of
  pinDepth:
   with Properties^ do
    begin
     Name            := 'Depth';
     VariableAddress := @FDepth;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '0.2';
     Result          := True;
    end;
  pinSpeed:
   with Properties^ do
    begin
     Name            := 'Speed';
     VariableAddress := @FSpeed;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '0.2';
     Result          := True;
    end;
 end;
end;

procedure TSEVibratoStaticModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSEVibratoPins(CurrentPin.PinID) of
  pinDepth: FVibrato.Depth := FDepth;
  pinSpeed: FVibrato.Speed := FSpeed;
 end;
end;

procedure TSEVibratoStaticModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Inp    : PDAVSingleFixedArray;
  Outp   : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Inp  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Outp := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Outp^[Sample] := FVibrato.ProcessSample32(Inp^[Sample]);
end;


{ TSEVibratoControllableModule }

class procedure TSEVibratoControllableModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Vibrato';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Vibrato';
  end;
end;

function TSEVibratoControllableModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case TSEVibratoPins(index) of
  pinDepth: with Properties^ do Direction := drIn;
  pinSpeed: with Properties^ do Direction := drIn;
 end;
end;

end.
