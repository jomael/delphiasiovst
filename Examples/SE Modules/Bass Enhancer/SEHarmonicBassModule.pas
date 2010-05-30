unit SEHarmonicBassModule;

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
//  SynthEdit is witten by Jef McClintock (see http://www.synthedit.com/      //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  DAV_Types, DAV_SECommon, DAV_SEModule, DAV_DspFrequencyDivider,
  DAV_DspPsychoacousticBassEnhancer;

type
  // define some constants to make referencing in/outs clearer
  TSEHarmonicBassPins = (pinInput, pinOutput, pinFrequency, pinRatio, pinResponse,
    pinHighpassSelect, pinDecay, pinInputLevel, pinOriginalBassLevel,
    pinHarmonicBassLevel, pinHighFrequencyLevel);

  TSEHarmonicBassModule = class(TSEModuleBase)
  private
    FInputBuffer        : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer       : PDAVSingleFixedArray;
    FStaticCount        : Integer;

    FFrequency          : Single;
    FRatio              : Single;
    FResponse           : Single;
    FHighpassSelect     : Integer;
    FDecay              : Single;
    FHarmonicBassLevel      : PDAVSingleFixedArray;
    FHighFrequencyLevel : PDAVSingleFixedArray;
    FInputLevel         : PDAVSingleFixedArray;
    FOriginalBassLevel  : PDAVSingleFixedArray;
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FBassEnhancer : THarmonicBass;
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    procedure SampleRateChanged; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer);
  end;

implementation

uses
  SysUtils;

{ TSEHarmonicBassModule }

constructor TSEHarmonicBassModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FBassEnhancer := THarmonicBass.Create
end;

destructor TSEHarmonicBassModule.Destroy;
begin
 FreeAndNil(FBassEnhancer);
 inherited;
end;

procedure TSEHarmonicBassModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TSEHarmonicBassModule.SampleRateChanged;
begin
 inherited;
 FBassEnhancer.SampleRate := SampleRate;
end;

procedure TSEHarmonicBassModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Inp    : PDAVSingleFixedArray;
  Outp   : PDAVSingleFixedArray;
  MBLev  : PDAVSingleFixedArray;
  HFLev  : PDAVSingleFixedArray;
  InpLv  : PDAVSingleFixedArray;
  OrigLv : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Inp    := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Outp   := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 MBLev  := PDAVSingleFixedArray(@FHarmonicBassLevel[BufferOffset]);
 HFLev  := PDAVSingleFixedArray(@FHighFrequencyLevel[BufferOffset]);
 InpLv  := PDAVSingleFixedArray(@FInputLevel[BufferOffset]);
 OrigLv := PDAVSingleFixedArray(@FOriginalBassLevel[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   Outp^[Sample] := FBassEnhancer.ProcessSample32(Inp^[Sample]);
   if (Sample div 2) = 0 then
    begin
     FBassEnhancer.HarmonicBassLevel  := MBLev^[Sample];
     FBassEnhancer.HighFrequencyLevel := HFLev^[Sample];
     FBassEnhancer.InputLevel         := InpLv^[Sample];
     FBassEnhancer.OriginalBassLevel  := OrigLv^[Sample];
    end;
  end;
end;

procedure TSEHarmonicBassModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TSEHarmonicBassModule.ChooseProcess;
begin
 if Pin[Integer(pinInput)].Status = stRun
  then OnProcess := SubProcess
  else
   begin
    FStaticCount := 2 * BlockSize;
    OnProcess := SubProcessStatic;
   end;
end;

// describe your module
class procedure TSEHarmonicBassModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Harmonic Bass Enhancer';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Harmonic Bass Enhancer';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSEHarmonicBassModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := True;
 case TSEHarmonicBassPins(index) of
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
  pinFrequency:
   with Properties^ do
    begin
     Name            := 'Frequency';
     VariableAddress := @FFrequency;
     Direction       := drIn;
     Datatype        := dtSingle;
     DefaultValue    := '80';
    end;
  pinRatio:
   with Properties^ do
    begin
     Name            := 'Ratio';
     VariableAddress := @FRatio;
     Direction       := drIn;
     Datatype        := dtSingle;
     DefaultValue    := '1';
    end;
  pinResponse:
   with Properties^ do
    begin
     Name            := 'Response';
     VariableAddress := @FResponse;
     Direction       := drIn;
     Datatype        := dtSingle;
     DefaultValue    := '20';
    end;
  pinHighpassSelect:
   with Properties^ do
    begin
     Name            := 'Highpass Select';
     VariableAddress := @FHighpassSelect;
     Direction       := drIn;
     Datatype        := dtEnum;
     DatatypeExtra   := 'range 0,2';
     DefaultValue    := '1';
    end;
  pinDecay:
   with Properties^ do
    begin
     Name            := 'Decay';
     VariableAddress := @FDecay;
     Direction       := drIn;
     Datatype        := dtSingle;
     DefaultValue    := '-15';
    end;
  pinInputLevel:
   with Properties^ do
    begin
     Name            := 'Input Level';
     VariableAddress := @FInputLevel;
     Direction       := drIn;
     Datatype        := dtFSample;
    end;
  pinHarmonicBassLevel:
   with Properties^ do
    begin
     Name            := 'Harmonic Bass Level';
     VariableAddress := @FHarmonicBassLevel;
     Direction       := drIn;
     Datatype        := dtFSample;
    end;
  pinHighFrequencyLevel:
   with Properties^ do
    begin
     Name            := 'High Frequency Level';
     VariableAddress := @FHighFrequencyLevel;
     Direction       := drIn;
     Datatype        := dtFSample;
    end;
  pinOriginalBassLevel:
   with Properties^ do
    begin
     Name            := 'Original Bass Level';
     VariableAddress := @FOriginalBassLevel;
     Direction       := drIn;
     Datatype        := dtFSample;
    end;
  else result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

// An input plug has changed value
procedure TSEHarmonicBassModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;

 case TSEHarmonicBassPins(CurrentPin.PinID) of
           pinInput : begin
                       ChooseProcess;
                       Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
                      end;
       pinFrequency : FBassEnhancer.Frequency := FFrequency;
           pinRatio : FBassEnhancer.Ratio := FRatio;
        pinResponse : FBassEnhancer.Response := FResponse;
  pinHighpassSelect : FBassEnhancer.HighpassSelect := THighpassSelect(FHighpassSelect);
           pinDecay : FBassEnhancer.Decay := FDecay;
 end;
end;

end.
