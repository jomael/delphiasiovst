unit DAV_DspR128;

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
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, SysUtils, DAV_Types, DAV_Classes, DAV_DspFilterBasics,
  DAV_DspDelayLines;


type
  PPLinkedLoudnessRecord = ^PLinkedLoudnessRecord;
  PLinkedLoudnessRecord = ^TLinkedLoudnessRecord;
  TLinkedLoudnessRecord = record
    Loudness : Single;
    Value    : Single;
    Next     : PLinkedLoudnessRecord;
  end;

  TCustomR128 = class(TDspSampleRatePersistent)
  private
    FShortIntSum       : Double;
    FMomIntSum         : Double;
    FAbsoluteGatedSum  : Double;
    FShortIntScale     : Double;
    FMomIntScale       : Double;
    FPeakHold          : Double;

    FUnitOffset        : Single;
    FSampleCount       : Integer;
    FOverlapSamples    : Integer;
    F400msSampleCount  : Integer;
    F2600msSampleCount : Integer;
    FTotalSamples      : Integer;
    FIsRunning         : Boolean;
  protected
    function GetPeakHold: Single;
    function GetLoudnessShort: Single;
    function GetLoudnessMomentary: Single;
    function GetLoudnessIntegration: Single; virtual; abstract;
    procedure ClearLinkedLoudness; virtual; abstract;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ResetPeak; virtual;

    property LoudnessShort: Double read GetLoudnessShort;
    property LoudnessMomentary: Double read GetLoudnessMomentary;
    property LoudnessIntegration: Single read GetLoudnessIntegration;
    property PeakHold: Double read GetPeakHold;
    property TotalSamples: Integer read FTotalSamples;
    property IntegrationIsRunning: Boolean read FIsRunning write FIsRunning;
  end;

  TMonoR128 = class(TCustomR128)
  private
    FShortIntValue       : Double;
    FMomIntValue         : Double;
    FAbsoluteGatedValue  : Double;
    FAbsoluteGatedCount  : Integer;
    FPreFilter           : TBasicHighShelfFilter;
    FRLBFilter           : TBasicLowcutFilter;
    FDelayLine400ms      : TDelayLineSamples32;
    FDelayLine2600ms     : TDelayLineSamples32;
    FLinkedLoudness      : PLinkedLoudnessRecord;
  protected
    function GetLoudnessIntegration: Single; override;
    procedure ClearLinkedLoudness; override;
    procedure SampleRateChanged; override;
    procedure ProcessLongTermSample(Value: Single);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ResetPeak; override;
    procedure ProcessSample(Value: Single);
  end;

  TStereoR128 = class(TCustomR128)
  private
    FShortIntValue       : array [0..1] of Double;
    FMomIntValue         : array [0..1] of Double;
    FAbsoluteGatedValue  : array [0..1] of Double;
    FAbsoluteGatedCount  : array [0..1] of Integer;
    FPreFilter           : array [0..1] of TBasicHighShelfFilter;
    FRLBFilter           : array [0..1] of TBasicLowcutFilter;
    FDelayLine400ms      : array [0..1] of TDelayLineSamples32;
    FDelayLine2600ms     : array [0..1] of TDelayLineSamples32;
    FLinkedLoudness      : array [0..1] of PLinkedLoudnessRecord;
  protected
    function GetLoudnessIntegration: Single; override;
    procedure ClearLinkedLoudness; override;
    procedure SampleRateChanged; override;
    procedure ProcessLongTermSample(const Index: Integer; Value: Single);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ResetPeak; override;
    procedure ProcessMono(Value: Single);
    procedure ProcessStereo(Left, Right: Single);
  end;

implementation

uses
  DAV_Approximations;

const
  CMeanSquareBias : Single = 1E-10;

constructor TCustomR128.Create;
begin
 // calculate buffer sizes
 if Abs(SampleRate) = 0 then
  begin
   F400msSampleCount := 17640;
   F2600msSampleCount := 114660;
  end
 else
  begin
   F400msSampleCount := Round(0.4 * Abs(SampleRate));
   F2600msSampleCount := Round(2.6 * Abs(SampleRate));
  end;

 FMomIntScale := 1 / F400msSampleCount;
 FShortIntScale := 1 / (F400msSampleCount + F2600msSampleCount);
 FOverlapSamples := F400msSampleCount div 4;

 FUnitOffset := 23;
 FMomIntSum := 0;
 FShortIntSum := 0;
 FAbsoluteGatedSum := 0;

 ResetPeak;
end;

destructor TCustomR128.Destroy;
begin
 ClearLinkedLoudness;
end;

procedure TCustomR128.ResetPeak;
begin
 FPeakHold := 0;
 FSampleCount := 0;
 FTotalSamples := 0;

 ClearLinkedLoudness;
end;

function TCustomR128.GetLoudnessMomentary: Single;
begin
 Result := FUnitOffset - 0.691 + 10 * FastLog10ContinousError3(FMomIntSum);
end;

function TCustomR128.GetLoudnessShort: Single;
begin
 Result := FUnitOffset - 0.691 + 10 * FastLog10ContinousError3(FShortIntSum);
end;

function TCustomR128.GetPeakHold: Single;
begin
 Result := FUnitOffset - 0.691 + 10 * FastLog10ContinousError3(FPeakHold);
end;



{ TMonoR128 }

procedure TMonoR128.ClearLinkedLoudness;
var
  LinkedLoudness     : PLinkedLoudnessRecord;
  ThisLinkedLoudness : PLinkedLoudnessRecord;
begin
 LinkedLoudness := FLinkedLoudness;
 FLinkedLoudness := nil;

 while Assigned(LinkedLoudness) do
  begin
   ThisLinkedLoudness := LinkedLoudness;
   LinkedLoudness := LinkedLoudness.Next;
   FreeMem(ThisLinkedLoudness);
  end;
end;

constructor TMonoR128.Create;
begin
 inherited;

 FPreFilter := TBasicHighShelfFilter.Create;
 with FPreFilter do
  begin
   if Abs(Self.SampleRate) > 0
    then SampleRate := Self.SampleRate;
   Frequency := 1500;
   Gain := 4;
   Bandwidth := 1.8;
  end;
 FRLBFilter := TBasicLowcutFilter.Create;
 with FRLBFilter do
  begin
   if Abs(Self.SampleRate) > 0
    then SampleRate := Self.SampleRate;
   Frequency := 50;
   Bandwidth := 1.8;
  end;

 FDelayLine400ms := TDelayLineSamples32.Create(F400msSampleCount);
 FDelayLine2600ms := TDelayLineSamples32.Create(F2600msSampleCount);

 FShortIntValue := 0;
 FMomIntValue := 0;
end;

destructor TMonoR128.Destroy;
begin
 FreeAndNil(FPreFilter);
 FreeAndNil(FRLBFilter);
 FreeAndNil(FDelayLine400ms);
 FreeAndNil(FDelayLine2600ms);
 inherited;
end;

function TMonoR128.GetLoudnessIntegration: Single;
var
  Value        : Single;
  LLRec        : PLinkedLoudnessRecord;
  Loudness     : Single;
  ItemCount    : Integer;
begin
 // calculate integrated loudness level
 if FAbsoluteGatedCount = 0
  then Value := CMeanSquareBias
  else Value := FAbsoluteGatedValue / FAbsoluteGatedCount;
 Loudness := -0.691 + 10 * FastLog10ContinousError3(Value) - 8;

 // calculate integrated loudness level
 ItemCount := 0;
 Value := 0;
 LLRec := FLinkedLoudness;
 while Assigned(LLRec) do
  begin
   if LLRec^.Loudness < Loudness
    then Break;

   Inc(ItemCount);
   Value := Value + LLRec^.Value;
   LLRec := LLRec^.Next;
  end;

 if ItemCount = 0
  then Value := CMeanSquareBias
  else Value := Value / ItemCount;

 Result := FUnitOffset - 0.691 + 10 * FastLog10ContinousError3(Value);
end;

procedure TMonoR128.ProcessLongTermSample(Value: Single);
var
  Loudness    : Single;
  LLRec       : PLinkedLoudnessRecord;
  LinkPointer : PPLinkedLoudnessRecord;
begin
 // calculate loudness
 Loudness := -0.691 + 10 * FastLog10ContinousError3(Value);

 // check for absolute threshold
 if Loudness < -70 then Exit;

 // allocate memory for loudness record
 GetMem(LLRec, SizeOf(TLinkedLoudnessRecord));

 // set loudness / Value
 LLRec^.Loudness := Loudness;
 LLRec^.Value := Value;

 // search for matching linked loudness position
 LinkPointer := @FLinkedLoudness;

 // update total sum
 FAbsoluteGatedValue := FAbsoluteGatedValue + Value;
 Inc(FAbsoluteGatedCount);

 // insert loudness in ordered list
 repeat
  if LinkPointer^ = nil then
   begin
    LinkPointer^ := LLRec;
    LLRec^.Next := nil;
    Exit;
   end else
  if (LinkPointer^)^.Loudness < Loudness then
   begin
    LLRec.Next := LinkPointer^;
    LinkPointer^ := LLRec;
    Exit;
   end;
  LinkPointer := @((LinkPointer^)^.Next);
 until False;
end;

procedure TMonoR128.ProcessSample(Value: Single);
var
  Value400ms   : Single;
  CurrentValue : Single;
begin
 // pre filter input
 CurrentValue := FRLBFilter.ProcessSample32(FPreFilter.ProcessSample32(Value));

 // calculate momentary integration (400 ms)
 Value400ms := FDelayLine400ms.ProcessSample32(CurrentValue);
 FMomIntValue := FMomIntValue + Sqr(CurrentValue) - Sqr(Value400ms);
 if FMomIntValue < CMeanSquareBias
  then FMomIntValue := CMeanSquareBias;
 FMomIntSum := FMomIntValue * FMomIntScale;

 // calculate short integration (3 seconds)
 FShortIntValue := FShortIntValue + Sqr(CurrentValue) -
   Sqr(FDelayLine2600ms.ProcessSample32(Value400ms));
 FShortIntSum := FShortIntValue * FShortIntScale;

 // eventually process long term sample
 Inc(FSampleCount);
 if FSampleCount >= FOverlapSamples then
  begin
   FSampleCount := 0;
   if FIsRunning
    then ProcessLongTermSample(FMomIntValue * FMomIntScale);
  end;

 // eventually increase total number of samples
 if FIsRunning then Inc(FTotalSamples);

 // override peak hold if necessary
 if FMomIntSum > FPeakHold
  then FPeakHold := FMomIntSum;
end;

procedure TMonoR128.ResetPeak;
begin
 inherited;

 FAbsoluteGatedValue := 0;
 FAbsoluteGatedCount := 0;
end;

procedure TMonoR128.SampleRateChanged;
begin
 inherited;

 if Abs(Self.SampleRate) > 0 then
  begin
   F400msSampleCount := Round(0.4 * Abs(SampleRate));
   F2600msSampleCount := Round(2.6 * Abs(SampleRate));

   FMomIntScale := 1 / F400msSampleCount;
   FShortIntScale := 1 / (F400msSampleCount + F2600msSampleCount);

   FPreFilter.SampleRate := Abs(SampleRate);
   FRLBFilter.SampleRate := Abs(SampleRate);
   FDelayLine400ms.BufferSize := F400msSampleCount;
   FDelayLine2600ms.BufferSize := F2600msSampleCount;

   FOverlapSamples := F400msSampleCount div 4;
  end;
end;









{ TStereoR128 }

constructor TStereoR128.Create;
var
  ChannelIndex : Integer;
begin
 inherited;

 // create classes
 for ChannelIndex := 0 to 1 do
  begin
   FPreFilter[ChannelIndex] := TBasicHighShelfFilter.Create;
   with FPreFilter[ChannelIndex] do
    begin
     if Abs(Self.SampleRate) > 0
      then SampleRate := Self.SampleRate;
     Frequency := 1500;
     Gain := 4;
     Bandwidth := 1.8;
    end;
   FRLBFilter[ChannelIndex] := TBasicLowcutFilter.Create;
   with FRLBFilter[ChannelIndex] do
    begin
     if Abs(Self.SampleRate) > 0
      then SampleRate := Self.SampleRate;
     Frequency := 50;
     Bandwidth := 1.8;
    end;

   FDelayLine400ms[ChannelIndex] := TDelayLineSamples32.Create(F400msSampleCount);
   FDelayLine2600ms[ChannelIndex] := TDelayLineSamples32.Create(F2600msSampleCount);

   FShortIntValue[ChannelIndex] := 0;
   FMomIntValue[ChannelIndex] := 0;
  end;
end;

destructor TStereoR128.Destroy;
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to 1 do
  begin
   FreeAndNil(FPreFilter[ChannelIndex]);
   FreeAndNil(FRLBFilter[ChannelIndex]);
   FreeAndNil(FDelayLine400ms[ChannelIndex]);
   FreeAndNil(FDelayLine2600ms[ChannelIndex]);
  end;
 inherited;
end;

procedure TStereoR128.ClearLinkedLoudness;
var
  ChannelIndex       : Integer;
  LinkedLoudness     : PLinkedLoudnessRecord;
  ThisLinkedLoudness : PLinkedLoudnessRecord;
begin
 for ChannelIndex := 0 to 1 do
  begin
   LinkedLoudness := FLinkedLoudness[ChannelIndex];
   FLinkedLoudness[ChannelIndex] := nil;

   while Assigned(LinkedLoudness) do
    begin
     ThisLinkedLoudness := LinkedLoudness;
     LinkedLoudness := LinkedLoudness.Next;
     FreeMem(ThisLinkedLoudness);
    end;
  end;
end;

function TStereoR128.GetLoudnessIntegration: Single;
var
  ChannelIndex : Integer;
  Value        : array [0..1] of Single;
  LLRec        : PLinkedLoudnessRecord;
  Loudness     : Single;
  ItemCount    : Integer;
begin
 // calculate integrated loudness level
 if FAbsoluteGatedCount[0] = 0
  then Value[0] := CMeanSquareBias
  else Value[0] := FAbsoluteGatedValue[0] / FAbsoluteGatedCount[0];
 if FAbsoluteGatedCount[1] = 0
  then Value[1] := CMeanSquareBias
  else Value[1] := FAbsoluteGatedValue[1] / FAbsoluteGatedCount[1];
 Loudness := -0.691 + 10 * FastLog10ContinousError3(Value[0] + Value[1]) - 8;

 // calculate integrated loudness level
 for ChannelIndex := 0 to 1 do
  begin
   ItemCount := 0;
   Value[ChannelIndex] := 0;
   LLRec := FLinkedLoudness[ChannelIndex];
   while Assigned(LLRec) do
    begin
     if LLRec^.Loudness < Loudness
      then Break;

     Inc(ItemCount);
     Value[ChannelIndex] := Value[ChannelIndex] + LLRec^.Value;
     LLRec := LLRec^.Next;
    end;
   if ItemCount = 0
    then Value[ChannelIndex] := CMeanSquareBias
    else Value[ChannelIndex] := Value[ChannelIndex] / ItemCount;
  end;

 Result := FUnitOffset - 0.691 + 10 * FastLog10ContinousError3(Value[0] + Value[1]);
end;

procedure TStereoR128.ProcessLongTermSample(const Index: Integer;
  Value: Single);
var
  Loudness    : Single;
  LLRec       : PLinkedLoudnessRecord;
  LinkPointer : PPLinkedLoudnessRecord;
begin
 // calculate loudness
 Loudness := -0.691 + 10 * FastLog10ContinousError3(Value);

 // check for absolute threshold
 if Loudness < -70 then Exit;

 // allocate memory for loudness record
 GetMem(LLRec, SizeOf(TLinkedLoudnessRecord));

 // set loudness / Value
 LLRec^.Loudness := Loudness;
 LLRec^.Value := Value;

 // search for matching linked loudness position
 LinkPointer := @FLinkedLoudness[Index];

 // update total sum
 FAbsoluteGatedValue[Index] := FAbsoluteGatedValue[Index] + Value;
 Inc(FAbsoluteGatedCount[Index]);

 // insert loudness in ordered list
 repeat
  if LinkPointer^ = nil then
   begin
    LinkPointer^ := LLRec;
    LLRec^.Next := nil;
    Exit;
   end else
  if (LinkPointer^)^.Loudness < Loudness then
   begin
    LLRec.Next := LinkPointer^;
    LinkPointer^ := LLRec;
    Exit;
   end;
  LinkPointer := @((LinkPointer^)^.Next);
 until False;
end;

procedure TStereoR128.ProcessMono(Value: Single);
var
  CurrentValue : Single;
  Value400ms   : Single;
begin
 // preprocess left channel
 CurrentValue := FRLBFilter[0].ProcessSample32(
   FPreFilter[0].ProcessSample32(Value));

 // calculate momentary integration (400 ms)
 Value400ms := FDelayLine400ms[0].ProcessSample32(CurrentValue);
 FMomIntValue[0] := FMomIntValue[0] + Sqr(CurrentValue) - Sqr(Value400ms);
 if FMomIntValue[0] < CMeanSquareBias
  then FMomIntValue[0] := CMeanSquareBias;

 // calculate short integration (3 seconds)
 FShortIntValue[0] := FShortIntValue[0] + Sqr(CurrentValue) -
   Sqr(FDelayLine2600ms[0].ProcessSample32(Value400ms));
 if FShortIntValue[0] < CMeanSquareBias
  then FShortIntValue[0] := CMeanSquareBias;

 // calculate sum
 FMomIntSum := FMomIntValue[0] * FMomIntScale;
 FShortIntSum := FShortIntValue[0] * FShortIntScale;

 // eventually process long term sample
 Inc(FSampleCount);
 if FSampleCount >= FOverlapSamples then
  begin
   FSampleCount := 0;
   if FIsRunning
    then ProcessLongTermSample(0, FMomIntValue[0] * FMomIntScale);
  end;

 // eventually increase total number of samples
 if FIsRunning then Inc(FTotalSamples);

 // override peak hold if necessary
 if FMomIntSum > FPeakHold
  then FPeakHold := FMomIntSum;
end;

procedure TStereoR128.ProcessStereo(Left, Right: Single);
var
  CurrentValue : Single;
  Value400ms   : Single;
begin
 // preprocess left channel
 CurrentValue := FRLBFilter[0].ProcessSample32(
   FPreFilter[0].ProcessSample32(Left));

 // calculate momentary integration (400 ms)
 Value400ms := FDelayLine400ms[0].ProcessSample32(CurrentValue);
 FMomIntValue[0] := FMomIntValue[0] + Sqr(CurrentValue) - Sqr(Value400ms);
 if FMomIntValue[0] < CMeanSquareBias
  then FMomIntValue[0] := CMeanSquareBias;

 // calculate short integration (3 seconds)
 FShortIntValue[0] := FShortIntValue[0] + Sqr(CurrentValue) -
   Sqr(FDelayLine2600ms[0].ProcessSample32(Value400ms));
 if FShortIntValue[0] < CMeanSquareBias
  then FShortIntValue[0] := CMeanSquareBias;

 // preprocess right channel
 CurrentValue := FRLBFilter[1].ProcessSample32(
   FPreFilter[1].ProcessSample32(Right));

 // calculate momentary integration (400 ms)
 Value400ms := FDelayLine400ms[1].ProcessSample32(CurrentValue);
 FMomIntValue[1] := FMomIntValue[1] + Sqr(CurrentValue) - Sqr(Value400ms);
 if FMomIntValue[1] < CMeanSquareBias
  then FMomIntValue[1] := CMeanSquareBias;

 // calculate short integration (3 seconds)
 FShortIntValue[1] := FShortIntValue[1] + Sqr(CurrentValue) -
   Sqr(FDelayLine2600ms[1].ProcessSample32(Value400ms));
 if FShortIntValue[1] < CMeanSquareBias
  then FShortIntValue[1] := CMeanSquareBias;

 // calculate sum
 FMomIntSum := (FMomIntValue[0] + FMomIntValue[1]) * FMomIntScale;
 FShortIntSum := (FShortIntValue[0] + FShortIntValue[1]) * FShortIntScale;

 // eventually process long term sample
 Inc(FSampleCount);
 if FSampleCount >= FOverlapSamples then
  begin
   FSampleCount := 0;
   if FIsRunning then
    begin
     ProcessLongTermSample(0, FMomIntValue[0] * FMomIntScale);
     ProcessLongTermSample(1, FMomIntValue[1] * FMomIntScale);
    end;
  end;

 // eventually increase total number of samples
 if FIsRunning then Inc(FTotalSamples);

 // override peak hold if necessary
 if FMomIntSum > FPeakHold
  then FPeakHold := FMomIntSum;
end;

procedure TStereoR128.ResetPeak;
begin
 inherited;

 FAbsoluteGatedValue[0] := 0;
 FAbsoluteGatedValue[1] := 0;
 FAbsoluteGatedCount[0] := 0;
 FAbsoluteGatedCount[1] := 0;
end;

procedure TStereoR128.SampleRateChanged;
var
  ChannelIndex : Integer;
begin
 inherited;

 if Abs(Self.SampleRate) > 0 then
  begin
   F400msSampleCount := Round(0.4 * Abs(SampleRate));
   F2600msSampleCount := Round(2.6 * Abs(SampleRate));

   FMomIntScale := 1 / F400msSampleCount;
   FShortIntScale := 1 / (F400msSampleCount + F2600msSampleCount);

   for ChannelIndex := 0 to 1 do
    begin
     FPreFilter[ChannelIndex].SampleRate := Abs(SampleRate);
     FRLBFilter[ChannelIndex].SampleRate := Abs(SampleRate);
     FDelayLine400ms[ChannelIndex].BufferSize := F400msSampleCount;
     FDelayLine2600ms[ChannelIndex].BufferSize := F2600msSampleCount;
    end;

   FOverlapSamples := F400msSampleCount div 4;
  end;
end;

end.
