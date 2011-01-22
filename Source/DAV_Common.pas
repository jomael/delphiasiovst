unit DAV_Common;

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
//  The initial developer of this code is Tobias Fleischer and                //
//  Christian-W. Budde                                                        //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2003-2010        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

{$IFDEF FPC}
uses LCLIntf, DAV_Types; {$DEFINE PUREPASCAL}
{$ELSE}
uses
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  {$IFDEF UseNativeTypes}Types, {$ENDIF}
  DAV_Types;
{$ENDIF}

{ Byte Ordering }

function Swap16(Value: SmallInt): SmallInt;
function Swap32(Value: LongInt): LongInt;
function Swap64(Value: Int64): Int64;
function Swap80(Value: Extended): Extended;

procedure Flip16(var Value);
procedure Flip32(var Value);
procedure Flip64(var Value);
procedure Flip80(var Value);


{ Convert }

function ms2Samples(const ms, SampleRate: Single): Single; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function Samples2ms(const Samples, SampleRate: Single): Single; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function Sync2Samples(const SyncFactor, BPM, SampleRate: Single): Integer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function GetSyncFactor(const BaseFactor: Single; const Dotted, Triads: Boolean): Single; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function Compare4(S1, S2 : PAnsiChar): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

function FrequencyToBark(Frequency: Single): Single; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FrequencyToBark(Frequency: Double): Double; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function Frequency2CriticalBandwidth(Frequency: Single): Single; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function Frequency2CriticalBandwidth(Frequency: Double): Double; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

function GermaniumDiode(Voltage: Double): Double;
function SiliconDiode(Voltage: Double): Double;
procedure Exchange8(var ValueA, ValueB);
procedure Exchange16(var ValueA, ValueB);
procedure Exchange32(var ValueA, ValueB);

// dB stuff
function dB_to_Amp(const Value: Single): Single; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function dB_to_Amp(const Value: Double): Double; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function SqrAmp2dB(const Value: Single): Single; overload;
function SqrAmp2dB(const Value: Double): Double; overload;
function Amp_to_dB(const Value: Single): Single; overload;
function Amp_to_dB(const Value: Double): Double; overload;
{$IFNDEF FPC}
procedure Amp_to_dB(var v: TDAV4SingleArray); overload; // TODO: move to VectorMath!
{$ENDIF}

// scale logarithmically from 20 Hz to 20 kHz
function FreqLinearToLog(const Value: Single): Single; overload;
function FreqLinearToLog(const Value: Double): Double; overload;
function FreqLogToLinear(const Value: Single): Single; overload;
function FreqLogToLinear(const Value: Double): Double; overload;

function ScaleLinearToLog(const Value: Single; const Min, Max: Single): Single; overload;
function ScaleLinearToLog(const Value: Double; const Min, Max: Double): Single; overload;
function ScaleLogToLinear(const Value: Single; const Min, Max: Single): Single; overload;
function ScaleLogToLinear(const Value: Double; const Min, Max: Double): Double; overload;


{ Limit & Clip, Min & Max }

function Limit(const Value: Single; Lower: Single = -1; Upper: Single = 1): Single; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function Limit(const Value: Double; Lower: Double = -1; Upper: Double = 1): Double; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function Limit(const Value: Integer; Lower: Integer = 0; Upper: Integer = 1): Integer; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function RoundLimit(const Value: Single; Lower: Integer = 0; Upper: Integer = 1): Integer; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function RoundLimit(const Value: Double; Lower: Integer = 0; Upper: Integer = 1): Integer; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function IntLimit(const Value: Integer; Lower: Integer = 0; Upper: Integer = 1): Integer; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function BranchlessClip(const Value, Lower, Upper: Single): Single; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function BranchlessClip(const Value, Lower, Upper: Double): Double; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function BranchlessClipLower(Value: Single; const Lower: Single): Single; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function BranchlessClipLower(Value: Double; const Lower: Double): Double; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function BranchlessClipUpper(Value: Single; const Upper: Single): Single; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function BranchlessClipUpper(Value: Double; const Upper: Double): Double; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function BranchlessClipPositive(Value: Single): Single; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function BranchlessClipPositive(Value: Double): Double; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function BranchlessClipNegative(Value: Single): Single; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function BranchlessClipNegative(Value: Double): Double; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
procedure WrapInt(var Value: Integer; Upper: Integer; Lower: Integer = 0);
function Smallest(const A, B: Single): Single; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function Smallest(const A, B: Double): Double; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function Largest(const A, B: Single): Single; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function Largest(const A, B: Double): Double; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function LimitAngle(const Angle: Single): Single; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function LimitAngle(const Angle: Double): Double; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

function FastFractional(const Value: Single): Single; overload;
function FastFractional(const Value: Double): Double; overload;
procedure FastAbs(var Value: Single); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure FastAbs(var Value: Double); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure FastAbs(var Value: TDAV4SingleArray); overload;
procedure FastNegative(var Value: Single); overload;
function FastSgn(const Value: Single): Integer;
function FastMin(const A, B: Single) : Single;
function FastMax(const A, B: Single) : Single;
function FastMod(const Arg1, Arg2: Single): Single; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

{$IFNDEF FPC}
function LaurentRoundInt(const Value: Single): Integer; overload;
function LaurentRoundInt(const Value: Double): Integer; overload;
procedure LaurentRoundInt(Input: PSingle; Output:PInteger; SampleFrames: Integer); overload;
function LaurentFastFloor(const Value: Single): Integer; overload;
function LaurentFastFloor(const Value: Double): Integer; overload;
procedure LaurentFastFloor(Input: PSingle; Output:PInteger; SampleFrames: Integer); overload;
function LaurentFastCeil(const Value: Single): Integer; overload;
function LaurentFastCeil(const Value: Double): Integer; overload;
procedure LaurentFastCeil(Input: PSingle; Output:PInteger; SampleFrames: Integer); overload;
function LaurentFastTrunc(const Value: Single): Integer; overload;
function LaurentFastTrunc(const Value: Double): Integer; overload;
function FastRound(Sample: Single): Integer; overload;
function FastRound(Sample: Double): Integer; overload;
{$ENDIF}

function OnOff(const Value: Single): Boolean;
function unDenormalize(const Value: Single): Single;
procedure DontRaiseExceptionsAndSetFPUcodeword;

{ String Stuff & Messages }

{$IFNDEF FPC}
{$IFDEF MSWINDOWS}
function GetApplicationFilename: string; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function GetApplicationDirectory: string; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
{$ENDIF}

{$IFNDEF DELPHI12_UP}
type
  TSysCharSet = set of AnsiChar;

function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

{$IFDEF MSWINDOWS}
procedure Msg(b: Boolean); overload;
procedure Msg(m: string; m2: string = ''); overload;
procedure Msg(i: Integer); overload;
procedure Msg(s: Single); overload;
procedure Msg(m: string; i: Integer); overload;
{$ENDIF}
{$ENDIF}

function SplitString(S: String; Delimiter: AnsiChar): TStrArray;
function MakeGoodFileName(s: string): string;
{$ENDIF}

function FloatWithUnit(const Value: Double): string;
function FloatToString(Value: Extended; Digits: Integer = -1): string;
function FloatToAnsiString(Value: Extended; Digits: Integer = -1): AnsiString;

{$IFDEF MSWINDOWS}
{$IFDEF CPU386}
function MethodToProcedure(Self: TObject; MethodAddr: Pointer): Pointer; overload;
function MethodToProcedure(Method: TMethod): Pointer; overload;
{$ENDIF}
{$ENDIF}

const
  CDenorm32          : Single = 1E-24;
  CTwoPI32           : Single = 2 * Pi;
  CFourPI32          : Single = 4 * Pi;
  CHalf32            : Single = 0.5;
  CQuarter32         : Single = 0.25;
  CTen32             : Single = 10;
  CTwenty32          : Single = 20;
  COneTwelfth32      : Single = 1 / 12;
  CMinusOneSixteenth : Single = -0.0625;
  CTwoMulTwo2Neg32   : Single = ((2.0/$10000) / $10000);  // 2^-32

  CDenorm64          : Double = 1E-34;
  CTwoPI64           : Double = 2 * Pi;
  CFourPI64          : Double = 4 * Pi;
  CHalf64            : Double = 0.5;
  CQuarter64         : Double = 0.25;
  CTen64             : Double = 10;
  CTwenty64          : Double = 20;

  CMaxLongInt        : Integer =  $7FFFFFFF;
  CMinLongInt        : Integer = -$7FFFFFFF - 1;
  CMaxInt64          : Int64 =  9223372036854775807;
  CMinInt64          : Int64 = -9223372036854775807 - 1;
  CMaxSingle         : Single = 3.40282346638528860e+38;
  CMinusHalf32       : Single = -0.5;

implementation

uses
  Math, SysUtils;

{ Byte Ordering }

type
  T16Bit = record
    case Integer of
      0 :  (v: SmallInt);
      1 :  (b: array[0..1] of Byte);
  end;

  T32Bit = record
    case Integer of
      0 :  (v: LongInt);
      1 :  (b: array[0..3] of Byte);
  end;

  T64Bit = record
    case Integer of
      0 :  (v: Int64);
      1 :  (b: array[0..7] of Byte);
  end;

  T80Bit = record
    case Integer of
      0 :  (v: Extended);
      1 :  (b: array[0..9] of Byte);
  end;

function Swap16(Value: SmallInt): SmallInt;
var
  t: Byte;
begin
 with T16Bit(Value) do
  begin
   t := b[0];
   b[0] := b[1];
   b[1] := t;
   Result := v;
  end;
end;

function Swap32(Value: LongInt): LongInt;
var
  Temp: Byte;
begin
 with T32Bit(Value) do
  begin
   Temp := b[0];
   b[0] := b[3];
   b[3] := Temp;
   Temp := b[1];
   b[1] := b[2];
   b[2] := Temp;
   Result := v;
  end;
end;

function Swap64(Value: Int64): Int64;
var
  Temp: Byte;
begin
 with T64Bit(Value) do
  begin
   Temp := b[0];
   b[0] := b[7];
   b[7] := Temp;
   Temp := b[1];
   b[1] := b[6];
   b[6] := Temp;
   Temp := b[2];
   b[2] := b[5];
   b[5] := Temp;
   Temp := b[3];
   b[3] := b[4];
   b[4] := Temp;
   Result := v;
  end;
end;

function Swap80(Value: Extended): Extended;
var
  Temp: Byte;
  T80B: T80Bit absolute Value;
begin
 with T80B do
  begin
   Temp := b[0];
   b[0] := b[9];
   b[9] := Temp;
   Temp := b[1];
   b[1] := b[8];
   b[8] := Temp;
   Temp := b[2];
   b[2] := b[7];
   b[7] := Temp;
   Temp := b[3];
   b[3] := b[6];
   b[6] := Temp;
   Temp := b[4];
   b[4] := b[5];
   b[5] := Temp;
   Result := V;
  end;
end;

procedure Flip16(var Value);
var
  t: Byte;
begin
 with T16Bit(Value) do
  begin
   t := b[0];
   b[0] := b[1];
   b[1] := t;
  end;
end;

procedure Flip32(var Value);
var
  Temp: Byte;
begin
 with T32Bit(Value) do
  begin
   Temp := b[0];
   b[0] := b[3];
   b[3] := Temp;
   Temp := b[1];
   b[1] := b[2];
   b[2] := Temp;
  end;
end;

procedure Flip64(var Value);
var
  Temp: Byte;
begin
 with T64Bit(Value) do
  begin
   Temp := b[0];
   b[0] := b[7];
   b[7] := Temp;
   Temp := b[1];
   b[1] := b[6];
   b[6] := Temp;
   Temp := b[2];
   b[2] := b[5];
   b[5] := Temp;
   Temp := b[3];
   b[3] := b[4];
   b[4] := Temp;
  end;
end;

procedure Flip80(var Value);
var
  Temp: Byte;
  T80B: T80Bit absolute Value;
begin
 with T80B do
  begin
   Temp := b[0];
   b[0] := b[9];
   b[9] := Temp;
   Temp := b[1];
   b[1] := b[8];
   b[8] := Temp;
   Temp := b[2];
   b[2] := b[7];
   b[7] := Temp;
   Temp := b[3];
   b[3] := b[6];
   b[6] := Temp;
   Temp := b[4];
   b[4] := b[5];
   b[5] := Temp;
  end;
end;


{ Convert }

function ms2Samples(const ms, SampleRate: Single): Single;
begin
 Result := ms * SampleRate * 0.001;
end;

function Samples2ms(const Samples, SampleRate: Single): Single;
begin
 Result := Samples * 1000 / SampleRate;
end;

function Sync2Samples(const SyncFactor, BPM, SampleRate: Single): Integer;
begin
 Result := Round(SyncFactor * SampleRate * 60 / BPM);
end;

function GetSyncFactor(const BaseFactor: Single; const Dotted, Triads: Boolean): Single;
begin
 Result := BaseFactor;
 if Dotted then Result := Result * 1.5;
 if Triads then Result := Result / 3;
end;

function Compare4(S1, S2: PAnsiChar): Boolean;
var
  i, Diff : Byte;
begin
 Result := False;
 for i := 0 to 3 do
  begin
   Diff := Byte(S1[i]) - Byte(S2[i]);
   if not (Diff in [0, 32, 224]) then Exit;
  end;
 Result := True;
end;


////////////////////////////////////////////////////////////////////////
//                                                                    //
// see for example "Zwicker: Psychoakustik, 1982; ISBN 3-540-11401-7  //
//                                                                    //
// input: freq in hz                                                  //
// output: barks                                                      //
//                                                                    //
////////////////////////////////////////////////////////////////////////

function FrequencyToBark(Frequency: Single): Single;
begin
 if (Frequency < 0) then Frequency := 0;
 Frequency := Frequency * 0.001;
 Result := 13.0 * arctan(0.76 * Frequency) +
   3.5 * arctan(sqr(Frequency * 0.1333333333333333));
end;

function FrequencyToBark(Frequency: Double): Double;
begin
 if (Frequency < 0) then Frequency := 0;
 Frequency := Frequency * 0.001;
 Result := 13.0 * arctan(0.76 * Frequency) +
   3.5 * arctan(sqr(Frequency / 7.5));
end;


////////////////////////////////////////////////////////////////////////
//                                                                    //
// see for example "Zwicker: Psychoakustik, 1982; ISBN 3-540-11401-7  //
//                                                                    //
// input: freq in hz                                                  //
// output: critical band width                                        //
//                                                                    //
////////////////////////////////////////////////////////////////////////

function Frequency2CriticalBandwidth(Frequency: Single): Single;
begin
 Result := 25 + 75 * Power(1 + 1.4 * sqr(Frequency * 0.001), 0.69);
end;

function Frequency2CriticalBandwidth(Frequency: Double): Double;
begin
 Result := 25 + 75 * Power(1 + 1.4 * sqr(Frequency * 0.001), 0.69);
end;


function GermaniumDiode(Voltage: Double): Double;
begin
 Result := 0.085 * (Voltage + Abs(Voltage)) * sqr(Voltage) * Voltage
end;

function SiliconDiode(Voltage: Double): Double;
begin
 Result := 40.6728602E-9 * (exp(17.7493332 * (Voltage + 0.3)) - 1);
end;

procedure Exchange8(var ValueA, ValueB);
var
  Temp : Byte;
begin
 Temp := Byte(ValueA);
 Byte(ValueA) := Byte(ValueB);
 Byte(ValueB) := Temp;
end;

procedure Exchange16(var ValueA, ValueB);
var
  Temp : Word;
begin
 Temp := Word(ValueA);
 Word(ValueA) := Word(ValueB);
 Word(ValueB) := Temp;
end;

procedure Exchange32(var ValueA, ValueB);
var
  Temp : Integer;
begin
 Temp := Integer(ValueA);
 Integer(ValueA) := Integer(ValueB);
 Integer(ValueB) := Temp;
end;


////////////////////////////////////////////////////
//                                                //
// Convert a value in dB's to a linear amplitude  //
//                                                //
////////////////////////////////////////////////////

function dB_to_Amp(const Value: Single): Single;
begin
 if (Value > -400.0)
  then Result := Exp(Value * 0.11512925464970228420089957273422) //Power(10, g / 20) //Power(2, g * 0.015051499783199059760686944736225)
  else Result := 0;
end;

function dB_to_Amp(const Value: Double): Double;
begin
 if (Value > -1000.0)
  then Result := Exp(Value * 0.11512925464970228420089957273422) //Power(10, g / 20) //Power(2, g * 0.015051499783199059760686944736225)
  else Result := 0;
end;                                                             // e^(x) = 2^(log2(e^x)) = 2^(x / ln(2))


///////////////////////////////////////////////////////////
//                                                       //
// Convert a squared value in dB's to a linear amplitude //
//                                                       //
///////////////////////////////////////////////////////////

function SqrAmp2dB(const Value: Single): Single;
{$IFDEF PUREPASCAL}
begin
 Result := 10 * Log10(Value);
end;
{$ELSE}
asm
 fldlg2
 fld Value
 fyl2x
 fmul CTen32
end;
{$ENDIF}

function SqrAmp2dB(const Value: Double): Double;
{$IFDEF PUREPASCAL}
begin
 Result := 10 * Log10(Value);
end;
{$ELSE}
asm
 fldlg2
 fld Value
 fyl2x
 fmul CTen64
end;
{$ENDIF}

function Amp_to_dB(const Value: Single): Single;
{$IFDEF PUREPASCAL}
begin
 Result := CTwenty32 * Log10(Value);
end;
{$ELSE}
asm
 fldlg2
 fld Value
 fyl2x
 fmul CTwenty32
end;
{$ENDIF}

function Amp_to_dB(const Value: Double): Double;
{$IFDEF PUREPASCAL}
begin
 Result := CTwenty64 * Log10(Value);
end;
{$ELSE}
asm
 fldlg2
 fld Value
 fyl2x
 fmul CTwenty64
end;
{$ENDIF}

procedure Amp_to_dB(var v: TDAV4SingleArray);
{$IFDEF PUREPASCAL}
begin
 v[0] := Amp_to_dB(v[0]);
 v[1] := Amp_to_dB(v[1]);
 v[2] := Amp_to_dB(v[2]);
 v[3] := Amp_to_dB(v[3]);
end;
{$ELSE}
asm
 fldlg2
 fld    [eax].Single
 fyl2x
 fmul   CTwenty32.Double
 fstp   [eax].Single
 fldlg2
 fld    [eax + 4].Single
 fyl2x
 fmul   CTwenty32.Double
 fstp   [eax + 4].Single
 fldlg2
 fld    [eax + 8].Single
 fyl2x
 fmul   CTwenty32.Double
 fstp   [eax + 8].Single
 fldlg2
 fld    [eax + 12].Single
 fyl2x
 fmul   CTwenty32.Double
 fstp   [eax + 12].Single
end;
{$ENDIF}


//////////////////////////////////////////////
//                                          //
// scale logarithmicly from 20 Hz to 20 kHz //
//                                          //
//////////////////////////////////////////////

function FreqLinearToLog(const Value: Single): Single;
{$IFDEF PUREPASCAL}
begin
 Result := CTwenty32 * Exp(value * 6.907755279);
end;
{$ELSE}
const
  fltl2: Single = 6.907755279;
asm
 fld     Value.Single
 fmul    fltl2
 fldl2e
 fmul
 fld     st(0)
 frndint
 fsub    st(1), st
 fxch    st(1)
 f2xm1
 fld1
 fadd
 fscale
 fstp    st(1)
 fmul CTwenty64.Double
end;
{$ENDIF}

function FreqLinearToLog(const Value: Double): Double;
{$IFDEF PUREPASCAL}
begin
 Result := CTwenty64 * Exp(value * 6.907755279);
end;
{$ELSE}
const
  fltl2: Double = 6.907755279;
asm
 fld     Value.Double
 fmul    fltl2
 fldl2e
 fmul
 fld     st(0)
 frndint
 fsub    st(1), st
 fxch    st(1)
 f2xm1
 fld1
 fadd
 fscale
 fstp    st(1)
 fmul CTwenty64.Double
end;
{$ENDIF}

function FreqLogToLinear(const Value: Single): Single;
const
  fltl1 : Single = 0.05;
  fltl2 : Single = 1.44764826019E-1;
{$IFDEF PUREPASCAL}
begin
 Result := ln(value * fltl1) * fltl2;
end;
{$ELSE}
asm
 fldln2
 fld Value.Single
 fmul fltl1
 fyl2x
 fmul fltl2
end;
{$ENDIF}

function FreqLogToLinear(const Value: Double): Double;
const
  fltl1 : Double = 0.05;
  fltl2 : Double = 1.44764826019E-1;
{$IFDEF PUREPASCAL}
begin
 Result := ln(value * fltl1) * fltl2;
end;
{$ELSE}
asm
 fldln2
 fld Value.Double
 fmul fltl1
 fyl2x
 fmul fltl2
end;
{$ENDIF}

function ScaleLinearToLog(const Value: Single; const Min, Max: Single): Single;
begin
 Result := Min * Exp(Value * ln(Max / Min));
end;

function ScaleLinearToLog(const Value: Double; const Min, Max: Double): Single; overload;
begin
 Result := Min * Exp(Value * ln(Max / Min));
end;

function ScaleLogToLinear(const Value: Single; const Min, Max: Single): Single; overload;
begin
 Result := ln(Value / Min) / ln(Max / Min);
end;

function ScaleLogToLinear(const Value: Double; const Min, Max: Double): Double; overload;
begin
 Result := ln(Value / Min - Max / Min);
end;



{ Limit & Clip, Min & Max }

// Limit a Value to be Lower <= Value <= Upper
function Limit(const Value: Single; Lower: Single = -1; Upper: Single = 1): Single;
begin
 if Value < Lower then Result := Lower else
 if Value > Upper then Result := Upper else Result := Value;
end;

// Limit a Value to be Lower <= Value <= Upper
function Limit(const Value: Double; Lower: Double = -1; Upper: Double = 1): Double;
begin
 if Value < Lower then Result := Lower else
 if Value > Upper then Result := Upper else Result := Value;
end;

// Limit a Value to be Lower <= Value <= Upper
function Limit(const Value: Integer; Lower: Integer = 0; Upper: Integer = 1): Integer;
begin
 if Value < Lower then Result := Lower else
 if Value > Upper then Result := Upper else Result := Value;
end;

// Limit a Value to be Lower <= Value <= Upper
function RoundLimit(const Value: Single; Lower: Integer = 0; Upper: Integer = 1): Integer;
begin
 Result := Round(Value);
 if Result < Lower then Result := Lower else
 if Result > Upper then Result := Upper;
end;

// Limit a Value to be Lower <= Value <= Upper
function RoundLimit(const Value: Double; Lower: Integer = 0; Upper: Integer = 1): Integer;
begin
 Result := Round(Value);
 if Result < Lower then Result := Lower else
 if Result > Upper then Result := Upper;
end;

// Limit a Value to be Lower <= Value <= Upper
function IntLimit(const Value: Integer; Lower: Integer = 0; Upper: Integer = 1): Integer;
begin
 if Value < Lower then Result := Lower else
 if Value > Upper then Result := Upper else Result := Value;
end;

function BranchlessClip(const Value, Lower, Upper: Single): Single;
begin
 Result := (Abs(Value - Lower) + (Lower + Upper) - Abs(Value - Upper)) * 0.5;
end;

function BranchlessClip(const Value, Lower, Upper: Double): Double;
begin
 Result := (Abs(Value - Lower) + (Lower + Upper) - Abs(Value - Upper)) * 0.5;
end;

function BranchlessClipLower(Value: Single; const Lower: Single): Single;
begin
 Value := Value - Lower;
 Result := (Value + Abs(Value)) * 0.5 + Lower;
end;

function BranchlessClipLower(Value: Double; const Lower: Double): Double;
begin
 Value := Value - Lower;
 Result := (Value + Abs(Value)) * 0.5 + Lower;
end;

function BranchlessClipUpper(Value: Single; const Upper: Single): Single;
begin
 Value := Upper - Value;
 Result := Upper -(Value + Abs(Value)) * 0.5;
end;

function BranchlessClipUpper(Value: Double; const Upper: Double): Double;
begin
 Value := Upper - Value;
 Result := Upper -(Value + Abs(Value)) * 0.5;
end;

function BranchlessClipPositive(Value: Single): Single;
begin
 Result := (Value + Abs(Value)) * 0.5;
end;

function BranchlessClipPositive(Value: Double): Double;
begin
 Result := (Value + Abs(Value)) * 0.5;
end;

function BranchlessClipNegative(Value: Single): Single;
begin
 Result := (Abs(Value) - Value) * 0.5;
end;

function BranchlessClipNegative(Value: Double): Double;
begin
 Result := (Abs(Value) - Value) * 0.5;
end;

procedure WrapInt(var Value: Integer; Upper: Integer; Lower: Integer = 0);
begin
 while Value >= Upper do Value := Value - Upper;
 while Value <  Lower do Value := Value + Upper;
end;

function Smallest(const A, B: Single): Single;
begin
 if A < B
  then Result := A
  else Result := B;
end;

function Smallest(const A, B: Double): Double;
begin
 if A < B
  then Result := A
  else Result := B;
end;

function Largest(const A, B: Single): Single;
begin
 if A > B
  then Result := A
  else Result := B;
end;

function Largest(const A, B: Double): Double;
begin
 if A > B
  then Result := A
  else Result := B;
end;

function LimitAngle(const Angle: Single): Single;
begin
 Result := Angle;
 while Result <    0 do Result := Result + 360;
 while Result >= 360 do Result := Result - 360;
end;

function LimitAngle(const Angle: Double): Double;
begin
 Result := Angle;
 while Result <    0 do Result := Result + 360;
 while Result >= 360 do Result := Result - 360;
end;


{ Math }

function FastFractional(const Value: Single): Single;
{$IFDEF PUREPASCAL}
begin
 Result := Value - Round(Value - 0.5);
end;
{$ELSE}
var i : Integer;
asm
 fld Value.Single
 fld Value.Single
 fsub CHalf64
 frndint
 fsubp
end;
{$ENDIF}

function FastFractional(const Value: Double): Double;
{$IFDEF PUREPASCAL}
begin
 Result := Value - Round(Value - 0.5);
end;
{$ELSE}
var i : Integer;
asm
 fld Value.Double
 fld Value.Double
 fsub CHalf64
 frndint
 fsubp
end;
{$ENDIF}

procedure FastAbs(var Value: Single);
var
  i : Integer absolute Value;
begin
 i := i and $7FFFFFFF;
end;

procedure FastAbs(var Value: Double);
var
  i : array [0..1] of Integer absolute Value;
begin
 i[0] := i[0] and $7FFFFFFF;
end;

procedure FastAbs(var Value: TDAV4SingleArray); overload;
{$IFDEF PUREPASCAL}
var
  i : Array [0..3] of Integer absolute Value;
begin
 i[0] := i[0] and $7FFFFFFF;
 i[1] := i[1] and $7FFFFFFF;
 i[2] := i[2] and $7FFFFFFF;
 i[3] := i[3] and $7FFFFFFF;
end;
{$ELSE}
asm
 fld  [eax].Single
 fabs
 fstp [eax].Single
 fld  [eax +  4].Single
 fabs
 fstp [eax +  4].Single
 fld  [eax +  8].Single
 fabs
 fstp [eax +  8].Single
 fld  [eax + 12].Single
 fabs
 fstp [eax + 12].Single
end;
{$ENDIF}

procedure FastNegative(var Value: Single);
var
  i : Integer absolute Value;
const
  CBitMask = $80000000;
begin
 i := Cardinal((@Value)^) xor CBitMask;
end;

function FastMod(const Arg1, Arg2: Single): Single;
var
  Norm : Single;
begin
 Norm := Arg1 / Arg2;
 Result := (Norm - round(Norm - 0.5)) * Arg2
end;


// LaurentRoundInt

function LaurentRoundInt(const Value: Single): Integer; overload;
{$IFDEF PUREPASCAL}
begin
 Result := Round(Value);
end;
{$ELSE}
asm
 fld   Value.Single
 fadd  st(0), st(0)
 fadd  CHalf32
 fistp Result.Integer
 sar   Result.Integer, 1
end;
{$ENDIF}

function LaurentRoundInt(const Value: Double): Integer; overload;
{$IFDEF PUREPASCAL}
begin
 Result := Round(Value);
end;
{$ELSE}
asm
 fld   Value.Double
 fadd  st(0), st(0)
 fadd  CHalf32
 fistp Result.Integer
 sar   Result.Integer, 1
end;
{$ENDIF}

procedure LaurentRoundInt(Input: PSingle; Output: PInteger; SampleFrames: Integer); overload;
{$IFDEF PUREPASCAL}
var
  i : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   Output^ := Round(Input^);
   Inc(Output);
   Inc(Input);
  end;
end;
{$ELSE}
asm
 @Start:
 fld   [eax].Single
 fadd  st(0), st(0)
 fadd  CHalf32
 fistp [edx].Integer
 sar   [edx].Integer, 1
 add   eax,4
 add   edx,4
 loop  @Start
end;
{$ENDIF}

function LaurentFastFloor(const Value: Single): Integer; overload;
{$IFDEF PUREPASCAL}
begin
 Result := Floor(Value);
end;
{$ELSE}
asm
 fld   Value.Single
 fadd  st(0), st(0)
 fsub  CHalf32
 fistp Result.Integer
 sar   Result.Integer, 1
end;
{$ENDIF}

function LaurentFastFloor(const Value: Double): Integer; overload;
{$IFDEF PUREPASCAL}
begin
 Result := Floor(Value);
end;
{$ELSE}
asm
 fld   Value.Double
 fadd  st(0), st(0)
 fsub  CHalf32
 fistp Result.Integer
 sar   Result.Integer, 1
end;
{$ENDIF}

procedure LaurentFastFloor(Input: PSingle; Output: PInteger; SampleFrames: Integer); overload;
{$IFDEF PUREPASCAL}
var
  i : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   Output^ := Floor(Input^);
   Inc(Output);
   Inc(Input);
  end;
end;
{$ELSE}
asm
 @Start:
 fld [eax].Single
 fadd  st(0), st(0)
 fsub  CHalf32
 fistp [edx].Integer
 sar   [edx].Integer, 1
 add eax,4
 add edx,4
 loop    @Start
end;
{$ENDIF}

// LaurentFastCeil

function LaurentFastCeil(const Value: Single): Integer; overload;
{$IFDEF PUREPASCAL}
begin
 Result := Ceil(Value);
end;
{$ELSE}
asm
 fld   Value.Single
 fadd  st(0), st(0)
 fsubr CMinusHalf32
 fistp Result.Integer
 sar   Result.Integer, 1
 neg   Result.Integer
end;
{$ENDIF}

function LaurentFastCeil(const Value: Double): Integer; overload;
{$IFDEF PUREPASCAL}
begin
 Result := Ceil(Value);
end;
{$ELSE}
asm
 fld   Value.Double
 fadd  st(0), st(0)
 fsubr CMinusHalf32
 fistp Result.Integer
 sar   Result.Integer, 1
 neg   Result.Integer
end;
{$ENDIF}

procedure LaurentFastCeil(Input: PSingle; Output: PInteger; SampleFrames: Integer); overload;
{$IFDEF PUREPASCAL}
var
  i : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   Output^ := Ceil(Input^);
   Inc(Output);
   Inc(Input);
  end;
end;
{$ELSE}
asm
 @Start:
 fld   [eax].Single
 fadd  st(0), st(0)
 fsubr CMinusHalf32
 fistp [edx].Integer
 sar   [edx].Integer, 1
 neg   [edx].Integer
 add   eax, 4
 add   edx, 4
 loop  @Start
end;
{$ENDIF}

// Laurent Fast Trunc

function LaurentFastTrunc(const Value: Single): Integer; overload;
{$IFDEF PUREPASCAL}
begin
 Result := Trunc(Value);
end;
{$ELSE}
var
  IntCast : Integer absolute Value;
asm
 fld   Value.Single
 fadd  st(0), st(0)
 fabs
 fadd  CMinusHalf32
 fistp Result.Integer
 sar   Result.Integer, 1
 test  IntCast, $80000000
 jz @done
 neg Result.Integer
 @done:
end;
{$ENDIF}

function LaurentFastTrunc(const Value: Double): Integer; overload;
{$IFDEF PUREPASCAL}
begin
 Result := Trunc(Value);
end;
{$ELSE}
var
  ByteCast : array [0..7] of Byte absolute Value;
asm
 fld   Value.Double
 fadd  st(0), st(0)
 fabs
 fadd  CMinusHalf32
 fistp Result.Integer
 sar   Result.Integer, 1
 test  ByteCast[4].Integer, $80000000
 jz    @done
 neg   Result.Integer
 @done:
end;
{$ENDIF}


function FastRound(Sample: Single): Integer; overload;
{$IFDEF PUREPASCAL}
begin
 Result := Round(Sample);
end;
{$ELSE}
asm
 fld Sample.Single
 frndint
 fistp Result.Integer
end;
{$ENDIF}

function FastRound(Sample: Double): Integer; overload;
{$IFDEF PUREPASCAL}
begin
 Result := Round(Sample);
end;
{$ELSE}
asm
 fld Sample.Double
 frndint
 fistp Result.Integer
end;
{$ENDIF}

function FastSgn(const Value: Single): Integer;
var
  IntCast : Integer absolute Value;
begin
 Result := 1 - ((Intcast shr 31) shl 1);
end;

function OnOff(const Value: Single): Boolean;
begin
 Result := Value > 0.5
end;

function UnDenormalize(const Value : Single) : Single;
var
  IntValue : Integer absolute Value;
begin
 if (IntValue and $7F8000) = 0
  then Result := 0.0
  else Result := Value;
end;

procedure DontRaiseExceptionsAndSetFPUcodeword;
{$IFDEF FPC}
var
  FpuCodeword : Word;
asm
 mov     FpuCodeword, $133F
 fnclex                     // Don't raise pending exceptions enabled by the new flags
 fldcw   FpuCodeword        // round FPU codeword, with exceptions disabled
{$ELSE}
const
  SCRound8087CW     : Word = $133F; // round FPU codeword, with exceptions disabled
  SCChop8087CW      : Word = $1F3F; // Trunc (chop) FPU codeword, with exceptions disabled
  SCRoundDown8087CW : Word = $173F; // exceptions disabled
  SCRoundUp8087CW   : Word = $1B3F; // exceptions disabled
asm
 fnclex                  // Don't raise pending exceptions enabled by the new flags
 fldcw   SCRound8087CW   // SCRound8087CW: Word = $133F; round FPU codeword, with exceptions disabled
 {$ENDIF}
end;


{ String Functions }

{$IFNDEF FPC}
{$IFDEF MSWINDOWS}
function GetApplicationFilename: string;
var
  s : PAnsiChar;
begin
 GetMem(s, $7FF);
 GetModuleFileNameA(hInstance, s, SizeOf(s));
 Result := ExtractFilename(string(StrPas(s)));
end;

function GetApplicationDirectory: string;
var
  s : PAnsiChar;
begin
 GetMem(s, $7FF);
 GetModuleFilenameA(hInstance, s, SizeOf(s));
 Result := ExtractFileDir(string(StrPas(s)));
end;
{$ENDIF}

{$IFNDEF DELPHI12_UP}
function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean;
begin
 Result := C in CharSet;
end;

{$IFDEF MSWINDOWS}
procedure Msg(b: Boolean);
begin if b then Msg('TRUE') else Msg('FALSE');end;
procedure Msg(m: string; m2: string = '');
begin MessageBox(0, PAnsiChar(m), PChar(m2), MB_OK); end;
procedure Msg(i: Integer);
begin Msg(IntToStr(i)); end;
procedure Msg(s: Single);
begin Msg(FloatToStrF(s, ffFixed, 3, 3)); end;
procedure Msg(m: string; i:Integer);
begin MessageBox(0, PAnsiChar(m + ' ' + IntToStr(i)), '', MB_OK); end;
{$ENDIF}
{$ENDIF}
{$WARNINGS ON}

function SplitString(S: String; Delimiter: AnsiChar): TStrArray;
var
  C : Integer;
begin
 repeat
  SetLength(Result, Length(Result) + 1);
  {$IFDEF DELPHI2009_UP}
  C := AnsiPos(string(Delimiter), S);
  {$ELSE}
  C := Pos(Delimiter, S);
  {$ENDIF}
  if C = 0 then C := Length(S) + 1;
  Result[Length(Result)- 1] := Copy(S, 1, C- 1);
  Delete(S, 1, C);
 until Length(S)= 0;
end;

function MakeGoodFileName(s: string): string;
var
  i: Integer;
begin
 Result := '';
 for i := 1 to Length(s) do
  {$IFDEF DELPHI2009_UP}
  if CharInSet(s[i], ['*', '\', '/', '[', ']', '"', '|', '<', '>', '?', ':'])
  {$ELSE}
  if not (s[i] in ['*', '\', '/', '[', ']', '"', '|', '<', '>', '?', ':'])
  {$ENDIF}
   then Result := Result + s[i]
   else Result := Result + '-';
end;
{$ENDIF}

function FloatWithUnit(const Value: Double): string;
begin
 if Value > 1    then Result := FloatToStrF(Value, ffFixed, 6, 3)+ 's' else
 if Value > 1E-3 then Result := FloatToStrF(1E3 * Value, ffFixed, 6, 3)+ 'ms' else
 if Value > 1E-6
  then Result := FloatToStrF(1E6 * Value, ffFixed, 6, 3)+ '�s'
  else Result := FloatToStrF(1E9 * Value, ffFixed, 6, 3)+ 'ns'
end;

function FloatToString(Value: Extended; Digits: Integer = -1): string;
begin
 {$IFDEF UseNativeFloatToStringConversion}
 if Digits >= 0
  then Result := FloatToStrF(Value, ffGeneral, Digits, Digits)
  else Result := FloatToStr(Value);
 {$ELSE}
 if IsNan(Value)
  then Result := 'Error' else
 if IsInfinite(Value)
  then Result := 'oo'
  else
 Result := IntToStr(Round(Value));
 {$ENDIF}
end;

function FloatToAnsiString(Value: Extended; Digits: Integer = -1): AnsiString;
begin
 {$IFDEF UseNativeFloatToStringConversion}
 if Digits >= 0
  then Result := AnsiString(FloatToStrF(Value, ffGeneral, Digits, Digits))
  else Result := AnsiString(FloatToStr(Value));
 {$ELSE}
 if IsNan(Value)
  then Result := 'Error' else
 if IsInfinite(Value)
  then Result := 'oo'
  else
 Result := IntToStr(Round(Value));
 {$ENDIF}
end;


{$DEFINE PUREPASCAL}

function FastMin(const A, B: Single) : Single;
{$IFDEF PUREPASCAL}
begin
 if A > B
  then Result := B
  else Result := A
end;
{$ELSE}
asm
 fld     DWORD PTR [EBP + $08]
 fld     DWORD PTR [EBP + $0C]
 fcomi   st(0), st(1)
 fcmovnb st(0), st(1)
 ffree   st(1)
end;
{$ENDIF}

function FastMax(const A, B: Single) : Single;
{$IFDEF PUREPASCAL}
begin
 if A < B
  then Result := B
  else Result := A
end;
{$ELSE}
asm
 fld     DWORD PTR [EBP + $0C]
 fld     DWORD PTR [EBP + $08]
 fcomi   st(0), st(1)
 fcmovnb st(0), st(1)
 ffree   st(1)
end;
{$ENDIF}

{ Object oriented code conversions }

{$IFDEF MSWINDOWS}
{$IFDEF CPU386}
function MethodToProcedure(Self: TObject; MethodAddr: Pointer): Pointer;
type
  TMethodToProcedure = packed record
    PopEax    : Byte;
    PushSelf  : record
      Opcode  : Byte;
      Self    : Pointer;
    end;
    PushEax   : Byte;
    Jump      : record
      Opcode  : Byte;
      ModRm   : Byte;
      PTarget : ^Pointer;
      Target  : Pointer;
    end;
  end;
var
  MTP : ^TMethodToProcedure absolute Result;
begin
 MTP := VirtualAlloc(nil, SizeOf(MTP^), MEM_COMMIT, PAGE_EXECUTE_READWRITE);
 with MTP^ do
  begin
   PopEax          := $58;
   PushSelf.Opcode := $68;
   PushSelf.Self   := Self;
   PushEax         := $50;
   Jump.Opcode     := $FF;
   Jump.ModRm      := $25;
   Jump.PTarget    := @jump.Target;
   Jump.Target     := MethodAddr;
  end;
end;

function MethodToProcedure(Method: TMethod): Pointer;
begin
 Result := MethodToProcedure(TObject(Method.data), Method.code);
end;
{$ENDIF}
{$ENDIF}

end.
