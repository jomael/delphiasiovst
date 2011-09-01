unit DAV_AsioConvert;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2003-2011        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

{$IFDEF DELPHI6_UP}
{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}
{$ELSE}
{$DEFINE PUREPASCAL}
{$ENDIF}

{-$DEFINE PUREPASCAL}

uses
  {$IFDEF FPC}LCLIntf{$ELSE}Windows{$ENDIF}, DAV_Common, DAV_ProcessorInfo;

type
  TProcessorType = (ptFPU, ptSSE, pt3DNow);

  TInConverter = record
                  ic32 : procedure(Source: Pointer; Target: PSingle; SampleCount: LongInt);
                  ic64 : procedure(Source: Pointer; Target: PDouble; SampleCount: LongInt);
                 end;
  TOutConverter = record
                   oc32 : procedure(Source: PSingle; Target: Pointer; SampleCount: LongInt);
                   oc64 : procedure(Source: PDouble; Target: Pointer; SampleCount: LongInt);
                  end;
  TClipBuffer = record
                 cb32 : procedure(Data: PSingle; SampleCount: Integer);
                 cb64 : procedure(Data: PDouble; SampleCount: Integer);
                end;

  TClipCheckFunction = function (Source: Pointer; SampleCount: LongInt): Boolean;

procedure Use_FPU;
procedure Use_SSE;
procedure Use_3DNow;
procedure Use_FPU_UDF;
procedure Use_FPU_TDF;

var
  ProcessorType       : TProcessorType;

  FromInt16MSB        : TInConverter;
  FromInt24MSB        : TInConverter;  // used for 20 bits as well
  FromInt32MSB        : TInConverter;
  FromSingleMSB       : TInConverter;  // IEEE 754 32 bit float
  FromDoubleMSB       : TInConverter;  // IEEE 754 64 bit Double float
  FromInt32MSB16      : TInConverter;  // 32 bit data with 16 bit alignment
  FromInt32MSB18      : TInConverter;  // 32 bit data with 18 bit alignment
  FromInt32MSB20      : TInConverter;  // 32 bit data with 20 bit alignment
  FromInt32MSB24      : TInConverter;  // 32 bit data with 24 bit alignment
  FromInt16LSB        : TInConverter;
  FromInt24LSB        : TInConverter;
  FromInt32LSB        : TInConverter;
  FromSingleLSB       : TInConverter;  // IEEE 754 32 bit float
  FromDoubleLSB       : TInConverter;  // IEEE 754 64 bit Double float
  FromInt32LSB16      : TInConverter;  // 32 bit data with 16 bit alignment
  FromInt32LSB18      : TInConverter;  // 32 bit data with 18 bit alignment
  FromInt32LSB20      : TInConverter;  // 32 bit data with 20 bit alignment
  FromInt32LSB24      : TInConverter;  // 32 bit data with 24 bit alignment
  ToInt16MSB          : TOutConverter;
  ToInt24MSB          : TOutConverter;  // used for 20 bits as well
  ToInt32MSB          : TOutConverter;
  ToSingleMSB         : TOutConverter;  // IEEE 754 32 bit float
  ToDoubleMSB         : TOutConverter;  // IEEE 754 64 bit Double float
  ToInt32MSB16        : TOutConverter;  // 32 bit data with 16 bit alignment
  ToInt32MSB18        : TOutConverter;  // 32 bit data with 18 bit alignment
  ToInt32MSB20        : TOutConverter;  // 32 bit data with 20 bit alignment
  ToInt32MSB24        : TOutConverter;  // 32 bit data with 24 bit alignment
  ToInt16LSB          : TOutConverter;
  ToInt24LSB          : TOutConverter;
  ToInt32LSB          : TOutConverter;
  ToSingleLSB         : TOutConverter;  // IEEE 754 32 bit float
  ToDoubleLSB         : TOutConverter;  // IEEE 754 64 bit Double float
  ToInt32LSB16        : TOutConverter;  // 32 bit data with 16 bit alignment
  ToInt32LSB18        : TOutConverter;  // 32 bit data with 18 bit alignment
  ToInt32LSB20        : TOutConverter;  // 32 bit data with 20 bit alignment
  ToInt32LSB24        : TOutConverter;  // 32 bit data with 24 bit alignment

  ClipCheckInt16MSB   : TClipCheckFunction;
  ClipCheckInt24MSB   : TClipCheckFunction;  // used for 20 bits as well
  ClipCheckInt32MSB   : TClipCheckFunction;
  ClipCheckSingleMSB  : TClipCheckFunction;  // IEEE 754 32 bit float
  ClipCheckDoubleMSB  : TClipCheckFunction;  // IEEE 754 64 bit Double float
  ClipCheckInt32MSB16 : TClipCheckFunction;  // 32 bit data with 16 bit alignment
  ClipCheckInt32MSB18 : TClipCheckFunction;  // 32 bit data with 18 bit alignment
  ClipCheckInt32MSB20 : TClipCheckFunction;  // 32 bit data with 20 bit alignment
  ClipCheckInt32MSB24 : TClipCheckFunction;  // 32 bit data with 24 bit alignment
  ClipCheckInt16LSB   : TClipCheckFunction;
  ClipCheckInt24LSB   : TClipCheckFunction;
  ClipCheckInt32LSB   : TClipCheckFunction;
  ClipCheckSingleLSB  : TClipCheckFunction;  // IEEE 754 32 bit float
  ClipCheckDoubleLSB  : TClipCheckFunction;  // IEEE 754 64 bit Double float
  ClipCheckInt32LSB16 : TClipCheckFunction;  // 32 bit data with 16 bit alignment
  ClipCheckInt32LSB18 : TClipCheckFunction;  // 32 bit data with 18 bit alignment
  ClipCheckInt32LSB20 : TClipCheckFunction;  // 32 bit data with 20 bit alignment
  ClipCheckInt32LSB24 : TClipCheckFunction;  // 32 bit data with 24 bit alignment


var
  MixBuffers : record
    mb32 : procedure(Data: PSingle; MixBuffer: PSingle; SampleCount: Integer);
    mb64 : procedure(Data: PDouble; MixBuffer: PDouble; SampleCount: Integer);
  end;

  Volume : record
    v32 : procedure(Data: PSingle; Volume: Single; SampleCount: Integer);
    v64 : procedure(Data: PDouble; Volume: Double; SampleCount: Integer);
  end;

  FadeInLinear  : record
    v32 : procedure(Data: PSingle; SampleCount: Integer);
    v64 : procedure(Data: PDouble; SampleCount: Integer);
  end;

  FadeOutLinear : record
    v32 : procedure(Data: PSingle; SampleCount: Integer);
    v64 : procedure(Data: PDouble; SampleCount: Integer);
  end;

  FadeLinear : record
    v32 : procedure(Data: PSingle; SampleCount: Integer; ScaleFactor, Gradient: Single);
    v64 : procedure(Data: PDouble; SampleCount: Integer; ScaleFactor, Gradient: Double);
  end;

  FadeExponential : record
    v32 : procedure(Data: PSingle; SampleCount: Integer; ScaleFactor, Gradient: Single);
    v64 : procedure(Data: PDouble; SampleCount: Integer; ScaleFactor, Gradient: Double);
  end;

  Trigger : record
    v32 : function(Data: PSingle; SampleCount: Integer; TriggerFaktor: Double): Integer;
    v64 : function(Data: PDouble; SampleCount: Integer; TriggerFaktor: Double): Integer;
  end;

  ClipDigital   : TClipBuffer;
  ClipAnalog    : TClipBuffer;
  EnableSSE     : Boolean;

implementation

uses
  Math {$IFDEF PUREPASCAL}, DAV_Approximations {$ENDIF};

{$IFNDEF FPU}
type
  TWrdArray  = Array[0..0] of SmallInt;
  PWrdArray  = ^TWrdArray;
  TDblArray  = Array[0..0] of Double;
  PDblArray  = ^TDblArray;
  TntgrArray = Array[0..0] of Integer;
  PntgrArray = ^TntgrArray;
  TSnglArray = Array[0..0] of Single;
  PSnglArray = ^TSnglArray;
{$ENDIF}

const
  CFloatToShort : Single = $7F;
  CShortToFloat : Single = 1 / $7F;
  CFloatToSmall : Single = $7FFF;
  CSmallToFloat : Single = 1 / $7FFF;
  CFloatToInt18 : Double = $1FFFF;
  CInt18ToFloat : Double = 1 / $1FFFF;
  CFloatToInt20 : Double = $7FFFF;
  CInt20ToFloat : Double = 1 / $7FFFF;
  CFloatToInt24 : Double = $7FFFFF;
  CInt24ToFloat : Double = 1 / $7FFFFF;
  CFloatToInt   : Double = $7FFFFFFF;
  CIntToFloat   : Double = 1 / $7FFFFFFF;
  CFloatToInt4  : array[0..3] of Single = ($7FFFFFFF, $7FFFFFFF, $7FFFFFFF,
    $7FFFFFFF);
  CIntToFloat4  : array[0..3] of Single = (1 / $7FFFFFFF, 1 / $7FFFFFFF,
    1 / $7FFFFFFF, 1 / $7FFFFFFF);

var
  RandSeed : LongInt;
{$WARNINGS OFF}


procedure ClipDigital_x86(Data: PSingle; SampleCount: Integer); overload;
{$IFDEF PUREPASCAL}
var
  SampleIndex: Integer;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  begin
   Data^ := 0.5 * (Abs(Data^ + 1) - Abs(Data^ - 1));
   Inc(Data);
  end;
end;
{$ELSE}
const
  c1a : Single = 1;
asm
 MOV  ECX, EDX
@Start:
 MOV  EDX, [EAX]
 AND  EDX, $7FFFFFFF
 cmp  EDX, c1a
 jle  @EndLoop
 MOV  EDX, [EAX]
 AND  EDX, $80000000
 ADD  EDX, c1a
 MOV  [EAX], EDX
 @EndLoop:
 ADD  EAX, 4
 LOOP @Start
end;
{$ENDIF}

procedure ClipDigital_x86(Data: PDouble; SampleCount: Integer); overload;
{$IFDEF PUREPASCAL}
var
  SampleIndex: Integer;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  begin
   Data^ := 0.5 * (Abs(Data^ + 1) - Abs(Data^ - 1));
   Inc(Data);
  end;
end;
{$ELSE}
const
  c05 : Double = 0.5;
asm
 MOV  ECX, EDX
 fld1
 FLD  c05
@Start:
 FLD  [EAX + 8 * ECX - 8].Double
 fadd ST(0), ST(2)
 FABS
 FLD  [EAX + 8 * ECX - 8].Double
 fsub ST(0), ST(3)
 FABS
 fsubp
 FMUL ST(0), ST(1)
 FSTP [EAX + 8 * ECX - 8].Double
 LOOP @Start
 FSTP ST(0)
 FSTP ST(0)
end;
{$ENDIF}

procedure ClipAnalog_FPU(Data: PSingle; SampleCount: Integer); overload;
{$IFDEF PUREPASCAL}
var
  SampleIndex : Integer;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  begin
   Data^ := 2 * FastTanh2Like3Term(Data^);
   Inc(Data);
  end;
{$ELSE}
const
  c3: Single = 3;
  c6: Single = 6;
asm
 FLD c3.Single                      // 3
 FLD c6.Single                      // 6, 3
 fld1                               // 1, 6, 3
 fld1                               // 1, 1, 6, 3
 FADDP                              // 2, 6, 3
@Start:
 DEC  EDX
 MOV  ECX, [EAX + 4 * EDX].Integer
 AND  ECX, $7FFFFFFF
 MOV  [esp - 4], ECX
 FLD  [esp - 4].Single              // abs(input), 2, 6, 3
 FLD  ST(3)                         // 3, abs(input), 2, 6, 3
 fadd  ST(0), ST(1)                 // 3 + abs(input), abs(input), 2, 6, 3
 FLD  ST(0)                         // 3 + abs(input), 3 + abs(input), abs(input), 2, 6, 3
 FMUL [EAX + 4 * EDX].Single        // input*(3 + abs(input)), 3 + abs(input), abs(input), 2, 6, 3
 fxch ST(2)                         // abs(input), 3 + abs(input), input*(3 + abs(input)), 2, 6, 3
 fmulp                              // abs(input)* (3 + abs(input)), input*(3 + abs(input)), 2, 6, 3
 fadd ST(0), ST(3)                  // 6 + abs(input)* (3 + abs(input)), input*(3 + abs(input)), 2, 6, 3
 fdiv                               // 6 + abs(input)* (3 + abs(input)) / input*(3 + abs(input)), 2, 6, 3
 FMUL ST(0), ST(1)                  // 2 * (6 + abs(input)* (3 + abs(input)) / input*(3 + abs(input))), 2, 6, 3
 FSTP [EAX + 4 * EDX].Single        // 2, 6, 3
 test EDX, EDX
 jg   @Start
 FSTP ST(0)
 FSTP ST(0)
 FSTP ST(0)
{$ENDIF}
end;

procedure ClipAnalog_FPU(Data: PDouble; SampleCount: Integer); overload;
{$IFDEF PUREPASCAL}
var
  SampleIndex : Integer;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  begin
   Data^ := 2 * FastTanh2Like3Term(Data^);
   Inc(Data);
  end;
{$ELSE}
const
  c3: Single = 3;
  c6: Single = 6;
asm
 FLD c3.Single                      // 3
 FLD c6.Single                      // 6, 3
 fld1                               // 1, 6, 3
 fld1                               // 1, 1, 6, 3
 FADDP                              // 2, 6, 3
@Start:
 DEC EDX
 MOV ECX, [EAX + 8 * EDX].Integer
 AND ECX, $7FFFFFFF
 MOV [esp - 8], ECX
 MOV ECX, [EAX + 4 * EDX + 4].Integer
 MOV [esp - 4], ECX
 FLD [esp - 8].Double               // abs(input), 2, 6, 3
 FLD ST(3)                          // 3, abs(input), 2, 6, 3
 fadd ST(0), ST(1)                  // 3 + abs(input), abs(input), 2, 6, 3
 FLD ST(0)                          // 3 + abs(input), 3 + abs(input), abs(input), 2, 6, 3
 FMUL [EAX + 8 * EDX].Double        // input*(3 + abs(input)), 3 + abs(input), abs(input), 2, 6, 3
 fxch ST(2)                         // abs(input), 3 + abs(input), input*(3 + abs(input)), 2, 6, 3
 fmulp                              // abs(input)* (3 + abs(input)), input*(3 + abs(input)), 2, 6, 3
 fadd ST(0), ST(3)                  // 6 + abs(input)* (3 + abs(input)), input*(3 + abs(input)), 2, 6, 3
 fdiv                               // 6 + abs(input)* (3 + abs(input)) / input*(3 + abs(input)), 2, 6, 3
 FMUL ST(0), ST(1)                  // 2 * (6 + abs(input)* (3 + abs(input)) / input*(3 + abs(input))), 2, 6, 3
 FSTP [EAX + 8 * EDX].Double        // 2, 6, 3
 test EDX, EDX
 jg @Start
 FSTP ST(0)
 FSTP ST(0)
 FSTP ST(0)
{$ENDIF}
end;

procedure FadeInLinear_FPU(Data: PSingle; SampleCount: Integer); overload;
{$IFDEF PUREPASCAL}
var
  SampleIndex : Integer;
  Value       : Double;
begin
 Value := 1 / (SampleCount - 1);
 for SampleIndex := 0 to SampleCount - 1 do
  begin
   Data^ := Data^ * SampleIndex * Value;
   Inc(Data);
  end;
end;
{$ELSE}
asm
 MOV  [esp - 4], EDX
 FILD [esp - 4].Single           // SampleCount
 fld1                            // 1, SampleCount
 fdivrp                          // 1 / SampleCount

 @FadeLoop:
  MOV  [esp - 4], EDX
  FILD [esp - 4].Single         // SampleIndex, 1 / SampleCount
  DEC  EDX
  FMUL ST(0), ST(1)             // SampleIndex / SampleCount, 1 / SampleCount
  FMUL [EAX + 4 * EDX].Single   // SampleIndex * Value / SampleCount, 1 / SampleCount
  FSTP [EAX + 4 * EDX].Single   // write back
 JNZ @FadeLoop
 FSTP ST(0)                      // clear stack
end;
{$ENDIF}

procedure FadeInLinear_FPU(Data: PDouble; SampleCount: Integer); overload;
{$IFDEF PUREPASCAL}
var
  SampleIndex : Integer;
  Value       : Double;
begin
 Value := 1 / (SampleCount - 1);
 for SampleIndex := 0 to SampleCount - 1 do
  begin
   Data^ := Data^ * SampleIndex * Value;
   Inc(Data);
  end;
end;
{$ELSE}
asm
 MOV [esp - 4], EDX
 FILD [esp - 4].Single             // SampleCount
 fld1                              // 1, SampleCount
 fdivrp                            // 1 / SampleCount

 @FadeLoop:
   MOV [esp - 4], EDX
   FILD [esp - 4].Single           // i, 1 / SampleCount
   FMUL ST(0), ST(1)               // i / SampleCount, 1 / SampleCount
   DEC EDX
   FMUL [EAX + 8 * EDX].Double     // i * Value / SampleCount, 1 / SampleCount
   FSTP [EAX + 8 * EDX].Double     // write back
 JNZ @FadeLoop
 FSTP ST(0)                        // clear stack
end;
{$ENDIF}

procedure FadeOutLinear_FPU(Data: PSingle; SampleCount: Integer); overload;
{$IFDEF PUREPASCAL}
var
  SampleIndex : Integer;
  Value       : Double;
begin
 Value := 1 / (SampleCount - 1);
 for SampleIndex := SampleCount - 1 downto 0 do
  begin
   Data^ := Data^ * SampleIndex * Value;
   Inc(Data);
  end;
end;
{$ELSE}
asm
 MOV [esp - 4], EDX
 FILD [esp - 4].Single               // SampleCount
 fld1                                // 1, SampleCount
 fdivrp                              // 1 / SampleCount

 @FadeLoop:
   MOV [esp - 4], EDX
   FILD [esp - 4].Single             // i, 1 / SampleCount
   FMUL ST(0), ST(1)                 // i / SampleCount, 1 / SampleCount
   fld1                              // 1, i / SampleCount, 1 / SampleCount
   fsubp                             // 1 - i / SampleCount, 1 / SampleCount
   DEC EDX
   FMUL [EAX + 4 * EDX - 4].Single   // Value * (1 - i / SampleCount), 1 / SampleCount
   FSTP [EAX + 4 * EDX - 4].Single   // write back
 JNZ @FadeLoop
 FSTP ST(0)                          // clear stack
end;
{$ENDIF}

procedure FadeOutLinear_FPU(Data: PDouble; SampleCount: Integer); overload;
{$IFDEF PUREPASCAL}
var
  SampleIndex : Integer;
  Value       : Double;
begin
 Value := 1 / (SampleCount - 1);
 for SampleIndex := SampleCount - 1 downto 0 do
  begin
   Data^ := Data^ * SampleIndex * Value;
   Inc(Data);
  end;
end;
{$ELSE}
asm
 MOV  [esp - 4], EDX
 FILD [esp - 4].Single              // SampleCount
 fld1                               // 1, SampleCount
 fdivrp                             // 1 / SampleCount

 @FadeLoop:
   MOV  [esp - 4], EDX
   FILD [esp - 4].Single            // i, 1 / SampleCount
   FMUL ST(0), ST(1)                // i / SampleCount, 1 / SampleCount
   fld1                             // 1, i / SampleCount, 1 / SampleCount
   fsubp                            // 1 - i / SampleCount, 1 / SampleCount
   DEC  EDX
   FMUL [EAX + 8 * EDX - 8].Double  // Value * (1 - i / SampleCount), 1 / SampleCount
   FSTP [EAX + 8 * EDX - 8].Double  // write back
 JNZ @FadeLoop
 FSTP ST(0)                         // clear stack
end;
{$ENDIF}

procedure FadeExponential_FPU(Data: PSingle; SampleCount: Integer; ScaleFactor, Gradient: Single); overload;
{-$IFDEF PUREPASCAL}
var
  SampleIndex : Integer;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  begin
   Data^ := Data^ * ScaleFactor;
   ScaleFactor := ScaleFactor * Gradient;
   if ScaleFactor > 1 then Exit;
   Inc(Data);
  end;
(*
{$ELSE}
asm
 This assembler snippet unfortunately processes the signal backwards. Do not
 use unless you rewrite the code! 

 fld1
 FLD Gradient.Double
 FLD ScaleFactor.Double
 MOV ECX, EAX

 @FadeLoop:
   FLD  [ECX + 4 * EDX - 4].Single  // Value, ScaleFactor, Gradient, 1
   FMUL ST(0), ST(1)                // Value * ScaleFactor, ScaleFactor, Gradient, 1
   FSTP [ECX + 4 * EDX - 4].Single  // ScaleFactor, Gradient, 1
   FMUL ST(0), ST(1)                // ScaleFactor * Gradient = ScaleFactor, Gradient, 1

   FCOMI ST(0), ST(2)               // ScaleFactor <-> 1 ?
   FSTSW AX                         // AX = FPU Status Word
   SAHF                             // AX -> EFLAGS register
   JB @FadeLoopEnd                  // if ScaleFactor > 1 then exit!

   DEC EDX
 JNZ @FadeLoop

 @FadeLoopEnd:
 FSTP ST(0)                    // clear stack
 FSTP ST(0)                    // clear stack
 FSTP ST(0)                    // clear stack
{$ENDIF}
*)
end;

procedure FadeExponential_FPU(Data: PDouble; SampleCount: Integer; ScaleFactor, Gradient : Double); overload;
{-$IFDEF PUREPASCAL}
var
  SampleIndex : Integer;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  begin
   Data^ := Data^ * ScaleFactor;
   ScaleFactor := ScaleFactor * Gradient;
   if ScaleFactor > 1 then Exit;
   Inc(Data);
  end;
(*
{$ELSE}
asm
 This assembler snippet unfortunately processes the signal backwards. Do not
 use unless you rewrite the code! 

 fld1                                // 1
 FLD Gradient.Double                  // Gradient, 1
 FLD ScaleFactor.Double           // ScaleFactor, Gradient, 1
 MOV ECX, EAX                        // ECX = EAX

 @FadeLoop:
   FLD  [ECX + 8 * EDX - 8].Double   // Value, ScaleFactor, Gradient, 1
   FMUL ST(0), ST(1)                 // Value * ScaleFactor, ScaleFactor, ...
   FSTP [ECX + 8 * EDX - 8].Double   // write back
   FMUL ST(0), ST(1)                 // Gradient * ScaleFactor, Gradient, 1

   FCOMI ST(0), ST(2)                // ScaleFactor <-> 1 ?
   FSTSW AX                          // AX = FPU Status Word
   SAHF                              // AX -> EFLAGS register
   JB @FadeLoopEnd                   // if ScaleFactor > 1 then Exit!

   DEC EDX
 JNZ @FadeLoop

 @FadeLoopEnd:
 FSTP ST(0)                          // clear stack
 FSTP ST(0)                          // clear stack
 FSTP ST(0)                          // clear stack
{$ENDIF}
*)
end;

procedure FadeLinear_FPU(Data: PSingle; SampleCount: Integer; ScaleFactor, Gradient: Single); overload;
{$IFDEF PUREPASCAL}
var
  SampleIndex : Integer;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  begin
   Data^ := Data^ * ScaleFactor;
   ScaleFactor := ScaleFactor + Gradient;
   if ScaleFactor > 1 then Exit;
   Inc(Data);
  end;
{$ELSE}
asm
 fld1
 FLD Gradient.Double
 FLD ScaleFactor.Double
 MOV ECX, EAX                        // ECX = EAX

 @FadeLoop:
   FLD  [ECX + 4 * EDX - 4].Single   // Value, ScaleFactor
   FMUL ST(0), ST(1)                 // Value * ScaleFactor, ScaleFactor
   FSTP [ECX + 4 * EDX - 4].Single   // write back
   fadd ST(0), ST(1)                 // ScaleFactor + Gradient

   FCOMI ST(0), ST(2)                // ScaleFactor <-> 1 ?
   FSTSW AX                          // AX = FPU Status Word
   SAHF                              // AX -> EFLAGS register
   JB @FadeLoopEnd                   // if ScaleFactor > 1 then exit!

   DEC EDX
 JNZ @FadeLoop

 @FadeLoopEnd:
 FSTP ST(0)                          // clear stack
 FSTP ST(0)                          // clear stack
 FSTP ST(0)                          // clear stack
{$ENDIF}
end;

procedure FadeLinear_FPU(Data: PDouble; SampleCount: Integer; ScaleFactor, Gradient: Double); overload;
{$IFDEF PUREPASCAL}
var
  SampleIndex : Integer;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  begin
   Data^ := Data^ * ScaleFactor;
   ScaleFactor := ScaleFactor + Gradient;
   if ScaleFactor > 1 then exit;
   Inc(Data);
  end;
{$ELSE}
asm
 fld1
 FLD Gradient.Double
 FLD ScaleFactor.Double
 MOV ECX, EAX                      // ECX = EAX

 @FadeLoop:
   FLD  [ECX + 8 * EDX - 8].Double // Value, ScaleFactor
   FMUL ST(0), ST(1)                // Value * ScaleFactor, ScaleFactor
   FSTP [ECX + 8 * EDX - 8].Double // write back
   FMUL ST(0), ST(1)                // ScaleFactor + Gradient

   FCOMI ST(0), ST(2)              // ScaleFactor <-> 1 ?
   FSTSW AX                        // AX = FPU Status Word
   SAHF                            // AX -> EFLAGS register
   JB @FadeLoopEnd                 // if ScaleFactor > 1 then exit!

   DEC EDX
 JNZ @FadeLoop

 @FadeLoopEnd:
 FSTP ST(0)                        // clear stack
 FSTP ST(0)                        // clear stack
 FSTP ST(0)                        // clear stack
{$ENDIF}
end;

function Trigger_FPU(Data: PSingle; SampleCount: Integer; TriggerFaktor : Double): Integer; overload;
{$IFDEF PUREPASCAL}
var
  SampleIndex : Integer;
begin
 Result := 0;
 for SampleIndex := 0 to SampleCount-1 do
  begin
   if abs(Data^)>TriggerFaktor
    then exit
    else Inc(Result);
   Inc(Data);
  end;
 Result := -1;
{$ELSE}
asm
 FLD TriggerFaktor.Double
 MOV ECX, EAX                  // ECX = EAX

 @FadeLoop:
   FLD  [ECX+4*EDX-4].Single   // Value, TriggerFaktor
   FABS                        // |Value|, TriggerFaktor

   FCOMI ST(0), ST(1)          // CurrentFadeFak <-> 1 ?
   FSTSW AX                    // AX = FPU Status Word
   SAHF                        // AX -> EFLAGS register
   FSTP ST(0)                  // TriggerFaktor
   JB @TriggerFound            // if |Value| > TriggerFaktor then exit!

   DEC EDX
 JNZ @FadeLoop

 MOV Result, -1                // not triggered
 JMP @FadeLoopEnd

 @TriggerFound:
 MOV Result, EDX               // triggered at sample EDX

 @FadeLoopEnd:
 FSTP ST(0)                    // clear stack
{$ENDIF}
end;

function Trigger_FPU(Data: PDouble; SampleCount: Integer; TriggerFaktor: Double): Integer; overload;
{$IFDEF PUREPASCAL}
var
  SampleIndex : Integer;
begin
 Result := 0;
 for SampleIndex := 0 to SampleCount-1 do
  begin
   if abs(Data^)>TriggerFaktor
    then exit
    else Inc(Result);
   Inc(Data);
  end;
 Result := -1;
{$ELSE}
asm
 FLD TriggerFaktor.Double
 MOV ECX, EAX                  // ECX = EAX

 @FadeLoop:
   FLD  [ECX+8*EDX-8].Double   // Value, TriggerFaktor
   FABS                        // |Value|, TriggerFaktor

   FCOMI ST(0), ST(1)          // CurrentFadeFak <-> 1 ?
   FSTSW AX                    // AX = FPU Status Word
   SAHF                        // AX -> EFLAGS register
   FSTP ST(0)                  // TriggerFaktor
   JB @TriggerFound            // if |Value| > TriggerFaktor then exit!

   DEC EDX
 JNZ @FadeLoop

 MOV Result, -1                // not triggered
 JMP @FadeLoopEnd

 @TriggerFound:
 MOV Result, EDX               // triggered at sample EDX

 @FadeLoopEnd:
 FSTP ST(0)                    // clear stack
{$ENDIF}
end;

// ReverseEndian3 : reverts 3-byte entities in place
procedure ReverseEndian3(Data: Pointer; SampleCount: LongInt);
{$IFDEF PUREPASCAL}
type
  TByte3Array = Array [0..2] of Byte;
  PByte3Array = ^TByte3Array;
var
  BufArray    : PByte3Array absolute Data;
  BufByte     : PByte absolute Data;
  SampleIndex : Integer;
  b           : Byte;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  begin
   b := BufArray[0];
   BufArray[0] := BufArray[2];
   BufArray[2] := b;
   Inc(BufByte, 3);
  end;
end;
{$ELSE}
asm
 MOV ECX, EDX
@Start:
 MOV dh, [EAX + 2]
 MOV dl, [EAX    ]
 MOV [EAX + 2], dl
 MOV [EAX    ], dh
 ADD  EAX, 3
 LOOP @Start
end;
{$ENDIF}

// ReverseEndian4 : reverts 4-byte entities in place
procedure ReverseEndian4(Data: Pointer; SampleCount: LongInt);
{$IFDEF PUREPASCAL}
type
  TByte4Array = Array [0..3] of Byte;
  PByte4Array = ^TByte4Array;
var
  BufArray    : PByte4Array absolute Data;
  BufByte     : PByte absolute Data;
  SampleIndex : Integer;
  b           : Byte;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  begin
   b := BufArray[0]; BufArray[0] := BufArray[3]; BufArray[3] := b;
   b := BufArray[1]; BufArray[1] := BufArray[2]; BufArray[2] := b;
   Inc(BufByte, 4);
  end;
end;
{$ELSE}
asm
 MOV ECX, SampleCount
@Start:
 MOV EDX, [EAX + 4 * ECX - 4]
 bswap EDX
 MOV [EAX + 4 * ECX - 4], EDX
 LOOP @Start
end;
{$ENDIF}

// ReverseEndian8 : reverts 8-byte entities in place
procedure ReverseEndian8(Data: Pointer; SampleCount: LongInt);
{$IFDEF PUREPASCAL}
type
  TByte4Array = Array [0..7] of Byte;
  PByte4Array = ^TByte4Array;
var
  BufArray    : PByte4Array absolute Data;
  BufByte     : PByte absolute Data;
  SampleIndex : Integer;
  b           : Byte;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  begin
   b := BufArray[0]; BufArray[0] := BufArray[7]; BufArray[7] := b;
   b := BufArray[1]; BufArray[1] := BufArray[6]; BufArray[6] := b;
   b := BufArray[2]; BufArray[2] := BufArray[5]; BufArray[5] := b;
   b := BufArray[3]; BufArray[3] := BufArray[4]; BufArray[4] := b;
   Inc(BufByte, 8);
  end;
end;
{$ELSE}
asm
 PUSH  EBX
 MOV   ECX, SampleCount
@Start:
 MOV   EDX, [EAX]
 MOV   EBX, [EAX + 4]
 bswap EDX
 bswap EBX
 MOV   [EAX + 4], EDX
 MOV   [EAX], EBX
 ADD   EAX, 8
 LOOP  @Start
 POP   EBX
end;
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////// FPU ////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

procedure Int16LSBToSingle_FPU(Source: Pointer; Target: PSingle; SampleCount: LongInt);
{$IFDEF PUREPASCAL}
var
  WordSource  : PWord absolute Source;
  SampleIndex : Integer;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  begin
   Target^ := WordSource^ * CSmallToFloat;
   Inc(WordSource);
   Inc(Target);
  end;
end;
{$ELSE}
asm
  FLD   CSmallToFloat //for speed
 @Start:
  FILD  [EAX + 2 * ECX - 2].Word
  FMUL  ST(0), ST(1)
  FSTP  [EDX + 4 * ECX - 4].Single;
  LOOP @Start
  FFREE ST(0)
end;
{$ENDIF}

procedure Int16LSBToDouble_FPU(Source: Pointer; Target: PDouble; SampleCount: LongInt);
{$IFDEF PUREPASCAL}
var
  SourceArray : PWrdArray absolute Source;
  TargetArray : PDblArray absolute Target;
  SampleIndex : Integer;
begin
 for SampleIndex := 0 to SampleCount - 1
  do TargetArray[SampleIndex] := SourceArray[SampleIndex] * CSmallToFloat;
end;
{$ELSE}
asm
  FLD   CSmallToFloat        //for speed
 @Start:
  FILD  [EAX + 2 * ECX - 2].Word
  FMUL  ST(0), ST(1)
  FSTP  [EDX + 8 * ECX - 8].Double;
  LOOP  @Start
  FFREE ST(0)
end;
{$ENDIF}

procedure Int24LSBToSingle_FPU(Source: Pointer; Target: PSingle; SampleCount: LongInt);
{$IFDEF PUREPASCAL}
var
  SourceInt   : PInteger absolute Source;
  SourceByte  : PByte absolute Source;
  TargetArray : PSnglArray absolute Target;
  SampleIndex : Integer;

// Bytes
// 00 01 02 03|04 05 06 07
// -- -- -- ++ ++ ++ .. ..


begin
 for SampleIndex := 0 to SampleCount - 1 do
  begin
   TargetArray[SampleIndex] := (SourceInt^ and $FFFFFF00) * CIntToFloat;
   Inc(SourceByte, 3);
  end;
end;
{$ELSE}
asm
 FLD   CIntToFloat
 PUSH  EBX
 MOV   ECX, SampleCount

@Start:
 MOV   EBX, [EAX]
 shl   EBX, 8
 AND   EBX, $FFFFFF00

 MOV   [esp - 4], EBX
 FILD  [esp - 4].Single
 FMUL  ST(0), ST(1)

 FSTP  [Target].Single
 ADD   EAX, 3
 ADD   EDX, 4
 LOOP  @Start
 POP   EBX
 FFREE ST(0)
end;
{$ENDIF}

procedure Int24LSBToDouble_FPU(Source: Pointer; Target: PDouble; SampleCount: LongInt); overload;
{$IFDEF PUREPASCAL}
var
  SourceInt   : PInteger absolute Source;
  SourceByte  : PByte absolute Source;
  TargetArray : PDblArray absolute Target;
  SampleIndex : Integer;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  begin
   TargetArray[SampleIndex] := (SourceInt^ shr 8) * CInt24ToFloat;
   Inc(SourceByte, 3);
  end;
end;
{$ELSE}
asm
 FLD   CIntToFloat
 PUSH  EBX
 MOV   ECX, SampleCount

@Start:
 MOV   EBX, [EAX]
 shl   EBX, 8
 AND   EBX, $FFFFFF00
 MOV   [esp - 4], EBX
 FILD  [esp - 4].Single
 FMUL  ST(0), ST(1)
 FSTP  [Target].Double
 ADD   EAX, 3
 ADD   EDX, 8
 LOOP  @Start
 POP   EBX
 FFREE ST(0)
end;
{$ENDIF}

procedure Int32LSBToSingle_FPU(Source: Pointer; Target: PSingle; SampleCount: LongInt); overload;
{$IFDEF PUREPASCAL}
var
  SourceArray : PntgrArray absolute Source;
  TargetArray : PSnglArray absolute Target;
  SampleIndex : Integer;
begin
 for SampleIndex := 0 to SampleCount - 1
  do TargetArray[SampleIndex] := SourceArray[SampleIndex]*CIntToFloat;
end;
{$ELSE}
asm
  FLD   CIntToFloat         //for speed
 @Start:
  DEC   ECX
  FILD  [EAX + 4 * ECX].DWord
  FMUL  ST(0), ST(1)
  FSTP  [EDX + 4 * ECX].Single
  JNZ   @Start
  FFREE ST(0)
end;
{$ENDIF}

procedure Int32LSBToDouble_FPU(Source: Pointer; Target: PDouble; SampleCount: LongInt); overload;
{$IFDEF PUREPASCAL}
var
  SourceArray : PntgrArray absolute Source;
  TargetArray : PDblArray absolute Target;
  i           : Integer;
begin
 for i := 0 to SampleCount - 1
  do TargetArray[i] := SourceArray[i] * CIntToFloat;
end;
{$ELSE}
asm
  FLD   CIntToFloat         //for speed
 @Start:
  DEC ECX
  FILD  [EAX + 4 * ECX].DWord
  FMUL  ST(0), ST(1)
  FSTP  [EDX + 8 * ECX].Double
  JNZ @Start
  FFREE ST(0)
end;
{$ENDIF}

procedure SingleLSBToSingle_FPU(Source: Pointer; Target: PSingle; SampleCount: LongInt); overload;
begin
 move(Source^, Target^, SampleCount*SizeOf(Single));
end;

procedure SingleLSBToDouble_FPU(Source: Pointer; Target: PDouble; SampleCount: LongInt); overload;
{$IFDEF PUREPASCAL}
var
  SourceArray : PSnglArray absolute Source;
  TargetArray : PDblArray absolute Target;
  i           : Integer;
begin
 for i := 0 to SampleCount - 1
  do TargetArray[i] := SourceArray[i];
end;
{$ELSE}
asm
 @Start:
  DEC ECX
  FLD   [EAX + 4 * ECX].Single
  FSTP  [EDX + 8 * ECX].Double
  JNZ @Start
end;
{$ENDIF}

procedure DoubleLSBToSingle_FPU(Source: Pointer; Target: PSingle; SampleCount: LongInt); overload;
{$IFDEF PUREPASCAL}
var
  SourceArray : PDblArray absolute Source;
  TargetArray : PSnglArray absolute Target;
  i           : Integer;
begin
 for i := 0 to SampleCount - 1
  do TargetArray[i] := SourceArray[i];
end;
{$ELSE}
asm
 @Start:
  DEC ECX
  FLD   [EAX+8*ECX].Double
  FSTP  [EDX+4*ECX].Single
  JNZ @Start
end;
{$ENDIF}

procedure DoubleLSBToDouble_FPU(Source: Pointer; Target: PDouble; SampleCount: LongInt); overload;
begin
 move(Source^, Target^, SampleCount*SizeOf(Double));
end;

procedure Int32LSB16ToSingle_FPU(Source: Pointer; Target: PSingle; SampleCount: LongInt); overload; // 32 bit data with 16 bit alignment
{$IFDEF PUREPASCAL}
var
  SourceArray : PntgrArray absolute Source;
  TargetArray : PSnglArray absolute Target;
  i           : Integer;
begin
 for i := 0 to SampleCount - 1
  do TargetArray[i] := SourceArray[i]*CSmallToFloat;
end;
{$ELSE}
asm
  FLD      CSmallToFloat
@Start:
  FILD     [EAX+4*ECX-4].DWord
  FMUL     ST(0), ST(1)
  FSTP     [EDX+4*ECX-4].Single
  LOOP @Start
  FFREE    ST(0)
end;
{$ENDIF}

procedure Int32LSB16ToDouble_FPU(Source: Pointer; Target: PDouble; SampleCount: LongInt); overload; // 32 bit data with 16 bit alignment
{$IFDEF PUREPASCAL}
var
  SourceArray : PntgrArray absolute Source;
  TargetArray : PDblArray absolute Target;
  i           : Integer;
begin
 for i := 0 to SampleCount - 1
  do TargetArray[i] := SourceArray[i]*CSmallToFloat;
end;
{$ELSE}
asm
  FLD      CSmallToFloat
@Start:
  FILD     [EAX+4*ECX-4].DWord
  FMUL     ST(0), ST(1)
  FSTP     [EDX+8*ECX-8].Double
  LOOP @Start
  FFREE    ST(0)
end;
{$ENDIF}

procedure Int32LSB18ToSingle_FPU(Source: Pointer; Target: PSingle; SampleCount: LongInt); overload; // 32 bit data with 18 bit alignment
{$IFDEF PUREPASCAL}
var
  SourceArray : PntgrArray absolute Source;
  TargetArray : PSnglArray absolute Target;
  i           : Integer;
begin
 for i := 0 to SampleCount - 1
  do TargetArray[i] := SourceArray[i]*CInt18ToFloat;
end;
{$ELSE}
asm
  FLD      CInt18ToFloat
@Start:
  FILD     [EAX+4*ECX-4].DWord
  FMUL     ST(0), ST(1)
  FSTP     [EDX+4*ECX-4].Single
  LOOP     @start
  FFREE    ST(0)
end;
{$ENDIF}

procedure Int32LSB18ToDouble_FPU(Source: Pointer; Target: PDouble; SampleCount: LongInt); overload; // 32 bit data with 18 bit alignment
{$IFDEF PUREPASCAL}
var
  SourceArray : PntgrArray absolute Source;
  TargetArray : PDblArray absolute Target;
  i           : Integer;
begin
 for i := 0 to SampleCount - 1
  do TargetArray[i] := SourceArray[i]*CInt18ToFloat;
end;
{$ELSE}
asm
  FLD      CInt18ToFloat
@Start:
  FILD     [EAX+4*ECX-4].DWord
  FMUL     ST(0), ST(1)
  FSTP     [EDX+8*ECX-8].Double
  LOOP     @start
  FFREE    ST(0)
end;
{$ENDIF}

procedure Int32LSB20ToSingle_FPU(Source: Pointer; Target: PSingle; SampleCount: LongInt); overload; // 32 bit data with 20 bit alignment
{$IFDEF PUREPASCAL}
var
  SourceArray : PntgrArray absolute Source;
  TargetArray : PSnglArray absolute Target;
  i           : Integer;
begin
 for i := 0 to SampleCount - 1
  do TargetArray[i] := SourceArray[i]*CInt20ToFloat;
end;
{$ELSE}
asm
  FLD      CInt20ToFloat
@Start:
  FILD     [EAX+4*ECX-4].DWord
  FMUL     ST(0), ST(1)
  FSTP     [EDX+4*ECX-4].Single
  LOOP     @start
  FFREE    ST(0)
end;
{$ENDIF}

procedure Int32LSB20ToDouble_FPU(Source: Pointer; Target: PDouble; SampleCount: LongInt); overload; // 32 bit data with 20 bit alignment
{$IFDEF PUREPASCAL}
var
  SourceArray : PntgrArray absolute Source;
  TargetArray : PDblArray absolute Target;
  i           : Integer;
begin
 for i := 0 to SampleCount - 1
  do TargetArray[i] := SourceArray[i]*CInt20ToFloat;
end;
{$ELSE}
asm
  FLD      CInt20ToFloat
@Start:
  FILD     [EAX+4*ECX-4].DWord
  FMUL     ST(0), ST(1)
  FSTP     [EDX+8*ECX-8].Double
  LOOP     @start
  FFREE    ST(0)
end;
{$ENDIF}

procedure Int32LSB24ToSingle_FPU(Source: Pointer; Target: PSingle; SampleCount: LongInt); overload; // 32 bit data with 24 bit alignment
{$IFDEF PUREPASCAL}
var
  SourceArray : PntgrArray absolute Source;
  TargetArray : PSnglArray absolute Target;
  i           : Integer;
begin
 for i := 0 to SampleCount - 1
  do TargetArray[i] := SourceArray[i] * CInt24ToFloat;
end;
{$ELSE}
asm
  FLD      CInt24ToFloat
@Start:
  FILD     [EAX + 4 * ECX - 4].DWord;
  FMUL     ST(0), ST(1)
  FSTP     [EDX + 4 * ECX - 4].Single
  LOOP     @start
  FFREE    ST(0)
end;
{$ENDIF}

procedure Int32LSB24ToDouble_FPU(Source: Pointer; Target: PDouble; SampleCount: LongInt); overload; // 32 bit data with 24 bit alignment
{$IFDEF PUREPASCAL}
var
  SourceArray : PntgrArray absolute Source;
  TargetArray : PDblArray absolute Target;
  i           : Integer;
begin
 for i := 0 to SampleCount - 1
  do TargetArray[i] := SourceArray[i] * CInt24ToFloat;
end;
{$ELSE}
asm
  FLD      CInt24ToFloat
@Start:
  FILD     [EAX+4*ECX-4].DWord
  FMUL     ST(0), ST(1)
  FSTP     [EDX+8*ECX-8].Double
  LOOP     @start
  FFREE    ST(0)
end;
{$ENDIF}

procedure Int16MSBToSingle_FPU(Source: Pointer; Target: PSingle; SampleCount: LongInt); overload;
{$IFDEF PUREPASCAL}
var
  SourceArray : PWrdArray absolute Source;
  TargetArray : PSnglArray absolute Target;
  i           : Integer;
begin
 ReverseEndian4(Source, SampleCount div 2);
 for i := 0 to SampleCount - 1
  do TargetArray[i] := SourceArray[i] * CSmallToFloat;
end;
{$ELSE}
asm
  PUSH EBX
  FLD   CSmallToFloat
 @Start:
  MOV bx, [EAX + 2 * ECX - 2]
  rol bx, $8
  MOV [EAX + 2 * ECX - 2], bx
  FILD  [EAX + 2 * ECX - 2].Word
  FMUL  ST(0), ST(1)
  FSTP  [EDX + 4 * ECX - 4].Single
  LOOP @start
  FFREE ST(0)
  POP EBX
end;
{$ENDIF}

procedure Int16MSBToDouble_FPU(Source: Pointer; Target: PDouble; SampleCount: LongInt); overload;
{$IFDEF PUREPASCAL}
var
  SourceArray : PWrdArray absolute Source;
  TargetArray : PDblArray absolute Target;
  i           : Integer;
begin
 ReverseEndian4(Source, SampleCount div 2);
 for i := 0 to SampleCount - 1
  do TargetArray[i] := SourceArray[i] * CSmallToFloat;
end;
{$ELSE}
asm
  PUSH EBX
  FLD   CSmallToFloat
 @Start:
  MOV bx, [EAX+2*ECX-2]
  rol bx, $8
  MOV [EAX+2*ECX-2], bx
  FILD  [EAX+2*ECX-2].Word
  FMUL  ST(0), ST(1)
  FSTP  [EDX+8*ECX-8].Double
  LOOP @start
  FFREE ST(0)
  POP EBX
end;
{$ENDIF}

procedure Int24MSBToSingle_FPU(Source: Pointer; Target: PSingle; SampleCount: LongInt); overload;
{$IFDEF PUREPASCAL}
begin
 ReverseEndian3(Source, SampleCount);
 Int24LSBToSingle_FPU(Source, Target, SampleCount);
end;
{$ELSE}
asm
 FLD CInt24ToFloat
 PUSH EBX
@Start:
 xor EBX, EBX

 MOV bl, [EAX + 2]
 MOV bh, [EAX + 1]
 ror EBX, 8
 MOV bh, [EAX]
 rol EBX, 8

 MOV [esp-4], EBX
 FILD [esp-4].Single
 FMUL  ST(0), ST(1)
 FSTP [Target].Single
 ADD  EAX, 3
 ADD  Target, 4
 LOOP @Start

 POP EBX
 FFREE ST(0)
end;
{$ENDIF}

procedure Int24MSBToDouble_FPU(Source: Pointer; Target: PDouble; SampleCount: LongInt); overload;
{$IFDEF PUREPASCAL}
begin
 ReverseEndian3(Source, SampleCount);
 Int24LSBToDouble_FPU(Source, Target, SampleCount);
end;
{$ELSE}
asm
 FLD CInt24ToFloat
 PUSH EBX
@Start:
 xor EBX, EBX

 MOV bl, [EAX + 2]
 MOV bh, [EAX + 1]
 ror EBX, 8
 MOV bh, [EAX]
 rol EBX, 8

 MOV [esp-4], EBX
 FILD [esp-4].Single
 FMUL  ST(0), ST(1)
 FSTP [Target].Double
 ADD  EAX, 3
 ADD  Target, 8
 LOOP @Start

 POP EBX
 FFREE ST(0)
end;
{$ENDIF}

procedure Int32MSBToSingle_FPU(Source: Pointer; Target: PSingle; SampleCount: LongInt); overload;
{$IFDEF PUREPASCAL}
begin
 ReverseEndian4(Source, SampleCount);
 Int32LSBToSingle_FPU(Source, Target, SampleCount);
end;
{$ELSE}
asm
 PUSH   EBX
 FLD    CIntToFloat
@Start:
 MOV    EBX, [EAX+4*ECX-4]
 bswap  EBX
 MOV    [EAX+4*ECX-4], EBX
 FILD   [EAX+4*ECX-4].DWord
 FMUL   ST(0), ST(1)
 FSTP   [EDX+4*ECX-4].Single
 LOOP   @start
 FFREE  ST(0)
 POP    EBX
end;
{$ENDIF}

procedure Int32MSBToDouble_FPU(Source: Pointer; Target: PDouble; SampleCount: LongInt); overload;
{$IFDEF PUREPASCAL}
begin
 ReverseEndian4(Source, SampleCount);
 Int32LSBToDouble_FPU(Source, Target, SampleCount);
end;
{$ELSE}
asm
 PUSH   EBX
 FLD    CIntToFloat
@Start:
 MOV    EBX, [EAX+4*ECX-4]
 bswap  EBX
 MOV    [EAX+4*ECX-4], EBX
 FILD   [EAX+4*ECX-4].DWord
 FMUL   ST(0), ST(1)
 FSTP   [EDX+8*ECX-8].Double
 LOOP   @start
 FFREE  ST(0)
 POP    EBX
end;
{$ENDIF}

procedure SingleMSBToSingle_FPU(Source: Pointer; Target: PSingle; SampleCount: LongInt); overload;
{$IFDEF PUREPASCAL}
begin
 move(Source^, Target^, SampleCount*SizeOf(Single));
 ReverseEndian4(Target, SampleCount);
end;
{$ELSE}
asm
 PUSH EBX
 MOV ECX, SampleCount
@Start:
 MOV EBX, [EAX+4*ECX-4]
 bswap EBX
 MOV [EDX+4*ECX-4], EBX
 LOOP @Start
 POP EBX
end;
{$ENDIF}

procedure SingleMSBToDouble_FPU(Source: Pointer; Target: PDouble; SampleCount: LongInt); overload;
{$IFDEF PUREPASCAL}
begin
 ReverseEndian4(Source, SampleCount);
 SingleLSBToDouble_FPU(Source, Target, SampleCount);
end;
{$ELSE}
asm
 PUSH EBX
 @Start:
  MOV EBX, [Source+4*SampleCount-4]
  bswap EBX
  MOV [esp-4], EBX
  FLD   [esp-4].Single
  FSTP  [Target+8*SampleCount-8].Double
  LOOP @Start
 POP EBX
end;
{$ENDIF}

procedure DoubleMSBToSingle_FPU(Source: Pointer; Target: PSingle; SampleCount: LongInt); overload;
begin
 ReverseEndian8(Source, SampleCount);
 DoubleLSBToSingle_FPU(Source, Target, SampleCount);
end;

procedure DoubleMSBToDouble_FPU(Source: Pointer; Target: PDouble; SampleCount: LongInt); overload;
begin
 move(Source^, Target^, SampleCount*SizeOf(Double));
 ReverseEndian8(Target, SampleCount);
end;

procedure Int32MSB16ToSingle_FPU(Source: Pointer; Target: PSingle; SampleCount: LongInt); overload;
{$IFDEF PUREPASCAL}
begin
 ReverseEndian4(Source, SampleCount);
 Int32LSB16ToSingle_FPU(Source, Target, SampleCount);
end;
{$ELSE}
asm
  PUSH     EBX
  FLD      CSmallToFloat
@Start:
  MOV      EBX, [EAX+4*ECX-4]
  bswap    EBX
  MOV      [EAX+4*ECX-4], EBX
  FILD     [EAX+4*ECX-4].DWord
  FMUL     ST(0), ST(1)
  FSTP     [EDX+4*ECX-4].Single
  LOOP     @start
  FFREE    ST(0)
  POP      EBX
end;
{$ENDIF}

procedure Int32MSB16ToDouble_FPU(Source: Pointer; Target: PDouble; SampleCount: LongInt); overload;
{$IFDEF PUREPASCAL}
begin
 ReverseEndian4(Source, SampleCount);
 Int32LSB16ToDouble_FPU(Source, Target, SampleCount);
end;
{$ELSE}
asm
  PUSH     EBX
  FLD      CSmallToFloat
@Start:
  MOV      EBX, [EAX+4*ECX-4]
  bswap    EBX
  MOV      [EAX+4*ECX-4], EBX
  FILD     [EAX+4*ECX-4].DWord
  FMUL     ST(0), ST(1)
  FSTP     [EDX+8*ECX-8].Double
  LOOP     @start
  FFREE    ST(0)
  POP      EBX
end;
{$ENDIF}

procedure Int32MSB18ToSingle_FPU(Source: Pointer; Target: PSingle; SampleCount: LongInt); overload;
{$IFDEF PUREPASCAL}
begin
 ReverseEndian4(Source, SampleCount);
 Int32LSB18ToSingle_FPU(Source, Target, SampleCount);
end;
{$ELSE}
asm
  PUSH     EBX
  FLD      CInt18ToFloat
@Start:
  MOV      EBX, [EAX+4*ECX-4]
  bswap    EBX
  MOV      [EAX+4*ECX-4], EBX
  FILD     [EAX+4*ECX-4].DWord
  FMUL     ST(0), ST(1)
  FSTP     [EDX+4*ECX-4].Single
  LOOP     @start
  FFREE    ST(0)
  POP      EBX
end;
{$ENDIF}

procedure Int32MSB18ToDouble_FPU(Source: Pointer; Target: PDouble; SampleCount: LongInt); overload;
{$IFDEF PUREPASCAL}
begin
 ReverseEndian4(Source, SampleCount);
 Int32LSB18ToDouble_FPU(Source, Target, SampleCount);
end;
{$ELSE}
asm
  PUSH     EBX
  FLD      CInt18ToFloat
@Start:
  MOV      EBX, [EAX+4*ECX-4]
  bswap    EBX
  MOV      [EAX+4*ECX-4], EBX
  FILD     [EAX+4*ECX-4].DWord
  FMUL     ST(0), ST(1)
  FSTP     [EDX+8*ECX-8].Double
  LOOP     @start
  FFREE    ST(0)
  POP      EBX
end;
{$ENDIF}

procedure Int32MSB20ToSingle_FPU(Source: Pointer; Target: PSingle; SampleCount: LongInt); overload;
{$IFDEF PUREPASCAL}
begin
 ReverseEndian4(Source, SampleCount);
 Int32LSB20ToSingle_FPU(Source, Target, SampleCount);
end;
{$ELSE}
asm
  PUSH     EBX
  FLD      CInt20ToFloat
@Start:
  MOV      EBX, [EAX+4*ECX-4]
  bswap    EBX
  MOV      [EAX+4*ECX-4], EBX
  FILD     [EAX+4*ECX-4].DWord
  FMUL     ST(0), ST(1)
  FSTP     [EDX+4*ECX-4].Single
  LOOP     @start
  FFREE    ST(0)
  POP      EBX
end;
{$ENDIF}

procedure Int32MSB20ToDouble_FPU(Source: Pointer; Target: PDouble; SampleCount: LongInt); overload;
{$IFDEF PUREPASCAL}
begin
 ReverseEndian4(Source, SampleCount);
 Int32LSB20ToDouble_FPU(Source, Target, SampleCount);
end;
{$ELSE}
asm
  PUSH     EBX
  FLD      CInt20ToFloat
@Start:
  MOV      EBX, [EAX+4*ECX-4]
  bswap    EBX
  MOV      [EAX+4*ECX-4], EBX
  FILD     [EAX+4*ECX-4].DWord
  FMUL     ST(0), ST(1)
  FSTP     [EDX+8*ECX-8].Double
  LOOP     @start
  FFREE    ST(0)
  POP      EBX
end;
{$ENDIF}

procedure Int32MSB24ToSingle_FPU(Source: Pointer; Target: PSingle; SampleCount: LongInt); overload;
{$IFDEF PUREPASCAL}
begin
 ReverseEndian4(Source, SampleCount);
 Int32LSB24ToSingle_FPU(Source, Target, SampleCount);
end;
{$ELSE}
asm
  PUSH     EBX
  FLD      CInt24ToFloat
@Start:
  MOV      EBX, [EAX+4*ECX-4]
  bswap    EBX
  MOV      [EAX+4*ECX-4], EBX
  FILD     [EAX+4*ECX-4].DWord
  FMUL     ST(0), ST(1)
  FSTP     [EDX+4*ECX-4].Single
  LOOP     @start
  FFREE    ST(0)
  POP      EBX
end;
{$ENDIF}

procedure Int32MSB24ToDouble_FPU(Source: Pointer; Target: PDouble; SampleCount: LongInt); overload;
{$IFDEF PUREPASCAL}
begin
 ReverseEndian4(Source, SampleCount);
 Int32LSB24ToDouble_FPU(Source, Target, SampleCount);
end;
{$ELSE}
asm
  PUSH     EBX
  FLD      CInt24ToFloat
@Start:
  MOV      EBX, [EAX+4*ECX-4]
  bswap    EBX
  MOV      [EAX+4*ECX-4], EBX
  FILD     [EAX+4*ECX-4].DWord
  FMUL     ST(0), ST(1)
  FSTP     [EDX+8*ECX-8].Double
  LOOP     @start
  FFREE    ST(0)
  POP      EBX
end;
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////

procedure SingleToInt16LSB_FPU(Source: PSingle; Target: Pointer; SampleCount: LongInt); overload;
asm
  FLD      CFloatToSmall    // move to register for speed
@Start:                // Samplecount already in ECX!
  FLD      [EAX + 4 * ECX - 4].Single
  FMUL     ST(0), ST(1)
  fistp    Word ptr [EDX + 2 * ECX - 2]
  LOOP     @start
  FFREE    ST(0)       // free after LOOP has finished
end;

procedure SingleToInt16LSB_UDF_FPU(Source: PSingle; Target: Pointer; SampleCount: LongInt); overload;
const
  CScaler: Double = ((1.0/$10000) / $10000);  // 2^-32
asm
 {$IFDEF CPUx86_64}
 PUSH   RBX
 FLD    CScaler                // move to register for speed
 FLD    CFloatToSmall              // move to register for speed
 @Start:                       // Samplecount already in ECX!
  FLD   [RAX + 4 * ECX - 4].Single
  FMUL  ST(0), ST(1)

  IMUL  EBX, RandSeed, $08088405
  INC   EBX
  MOV   RandSeed, EBX
  FILD  RandSeed
  FMUL  ST(0), ST(3)
  FADDP

  fistp Word ptr [EDX + 2 * ECX - 2]
 LOOP   @start
 FFREE  ST(0)                // free after LOOP has finished
 FFREE  ST(1)                // free after LOOP has finished
 POP    RBX
 {$ELSE}
 PUSH   EBX
 FLD    CScaler                // move to register for speed
 FLD    CFloatToSmall              // move to register for speed
 @Start:                       // Samplecount already in ECX!
  FLD   [EAX + 4 * ECX - 4].Single
  FMUL  ST(0), ST(1)

  IMUL  EBX, RandSeed, $08088405
  INC   EBX
  MOV   RandSeed, EBX
  FILD  RandSeed
  FMUL  ST(0), ST(3)
  FADDP

  fistp Word ptr [EDX + 2 * ECX - 2]
 LOOP   @start
 FFREE  ST(0)                // free after LOOP has finished
 FFREE  ST(1)                // free after LOOP has finished
 POP    EBX
 {$ENDIF}
end;

procedure SingleToInt16LSB_TDF_FPU(Source: PSingle; Target: Pointer; SampleCount: LongInt); overload;
const
  CScaler: Double = ((0.5/$10000) / $10000);  // 2^-32
asm
 {$IFDEF CPUx86_64}
 PUSH   RBX
 FLD    CScaler                // move to register for speed
 FLD    CFloatToSmall              // move to register for speed
 @Start:                       // Samplecount already in ECX!
  FLD   [EAX + 4 * ECX - 4].Single
  FMUL  ST(0), ST(1)

  IMUL  RBX, RandSeed, $08088405
  INC   RBX
  MOV   RandSeed, Rbx
  FILD  RandSeed
  IMUL  Rbx, RandSeed, $08088405
  INC   Rbx
  MOV   RandSeed, Rbx
  FILD  RandSeed
  FADDP
  FMUL  ST(0), ST(3)
  FADDP

  fistp Word ptr [EDX + 2 * ECX - 2]
 LOOP   @start
 FFREE  ST(0)                // free after LOOP has finished
 FFREE  ST(1)                // free after LOOP has finished
 POP    RBX
 {$ELSE}
 PUSH   EBX
 FLD    CScaler                // move to register for speed
 FLD    CFloatToSmall              // move to register for speed
 @Start:                       // Samplecount already in ECX!
  FLD   [EAX + 4 * ECX - 4].Single
  FMUL  ST(0), ST(1)

  IMUL  EBX, RandSeed, $08088405
  INC   EBX
  MOV   RandSeed, EBX
  FILD  RandSeed
  IMUL  EBX, RandSeed, $08088405
  INC   EBX
  MOV   RandSeed, EBX
  FILD  RandSeed
  FADDP
  FMUL  ST(0), ST(3)
  FADDP

  fistp Word ptr [EDX + 2 * ECX - 2]
 LOOP   @start
 FFREE  ST(0)                // free after LOOP has finished
 FFREE  ST(1)                // free after LOOP has finished
 POP    EBX
 {$ENDIF}
end;

procedure DoubleToInt16LSB_FPU(Source: PDouble; Target: Pointer; SampleCount: LongInt); overload;
asm
  FLD      CFloatToSmall    // move to register for speed
@Start:                // Samplecount already in ECX!
  FLD      [EAX + 8 * ECX - 8].Double
  FMUL     ST(0), ST(1)
  fistp    Word ptr [EDX + 2 * ECX - 2]
  LOOP     @start
  FFREE    ST(0)       // free after LOOP has finished
end;

procedure DoubleToInt16LSB_UDF_FPU(Source: PDouble; Target: Pointer; SampleCount: LongInt); overload;
const Scaler: Double = ((1.0/$10000) / $10000);  // 2^-32
asm
{$IFDEF CPUx86_64}
{$ELSE}
  PUSH    EBX
  FLD     Scaler               // move to register for speed
  FLD     CFloatToSmall        // move to register for speed
@Start:                        // Samplecount already in ECX!
  FLD     [EAX + 8 * ECX - 8].Double
  FMUL    ST(0), ST(1)

  IMUL    EBX, RandSeed, $08088405
  INC     EBX
  MOV     RandSeed, EBX
  FILD    RandSeed
  FMUL    ST(0), ST(3)
  FADDP

  FISTP   Word PTR [EDX + 2 * ECX - 2]
  LOOP    @Start
  FSTP    ST(0)                // free after LOOP has finished
  FSTP    ST(0)                // free after LOOP has finished
  POP     EBX
{$ENDIF}
end;

procedure DoubleToInt16LSB_TDF_FPU(Source: PDouble; Target: Pointer; SampleCount: LongInt); overload;
const Scaler: Double = ((0.5/$10000) / $10000);  // 2^-32
asm
  PUSH    EBX
  FLD     Scaler              // move to register for speed
  FLD     CFloatToSmall            // move to register for speed
@Start:                       // Samplecount already in ECX!
  FLD     [EAX + 8 * ECX - 8].Double
  FMUL    ST(0), ST(1)

  IMUL    EBX, RandSeed, $08088405
  INC     EBX
  MOV     RandSeed, EBX
  FILD    RandSeed
  IMUL    EBX, RandSeed, $08088405
  INC     EBX
  MOV     RandSeed, EBX
  FILD    RandSeed
  FADDP
  FMUL    ST(0), ST(3)
  FADDP

  fistp   Word ptr [EDX + 2 * ECX - 2]
 LOOP     @start
 FSTP     ST(0)                // free after LOOP has finished
 FSTP     ST(0)                // free after LOOP has finished
 POP      EBX
end;



procedure SingleToInt24LSB_FPU(Source: PSingle; Target: Pointer; SampleCount: LongInt); overload;
{$IFDEF PUREPASCAL}
var
  SourceArray : PSnglArray absolute Source;
  TargetInt   : PByte absolute Target;
  i           : Integer;
begin
 for i := 0 to SampleCount - 1 do
  begin
   PInteger(TargetInt)^ := Round(SourceArray^[i] * CFloatToInt24);
   Inc(TargetInt, 3);
  end;
end;
{$ELSE}
asm
  PUSH EBX
  FLD   CFloatToInt24         //for speed
 @Start:
  FLD   [EAX].Single
  FMUL  ST(0), ST(1)
  fistp [esp - 4].DWord
  MOV   EBX, [esp - 4]        //  EBX = EHEL BHBL
  AND   EBX, $FFFFFF          //  EBX = 00EL BHBL
  MOV   [EDX], EBX            // [EDX] = 00EL BHBL

  ADD   EAX, 4
  ADD   EDX, 3
  DEC   ECX
  JNZ   @Start
  FSTP     ST(0)               // free after LOOP has finished
  POP EBX
end;
{$ENDIF}

procedure SingleToInt24LSB_UDF_FPU(Source: PSingle; Target: Pointer; SampleCount: LongInt); overload;
const
  CScaler : Double = ((1/$10000) / $10000);  // 2^-32
asm
  PUSH  EBX
  FLD   CScaler                // move to register for speed
  FLD   CFloatToInt24                 // for speed
 @Start:
  FLD   [EAX].DWord
  FMUL  ST(0), ST(1)

  IMUL  EBX, RandSeed, $08088405
  INC   EBX
  MOV   RandSeed, EBX
  FILD  RandSeed
  FMUL  CScaler.Double
  FADDP

  fistp [esp - 4].Single
  MOV   EBX, [esp - 4]
  MOV   [EDX], bx
  ror   EBX, 8
  MOV   [EDX + 2], bh
  ADD   EAX, 4
  ADD   EDX, 3
  DEC   ECX
  JNZ   @Start
  FSTP  ST(0)                  // free after LOOP has finished
  FSTP  ST(0)                  // free after LOOP has finished
  POP   EBX
end;

procedure SingleToInt24LSB_TDF_FPU(Source: PSingle; Target: Pointer; SampleCount: LongInt); overload;
const
  CScaler: Double = ((0.5/$10000) / $10000);  // 2^-32
asm
  PUSH  EBX
  FLD   CScaler                 // move to register for speed
  FLD   CFloatToInt24                  //for speed
 @Start:
  FLD   [EAX].Single
  FMUL  ST(0), ST(1)

  IMUL  EBX, RandSeed, $08088405
  INC   EBX
  MOV   RandSeed, EBX
  FILD  RandSeed
  IMUL  EBX, RandSeed, $08088405
  INC   EBX
  MOV   RandSeed, EBX
  FILD  RandSeed
  FADDP
  FMUL  ST(0), ST(3)
  FADDP

  fistp [esp-4].DWord;
  MOV   EBX, [esp - 4]
  AND   EBX, $FFFFFF
  MOV   [EDX], EBX
  ADD   EAX, 4
  ADD   EDX, 3
  DEC   ECX
  JNZ   @Start
  FSTP  ST(0)                   // free after LOOP has finished
  FSTP  ST(0)                   // free after LOOP has finished
  POP EBX
end;

procedure DoubleToInt24LSB_FPU(Source: PDouble; Target: Pointer; SampleCount: LongInt); overload;
asm
  PUSH  EBX
  FLD   CFloatToInt24         //for speed
 @Start:
  FLD   [EAX].Double
  FMUL  ST(0), ST(1)
  fistp [esp - 4].DWord
  MOV   EBX, [esp - 4]
  AND   EBX, $FFFFFF
  MOV   [EDX], EBX
  ADD   EAX, 8
  ADD   EDX, 3
  DEC   ECX
  JNZ   @Start
  FFREE ST(0)
  POP   EBX
end;

procedure DoubleToInt24LSB_UDF_FPU(Source: PDouble; Target: Pointer; SampleCount: LongInt); overload;
const
  CScaler: Double = ((1/$10000) / $10000);  // 2^-32
asm
  PUSH  EBX
  FLD   CScaler               // move to register for speed
  FLD   CFloatToInt24                // for speed
 @Start:
  FLD   [EAX].Double
  FMUL  ST(0), ST(1)

  IMUL  EBX, RandSeed, $08088405
  INC   EBX
  MOV   RandSeed, EBX
  FILD  RandSeed
  FMUL  CScaler.Double
  FADDP

  fistp [esp - 4].DWord;
  MOV   EBX, [esp - 4]
  AND   EBX, $FFFFFF
  MOV   [EDX], EBX
  ADD   EAX, 8
  ADD   EDX, 3
  DEC   ECX
  JNZ   @Start
  FSTP  ST(0)                 // free after LOOP has finished
  FSTP  ST(0)                 // free after LOOP has finished
  POP   EBX
end;

procedure DoubleToInt24LSB_TDF_FPU(Source: PDouble; Target: Pointer; SampleCount: LongInt); overload;
const
  CScaler: Double = ((0.5/$10000) / $10000);  // 2^-32
asm
  PUSH  EBX
  FLD   CScaler                 // move to register for speed
  FLD   CFloatToInt24                  //for speed
 @Start:
  FLD   [EAX].Double
  FMUL  ST(0), ST(1)

  IMUL  EBX, RandSeed, $08088405
  INC   EBX
  MOV   RandSeed, EBX
  FILD  RandSeed
  IMUL  EBX, RandSeed, $08088405
  INC   EBX
  MOV   RandSeed, EBX
  FILD  RandSeed
  FADDP
  FMUL  ST(0), ST(3)
  FADDP

  fistp [esp - 4].Integer
  MOV   EBX, [esp - 4]
  AND   EBX, $FFFFFF
  MOV   [EDX], EBX
  ADD   EAX, 8
  ADD   EDX, 3
  DEC   ECX
  JNZ   @Start
  FSTP  ST(0)               // free after LOOP has finished
  FSTP  ST(0)               // free after LOOP has finished
  POP   EBX
end;

procedure SingleToInt32LSB_FPU(Source: PSingle; Target: Pointer; SampleCount: LongInt); overload;
asm
  FLD   CFloatToInt         //for speed
 @Start:
  FLD   [EAX + 4 * ECX - 4].Single
  FMUL  ST(0), ST(1)
  fistp [EDX + 4 * ECX - 4].Integer
  LOOP  @Start
  FFREE ST(0)
end;

procedure DoubleToInt32LSB_FPU(Source: PDouble; Target: Pointer; SampleCount: LongInt); overload;
asm
  FLD   CFloatToInt         //for speed
 @Start:
  FLD   [EAX + 8 * ECX - 8].Double
  FMUL  ST(0), ST(1)
  fistp [EDX + 4 * ECX - 4].Integer
  LOOP  @Start
  FFREE ST(0)
end;

procedure SingleToSingleLSB_FPU(Source: PSingle; Target: Pointer; SampleCount: LongInt); overload;
begin
 Move(Source^, Target^, SampleCount * SizeOf(Single));
end;

procedure DoubleToSingleLSB_FPU(Source: PDouble; Target: Pointer; SampleCount: LongInt); overload;
asm
 @Start:
  FLD   [Source + 8 * ECX - 8].Double
  FSTP  [Target + 4 * ECX - 4].Single
  LOOP @Start
end;

procedure SingleToDoubleLSB_FPU(Source: PSingle; Target: Pointer; SampleCount: LongInt); overload;
asm
 @Start:
  FLD   [EAX + 4 * ECX - 4].Single
  FSTP  [EDX + 8 * ECX - 8].Double
  LOOP @Start
end;

procedure DoubleToDoubleLSB_FPU(Source: PDouble; Target: Pointer; SampleCount: LongInt); overload;
begin
 Move(Source^, Target^, SampleCount * SizeOf(Double));
end;

procedure SingleToInt32LSB16_FPU(Source: PSingle; Target: Pointer; SampleCount: LongInt); overload;
asm
  FLD      CFloatToSmall
@Start:
  FLD      [EAX + 4 * ECX - 4].Single
  FMUL     ST(0), ST(1)
  fistp    [EDX + 4 * ECX - 4].DWord
  LOOP @Start
  FFREE    ST(0)
end;

procedure SingleToInt32LSB16_UDF_FPU(Source: PSingle; Target: Pointer; SampleCount: LongInt); overload;
const
  Scaler: Double = ((1.0 / $10000) / $10000);  // 2^-32
asm
 PUSH EBX
 FLD    Scaler                 // move to register for speed
 FLD    CFloatToSmall               // move to register for speed
 @Start:                       // Samplecount already in ECX!
  FLD     [EAX + 4 * ECX - 4].Single
  FMUL    ST(0), ST(1)

  IMUL    EBX, RandSeed, $08088405
  INC     EBX
  MOV     RandSeed, EBX
  FILD    RandSeed
  FMUL    ST(0), ST(3)
  FADDP

  fistp   [EDX+4*ECX-4].DWord
 LOOP     @start
 FFREE    ST(0)                // free after LOOP has finished
 FFREE    ST(1)                // free after LOOP has finished
 POP EBX
end;

procedure SingleToInt32LSB16_TDF_FPU(Source: PSingle; Target: Pointer; SampleCount: LongInt); overload;
const Scaler: Double = ((0.5/$10000) / $10000);  // 2^-32
asm
 PUSH   EBX
 FLD    Scaler                 // move to register for speed
 FLD    CFloatToSmall               // move to register for speed
 @Start:                       // Samplecount already in ECX!
  FLD   [EAX + 4 * ECX - 4].Single
  FMUL  ST(0), ST(1)

  IMUL  EBX, RandSeed, $08088405
  INC   EBX
  MOV   RandSeed, EBX
  FILD  RandSeed
  IMUL  EBX, RandSeed, $08088405
  INC   EBX
  MOV   RandSeed, EBX
  FILD  RandSeed
  FADDP
  FMUL  ST(0), ST(3)
  FADDP

  fistp [EDX + 4 * ECX - 4].DWord
 LOOP   @start
 FFREE  ST(0)                // free after LOOP has finished
 FFREE  ST(1)                // free after LOOP has finished
 POP    EBX
end;

procedure DoubleToInt32LSB16_FPU(Source: PDouble; Target: Pointer; SampleCount: LongInt); overload;
asm
  FLD   CFloatToSmall
@Start:
  FLD   [EAX + 8 * ECX - 8].Double
  FMUL  ST(0), ST(1)
  fistp [EDX+4*ECX-4].DWord
  LOOP  @Start
  FFREE ST(0)
end;

procedure DoubleToInt32LSB16_UDF_FPU(Source: PDouble; Target: Pointer; SampleCount: LongInt); overload;
const
  CScaler: Double = ((1.0/$10000) / $10000);  // 2^-32
asm
 PUSH   EBX
 FLD    CScaler                 // move to register for speed
 FLD    CFloatToSmall               // move to register for speed
 @Start:                        // Samplecount already in ECX!
  FLD   [EAX + 8 * ECX - 8].Double
  FMUL  ST(0), ST(1)

  IMUL  EBX, RandSeed, $08088405
  INC   EBX
  MOV   RandSeed, EBX
  FILD  RandSeed
  FMUL  ST(0), ST(3)
  FADDP

  fistp [EDX+4*ECX-4].DWord
 LOOP   @start
 FFREE  ST(0)                // free after LOOP has finished
 FFREE  ST(1)                // free after LOOP has finished
 POP    EBX
end;

procedure DoubleToInt32LSB16_TDF_FPU(Source: PDouble; Target: Pointer; SampleCount: LongInt); overload;
const
  CScaler: Double = ((0.5/$10000) / $10000);  // 2^-32
asm
 PUSH   EBX
 FLD    CScaler                 // move to register for speed
 FLD    CFloatToSmall               // move to register for speed
 @Start:                       // Samplecount already in ECX!
  FLD   [EAX + 8 * ECX - 8].Double
  FMUL  ST(0), ST(1)

  IMUL  EBX, RandSeed, $08088405
  INC   EBX
  MOV   RandSeed, EBX
  FILD  RandSeed
  IMUL  EBX, RandSeed, $08088405
  INC   EBX
  MOV   RandSeed, EBX
  FILD  RandSeed
  FADDP
  FMUL  ST(0), ST(3)
  FADDP

  fistp [EDX + 4 * ECX - 4].DWord
 LOOP   @start
 FFREE  ST(0)                // free after LOOP has finished
 FFREE  ST(1)                // free after LOOP has finished
 POP    EBX
end;

procedure SingleToInt32LSB18_FPU(Source: PSingle; Target: Pointer; SampleCount: LongInt); overload;
asm
  FLD   CFloatToInt18
 @Start:
  FLD   [EAX + 4 * ECX - 4].Single
  FMUL  ST(0), ST(1)
  fistp [EDX + 4 * ECX - 4].DWord
  LOOP  @Start
  FFREE ST(0)
end;

procedure SingleToInt32LSB18_UDF_FPU(Source: PSingle; Target: Pointer; SampleCount: LongInt); overload;
const
  CScaler : Double = ((1.0/$10000) / $10000);  // 2^-32
asm
 PUSH   EBX
 FLD    CScaler                 // move to register for speed
 FLD    CFloatToInt18                  // move to register for speed
 @Start:                        // Samplecount already in ECX!
  FLD   [EAX + 4 * ECX - 4].Single
  FMUL  ST(0), ST(1)

  IMUL  EBX, RandSeed, $08088405
  INC   EBX
  MOV   RandSeed, EBX
  FILD  RandSeed
  FMUL  ST(0), ST(3)
  FADDP

  fistp [EDX + 4 * ECX - 4].DWord
 LOOP   @start
 FFREE  ST(0)                // free after LOOP has finished
 FFREE  ST(1)                // free after LOOP has finished
 POP    EBX
end;

procedure SingleToInt32LSB18_TDF_FPU(Source: PSingle; Target: Pointer; SampleCount: LongInt); overload;
const
  CScaler : Double = ((0.5/$10000) / $10000);  // 2^-32
asm
 PUSH   EBX
 FLD    CScaler                // move to register for speed
 FLD    CFloatToInt18                 // move to register for speed
 @Start:                       // Samplecount already in ECX!
  FLD   [EAX + 4 * ECX - 4].Single
  FMUL  ST(0), ST(1)

  IMUL  EBX, RandSeed, $08088405
  INC   EBX
  MOV   RandSeed, EBX
  FILD  RandSeed
  IMUL  EBX, RandSeed, $08088405
  INC   EBX
  MOV   RandSeed, EBX
  FILD  RandSeed
  FADDP
  FMUL  ST(0), ST(3)
  FADDP

  fistp [EDX + 4 * ECX - 4].DWord
 LOOP   @start
 FFREE  ST(0)                // free after LOOP has finished
 FFREE  ST(1)                // free after LOOP has finished
 POP    EBX
end;

procedure DoubleToInt32LSB18_FPU(Source: PDouble; Target: Pointer; SampleCount: LongInt); overload;
asm
  FLD   CFloatToInt18
 @Start:
  FLD   [EAX + 8 * ECX - 8].Double
  FMUL  ST(0), ST(1)
  fistp [EDX+4*ECX-4].DWord
  LOOP @Start
  FFREE ST(0)
end;

procedure DoubleToInt32LSB18_UDF_FPU(Source: PDouble; Target: Pointer; SampleCount: LongInt); overload;
const Scaler: Double = ((1.0/$10000) / $10000);  // 2^-32
asm
 PUSH   EBX
 FLD    Scaler                 // move to register for speed
 FLD    CFloatToInt18                  // move to register for speed
 @Start:                       // Samplecount already in ECX!
  FLD   [EAX + 8 * ECX - 8].Double
  FMUL  ST(0), ST(1)

  IMUL  EBX, RandSeed, $08088405
  INC   EBX
  MOV   RandSeed, EBX
  FILD  RandSeed
  FMUL  ST(0), ST(3)
  FADDP

  fistp [EDX + 4 * ECX - 4].DWord
 LOOP   @start
 FFREE  ST(0)                // free after LOOP has finished
 FFREE  ST(1)                // free after LOOP has finished
 POP    EBX
end;

procedure DoubleToInt32LSB18_TDF_FPU(Source: PDouble; Target: Pointer; SampleCount: LongInt); overload;
const
  CScaler: Double = ((0.5/$10000) / $10000);  // 2^-32
asm
 PUSH   EBX
 FLD    CScaler                 // move to register for speed
 FLD    CFloatToInt18                  // move to register for speed
 @Start:                        // Samplecount already in ECX!
  FLD   [EAX + 8 * ECX - 8].Double
  FMUL  ST(0), ST(1)

  IMUL  EBX, RandSeed, $08088405
  INC   EBX
  MOV   RandSeed, EBX
  FILD  RandSeed
  IMUL  EBX, RandSeed, $08088405
  INC   EBX
  MOV   RandSeed, EBX
  FILD  RandSeed
  FADDP
  FMUL  ST(0), ST(3)
  FADDP

  fistp [EDX + 4 * ECX - 4].DWord
 LOOP   @start
 FFREE  ST(0)                   // free after LOOP has finished
 FFREE  ST(1)                   // free after LOOP has finished
 POP    EBX
end;

procedure SingleToInt32LSB20_FPU(Source: PSingle; Target: Pointer; SampleCount: LongInt); overload;
asm
  FLD   CFloatToInt20
 @Start:
  FLD   [EAX + 4 * ECX - 4].Single
  FMUL  ST(0), ST(1)
  fistp [EDX + 4 * ECX - 4].DWord
  LOOP  @Start
  FFREE ST(0)
end;

procedure SingleToInt32LSB20_UDF_FPU(Source: PSingle; Target: Pointer; SampleCount: LongInt); overload;
const
  CScaler: Double = ((1.0/$10000) / $10000);  // 2^-32
asm
 PUSH   EBX
 FLD    CScaler                // move to register for speed
 FLD    CFloatToInt20                  // move to register for speed
 @Start:                       // Samplecount already in ECX!
  FLD   [EAX + 4 * ECX - 4].Single
  FMUL  ST(0), ST(1)

  IMUL  EBX, RandSeed, $08088405
  INC   EBX
  MOV   RandSeed, EBX
  FILD  RandSeed
  FMUL  ST(0), ST(3)
  FADDP

  fistp [EDX + 4 * ECX - 4].DWord
 LOOP   @start
 FFREE  ST(0)                // free after LOOP has finished
 FFREE  ST(1)                // free after LOOP has finished
 POP    EBX
end;

procedure SingleToInt32LSB20_TDF_FPU(Source: PSingle; Target: Pointer; SampleCount: LongInt); overload;
const
  CScaler: Double = ((0.5/$10000) / $10000);  // 2^-32
asm
 PUSH   EBX
 FLD    CScaler                // move to register for speed
 FLD    CFloatToInt20                 // move to register for speed
 @Start:                       // Samplecount already in ECX!
  FLD   [EAX + 4 * ECX - 4].Single
  FMUL  ST(0), ST(1)

  IMUL  EBX, RandSeed, $08088405
  INC   EBX
  MOV   RandSeed, EBX
  FILD  RandSeed
  IMUL  EBX, RandSeed, $08088405
  INC   EBX
  MOV   RandSeed, EBX
  FILD  RandSeed
  FADDP
  FMUL  ST(0), ST(3)
  FADDP

  fistp [EDX + 4 * ECX - 4].DWord
 LOOP   @start
 FFREE  ST(0)                // free after LOOP has finished
 FFREE  ST(1)                // free after LOOP has finished
 POP    EBX
end;

procedure DoubleToInt32LSB20_FPU(Source: PDouble; Target: Pointer; SampleCount: LongInt); overload;
asm
  FLD   CFloatToInt20
 @Start:
  FLD   [EAX + 8 * ECX - 8].Double
  FMUL  ST(0), ST(1)
  fistp [EDX + 4 * ECX - 4].DWord
  LOOP  @Start
  FFREE ST(0)
end;

procedure DoubleToInt32LSB20_UDF_FPU(Source: PDouble; Target: Pointer; SampleCount: LongInt); overload;
const
  CScaler: Double = ((1.0/$10000) / $10000);  // 2^-32
asm
 PUSH   EBX
 FLD    CScaler                // move to register for speed
 FLD    CFloatToInt20                 // move to register for speed
 @Start:                       // Samplecount already in ECX!
  FLD   [EAX + 8 * ECX - 8].Double
  FMUL  ST(0), ST(1)

  IMUL  EBX, RandSeed, $08088405
  INC   EBX
  MOV   RandSeed, EBX
  FILD  RandSeed
  FMUL  ST(0), ST(3)
  FADDP

  fistp   [EDX + 4 * ECX - 4].DWord
 LOOP     @start
 FFREE    ST(0)                // free after LOOP has finished
 FFREE    ST(1)                // free after LOOP has finished
 POP EBX
end;

procedure DoubleToInt32LSB20_TDF_FPU(Source: PDouble; Target: Pointer; SampleCount: LongInt); overload;
const Scaler: Double = ((0.5/$10000) / $10000);  // 2^-32
asm
 PUSH EBX
 FLD    Scaler                 // move to register for speed
 FLD    CFloatToInt20                  // move to register for speed
 @Start:                       // Samplecount already in ECX!
  FLD      [EAX + 8 * ECX - 8].Double
  FMUL     ST(0), ST(1)

  IMUL  EBX, RandSeed, $08088405
  INC   EBX
  MOV RandSeed, EBX
  FILD  RandSeed
  IMUL  EBX, RandSeed, $08088405
  INC   EBX
  MOV RandSeed, EBX
  FILD  RandSeed
  FADDP
  FMUL ST(0), ST(3)
  FADDP

  fistp   [EDX+4*ECX-4].DWord
 LOOP     @start
 FFREE    ST(0)                // free after LOOP has finished
 FFREE    ST(1)                // free after LOOP has finished
 POP EBX
end;

procedure SingleToInt32LSB24_FPU(Source: PSingle; Target: Pointer; SampleCount: LongInt); overload;
asm
  FLD   CFloatToInt24
 @Start:
  FLD   [EAX+4*ECX-4].Single
  FMUL  ST(0), ST(1)
  fistp [EDX+4*ECX-4].DWord
  LOOP @Start
  FFREE ST(0)
end;

procedure SingleToInt32LSB24_UDF_FPU(Source: PSingle; Target: Pointer; SampleCount: LongInt); overload;
const Scaler: Double = ((1.0/$10000) / $10000);  // 2^-32
asm
 PUSH EBX
 FLD    Scaler                 // move to register for speed
 FLD    CFloatToInt24                  // move to register for speed
 @Start:                       // Samplecount already in ECX!
  FLD      [EAX+4*ECX-4].Single
  FMUL     ST(0), ST(1)

  IMUL  EBX, RandSeed, $08088405
  INC   EBX
  MOV RandSeed, EBX
  FILD  RandSeed
  FMUL ST(0), ST(3)
  FADDP

  fistp   [EDX+4*ECX-4].DWord
 LOOP     @start
 FFREE    ST(0)                // free after LOOP has finished
 FFREE    ST(1)                // free after LOOP has finished
 POP EBX
end;

procedure SingleToInt32LSB24_TDF_FPU(Source: PSingle; Target: Pointer; SampleCount: LongInt); overload;
const Scaler: Double = ((0.5/$10000) / $10000);  // 2^-32
asm
 PUSH EBX
 FLD    Scaler                 // move to register for speed
 FLD    CFloatToInt24                  // move to register for speed
 @Start:                       // Samplecount already in ECX!
  FLD      [EAX+4*ECX-4].Single
  FMUL     ST(0), ST(1)

  IMUL  EBX, RandSeed, $08088405
  INC   EBX
  MOV RandSeed, EBX
  FILD  RandSeed
  IMUL  EBX, RandSeed, $08088405
  INC   EBX
  MOV RandSeed, EBX
  FILD  RandSeed
  FADDP
  FMUL ST(0), ST(3)
  FADDP

  fistp   [EDX+4*ECX-4].DWord
 LOOP     @start
 FFREE    ST(0)                // free after LOOP has finished
 FFREE    ST(1)                // free after LOOP has finished
 POP EBX
end;

procedure DoubleToInt32LSB24_FPU(Source: PDouble; Target: Pointer; SampleCount: LongInt); overload;
asm
  FLD   CFloatToInt24
 @Start:
  FLD   [EAX + 8 * ECX - 8].Double
  FMUL  ST(0), ST(1)
  fistp [EDX+4*ECX-4].DWord
  LOOP @Start
  FFREE ST(0)
end;

procedure DoubleToInt32LSB24_UDF_FPU(Source: PDouble; Target: Pointer; SampleCount: LongInt); overload;
const Scaler: Double = ((1.0/$10000) / $10000);  // 2^-32
asm
 PUSH EBX
 FLD    Scaler                 // move to register for speed
 FLD    CFloatToInt24                  // move to register for speed
 @Start:                       // Samplecount already in ECX!
  FLD      [EAX + 8 * ECX - 8].Double
  FMUL     ST(0), ST(1)

  IMUL  EBX, RandSeed, $08088405
  INC   EBX
  MOV RandSeed, EBX
  FILD  RandSeed
  FMUL ST(0), ST(3)
  FADDP

  fistp   [EDX+4*ECX-4].DWord
 LOOP     @start
 FFREE    ST(0)                // free after LOOP has finished
 FFREE    ST(1)                // free after LOOP has finished
 POP EBX
end;

procedure DoubleToInt32LSB24_TDF_FPU(Source: PDouble; Target: Pointer; SampleCount: LongInt); overload;
const Scaler: Double = ((0.5/$10000) / $10000);  // 2^-32
asm
 PUSH EBX
 FLD    Scaler                 // move to register for speed
 FLD    CFloatToInt24                  // move to register for speed
 @Start:                       // Samplecount already in ECX!
  FLD   [EAX + 8 * ECX - 8].Double
  FMUL  ST(0), ST(1)

  IMUL  EBX, RandSeed, $08088405
  INC   EBX
  MOV   RandSeed, EBX
  FILD  RandSeed
  IMUL  EBX, RandSeed, $08088405
  INC   EBX
  MOV   RandSeed, EBX
  FILD  RandSeed
  FADDP
  FMUL  ST(0), ST(3)
  FADDP

  fistp   [EDX+4*ECX-4].DWord
 LOOP     @start
 FFREE    ST(0)                // free after LOOP has finished
 FFREE    ST(1)                // free after LOOP has finished
 POP EBX
end;

procedure SingleToInt16MSB_FPU(Source: PSingle; Target: Pointer; SampleCount: LongInt); overload;
asm
   PUSH EBX
   FLD      CFloatToSmall
 @Start:
   FLD      [EAX+4*ECX-4].Single
   FMUL     ST(0), ST(1)
   fistp    Word ptr [EDX+2*ECX-2]
   MOV      bx, [EDX+2*ECX-2]
   rol      bx, $8
   MOV      [EDX+2*ECX-2], bx
   LOOP @Start
   FFREE    ST(0)
   POP EBX
end;

procedure DoubleToInt16MSB_FPU(Source: PDouble; Target: Pointer; SampleCount: LongInt); overload;
asm
   PUSH EBX
   FLD      CFloatToSmall
 @Start:
   FLD      [EAX + 8 * ECX - 8].Double
   FMUL     ST(0), ST(1)
   fistp    Word ptr [EDX+2*ECX-2]
   MOV      bx, [EDX+2*ECX-2]
   rol      bx, $8
   MOV      [EDX+2*ECX-2], bx
   LOOP @Start
   FFREE    ST(0)
   POP EBX
end;

procedure SingleToInt24MSB_FPU(Source: PSingle; Target: Pointer; SampleCount: LongInt); overload;
begin
 SingleToInt24LSB_FPU(Source, Target, SampleCount);
 ReverseEndian3(Target, SampleCount);
end;

procedure DoubleToInt24MSB_FPU(Source: PDouble; Target: Pointer; SampleCount: LongInt); overload;
begin
 DoubleToInt24LSB_FPU(Source, Target, SampleCount);
 ReverseEndian3(Target, SampleCount);
end;

procedure SingleToInt32MSB_FPU(Source: PSingle; Target: Pointer; SampleCount: LongInt); overload;
asm
  PUSH EBX
  FLD   CFloatToInt         //for speed
 @Start:
  FLD   [EAX+4*ECX-4].Single
  FMUL  ST(0), ST(1)
  fistp [EDX+4*ECX-4].DWord;
  MOV EBX, [EDX+4*ECX-4]
  bswap EBX
  MOV [EDX+4*ECX-4], EBX
  LOOP @Start
  FFREE ST(0)
  POP EBX
end;

procedure DoubleToInt32MSB_FPU(Source: PDouble; Target: Pointer; SampleCount: LongInt); overload;
asm
  PUSH EBX
  FLD   CFloatToInt         //for speed
 @Start:
  FLD   [EAX + 8 * ECX - 8].Double
  FMUL  ST(0), ST(1)
  fistp [EDX+4*ECX-4].DWord
  MOV EBX, [EDX+4*ECX-4]
  bswap EBX
  MOV [EDX+4*ECX-4], EBX
  LOOP @Start
  FFREE ST(0)
  POP EBX
end;

procedure SingleToSingleMSB_FPU(Source: PSingle; Target: Pointer; SampleCount: LongInt); overload;
begin
 move(Source^, Target^, SampleCount * SizeOf(Single));
 ReverseEndian4(Target, SampleCount);
end;

procedure DoubleToSingleMSB_FPU(Source: PDouble; Target: Pointer; SampleCount: LongInt); overload;
{$IFDEF PUREPASCAL}
begin
 DoubleLSBToSingle_FPU(Source, Target, SampleCount);
 ReverseEndian4(Target, SampleCount);
end;
{$ELSE}
asm
 PUSH EBX
 @Start:
  FLD   [Source+8*SampleCount-8].Double
  FSTP  [Target+4*SampleCount-4].Single
  MOV EBX, [Target+4*SampleCount-4]
  bswap EBX
  MOV [Target+4*SampleCount-4], EBX
  LOOP @Start
 POP EBX
end;
{$ENDIF}

procedure SingleToDoubleMSB_FPU(Source: PSingle; Target: Pointer; SampleCount: LongInt); overload;
begin
 SingleToDoubleLSB_FPU(Source, Target, SampleCount);
 ReverseEndian8(Target, SampleCount);
end;

procedure DoubleToDoubleMSB_FPU(Source: PDouble; Target: Pointer; SampleCount: LongInt); overload;
begin
 move(Source^, Target^, SampleCount * SizeOf(Double));
 ReverseEndian8(Target, SampleCount);
end;

procedure SingleToInt32MSB16_FPU(Source: PSingle; Target: Pointer; SampleCount: LongInt); overload;
asm
  PUSH EBX
  FLD   CFloatToSmall
 @Start:
  FLD   [EAX+4*ECX-4].Single
  FMUL  ST(0), ST(1)
  fistp [EDX+4*ECX-4].DWord
  MOV EBX, [EDX+4*ECX-4]
  bswap EBX
  MOV [EDX+4*ECX-4], EBX
  LOOP @Start
  FSTP ST(0)
  POP EBX
end;

procedure DoubleToInt32MSB16_FPU(Source: PDouble; Target: Pointer; SampleCount: LongInt); overload;
asm
  PUSH EBX
  FLD   CFloatToSmall
 @Start:
  FLD   [EAX + 8 * ECX - 8].Double
  FMUL  ST(0), ST(1)
  fistp [EDX+4*ECX-4].DWord
  MOV EBX, [EDX+4*ECX-4]
  bswap EBX
  MOV [EDX+4*ECX-4], EBX
  LOOP @Start
  FSTP ST(0)
  POP EBX
end;

procedure SingleToInt32MSB18_FPU(Source: PSingle; Target: Pointer; SampleCount: LongInt); overload;
asm
  PUSH EBX
  FLD   CFloatToInt18
 @Start:
  FLD   [EAX+4*ECX-4].Single
  FMUL  ST(0), ST(1)
  fistp [EDX+4*ECX-4].DWord
  MOV EBX, [EDX+4*ECX-4]
  bswap EBX
  MOV [EDX+4*ECX-4], EBX
  LOOP @Start
  FFREE ST(0)
  POP EBX
end;

procedure DoubleToInt32MSB18_FPU(Source: PDouble; Target: Pointer; SampleCount: LongInt); overload;
asm
  PUSH EBX
  FLD   CFloatToInt18
 @Start:
  FLD   [EAX + 8 * ECX - 8].Double
  FMUL  ST(0), ST(1)
  fistp [EDX+4*ECX-4].DWord
  MOV EBX, [EDX+4*ECX-4]
  bswap EBX
  MOV [EDX+4*ECX-4], EBX
  LOOP @Start
  FFREE ST(0)
  POP EBX
end;

procedure SingleToInt32MSB20_FPU(Source: PSingle; Target: Pointer; SampleCount: LongInt); overload;
asm
  PUSH EBX
  FLD   CFloatToInt20
 @Start:
  FLD   [EAX+4*ECX-4].Single
  FMUL  ST(0), ST(1)
  fistp [EDX+4*ECX-4].DWord
  MOV EBX, [EDX+4*ECX-4]
  bswap EBX
  MOV [EDX+4*ECX-4], EBX
  LOOP @Start
  FFREE ST(0)
  POP EBX
end;

procedure DoubleToInt32MSB20_FPU(Source: PDouble; Target: Pointer; SampleCount: LongInt); overload;
asm
  PUSH EBX
  FLD   CFloatToInt20
 @Start:
  FLD   [EAX + 8 * ECX - 8].Double
  FMUL  ST(0), ST(1)
  fistp [EDX+4*ECX-4].DWord
  MOV EBX, [EDX+4*ECX-4]
  bswap EBX
  MOV [EDX+4*ECX-4], EBX
  LOOP @Start
  FFREE ST(0)
  POP EBX
end;

procedure SingleToInt32MSB24_FPU(Source: PSingle; Target: Pointer; SampleCount: LongInt); overload;
asm
  PUSH  EBX
  FLD   CFloatToInt24
 @Start:
  FLD   [EAX + 4 * ECX - 4].Single
  FMUL  ST(0), ST(1)
  fistp [EDX + 4 * ECX - 4].DWord
  MOV   EBX, [EDX + 4 * ECX - 4]
  bswap EBX
  MOV   [EDX + 4 * ECX - 4], EBX
  LOOP  @Start
  FFREE ST(0)
  POP   EBX
end;

procedure DoubleToInt32MSB24_FPU(Source: PDouble; Target: Pointer; SampleCount: LongInt); overload;
asm
  PUSH  EBX
  FLD   CFloatToInt24
 @Start:
  FLD   [EAX + 8 * ECX - 8].Double
  FMUL  ST(0), ST(1)
  fistp [EDX + 4 * ECX - 4].DWord
  MOV   EBX, [EDX + 4 * ECX - 4]
  bswap EBX
  MOV   [EDX + 4 * ECX - 4], EBX
  LOOP  @Start
  FFREE ST(0)
  POP   EBX
end;

function ClipCheckInt16LSB_FPU(Source: Pointer; SampleCount: LongInt): Boolean;
var
  SampleIndex : Integer;
  Value       : PSmallInt absolute Source;
begin
 Result := False;
 for SampleIndex := 0 to SampleCount-1 do
  begin
   if (Value^ = $7FFF) or (Value^ = $8000) then
    begin
     Result := True;
     Exit;
    end;
   Inc(Value);
  end;
end;

function ClipCheckInt24LSB_FPU(Source: Pointer; SampleCount: LongInt): Boolean;
var
  SampleIndex : Integer;
  Value       : PInteger absolute Source;
  ByteValue   : PByte absolute Source;
begin
 Result := False;
 for SampleIndex := 0 to SampleCount - 1 do
  begin
   if (Value^ = $7FFFFF) or (Value^ = $800000) then
    begin
     Result := True;
     Exit;
    end;
   Inc(ByteValue, 3);
  end;
end;

function ClipCheckInt32LSB_FPU(Source: Pointer; SampleCount: LongInt): Boolean;
var
  SampleIndex : Integer;
  Value       : PInteger absolute Source;
begin
 Result := False;
 for SampleIndex := 0 to SampleCount - 1 do
  begin
   if (Value^ = $7FFFFFF) or (Value^ = $80000000) then
    begin
     Result := True;
     Exit;
    end;
   Inc(Value);
  end;
end;

function ClipCheckInt32LSB16_FPU(Source: Pointer; SampleCount: LongInt): Boolean;
var
  i : Integer;
  v : PInteger absolute Source;
begin
 Result := False;
 for i := 0 to SampleCount-1 do
  begin
   if (v^ = $7FFF) or (v^ = $8000) then
    begin
     Result := True;
     Exit;
    end;
   Inc(v);
  end;
end;

function ClipCheckInt32LSB18_FPU(Source: Pointer; SampleCount: LongInt): Boolean;
var i : Integer;
    v : PInteger absolute Source;
begin
 Result := False;
 for i := 0 to SampleCount-1 do
  begin
   if (v^ = $1FFFF) or (v^ = $20000) then
    begin
     Result := True;
     Exit;
    end;
   Inc(v);
  end;
end;

function ClipCheckInt32LSB20_FPU(Source: Pointer; SampleCount: LongInt): Boolean;
var i : Integer;
    v : PInteger absolute Source;
begin
 Result := False;
 for i := 0 to SampleCount-1 do
  begin
   if (v^ = $7FFFF) or (v^ = $80000) then
    begin
     Result := True;
     Exit;
    end;
   Inc(v);
  end;
end;

function ClipCheckInt32LSB24_FPU(Source: Pointer; SampleCount: LongInt): Boolean;
var i : Integer;
    v : PInteger absolute Source;
begin
 Result := False;
 for i := 0 to SampleCount-1 do
  begin
   if (v^ = $7FFFFF) or (v^ = $800000) then
    begin
     Result := True;
     Exit;
    end;
   Inc(v);
  end;
end;

function ClipCheckSingleLSB_FPU(Source: Pointer; SampleCount: LongInt): Boolean;
{$IFDEF PUREPASCAL}
var i : Integer;
    v : PSingle absolute Source;
begin
 Result := False;
 for i := 0 to SampleCount-1 do
  begin
   if (v^>1) or (v^<1) then
    begin
     Result := True;
     Exit;
    end;
   Inc(v);
  end;
end;
{$ELSE}
asm
 MOV Result, 1                 // Annahme, es klippt!
 MOV ECX, EAX                  // ECX = EAX
 xor EAX, EAX
 fld1
 @FadeLoop:
   FLD  [ECX+4*EDX-4].Single   // Value, CurrentFadeFak
   FABS
   FCOMI ST(0), ST(1)          // CurrentFadeFak <-> 1 ?
{
   FSTSW AX                    // AX = FPU Status Word
   SAHF                        // AX -> EFLAGS register
}
   FSTP ST(0)                  // clear stack
   ja @FadeLoopEnd             // if CurrentFadeFak > 1 then exit!

   DEC EDX
 JNZ @FadeLoop
 MOV Result, 0                 // na gut, klippt doch nicht :-/

 @FadeLoopEnd:
 FSTP ST(0)                    // clear stack
end;
{$ENDIF}

function ClipCheckDoubleLSB_FPU(Source: Pointer; SampleCount: LongInt): Boolean;
{$IFDEF PUREPASCAL}
var i : Integer;
    v : PDouble absolute Source;
begin
 Result := False;
 for i := 0 to SampleCount-1 do
  begin
   if (v^>1) or (v^<1) then
    begin
     Result := True;
     Exit;
    end;
   Inc(v);
  end;
end;
{$ELSE}
asm
 MOV Result, 1                 // Annahme, es klippt!
 MOV ECX, EAX                  // ECX = EAX
 xor EAX, EAX
 fld1
 @FadeLoop:
   FLD  [ECX+8*EDX-8].Double   // Value, CurrentFadeFak
   FABS
   FCOMI ST(0), ST(1)          // CurrentFadeFak <-> 1 ?
{
   FSTSW AX                    // AX = FPU Status Word
   SAHF                        // AX -> EFLAGS register
}
   FSTP ST(0)                  // clear stack
   ja @FadeLoopEnd             // if CurrentFadeFak > 1 then exit!

   DEC EDX
 JNZ @FadeLoop
 MOV Result, 0                 // na gut, klippt doch nicht :-/

 @FadeLoopEnd:
 FSTP ST(0)                    // clear stack
end;
{$ENDIF}

function ClipCheckInt16MSB_FPU(Source: Pointer; SampleCount: LongInt): Boolean;
var i : Integer;
    v : PSmallInt absolute Source;
begin
 Result := False;
 for i := 0 to SampleCount - 1 do
  begin
   if (v^ = $FF7F) or (v^ = $0080) then
    begin
     Result := True;
     Exit;
    end;
   Inc(v);
  end;
end;

function ClipCheckInt24MSB_FPU(Source: Pointer; SampleCount: LongInt): Boolean;
var i : Integer;
    v : PInteger absolute Source;
    b : PByte absolute Source;
begin
 Result := False;
 for i := 0 to SampleCount - 1 do
  begin
   // ToDo BitReverse!!
   if (v^ = $7FFFFF) or (v^ = $800000) then
    begin
     Result := True;
     Exit;
    end;
   Inc(b, 3);
  end;
end;

function ClipCheckInt32MSB_FPU(Source: Pointer; SampleCount: LongInt): Boolean;
var i : Integer;
    v : PInteger absolute Source;
begin
 Result := False;
 for i := 0 to SampleCount-1 do
  begin
   if (v^ = $FFFFF7F) or (v^ = $80) then
    begin
     Result := True;
     Exit;
    end;
   Inc(v);
  end;
end;

function ClipCheckInt32MSB16_FPU(Source: Pointer; SampleCount: LongInt): Boolean;
var i : Integer;
    v : PInteger absolute Source;
begin
 Result := False;
 for i := 0 to SampleCount-1 do
  begin
   if (v^ = $7FFF) or (v^ = $8000) then
    begin
     Result := True;
     Exit;
    end;
   Inc(v);
  end;
end;

function ClipCheckInt32MSB18_FPU(Source: Pointer; SampleCount: LongInt): Boolean;
var i : Integer;
    v : PInteger absolute Source;
begin
 Result := False;
 for i := 0 to SampleCount-1 do
  begin
   if (v^ = $1FFFF) or (v^ = $20000) then
    begin
     Result := True;
     Exit;
    end;
   Inc(v);
  end;
end;

function ClipCheckInt32MSB20_FPU(Source: Pointer; SampleCount: LongInt): Boolean;
var i : Integer;
    v : PInteger absolute Source;
begin
 Result := False;
 for i := 0 to SampleCount-1 do
  begin
   if (v^ = $7FFFF) or (v^ = $80000) then
    begin
     Result := True;
     Exit;
    end;
   Inc(v);
  end;
end;

function ClipCheckInt32MSB24_FPU(Source: Pointer; SampleCount: LongInt): Boolean;
var i : Integer;
    v : PInteger absolute Source;
begin
 Result := False;
 for i := 0 to SampleCount-1 do
  begin
   if (v^ = $7FFFFF) or (v^ = $800000) then
    begin
     Result := True;
     Exit;
    end;
   Inc(v);
  end;
end;

function ClipCheckSingleMSB_FPU(Source: Pointer; SampleCount: LongInt): Boolean;
var i : Integer;
    v : PSingle absolute Source;
begin
 Result := False;
 for i := 0 to SampleCount-1 do
  begin
   // ToDo ByteSwap here
   if (v^>1) or (v^<1) then
    begin
     Result := True;
     Exit;
    end;
   Inc(v);
  end;
end;

function ClipCheckDoubleMSB_FPU(Source: Pointer; SampleCount: LongInt): Boolean;
var
  i : Integer;
  v : PDouble absolute Source;
begin
 Result := False;
 for i := 0 to SampleCount - 1 do
  begin
   // ToDo ByteSwap here
   if (v^ > 1) or (v^ < 1) then
    begin
     Result := True;
     Exit;
    end;
   Inc(v);
  end;
end;

procedure MixBuffers_FPU(Data: PSingle; MixBuffer: PSingle; SampleCount: Integer); overload;
asm
@Start:
  FLD   [EAX + 4 * ECX - 4].Single
  fadd  [EDX + 4 * ECX - 4].Single
  FSTP  [EDX + 4 * ECX - 4].Single
  LOOP @Start
end;

procedure MixBuffers_FPU(Data: PDouble; MixBuffer: PDouble; SampleCount: Integer); overload;
asm
@Start:
  FLD   [EAX + 8 * ECX - 8].Double
  fadd  [EDX + 8 * ECX - 8].Double
  FSTP  [EDX + 8 * ECX - 8].Double
  LOOP @Start
end;

procedure Volume_FPU(Data:PSingle; Volume:Single; SampleCount:Integer); overload;
asm
  MOV     ECX, SampleCount
  FLD     Volume.Single
@Start:
  FLD     [EAX + 4 * ECX - 4].Single
  FMUL    ST(0), ST(1)
  FSTP    [EAX + 4 * ECX - 4].Single
  LOOP    @Start
  FSTP    ST(0)
end;

procedure Volume_FPU(Data:PDouble; Volume:Double; SampleCount:Integer); overload;
asm
  MOV     ECX, SampleCount
  FLD     Volume.Double
@Start:
  FLD     [EAX + 8 * ECX - 8].Double
  FMUL    ST(0), ST(1)
  FSTP    [EAX + 8 * ECX - 8].Double
  LOOP    @Start
  FSTP    ST(0)
end;

{$IFNDEF DELPHI5}

////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////// SSE ////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

procedure ClipDigital_SSE(Data: PSingle; SampleCount: Integer);
const
  c1a : Single = 1;
  mm1pos : array[0..3] of Single = (1, 1, 1, 1);
  mm1neg : array[0..3] of Single = (-1, -1, -1, -1);
asm
 MOV ECX, EDX
 PUSH ECX
 SHR ECX, 4  // number of large iterations = number of elements / 16
 JZ @SkipLargeAddLoop
 MOVUPS xmm0, mm1pos
 MOVUPS xmm1, mm1neg
@LargeAddLoop:
 MOVUPS xmm2, [EAX   ]
 MOVUPS xmm3, [EAX+$10]
 MOVUPS xmm4, [EAX+$20]
 MOVUPS xmm5, [EAX+$30]
 minps xmm2, xmm0
 minps xmm3, xmm0
 minps xmm4, xmm0
 minps xmm5, xmm0
 maxps xmm2, xmm1
 maxps xmm3, xmm1
 maxps xmm4, xmm1
 maxps xmm5, xmm1
 MOVUPS [EAX    ], xmm2
 MOVUPS [EAX+$10], xmm3
 MOVUPS [EAX+$20], xmm4
 MOVUPS [EAX+$30], xmm5
 ADD EAX, $40
 LOOP @LargeAddLoop

@SkipLargeAddLoop:
 POP ECX
 AND ECX, $0000000F
 JZ @EndAdd

@SmallAddLoop:
 MOV EDX, [EAX]
 AND EDX, $7FFFFFFF
 cmp EDX, c1a
 jle @Weiter
 MOV EDX, [EAX]
 AND EDX, $80000000
 ADD EDX, c1a
 MOV [EAX], EDX
@Weiter:
 ADD EAX, 4
 LOOP @SmallAddLoop

@EndAdd:
end;

procedure ClipAnalog_SSE(Data: PSingle; SampleCount: Integer);
const
  c3: Single = 3;
  c6: Single = 6;
  mm1sgn : array[0..3] of Integer = ($7FFFFFFF, $7FFFFFFF, $7FFFFFFF, $7FFFFFFF);
  mmc2   : array[0..3] of Single = (2, 2, 2, 2);
  mmc3   : array[0..3] of Single = (3, 3, 3, 3);
  mmc6   : array[0..3] of Single = (6, 6, 6, 6);
// a := abs(x); b := 3+a; Result := (x*b)/(a*b+6);
asm
 MOV ECX, EDX
 PUSH ECX
 SHR ECX, 2  // number of large iterations = number of elements / 16
 JZ @SkipLargeAddLoop
 MOVUPS xmm0, mm1sgn
 MOVUPS xmm1, mmc3
 MOVUPS xmm2, mmc6
 MOVUPS xmm3, mmc2
@LargeAddLoop:
 MOVUPS xmm4, [EAX]  // xmm3 = x
 movaps xmm5, xmm4   // xmm4 = x
 andps xmm5, xmm0    // xmm4 = |x|
 movaps xmm6, xmm5   // xmm5 = |x|
 addps xmm5, xmm1    // xmm4 = |x|+3
 MULPS xmm4, xmm5    // xmm3 = x*(|x|+3)
 MULPS xmm6, xmm5    // xmm5 = |x|*(|x|+3)
 addps xmm6, xmm2    // xmm5 = |x|*(|x|+3) + 6
 divps xmm4, xmm6    // xmm4 = x*(|x|+3)/(|x|*(|x|+3)+6)
 MULPS xmm4, xmm3    // xmm4 = 2*(x*(|x|+3)/(|x|*(|x|+3)+6))
 MOVUPS [EAX], xmm4

 ADD EAX, $10
 LOOP @LargeAddLoop

@SkipLargeAddLoop:
 POP ECX
 AND ECX, $00000003
 JZ @EndAdd

 fld1
 fld1
 FADDP

@SmallAddLoop:
 FLD [EAX].Single
 FABS
 FLD c3
 fadd ST(0), ST(1)
 FLD ST(0)
 FMUL [EAX].Single
 fxch ST(2)
 fmulp
 fadd c6.Single
 fdiv
 FMUL ST(0), ST(1)
 FSTP [EAX].Single
 ADD EAX, 4
 LOOP @SmallAddLoop
 FFREE ST(0)
@EndAdd:
end;

procedure SingleToSingle_SSE(Source: PSingle; Target: Pointer; SampleCount: LongInt);   // IEEE 754 32 bit float
asm
 PUSH ECX
 SHR ECX, 5  // number of large iterations = number of elements / 16
 JZ @SkipLargeAddLoop
@LargeAddLoop:
 prefetcht0 [EAX+$80]
 MOVUPS xmm0, [EAX    ]
 MOVUPS xmm1, [EAX+$10]
 MOVUPS xmm2, [EAX+$20]
 MOVUPS xmm3, [EAX+$30]
 MOVUPS xmm4, [EAX+$40]
 MOVUPS xmm5, [EAX+$50]
 MOVUPS xmm6, [EAX+$60]
 MOVUPS xmm7, [EAX+$70]

 // TODO!!!

 MOVUPS [EDX    ], xmm0
 MOVUPS [EDX+$10], xmm1
 MOVUPS [EDX+$20], xmm2
 MOVUPS [EDX+$30], xmm3
 MOVUPS [EDX+$40], xmm4
 MOVUPS [EDX+$50], xmm5
 MOVUPS [EDX+$60], xmm6
 MOVUPS [EDX+$70], xmm7

 ADD EAX, $80
 ADD EDX, $80
 LOOP @LargeAddLoop

@SkipLargeAddLoop:
 POP ECX
 AND ECX, $0000001F
 JZ @EndAdd

 SHR ECX, 2 // number of small iterations = (number of elements modulo 16) / 4

@SmallAddLoop:
 MOVUPS xmm0, [EAX]
 MOVUPS [EDX], xmm0

 ADD EAX, 16
 ADD EDX, 16
 DEC ECX
 JNZ @SmallAddLoop

@EndAdd:
end;

procedure SingleToInt32LSB_SSE(Source: PSingle; Target: Pointer; SampleCount: LongInt);
asm
 MOVUPS xmm7, CFloatToInt4

 PUSH ECX
 SHR ECX, 4  // number of large iterations = number of elements / 16
 JZ @SkipLargeAddLoop
@LargeAddLoop:
 movlps xmm0, [EAX]
 prefetcht0 [EAX+64]
 MULPS xmm0, xmm7
 cvttps2pi MM0, xmm0
 movlps xmm1, [EAX+8]
 MULPS xmm1, xmm7
 cvttps2pi MM1, xmm1

 movlps xmm2, [EAX+16]
 MULPS xmm2, xmm7
 cvttps2pi MM2, xmm2
 movlps xmm3, [EAX+24]
 MULPS xmm3, xmm7
 cvttps2pi MM3, xmm3

 movlps xmm4, [EAX+32]
 MULPS xmm4, xmm7
 cvttps2pi MM4, xmm4
 movlps xmm5, [EAX+40]
 MULPS xmm5, xmm7
 cvttps2pi mm5, xmm5

 movlps xmm6, [EAX+48]
 MULPS xmm6, xmm7
 cvttps2pi mm6, xmm6
 movlps xmm6, [EAX+56]
 MULPS xmm6, xmm7
 cvttps2pi mm7, xmm6

 movntq [EDX], MM0
 movntq [EDX+8], MM1
 movntq [EDX+16], MM2
 movntq [EDX+24], MM3
 movntq [EDX+32], MM4
 movntq [EDX+40], mm5
 movntq [EDX+48], mm6
 movntq [EDX+56], mm7

 ADD EAX, 64
 ADD EDX, 64
 DEC ECX
 JNZ @LargeAddLoop

@SkipLargeAddLoop:
 POP ECX
 AND ECX, $0000000F
 JZ @EndAdd

 SHR ECX, 2 // number of small iterations = (number of elements modulo 16) / 4

@SmallAddLoop:
 movlps xmm0, [EAX]
 MULPS xmm0, xmm7
 cvttps2pi MM0, xmm0
 movlps xmm1, [EAX+8]
 MULPS xmm1, xmm7
 cvttps2pi MM1, xmm1

 ADD EAX, 16
 ADD EDX, 16
 DEC ECX
 JNZ @SmallAddLoop

@EndAdd:
 emms
end;

procedure MixBuffers_SSE(Data:PSingle; MixBuffer:PSingle; SampleCount:Integer);
asm
 PUSH ECX
 SHR ECX, 4  // number of large iterations = number of elements / 16
 JZ @SkipLargeAddLoop
@LargeAddLoop:
  MOVUPS xmm0, [EAX    ]
  MOVUPS xmm1, [EAX+$10]
  MOVUPS xmm2, [EAX+$20]
  MOVUPS xmm3, [EAX+$30]
  MOVUPS xmm4, [EDX    ]
  MOVUPS xmm5, [EDX+$10]
  MOVUPS xmm6, [EDX+$20]
  MOVUPS xmm7, [EDX+$30]
  addps xmm0, xmm4
  addps xmm1, xmm5
  addps xmm2, xmm6
  addps xmm3, xmm7
  MOVUPS [EDX    ], xmm0
  MOVUPS [EDX+$10], xmm1
  MOVUPS [EDX+$20], xmm2
  MOVUPS [EDX+$30], xmm3
  ADD EAX, 64
  ADD EDX, 64
  DEC ECX
  JNZ @LargeAddLoop

@SkipLargeAddLoop:
  POP ECX
  AND ECX, $0000000F
  JZ @EndAdd

@AddLoop:
  FLD   [EAX+4*ECX-4].Single
  fadd  [EDX+4*ECX-4].Single
  FSTP  [EDX+4*ECX-4].Single
  LOOP @AddLoop

@EndAdd:
end;

{$IFNDEF FPC}
procedure Volume_SSE(Data:PSingle; Volume:Single; SampleCount:Integer);
asm
  MOV   ECX, SampleCount
  movss xmm4, Volume.Single
  shufps xmm4, xmm4, 0h

  PUSH ECX
  SHR ECX, 4
  JZ @SkipHugeLoop
@HugeLoop:
  MOVUPS  xmm0, [EAX]
  MULPS  xmm0, xmm4
  MOVUPS  xmm1, [EAX+16]
  MULPS  xmm1, xmm4
  MOVUPS  xmm2, [EAX+32]
  MULPS  xmm2, xmm4
  MOVUPS  xmm3, [EAX+48]
  MULPS  xmm3, xmm4

  MOVUPS  [EAX], xmm0
  MOVUPS  [EAX+16], xmm1
  MOVUPS  [EAX+32], xmm2
  MOVUPS  [EAX+48], xmm3
  ADD   EAX, 64
  LOOP  @HugeLoop

@SkipHugeLoop:
  POP ECX
  AND ECX, $0000000F
  JZ @EndSmallLoop

  PUSH ECX
  SHR ECX, 2
  JZ @SkipLargeLoop
@LargeLoop:
  MOVUPS  xmm0, [EAX]
  MULPS  xmm0, xmm4
  MOVUPS  [EAX], xmm0
  ADD   EAX, 16
  LOOP  @LargeLoop

@SkipLargeLoop:
  POP ECX
  AND ECX, $00000003
  JZ @EndSmallLoop

  FLD Volume.Single
@SmallLoop:
  FLD [EAX].Single
  FMUL ST(0), ST(1)
  FSTP [EAX].Single
  ADD EAX, 4
  LOOP  @SmallLoop

@EndSmallLoop:
  FFREE ST(0)
end;
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////// 3DNow //////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

const
(*
  C2FloatToShort : array[0..1] of Single = $7F;
  C2ShortToFloat : array[0..1] of Single = 1 / $7F;
  C2FloatToSmall : array[0..1] of Single = $7FFF;
  C2SmallToFloat : Single = 1 / $7FFF;
  C2FloatToInt18 : Double = $1FFFF;
  C2Int18ToFloat : Double = 1 / $1FFFF;
  C2FloatToInt20 : Double = $7FFFF;
  C2Int20ToFloat : Double = 1 / $7FFFF;
  C2FloatToInt24 : Double = $7FFFFF;
  C2Int24ToFloat : Double = 1 / $7FFFFF;
  C2FloatToInt   : Double = $7FFFFFFF;
  C2IntToFloat   : Double = 1 / $7FFFFFFF;
*)
  mmInt      : array[0..1] of Integer = ($30000000, $30000000);
  mmDivInt   : array[0..1] of Integer = ($4F000000, $4F000000);
  mmInt16    : array[0..1] of Single  = (1 / $7FFF, 1 / $7FFF);
  mmDivInt16 : array[0..1] of Single  = ($7FFF, $7FFF);
  mmInt18    : array[0..1] of Single  = (1/131071, 1/131071);
  mmDivInt18 : array[0..1] of Single  = (131071, 131071);
  mmInt20    : array[0..1] of Single  = (1/524287, 1/524287);
  mmDivInt20 : array[0..1] of Single  = (524287, 524287);
  mmInt24    : array[0..1] of Single  = (1/8388607, 1/8388607);
  mmDivInt24 : array[0..1] of Single  = (8388607, 8388607);
  mmSmall    : array[0..1] of Integer = ($30000000, $30000000);
  mmDivSmall : array[0..1] of Integer = ($4F000000, $4F000000);

procedure SingleToInt16LSB_3DNow(Source: PSingle; Target: Pointer; SampleCount: LongInt);
var
  temp64 : array[0..1] of Integer;
asm
  FEMMS                     // Fast MMX Enter/Leave
  SHR       ECX, 3          // Unroll the LOOP by 8
  MOVQ      MM4, mmDivSmall // use MM1 as 1/high(Integer) divider
  PREFETCH  [EAX]           // Holds the total mmx0..7 line in cache
                            // until modified and written
  @Loop2:
  MOVQ      MM0, [EAX]      // Spl1 | Spl2
  MOVQ      MM1, [EAX + 8]  // Spl3 | Spl4
  MOVQ      MM2, [EAX +16]  // Spl5 | Spl7
  MOVQ      MM3, [EAX +24]  // Spl7 | Spl8
  PFMUL     MM0, MM4        // Multiply by Low(Integer)
  PFMUL     MM1, MM4
  PFMUL     MM2, MM4
  PFMUL     MM3, MM4
  PF2ID     MM0, MM0        // convert to FP
  PF2ID     MM1, MM1
  PF2ID     MM2, MM2
  PF2ID     MM3, MM3
  MOVQ      [temp64], MM0
//  MOV       [EDX], temp64;
  MOVQ      temp64, MM1
  MOVQ      temp64, MM2
  MOVQ      temp64, MM3
{
  MOVQ      [EDX]   , MM0   // Store Sample back to RAM
  MOVQ      [EDX+ 8], MM1
  MOVQ      [EDX+16], MM2
  MOVQ      [EDX+24], MM3
}
  ADD       EAX, 32
  ADD       EDX, 32
  PREFETCH  [EAX]           // Inform mmu about next Sample Position
  LOOP      @Loop2
  FEMMS                     // Fast MMX Enter/Leave
end;

procedure SingleToInt32LSB_3DNow(Source: PSingle; Target: Pointer; SampleCount: LongInt);
asm
  FEMMS                     // Fast MMX Enter/Leave
  SHR       ECX, 3          // Unroll the LOOP by 8
  MOVQ      MM4, mmDivInt   // use MM1 as 1/high(Integer) divider
  PREFETCH [EAX]            // Holds the total mmx0..7 line in cache
                            // until modified and written
  @Loop2:
  MOVQ      MM0, [EAX]      // Spl1 | Spl2
  MOVQ      MM1, [EAX + 8]  // Spl3 | Spl4
  MOVQ      MM2, [EAX +16]  // Spl5 | Spl7
  MOVQ      MM3, [EAX +24]  // Spl7 | Spl8
  PFMUL     MM0, MM4        // Multiply by Low(Integer)
  PFMUL     MM1, MM4
  PFMUL     MM2, MM4
  PFMUL     MM3, MM4
  PF2ID     MM0, MM0        // convert to FP
  PF2ID     MM1, MM1
  PF2ID     MM2, MM2
  PF2ID     MM3, MM3
  MOVQ      [EDX]   , MM0   // Store Sample back to RAM
  MOVQ      [EDX+ 8], MM1
  MOVQ      [EDX+16], MM2
  MOVQ      [EDX+24], MM3
  ADD       EAX, 32
  ADD       EDX, 32
  PREFETCH  [EAX]          // Inform mmu about next Sample Position
  LOOP      @Loop2
  FEMMS                    // Fast MMX Enter/Leave
end;

procedure SingleToInt32LSB16_3DNow(Source: PSingle; Target: Pointer; SampleCount: LongInt);
asm
  FEMMS                     // Fast MMX Enter/Leave
  SHR       ECX, 3          // Unroll the LOOP by 8
  MOVQ      MM4, mmDivInt16 // use MM1 as 1/high(Integer) divider
  PREFETCH [EAX]            // Holds the total mmx0..7 line in cache
                            // until modified and written
  @Loop2:
  MOVQ      MM0, [EAX]      // Spl1 | Spl2
  MOVQ      MM1, [EAX + 8]  // Spl3 | Spl4
  MOVQ      MM2, [EAX +16]  // Spl5 | Spl7
  MOVQ      MM3, [EAX +24]  // Spl7 | Spl8
  PFMUL     MM0, MM4        // Multiply by Low(Integer)
  PFMUL     MM1, MM4
  PFMUL     MM2, MM4
  PFMUL     MM3, MM4
  PF2ID     MM0, MM0        // convert to FP
  PF2ID     MM1, MM1
  PF2ID     MM2, MM2
  PF2ID     MM3, MM3
  MOVQ      [EDX]   , MM0   // Store Sample back to RAM
  MOVQ      [EDX+ 8], MM1
  MOVQ      [EDX+16], MM2
  MOVQ      [EDX+24], MM3
  ADD       EAX, 32
  ADD       EDX, 32
  PREFETCH  [EAX]          // Inform mmu about next Sample Position
  LOOP      @Loop2
  FEMMS                    // Fast MMX Enter/Leave
end;

procedure SingleToInt32LSB18_3DNow(Source: PSingle; Target: Pointer; SampleCount: LongInt);
asm
  FEMMS                     // Fast MMX Enter/Leave
  SHR       ECX, 3          // Unroll the LOOP by 8
  MOVQ      MM4, mmDivInt18 // use MM1 as 1/high(Integer) divider
  PREFETCH [EAX]            // Holds the total mmx0..7 line in cache
                            // until modified and written
  @Loop2:
  MOVQ      MM0, [EAX]      // Spl1 | Spl2
  MOVQ      MM1, [EAX + 8]  // Spl3 | Spl4
  MOVQ      MM2, [EAX +16]  // Spl5 | Spl7
  MOVQ      MM3, [EAX +24]  // Spl7 | Spl8
  PFMUL     MM0, MM4        // Multiply by Low(Integer)
  PFMUL     MM1, MM4
  PFMUL     MM2, MM4
  PFMUL     MM3, MM4
  PF2ID     MM0, MM0        // convert to FP
  PF2ID     MM1, MM1
  PF2ID     MM2, MM2
  PF2ID     MM3, MM3
  MOVQ      [EDX]   , MM0   // Store Sample back to RAM
  MOVQ      [EDX+ 8], MM1
  MOVQ      [EDX+16], MM2
  MOVQ      [EDX+24], MM3
  ADD       EAX, 32
  ADD       EDX, 32
  PREFETCH  [EAX]          // Inform mmu about next Sample Position
  LOOP      @Loop2
  FEMMS                    // Fast MMX Enter/Leave
end;

procedure SingleToInt32LSB20_3DNow(Source: PSingle; Target: Pointer; SampleCount: LongInt);
asm
  FEMMS                     // Fast MMX Enter/Leave
  SHR       ECX, 3          // Unroll the LOOP by 8
  MOVQ      MM4, mmDivInt20 // use MM1 as 1/high(Integer) divider
  PREFETCH [EAX]            // Holds the total mmx0..7 line in cache
                            // until modified and written
  @Loop2:
  MOVQ      MM0, [EAX]      // Spl1 | Spl2
  MOVQ      MM1, [EAX + 8]  // Spl3 | Spl4
  MOVQ      MM2, [EAX +16]  // Spl5 | Spl7
  MOVQ      MM3, [EAX +24]  // Spl7 | Spl8
  PFMUL     MM0, MM4        // Multiply by Low(Integer)
  PFMUL     MM1, MM4
  PFMUL     MM2, MM4
  PFMUL     MM3, MM4
  PF2ID     MM0, MM0        // convert to FP
  PF2ID     MM1, MM1
  PF2ID     MM2, MM2
  PF2ID     MM3, MM3
  MOVQ      [EDX]   , MM0   // Store Sample back to RAM
  MOVQ      [EDX+ 8], MM1
  MOVQ      [EDX+16], MM2
  MOVQ      [EDX+24], MM3
  ADD       EAX, 32
  ADD       EDX, 32
  PREFETCH  [EAX]          // Inform mmu about next Sample Position
  LOOP      @Loop2
  FEMMS                    // Fast MMX Enter/Leave
end;

procedure SingleToInt32LSB24_3DNow(Source: PSingle; Target: Pointer; SampleCount: LongInt);
asm
  FEMMS                     // Fast MMX Enter/Leave
  SHR       ECX, 3          // Unroll the LOOP by 8
  MOVQ      MM4, mmDivInt24 // use MM1 as 1/high(Integer) divider
  PREFETCH [EAX]            // Holds the total mmx0..7 line in cache
                            // until modified and written
  @Loop2:
  MOVQ      MM0, [EAX]      // Spl1 | Spl2
  MOVQ      MM1, [EAX + 8]  // Spl3 | Spl4
  MOVQ      MM2, [EAX +16]  // Spl5 | Spl7
  MOVQ      MM3, [EAX +24]  // Spl7 | Spl8
  PFMUL     MM0, MM4        // Multiply by Low(Integer)
  PFMUL     MM1, MM4
  PFMUL     MM2, MM4
  PFMUL     MM3, MM4
  PF2ID     MM0, MM0        // convert to FP
  PF2ID     MM1, MM1
  PF2ID     MM2, MM2
  PF2ID     MM3, MM3
  MOVQ      [EDX]   , MM0   // Store Sample back to RAM
  MOVQ      [EDX+ 8], MM1
  MOVQ      [EDX+16], MM2
  MOVQ      [EDX+24], MM3
  ADD       EAX, 32
  ADD       EDX, 32
  PREFETCH  [EAX]          // Inform mmu about next Sample Position
  LOOP      @Loop2
  FEMMS                    // Fast MMX Enter/Leave
end;

procedure ToInt16LSB_3DNow(Source: Pointer; Target: PSingle; SampleCount: LongInt);
asm
  FEMMS                    // Fast MMX Enter/Leave
  SHR       ECX, 3         // unroll the LOOP by 8
  MOVQ      MM4, mmSmall   // use MM4 as 1/high(Integer) divider
  PREFETCHW [EAX]          // give the mmu a heads-up,
                           // load the total line of mmx0..7 data in the cache
                           // and prepare for modification. (If I understand AMD correctly)
  @Loop2:
  MOVQ      MM0, [EAX]     // Spl1 | Spl2
  MOVQ      MM1, [EAX+8]   // Spl3 | Spl4
  MOVQ      MM2, [EAX+16]  // Spl5 | Spl7
  MOVQ      MM3, [EAX+24]  // Spl7 | Spl8
  PI2FD     MM0, MM0       // convert to FP
  PI2FD     MM1, MM1
  PI2FD     MM2, MM2
  PI2FD     MM3, MM3
  PFMUL     MM0, MM4       // divide by high(Integer)
  PFMUL     MM1, MM4
  PFMUL     MM2, MM4
  PFMUL     MM3, MM4
  MOVQ      [EDX], MM0     // Store Sample back to RAM
  MOVQ      [EDX+8], MM1
  MOVQ      [EDX+16], MM2
  MOVQ      [EDX+24], MM3
  ADD       EAX, 32
  ADD       EDX, 32
  PREFETCHW [EAX]          // Inform mmu about next Sample Position
  LOOP      @Loop2
  FEMMS                    // Fast MMX Enter/Leave
end;

procedure Int32LSBToSingle_3DNow(Source: Pointer; Target: PSingle; SampleCount: LongInt);
asm
  FEMMS                    // Fast MMX Enter/Leave
  SHR       ECX, 3         // unroll the LOOP by 8
  MOVQ      MM4, mmInt     // use MM4 as 1/high(Integer) divider
  PREFETCHW [EAX]          // give the mmu a heads-up,
                           // load the total line of mmx0..7 data in the cache
                           // and prepare for modification. (If I understand AMD correctly)
  @Loop2:
  MOVQ      MM0, [EAX]     // Spl1 | Spl2
  MOVQ      MM1, [EAX+8]   // Spl3 | Spl4
  MOVQ      MM2, [EAX+16]  // Spl5 | Spl7
  MOVQ      MM3, [EAX+24]  // Spl7 | Spl8
  PI2FD     MM0, MM0       // convert to FP
  PI2FD     MM1, MM1
  PI2FD     MM2, MM2
  PI2FD     MM3, MM3
  PFMUL     MM0, MM4       // divide by high(Integer)
  PFMUL     MM1, MM4
  PFMUL     MM2, MM4
  PFMUL     MM3, MM4
  MOVQ      [EDX], MM0     // Store Sample back to RAM
  MOVQ      [EDX+8], MM1
  MOVQ      [EDX+16], MM2
  MOVQ      [EDX+24], MM3
  ADD       EAX, 32
  ADD       EDX, 32
  PREFETCHW [EAX]          // Inform mmu about next Sample Position
  LOOP      @Loop2
  FEMMS                    // Fast MMX Enter/Leave
end;

procedure Int32LSB16ToSingle_3DNow(Source: Pointer; Target: PSingle; SampleCount: LongInt);
asm
  FEMMS                    // Fast MMX Enter/Leave
  SHR       ECX, 3         // unroll the LOOP by 8
  MOVQ      MM4, mmInt16   // use MM4 as 1/high(Integer) divider
  PREFETCHW [EAX]          // give the mmu a heads-up,
                           // load the total line of mmx0..7 data in the cache
                           // and prepare for modification. (If I understand AMD correctly)
  @Loop2:
  MOVQ      MM0, [EAX]     // Spl1 | Spl2
  MOVQ      MM1, [EAX+8]   // Spl3 | Spl4
  MOVQ      MM2, [EAX+16]  // Spl5 | Spl7
  MOVQ      MM3, [EAX+24]  // Spl7 | Spl8
  PI2FD     MM0, MM0       // convert to FP
  PI2FD     MM1, MM1
  PI2FD     MM2, MM2
  PI2FD     MM3, MM3
  PFMUL     MM0, MM4       // divide by high(Integer)
  PFMUL     MM1, MM4
  PFMUL     MM2, MM4
  PFMUL     MM3, MM4
  MOVQ      [EDX], MM0     // Store Sample back to RAM
  MOVQ      [EDX+8], MM1
  MOVQ      [EDX+16], MM2
  MOVQ      [EDX+24], MM3
  ADD       EAX, 32
  ADD       EDX, 32
  PREFETCHW [EAX]          // Inform mmu about next Sample Position
  LOOP      @Loop2
  FEMMS                    // Fast MMX Enter/Leave
end;

procedure Int32LSB18ToSingle_3DNow(Source: Pointer; Target: PSingle; SampleCount: LongInt);
asm
  FEMMS                    // Fast MMX Enter/Leave
  SHR       ECX, 3         // unroll the LOOP by 8
  MOVQ      MM4, mmInt18   // use MM4 as 1/high(Integer) divider
  PREFETCHW [EAX]          // give the mmu a heads-up,
                           // load the total line of mmx0..7 data in the cache
                           // and prepare for modification. (If I understand AMD correctly)
  @Loop2:
  MOVQ      MM0, [EAX]     // Spl1 | Spl2
  MOVQ      MM1, [EAX+8]   // Spl3 | Spl4
  MOVQ      MM2, [EAX+16]  // Spl5 | Spl7
  MOVQ      MM3, [EAX+24]  // Spl7 | Spl8
  PI2FD     MM0, MM0       // convert to FP
  PI2FD     MM1, MM1
  PI2FD     MM2, MM2
  PI2FD     MM3, MM3
  PFMUL     MM0, MM4       // divide by high(Integer)
  PFMUL     MM1, MM4
  PFMUL     MM2, MM4
  PFMUL     MM3, MM4
  MOVQ      [EDX], MM0     // Store Sample back to RAM
  MOVQ      [EDX+8], MM1
  MOVQ      [EDX+16], MM2
  MOVQ      [EDX+24], MM3
  ADD       EAX, 32
  ADD       EDX, 32
  PREFETCHW [EAX]          // Inform mmu about next Sample Position
  LOOP      @Loop2
  FEMMS                    // Fast MMX Enter/Leave
end;

procedure Int32LSB20ToSingle_3DNow(Source: Pointer; Target: PSingle; SampleCount: LongInt);
asm
  FEMMS                    // Fast MMX Enter/Leave
  SHR       ECX, 3         // unroll the LOOP by 8
  MOVQ      MM4, mmInt20   // use MM4 as 1/high(Integer) divider
  PREFETCHW [EAX]          // give the mmu a heads-up,
                           // load the total line of mmx0..7 data in the cache
                           // and prepare for modification. (If I understand AMD correctly)
  @Loop2:
  MOVQ      MM0, [EAX]     // Spl1 | Spl2
  MOVQ      MM1, [EAX+8]   // Spl3 | Spl4
  MOVQ      MM2, [EAX+16]  // Spl5 | Spl7
  MOVQ      MM3, [EAX+24]  // Spl7 | Spl8
  PI2FD     MM0, MM0       // convert to FP
  PI2FD     MM1, MM1
  PI2FD     MM2, MM2
  PI2FD     MM3, MM3
  PFMUL     MM0, MM4       // divide by high(Integer)
  PFMUL     MM1, MM4
  PFMUL     MM2, MM4
  PFMUL     MM3, MM4
  MOVQ      [EDX], MM0     // Store Sample back to RAM
  MOVQ      [EDX+8], MM1
  MOVQ      [EDX+16], MM2
  MOVQ      [EDX+24], MM3
  ADD       EAX, 32
  ADD       EDX, 32
  PREFETCHW [EAX]          // Inform mmu about next Sample Position
  LOOP      @Loop2
  FEMMS                    // Fast MMX Enter/Leave
end;

procedure Int32LSB24ToSingle_3DNow(Source: Pointer; Target: PSingle; SampleCount: LongInt);
asm
  FEMMS                    // Fast MMX Enter/Leave
  SHR       ECX, 3         // unroll the LOOP by 8
  MOVQ      MM4, mmInt24   // use MM4 as 1/high(Integer) divider
  PREFETCHW [EAX]          // give the mmu a heads-up,
                           // load the total line of mmx0..7 data in the cache
                           // and prepare for modification. (If I understand AMD correctly)
  @Loop2:
  MOVQ      MM0, [EAX]     // Spl1 | Spl2
  MOVQ      MM1, [EAX+8]   // Spl3 | Spl4
  MOVQ      MM2, [EAX+16]  // Spl5 | Spl7
  MOVQ      MM3, [EAX+24]  // Spl7 | Spl8
  PI2FD     MM0, MM0       // convert to FP
  PI2FD     MM1, MM1
  PI2FD     MM2, MM2
  PI2FD     MM3, MM3
  PFMUL     MM0, MM4       // divide by high(Integer)
  PFMUL     MM1, MM4
  PFMUL     MM2, MM4
  PFMUL     MM3, MM4
  MOVQ      [EDX], MM0     // Store Sample back to RAM
  MOVQ      [EDX+8], MM1
  MOVQ      [EDX+16], MM2
  MOVQ      [EDX+24], MM3
  ADD       EAX, 32
  ADD       EDX, 32
  PREFETCHW [EAX]          // Inform mmu about next Sample Position
  LOOP      @Loop2
  FEMMS                    // Fast MMX Enter/Leave
end;

procedure MixBuffers_3DNow(Data:PSingle; MixBuffer:PSingle; SampleCount:Integer);
asm
  FEMMS
  SHR       ECX, 3
  PREFETCHW [EAX]
@Start:
  MOVQ      MM0, [EAX]
  MOVQ      MM1, [EAX+8]
  MOVQ      MM2, [EAX+16]
  MOVQ      MM3, [EAX+24]
  MOVQ      MM4, [EDX]
  MOVQ      mm5, [EDX+8]
  MOVQ      mm6, [EDX+16]
  MOVQ      mm7, [EDX+24]
  pfadd     MM4, MM0
  pfadd     mm5, MM1
  pfadd     mm6, MM2
  pfadd     mm7, MM3
  MOVQ      [EDX], MM4
  MOVQ      [EDX+8], mm5
  MOVQ      [EDX+16], mm6
  MOVQ      [EDX+24], mm7
  ADD       EAX, 32
  ADD       EDX, 32
  PREFETCHW [EAX]          // Inform mmu about next Sample Position
  LOOP  @Start
  FEMMS
end;

procedure Volume_3DNow(Data: PSingle; Volume: Single; SampleCount: Integer);
var volArray : array[0..1] of Single;
asm
  FLD       Volume.Single
  fst       [volArray].Single
  fst       [volArray+4].Single
  MOV       ECX, SampleCount
  SHR       ECX, 3
  PUSH      ECX
  JZ        @SkipLargeLoop

  FEMMS
  MOVQ      MM4, volArray

  PREFETCHW [EAX]
@LargeLoop:
  MOVQ      MM0, [EAX]
  MOVQ      MM1, [EAX+8]
  MOVQ      MM2, [EAX+16]
  MOVQ      MM3, [EAX+24]
  PFMUL     MM0, MM4
  PFMUL     MM1, MM4
  PFMUL     MM2, MM4
  PFMUL     MM3, MM4
  MOVQ      [EAX], MM0
  MOVQ      [EAX+8], MM1
  MOVQ      [EAX+16], MM2
  MOVQ      [EAX+24], MM3
  ADD       EAX, 32
  ADD       EDX, 32
  PREFETCHW [EAX]
  LOOP      @LargeLoop
  FEMMS
@SkipLargeLoop:
  POP ECX
  and ECX, $00000007
  JZ @EndSmallLoop

  FLD Volume.Single
@SmallLoop:
  FLD [EAX].Single
  FMUL ST(0), ST(1)
  FSTP [EAX].Single
  ADD EAX, 4
  LOOP  @SmallLoop

@EndSmallLoop:
  FFREE ST(0)
end;

{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
///////////////////////////////// Selection ////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

procedure Use_FPU;
begin
  FromInt16MSB.ic32    := Int16MSBToSingle_FPU;
  FromInt24MSB.ic32    := Int24MSBToSingle_FPU;
  FromInt32MSB.ic32    := Int32MSBToSingle_FPU;
  FromSingleMSB.ic32   := SingleMSBToSingle_FPU;
  FromDoubleMSB.ic32   := DoubleMSBToSingle_FPU;
  FromInt32MSB16.ic32  := Int32MSB16ToSingle_FPU;
  FromInt32MSB18.ic32  := Int32MSB18ToSingle_FPU;
  FromInt32MSB20.ic32  := Int32MSB20ToSingle_FPU;
  FromInt32MSB24.ic32  := Int32MSB24ToSingle_FPU;
  FromInt16LSB.ic32    := Int16LSBToSingle_FPU;
  FromInt24LSB.ic32    := Int24LSBToSingle_FPU;
  FromInt32LSB.ic32    := Int32LSBToSingle_FPU;
  FromSingleLSB.ic32   := SingleLSBToSingle_FPU;
  FromDoubleLSB.ic32   := DoubleLSBToSingle_FPU;
  FromInt32LSB16.ic32  := Int32LSB16ToSingle_FPU;
  FromInt32LSB18.ic32  := Int32LSB18ToSingle_FPU;
  FromInt32LSB20.ic32  := Int32LSB20ToSingle_FPU;
  FromInt32LSB24.ic32  := Int32LSB24ToSingle_FPU;
  /////////////////////////////////////////////
  FromInt16MSB.ic64    := Int16MSBToDouble_FPU;
  FromInt24MSB.ic64    := Int24MSBToDouble_FPU;
  FromInt32MSB.ic64    := Int32MSBToDouble_FPU;
  FromSingleMSB.ic64   := SingleMSBToDouble_FPU;
  FromDoubleMSB.ic64   := DoubleMSBToDouble_FPU;
  FromInt32MSB16.ic64  := Int32MSB16ToDouble_FPU;
  FromInt32MSB18.ic64  := Int32MSB18ToDouble_FPU;
  FromInt32MSB20.ic64  := Int32MSB20ToDouble_FPU;
  FromInt32MSB24.ic64  := Int32MSB24ToDouble_FPU;
  FromInt16LSB.ic64    := Int16LSBToDouble_FPU;
  FromInt24LSB.ic64    := Int24LSBToDouble_FPU;
  FromInt32LSB.ic64    := Int32LSBToDouble_FPU;
  FromSingleLSB.ic64   := SingleLSBToDouble_FPU;
  FromDoubleLSB.ic64   := DoubleLSBToDouble_FPU;
  FromInt32LSB16.ic64  := Int32LSB16ToDouble_FPU;
  FromInt32LSB18.ic64  := Int32LSB18ToDouble_FPU;
  FromInt32LSB20.ic64  := Int32LSB20ToDouble_FPU;
  FromInt32LSB24.ic64  := Int32LSB24ToDouble_FPU;
  ///////////////////////////////////////////////
  ToInt16MSB.oc32      := SingleToInt16MSB_FPU;
  ToInt24MSB.oc32      := SingleToInt24MSB_FPU;
  ToInt32MSB.oc32      := SingleToInt32MSB_FPU;
  ToSingleMSB.oc32     := SingleToSingleMSB_FPU;
  ToDoubleMSB.oc32     := SingleToDoubleMSB_FPU;
  ToInt32MSB16.oc32    := SingleToInt32MSB16_FPU;
  ToInt32MSB18.oc32    := SingleToInt32MSB18_FPU;
  ToInt32MSB20.oc32    := SingleToInt32MSB20_FPU;
  ToInt32MSB24.oc32    := SingleToInt32MSB24_FPU;
  ToInt16LSB.oc32      := SingleToInt16LSB_FPU;
  ToInt24LSB.oc32      := SingleToInt24LSB_FPU;
  ToInt32LSB.oc32      := SingleToInt32LSB_FPU;
  ToSingleLSB.oc32     := SingleToSingleLSB_FPU;
  ToDoubleLSB.oc32     := SingleToDoubleLSB_FPU;
  ToInt32LSB16.oc32    := SingleToInt32LSB16_FPU;
  ToInt32LSB18.oc32    := SingleToInt32LSB18_FPU;
  ToInt32LSB20.oc32    := SingleToInt32LSB20_FPU;
  ToInt32LSB24.oc32    := SingleToInt32LSB24_FPU;
  ///////////////////////////////////////////////
  ToInt16MSB.oc64      := DoubleToInt16MSB_FPU;
  ToInt24MSB.oc64      := DoubleToInt24MSB_FPU;
  ToInt32MSB.oc64      := DoubleToInt32MSB_FPU;
  ToSingleMSB.oc64     := DoubleToSingleMSB_FPU;
  ToDoubleMSB.oc64     := DoubleToDoubleMSB_FPU;
  ToInt32MSB16.oc64    := DoubleToInt32MSB16_FPU;
  ToInt32MSB18.oc64    := DoubleToInt32MSB18_FPU;
  ToInt32MSB20.oc64    := DoubleToInt32MSB20_FPU;
  ToInt32MSB24.oc64    := DoubleToInt32MSB24_FPU;
  ToInt16LSB.oc64      := DoubleToInt16LSB_FPU;
  ToInt24LSB.oc64      := DoubleToInt24LSB_FPU;
  ToInt32LSB.oc64      := DoubleToInt32LSB_FPU;
  ToSingleLSB.oc64     := DoubleToSingleLSB_FPU;
  ToDoubleLSB.oc64     := DoubleToDoubleLSB_FPU;
  ToInt32LSB16.oc64    := DoubleToInt32LSB16_FPU;
  ToInt32LSB18.oc64    := DoubleToInt32LSB18_FPU;
  ToInt32LSB20.oc64    := DoubleToInt32LSB20_FPU;
  ToInt32LSB24.oc64    := DoubleToInt32LSB24_FPU;
  MixBuffers.mb32      := MixBuffers_FPU;
  MixBuffers.mb64      := MixBuffers_FPU;
  Volume.v32           := Volume_FPU;
  Volume.v64           := Volume_FPU;
  ClipDigital.cb32     := ClipDigital_x86;
  ClipDigital.cb64     := ClipDigital_x86;
  ClipAnalog.cb32      := ClipAnalog_FPU;
  ClipAnalog.cb64      := ClipAnalog_FPU;
  FadeInLinear.v32     := FadeInLinear_FPU;
  FadeInLinear.v64     := FadeInLinear_FPU;
  FadeOutLinear.v32    := FadeOutLinear_FPU;
  FadeOutLinear.v64    := FadeOutLinear_FPU;
  FadeLinear.v32       := FadeLinear_FPU;
  FadeLinear.v64       := FadeLinear_FPU;
  FadeExponential.v32  := FadeExponential_FPU;
  FadeExponential.v64  := FadeExponential_FPU;
  Trigger.v32          := Trigger_FPU;
  Trigger.v64          := Trigger_FPU;

  ClipCheckInt16MSB    := ClipCheckInt16MSB_FPU;
  ClipCheckInt24MSB    := ClipCheckInt24MSB_FPU;
  ClipCheckInt32MSB    := ClipCheckInt32MSB_FPU;
  ClipCheckSingleMSB   := ClipCheckSingleMSB_FPU;
  ClipCheckDoubleMSB   := ClipCheckDoubleMSB_FPU;
  ClipCheckInt32MSB16  := ClipCheckInt32MSB16_FPU;
  ClipCheckInt32MSB18  := ClipCheckInt32MSB18_FPU;
  ClipCheckInt32MSB20  := ClipCheckInt32MSB20_FPU;
  ClipCheckInt32MSB24  := ClipCheckInt32MSB24_FPU;
  ClipCheckInt16LSB    := ClipCheckInt16LSB_FPU;
  ClipCheckInt24LSB    := ClipCheckInt24LSB_FPU;
  ClipCheckInt32LSB    := ClipCheckInt32LSB_FPU;
  ClipCheckSingleLSB   := ClipCheckSingleLSB_FPU;
  ClipCheckDoubleLSB   := ClipCheckDoubleLSB_FPU;
  ClipCheckInt32LSB16  := ClipCheckInt32LSB16_FPU;
  ClipCheckInt32LSB18  := ClipCheckInt32LSB18_FPU;
  ClipCheckInt32LSB20  := ClipCheckInt32LSB20_FPU;
  ClipCheckInt32LSB24  := ClipCheckInt32LSB24_FPU;
end;

procedure Use_FPU_UDF;
begin
 Use_FPU;
 RandSeed := GetTickCount;
 ToInt16LSB.oc32      := SingleToInt16LSB_UDF_FPU;
 ToInt24LSB.oc32      := SingleToInt24LSB_UDF_FPU;
 ToInt32LSB16.oc32    := SingleToInt32LSB16_UDF_FPU;
 ToInt32LSB18.oc32    := SingleToInt32LSB18_UDF_FPU;
 ToInt32LSB20.oc32    := SingleToInt32LSB20_UDF_FPU;
 ToInt32LSB24.oc32    := SingleToInt32LSB24_UDF_FPU;
 ///////////////////////////////////////////////
 ToInt16LSB.oc64      := DoubleToInt16LSB_UDF_FPU;
 ToInt24LSB.oc64      := DoubleToInt24LSB_UDF_FPU;
 ToInt32LSB16.oc64    := DoubleToInt32LSB16_UDF_FPU;
 ToInt32LSB18.oc64    := DoubleToInt32LSB18_UDF_FPU;
 ToInt32LSB20.oc64    := DoubleToInt32LSB20_UDF_FPU;
 ToInt32LSB24.oc64    := DoubleToInt32LSB24_UDF_FPU;
end;

procedure Use_FPU_TDF;
begin
 Use_FPU;
 RandSeed := GetTickCount;
 ToInt16LSB.oc32      := SingleToInt16LSB_TDF_FPU;
 ToInt24LSB.oc32      := SingleToInt24LSB_TDF_FPU;
 ToInt32LSB16.oc32    := SingleToInt32LSB16_TDF_FPU;
 ToInt32LSB18.oc32    := SingleToInt32LSB18_TDF_FPU;
 ToInt32LSB20.oc32    := SingleToInt32LSB20_TDF_FPU;
 ToInt32LSB24.oc32    := SingleToInt32LSB24_TDF_FPU;
 ///////////////////////////////////////////////
 ToInt16LSB.oc64      := DoubleToInt16LSB_TDF_FPU;
 ToInt24LSB.oc64      := DoubleToInt24LSB_TDF_FPU;
 ToInt32LSB16.oc64    := DoubleToInt32LSB16_TDF_FPU;
 ToInt32LSB18.oc64    := DoubleToInt32LSB18_TDF_FPU;
 ToInt32LSB20.oc64    := DoubleToInt32LSB20_TDF_FPU;
 ToInt32LSB24.oc64    := DoubleToInt32LSB24_TDF_FPU;
end;

procedure Use_SSE;
begin
 {$IFNDEF DELPHI5}
// ToInt32LSB.oc32 := SingleToInt32LSB_SSE;
 MixBuffers.mb32   := MixBuffers_SSE;
 ClipDigital.cb32  := ClipDigital_SSE;
 ClipAnalog.cb32   := ClipAnalog_SSE;
 {$IFNDEF FPC}
 Volume.v32        := Volume_SSE;
 {$ENDIF}
 {$ENDIF}
end;

procedure Use_3DNow;
begin
 {$IFNDEF DELPHI5}
{
 ToInt16LSB := SingleToInt16LSB_3DNow;
 FromInt16LSB := ToInt16LSB_3DNow;
 ToInt24LSB := SingleToInt24LSB_3DNow;
 FromInt24LSB := ToInt24LSB_3DNow;
}
 ToInt32LSB.oc32     := SingleToInt32LSB_3DNow;
 FromInt32LSB.ic32   := Int32LSBToSingle_3DNow;
 ToInt32LSB16.oc32   := SingleToInt32LSB16_3DNow;
 FromInt32LSB16.ic32 := Int32LSB16ToSingle_3DNow;
 ToInt32LSB18.oc32   := SingleToInt32LSB18_3DNow;
 FromInt32LSB18.ic32 := Int32LSB18ToSingle_3DNow;
 ToInt32LSB20.oc32   := SingleToInt32LSB20_3DNow;
 FromInt32LSB20.ic32 := Int32LSB20ToSingle_3DNow;
 ToInt32LSB24.oc32   := SingleToInt32LSB24_3DNow;
 FromInt32LSB24.ic32 := Int32LSB24ToSingle_3DNow;
 MixBuffers.mb32     := MixBuffers_3DNow;
 Volume.v32          := Volume_3DNow;
 {$ENDIF}
end;

procedure BindFunctions;
begin
 Use_FPU;

 {$IFNDEF FPC}
 if Assigned(ProcessorInfo) then
  begin
   if ssSSE in ProcessorInfo.SupportsSSE
    then Use_SSE;
   if ProcessorInfo.Has3DNow
    then Use_3DNow;
  end;
 {$ENDIF}
end;

initialization
  BindFunctions;

end.
