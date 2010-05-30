unit DAV_DspWindowing;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2007-2009        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Types, DAV_Classes, DAV_Complex;

const
  CHamming : array [0..1] of Double = (0.54, -0.46);
  CHanning : Double = -0.5;
  CBlackman : array [0..2] of Double = (0.34, -0.5, 0.16);

type
  TParameterRecord = record
    ComplexPosition          : TComplexDouble;
    ComplexAngle             : TComplexDouble;
    SpectrumCorrectionFactor : Double;
    SpuaredCorrectionFactor  : Double;
    CoefficientPointer       : PDAVDoubleFixedArray;
  end;

procedure ApplyTriangleWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer); overload;
procedure ApplyHanningWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer); overload;
procedure ApplyHammingWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer); overload;
procedure ApplyBlackmanWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer); overload;
procedure ApplyBlackmanHarrisWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer); overload;
procedure ApplyGaussianWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer); overload;
procedure ApplyKaiserBesselWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer; const Alpha: Single); overload;

procedure ApplyTriangleWindow(var Data: TDAVSingleDynArray); overload;
procedure ApplyHanningWindow(var Data: TDAVSingleDynArray); overload;
procedure ApplyHammingWindow(var Data: TDAVSingleDynArray); overload;
procedure ApplyBlackmanWindow(var Data: TDAVSingleDynArray); overload;
procedure ApplyBlackmanHarrisWindow(var Data: TDAVSingleDynArray); overload;
procedure ApplyGaussianWindow(var Data: TDAVSingleDynArray); overload;
procedure ApplyKaiserBesselWindow(var Data: TDAVSingleDynArray; const Alpha: Single); overload;

procedure DoWinLoopCos2T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos2T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos2T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopCos2T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
procedure DoWinLoopCos3T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos3T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos3T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopCos3T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
procedure DoWinLoopCos4T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos4T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos4T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopCos4T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
procedure DoWinLoopCos5T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos5T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos5T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopCos5T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
procedure DoWinLoopCos6T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos6T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos6T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopCos6T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
procedure DoWinLoopCos7T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos7T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos7T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopCos7T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
procedure DoWinLoopCos8T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos8T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos8T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopCos8T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
procedure DoWinLoopCos9T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos9T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos9T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopCos9T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
procedure DoWinLoopCos10T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos10T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos10T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopCos10T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
procedure DoWinLoopCos11T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos11T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos11T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopCos11T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
procedure DoWinLoopTriangle32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopTriangle64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopTriangle32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopTriangle64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
procedure DoWinLoopCosine32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCosine64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCosine32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopCosine64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
procedure DoWinLoopLanczos32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopLanczos64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopLanczos32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopLanczos64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
procedure DoWinLoopHanning32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopHanning64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopHanning32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopHanning64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);

implementation

uses
  DAV_Math;

// Generate window function (Triangle)
procedure ApplyTriangleWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer);
var
  i : Integer;
begin
 for i := 0 to (SampleFrames div 2) - 1
  do Data^[i] := i / (SampleFrames div 2) * Data^[i];
 for i := (SampleFrames div 2) to SampleFrames - 1
  do Data^[i] := (SampleFrames - i) / (SampleFrames div 2) * Data^[i];
end;

procedure ApplyTriangleWindow(var Data: TDAVSingleDynArray);
begin
 ApplyHanningWindow(@Data[0], Length(Data));
end;


// Generate window function (Hanning)
procedure ApplyHanningWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer);
var
  i, j : Integer;
  k    : Double;
begin
 j := SampleFrames - 1;
 k := 1 / j;
 for i := 0 to j
  do Data^[i] := Data^[i] * (0.5 * (1.0 - cos(2 * PI * i * k)));
end;

procedure ApplyHanningWindow(var Data: TDAVSingleDynArray);
begin
 ApplyHanningWindow(@Data[0], Length(Data));
end;


// Generate window function (Hamming)
procedure ApplyHammingWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer);
var
  i, j : Integer;
  k    : Double;
begin
 j := SampleFrames - 1;
 k := 1 / j;
 for i := 0 to j
  do Data^[i] := Data^[i] * (0.54 - (0.46 * cos(2 * PI * i * k)));
end;

procedure ApplyHammingWindow(var Data: TDAVSingleDynArray);
begin
 ApplyHammingWindow(@Data[0], Length(Data));
end;


// Generate window function (Gaussian)
procedure ApplyGaussianWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer);
var
  i, j : Integer;
begin
 j := SampleFrames - 1;
 for i := 0 to j
  do Data^[i] := Data^[i] * (exp(-5.0 / (sqr(j)) * (2 * i - j) * (2 * i - j)));
end;

procedure ApplyGaussianWindow(var Data: TDAVSingleDynArray);
begin
 ApplyGaussianWindow(@Data[0], Length(Data));
end;


// Generate window function (Blackman)
procedure ApplyBlackmanWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer);
var
  Sample : Integer;
  Phase  : TComplexDouble;
  Value  : TComplexDouble;
const
  CBlackman : array [0..2] of Double = (0.34, -0.5, 0.16);
begin
 Value.Re := 1;
 Value.Im := 0;
 GetSinCos(2 * PI / (SampleFrames - 1), Phase.Im, Phase.Re);
 for Sample := 0 to SampleFrames - 1 do
  begin
   // using the chebyshev polynom identity to get rid of the cos(2*x)
   Data^[Sample] := Data^[Sample] * (CBlackman[0] + Value.Re * (CBlackman[1] + CBlackman[2] * Value.Re));
   ComplexMultiplyInplace64(Value, Phase);
  end;
end;

procedure ApplyBlackmanWindow(var Data: TDAVSingleDynArray);
begin
 ApplyBlackmanWindow(@Data[0], Length(Data));
end;


// Generate window function (Blackman-Harris)
procedure ApplyBlackmanHarrisWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer);
var
  i, j : Integer;
  k    : Double;
begin
 j := SampleFrames - 1;
 k := 1 / j;
 for i := 0 to j
  do Data^[i] := Data^[i] * (0.35875 - 0.48829 * cos(2 * PI * (i + 0.5) * k)
                           + 0.14128 * cos(4 * PI * (i + 0.5) * k)
                           - 0.01168 * cos(6 * PI * (i + 0.5) * k));
end;

procedure ApplyBlackmanHarrisWindow(var Data: TDAVSingleDynArray);
begin
 ApplyBlackmanHarrisWindow(@Data[0], Length(Data));
end;


function Io(const Value: Double): Double;
var
  y, de : Double;
  i     : Integer;
  sde   : Double;
const
  CEpsilon: Double = 1E-08;
begin
 y := 0.5 * Value;
 de := 1.0;
 Result := 1;
 for i := 1 to 25 do
  begin
   de := de * y / i;
   sde := sqr(de);
   Result := Result + sde;
   if (Result * CEpsilon - sde) > 0
    then break;
  end;
end;

// Generate window function (Kaiser-Bessel)
procedure ApplyKaiserBesselWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer; const Alpha: Single); overload;
var
  i    : Integer;
  bes  : Double;
  odd  : Integer;
  xi   : Double;
  xind : Double;
begin
 bes := 1.0 / Io(Alpha);
 odd := SampleFrames mod 2;
 xind := sqr(SampleFrames - 1);
 for i := 0 to SampleFrames - 1 do
  begin
   if (odd = 1)
    then xi := i + 0.5
    else xi := i;
   xi  := 4 * sqr(xi);
   Data^[i] := Io(Alpha * sqrt(1 - xi/xind)) * bes;
  end;
end;

procedure ApplyKaiserBesselWindow(var Data: TDAVSingleDynArray; const Alpha: Single);
begin
 ApplyKaiserBesselWindow(@Data[0], Length(Data), Alpha);
end;

procedure DoWinLoopCos2T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld st(1)
   fmul [ebx + 8].Double
   fadd [ebx].Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (CoefficientPointer[0] - PDAV2SingleArray(CoefficientPointer)^[1] * ComplexPosition.Im);
    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re - ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end
end;
{$ENDIF}

procedure DoWinLoopCos2T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld st(1)
   fmul [ebx + 8].Double
   fadd [ebx].Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (CoefficientPointer[0] - PDAV2DoubleArray(CoefficientPointer)^[1] * ComplexPosition.Im);
    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re - ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end
end;
{$ENDIF}

procedure DoWinLoopCos2T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld st(1)
   fmul [ebx + 8].Double
   fadd [ebx].Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fld [edi].Single
   fmul st(0), st(1)
   fstp [edi].Single

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4
   sub edi, 4

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := CoefficientPointer[0] - PDAV2SingleArray(CoefficientPointer)^[1] * ComplexPosition.Im;
    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor  := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re - ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end
end;
{$ENDIF}

procedure DoWinLoopCos2T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld st(1)
   fmul [ebx + 8].Double
   fadd [ebx].Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fld [edi].Double
   fmul st(0), st(1)
   fstp [edi].Double

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8
   sub edi, 8

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop edi
 pop ebx
@exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := CoefficientPointer[0] - PDAV2DoubleArray(CoefficientPointer)^[1] * ComplexPosition.Im;
    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor  := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re - ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end
end;
{$ENDIF}

procedure DoWinLoopCos3T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := PDAV4SingleArray(CoefficientPointer)[0] + ComplexPosition.Im  *
      (PDAV4SingleArray(CoefficientPointer)^[1] + ComplexPosition.Im  *
       PDAV4SingleArray(CoefficientPointer)^[2]);
    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re - ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end
end;
{$ENDIF}

procedure DoWinLoopCos3T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV4DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im  *
          (PDAV4DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im  *
           PDAV4DoubleArray(CoefficientPointer)[2]));
    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re - ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos3T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fld [edi].Single
   fmul st(0), st(1)
   fstp [edi].Single

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4
   sub edi, 4

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV4SingleArray(CoefficientPointer)[0] + ComplexPosition.Im  *
          (PDAV4SingleArray(CoefficientPointer)[1] + ComplexPosition.Im  *
           PDAV4SingleArray(CoefficientPointer)[2]));
    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re - ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos3T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fld [edi].Double
   fmul st(0), st(1)
   fstp [edi].Double

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8
   sub edi, 8

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV4DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im  *
          (PDAV4DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im  *
           PDAV4DoubleArray(CoefficientPointer)[2]));
    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos4T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV4SingleArray(CoefficientPointer)[0] + ComplexPosition.Im  * 
      (PDAV4SingleArray(CoefficientPointer)[1] + ComplexPosition.Im  *
      (PDAV4SingleArray(CoefficientPointer)[2] + ComplexPosition.Im  *
       PDAV4SingleArray(CoefficientPointer)[3])));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos4T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV4DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im *
          (PDAV4DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im *
          (PDAV4DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
           PDAV4DoubleArray(CoefficientPointer)[3])));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos4T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fld [edi].Single
   fmul st(0), st(1)
   fstp [edi].Single

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4
   sub edi, 4

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV4SingleArray(CoefficientPointer)[0] + ComplexPosition.Im *
      (PDAV4SingleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV4SingleArray(CoefficientPointer)[2] + ComplexPosition.Im *
       PDAV4SingleArray(CoefficientPointer)[3])));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos4T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fld [edi].Double
   fmul st(0), st(1)
   fstp [edi].Double

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8
   sub edi, 8

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV4DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im *
      (PDAV4DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV4DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
       PDAV4DoubleArray(CoefficientPointer)[3])));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos5T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV6SingleArray(CoefficientPointer)[0] + ComplexPosition.Im * 
      (PDAV6SingleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV6SingleArray(CoefficientPointer)[2] + ComplexPosition.Im *
      (PDAV6SingleArray(CoefficientPointer)[3] + ComplexPosition.Im *
       PDAV6SingleArray(CoefficientPointer)[4]))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos5T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV6DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im *
       PDAV6DoubleArray(CoefficientPointer)[4]))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos5T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fld [edi].Single
   fmul st(0), st(1)
   fstp [edi].Single

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4
   sub edi, 4

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV6SingleArray(CoefficientPointer)[0] + ComplexPosition.Im *
      (PDAV6SingleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV6SingleArray(CoefficientPointer)[2] + ComplexPosition.Im *
      (PDAV6SingleArray(CoefficientPointer)[3] + ComplexPosition.Im *
       PDAV6SingleArray(CoefficientPointer)[4]))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos5T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fld [edi].Double
   fmul st(0), st(1)
   fstp [edi].Double

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8
   sub edi, 8

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV6DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im *
       PDAV6DoubleArray(CoefficientPointer)[4]))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos6T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV6SingleArray(CoefficientPointer)[0] + ComplexPosition.Im *
      (PDAV6SingleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV6SingleArray(CoefficientPointer)[2] + ComplexPosition.Im *
      (PDAV6SingleArray(CoefficientPointer)[3] + ComplexPosition.Im *
      (PDAV6SingleArray(CoefficientPointer)[4] + ComplexPosition.Im *
       PDAV6SingleArray(CoefficientPointer)[5])))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos6T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV6DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[4] + ComplexPosition.Im *
       PDAV6DoubleArray(CoefficientPointer)[5])))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos6T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fld [edi].Single
   fmul st(0), st(1)
   fstp [edi].Single

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4
   sub edi, 4

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV6SingleArray(CoefficientPointer)[0] + ComplexPosition.Im *
      (PDAV6SingleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV6SingleArray(CoefficientPointer)[2] + ComplexPosition.Im *
      (PDAV6SingleArray(CoefficientPointer)[3] + ComplexPosition.Im *
      (PDAV6SingleArray(CoefficientPointer)[4] + ComplexPosition.Im *
       PDAV6SingleArray(CoefficientPointer)[5])))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos6T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fld [edi].Double
   fmul st(0), st(1)
   fstp [edi].Double

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8
   sub edi, 8

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV6DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[4] + ComplexPosition.Im *
       PDAV6DoubleArray(CoefficientPointer)[5])))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos7T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld [ebx + 48].Double
   fmul st(0), st(2)
   fadd [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV8SingleArray(CoefficientPointer)[0] + ComplexPosition.Im *
      (PDAV8SingleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV8SingleArray(CoefficientPointer)[2] + ComplexPosition.Im *
      (PDAV8SingleArray(CoefficientPointer)[3] + ComplexPosition.Im *
      (PDAV8SingleArray(CoefficientPointer)[4] + ComplexPosition.Im *
      (PDAV8SingleArray(CoefficientPointer)[5] + ComplexPosition.Im *
       PDAV8SingleArray(CoefficientPointer)[6]))))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos7T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld [ebx + 48].Double
   fmul st(0), st(2)
   fadd [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV8DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[4] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[5] + ComplexPosition.Im *
       PDAV8DoubleArray(CoefficientPointer)[6]))))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos7T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld [ebx + 48].Double
   fmul st(0), st(2)
   fadd [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fld [edi].Single
   fmul st(0), st(1)
   fstp [edi].Single

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4
   sub edi, 4

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV8SingleArray(CoefficientPointer)[0] + ComplexPosition.Im *
      (PDAV8SingleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV8SingleArray(CoefficientPointer)[2] + ComplexPosition.Im *
      (PDAV8SingleArray(CoefficientPointer)[3] + ComplexPosition.Im *
      (PDAV8SingleArray(CoefficientPointer)[4] + ComplexPosition.Im *
      (PDAV8SingleArray(CoefficientPointer)[5] + ComplexPosition.Im *
       PDAV8SingleArray(CoefficientPointer)[6]))))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos7T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld [ebx + 48].Double
   fmul st(0), st(2)
   fadd [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fld [edi].Double
   fmul st(0), st(1)
   fstp [edi].Double

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8
   sub edi, 8

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV8DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im * 
      (PDAV8DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[4] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[5] + ComplexPosition.Im *
       PDAV8DoubleArray(CoefficientPointer)[6]))))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos8T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld [ebx + 56].Double
   fmul st(0), st(2)
   fadd [ebx + 48].Double
   fmul st(0), st(2)
   fadd [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV8SingleArray(CoefficientPointer)[0] + ComplexPosition.Im *
      (PDAV8SingleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV8SingleArray(CoefficientPointer)[2] + ComplexPosition.Im *
      (PDAV8SingleArray(CoefficientPointer)[3] + ComplexPosition.Im *
      (PDAV8SingleArray(CoefficientPointer)[4] + ComplexPosition.Im *
      (PDAV8SingleArray(CoefficientPointer)[5] + ComplexPosition.Im *
      (PDAV8SingleArray(CoefficientPointer)[6] + ComplexPosition.Im *
       PDAV8SingleArray(CoefficientPointer)[7])))))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos8T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld [ebx + 56].Double
   fmul st(0), st(2)
   fadd [ebx + 48].Double
   fmul st(0), st(2)
   fadd [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV8DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[4] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[5] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[6] + ComplexPosition.Im *
       PDAV8DoubleArray(CoefficientPointer)[7])))))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos8T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld [ebx + 56].Double
   fmul st(0), st(2)
   fadd [ebx + 48].Double
   fmul st(0), st(2)
   fadd [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fld [edi].Single
   fmul st(0), st(1)
   fstp [edi].Single

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4
   sub edi, 4

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV8SingleArray(CoefficientPointer)[0] + ComplexPosition.Im *
      (PDAV8SingleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV8SingleArray(CoefficientPointer)[2] + ComplexPosition.Im *
      (PDAV8SingleArray(CoefficientPointer)[3] + ComplexPosition.Im *
      (PDAV8SingleArray(CoefficientPointer)[4] + ComplexPosition.Im *
      (PDAV8SingleArray(CoefficientPointer)[5] + ComplexPosition.Im *
      (PDAV8SingleArray(CoefficientPointer)[6] + ComplexPosition.Im *
       PDAV8SingleArray(CoefficientPointer)[7])))))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos8T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld [ebx + 56].Double
   fmul st(0), st(2)
   fadd [ebx + 48].Double
   fmul st(0), st(2)
   fadd [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fld [edi].Double
   fmul st(0), st(1)
   fstp [edi].Double

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8
   sub edi, 8

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV8DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im *
        (PDAV8DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im *
        (PDAV8DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
        (PDAV8DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im *
        (PDAV8DoubleArray(CoefficientPointer)[4] + ComplexPosition.Im *
        (PDAV8DoubleArray(CoefficientPointer)[5] + ComplexPosition.Im *
        (PDAV8DoubleArray(CoefficientPointer)[6] + ComplexPosition.Im *
         PDAV8DoubleArray(CoefficientPointer)[7])))))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos9T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld [ebx + 64].Double
   fmul st(0), st(2)
   fadd [ebx + 56].Double
   fmul st(0), st(2)
   fadd [ebx + 48].Double
   fmul st(0), st(2)
   fadd [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV16SingleArray(CoefficientPointer)[0] + ComplexPosition.Im * 
      (PDAV16SingleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV16SingleArray(CoefficientPointer)[2] + ComplexPosition.Im *
      (PDAV16SingleArray(CoefficientPointer)[3] + ComplexPosition.Im *
      (PDAV16SingleArray(CoefficientPointer)[4] + ComplexPosition.Im *
      (PDAV16SingleArray(CoefficientPointer)[5] + ComplexPosition.Im *
      (PDAV16SingleArray(CoefficientPointer)[6] + ComplexPosition.Im *
      (PDAV16SingleArray(CoefficientPointer)[7] + ComplexPosition.Im *
       PDAV16SingleArray(CoefficientPointer)[8]))))))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos9T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld [ebx + 64].Double
   fmul st(0), st(2)
   fadd [ebx + 56].Double
   fmul st(0), st(2)
   fadd [ebx + 48].Double
   fmul st(0), st(2)
   fadd [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV16DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[4] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[5] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[6] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[7] + ComplexPosition.Im *
       PDAV16DoubleArray(CoefficientPointer)[8]))))))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos9T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld [ebx + 64].Double
   fmul st(0), st(2)
   fadd [ebx + 56].Double
   fmul st(0), st(2)
   fadd [ebx + 48].Double
   fmul st(0), st(2)
   fadd [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fld [edi].Single
   fmul st(0), st(1)
   fstp [edi].Single

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4
   sub edi, 4

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV16SingleArray(CoefficientPointer)[0] + ComplexPosition.Im * 
      (PDAV16SingleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV16SingleArray(CoefficientPointer)[2] + ComplexPosition.Im *
      (PDAV16SingleArray(CoefficientPointer)[3] + ComplexPosition.Im *
      (PDAV16SingleArray(CoefficientPointer)[4] + ComplexPosition.Im *
      (PDAV16SingleArray(CoefficientPointer)[5] + ComplexPosition.Im *
      (PDAV16SingleArray(CoefficientPointer)[6] + ComplexPosition.Im *
      (PDAV16SingleArray(CoefficientPointer)[7] + ComplexPosition.Im *
       PDAV16SingleArray(CoefficientPointer)[8]))))))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos9T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld [ebx + 64].Double
   fmul st(0), st(2)
   fadd [ebx + 56].Double
   fmul st(0), st(2)
   fadd [ebx + 48].Double
   fmul st(0), st(2)
   fadd [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fld [edi].Double
   fmul st(0), st(1)
   fstp [edi].Double

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8
   sub edi, 8

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV16DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[4] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[5] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[6] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[7] + ComplexPosition.Im *
       PDAV16DoubleArray(CoefficientPointer)[8]))))))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos10T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld [ebx + 72].Double
   fmul st(0), st(2)
   fadd [ebx + 64].Double
   fmul st(0), st(2)
   fadd [ebx + 56].Double
   fmul st(0), st(2)
   fadd [ebx + 48].Double
   fmul st(0), st(2)
   fadd [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV16SingleArray(CoefficientPointer)[0] + ComplexPosition.Im *
        (PDAV16SingleArray(CoefficientPointer)[1] + ComplexPosition.Im *
        (PDAV16SingleArray(CoefficientPointer)[2] + ComplexPosition.Im * 
        (PDAV16SingleArray(CoefficientPointer)[3] + ComplexPosition.Im * 
        (PDAV16SingleArray(CoefficientPointer)[4] + ComplexPosition.Im * 
        (PDAV16SingleArray(CoefficientPointer)[5] + ComplexPosition.Im * 
        (PDAV16SingleArray(CoefficientPointer)[6] + ComplexPosition.Im * 
        (PDAV16SingleArray(CoefficientPointer)[7] + ComplexPosition.Im *
        (PDAV16SingleArray(CoefficientPointer)[8] + ComplexPosition.Im *
         PDAV16SingleArray(CoefficientPointer)[9])))))))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos10T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld [ebx + 72].Double
   fmul st(0), st(2)
   fadd [ebx + 64].Double
   fmul st(0), st(2)
   fadd [ebx + 56].Double
   fmul st(0), st(2)
   fadd [ebx + 48].Double
   fmul st(0), st(2)
   fadd [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV16DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[4] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[5] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[6] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[7] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[8] + ComplexPosition.Im * 
         PDAV16DoubleArray(CoefficientPointer)[9])))))))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos10T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld [ebx + 72].Double
   fmul st(0), st(2)
   fadd [ebx + 64].Double
   fmul st(0), st(2)
   fadd [ebx + 56].Double
   fmul st(0), st(2)
   fadd [ebx + 48].Double
   fmul st(0), st(2)
   fadd [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fld [edi].Single
   fmul st(0), st(1)
   fstp [edi].Single

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4
   sub edi, 4

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV16SingleArray(CoefficientPointer)[0] + ComplexPosition.Im * 
        (PDAV16SingleArray(CoefficientPointer)[1] + ComplexPosition.Im * 
        (PDAV16SingleArray(CoefficientPointer)[2] + ComplexPosition.Im * 
        (PDAV16SingleArray(CoefficientPointer)[3] + ComplexPosition.Im * 
        (PDAV16SingleArray(CoefficientPointer)[4] + ComplexPosition.Im *
        (PDAV16SingleArray(CoefficientPointer)[5] + ComplexPosition.Im * 
        (PDAV16SingleArray(CoefficientPointer)[6] + ComplexPosition.Im * 
        (PDAV16SingleArray(CoefficientPointer)[7] + ComplexPosition.Im * 
        (PDAV16SingleArray(CoefficientPointer)[8] + ComplexPosition.Im * 
         PDAV16SingleArray(CoefficientPointer)[9])))))))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos10T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld [ebx + 72].Double
   fmul st(0), st(2)
   fadd [ebx + 64].Double
   fmul st(0), st(2)
   fadd [ebx + 56].Double
   fmul st(0), st(2)
   fadd [ebx + 48].Double
   fmul st(0), st(2)
   fadd [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fld [edi].Double
   fmul st(0), st(1)
   fstp [edi].Double

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8
   sub edi, 8

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV16DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[4] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[5] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[6] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[7] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[8] + ComplexPosition.Im * 
         PDAV16DoubleArray(CoefficientPointer)[9])))))))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos11T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld [ebx + 80].Double
   fmul st(0), st(2)
   fadd [ebx + 72].Double
   fmul st(0), st(2)
   fadd [ebx + 64].Double
   fmul st(0), st(2)
   fadd [ebx + 56].Double
   fmul st(0), st(2)
   fadd [ebx + 48].Double
   fmul st(0), st(2)
   fadd [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV16SingleArray(CoefficientPointer)[0] + ComplexPosition.Im * 
        (PDAV16SingleArray(CoefficientPointer)[1] + ComplexPosition.Im * 
        (PDAV16SingleArray(CoefficientPointer)[2] + ComplexPosition.Im * 
        (PDAV16SingleArray(CoefficientPointer)[3] + ComplexPosition.Im * 
        (PDAV16SingleArray(CoefficientPointer)[4] + ComplexPosition.Im * 
        (PDAV16SingleArray(CoefficientPointer)[5] + ComplexPosition.Im * 
        (PDAV16SingleArray(CoefficientPointer)[6] + ComplexPosition.Im * 
        (PDAV16SingleArray(CoefficientPointer)[7] + ComplexPosition.Im * 
        (PDAV16SingleArray(CoefficientPointer)[8] + ComplexPosition.Im *
        (PDAV16SingleArray(CoefficientPointer)[9] + ComplexPosition.Im * 
         PDAV16SingleArray(CoefficientPointer)[10]))))))))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos11T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld [ebx + 80].Double
   fmul st(0), st(2)
   fadd [ebx + 72].Double
   fmul st(0), st(2)
   fadd [ebx + 64].Double
   fmul st(0), st(2)
   fadd [ebx + 56].Double
   fmul st(0), st(2)
   fadd [ebx + 48].Double
   fmul st(0), st(2)
   fadd [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV16DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[4] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[5] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[6] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[7] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[8] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[9] + ComplexPosition.Im * 
         PDAV16DoubleArray(CoefficientPointer)[10]))))))))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos11T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld [ebx + 80].Double
   fmul st(0), st(2)
   fadd [ebx + 72].Double
   fmul st(0), st(2)
   fadd [ebx + 64].Double
   fmul st(0), st(2)
   fadd [ebx + 56].Double
   fmul st(0), st(2)
   fadd [ebx + 48].Double
   fmul st(0), st(2)
   fadd [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fld [edi].Single
   fmul st(0), st(1)
   fstp [edi].Single

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4
   sub edi, 4

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV16SingleArray(CoefficientPointer)[0] + ComplexPosition.Im * 
        (PDAV16SingleArray(CoefficientPointer)[1] + ComplexPosition.Im * 
        (PDAV16SingleArray(CoefficientPointer)[2] + ComplexPosition.Im * 
        (PDAV16SingleArray(CoefficientPointer)[3] + ComplexPosition.Im * 
        (PDAV16SingleArray(CoefficientPointer)[4] + ComplexPosition.Im * 
        (PDAV16SingleArray(CoefficientPointer)[5] + ComplexPosition.Im * 
        (PDAV16SingleArray(CoefficientPointer)[6] + ComplexPosition.Im * 
        (PDAV16SingleArray(CoefficientPointer)[7] + ComplexPosition.Im * 
        (PDAV16SingleArray(CoefficientPointer)[8] + ComplexPosition.Im * 
        (PDAV16SingleArray(CoefficientPointer)[9] + ComplexPosition.Im * 
         PDAV16SingleArray(CoefficientPointer)[10]))))))))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos11T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld [ebx + 80].Double
   fmul st(0), st(2)
   fadd [ebx + 72].Double
   fmul st(0), st(2)
   fadd [ebx + 64].Double
   fmul st(0), st(2)
   fadd [ebx + 56].Double
   fmul st(0), st(2)
   fadd [ebx + 48].Double
   fmul st(0), st(2)
   fadd [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fld [edi].Double
   fmul st(0), st(1)
   fstp [edi].Double

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8
   sub edi, 8

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV16DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[4] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[5] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[6] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[7] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[8] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[9] + ComplexPosition.Im *
         PDAV16DoubleArray(CoefficientPointer)[10]))))))))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopTriangle32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // Offset, fSpkCorFak, fSpkCorFakSq
 fld [edx     ].Double   // Counter, Offset, fSpkCorFak, fSpkCorFakSq

 @triloop:
   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fadd st(2), st(0)
   fld st(0)
   fmul st(0), st(0)
   faddp st(4), st(0)

   fadd st(0), st(1)
   add eax, 4
 loop @triloop

 fstp [edx     ].Double   // Counter, Offset, fSpkCorFak, fSpkCorFakSq
 fstp [edx + 16].Double   // Offset, fSpkCorFak, fSpkCorFakSq
 fstp [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double   // SpuaredCorrectionFactor

 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    SpectrumCorrectionFactor := SpectrumCorrectionFactor + ComplexPosition.Re;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + Sqr(ComplexPosition.Re);
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * ComplexPosition.Re;
    ComplexPosition.Re := ComplexPosition.Re + ComplexAngle.Re;
   end;
end;
{$ENDIF}

procedure DoWinLoopTriangle32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // Offset, fSpkCorFak, fSpkCorFakSq
 fld [edx     ].Double   // Counter, Offset, fSpkCorFak, fSpkCorFakSq

 @cosloop:
   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single
   fld [edi].Single
   fmul st(0), st(1)
   fstp [edi].Single

   fadd st(2), st(0)
   fadd st(2), st(0)
   fld st(0)
   fmul st(0), st(0)
   faddp st(4), st(0)

   fadd st(0), st(1)                 // Cnt + Ofs, Ofs, fSpkCorFak, fSpkCorFakSq
   add eax, 4
   sub edi, 4
 loop @cosloop

 fstp [edx     ].Double   // Counter, Offset, fSpkCorFak, fSpkCorFakSq
 fstp [edx + 16].Double   // Offset, fSpkCorFak, fSpkCorFakSq
 fstp [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double   // SpuaredCorrectionFactor

 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    SpectrumCorrectionFactor   := SpectrumCorrectionFactor + 2 * ComplexPosition.Re;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + 2 * Sqr(ComplexPosition.Re);
    StartAdr[SampleIndex]  := StartAdr[SampleIndex] * ComplexPosition.Re;
    EndAdr[-SampleIndex]   := EndAdr[-SampleIndex] * ComplexPosition.Re;
    ComplexPosition.Re := ComplexPosition.Re + ComplexAngle.Re;
   end;
end;
{$ENDIF}

procedure DoWinLoopTriangle64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // Offset, fSpkCorFak, fSpkCorFakSq
 fld [edx     ].Double   // Counter, Offset, fSpkCorFak, fSpkCorFakSq

 @triloop:
   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fadd st(2), st(0)
   fld st(0)
   fmul st(0), st(0)
   faddp st(4), st(0)

   fadd st(0), st(1)
   add eax, 8
 loop @triloop

 fstp [edx     ].Double   // Counter, Offset, fSpkCorFak, fSpkCorFakSq
 fstp [edx + 16].Double   // Offset, fSpkCorFak, fSpkCorFakSq
 fstp [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double   // SpuaredCorrectionFactor
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    SpectrumCorrectionFactor := SpectrumCorrectionFactor + ComplexPosition.Re;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + Sqr(ComplexPosition.Re);
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * ComplexPosition.Re;
    ComplexPosition.Re := ComplexPosition.Re + ComplexAngle.Re;
   end
end;
{$ENDIF}

procedure DoWinLoopTriangle64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // Offset, fSpkCorFak, fSpkCorFakSq
 fld [edx     ].Double   // Counter, Offset, fSpkCorFak, fSpkCorFakSq

 @cosloop:
   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double
   fld [edi].Double
   fmul st(0), st(1)
   fstp [edi].Double

   fadd st(2), st(0)
   fadd st(2), st(0)
   fld st(0)
   fmul st(0), st(0)
   faddp st(4), st(0)

   fadd st(0), st(1)                 // Cnt + Ofs, Ofs, fSpkCorFak, fSpkCorFakSq
   add eax, 8
   sub edi, 8
 loop @cosloop

 fstp [edx     ].Double   // Counter, Offset, fSpkCorFak, fSpkCorFakSq
 fstp [edx + 16].Double   // Offset, fSpkCorFak, fSpkCorFakSq
 fstp [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double   // SpuaredCorrectionFactor

 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    SpectrumCorrectionFactor   := SpectrumCorrectionFactor + 2 * ComplexPosition.Re;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + 2 * Sqr(ComplexPosition.Re);
    StartAdr[SampleIndex]  := StartAdr[SampleIndex] * ComplexPosition.Re;
    EndAdr[-SampleIndex]   := EndAdr[-SampleIndex] * ComplexPosition.Re;
    ComplexPosition.Re := ComplexPosition.Re + ComplexAngle.Re;
   end;
end;
{$ENDIF}

procedure DoWinLoopCosine32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld st(0)
   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im
   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im
   add eax, 4
 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
    CurrentValue := (0.5 -  0.5 * ComplexPosition.Im);

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
   end
end;
{$ENDIF}

procedure DoWinLoopCosine32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld st(0)
   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fld [edi].Single
   fmul st(0), st(1)
   fstp [edi].Single

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4
   sub edi, 4
 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor

 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
    CurrentValue := (0.5 -  0.5 * ComplexPosition.Im);

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
   end
end;
{$ENDIF}

procedure DoWinLoopCosine64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld st(0)
   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im
   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im
   add eax, 8
 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
    CurrentValue := (0.5 -  0.5 * ComplexPosition.Im);

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
   end
end;
{$ENDIF}

procedure DoWinLoopCosine64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld st(0)
   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fld [edi].Double
   fmul st(0), st(1)
   fstp [edi].Double

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im
   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im
   add eax, 8
   sub edi, 8
 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
    CurrentValue := (0.5 -  0.5 * ComplexPosition.Im);

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
   end
end;
{$ENDIF}

procedure DoWinLoopLanczos32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld st(0)
   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im
   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im
   add eax, 4
 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
    CurrentValue := (0.5 -  0.5 * ComplexPosition.Im);

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
   end
end;
{$ENDIF}

procedure DoWinLoopLanczos32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld st(0)
   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fld [edi].Single
   fmul st(0), st(1)
   fstp [edi].Single

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4
   sub edi, 4
 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor

 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
    CurrentValue := (0.5 -  0.5 * ComplexPosition.Im);

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
   end
end;
{$ENDIF}

procedure DoWinLoopLanczos64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld st(0)
   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im
   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im
   add eax, 8
 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
    CurrentValue := (0.5 -  0.5 * ComplexPosition.Im);

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
   end
end;
{$ENDIF}

procedure DoWinLoopLanczos64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld st(0)
   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double
   fld [edi].Double
   fmul st(0), st(1)
   fstp [edi].Double

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im
   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im
   add eax, 8
   sub edi, 8
 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
    CurrentValue := (0.5 -  0.5 * ComplexPosition.Im);

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
   end
end;
{$ENDIF}

procedure DoWinLoopHanning32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld st(1)
   fmul CHanning.Double
   fsub CHanning.Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re - ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
    CurrentValue := (0.5 -  0.5 * ComplexPosition.Im);

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
   end
end;
{$ENDIF}

procedure DoWinLoopHanning32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld st(1)
   fmul CHanning.Double
   fsub CHanning.Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fld [edi].Single
   fmul st(0), st(1)
   fstp [edi].Single

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4
   sub edi, 4

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re - ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
    CurrentValue := (0.5 -  0.5 * ComplexPosition.Im);

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
   end
end;
{$ENDIF}

procedure DoWinLoopHanning64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld st(1)
   fmul CHanning.Double
   fsub CHanning.Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re - ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
    CurrentValue := (0.5 -  0.5 * ComplexPosition.Im);

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
   end
end;
{$ENDIF}

procedure DoWinLoopHanning64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpuaredCorrectionFactor
 fld [edx + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fld [edx     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @cosloop:
   fld st(1)
   fmul CHanning.Double
   fsub CHanning.Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fld [edi].Double
   fmul st(0), st(1)
   fstp [edi].Double

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Im * i, SampleIndex, i, Re, Im
   fld st(3)             // Re, Im * i, SampleIndex, i, Re, Im
   fmul st(0), st(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   fsubrp                // newRe, SampleIndex, i, Re, Im

   fld st(4)             // Im, newRe, SampleIndex, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * SampleIndex, i, Re, Im
   fld st(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   fxch                  // Im * SampleIndex, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8
   sub edi, 8

 loop @cosloop

 fstp [edx     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp st(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 fstp [edx + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 fstp [edx + 40].Double  // SpuaredCorrectionFactor
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re - ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
    CurrentValue := (0.5 -  0.5 * ComplexPosition.Im);

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor  := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
   end
end;
{$ENDIF}

end.