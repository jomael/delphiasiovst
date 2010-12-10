object StkPitchShiftModule: TStkPitchShiftModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'Stk PitchShift'
  ProductName = 'DAV Stk Examples'
  VendorName = 'Delphi ASIO & VST Projects'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Default Network PitchShift'
  IORatio = 1.000000000000000000
  UniqueID = 'STKR'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default Network PitchShift'
      VSTModule = Owner
    end
    item
      DisplayName = 'Default JC PitchShift'
      VSTModule = Owner
    end
    item
      DisplayName = 'Default Blended PitchShift'
      VSTModule = Owner
    end
    item
      DisplayName = 'Short'
      VSTModule = Owner
    end
    item
      DisplayName = 'Long'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Semitones'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 24.000000000000000000
      MaxInteger = 24
      Min = -24.000000000000000000
      MinInteger = -24
      ReportVST2Properties = True
      ShortLabel = 'Semi'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamDelayChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Effect Mix'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'Mix'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParamMixChange
    end>
  ParameterCategories = <>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcess
  OnProcess64Replacing = VSTModuleProcessDoubleReplacing
  OnProcess32Replacing = VSTModuleProcess
  OnSampleRateChange = VSTModuleSampleRateChange
  Left = 284
  Top = 121
  Height = 150
  Width = 215
end
