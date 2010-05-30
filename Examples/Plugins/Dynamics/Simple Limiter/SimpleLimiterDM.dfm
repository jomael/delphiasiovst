object SimpleLimiterDataModule: TSimpleLimiterDataModule
  OldCreateOrder = False
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'Simple Limiter'
  ProductName = 'DAV Dynamics Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Default'
  IORatio = 1.000000000000000000
  UniqueID = 'SiGa'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Threshold'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 0
      Min = -96.000000000000000000
      MinInteger = -96
      ReportVST2Properties = True
      ShortLabel = 'thrshld'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = SGDMThresholdChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Attack'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 5.000000000000000000
      LargeStepInteger = 2
      Max = 20.000000000000000000
      MaxInteger = 20
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Attack'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterAttackChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Release'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 5
      Max = 200.000000000000000000
      MaxInteger = 200
      Min = 10.000000000000000000
      MinInteger = 10
      ReportVST2Properties = True
      ShortLabel = 'Release'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterReleaseChange
    end>
  ParameterCategories = <>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  OnSampleRateChange = VSTModuleSampleRateChange
  Left = 218
  Top = 81
  Height = 150
  Width = 215
end
