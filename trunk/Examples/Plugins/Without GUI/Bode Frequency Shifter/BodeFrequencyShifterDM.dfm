object BodeFrequencyShifterDataModule: TBodeFrequencyShifterDataModule
  OldCreateOrder = True
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Version = '1.0'
  EffectName = 'Bode Frequency Shifter'
  ProductName = 'DAV Effect Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  numInputs = 1
  numOutputs = 1
  CurrentProgramName = 'Default'
  IORatio = 1.000000000000000000
  UniqueID = 'DBFS'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLogarithmic
      CurveFactor = 100000.000000000000000000
      DisplayName = 'Frequency'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      Max = 1000.000000000000000000
      MaxInteger = 1000
      Min = 0.009999999776482582
      ReportVST2Properties = True
      ShortLabel = 'Freq.'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParameterFrequencyChange
      OnCustomParameterLabel = ParameterFrequencyLabel
      OnCustomParameterDisplay = ParameterFrequencyDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Mix'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      Min = -100.000000000000000000
      MinInteger = -100
      ReportVST2Properties = True
      ShortLabel = 'Mix'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParameterMixChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Coefficients'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 32.000000000000000000
      MaxInteger = 32
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Coeffs'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterCoeffsChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Transition Bandwidth'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      Max = 0.200000002980232200
      MaxInteger = 1
      Min = 0.000009999999747379
      ShortLabel = 'TranBW'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterTransitionBWChange
    end>
  ParameterCategories = <>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnProcess = VSTModuleProcessMono
  OnProcess32Replacing = VSTModuleProcessMono
  OnSampleRateChange = VSTModuleSampleRateChange
  Left = 286
  Top = 77
  Height = 150
  Width = 215
end
