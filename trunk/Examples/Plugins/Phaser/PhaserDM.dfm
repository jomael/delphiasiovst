object PhaserModule: TPhaserModule
  OldCreateOrder = False
  Flags = [effFlagsHasEditor, effFlagsCanMono, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'Phaser VST Example'
  ProductName = 'DAV Effect Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Default'
  IORatio = 1.000000000000000000
  UniqueID = 'phsx'
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
      DisplayName = 'Depth'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesFloatStep, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 100.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'depth'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = PMDepthChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Feedback'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesFloatStep, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 100.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'fdbck'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = PMFeedbackChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      DisplayName = 'Minimum'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 200.000000000000000000
      LargeStepInteger = 200
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ReportVST2Properties = True
      ShortLabel = 'min'
      SmallStepFloat = 20.000000000000000000
      StepFloat = 20.000000000000000000
      StepInteger = 20
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = PMMinimumChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      DisplayName = 'Maximum'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 200.000000000000000000
      LargeStepInteger = 200
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ReportVST2Properties = True
      ShortLabel = 'max'
      SmallStepFloat = 20.000000000000000000
      StepFloat = 20.000000000000000000
      StepInteger = 20
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = PMMaximumChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1.000000000000000000
      DisplayName = 'Rate'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 10.000000000000000000
      MaxInteger = 10
      ReportVST2Properties = True
      ShortLabel = 'rate'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = PMRateChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Stages'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 5.000000000000000000
      MaxInteger = 5
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'stage'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = PMStagesChange
    end>
  ParameterCategories = <>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcess
  OnProcessDoubleReplacing = VSTModuleProcessDoubleReplacing
  OnProcessReplacing = VSTModuleProcess
  Left = 272
  Top = 81
  Height = 150
  Width = 215
end
