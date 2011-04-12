object VOLDataModule: TVOLDataModule
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'VST Opcode Logger'
  ProductName = 'DAV Tool Example'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgramName = 'Init'
  IORatio = 1.000000000000000000
  UniqueID = 'DVOl'
  ShellPlugins = <>
  TruncateStrings = True
  Programs = <
    item
      DisplayName = 'Init'
      VSTModule = Owner
    end
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end
    item
      DisplayName = 'Test'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      Category = 'Category 1'
      DisplayName = 'Parameter 1'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'Param1'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Category 1'
      DisplayName = 'Parameter 2'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 20.000000000000000000
      MaxInteger = 20
      Min = 10.000000000000000000
      MinInteger = 10
      ReportVST2Properties = True
      ShortLabel = 'Param2'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Parameter 3'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ReportVST2Properties = True
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
    end>
  ParameterCategories = <
    item
      DisplayName = 'Category 1'
      VSTModule = Owner
    end>
  OnOpen = VSTModuleOpen
  OnEditOpen = VSTModuleEditOpen
  Left = 286
  Top = 77
  Height = 150
  Width = 215
end
