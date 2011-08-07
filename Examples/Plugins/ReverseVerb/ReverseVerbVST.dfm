object ReverseVerbDataModule: TReverseVerbDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Version = '1.0'
  EffectName = 'ReverseVerb'
  ProductName = 'DAV Effect Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  BlockSize = 8192
  CurrentProgramName = 'Forward Reverb'
  BlockModeSize = 8192
  InitialDelay = 2048
  IORatio = 1.000000000000000000
  UniqueID = 'Conv'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Forward Reverb'
      VSTModule = Owner
    end
    item
      DisplayName = 'Smooth Backward'
      VSTModule = Owner
    end
    item
      DisplayName = 'Rough Backward'
      VSTModule = Owner
    end
    item
      DisplayName = 'Mixed Backward'
      VSTModule = Owner
    end
    item
      DisplayName = 'Just Backward'
      VSTModule = Owner
    end
    item
      DisplayName = 'Short Compressed'
      VSTModule = Owner
    end
    item
      DisplayName = 'Dry'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Direct Gain'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 6.000000000000000000
      MaxInteger = 6
      Min = -90.000000000000000000
      MinInteger = -90
      ReportVST2Properties = True
      ShortLabel = 'Direct '
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterDirectChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Early Reflections Gain'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 6.000000000000000000
      MaxInteger = 6
      Min = -90.000000000000000000
      MinInteger = -90
      ReportVST2Properties = True
      ShortLabel = 'ER Gain'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterERGainChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100.000000000000000000
      DisplayName = 'ER Time'
      LargeStepFloat = 2.000000000000000000
      Max = 300.000000000000000000
      MaxInteger = 300
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'ER Time'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterERTimeChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Tail Gain'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 6.000000000000000000
      MaxInteger = 6
      Min = -90.000000000000000000
      MinInteger = -90
      ReportVST2Properties = True
      ShortLabel = 'Tail Ga'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterTailChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100.000000000000000000
      DisplayName = 'T60'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 1000.000000000000000000
      MaxInteger = 1000
      Min = 10.000000000000000000
      MinInteger = 10
      ReportVST2Properties = True
      ShortLabel = 'T60'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterT60Change
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      DisplayName = 'Damping'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 1000.000000000000000000
      LargeStepInteger = 1000
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ReportVST2Properties = True
      ShortLabel = 'Damping'
      SmallStepFloat = 10.000000000000000000
      StepFloat = 100.000000000000000000
      StepInteger = 100
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParameterDampingChange
      OnCustomParameterLabel = ParameterDampingLabel
      OnCustomParameterDisplay = ParameterDampingDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Direction'
      Flags = [ppfParameterIsSwitch, ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Dir.'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterDirectionChange
      OnCustomParameterDisplay = ParameterDirectionDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Compressor Threshold'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      MaxInteger = 0
      Min = -60.000000000000000000
      MinInteger = -60
      ReportVST2Properties = True
      ShortLabel = 'Thres.'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterThresholdChange
      Max = 0.000000000000000000
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100.000000000000000000
      DisplayName = 'Compressor Ratio'
      Flags = [ppfParameterUsesFloatStep]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Ratio'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = ' : 1'
      VSTModule = Owner
      OnParameterChange = ParameterCompressorRatioChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Output Gain'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      Max = 6.000000000000000000
      MaxInteger = 6
      Min = -30.000000000000000000
      MinInteger = -30
      ReportVST2Properties = True
      ShortLabel = 'Gain'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterOutputGainChange
    end>
  ParameterCategories = <>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnAfterProgramChange = VSTModuleAfterProgramChange
  OnBeforeProgramChange = VSTModuleBeforeProgramChange
  OnBlockSizeChange = VSTModuleBlockSizeChange
  OnProcess = VSTModuleProcessForward
  OnProcess32Replacing = VSTModuleProcessForward
  OnSampleRateChange = VSTModuleSampleRateChange
  Left = 191
  Top = 76
  Height = 150
  Width = 215
end
