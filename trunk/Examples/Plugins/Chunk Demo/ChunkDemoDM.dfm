object ChunkDemoDataModule: TChunkDemoDataModule
  OldCreateOrder = True
  Flags = [effFlagsHasEditor, effFlagsCanReplacing, effFlagsProgramChunks]
  Version = '1.0'
  EffectName = 'ChunkDemo'
  ProductName = 'DAV Effect Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgramName = 'Preset 1 '
  IORatio = 1.000000000000000000
  UniqueID = 'Chnk'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Preset 1 '
      VSTModule = Owner
    end
    item
      DisplayName = 'Preset 2'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Alpha'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'Alpha'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterAlphaChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Beta'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'Beta'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterBetaChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Gamma'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'Gamma'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterGammaChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Delta'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'Delta'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterDeltaChange
    end>
  ParameterCategories = <>
  OnGetChunkParameter = VSTModuleGetChunkParameter
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnEditOpen = VSTModuleEditOpen
  OnParameterChange = VSTModuleParameterChange
  OnProcess = VSTModuleProcess
  OnProcess32Replacing = VSTModuleProcess
  OnSampleRateChange = VSTModuleSampleRateChange
  Left = 218
  Top = 77
  Height = 150
  Width = 215
end
