object BaxxpanderModule: TBaxxpanderModule
  Flags = [effFlagsHasEditor, effFlagsCanMono, effFlagsCanReplacing]
  Version = '1'
  EffectName = 'Baxxpander'
  ProductName = 'DAV Effect Examples'
  VendorName = 'Delphi ASIO & VST Project'
  VersionRelease = 1
  CanDos = [vcdReceiveVstMidiEvent, vcdReceiveVstTimeInfo, vcdPlugAsSend, vcdMixDryWet, vcdMetapass, vcd1in1out, vcd2in2out, vcd4in4out, vcd4in8out, vcd8in4out, vcdLiveWithoutToolbar, vcdBypass, vcdCockosExtension]
  SampleRate = 44100.000000000000000000
  CurrentProgramName = 'Bass Only'
  IORatio = 1.000000000000000000
  UniqueID = 'BAXX'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Bass Only'
      VSTModule = Owner
    end
    item
      DisplayName = 'Moderate Saturation'
      VSTModule = Owner
    end
    item
      DisplayName = 'High Shape Saturation'
      VSTModule = Owner
    end
    item
      DisplayName = 'High Limit Saturation'
      VSTModule = Owner
    end
    item
      DisplayName = 'Maximum Saturation'
      VSTModule = Owner
    end
    item
      DisplayName = '50% Mix Maximum Saturation'
      VSTModule = Owner
    end
    item
      DisplayName = '25% Mix Maximum Saturation'
      VSTModule = Owner
    end
    item
      DisplayName = '50% Mix Bass Only'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Dry/Wet'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'Dry/Wet'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParameterDryWetChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Limit'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'Limit'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParameterLimitChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Mixer'
      Flags = [ppfParameterUsesFloatStep]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      Min = -100.000000000000000000
      MinInteger = -100
      ReportVST2Properties = True
      ShortLabel = 'Mixer'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParameterMixerChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'On/Off'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'On/Off'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterOnOffChange
      OnCustomParameterDisplay = ParameterOnOffDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Shape'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'Shape'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParameterShapeChange
    end>
  ParameterCategories = <>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnProcess = VSTModuleProcessNormal
  OnProcess32Replacing = VSTModuleProcessNormal
  OnSampleRateChange = VSTModuleSampleRateChange
  Left = 218
  Top = 77
  Height = 150
  Width = 215
end
