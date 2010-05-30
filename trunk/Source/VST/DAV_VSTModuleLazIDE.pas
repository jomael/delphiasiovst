unit DAV_VSTModuleLazIDE;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, DAV_VSTModule, Forms, LazIDEIntf, ProjectIntf,
  FormEditingIntf;

type
  { TVSTModuleLibraryDescriptor }
  TVSTModuleLibraryDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;

  { TFileDescPascalUnitWithVSTModule }
  TFileDescPascalUnitWithVSTModule = class(TFileDescPascalUnitWithResource)
  public
    constructor Create; override;
    function GetInterfaceUsesSection: string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
  end;

var
  ProjectDescriptorVSTModuleLibrary: TVSTModuleLibraryDescriptor;
  FileDescriptorVSTModule: TFileDescPascalUnitWithVSTModule;

procedure Register;

implementation

procedure Register;
begin
  FileDescriptorVSTModule := TFileDescPascalUnitWithVSTModule.Create;
  RegisterProjectFileDescriptor(FileDescriptorVSTModule);
  ProjectDescriptorVSTModuleLibrary := TVSTModuleLibraryDescriptor.Create;
  RegisterProjectDescriptor(ProjectDescriptorVSTModuleLibrary);
  FormEditingHook.RegisterDesignerBaseClass(TVSTModule);
end;

{ TVSTModuleApplicationDescriptor }

constructor TVSTModuleLibraryDescriptor.Create;
begin
  inherited Create;
  Name := 'VSTModule';
end;

function TVSTModuleLibraryDescriptor.GetLocalizedName: string;
begin
  Result := 'VST Plugin';
end;

function TVSTModuleLibraryDescriptor.GetLocalizedDescription: string;
begin
  Result := 'VST Plugin'#13#13'VST Plugin Wizard in Free Pascal';
end;

function TVSTModuleLibraryDescriptor.InitProject(AProject: TLazProject): TModalResult;
var
  le: string;
  NewSource: String;
  MainFile: TLazProjectFile;
begin
  inherited InitProject(AProject);

  MainFile := AProject.CreateProjectFile('VSTPlugin1.lpr');
  MainFile.IsPartOfProject := True;
  AProject.AddFile(MainFile, False);
  AProject.MainFileID := 0;

  // create program source
  le := LineEnding;
  NewSource := 'library VSTPlugin1;' + le
    + le
    + '{$mode objfpc}{$H+}' + le
    + le
    + 'uses' + le
    + '  DAV_VSTEffect,' + le
    + '  DAV_VSTModule;' + le
    + le
    + 'function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;' + le
    + 'var VSTModule1 : TVSTModule1;' + le
    + 'begin' + le
    + ' try' + le
    + '  VSTModule1 := TVSTModule1.Create(nil);' + le
    + '  VSTModule1.Effect^.user := VSTModule1;' + le
    + '  VSTModule1.AudioMaster := audioMaster;' + le
    + '  Result := VSTModule1.Effect;' + le
    + ' except' + le
    + '  Result := nil;' + le
    + ' end;' + le
    + 'end;' + le
    + le
    + 'exports Main name ''main'';' + le
    + 'exports Main name ''VSTPluginMain'';' + le
    + le
    + 'begin' + le
    + 'end.';
  AProject.MainFile.SetSourceText(NewSource);

  // add
  AProject.AddPackageDependency('DAV_Common_Lazarus');
  AProject.AddPackageDependency('DAV_VSTPlugin_Lazarus');

  // compiler options
  AProject.LazCompilerOptions.Win32GraphicApp := True;
  AProject.LazCompilerOptions.ExecutableType := cetLibrary;
  Result :=  mrOK;
end;

function TVSTModuleLibraryDescriptor.CreateStartFiles(
  AProject: TLazProject): TModalResult;
begin
  if AProject = nil then ;
  LazarusIDE.DoNewEditorFile(FileDescriptorVSTModule, '', '',
    [nfIsPartOfProject, nfOpenInEditor, nfCreateDefaultSrc]);
  Result :=  mrOK;
end;

{ TFileDescPascalUnitWithVSTModule }

constructor TFileDescPascalUnitWithVSTModule.Create;
begin
  inherited Create;
  Name := 'VSTModule';
  ResourceClass := TVSTModule;
  UseCreateFormStatements := True;
end;

function TFileDescPascalUnitWithVSTModule.GetInterfaceUsesSection: string;
begin
  Result := inherited GetInterfaceUsesSection;
  Result := Result + ', DAV_VSTModule';
end;

function TFileDescPascalUnitWithVSTModule.GetLocalizedName: string;
begin
  Result := 'VST Module';
end;

function TFileDescPascalUnitWithVSTModule.GetLocalizedDescription: string;
begin
  Result := 'VST Module'#13
         +'A datamodule for VST Plugins';
end;

initialization

end.
