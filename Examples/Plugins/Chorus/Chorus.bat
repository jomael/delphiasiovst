@"..\..\..\Bin\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win32\VST\Chorus.dll"
@"..\..\..\Bin\VstPluginScreenshotTool.exe" "..\..\..\Bin\Win32\VST\Chorus.dll"
@move "..\..\..\Bin\Win32\VST\Chorus.png" "..\..\..\Screenshots\Chorus.png"
@7z a "..\..\..\Archive\Chorus.7z" "..\..\..\Bin\*\VST\Chorus.dll" "..\..\..\Bin\License.txt" "..\..\..\Bin\Chorus.pdf"
@makensis /V2 "..\..\..\Install Scripts\Install Script Chorus.nsi"
@ftps -s:"..\..\..\Release Scripts\Chorus.ftp"
@WinSCP -script="..\..\..\Release Scripts\Chorus.scp"