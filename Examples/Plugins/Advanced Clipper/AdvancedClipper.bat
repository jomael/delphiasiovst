@copy "..\..\..\Bin\VST\32-Bit\AdvancedClipper.DLL" "..\..\..\Bin\VST\32-Bit\Advanced Clipper.DLL"
@"..\..\..\Bin\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\VST\32-Bit\Advanced Clipper.DLL"
@"..\..\..\Bin\VST\32-Bit\VstPluginScreenshotTool.exe" "..\..\..\Bin\VST\32-Bit\Advanced Clipper.DLL"
@move "..\..\..\Bin\VST\32-Bit\AdvancedClipper.png" "..\..\..\Screenshots\Advanced Clipper.png"
@7z.exe a "..\..\..\Archive\AdvancedClipper.7z" "..\..\..\Bin\VST\*\Advanced Clipper.dll" "..\..\..\Manuals\Advanced Clipper.pdf" "..\..\..\Bin\License.txt"
@makensis.exe /V2 "..\..\..\Install Scripts\Install Script Advanced Clipper.nsi"
@ftps -s:"..\..\..\Release Scripts\Advanced Clipper.ftp"
@WinSCP -script="..\..\..\Release Scripts\Advanced Clipper.scp"