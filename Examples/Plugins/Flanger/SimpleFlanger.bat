@"..\..\..\Bin\Win32\madExceptPatch.exe" "..\..\..\Bin\Win32\VST\SimpleFlanger.dll " .\SimpleFlanger.mes
@if exist "..\..\..\Bin\Win32\VST\SimpleFlanger.dll" @move "..\..\..\Bin\Win32\VST\SimpleFlanger.dll" "..\..\..\Bin\Win32\VST\Flanger.dll"
@if exist "..\..\..\Bin\Win64\VST\SimpleFlanger.dll" @move "..\..\..\Bin\Win64\VST\SimpleFlanger.dll" "..\..\..\Bin\Win64\VST\Flanger.dll"
@if not exist "..\..\..\Bin\Win32\VST\Flanger.dll" goto Error
@if not exist "..\..\..\Bin\Win64\VST\Flanger.dll" goto Error
@"..\..\..\Bin\Win32\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win32\VST\Flanger.dll"
@"..\..\..\Bin\Win64\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win64\VST\Flanger.dll"
@"..\..\..\Bin\Win32\VST-Plugin Screenshot Tool.exe" "..\..\..\Bin\Win32\VST\Flanger.dll" "..\..\..\Screenshots\Flanger.png"
@7z a "..\..\..\Archive\Flanger.7z" "..\..\..\Bin\*\VST\Flanger.dll" "..\..\..\Bin\License.txt"
@makensis /V2 "..\..\..\Install Scripts\Install Script Flanger.nsi"
@IF ERRORLEVEL==1 GOTO Error
@ftps -s:"..\..\..\Release Scripts\Flanger.ftp"
@WinSCP -script="..\..\..\Release Scripts\Flanger.scp"
Exit
:Error
echo Script Error
Pause
