;NSIS Modern User Interface version 1.70
;Lightweight Compressor Installer
;Written by Christian Budde

SetCompressor lzma

;--------------------------------
;Include Modern UI
;  !include "Sections.nsh"
  !include "MUI.nsh"
  !include "x64.nsh"


;--------------------------------
;General

  ;Name and file
  Name "Lightweight Series Installer"
  OutFile "Lightweight_Series_Install.exe"

  ;Default installation folder
  InstallDir "$PROGRAMFILES\VSTPlugIns"
  
  ;Get installation folder from registry if available
  InstallDirRegKey HKLM "SOFTWARE\VST" "VSTPluginsPath"

  BrandingText "Delphi ASIO && VST Project"

  ; Turn on the xp style of drawing
  XPStyle ON


;--------------------------------
;Variables

  Var BugReportState


;--------------------------------
;Interface Settings

  !define PRODUCT_NAME "Lightweight Series"
  !define PRODUCT_VERSION "1.0.1"
  !define PRODUCT_PUBLISHER "Christian Budde"
  !define PRODUCT_WEB_SITE "http://delphiasiovst.sourceforge.net/"
  !define PRODUCT_DIR_REGKEY "Software\Delphi ASIO & VST Packages\${PRODUCT_NAME}"
  !define PRODUCT_DIR_ROOT_KEY "HKLM"
  !define PRODUCT_UNINST_KEY "Software\Delphi ASIO & VST Packages\Uninstall\${PRODUCT_NAME}"
  !define PRODUCT_UNINST_ROOT_KEY "HKLM"
  !define MUI_ABORTWARNING


;--------------------------------
;Language Selection Dialog Settings

  ;Remember the installer language
  !define MUI_LANGDLL_REGISTRY_ROOT "HKLM" 
  !define MUI_LANGDLL_REGISTRY_KEY "SOFTWARE\Delphi ASIO & VST Packages\${PRODUCT_NAME}"
  !define MUI_LANGDLL_REGISTRY_VALUENAME "Installer Language"


;--------------------------------
;Reserve Files
  
  ;These files should be inserted before other files in the data block
  ;Keep these lines before any File command
  ;Only for solid compression (by default, solid compression is enabled for BZIP2 and LZMA)
  
    ReserveFile "madExcept Patch.dll"
    ReserveFile "ioBugReport.ini"
  !insertmacro MUI_RESERVEFILE_INSTALLOPTIONS
;  !insertmacro MUI_RESERVEFILE_LANGDLL


;--------------------------------
;Installer Functions

Function .onInit

;  !insertmacro MUI_LANGDLL_DISPLAY  
  !insertmacro MUI_INSTALLOPTIONS_EXTRACT "ioBugReport.ini"

FunctionEnd


;--------------------------------
;Pages

  !insertmacro MUI_PAGE_WELCOME
  !insertmacro MUI_PAGE_LICENSE "..\Bin\License.txt"
  !insertmacro MUI_PAGE_COMPONENTS
  !insertmacro MUI_PAGE_DIRECTORY
  Page custom BugReportPatch
  !insertmacro MUI_PAGE_INSTFILES
  !insertmacro MUI_PAGE_FINISH
  !insertmacro MUI_UNPAGE_WELCOME
  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES


;--------------------------------
;Languages
 
  !insertmacro MUI_LANGUAGE "English"
;  !insertmacro MUI_LANGUAGE "German"


;--------------------------------
;Installer Sections

Section "VST-Plugins" SecVSTPlugin
  SetOutPath "$INSTDIR"
  
  !system 'copy "..\Bin\LightweightLimiter.dll" "..\Bin\Lightweight Limiter.dll"'  
  !system 'copy "..\Bin\LightweightCompressor.dll" "..\Bin\Lightweight Compressor.dll"'  
  !system 'copy "..\Bin\LightweightGate.dll" "..\Bin\Lightweight Gate.dll"'  
  !system 'copy "..\Bin\LightweightFeedbackLimiter.dll" "..\Bin\Lightweight Feedback Limiter.dll"'  
  !system 'copy "..\Bin\LightweightFeedbackCompressor.dll" "..\Bin\Lightweight Feedback Compressor.dll"'  
  !system 'copy "..\Bin\LightweightUpwardCompressor.dll" "..\Bin\Lightweight Upward Compressor.dll"'  
  !system 'copy "..\Bin\LightweightMultibandCompressor.dll" "..\Bin\Lightweight Multiband Compressor.dll"'  
  
  ;ADD YOUR OWN FILES HERE...
  File "..\Bin\Lightweight Limiter.dll"
  File "..\Bin\Lightweight Compressor.dll"
  File "..\Bin\Lightweight Gate.dll"
  File "..\Bin\Lightweight Feedback Compressor.dll"
  File "..\Bin\Lightweight Feedback Limiter.dll"
  File "..\Bin\Lightweight Multiband Compressor.dll"
  File "..\Bin\Lightweight Upward Compressor.dll"

  !insertmacro MUI_INSTALLOPTIONS_READ $BugReportState "ioBugReport.ini" "Field 1" "State"  
  IntCmp $BugReportState 0 SkipDLLCall
    
  SetOutPath $TEMP                      ; create temp directory
  File "madExcept Patch.dll"            ; copy dll there
  
  StrCpy $0 "$INSTDIR\Lightweight Limiter.dll" 
  System::Call 'madExcept Patch::PatchMadExceptDLL(t) i (r0).r1'
  StrCpy $0 "$INSTDIR\Lightweight Gate.dll" 
  System::Call 'madExcept Patch::PatchMadExceptDLL(t) i (r0).r1'
  StrCpy $0 "$INSTDIR\Lightweight Compressor.dll" 
  System::Call 'madExcept Patch::PatchMadExceptDLL(t) i (r0).r1'
  StrCpy $0 "$INSTDIR\Lightweight Feedback Limiter.dll" 
  System::Call 'madExcept Patch::PatchMadExceptDLL(t) i (r0).r1'
  StrCpy $0 "$INSTDIR\Lightweight Feedback Compressor.dll" 
  System::Call 'madExcept Patch::PatchMadExceptDLL(t) i (r0).r1'
  StrCpy $0 "$INSTDIR\Lightweight Multiband Compressor.dll" 
  System::Call 'madExcept Patch::PatchMadExceptDLL(t) i (r0).r1'
  StrCpy $0 "$INSTDIR\Lightweight Upward Compressor.dll" 
  System::Call 'madExcept Patch::PatchMadExceptDLL(t) i (r0).r1'
  System::Free 0
  Delete "madExcept Patch.dll"
  
  IntCmp $1 0 SkipDLLCall
  DetailPrint  "Bug Report DLL Patch applied"
SkipDLLCall:

  ;Store installation folder
  WriteRegStr HKLM "SOFTWARE\Delphi ASIO & VST Packages\${PRODUCT_NAME}" "" $INSTDIR
  
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall_Lightweight_Series.exe"
SectionEnd

Section "Manuals" SecManual
  SetOutPath "$INSTDIR"
  
  ;ADD YOUR OWN FILES HERE...
  File "..\Manuals\Lightweight Limiter Manual.pdf"
  File "..\Manuals\Lightweight Gate Manual.pdf"
  File "..\Manuals\Lightweight Compressor Manual.pdf"
  File "..\Manuals\Lightweight Feedback Compressor Manual.pdf"
  File "..\Manuals\Lightweight Multiband Compressor Manual.pdf"

  ;Store installation folder
  WriteRegStr HKLM "SOFTWARE\Delphi ASIO & VST Packages\${PRODUCT_NAME}" "" $INSTDIR
  
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall_Lightweight_Series.exe"
SectionEnd


;--------------------------------
;Installer Functions

Function BugReportPatch
  ${If} ${SectionIsSelected} ${SecVSTPlugin}
  Goto IsVST
  ${EndIf}
  Goto NoVST

  IsVST:
  !insertmacro MUI_HEADER_TEXT "$(TEXT_IO_TITLE)" "$(TEXT_IO_SUBTITLE)"
  !insertmacro MUI_INSTALLOPTIONS_DISPLAY "ioBugReport.ini"

  NoVST:
FunctionEnd


;--------------------------------
;Descriptions

  ;Language strings
  LangString TEXT_IO_TITLE ${LANG_ENGLISH} "InstallOptions page"
  LangString TEXT_IO_SUBTITLE ${LANG_ENGLISH} "Lightweight Series VST Plugin"

  LangString DESC_SecVSTPlugin ${LANG_ENGLISH} "Lightweight Series VST Plugin"
  LangString DESC_SecManual ${LANG_ENGLISH} "Lightweight Series Manual"

  ;Assign language strings to sections
  !insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
    !insertmacro MUI_DESCRIPTION_TEXT ${SecVSTPlugin} $(DESC_SecVSTPlugin)
    !insertmacro MUI_DESCRIPTION_TEXT ${SecManual} $(DESC_SecManual)
  !insertmacro MUI_FUNCTION_DESCRIPTION_END


;--------------------------------
;Uninstaller Section

Section "Uninstall"

  ;ADD YOUR OWN FILES HERE...
  Delete "$INSTDIR\Lightweight Limiter.dll"
  Delete "$INSTDIR\Lightweight Gate.dll"
  Delete "$INSTDIR\Lightweight Compressor.dll"
  Delete "$INSTDIR\Lightweight Feedback Limiter.dll"
  Delete "$INSTDIR\Lightweight Feedback Compressor.dll"
  Delete "$INSTDIR\Lightweight Multiband Compressor.dll"
  Delete "$INSTDIR\Lightweight Limiter Manual.pdf"
  Delete "$INSTDIR\Lightweight Gate Manual.pdf"
  Delete "$INSTDIR\Lightweight Compressor Manual.pdf"
  Delete "$INSTDIR\Lightweight Feedback Compressor Manual.pdf"
  Delete "$INSTDIR\Lightweight Multiband Compressor Manual.pdf"
  DeleteRegKey HKLM "SOFTWARE\Delphi ASIO & VST Packages\${PRODUCT_NAME}"

SectionEnd