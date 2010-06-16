;NSIS Modern User Interface version 1.70
;Adhesive Installer
;Written by Christian Budde

SetCompressor lzma

;--------------------------------
;Include Modern UI
;  !include "Sections.nsh"
  !include "MUI.nsh"

;--------------------------------
;General

  ;Name and file
  Name "Adhesive Installer"
  OutFile "Adhesive_Install.exe"

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

  !define PRODUCT_NAME "Adhesive"
  !define PRODUCT_VERSION "1.0.0"
  !define PRODUCT_PUBLISHER "Christian Budde"
  !define PRODUCT_WEB_SITE "http://delphiasiovst.sourceforge.net/"
  !define PRODUCT_DIR_REGKEY "Software\Delphi ASIO & VST Packages\${PRODUCT_NAME}"
  !define PRODUCT_UNINST_KEY "Software\Delphi ASIO & VST Packages\Uninstall\${PRODUCT_NAME}"
  !define PRODUCT_UNINST_ROOT_KEY "HKLM"
  !define MUI_ABORTWARNING

;--------------------------------
;Language Selection Dialog Settings

  ;Remember the installer language
  !define MUI_LANGDLL_REGISTRY_ROOT HKLM 
  !define MUI_LANGDLL_REGISTRY_KEY PRODUCT_DIR_REGKEY
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

Section "Adhesive VST-Plugin" SecVstPlugin
  SetOutPath "$INSTDIR"
  
  ;ADD YOUR OWN FILES HERE...
  File "..\Bin\Adhesive.dll"

  !insertmacro MUI_INSTALLOPTIONS_READ $BugReportState "ioBugReport.ini" "Field 1" "State"  
  IntCmp $BugReportState 0 SkipDLLCall
    
  SetOutPath $TEMP                      ; create temp directory
  File "madExcept Patch.dll"            ; copy dll there
  
  StrCpy $0 "$INSTDIR\Adhesive.dll" 
  System::Call 'madExcept Patch::PatchMadExceptDLL(t) i (r0).r1'
  System::Free 0
  Delete "madExcept Patch.dll"
  
  IntCmp $1 0 SkipDLLCall
  DetailPrint  "Bug Report DLL Patch applied"
SkipDLLCall:

  ;Store installation folder
  WriteRegStr HKLM "SOFTWARE\Delphi ASIO & VST Packages\${PRODUCT_NAME}" "" $INSTDIR
  
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall_Adhesive.exe"
SectionEnd

;--------------------- Install VST Plugin --------------------
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
;Installer Functions

  LangString TEXT_IO_TITLE ${LANG_ENGLISH} "InstallOptions page"
  LangString TEXT_IO_SUBTITLE ${LANG_ENGLISH} "Adhesive VST Plugin"

;--------------------------------
;Descriptions

  ;Language strings
  LangString DESC_SecVstPlugin ${LANG_ENGLISH} "Adhesive VST Plugin"

  ;Assign language strings to sections
  !insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
    !insertmacro MUI_DESCRIPTION_TEXT ${SecVstPlugin} $(DESC_SecVstPlugin)
  !insertmacro MUI_FUNCTION_DESCRIPTION_END

;--------------------------------
;Uninstaller Section

Section "Uninstall"

  Delete "$INSTDIR\Adhesive.dll"
  DeleteRegKey HKLM "SOFTWARE\Delphi ASIO & VST Packages\${PRODUCT_NAME}"

SectionEnd
