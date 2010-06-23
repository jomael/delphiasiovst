;NSIS Modern User Interface version 1.70
;SEM Module Screenshot Tool Installer
;Written by Christian Budde

SetCompressor lzma

;--------------------------------
;Include Modern UI
;  !include "Sections.nsh"
  !include "MUI.nsh"


;--------------------------------
;General

  ;Name and file
  Name "SEM Screenshot Tool Installer"
  OutFile "SEM_Screenshot_Tool_Install.exe"

  ;Default installation folder
  InstallDir "$PROGRAMFILES\SEM Module Screenshot Tool"
  
  BrandingText "Delphi ASIO && SEM Project"

  ; Turn on the xp style of drawing
  XPStyle ON


;--------------------------------
;Interface Settings

  !define PRODUCT_NAME "SEM Module Screenshot Tool"
  !define PRODUCT_VERSION "1.0.0"
  !define PRODUCT_PUBLISHER "Christian Budde"
  !define PRODUCT_WEB_SITE "http://delphiasioSEM.sourceforge.net/"
  !define PRODUCT_DIR_REGKEY "Software\Delphi ASIO & SEM Packages\${PRODUCT_NAME}"
  !define PRODUCT_UNINST_KEY "Software\Delphi ASIO & SEM Packages\Uninstall\${PRODUCT_NAME}"
  !define PRODUCT_UNINST_ROOT_KEY "HKLM"
  !define MUI_ABORTWARNING


;--------------------------------
;Language Selection Dialog Settings

  ;Remember the installer language
  !define MUI_LANGDLL_REGISTRY_ROOT "HKLM" 
  !define MUI_LANGDLL_REGISTRY_KEY PRODUCT_DIR_REGKEY
  !define MUI_LANGDLL_REGISTRY_VALUENAME "Installer Language"


;--------------------------------
;Pages

  !insertmacro MUI_PAGE_WELCOME
  !insertmacro MUI_PAGE_LICENSE "..\Bin\License.txt"
  !insertmacro MUI_PAGE_COMPONENTS
  !insertmacro MUI_PAGE_DIRECTORY
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

Section "Program Files" SecProgramFiles
  SetOutPath "$INSTDIR"
  
  !system 'copy "..\Bin\SemScreenshotTool.exe" "..\Bin\SE Module Screenshot Tool.exe"'  

  ;ADD YOUR OWN FILES HERE...
  File "..\Bin\SE Module Screenshot Tool.exe"

  ;Store installation folder
  WriteRegStr HKLM PRODUCT_DIR_REGKEY "" $INSTDIR
  
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall_SEM_Screenshot_Tool.exe"
SectionEnd

Section "Manual" SecManual
  SetOutPath "$INSTDIR"
  
  ;ADD YOUR OWN FILES HERE...
  File "..\Bin\SE Module Screenshot Tool.pdf"

  ;Store installation folder
  WriteRegStr HKLM PRODUCT_DIR_REGKEY "" $INSTDIR
  
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall_SEM_Screenshot_Tool.exe"
SectionEnd


;--------------------------------
;Descriptions

  ;Language strings
  LangString TEXT_IO_TITLE ${LANG_ENGLISH} "InstallOptions page"
  LangString TEXT_IO_SUBTITLE ${LANG_ENGLISH} "SEM Module Screenshot Tool Program Files"

  LangString DESC_SecProgramFiles ${LANG_ENGLISH} "SEM Module Screenshot Tool Program Files"
  LangString DESC_SecManual ${LANG_ENGLISH} "SEM Module Screenshot Tool Manual"

  ;Assign language strings to sections
  !insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
    !insertmacro MUI_DESCRIPTION_TEXT ${SecProgramFiles} $(DESC_SecProgramFiles)
    !insertmacro MUI_DESCRIPTION_TEXT ${SecManual} $(DESC_SecManual)
  !insertmacro MUI_FUNCTION_DESCRIPTION_END


;--------------------------------
;Uninstaller Section

Section "Uninstall"

  ;ADD YOUR OWN FILES HERE...
  Delete "$INSTDIR\SE Module Screenshot Tool.exe"
  Delete "$INSTDIR\SE Module Screenshot Tool.pdf"
  DeleteRegKey HKLM PRODUCT_DIR_REGKEY

SectionEnd