unit RpeqGUI;

interface

uses 
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule;

type
  TFmRpeq = class(TForm)
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

end.
