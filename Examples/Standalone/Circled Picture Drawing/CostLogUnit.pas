unit CostLogUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFmCostLog = class(TForm)
    LogCost: TMemo;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FmCostLog: TFmCostLog;

implementation

{$R *.dfm}

end.

