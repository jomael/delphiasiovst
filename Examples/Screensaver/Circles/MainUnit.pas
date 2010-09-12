unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DAV_GuiCommon, DAV_GuiPixelMap, DAV_GuiVector, DAV_GuiVectorPixel;

type
  TMainForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    FPixelMap    : TGuiPixelMapMemory;
    FMouseCount  : Integer;
    FOldMousePos : TPoint;
    FCircles     : array of TGuiPixelCircle;
    procedure FormIdle(Sender: TObject; var Done: Boolean);
  public
    procedure Render;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
var
  Index    : Integer;
  IntFloat : TIntFloatRecord;
begin
 SetLength(FCircles, 16);

 FMouseCount := 0;
 FPixelMap := TGuiPixelMapMemory.Create;
 ControlStyle := ControlStyle + [csOpaque];
 Left := 0;
 Top := 0;
 ClientHeight := Screen.Height;
 ClientWidth := Screen.Width;

 IntFloat.Fractal := 0;
 for Index := 0 to Length(FCircles) - 1 do
  begin
   FCircles[Index] := TGuiPixelCircle.Create;
   with FCircles[Index], Primitive do
    begin
     Color := Random($3FFF); //Random($FFFFFF);
     Alpha := 1 + Random($FE);
     IntFloat.Value := -$FF + Random(ClientWidth - 1 + 2 * $FF);
     CenterX := IntFloat;
     IntFloat.Value := -$FF + Random(ClientHeight - 1 + 2 * $FF);
     CenterY := IntFloat;
     IntFloat.Value := 0; //$FF - Alpha;
     Radius := IntFloat;
    end;
  end;

 Application.OnIdle := FormIdle;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FPixelMap);
end;

procedure TMainForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
 Close;
end;

procedure TMainForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
 if (FOldMousePos.X <> Mouse.CursorPos.X) or
    (FOldMousePos.Y <> Mouse.CursorPos.Y) then
  begin
   Inc(FMouseCount);
   FOldMousePos := Mouse.CursorPos;
   if FMouseCount >= 5
    then Close;
  end;
end;

procedure TMainForm.FormPaint(Sender: TObject);
begin
 if Assigned(FPixelMap)
  then FPixelMap.PaintTo(Canvas, 0, 0);
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
 if Assigned(FPixelMap)
  then FPixelMap.SetSize(ClientWidth, ClientHeight);
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  TaskbarHandle: THandle;
begin
 // hide taskbar
 TaskbarHandle := FindWindow('Shell_TrayWnd', nil);
 ShowWindow(TaskbarHandle, SW_HIDE);
end;

procedure TMainForm.Render;
var
  Index    : Integer;
  IntFloat : TIntFloatRecord;
const
  CShade : TPixel32 = (ARGB : $03000000);
begin
 if Random(3) = 0
  then FPixelMap.FillRect(Rect(0, 0, ClientWidth, ClientHeight), CShade);
 IntFloat.Fractal := 0;
 for Index := 0 to Length(FCircles) - 1 do
  with FCircles[Index], Primitive do
   begin
    Alpha := Round(0.94 * Alpha) - 1;
    if Alpha = 0 then
     begin
      IntFloat.Value := -$FF + Random(ClientWidth - 1 + 2 * $FF);
      CenterX := IntFloat;
      IntFloat.Value := -$FF + Random(ClientHeight - 1 + 2 * $FF);
      CenterY := IntFloat;
      IntFloat.Value := 0;
      Radius := IntFloat;
      Color := Random($3FFF);
      Alpha := 1 + Random($FE);
     end
    else
     begin
      IntFloat := Radius;
      IntFloat.Value := Round(Sqrt(Sqr(IntFloat.Value) + 256));
      Radius := IntFloat;
     end;
    DrawFixedPoint(FPixelMap);
   end;
 Invalidate;
end;

procedure TMainForm.FormIdle(Sender: TObject; var Done: Boolean);
begin
 Done := False;
 Render;
 Sleep(50);
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  TaskbarHandle: THandle;
begin
 TaskbarHandle := FindWindow('Shell_TrayWnd', nil);
 ShowWindow(TaskbarHandle, SW_SHOWNORMAL);
end;

end.
