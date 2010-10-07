unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  DAV_GuiCommon, DAV_GuiPixelMap, DAV_GuiStitchedControls,
  DAV_GuiStitchedSwitch, DAV_GuiStitchedPngList;

type
  TFmSwitchTest = class(TForm)
    GuiStitchedImageList: TGuiStitchedImageList;
    GuiStichedSwitch0: TGuiStichedSwitch;
    GuiStichedSwitch1: TGuiStichedSwitch;
    GuiStichedSwitch2: TGuiStichedSwitch;
    GuiStichedSwitch3: TGuiStichedSwitch;
    GuiStitchedPNGList: TGuiStitchedPNGList;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure GuiStichedSwitchChange(Sender: TObject);
  private
    FBackground : TGuiCustomPixelMap;
  end;

var
  FmSwitchTest: TFmSwitchTest;

implementation

{$R *.dfm}

uses
  DAV_GuiFileFormats, DAV_GuiPng;

procedure TFmSwitchTest.FormCreate(Sender: TObject);
begin
 with GuiStitchedImageList[0], StitchedPixelMap do
  begin
   LoadFromFile('BigStop.png');
   GlyphCount := 2;
  end;
 FBackground := TGuiPixelMapMemory.Create;

 GuiStichedSwitch2.Transparent := True;
end;

procedure TFmSwitchTest.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackground);
end;

procedure TFmSwitchTest.FormPaint(Sender: TObject);
begin
 if Assigned(FBackground)
  then FBackground.PaintTo(Canvas);
end;

procedure TFmSwitchTest.FormResize(Sender: TObject);
var
  x, y   : Integer;
  s      : array[0..1] of Single;
  b      : ShortInt;
  ScnLn  : PPixel32Array;
begin
 if Assigned(FBackground) then
  with FBackground do
   begin
    SetSize(ClientWidth, ClientHeight);
    s[0] := 0;
    s[1] := 0;
    for y := 0 to Height - 1 do
     begin
      ScnLn := Scanline[y];
      for x := 0 to Width - 1 do
       begin
        s[1] := 0.97 * s[0] + 0.03 * (2 * Random - 1);
        b := Round($7F + $3F * s[1]);
        s[0] := s[1];
        ScnLn[x].B := b;
        ScnLn[x].G := b;
        ScnLn[x].R := b;
       end;
     end;
   end;
end;

procedure TFmSwitchTest.GuiStichedSwitchChange(Sender: TObject);
begin
 with Sender as TGuiStichedSwitch do Transparent := GlyphIndex = 1;
end;

end.
