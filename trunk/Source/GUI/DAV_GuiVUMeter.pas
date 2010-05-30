unit DAV_GuiVUMeter;

interface

{$I ..\DAV_Compiler.inc}

uses
  Windows, Classes, Graphics, Forms, Messages, SysUtils, Controls,
  DAV_GuiBaseControl;

type
  TCustomGuiVUMeter = class(TBufferedGraphicControl)
  private
    FAutoSize      : Boolean;
    FVUMeterBitmap : TBitmap;
    FNumGlyphs     : Integer;
    FLastGlyph     : Integer;
    FGlyphIndex    : Integer;
    FStitchKind    : TGuiStitchKind;
    procedure DoAutoSize;
    procedure SetAutoSize(const Value: Boolean); reintroduce;
    procedure SetVUMeterBitmap(const Value: TBitmap);
    procedure SetNumGlyphs(const Value: Integer);
    procedure SetGlyphIndex(Value: Integer);
    procedure SetStitchKind(const Value: TGuiStitchKind);
  protected
    procedure SettingsChanged(Sender: TObject); virtual;
    procedure UpdateBuffer; override;
    procedure AutoSizeChanged; virtual;
    procedure NumGlyphsChanged; virtual;
    procedure GlyphIndexChanged; virtual;
    procedure StitchKindChanged; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Color;

    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    property GlyphIndex: Integer read FGlyphIndex write SetGlyphIndex;
    property NumGlyphs: Integer read FNumGlyphs write SetNumGlyphs default 1;
    property VUMeterBitmap: TBitmap read FVUMeterBitmap write SetVUMeterBitmap;
    property StitchKind: TGuiStitchKind read FStitchKind write SetStitchKind;
  end;

  TGuiVUMeter = class(TCustomGuiVUMeter)
  published
    property AutoSize;
    property Color;
    property NumGlyphs;
    property PopupMenu;
    property GlyphIndex;
    property StitchKind;
    property VUMeterBitmap;
  end;

implementation

//uses Consts;

{ TCustomGuiVUMeter }

constructor TCustomGuiVUMeter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGlyphIndex             := 0;
  FNumGlyphs              := 1;
  FLastGlyph              := -1;
  FStitchKind             := skHorizontal;
  FVUMeterBitmap          := TBitmap.Create;
  FVUMeterBitmap.OnChange := SettingsChanged;
end;

destructor TCustomGuiVUMeter.Destroy;
begin
  FreeAndNil(FVUMeterBitmap);
  inherited;
end;

procedure TCustomGuiVUMeter.DoAutoSize;
begin
 if FVUMeterBitmap.Empty or (FNumGlyphs = 0) then Exit;

 if FStitchKind = skVertical then
  begin
   Width  := FVUMeterBitmap.Width;
   Height := FVUMeterBitmap.Height div FNumGlyphs;
  end
 else
  begin
   Width  := FVUMeterBitmap.Width div FNumGlyphs;
   Height := FVUMeterBitmap.Height;
  end;
end;

procedure TCustomGuiVUMeter.UpdateBuffer;
var
  theRect : TRect;
  GlyphNr : Integer;
begin
 if (Width <= 0) and (Height <= 0) then
  with FBuffer.Canvas do
   begin
    Brush.Color := Self.Color;
    FillRect(ClipRect);
   end
 else
  with FBuffer.Canvas do
   begin
    GlyphNr := FGlyphIndex;
    if (GlyphNr >= FNumGlyphs) then GlyphNr := FNumGlyphs - 1 else
    if (GlyphNr < 0) then GlyphNr := 0;
    if GlyphNr = FLastGlyph then Exit;
    theRect := ClientRect;

    if FStitchKind = skVertical then
     begin
      theRect.Top    := FVUMeterBitmap.Height * GlyphNr div FNumGlyphs;
      theRect.Bottom := FVUMeterBitmap.Height * (GlyphNr + 1) div FNumGlyphs;
     end
    else
     begin
      theRect.Left  := FVUMeterBitmap.Width * GlyphNr div FNumGlyphs;
      theRect.Right := FVUMeterBitmap.Width * (GlyphNr + 1) div FNumGlyphs;
     end;

    Lock;
    CopyRect(Clientrect, FVUMeterBitmap.Canvas, theRect);
    Unlock;
    FLastGlyph := GlyphNr;
   end;
end;

procedure TCustomGuiVUMeter.SetAutoSize(const Value: Boolean);
begin
 if FAutoSize <> Value then
  begin
   FAutoSize := Value;
   AutoSizeChanged;
  end;
end;

procedure TCustomGuiVUMeter.AutoSizeChanged;
begin
 if Autosize then DoAutoSize;
end;

procedure TCustomGuiVUMeter.SetVUMeterBitmap(const Value: TBitmap);
begin
 FVUMeterBitmap.Assign(Value);
 DoAutoSize;
end;

procedure TCustomGuiVUMeter.SetNumGlyphs(const Value: Integer);
begin
 if FNumGlyphs <> Value then
  begin
   FNumGlyphs := Value;
   NumGlyphsChanged;
  end;
end;

procedure TCustomGuiVUMeter.NumGlyphsChanged;
begin
 FLastGlyph := -1;
 DoAutoSize;
end;

procedure TCustomGuiVUMeter.SetGlyphIndex(Value: Integer);
begin
 if Value < 0 then Value := 0 else
 if Value > FNumGlyphs then Value := FNumGlyphs;

 if FGlyphIndex <> Value then
  begin
   FGlyphIndex := Value;
   GlyphIndexChanged;
  end;
end;

procedure TCustomGuiVUMeter.GlyphIndexChanged;
begin
 Invalidate;
end;

procedure TCustomGuiVUMeter.SetStitchKind(const Value: TGuiStitchKind);
begin
 if FStitchKind <> Value then
  begin
   FStitchKind := Value;
   StitchKindChanged;
  end;
end;

procedure TCustomGuiVUMeter.StitchKindChanged;
begin
 FLastGlyph := -1;
 DoAutoSize;
end;

procedure TCustomGuiVUMeter.SettingsChanged(Sender: TObject);
begin
 FVUMeterBitmap.Canvas.Brush.Color := Self.Color;
 Invalidate;
 FLastGlyph := -1;
end;

end.
