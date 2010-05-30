unit VSmain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, DAV_Types, DAV_VSTHost, DAV_Sonogram,
  DAV_DspWindowFunctions, DAV_DspWindowFunctionsAdvanced,
  DAV_DspSweepOscillator;

type
  TFmSonogram = class(TForm)
    EdVSTPlugin: TEdit;
    LbVstPlugin: TLabel;
    BtSelect: TButton;
    PbSonogram: TPaintBox;
    OD: TOpenDialog;
    VstHost: TVstHost;
    procedure BtSelectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure EdVSTPluginChange(Sender: TObject);
    procedure PbSonogramPaint(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FIniFileName : TFileName;
    FSonogram    : TBitmapSonogram32;
    FSweepOsc    : TRangeSweepOscillator64;
  public
  end;

var
  FmSonogram: TFmSonogram;

implementation

uses
  IniFiles;

{$R *.dfm}

procedure TFmSonogram.FormCreate(Sender: TObject);
begin
 FIniFileName := ExtractFilePath(ParamStr(0)) + 'V2S.ini';
 FSonogram := TBitmapSonogram32.Create;
 with FSonogram do
  begin
   Bitmap.Width := PbSonogram.Width;
   Bitmap.Height := PbSonogram.Height;
   FFTOrder := 12;
   OverlapFactor := 16;
   WindowClass := TWindowFunctionLawrey6T;
  end;

 FSweepOsc := TRangeSweepOscillator64.Create;
 with FSweepOsc do
  begin
   Amplitude := 1;
   StartFrequency := FSonogram.LowerFrequency;
   StopFrequency := FSonogram.UpperFrequency;
  end;

 OD.InitialDir := VstHost.PlugInDir;
end;

procedure TFmSonogram.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FSonogram);
 FreeAndNil(FSweepOsc);
end;

procedure TFmSonogram.FormShow(Sender: TObject);
begin
 with TIniFile.Create(FIniFileName) do
  try
   Left := ReadInteger('Layout', 'Left', Left);
   Top := ReadInteger('Layout', 'Top', Top);
   EdVSTPlugin.Text := ReadString('Recent', 'Last Plugin', EdVSTPlugin.Text);
  finally
   Free;
  end;
end;

procedure TFmSonogram.PbSonogramPaint(Sender: TObject);
begin
 PbSonogram.Canvas.Draw(0, 0, FSonogram.Bitmap);
end;

procedure TFmSonogram.EdVSTPluginChange(Sender: TObject);
var
  Input   : TDAVArrayOfSingleFixedArray;
  Output  : TDAVArrayOfSingleFixedArray;
  Channel : Integer;
  Sample  : Integer;
const
  CFixedSize = 1 shl 16;
begin
 try
  VstHost.BlockSize := CFixedSize;
  VstHost[0].LoadFromFile(EdVSTPlugin.Text);
  if (VstHost[0].numInputs = 0) or (VstHost[0].numOutputs = 0)
   then raise Exception.Create('No input or output');

  SetLength(Input, VstHost[0].numInputs);
  SetLength(Output, VstHost[0].numOutputs);
  FSweepOsc.ModulationFrequency := 22050 / CFixedSize;

  // Reset
  FSweepOsc.Reset;
  FSonogram.Reset;

  for Channel := 0 to Length(Input) - 1 do
   begin
    GetMem(Input[Channel], CFixedSize * SizeOf(Single));
    FillChar(Input[Channel]^, CFixedSize * SizeOf(Single), 0);
    for Sample := 0 to CFixedSize - 1 do
     begin
      Input[Channel]^[Sample] := FSweepOsc.Sine;
      FSweepOsc.CalculateNextSample;
     end;
   end;
  for Channel := 0 to Length(Output) - 1 do
   begin
    GetMem(Output[Channel], CFixedSize * SizeOf(Single));
    FillChar(Output[Channel]^, CFixedSize * SizeOf(Single), 0);
   end;

  try
   VstHost[0].Active := True;
   VstHost[0].ProcessReplacing(@Input[0], @Output[0], CFixedSize);

   FSonogram.ProcessBlock32(Output[0], CFixedSize);
  finally
   for Channel := 0 to Length(Input) - 1 do Dispose(Input[Channel]);
   for Channel := 0 to Length(Output) - 1 do Dispose(Output[Channel]);
  end;
  PbSonogram.Invalidate;
 except
  MessageDlg('Plugin could not be loaded!', mtError, [mbOK], 0);
 end;
end;

procedure TFmSonogram.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 with TIniFile.Create(FIniFileName) do
  try
   WriteInteger('Layout', 'Left', Left);
   WriteInteger('Layout', 'Top', Top);
   WriteString('Recent', 'Last Plugin', EdVSTPlugin.Text);
  finally
   Free;
  end;
end;

procedure TFmSonogram.BtSelectClick(Sender: TObject);
begin
 with OD do
  begin
   with TIniFile.Create(FIniFileName) do
    try
     InitialDir := ReadString('Recent', 'Plugin Directory', InitialDir);
     if Execute then
      begin
       EdVSTPlugin.Text := OD.FileName;
       WriteString('Recent', 'Plugin Directory', ExtractFilePath(OD.FileName));
      end;
    finally
     Free;
    end;
  end;
end;

end.
