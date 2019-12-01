unit Weather.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  System.Net.HttpClient, Vcl.StdCtrls, IPPeerClient, REST.Client,
  Data.Bind.Components, Data.Bind.ObjectScope, Weather.Classes, System.JSON,
  Vcl.ExtCtrls, Vcl.Imaging.pngimage, System.ImageList, Vcl.ImgList, HGM.Button,
  Direct2D, D2D1, System.Generics.Collections, HGM.Controls.Labels,
  HGM.Controls.Labels.Base, Vcl.Menus, HGM.Common.Settings, System.Types,
  Vcl.WinXCtrls;

type
  TFormWeather = class(TForm)
    LabelTemp: TLabel;
    LabelLoc: TLabel;
    RESTClient: TRESTClient;
    RESTRequest: TRESTRequest;
    RESTResponse: TRESTResponse;
    LabelPressure: TLabel;
    LabelHumidity: TLabel;
    LabelWind: TLabel;
    LabelCloudiness: TLabel;
    ImageIcon: TImage;
    ImageList24: TImageList;
    TimerUpdate: TTimer;
    PopupMenu: TPopupMenu;
    MenuItemFix: TMenuItem;
    MenuItemSettings: TMenuItem;
    MenuItemUpdate: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    TrayIcon: TTrayIcon;
    MenuItemQuit: TMenuItem;
    TimerTesting: TTimer;
    ActivityIndicator: TActivityIndicator;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure TimerUpdateTimer(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure RESTRequestAfterExecute(Sender: TCustomRESTRequest);
    procedure MenuItemFixClick(Sender: TObject);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure MenuItemQuitClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItemSettingsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TimerTestingTimer(Sender: TObject);
    procedure MenuItemUpdateClick(Sender: TObject);
  private
    FWeather: TWeather;
    FSettings: TSettingsReg;
    FCity: string;
    FRotate: Integer;
    FTemp: Integer;
    FFixIt: Boolean;
    FColor: TColor;
    procedure SetFixIt(const Value: Boolean);
    procedure SetFontColor(AColor: TColor);
    property FixIt: Boolean read FFixIt write SetFixIt;
  public
    FLocateError: Boolean;
    FCheckIt: Boolean;
    procedure SetThemeColor(AColor: TColor);
    procedure UpdateWeather;
  end;

var
  FormWeather: TFormWeather;

implementation

uses
  HGM.Common.Utils, Math, Weather.Settings;

{$R *.dfm}

function DegToStr(Value: Integer): string;
begin
  if Between(0, Value, 20) then
    Exit('Ñ');
  if Between(20, Value, 75) then
    Exit('ÑÂ');
  if Between(75, Value, 105) then
    Exit('Â');
  if Between(105, Value, 165) then
    Exit('ÞÂ');
  if Between(165, Value, 195) then
    Exit('Þ');
  if Between(195, Value, 255) then
    Exit('ÞÇ');
  if Between(255, Value, 285) then
    Exit('Ç');
  if Between(285, Value, 345) then
    Exit('ÑÇ');
  if Between(345, Value, 360) then
    Exit('Ñ');
end;

procedure TFormWeather.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT;
end;

procedure TFormWeather.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FSettings.SetBool('HGMWeather', 'Fixed', FixIt);
  FSettings.SetParamWindow('HGMWeather', Self, [wpsCoord]);
  FSettings.SetInt('HGMWeather', 'Color', FColor);
end;

procedure TFormWeather.SetFontColor(AColor: TColor);
begin
  LabelTemp.Font.Color := AColor;
  LabelLoc.Font.Color := AColor;
  LabelPressure.Font.Color := AColor;
  LabelHumidity.Font.Color := AColor;
  LabelWind.Font.Color := AColor;
  LabelCloudiness.Font.Color := AColor;
end;

procedure TFormWeather.SetThemeColor(AColor: TColor);
var
  i: Integer;
begin
  for i := 0 to ImageList24.Count - 1 do
    ColorImages(ImageList24, i, AColor);
  SetFontColor(AColor);
end;

procedure TFormWeather.FormCreate(Sender: TObject);
begin
  FSettings := TSettingsReg.Create(rrHKCU, 'Software\');
  FWeather := TWeather.Create;
  FLocateError := False;
  FCheckIt := False;
  FRotate := 0;
  Left := Screen.Width - (ClientWidth + 100);
  Top := 100;
  FixIt := FSettings.GetBool('HGMWeather', 'Fixed', False);
  FCity := FSettings.GetStr('HGMWeather', 'Locate', 'Ìîñêâà');
  FColor := FSettings.GetInt('HGMWeather', 'Color', clWhite);
  FSettings.GetParamWindow('HGMWeather', Self, [wpsCoord]);
  SetThemeColor(FColor);
  ActivityIndicator.Animate := True;
  ActivityIndicator.IndicatorSize := aisXLarge;
  ActivityIndicator.IndicatorColor := aicWhite;
  UpdateWeather;
  TimerUpdate.Enabled := not FLocateError;
end;

procedure TFormWeather.FormDestroy(Sender: TObject);
begin
  FSettings.Free;
  FWeather.Free;
end;

procedure TFormWeather.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (not FixIt) then
  begin
    ReleaseCapture;
    SendMessage(Handle, WM_SYSCOMMAND, 61458, 0);
  end;
  if Button = mbRight then
  begin
    PopupMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  end;
end;

procedure TFormWeather.FormPaint(Sender: TObject);
var
  ICO: TIcon;
  Pt: TPoint;
  R: TRect;
begin
  with TDirect2DCanvas.Create(Canvas, ClientRect) do
  begin
    BeginDraw;
    Pt := Point(LabelWind.Left + LabelWind.Width, LabelWind.Top - 6);
    RenderTarget.SetTransform(TD2D1Matrix3x2F.Rotation(180 + FRotate, Pt.X + 12, Pt.Y + 12));
    ICO := TIcon.Create;
    ImageList24.GetIcon(4, ICO);
    Draw(Pt.X, Pt.Y, ICO);
    ICO.Free;
    RenderTarget.SetTransform(TD2DMatrix3x2F.Identity);

    Brush.Color := Color;
    Pen.Color := clGray;
    Pen.Width := 3;
    //
    R := TRect.Create(TPoint.Create(10, 80), 30, 30);
    Ellipse(R);
    //
    R.Inflate(-5, 0);
    R.Top := 10;
    R.Bottom := 100;
    RoundRect(R, 20, 20);
    //
    Pen.Color := Color;
    R := TRect.Create(TPoint.Create(10, 80), 30, 30);
    R.Inflate(-3, -3);
    Ellipse(R);
    //
    Brush.Color := ColorRedOrBlue(Round(100 * ((Min(Max(-50, FTemp), 50) + 50) / 100))); //$008A8AFF;
    Pen.Color := Brush.Color;
    R := TRect.Create(TPoint.Create(10, 80), 30, 30);
    R.Inflate(-5, -5);
    Ellipse(R);
    //
    Pen.Width := 1;
    R := TRect.Create(TPoint.Create(10, 80), 30, 30);
    R.Inflate(-10, 0);
    //R.Top := 20;
    //R.Top := 90; 70
    R.Left := R.Left - 1;
    R.Top := 90 - Round(70 * ((Min(Max(-50, FTemp), 50) + 50) / 100));
    R.Bottom := 100;
    R.Width := R.Width + 1;
    Rectangle(R);
    EndDraw;
    Free;
  end;                    //6, 30
  ImageList24.Draw(Canvas, LabelWind.Left - 30, LabelWind.Top - 6, 1, True);
  ImageList24.Draw(Canvas, LabelPressure.Left - 30, LabelPressure.Top - 6, 2, True);
  ImageList24.Draw(Canvas, LabelHumidity.Left - 30, LabelHumidity.Top - 6, 0, True);
  ImageList24.Draw(Canvas, LabelCloudiness.Left - 30, LabelCloudiness.Top - 6, 3, True);
end;

procedure TFormWeather.FormResize(Sender: TObject);
begin
  Repaint;
end;

procedure TFormWeather.FormShow(Sender: TObject);
begin
  ShowWindow(Application.Handle, SW_HIDE);
end;

procedure TFormWeather.MenuItemFixClick(Sender: TObject);
begin
  FixIt := not FixIt;
end;

procedure TFormWeather.MenuItemQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormWeather.MenuItemSettingsClick(Sender: TObject);
begin
  if TFormSettings.Execute(FCity, FColor) then
  begin
    FSettings.SetStr('HGMWeather', 'Locate', FCity);
    FSettings.SetInt('HGMWeather', 'Color', FColor);
  end;
  SetThemeColor(FColor);
  UpdateWeather;
  Repaint;
end;

procedure TFormWeather.MenuItemUpdateClick(Sender: TObject);
begin
  ActivityIndicator.Animate := True;
  ActivityIndicator.Visible := True;
  LabelTemp.Hide;
  Repaint;
  UpdateWeather;
end;

procedure TFormWeather.RESTRequestAfterExecute(Sender: TCustomRESTRequest);
var
  Mem: TMemoryStream;
  PNG: TPNGImage;
begin
  if RESTResponse.StatusCode <> 200 then
  begin
    FLocateError := True;
    Exit;
  end;
  FLocateError := False;
  if FCheckIt then
    Exit;
  FWeather.ParseFromJson(TJSONObject(RESTRequest.Response.JSONValue));
  Mem := DownloadURL('https://openweathermap.org/img/wn/' + FWeather.weather[0].icon + '@2x.png');
  if Mem.Size > 0 then
  begin
    PNG := TPngImage.Create;
    try
      PNG.LoadFromStream(Mem);
      ImageIcon.Picture.Assign(PNG);
    finally
      PNG.Free;
    end;
  end
  else
    ImageIcon.Picture.Assign(nil);
  Mem.Free;

  LabelLoc.Caption := FCity; //FWeather.name;
  FTemp := Round(FWeather.main.temp - 273.15);
  LabelTemp.Caption := FTemp.ToString + '°';
  if FTemp > 0 then
    LabelTemp.Caption := '+' + LabelTemp.Caption;
  LabelPressure.Caption := Round(FWeather.main.pressure * 0.750062).ToString + ' ìì ðò.ñò.';
  LabelHumidity.Caption := FWeather.main.humidity.ToString + '%';
  LabelCloudiness.Caption := FWeather.clouds.all.ToString + '%';
  FRotate := Round(FWeather.wind.deg);
  LabelWind.Caption := FWeather.wind.speed.ToString + ' ì/ñ ' + DegToStr(FRotate);
  ActivityIndicator.Animate := False;
  ActivityIndicator.Visible := False;
  LabelTemp.Visible := True;
  Repaint;
end;

procedure TFormWeather.SetFixIt(const Value: Boolean);
begin
  FFixIt := Value;
  AlphaBlend := Value;
  MenuItemFix.Checked := Value;
end;

procedure TFormWeather.TimerTestingTimer(Sender: TObject);
begin

  FTemp := FTemp + 1;
  if FTemp >= 60 then
    FTemp := -60;

  LabelTemp.Caption := FTemp.ToString + '°';
  if FTemp > 0 then
    LabelTemp.Caption := '+' + LabelTemp.Caption;

  FRotate := FRotate + 10;
  if FRotate >= 360 then
    FRotate := 0;
  LabelWind.Caption := FWeather.wind.speed.ToString + ' ì/ñ ' + DegToStr(FRotate);

  Repaint;
end;

procedure TFormWeather.TimerUpdateTimer(Sender: TObject);
begin
  UpdateWeather;
end;

procedure TFormWeather.UpdateWeather;
begin
  RESTRequest.Params.ParameterByName('q').Value := FCity;
  RESTRequest.ExecuteAsync;
end;

end.

