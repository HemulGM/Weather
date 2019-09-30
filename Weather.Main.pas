unit Weather.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  System.Net.HttpClient, Vcl.StdCtrls, IPPeerClient, REST.Client,
  Data.Bind.Components, Data.Bind.ObjectScope, Weather.Classes, System.JSON,
  Vcl.ExtCtrls, Vcl.Imaging.pngimage, System.ImageList, Vcl.ImgList, HGM.Button,
  Direct2D, D2D1, System.Generics.Collections, Vcl.WinXCtrls,
  HGM.Controls.Labels, HGM.Controls.Labels.Base;

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
    ButtonFlatSettings: TButtonFlat;
    ImageList32: TImageList;
    ButtonFlatUpdate: TButtonFlat;
    ImageList24: TImageList;
    PanelSettings: TPanel;
    EditCity: TEdit;
    Label1: TLabel;
    ButtonFlatSetCancel: TButtonFlat;
    ButtonFlatSetOk: TButtonFlat;
    TimerUpdate: TTimer;
    LabelError: TLabel;
    hLabel1: ThLabel;
    procedure FormCreate(Sender: TObject);
    procedure ButtonFlatUpdateClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ButtonFlatSetCancelClick(Sender: TObject);
    procedure ButtonFlatSetOkClick(Sender: TObject);
    procedure ButtonFlatSettingsClick(Sender: TObject);
    procedure TimerUpdateTimer(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    FWeather: TWeather;
    FCity: string;
    FRotate: Integer;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public declarations }
  end;

var
  FormWeather: TFormWeather;

implementation

uses
  HGM.Common.Utils;

{$R *.dfm}

function DegToStr(Value: Integer): string;
begin
  if Between(0, Value, 20) then
    Exit('С');
  if Between(20, Value, 75) then
    Exit('СВ');
  if Between(75, Value, 105) then
    Exit('В');
  if Between(105, Value, 165) then
    Exit('ЮВ');
  if Between(165, Value, 195) then
    Exit('Ю');
  if Between(195, Value, 255) then
    Exit('ЮЗ');
  if Between(255, Value, 285) then
    Exit('З');
  if Between(285, Value, 345) then
    Exit('СЗ');
  if Between(345, Value, 360) then
    Exit('С');
end;

procedure TFormWeather.ButtonFlatSetCancelClick(Sender: TObject);
begin
  PanelSettings.Hide;
end;

procedure TFormWeather.ButtonFlatSetOkClick(Sender: TObject);
begin
  FCity := EditCity.Text;
  PanelSettings.Hide;
  ButtonFlatUpdateClick(nil);
end;

procedure TFormWeather.ButtonFlatSettingsClick(Sender: TObject);
begin
  EditCity.Text := FCity;
  PanelSettings.Show;
  PanelSettings.BringToFront;
end;

procedure TFormWeather.ButtonFlatUpdateClick(Sender: TObject);
var
  Mem: TMemoryStream;
  PNG: TPNGImage;
  temp: Integer;
begin
  RESTRequest.Params.ParameterByName('q').Value := FCity;
  RESTRequest.Execute;
  if RESTResponse.StatusCode <> 200 then
  begin
    LabelError.Show;
    ButtonFlatSettingsClick(nil);
    Exit;
  end;
  LabelError.Hide;
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

  LabelLoc.Caption := FWeather.name;
  temp := Round(FWeather.main.temp - 273.15);
  LabelTemp.Caption := temp.ToString + '°';
  if temp > 0 then
    LabelTemp.Caption := '+' + LabelTemp.Caption;
  LabelPressure.Caption := Round(FWeather.main.pressure * 0.750062).ToString + ' мм рт.ст.';
  LabelHumidity.Caption := FWeather.main.humidity.ToString + '%';
  LabelWind.Caption := FWeather.wind.speed.ToString + ' м/с ' + DegToStr(Round(FWeather.wind.deg));
  LabelCloudiness.Caption := FWeather.clouds.all.ToString + '%';
  FRotate := Round(360 - FWeather.wind.deg);
end;

procedure TFormWeather.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or WS_BORDER or WS_THICKFRAME;
end;

procedure TFormWeather.FormActivate(Sender: TObject);
begin
  ButtonFlatSettings.Visible := True;
  ButtonFlatUpdate.Visible := True;
end;

procedure TFormWeather.FormCreate(Sender: TObject);
begin
  FWeather := TWeather.Create;
  FRotate := 0;
  FCity := 'Междуреченск';
  PanelSettings.Hide;
  PanelSettings.BoundsRect := ClientRect;
  ButtonFlatUpdateClick(nil);
  Left := Screen.Width - ClientWidth;
  Top := 0;
end;

procedure TFormWeather.FormDeactivate(Sender: TObject);
begin
  ButtonFlatSettings.Visible := False;
  ButtonFlatUpdate.Visible := False;
end;

procedure TFormWeather.FormDestroy(Sender: TObject);
begin
  FWeather.Free;
end;

procedure TFormWeather.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ReleaseCapture;
  SendMessage(Handle, WM_SYSCOMMAND, 61458, 0);
end;

procedure TFormWeather.FormPaint(Sender: TObject);
var
  D2: TDirect2DCanvas;
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
    ImageList24.GetIcon(7, ICO);
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
    Brush.Color := $008A8AFF;
    Pen.Color := $008A8AFF;
    R := TRect.Create(TPoint.Create(10, 80), 30, 30);
    R.Inflate(-5, -5);
    Ellipse(R);
    //
    R := TRect.Create(TPoint.Create(10, 80), 30, 30);
    //
    Pen.Width := 1;
    R := TRect.Create(TPoint.Create(10, 80), 30, 30);
    R.Inflate(-10, 0);
    //R.Top := 20;
    //R.Top := 80;
    R.Top := 50;
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

procedure TFormWeather.TimerUpdateTimer(Sender: TObject);
begin
  if PanelSettings.Visible then
    Exit;
  ButtonFlatUpdateClick(nil);
end;

end.

