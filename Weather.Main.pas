unit Weather.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  System.Net.HttpClient, Vcl.StdCtrls, IPPeerClient, REST.Client,
  Data.Bind.Components, Data.Bind.ObjectScope, Weather.Classes, System.JSON,
  Vcl.ExtCtrls, Vcl.Imaging.pngimage, System.ImageList, Vcl.ImgList, HGM.Button,
  Direct2D, D2D1, acPNG;

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
    ButtonFlat1: TButtonFlat;
    ButtonFlat2: TButtonFlat;
    ButtonFlat3: TButtonFlat;
    ButtonFlat4: TButtonFlat;
    PanelSettings: TPanel;
    EditCity: TEdit;
    Label1: TLabel;
    ButtonFlatSetCancel: TButtonFlat;
    ButtonFlatSetOk: TButtonFlat;
    TimerUpdate: TTimer;
    LabelError: TLabel;
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
  private
    FWeather: TWeather;
    FCity: string;
    FRotate: Integer;
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

procedure TFormWeather.FormPaint(Sender: TObject);
var
  D2: TDirect2DCanvas;
  ICO: TIcon;
  Pt: TPoint;
begin
  with TDirect2DCanvas.Create(Canvas, ClientRect) do
  begin
    BeginDraw;
    Pt := Point(LabelWind.Left + LabelWind.Width, LabelWind.Top - 8);
    RenderTarget.SetTransform(TD2D1Matrix3x2F.Rotation(90 - FRotate, Pt.X + 12, Pt.Y + 12));
    ICO := TIcon.Create;
    ImageList24.GetIcon(4, ICO);
    Draw(Pt.X, Pt.Y, ICO);
    ICO.Free;
    RenderTarget.SetTransform(TD2DMatrix3x2F.Identity);
    EndDraw;
    Free;
  end;
end;

procedure TFormWeather.FormResize(Sender: TObject);
begin
  Repaint;
end;

procedure TFormWeather.TimerUpdateTimer(Sender: TObject);
begin
  if PanelSettings.Visible then Exit;
  ButtonFlatUpdateClick(nil);
end;

end.

