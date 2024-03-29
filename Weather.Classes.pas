unit Weather.Classes;

interface

uses
  Generics.Collections, Rest.Json, System.JSON;

type
  TSysClass = class
  private
    FCountry: string;
    FMessage: Extended;
    FSunrise: Extended;
    FSunset: Extended;
  public
    property country: string read FCountry write FCountry;
    property message: Extended read FMessage write FMessage;
    property sunrise: Extended read FSunrise write FSunrise;
    property sunset: Extended read FSunset write FSunset;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TSysClass;
  end;

  TCloudsClass = class
  private
    FAll: Extended;
  public
    property all: Extended read FAll write FAll;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TCloudsClass;
  end;

  TWindClass = class
  private
    FDeg: Extended;
    FSpeed: Extended;
  public
    property deg: Extended read FDeg write FDeg;
    property speed: Extended read FSpeed write FSpeed;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TWindClass;
  end;

  TWeatherData = class
  private
    FGrnd_level: Extended;
    FHumidity: Extended;
    FPressure: Extended;
    FSea_level: Extended;
    FTemp: Extended;
    FTemp_max: Extended;
    FTemp_min: Extended;
  public
    property grnd_level: Extended read FGrnd_level write FGrnd_level;
    property humidity: Extended read FHumidity write FHumidity;
    property pressure: Extended read FPressure write FPressure;
    property sea_level: Extended read FSea_level write FSea_level;
    property temp: Extended read FTemp write FTemp;
    property temp_max: Extended read FTemp_max write FTemp_max;
    property temp_min: Extended read FTemp_min write FTemp_min;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TWeatherData;
  end;

  TWeatherItem = class
  private
    FDescription: string;
    FIcon: string;
    FId: Extended;
    FMain: string;
  public
    property description: string read FDescription write FDescription;
    property icon: string read FIcon write FIcon;
    property id: Extended read FId write FId;
    property main: string read FMain write FMain;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TWeatherItem;
  end;

  TGeoCoords = class
  private
    FLat: Extended;
    FLon: Extended;
  public
    property lat: Extended read FLat write FLat;
    property lon: Extended read FLon write FLon;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TGeoCoords;
  end;

  TWeather = class
  private
    FBase: string;
    FClouds: TCloudsClass;
    FCod: Extended;
    FCoord: TGeoCoords;
    FDt: Extended;
    FId: Extended;
    FMain: TWeatherData;
    FName: string;
    FSys: TSysClass;
    FTimezone: Extended;
    FWeather: TArray<TWeatherItem>;
    FWind: TWindClass;
  public
    property base: string read FBase write FBase;
    property clouds: TCloudsClass read FClouds write FClouds;
    property cod: Extended read FCod write FCod;
    property coord: TGeoCoords read FCoord write FCoord;
    property dt: Extended read FDt write FDt;
    property id: Extended read FId write FId;
    property main: TWeatherData read FMain write FMain;
    property name: string read FName write FName;
    property sys: TSysClass read FSys write FSys;
    property timezone: Extended read FTimezone write FTimezone;
    property weather: TArray<TWeatherItem> read FWeather write FWeather;
    property wind: TWindClass read FWind write FWind;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TWeather;
    procedure ParseFromJson(AJsonObject: TJSOnObject);
  end;

implementation

{TSysClass}

function TSysClass.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TSysClass.FromJsonString(AJsonString: string): TSysClass;
begin
  result := TJson.JsonToObject<TSysClass>(AJsonString)
end;

{TCloudsClass}

function TCloudsClass.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TCloudsClass.FromJsonString(AJsonString: string): TCloudsClass;
begin
  result := TJson.JsonToObject<TCloudsClass>(AJsonString)
end;

{TWindClass}

function TWindClass.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TWindClass.FromJsonString(AJsonString: string): TWindClass;
begin
  result := TJson.JsonToObject<TWindClass>(AJsonString)
end;

{TWeatherData}

function TWeatherData.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TWeatherData.FromJsonString(AJsonString: string): TWeatherData;
begin
  result := TJson.JsonToObject<TWeatherData>(AJsonString)
end;

{TWeatherItem}

function TWeatherItem.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TWeatherItem.FromJsonString(AJsonString: string): TWeatherItem;
begin
  result := TJson.JsonToObject<TWeatherItem>(AJsonString)
end;

{TGeoCoords}

function TGeoCoords.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TGeoCoords.FromJsonString(AJsonString: string): TGeoCoords;
begin
  result := TJson.JsonToObject<TGeoCoords>(AJsonString)
end;

{TWeather}

constructor TWeather.Create;
begin
  inherited;
  FCoord := TGeoCoords.Create();
  FMain := TWeatherData.Create();
  FWind := TWindClass.Create();
  FClouds := TCloudsClass.Create();
  FSys := TSysClass.Create();
end;

destructor TWeather.Destroy;
var
  LweatherItem: TWeatherItem;
begin

  for LweatherItem in FWeather do
    LweatherItem.Free;

  FCoord.Free;
  FMain.Free;
  FWind.Free;
  FClouds.Free;
  FSys.Free;
  inherited;
end;

function TWeather.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TWeather.FromJsonString(AJsonString: string): TWeather;
begin
  Result := TJson.JsonToObject<TWeather>(AJsonString)
end;

procedure TWeather.ParseFromJson(AJsonObject: TJsonObject);
var
  LweatherItem: TWeatherItem;
begin
  for LweatherItem in FWeather do
    LweatherItem.Free;
  TJson.JsonToObject(Self, AJsonObject);
end;

end.

