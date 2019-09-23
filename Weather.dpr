program Weather;

uses
  Vcl.Forms,
  Weather.Main in 'Weather.Main.pas' {FormWeather},
  Weather.Classes in 'Weather.Classes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  ReportMemoryLeaksOnShutdown := True;
  Application.CreateForm(TFormWeather, FormWeather);
  Application.Run;
end.
