program Weather;

uses
  Winapi.Windows,
  Vcl.Forms,
  Weather.Main in 'Weather.Main.pas' {FormWeather},
  Weather.Classes in 'Weather.Classes.pas',
  Weather.Settings in 'Weather.Settings.pas' {FormSettings};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := False;
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
  Application.Title := 'Погода';
  Application.CreateForm(TFormWeather, FormWeather);
  Application.CreateForm(TFormSettings, FormSettings);
  Application.Run;
end.

