unit Weather.Settings;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, HGM.Button, System.ImageList, Vcl.ImgList, Vcl.ExtCtrls,
  HGM.Controls.Labels;

type
  TFormSettings = class(TForm)
    ButtonFlatSetCancel: TButtonFlat;
    ButtonFlatSetOk: TButtonFlat;
    Label1: TLabel;
    LabelError: TLabel;
    ImageList24: TImageList;
    EditCity: TEdit;
    Label2: TLabel;
    LabelExColor: TLabelEx;
    ColorDialog: TColorDialog;
    procedure ButtonFlatSetOkClick(Sender: TObject);
    procedure ButtonFlatSetCancelClick(Sender: TObject);
    procedure LabelExColorClick(Sender: TObject);
  private
    { Private declarations }
  public
    class function Execute(var City: string; var AColor: TColor): Boolean;
  end;

var
  FormSettings: TFormSettings;

implementation

uses
  Weather.Main;

{$R *.dfm}

procedure TFormSettings.ButtonFlatSetCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFormSettings.ButtonFlatSetOkClick(Sender: TObject);
begin
  LabelError.Hide;
  Application.ProcessMessages;
  with FormWeather do
  begin
    FCheckIt := True;
    RESTRequest.Params.ParameterByName('q').Value := EditCity.Text;
    RESTRequest.Execute;
    FCheckIt := False;
    if FLocateError then
    begin
      LabelError.Show;
      Exit;
    end
    else
      Self.ModalResult := mrOk;
  end;
end;

class function TFormSettings.Execute(var City: string; var AColor: TColor): Boolean;
begin
  with TFormSettings.Create(nil) do
  begin
    EditCity.Text := City;
    LabelExColor.StyledColor(AColor);
    Result := ShowModal = mrOk;
    if Result then
    begin
      City := EditCity.Text;
      AColor := LabelExColor.Brush.Color;
    end;
    Free;
  end;
end;

procedure TFormSettings.LabelExColorClick(Sender: TObject);
begin
  if ColorDialog.Execute(Handle) then
  begin
    LabelExColor.StyledColor(ColorDialog.Color);
    FormWeather.SetThemeColor(ColorDialog.Color);
    FormWeather.Repaint;
  end;
end;

end.

