unit Weather.Settings;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, HGM.Button, System.ImageList, Vcl.ImgList;

type
  TFormSettings = class(TForm)
    ButtonFlatSetCancel: TButtonFlat;
    ButtonFlatSetOk: TButtonFlat;
    Label1: TLabel;
    LabelError: TLabel;
    ImageList24: TImageList;
    EditCity: TEdit;
    procedure ButtonFlatSetOkClick(Sender: TObject);
    procedure ButtonFlatSetCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    class function Execute(var City: string): Boolean;
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

class function TFormSettings.Execute(var City: string): Boolean;
begin
  with TFormSettings.Create(nil) do
  begin
    EditCity.Text := City;
    Result := ShowModal = mrOk;
    if Result then
      City := EditCity.Text;
    Free;
  end;
end;

end.

