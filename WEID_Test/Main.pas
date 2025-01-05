unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm3 = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    Edit2: TEdit;
    Edit3: TEdit;
    Button2: TButton;
    Edit4: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

uses
  WEID_Delphi;

procedure TForm3.Button1Click(Sender: TObject);
begin
  Edit2.Text := OidToWeid(Edit1.Text);
end;

procedure TForm3.Button2Click(Sender: TObject);
var
  weid: string;
begin
  weid := Edit3.Text;
  Edit4.Text := WeidToOid(weid);
  if Edit4.Text <> '' then Edit3.Text := weid;
end;

end.
