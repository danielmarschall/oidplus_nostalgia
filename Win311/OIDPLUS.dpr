program OIDPLUS;

uses
  Forms,
  SortStr in 'SortStr.pas',
  Main in 'Main.pas' {Form1},
  FUNCS in 'FUNCS.pas',
  OIDFILE in 'OIDFILE.pas',
  RAFILE in 'RAFILE.pas';

{$R *.RES}

begin
  { Application.Initialize; }
  Application.Title := 'OIDPlus';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
