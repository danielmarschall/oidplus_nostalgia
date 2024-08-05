program OIDPLUS;

uses
  Forms,
  Main in 'Main.pas' {Form1},
  SortStrings in 'SortStrings.pas',
  FUNCS in 'FUNCS.pas',
  OIDFILE in 'OIDFILE.pas',
  RAFILE in 'RAFILE.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
