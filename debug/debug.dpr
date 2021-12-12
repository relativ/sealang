program debug;

uses
  Vcl.Forms,
  uPSI_HTTPApp in '..\utils\uPSI_HTTPApp.pas',
  SessionUnit in '..\library\SessionUnit.pas',
  MVCFramework.Session in '..\utils\MVCFramework.Session.pas',
  main in 'main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
