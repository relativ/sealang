program debug;

uses
  Vcl.Forms,
  main in 'main.pas' {Form1},
  uPSI_HTTPApp in '..\utils\uPSI_HTTPApp.pas',
  MultipartParser in '..\utils\MultipartParser.pas',
  SessionUnit in '..\library\SessionUnit.pas',
  MVCFramework.Session in '..\utils\MVCFramework.Session.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
