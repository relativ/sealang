unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DBXMySQL, Data.FMTBcd,
  Data.SqlExpr, Data.DB, Vcl.StdCtrls, uPSComponent, uPSComponent_DB,
  uPSComponent_Default;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    SQLQuery1: TSQLQuery;
    SQLDataSet1: TSQLDataSet;
    SQLTable1: TSQLTable;
    Button1: TButton;
    Button2: TButton;
    PSScript1: TPSScript;
    PSImport_DB1: TPSImport_DB;
    PSImport_Classes1: TPSImport_Classes;
    SQLConnection1: TSQLConnection;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin

  SQLConnection1.DriverName := 'MySQL';
 // SQLConnection1.ConnectionName := 'MySQLConnection';
  SQLConnection1.Params.Values['DriverName'] := 'MySQL';
  SQLConnection1.Params.Values['Password'] := '';
  SQLConnection1.Params.Values['HostName'] := 'localhost';
  SQLConnection1.Params.Values['Database'] := 'mysql';
  SQLConnection1.Params.Values['User_Name'] := 'root';
  SQLConnection1.Connected := true;
  SQLQuery1.Open;
  SQLQuery1.SQL.Text := '';
  while not SQLQuery1.Eof do
  begin

    Memo1.Lines.Add(SQLQuery1.FieldByName('User').AsString);
    SQLQuery1.Next;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  a: TDataSetErrorEvent;
begin
  PSScript1.Script.Text := Memo1.Lines.Text;
  PSScript1.Compile();
  PSScript1.Execute();
end;

end.
