unit main;

interface

uses System.SysUtils, System.Classes, Web.HTTPApp,
  Vcl.Forms,
  Windows, Messages, Graphics, Controls,
  ExtCtrls, StdCtrls, uPSCompiler, uPSRuntime, uPSDisassembly, uPSPreprocessor, uPSUtils,
  Menus, uPSC_comobj, uPSR_comobj, uPSComponent, uPSC_dateutils, uPSI_HTTPApp,
  MultipartParser, MVCFramework.Session, SessionUnit, uPSComponent_DB,
  System.Generics.Collections, uPSC_DB, uPSR_DB, Data.FMTBcd,
  Data.DBXMySQL, Data.DB, Data.SqlExpr, uPSComponent_Default, uPSC_std, uPSC_classes,
  uPSC_graphics, uPSC_controls, uPSC_forms, uPSC_stdctrls, uPSR_std,
  uPSR_classes, uPSR_controls, uPSR_forms, uPSR_dll, DBAccess, Uni, MemDS,
  uPSI_SQLConnection, UniProvider, ODBCUniProvider, AdvantageUniProvider,
  MongoDBUniProvider, SQLiteUniProvider, SQLServerUniProvider,
  PostgreSQLUniProvider, OracleUniProvider, NexusDBUniProvider,
  MySQLUniProvider, InterBaseUniProvider, DBFUniProvider, DB2UniProvider,
  ASEUniProvider, AccessUniProvider;


type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button2: TButton;
    PSScript1: TPSScript;
    UniConnection1: TUniConnection;
    UniQuery1: TUniQuery;
    procedure Button2Click(Sender: TObject);
    procedure PSScript1ExecImport(Sender: TObject; se: TPSExec;
      x: TPSRuntimeClassImporter);
    procedure PSScript1Compile(Sender: TPSScript);
    procedure PSScript1Execute(Sender: TPSScript);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button2Click(Sender: TObject);
var
  PSScript: TPSScript;
  sCode, d: AnsiString;
  mainFileName: string;
  i: integer;


  procedure Outputtxt(const s: string);
  begin
    Memo1.Lines.Add(s);
  end;

begin
  try

      PSScript1.Script.Text := Memo1.Lines.Text;
      PSScript1.Comp.AllowNoEnd := true;
      PSScript1.Comp.AllowNoBegin := true;
      PSScript1.Comp.AllowUnit := true;
      if PSScript1.Compile() then
      begin
        PSScript1.Execute();
      end
      else
      begin

        Outputtxt('Failed when compiling <br/>');
        for i:= 0 to PSScript1.CompilerMessageCount - 1 do
          Outputtxt(PSScript1.CompilerMessages[i].MessageToString + #13);
      end;
  finally
  end;
end;

procedure TForm1.PSScript1Compile(Sender: TPSScript);
begin
  RegisterDateTimeLibrary_C(Sender.Comp);
  SIRegister_Std(Sender.Comp);
  SIRegister_Classes(Sender.Comp, True);
  SIRegister_Graphics(Sender.Comp, True);
  SIRegister_Controls(Sender.Comp);
  SIRegister_stdctrls(Sender.Comp);
  SIRegister_Forms(Sender.Comp);
  SIRegister_ComObj(Sender.Comp);

  SIRegister_HTTPApp(Sender.Comp);

  SIRegister_DB(Sender.Comp);

  SIRegister_SQLConnection(Sender.Comp);

  Sender.AddRegisteredVariable('MEMO1', 'TMemo');
end;

procedure TForm1.PSScript1ExecImport(Sender: TObject; se: TPSExec;
  x: TPSRuntimeClassImporter);
begin
  RIRegister_Std(x);
  RIRegister_Classes(x,true);
  RIRegister_Controls(x);
  RIRegister_Forms(x);
  RegisterDLLRuntime(se);
  RegisterClassLibraryRuntime(se, x);
  RIRegister_ComObj(se);
  RIRegisterTObject(x);

  RIRegister_HTTPApp(x);
  RIRegister_HTTPApp_Routines(se);

  RIRegister_DB(x);

  RIRegister_SQLConnection(x);
end;

procedure TForm1.PSScript1Execute(Sender: TPSScript);
begin
  Sender.SetVarToInstance('MEMO1', Memo1);
end;

end.
