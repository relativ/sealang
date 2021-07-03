unit main;

interface

uses System.SysUtils, System.Classes, Web.HTTPApp,
  Vcl.Forms,
  Windows, Messages, Graphics, Controls,
  ExtCtrls, StdCtrls, uPSCompiler, uPSRuntime, uPSDisassembly, uPSPreprocessor, uPSUtils,
  Menus, uPSC_comobj, uPSR_comobj, uPSComponent, uPSC_dateutils, uPSI_HTTPApp,
  MVCFramework.Session, SessionUnit, uPSComponent_DB,
  System.Generics.Collections, uPSC_DB, uPSR_DB, Data.FMTBcd,
  Data.DBXMySQL, Data.DB, Data.SqlExpr, uPSComponent_Default, uPSC_std, uPSC_classes,
  uPSC_graphics, uPSC_controls, uPSC_forms, uPSC_stdctrls, uPSR_std,
  uPSR_classes, uPSR_controls, uPSR_forms, uPSR_dll, uPSComponent_StdCtrls,
  uPSComponent_Controls, uPSComponent_COM, IdUDPBase, IdUDPClient,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  pngimage, IdExplicitTLSClientServerBase, IdMessageClient, IdSMTPBase, IdSMTP,
  IdFTP;


type
  TRegisterPlugin = function(): TPSPlugin; stdcall;

  TForm1 = class(TForm)
    Memo1: TMemo;
    Button2: TButton;
    PSScript1: TPSScript;
    PSDllPlugin1: TPSDllPlugin;
    PSImport_Classes1: TPSImport_Classes;
    PSImport_DateUtils1: TPSImport_DateUtils;
    PSImport_ComObj1: TPSImport_ComObj;
    PSImport_DB1: TPSImport_DB;
    PSImport_Controls1: TPSImport_Controls;
    PSImport_StdCtrls1: TPSImport_StdCtrls;
    IdFTP1: TIdFTP;
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PSScript1Execute(Sender: TPSScript);
    procedure PSScript1Compile(Sender: TPSScript);
    procedure PSScript1ExecImport(Sender: TObject; const se: TPSExec;
      const x: TPSRuntimeClassImporter);
  private
    procedure DLLPlugins(PSScript: TPSScript);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


procedure TForm1.DLLPlugins(PSScript: TPSScript);
var
 dllHandle : cardinal;
 searchResult : TSearchRec;
 ExtensionPath: string;
 DLLHandleList: TList<UInt64>;
 method: TRegisterPlugin;
 plugin: TPSPlugin;
 psPluginItem: TPSPluginItem;
begin
  DLLHandleList:= TList<UInt64>.Create;

  ExtensionPath := ExtractFilePath(Application.ExeName);

  if findfirst(ExtensionPath + '*.dll', faAnyFile, searchResult) = 0 then
  begin
    repeat
      dllHandle := LoadLibrary(PWideChar(ExtensionPath + searchResult.Name));

      @method := GetProcAddress(dllHandle, 'PSPluginCreate') ;
      if Assigned (method) then
      begin
        plugin := method();

        psPluginItem := PSScript.Plugins.Add as TPSPluginItem;
        psPluginItem.Plugin := plugin;
      end;

      //FreeLibrary(dllHandle) ;
    until FindNext(searchResult) <> 0;
    FindClose(searchResult.FindHandle);
  end;


end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  DLLPlugins(PSScript1);
 // plugin := TPSImport.Create(nil);
  //(PSScript1.Plugins.Add as TPSPluginItem).Plugin := TPSPlugin(plugin.GetSelf);
end;

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
  SIRegister_HTTPApp(Sender.Comp);

  Sender.AddRegisteredVariable('MEMO1', 'TMemo');
end;

procedure TForm1.PSScript1ExecImport(Sender: TObject; const se: TPSExec;
  const x: TPSRuntimeClassImporter);
begin
  RegisterClassLibraryRuntime(se, x);
  RIRegisterTObject(x);

  RIRegister_HTTPApp(x);
  RIRegister_HTTPApp_Routines(se);
end;

procedure TForm1.PSScript1Execute(Sender: TPSScript);
begin
  Sender.SetVarToInstance('MEMO1', Memo1);


end;

end.
