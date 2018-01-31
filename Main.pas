unit Main;

interface

uses System.SysUtils, System.Classes, Web.HTTPApp,
  Windows, Messages, Graphics, Controls,
  ExtCtrls, StdCtrls, uPSCompiler, uPSRuntime, uPSDisassembly, uPSPreprocessor, uPSUtils,
  Menus, uPSC_comobj, uPSR_comobj, uPSComponent;

type
  TPascalModule = class(TWebModule)
    procedure WebModuleException(Sender: TObject; E: Exception;
      var Handled: Boolean);
    procedure PascalModuleDefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  private
    
    procedure Compile(Request: TWebRequest; Response: TWebResponse);
    procedure MyOnCompile(Sender: TPSScript);
    procedure OnExecImport(Sender: TObject; se: TPSExec;
      x: TPSRuntimeClassImporter);
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TPascalModule;

implementation

uses
  uPSC_dll, uPSR_dll, uPSDebugger,
  uPSR_std, uPSC_std, uPSR_stdctrls, uPSC_stdctrls,
  uPSR_forms, uPSC_forms,

  uPSC_graphics,
  uPSC_controls,
  uPSC_classes,
  uPSR_graphics,
  uPSR_controls,
  uPSR_classes;

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

var
  Imp: TPSRuntimeClassImporter;
  Line: integer;
  IgnoreRunline: Boolean = False;
  FCompilerOutput: string;
  _Request: TWebRequest; _Response: TWebResponse;


procedure RunLine(Sender: TPSExec);
begin
  if IgnoreRunline then Exit;
  Line := (Line + 1) mod 15;
  Sender.GetVar('');
  //if i = 0 then Application.ProcessMessages;
end;

function ParsePascalCodes(code: string): string;
  var
    sList: TStringList;
    I: integer;
    bStartCode: boolean;
    sLineCode,strLine: string;
  begin
    sLineCode := StringReplace(code, '?>', '', [rfReplaceAll]);
    sLineCode := StringReplace(sLineCode, '<?pas', '', [rfReplaceAll]);
    {sList:= TStringList.Create;
    sList.Text := code;
    for I := 0 to sList.Count -1 do
    begin
      strLine := sList[I];
      if Pos('<?pas',strLine) > 0  then
        bStartCode := true;

      if bStartCode then
      begin
        if Pos('<?pas',strLine) > 0 then
          sLineCode := sLineCode + copy(strLine, Pos('<?pas',strLine) + 5, Length(strLine)) + #1310
        else
           sLineCode := sLineCode + strLine + #1310;
        sLineCode := StringReplace(sLineCode, '?>', '', [rfReplaceAll]);
      end;

    end;
    }

    Result := sLineCode;
  end;

function StringLoadFile(const Filename: string): string;
var
  sList: TStringList;
begin
  sList:= TStringList.Create;
  try
    sList.LoadFromFile(Filename);
    Result := sList.Text;
  finally
    sList.Free;
  end;
end;


procedure Writeln(s: string);
begin
  _Response.Content := _Response.Content + s+ #13#10;
end;

procedure Write(s: string);
begin
  _Response.Content := _Response.Content + s;
end;

procedure echo(s:string);
begin
  _Response.Content := _Response.Content + s;
end;

procedure TPascalModule.MyOnCompile(Sender: TPSScript);
begin
  Sender.AddFunction(@Writeln, 'procedure Writeln(s: string)');
  Sender.AddFunction(@Writeln, 'procedure Write(s: string)');
  Sender.AddFunction(@echo, 'procedure echo(s: string)');
  SIRegister_Std(Sender.Comp);
  SIRegister_Classes(Sender.Comp, True);
  SIRegister_Graphics(Sender.Comp, True);
  SIRegister_Controls(Sender.Comp);
  SIRegister_stdctrls(Sender.Comp);
  //SIRegister_Forms(Sender.Comp);
  SIRegister_ComObj(Sender.Comp);

end;

procedure TPascalModule.OnExecImport(Sender: TObject; se: TPSExec; x: TPSRuntimeClassImporter);
begin
  RIRegister_Std(x);
  RIRegister_Classes(x,true);
  RIRegister_Controls(x);
  RIRegister_Forms(x);
  RegisterDLLRuntime(se);
  RegisterClassLibraryRuntime(se, x);
  RIRegister_ComObj(se);
end;



procedure TPascalModule.Compile(Request: TWebRequest; Response: TWebResponse);
var
  PSScript: TPSScript;
  sCode, d: AnsiString;
  mainFileName: string;
  i: integer;

  procedure Outputtxt(const s: string);
  begin
    Response.Content := Response.Content + s;
  end;

  procedure OutputMsgs;
  var
    l: Longint;
    b: Boolean;
  begin
    b := False;
    for l := 0 to PSScript.Comp.MsgCount - 1 do
    begin
      Outputtxt(PSScript.Comp.Msg[l].MessageToString);
      if (not b) and (PSScript.Comp.Msg[l] is TPSPascalCompilerError) then
      begin
        b := True;
      end;
    end;
  end;


begin
  try
    mainFileName := StringReplace(Request.PathTranslated, '/', '\', [rfReplaceAll]);

    sCode := StringLoadFile(mainFileName);
    sCode:= ParsePascalCodes(sCode);

    PSScript:= TPSScript.Create(nil);
    PSScript.Script.Text := sCode;
    //PSScript.SetPointerToData('Request', @Request, PSScript.FindNamedType('TWebRequest'));
    //PSScript.SetPointerToData('Response', @Response, PSScript.FindNamedType('TWebResponse'));
    //PSScript.SetVarToInstance('Request', Request);
    //PSScript.SetVarToInstance('Response', Response);
    PSScript.OnCompile := MyOnCompile;
    PSScript.OnExecImport := OnExecImport;
    PSScript.Comp.OnExternalProc := DllExternalProc;
    PSScript.Comp.AllowNoEnd := true;
    if PSScript.Compile() then
    begin
      PSScript.Execute();
    end
    else
    begin

      Outputtxt('Failed when compiling <br/>');
      for i:= 0 to PSScript.CompilerMessageCount - 1 do
        Outputtxt(PSScript.CompilerMessages[i].MessageToString + #13);
      PSScript.Free;
    end;
    PSScript.Free;
  finally
  end;
end;

procedure TPascalModule.PascalModuleDefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  {Response.Content := 'PathTranslated : '+Request.PathTranslated + '<br>';
  Response.Content := Response.Content + 'PathInfo : '+Request.PathInfo + '<br>';
  Response.Content := Response.Content + 'Query : '+Request.Query + '<br>';
  Response.Content := Response.Content + 'ScriptName : '+Request.ScriptName + '<br>';
  Response.Content := Response.Content + 'InternalPathInfo : '+Request.InternalPathInfo + '<br>';
   }
  _Request := Request;
  _Response := Response;

  Compile(Request, Response);
  Response.SendResponse;
  Handled := true;
end;

procedure TPascalModule.WebModuleException(Sender: TObject; E: Exception;
  var Handled: Boolean);
var
  sList: TStringList;
begin
  sList:= TStringList.Create;
  sList.Text := E.Message+ #13#10+ #13#10;
  sList.Text := sList.Text + E.StackTrace+ #13#10;
  sList.Text := sList.Text + '-----------------------------------------'+ #13#10;
  sList.SaveToFile(ExtractFilePath(GetModuleName(HInstance)) + '\error.log');
  sList.Free;

end;

end.
