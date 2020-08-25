unit Main;

interface

uses System.SysUtils, System.Classes, Web.HTTPApp,
  Windows, Messages, Graphics, Controls,
  ExtCtrls, StdCtrls, uPSCompiler, uPSRuntime, uPSDisassembly, uPSPreprocessor, uPSUtils,
  Menus, uPSC_comobj, uPSR_comobj, uPSComponent, uPSC_dateutils, uPSI_HTTPApp,
  MultipartParser, MVCFramework.Session, SessionUnit, uPSComponent_DB,
  System.Generics.Collections, uPSC_DB, uPSR_DB, uPSComponent_StdCtrls,
  uPSComponent_Controls, uPSComponent_COM, uPSComponent_Default, Jpeg,
  pngimage;

type
  PPSPascalCompiler = ^ TPSPascalCompiler;
  PPSRuntimeClassImporter = ^ TPSRuntimeClassImporter;

  TRegisterPlugin = function(): TPSPlugin; stdcall;

  TPascalModule = class(TWebModule)
    PSImport_Classes: TPSImport_Classes;
    PSImport_DateUtils: TPSImport_DateUtils;
    PSImport_ComObj: TPSImport_ComObj;
    PSImport_DB: TPSImport_DB;
    PSImport_Controls: TPSImport_Controls;
    PSImport_StdCtrls: TPSImport_StdCtrls;
    procedure WebModuleException(Sender: TObject; E: Exception;
      var Handled: Boolean);
    procedure PascalModuleDefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleDestroy(Sender: TObject);
    procedure WebModuleCreate(Sender: TObject);
  private
    SessionObject : TSession;
    PluginList: TList<TPSPlugin>;
    RunTimeVariables: TDictionary<string, string>;
    AppPath, MainFileName: string;
    procedure Compile(Request: TWebRequest; Response: TWebResponse);
    procedure MyOnCompile(Sender: TPSScript);
    procedure OnExecImport(Sender: TObject; se: TPSExec;
      x: TPSRuntimeClassImporter);
    procedure MyOnExecute(Sender: TPSScript);
    procedure echo(s: string);
    procedure Write(s: string);
    procedure Writeln(s: string);
    function ParsePascalCodes(code: string; RunTimeVariables: TDictionary<string, string>; checkVariables: boolean = true): string;

    procedure DLLPlugins();
  public

  end;

var
  WebModuleClass: TComponentClass = TPascalModule;

  function GetSessionID(Request: TWebRequest; Response: TWebResponse): string;

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
  uPSR_classes,
  ExternalFunctions;

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

var
  Imp: TPSRuntimeClassImporter;
  Line: integer;
  IgnoreRunline: Boolean = False;
  FCompilerOutput: string;


procedure RunLine(Sender: TPSExec);
begin
  if IgnoreRunline then Exit;
  Line := (Line + 1) mod 15;
  Sender.GetVar('');
  //if i = 0 then Application.ProcessMessages;
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

function ParseCheckVariables(line: string; RunTimeVariables: TDictionary<string, string>): string;
var
  strLine, varStr, name, types: string;
  sNames: TStringList;
  I: Integer;
begin
  strLine := line;
  while (Pos('var ', LowerCase(strLine)) > 0) do
  begin
    if Trim(strLine) = 'var' then
      break;

    varStr := copy(strLine, Pos('var ', LowerCase(strLine)), Pos(';', strLine)+ 1);
    Delete(varStr, 1,Pos('var ', LowerCase(varStr)) + 3);
    name := copy(varStr, 0, Pos(':', varStr) -1);
    name := trim(name);
    Delete(varStr, 1, Pos(':', varStr));
    types := copy(varStr, 0, Pos(';', varStr) - 1);
    types := trim(types);

    sNames:= TStringList.Create;
    sNames.Text := StringReplace(name, ',',#13,[rfReplaceAll]);
    for I := 0 to sNames.Count -1 do
      RunTimeVariables.Add(Trim(sNames[I]), types);
    sNames.Free;
    Delete(strLine, Pos('var ', LowerCase(strLine)), Pos(';', strLine) + 1);
  end;
  Result := strLine;
end;

function ParseScriptLine(lineCode: string): string;
var
  sLineCode,strLine, tmpStr: string;
begin
  strLine := lineCode;

  while(Pos('<?pas',strLine) > 0) do
  begin
    tmpStr := copy(strLine, 0, Pos('<?pas',strLine) - 1);
    if tmpStr <> '' then
    begin
      tmpStr := StringReplace(tmpStr, chr(39), chr(39) + chr(39), [rfReplaceAll]);
      sLineCode := sLineCode + 'echo(' + chr(39) + tmpStr + chr(39) + '); ';
      Delete(strLine, 1, Pos('<?pas',strLine) -1);
    end;

    //tmpStr := copy(strLine, Pos('<?pas',strLine) + 4, Length(strLine)) + #1310;
    Delete(strLine, 1, Pos('<?pas',strLine) + 4);
    if Pos('?>',strLine) > 0 then
    begin
      tmpStr := copy(strLine, 0 , Pos('?>',strLine) - 1);
      Delete(strLine, 1, Pos('?>',strLine) + 1);
    end else
      tmpStr := strLine + ' ';

    sLineCode := sLineCode + tmpStr + #13#10;
  end;

  if strLine <> '' then
  begin
      tmpStr := StringReplace(strLine, chr(39), chr(39) + chr(39), [rfReplaceAll]);
      sLineCode := sLineCode + 'echo(' + chr(39) + tmpStr + chr(39) + '); ';
      strLine := '';
  end;

  result := sLineCode;

end;

function TPascalModule.ParsePascalCodes(code: string; RunTimeVariables: TDictionary<string, string>; checkVariables: boolean = true): string;
var
  sList, tmpList, sLineCode: TStringList;
  I: integer;
  bStartCode, bEndCode, bOnylCode: boolean;
  strLine, tmpStr, strInclude: string;
  A: Integer;
  sIncludeList: TStringList;
label StartParsing;
begin
  sList:= TStringList.Create;
  sList.Text := code;

  sIncludeList:= TStringList.Create;
  sLineCode:= TStringList.Create;

  bStartCode := false;
  bEndCode   := true;
  I := 0;
  while I < sList.Count do
  begin
    StartParsing:
    strLine := sList[I];
    if Pos('<?pas',strLine) > 0  then
    begin
      bStartCode := true;
      bEndCode := false;
      bOnylCode := false;
    end else if Pos('?>',strLine) > 0  then
    begin
      bStartCode := false;
      bEndCode := true;
      bOnylCode := false;
    end else if bStartCode then
    begin
      bOnylCode := true;
      bStartCode := false;
    end;

    if (checkVariables) then
    begin
      strLine := ParseCheckVariables(strLine, RunTimeVariables);
    end;



    if bStartCode then
    begin
        sLineCode.Add(ParseScriptLine(strLine));
    end else if bEndCode then
    begin
      strLine := StringReplace(strLine, '?>', '', [rfReplaceAll]);
      if Trim(strLine) <> '' then
      begin
        tmpStr := StringReplace(strLine, chr(39), chr(39) + chr(39), [rfReplaceAll]);
        sLineCode.Add('echo(' + chr(39) + tmpStr + chr(39) + '); ');
      end;
    end else if bOnylCode then
    begin
        if Pos('{$I', strLine) > 0 then
        begin
          tmpStr := strLine;

          Delete(tmpStr, 1, Pos(chr(39), tmpStr));
          strInclude := Copy(tmpStr, 0, Pos(chr(39), tmpStr) - 1);

          if sIncludeList.IndexOf(strInclude) = -1 then
          begin
            sIncludeList.Add(strInclude);
            strInclude := StringLoadFile( AppPath + strInclude);
            strInclude:= ParsePascalCodes(strInclude,RunTimeVariables, false);

            tmpStr := Copy(strLine, Pos('{$I', strLine), Pos('}', strLine) + 1 );

            sList.Text := StringReplace(sList.Text, tmpStr, strInclude, [rfReplaceAll]);

            goto StartParsing;
          end else raise Exception.Create('Circular include ' + strInclude);


        end else
          sLineCode.Add(strLine);
    end;

    Inc(I);

  end;


  Result := sLineCode.Text;
end;


procedure TPascalModule.WebModuleCreate(Sender: TObject);
begin
  SessionObject := TSession.Create;
  PluginList := TList<TPSPlugin>.Create;
  DLLPlugins();
end;

procedure TPascalModule.Write(s: string);
begin
  Response.Content := Response.Content + s;
end;

procedure TPascalModule.Writeln(s: string);
begin
  Write(s+ #13#10);
end;

procedure TPascalModule.echo(s:string);
begin
  Write(s);
end;

procedure TPascalModule.MyOnExecute(Sender: TPSScript);
begin
  Sender.SetVarToInstance('RESPONSE', Response);
  Sender.SetVarToInstance('REQUEST', Request);
  Sender.SetVarToInstance('SESSION', SessionObject);
  Sender.SetVarToInstance('COOKIES', Request.CookieFields);

end;

procedure TPascalModule.MyOnCompile(Sender: TPSScript);
var
  I: Integer;
  RTVariableKeys: TArray<string>;
  VName, VType :string;
begin
  Sender.AddMethod(self, @TPascalModule.Writeln, 'procedure Writeln(s: string)');
  Sender.AddMethod(self, @TPascalModule.Write, 'procedure Write(s: string)');
  Sender.AddMethod(self, @TPascalModule.echo, 'procedure echo(s: string)');

  ImplementFucntions(Sender);


  RegisterDateTimeLibrary_C(Sender.Comp);
  SIRegister_Graphics(Sender.Comp, True);

  SIRegister_HTTPApp(Sender.Comp);

  SIRegister_DB(Sender.Comp);


  Sender.AddRegisteredVariable('RESPONSE', 'TWebResponse');
  Sender.AddRegisteredVariable('REQUEST', 'TWebRequest');
  Sender.AddRegisteredVariable('SESSION', 'TSession');
  Sender.AddRegisteredVariable('COOKIES', 'TStrings');
  RTVariableKeys := RunTimeVariables.Keys.ToArray();
  for VName in RTVariableKeys do
  begin
    VType := RunTimeVariables[VName];
    Sender.AddRegisteredVariable(VName, VType);
  end;

end;

procedure TPascalModule.OnExecImport(Sender: TObject; se: TPSExec; x: TPSRuntimeClassImporter);
begin

  RegisterDLLRuntime(se);
  RegisterClassLibraryRuntime(se, x);
  RIRegisterTObject(x);

  RIRegister_HTTPApp(x);
  RIRegister_HTTPApp_Routines(se);

  RIRegister_DB(x);

end;

procedure TPascalModule.DLLPlugins();
var
 dllHandle : cardinal;
 searchResult : TSearchRec;
 ExtensionPath: string;
 DLLHandleList: TList<UInt64>;
 method: TRegisterPlugin;
 plugin: TPSPlugin;
begin
  DLLHandleList:= TList<UInt64>.Create;

  ExtensionPath := ExtractFilePath(GetModuleName(HInstance)) + 'extensions\';

  if findfirst(ExtensionPath + '*.dll', faAnyFile, searchResult) = 0 then
  begin
    repeat
      if searchResult.Name = 'mod_pascal.dll' then continue;

      dllHandle := LoadLibrary(PWideChar(ExtensionPath + searchResult.Name));

      @method := GetProcAddress(dllHandle, 'PSPluginCreate') ;
      if Assigned (method) then
      begin
        plugin := method();
        PluginList.Add(plugin);
      end;

      //FreeLibrary(dllHandle) ;
    until FindNext(searchResult) <> 0;
    FindClose(searchResult.FindHandle);
  end;


end;

procedure TPascalModule.Compile(Request: TWebRequest; Response: TWebResponse);
var
  PSScript: TPSScript;
  sCode, d: AnsiString;
  mainFileName: string;
  i: integer;
  PSImport_DB: TPSImport_DB;
  Plugin: TPSPluginItem;


  procedure Outputtxt(const s: string);
  begin
    Response.Content := Response.Content + s;
  end;

begin
  try

      mainFileName := StringReplace(Request.PathTranslated, '/', '\', [rfReplaceAll]);

      PSScript:= TPSScript.Create(nil);
      (PSScript.Plugins.Add as TPSPluginItem).Plugin := PSImport_Classes;
      (PSScript.Plugins.Add as TPSPluginItem).Plugin := PSImport_DateUtils;
      (PSScript.Plugins.Add as TPSPluginItem).Plugin := PSImport_ComObj;
      (PSScript.Plugins.Add as TPSPluginItem).Plugin := PSImport_DB;
      (PSScript.Plugins.Add as TPSPluginItem).Plugin := PSImport_Controls;
      (PSScript.Plugins.Add as TPSPluginItem).Plugin := PSImport_StdCtrls;
      for I := 0 to PluginList.Count -1 do
      begin
        (PSScript.Plugins.Add as TPSPluginItem).Plugin := PluginList.Items[I];
      end;

      PSScript.MainFileName := MainFileName;
      RunTimeVariables:= TDictionary<string, string>.Create();
      sCode := StringLoadFile(mainFileName);
      sCode:= ParsePascalCodes(sCode, RunTimeVariables);

      PSScript.Script.Text := sCode;

      //PSScript.SetPointerToData('Request', @Request, PSScript.FindNamedType('TWebRequest'));
      //PSScript.SetPointerToData('Response', @Response, PSScript.FindNamedType('TWebResponse'));
      //PSScript.SetVarToInstance('Request', Request);
      //PSScript.SetVarToInstance('Response', Response);
      PSScript.OnCompile := MyOnCompile;
      PSScript.OnExecImport := OnExecImport;
      PSScript.OnExecute := MyOnExecute;
      PSScript.Comp.OnExternalProc := DllExternalProc;
      PSScript.Comp.AllowNoEnd := true;
      PSScript.Comp.AllowNoBegin := true;
      PSScript.Comp.AllowUnit := true;
      if PSScript.Compile() then
      begin
        PSScript.Execute();
      end
      else
      begin

        Outputtxt('Failed when compiling <br/>');
        for i:= 0 to PSScript.CompilerMessageCount - 1 do
          Outputtxt(PSScript.CompilerMessages[i].MessageToString + #13);
      end;
      PSScript.Free;
      RunTimeVariables.Free;
  finally
  end;
end;

function GetSessionID(Request: TWebRequest; Response: TWebResponse): string;
var
  Cookie: TCookie;
  UniqueID : TGUID;
  PasSessionID: string;
begin
  PasSessionID := Request.CookieFields.Values['_PascalWebSessionID_'];

  if PasSessionID = '' then
  begin
    CreateGUID(UniqueID);
    Cookie := Response.Cookies.Add;
    Cookie.Name := '_PascalWebSessionID_';
    Cookie.Value := GUIDToString(UniqueID);
    Result := Cookie.Value;
  end else begin
    Result := PasSessionID;
  end;

end;

procedure TPascalModule.PascalModuleDefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  SessionID: string;
begin

  try

    SessionID := GetSessionID(Request, Response);

    SessionObject.SetSessionId(SessionID);

    MainFileName := StringReplace(Request.PathTranslated, '/', '\', [rfReplaceAll]);
    AppPath := ExtractFilePath(MainFileName);



    Compile(Request, Response);

    Response.SendResponse;
    Handled := true;
  except on E: Exception do
    echo (E.Message + '<br/><br/>' + E.StackTrace);
  end;
end;

procedure TPascalModule.WebModuleDestroy(Sender: TObject);
var
  I: Integer;
begin
  SessionObject.Free;
  PluginList.Free;
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
