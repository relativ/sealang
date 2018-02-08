unit Main;

interface

uses System.SysUtils, System.Classes, Web.HTTPApp,
  Windows, Messages, Graphics, Controls,
  ExtCtrls, StdCtrls, uPSCompiler, uPSRuntime, uPSDisassembly, uPSPreprocessor, uPSUtils,
  Menus, uPSC_comobj, uPSR_comobj, uPSComponent, uPSC_dateutils, uPSI_HTTPApp,
  MultipartParser, MVCFramework.Session, Session;

type
  TPascalModule = class(TWebModule)
    procedure WebModuleException(Sender: TObject; E: Exception;
      var Handled: Boolean);
    procedure PascalModuleDefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    Session : TSession;
    AppPath: string;
    procedure Compile(Request: TWebRequest; Response: TWebResponse);
    procedure MyOnCompile(Sender: TPSScript);
    procedure OnExecImport(Sender: TObject; se: TPSExec;
      x: TPSRuntimeClassImporter);
    procedure MyOnExecute(Sender: TPSScript);
    procedure echo(s: string);
    procedure Write(s: string);
    procedure Writeln(s: string);
    function ParsePascalCodes(code: string): string;
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

function TPascalModule.ParsePascalCodes(code: string): string;
  var
    sList, tmpList: TStringList;
    I: integer;
    bStartCode, bEndCode, bOnylCode: boolean;
    sLineCode,strLine, tmpStr, strInclude: string;
    A: Integer;
    sIncludeList: TStringList;
  label StartParsing;
  begin
    sList:= TStringList.Create;
    sList.Text := code;

    sIncludeList:= TStringList.Create;

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

      if bStartCode then
      begin
          sLineCode := sLineCode + ParseScriptLine(strLine);
      end else if bEndCode then
      begin
        strLine := StringReplace(strLine, '?>', '', [rfReplaceAll]);
        if Trim(strLine) <> '' then
        begin
          tmpStr := StringReplace(strLine, chr(39), chr(39) + chr(39), [rfReplaceAll]);
          sLineCode := sLineCode + 'echo(' + chr(39) + tmpStr + chr(39) + '); ' + #13#10;
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
              strInclude:= ParsePascalCodes(strInclude);

              tmpStr := Copy(strLine, Pos('{$I', strLine), Pos('}', strLine) + 1 );

              sList.Text := StringReplace(sList.Text, tmpStr, strInclude, [rfReplaceAll]);

              goto StartParsing;
            end else raise Exception.Create('Circular include ' + strInclude);


          end else
            sLineCode := sLineCode + strLine + #13#10;
      end;

      Inc(I);

    end;


    Result := sLineCode;
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
  Sender.SetVarToInstance('SESSION', Session);
end;

procedure TPascalModule.MyOnCompile(Sender: TPSScript);
begin
  Sender.AddMethod(self, @TPascalModule.Writeln, 'procedure Writeln(s: string)');
  Sender.AddMethod(self, @TPascalModule.Write, 'procedure Write(s: string)');
  Sender.AddMethod(self, @TPascalModule.echo, 'procedure echo(s: string)');

  ImplementFucntions(Sender);


  RegisterDateTimeLibrary_C(Sender.Comp);
  SIRegister_Std(Sender.Comp);
  SIRegister_Classes(Sender.Comp, True);
  SIRegister_Graphics(Sender.Comp, True);
  SIRegister_Controls(Sender.Comp);
  SIRegister_stdctrls(Sender.Comp);
  SIRegister_Forms(Sender.Comp);
  SIRegister_ComObj(Sender.Comp);

  SIRegister_HTTPApp(Sender.Comp);

  Sender.AddRegisteredVariable('RESPONSE', 'TWebResponse');
  Sender.AddRegisteredVariable('REQUEST', 'TWebRequest');
  Sender.AddRegisteredVariable('SESSION', 'TSession');

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
  RIRegisterTObject(x);

  RIRegister_HTTPApp(x);
  RIRegister_HTTPApp_Routines(se);
end;



procedure TPascalModule.Compile(Request: TWebRequest; Response: TWebResponse);
var
  PSScript: TPSScript;
  sCode, d: AnsiString;
  mainFileName: string;
  i: integer;
  SessionID: string;


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

    SessionID := GetSessionID(Request, Response);

    Session.SessionID := SessionID;
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
      PSScript.Free;
    end;
    PSScript.Free;
    Session.Free;
  finally
  end;
end;

function GetSessionID(Request: TWebRequest; Response: TWebResponse): string;
var
  I: Integer;
  Cookie: TCookie;
  bFound: boolean;
  UniqueID : TGUID;
begin
  bFound := true;
  for I := 0 to Response.Cookies.Count -1 do
  begin
    Cookie := Response.Cookies.Items[I];
    if Cookie.Name = '_PascalWebSessionID_' then
    begin
      bFound := false;
      Result := Cookie.Value;
      break;
    end;
  end;

  if not bFound then
  begin
    CreateGUID(UniqueID);
    Cookie := Response.Cookies.Add;
    Cookie.Name := '_PascalWebSessionID_';
    Cookie.Value := GUIDToString(UniqueID);
    Result := Cookie.Value;
  end;

end;

procedure TPascalModule.PascalModuleDefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin

  try

    AppPath := StringReplace(Request.PathTranslated, '/', '\', [rfReplaceAll]);
    AppPath := ExtractFilePath(AppPath);

    Compile(Request, Response);
    Response.SendResponse;
    Handled := true;
  except on E: Exception do
    echo (E.Message + '<br/><br/>' + E.StackTrace);
  end;
end;

procedure TPascalModule.WebModuleCreate(Sender: TObject);
begin
  Session := TSession.Create;
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

procedure TPascalModule.WebModuleDestroy(Sender: TObject);
begin
  Session.Free;
end;

end.
