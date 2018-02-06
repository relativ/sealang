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
        Delete(strLine, 0, Pos('<?pas',strLine) -1);
      end;

      //tmpStr := copy(strLine, Pos('<?pas',strLine) + 4, Length(strLine)) + #1310;
      Delete(strLine, 1, Pos('<?pas',strLine) + 4);
      if Pos('?>',strLine) > 0 then
      begin
        tmpStr := copy(strLine, 0 , Pos('?>',strLine) - 1);
        Delete(strLine, 0, Pos('?>',strLine) + 1);
      end else
        tmpStr := strLine + ' ';

      sLineCode := sLineCode + tmpStr + #13#10;
    end;

  result := sLineCode;

end;

function ParsePascalCodes(code: string): string;
  var
    sList: TStringList;
    I: integer;
    bStartCode, bEndCode, bOnylCode: boolean;
    sLineCode,strLine, tmpStr: string;
  begin
    sList:= TStringList.Create;
    sList.Text := code;

    bStartCode := false;
    bEndCode   := true;
    for I := 0 to sList.Count -1 do
    begin
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
        sLineCode := sLineCode + strLine + #13#10;
      end;


    end;


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

function Replace(const S, OldPattern, NewPattern: string): string;
begin
  result := StringReplace(S, OldPattern, NewPattern, [rfReplaceAll]);
end;

procedure TPascalModule.MyOnCompile(Sender: TPSScript);
begin
  Sender.AddFunction(@Writeln, 'procedure Writeln(s: string)');
  Sender.AddFunction(@Write, 'procedure Write(s: string)');
  Sender.AddFunction(@echo, 'procedure echo(s: string)');

  Sender.AddFunction(@StringReplace, 'function Replace(const S, OldPattern, NewPattern: string): string;');
  Sender.AddFunction(@ExtractFilePath, 'function ExtractFilePath(const FileName: string): string;');
  Sender.AddFunction(@ExtractFileDir, 'function ExtractFileDir(const FileName: string): string;');
  Sender.AddFunction(@ExtractFileName, 'function ExtractFileName(const FileName: string): string;');
  Sender.AddFunction(@ExtractFileExt, 'function ExtractFileExt(const FileName: string): string;');


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
