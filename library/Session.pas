unit Session;

interface

uses
   SysUtils
  ,Classes
  , uPSCompiler, uPSRuntime, uPSDisassembly, uPSPreprocessor, uPSUtils,
  Menus, uPSC_comobj, uPSR_comobj, uPSComponent, uPSC_dateutils,
  MVCFramework.Session;


  type
    TSession = class(TObject)
    private
      FSessionID: string;
    public
      property SessionID: string read FSessionID write FSessionID;
      function GetValue(name: string): string;
      procedure SetValue(name, value: string);
      procedure Delete(name: string);

    end;

implementation



{ TSession }


procedure TSession.Delete(name: string);
var
  v: TWebSessionMemory;
begin
  v := SessionList.Items[SessionID] as TWebSessionMemory;
  if v <> nil then
  begin
    v.Delete(name);
  end;

end;

function TSession.GetValue(name: string): string;
var
  v: TWebSessionMemory;
begin
  v := SessionList.Items[SessionID] as TWebSessionMemory;
  if v <> nil then
  begin
    Result := v.Items[name];
  end else Result := '';

end;

procedure TSession.SetValue(name, value: string);
var
  v: TWebSessionMemory;
begin
  v := SessionList.Items[SessionID] as TWebSessionMemory;
  if v = nil then
  begin
    v:= TWebSessionMemory.Create(SessionID, 600000);
    SessionList.Add(SessionID, v);
  end;
  v.Items[name] := value;

end;

end.
