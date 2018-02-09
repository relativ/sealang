unit SessionUnit;

interface

uses
   SysUtils
  ,Classes
  ,MVCFramework.Session;


  type
    TSession = class(TObject)
    private
      FSessionID: string;
    public
      function GetValue(name: string): string;
      procedure SetValue(name: string; value: string);
      procedure Delete(name: string);
      procedure SetSessionId(sessionId: string);

      property SessionID: string read FSessionID;

    end;

implementation


{ TSession }


procedure TSession.Delete(name: string);
var
  v: TWebSessionMemory;
begin
  if SessionID = '' then
    raise Exception.Create('Session id needed!');
  v := SessionList.Items[SessionID];
  if v <> nil then
  begin
    v.Delete(name);
  end;

end;

function TSession.GetValue(name: string): string;
var
  v: TWebSessionMemory;
begin
  if SessionID = '' then
    raise Exception.Create('Session id needed!');
  v := SessionList.Items[SessionID];
  if v <> nil then
  begin
    Result := v.Items[name];
  end else Result := '';

end;

procedure TSession.SetSessionId(sessionId: string);
begin
  FSessionID := sessionId;
end;

procedure TSession.SetValue(name:string; value: string);
var
  v: TWebSessionMemory;
begin
  if SessionID = '' then
    raise Exception.Create('Session id needed!');
  if not SessionList.TryGetValue(SessionID, v) then
  begin
    v:= TWebSessionMemory.Create(SessionID, 600000);
    v.Items[name] := value;
    SessionList.Add(SessionID, v);
  end else
    v.Items[name] := value;




end;

end.
