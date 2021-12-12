unit SessionUnit;

interface

uses
   SysUtils
  ,Classes
  ,MVCFramework.Session,
  System.Generics.Collections;


  type
    TSession = class(TObject)
    private
      FSessionID: string;
      FSocketList: TDictionary<string, pointer>;
    public
      constructor Create;
      destructor Destroy; override;
      function GetValue(name: string): string;
      function GetPointer(name: string): pointer;
      procedure SetValue(name: string; value: string);
      procedure SetPointer(name: string; value: pointer);
      procedure Delete(name: string);
      procedure SetSessionId(sessionId: string);

      property SessionID: string read FSessionID;

    end;

implementation


{ TSession }


constructor TSession.Create;
begin
  inherited;
  FSocketList:= TDictionary<string, pointer>.Create();
end;

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
  if SessionList.TryGetValue(SessionID, v) then
  begin
    Result := v.Items[name];
  end else Result := '';

end;

destructor TSession.Destroy;
begin
  FSocketList.Free;
  inherited;
end;

function TSession.GetPointer(name: string): pointer;
begin

  Result := FSocketList.Items[name]
end;

procedure TSession.SetSessionId(sessionId: string);
begin
  FSessionID := sessionId;
end;

procedure TSession.SetPointer(name: string; value: pointer);
var
  pTmp: pointer;
begin
  if FSocketList.TryGetValue(name, pTmp) then
    FSocketList.Items[name] := value
  else
    FSocketList.Add(name, value);
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
