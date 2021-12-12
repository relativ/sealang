unit Socket;

interface

uses SysUtils ,Classes, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdIOHandler, IdGlobal, IdUDPBase,
  IdUDPClient, IdHTTP, IdMultipartFormData, IdMessageClient, IdSMTPBase, IdSMTP,
  IdMessage, IdEMailAddress, IdAttachment, IdAttachmentFile,
  IdFTP, IdFTPCommon, IdTCPServer, IdContext, IdCustomTCPServer;

type

  TSocketIOHandler = class(TObject)
  private
    FIOHandler: TIdIOHandler;
  public
    procedure SetIOHandler(IOHandler: TIdIOHandler);
    procedure Write(Value: string);
    procedure WriteLn(Value: string);
    procedure WriteStream(Value: TStream; ASize: integer);
    procedure ReadStream(AStream: TStream; AByteCount: integer = -1);
    function ReadLn: string;
  end;

  TSocketIdTCPConnection = class(TObject)
  private
    FIdTCPConnection: TIdTCPConnection;
    FSocketIOHandler: TSocketIOHandler;

    function GetIP: string;
    function GetPeerIP: string;
    function GetPeerPort: integer;
    function GetPort: integer;
  public
    destructor Destroy; override;
    procedure SetSocketIOHandler(const Value: TSocketIOHandler);
    function Connected: Boolean;
    property IOHandler: TSocketIOHandler read FSocketIOHandler write SetSocketIOHandler;
    property PeerIP: string read GetPeerIP;
    property PeerPort: integer read GetPeerPort;
    property IP: string read GetIP;
    property Port: integer read GetPort;
  end;

  TSocketIdContext = class(TObject)
  private
    FIdContext: TIdContext;
    FSocketIdTCPConnection: TSocketIdTCPConnection;
  public
    destructor Destroy; override;
    procedure SetIdContext(IdContext: TIdContext);
    procedure RemoveFromList;
    property Connection: TSocketIdTCPConnection read FSocketIdTCPConnection;

  end;

  TSocketIdServerThreadExceptionEvent = procedure(AContext: TSocketIdContext; AException: string) of object;
  TSocketIdServerThreadEvent = procedure(AContext: TSocketIdContext) of object;

  TTCPServer = class(TObject)
  private
    FIdTCPServer: TIdTcpServer;
    FOnExecute: TSocketIdServerThreadEvent;
    FOnConnect: TSocketIdServerThreadEvent;
    FOnDisconnect: TSocketIdServerThreadEvent;
    FOnException: TSocketIdServerThreadExceptionEvent;
    function GetActive: boolean;
    function GetDefaultPort: integer;
    function GetListenQueue: integer;
    function GetMaxConnections: integer;
    procedure SetActive(val: boolean);
    procedure SetDefaultPort(val: integer);
    procedure SetListenQueue(val: integer);
    procedure SetMaxConnections(val: integer);

    procedure IdTCPServerOnExecute(AContext: TIdContext);
    procedure IdTCPServerOnConnect(AContext: TIdContext);
    procedure IdTCPServerOnDisconnect(AContext: TIdContext);
    procedure IdTCPServerOnException(AContext: TIdContext; AException: Exception);

  public
    constructor Create;
    destructor Destroy; override;
    property Active: Boolean read GetActive write SetActive;
    property DefaultPort: integer read GetDefaultPort write SetDefaultPort;
    property ListenQueue: integer read GetListenQueue write SetListenQueue;
    property MaxConnections: Integer read GetMaxConnections write SetMaxConnections;

    property OnExecute: TSocketIdServerThreadEvent read FOnExecute write FOnExecute;
    property OnConnect: TSocketIdServerThreadEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TSocketIdServerThreadEvent read FOnDisconnect write FOnDisconnect;
    property OnException: TSocketIdServerThreadExceptionEvent read FOnException write FOnException;
  end;

  TTCPClient = class(TObject)
  private
    FSocketIOHandler: TSocketIOHandler;
    IdTCPClient: TIdTCPClient;
    function GetBoundIP: string;
    function GetBoundPort: integer;
    procedure SetBoundIP(const Value: string);
    procedure SetBoundPort(const Value: integer);
    function GetReadTimeout: Integer;
    procedure SetReadTimeout(const Value: Integer);
    function GetHost: string;
    function GetPort: integer;
    procedure SetHost(const Value: string);
    procedure SetPort(const Value: integer);
    function GetIdIOHandler: TSocketIOHandler;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Connect;
    procedure Disconnect;
    function Connected: boolean;

    property BoundIP: string read GetBoundIP write SetBoundIP;
    property BoundPort: integer read GetBoundPort write SetBoundPort;
    property ReadTimeout: Integer read GetReadTimeout write SetReadTimeout;
    property Host: string read GetHost write SetHost;
    property Port: integer read GetPort write SetPort;
    property IOHandler: TSocketIOHandler read GetIdIOHandler;
  end;

  TUDPClient = class(TObject)
  private
    IdUDPClient: TIdUDPClient;
    function GetHost: string;
    function GetPort: integer;
    function GetReceiveTimeout: Integer;
    procedure SetHost(const Value: string);
    procedure SetPort(const Value: integer);
    procedure SetReceiveTimeout(const Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
    property ReceiveTimeout: Integer read GetReceiveTimeout write SetReceiveTimeout;
    property Host: string read GetHost write SetHost;
    property Port: integer read GetPort write SetPort;

    procedure Send(const AData: string);
    procedure SendBuffer(const ABuffer: TIdBytes);
    function ReceiveBuffer(var ABuffer : TIdBytes;
                              const AMSec: Integer = IdTimeoutDefault): Integer;
    function ReceiveString(): string;
  end;

  //di�er propertyler entegre edilecek
  THttpClient = class(TObject)
  private
    IdHTTP: TIdHTTP;
  public
    constructor Create;
    destructor Destroy; override;

    function Delete(AURL: string): string;
    function Options(AURL: string): string;
    function Trace(AURL: string): string;
    procedure Head(AURL: string);
    function Post(AURL: string; ASource: TStrings): string;
    function PostFile(AURL: string; AFormData, AFileList: TStrings): string;
    function Put(AURL: string; ASource: TStream): string;
    function Patch(AURL: string; ASource: TStream): string;
    function Get(AURL: string): string;
    function GetFile(AURL: string): TStream;
  end;

  TEmail = class(TObject)
  private

  public
     class procedure SendMail(const host, username, password, subject, from: string; port: integer; ato: array of string;
                          messages: TStringList; attachments: array of string);
  end;


  TOnStatusEvent = procedure(ASender: TObject; AStatusText: string) of object;



  TFTP = class(TObject)
  private
    FIdFtp: TIdFTP;
    FOnFTPStatus: TOnStatusEvent;
    function GetHost: string;
    procedure SetHost(Value: string);
    function GetPassword: string;
    procedure SetPassword(Value: string);
    function GetUsername: string;
    procedure SetUsername(Value: string);
    function GetListResult: TStrings;
    function GetPort: integer;
    procedure SetPort(Value: integer);
    function GetPassive: boolean;
    procedure SetPassive(Value: boolean);
    procedure OnStatusEvent(ASender: TObject; const AStatus: TIdStatus;
      const AStatusText: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Abort;
    procedure ChangeDir(ADirname: string);
    procedure ChangeDirUp;
    procedure Connect;
    procedure Delete(AFilename: string);
    procedure Get(Afilename: string; ADest: TStream);
    procedure List;
    procedure Login;
    procedure MakeDir(ADirname: string);
    procedure Put(AsourceFile: string; ADestFile: string);
    procedure PutStream(ASource: TStream; ADeestFile: string);
    procedure RemoveDir(ADirname: string);
    procedure Rename(AsourceFile: string; AdestFile: string);
    procedure Status(AStatus: TStrings);
    function Connected: boolean;
    procedure Disconnect;
    function Size(AFilename: string): integer;
    property Host: string read GetHost write SetHost;
    property Passive: boolean read GetPassive write SetPassive;
    property Password: string read GetPassword write SetPassword;
    property Username: string read GetUsername write SetUsername;
    property Port: integer read GetPort write SetPort;
    property ListResult: TStrings read GetListResult;
    property OnStatus: TOnStatusEvent read FOnFTPStatus write FOnFTPStatus;
  end;








implementation


{ TTCPClient }

procedure TTCPClient.Connect;
begin
  IdTCPClient.Connect;
  if IdTCPClient.Connected then
    FSocketIOHandler.SetIOHandler(IdTCPClient.IOHandler);
end;

procedure TTCPClient.Disconnect;
begin
  IdTCPClient.Disconnect;
end;

function TTCPClient.Connected: boolean;
begin
  Result := IdTCPClient.Connected;
end;

constructor TTCPClient.Create;
begin
  IdTCPClient := TIdTCPClient.Create(nil);
  FSocketIOHandler:= TSocketIOHandler.Create;

end;

destructor TTCPClient.Destroy;
begin
  FSocketIOHandler.Free;
  IdTCPClient.Free;
  inherited;
end;

function TTCPClient.GetBoundIP: string;
begin
  Result := IdTCPClient.BoundIP;
end;

function TTCPClient.GetBoundPort: integer;
begin
    Result := IdTCPClient.BoundPort;
end;

function TTCPClient.GetHost: string;
begin
  Result := IdTCPClient.Host;
end;

function TTCPClient.GetIdIOHandler: TSocketIOHandler;
begin
  Result := FSocketIOHandler;
end;

function TTCPClient.GetPort: integer;
begin
  Result := IdTCPClient.Port;
end;

function TTCPClient.GetReadTimeout: Integer;
begin
  Result := IdTCPClient.ReadTimeout;
end;

procedure TTCPClient.SetBoundIP(const Value: string);
begin
    IdTCPClient.BoundIP := Value;
end;

procedure TTCPClient.SetBoundPort(const Value: integer);
begin
    IdTCPClient.BoundPort := Value;
end;

procedure TTCPClient.SetHost(const Value: string);
begin
  IdTCPClient.Host := Value;
end;

procedure TTCPClient.SetPort(const Value: integer);
begin
  IdTCPClient.Port := Value;
end;

procedure TTCPClient.SetReadTimeout(const Value: Integer);
begin
  IdTCPClient.ReadTimeout := Value;
end;

{ TSocketIOHandler }

function TSocketIOHandler.ReadLn: string;
begin
  Result := FIOHandler.ReadLn(IndyTextEncoding_UTF8);
end;

procedure TSocketIOHandler.ReadStream(AStream: TStream; AByteCount: integer = -1);
begin
  FIOHandler.ReadStream(AStream, AByteCount);
end;

procedure TSocketIOHandler.SetIOHandler(IOHandler: TIdIOHandler);
begin
  FIOHandler := IOHandler;
  FIOHandler.DefStringEncoding := IndyTextEncoding_UTF8;
end;

procedure TSocketIOHandler.WriteStream(Value: TStream; ASize: integer);
begin
  FIOHandler.Write(Value, ASize);
end;

procedure TSocketIOHandler.Write(Value: string);
begin
  FIOHandler.Write(Value, IndyUTF8Encoding);
end;

procedure TSocketIOHandler.WriteLn(Value: string);
begin
  FIOHandler.WriteLn(Value, IndyUTF8Encoding);
end;

{ TUDPClient }

procedure TUDPClient.Connect;
begin
  IdUDPClient.Connect;
end;

constructor TUDPClient.Create;
begin
  IdUDPClient := TIdUDPClient.Create(nil);
end;

destructor TUDPClient.Destroy;
begin
  IdUDPClient.Free;
  inherited;
end;

procedure TUDPClient.Disconnect;
begin
  IdUDPClient.Disconnect;
end;

function TUDPClient.GetHost: string;
begin
  Result := IdUDPClient.Host;
end;

function TUDPClient.GetPort: integer;
begin
  Result := IdUDPClient.Port;
end;

function TUDPClient.GetReceiveTimeout: Integer;
begin
  Result := IdUDPClient.ReceiveTimeout
end;

function TUDPClient.ReceiveBuffer(var ABuffer: TIdBytes;
  const AMSec: Integer): Integer;
begin
  Result := IdUDPClient.ReceiveBuffer(ABuffer, AMSec);
end;

function TUDPClient.ReceiveString: string;
begin
  Result := IdUDPClient.ReceiveString(-1, IndyTextEncoding_UTF8);
end;

procedure TUDPClient.Send(const AData: string);
begin
  IdUDPClient.Send(AData, IndyTextEncoding_UTF8);
end;

procedure TUDPClient.SendBuffer(const ABuffer: TIdBytes);
begin
  IdUDPClient.SendBuffer(ABuffer);
end;

procedure TUDPClient.SetHost(const Value: string);
begin
  IdUDPClient.Host := Value;
end;

procedure TUDPClient.SetPort(const Value: integer);
begin
  IdUDPClient.Port := Value;
end;

procedure TUDPClient.SetReceiveTimeout(const Value: Integer);
begin
  IdUDPClient.ReceiveTimeout := Value;
end;

{ THttpClient }

constructor THttpClient.Create;
begin
  IdHTTP := TIdHTTP.Create(nil);
end;

function THttpClient.Delete(AURL: string): string;
begin
  Result := IdHTTP.Delete(AURL);
end;

destructor THttpClient.Destroy;
begin
  IdHTTP.Free;
  inherited;
end;

function THttpClient.Get(AURL: string): string;
begin
  Result := IdHTTP.Get(AURL);
end;

function THttpClient.GetFile(AURL: string): TStream;
var
  ms: TMemoryStream;
begin
  ms:= TMemoryStream.Create;
  ms.Position := 0;
  IdHTTP.Get(aURL, ms);
  Result:= ms;
end;

procedure THttpClient.Head(AURL: string);
begin
  IdHTTP.Head(AURL);
end;

function THttpClient.Options(AURL: string): string;
begin
  Result := IdHTTP.Options(AURL);
end;

function THttpClient.Patch(AURL: string; ASource: TStream): string;
begin
  Result := IdHTTP.Patch(AURL, ASource);
end;

function THttpClient.Post(AURL: string; ASource: TStrings): string;
begin
  Result := IdHTTP.Post(AURL, ASource);
end;

function THttpClient.PostFile(AURL: string; AFormData, AFileList: TStrings): string;
var
  Data: TIdMultiPartFormDataStream;
  I: Integer;
begin
  Data:= TIdMultiPartFormDataStream.Create();
  try

    for I := 0 to AFormData.Count-1 do
    begin
      Data.AddFormField(AFormData.Names[I], AFormData.Values[AFormData.Names[I]]);
    end;

    for I := 0 to AFileList.Count-1 do
    begin
      Data.AddFile(AFileList.Names[I], AFileList.Values[AFileList.Names[I]]);
    end;

    Result := IdHTTP.Post(AURL, Data);
  finally
    Data.Free;
  end;

end;

function THttpClient.Put(AURL: string; ASource: TStream): string;
begin
  Result := IdHTTP.Put(AURL, ASource);
end;

function THttpClient.Trace(AURL: string): string;
begin
  Result := IdHTTP.Trace(AURL);
end;

{ TEmail }

class procedure TEmail.SendMail(const host, username, password, subject, from: string; port: integer; ato: array of string;
  messages: TStringList; attachments: array of string);
var
  SMTP: TIdSMTP;
  Msg: TIdMessage;
  I: Integer;
  eAI: TIdEMailAddressItem;
begin
  Msg := TIdMessage.Create(nil);
  try
    Msg.From.Address := from;
    for I := Low(ato) to High(ato) do
    begin
      eAI := Msg.Recipients.Add;
      eAI.Address := ato[I];
    end;
    Msg.Body.Text := messages.Text;
    for I := Low(attachments) to High(attachments) do
    begin
      TIdAttachmentFile.Create(Msg.MessageParts, attachments[I]);
    end;
    Msg.Subject := subject;
    SMTP := TIdSMTP.Create(nil);
    try
      SMTP.Host := host;
      SMTP.Port := port;
      SMTP.AuthType := satDefault;
      SMTP.Username := username;
      SMTP.Password := password;
      SMTP.Connect;
      SMTP.Send(Msg);
    finally
      SMTP.Free;
    end;
  finally
    Msg.Free;
  end;
end;

{ TFTP }

procedure TFTP.Abort;
begin
  try FIdFtp.Abort; except end;
end;

procedure TFTP.ChangeDir(ADirname: string);
begin
  try FIdFtp.ChangeDir(ADirname);  except end;

end;

procedure TFTP.ChangeDirUp;
begin
  try FIdFtp.ChangeDirUp;  except end;
end;

procedure TFTP.Connect;
begin
  try FIdFtp.Connect; FIdFtp.AUTHCmd := tAuthTLS; except end;
end;

function TFTP.Connected: boolean;
begin
    Result := FIdFtp.Connected;
end;

procedure TFTP.OnStatusEvent(ASender: TObject; const AStatus: TIdStatus;
   const AStatusText: string);
begin
  if Assigned(FOnFTPStatus) then
    FOnFTPStatus(Self, AStatusText);
end;

constructor TFTP.Create;
begin
  inherited;
  FIdFtp := TIdFTP.Create(nil);
  FIdFtp.OnStatus := OnStatusEvent;

end;

procedure TFTP.Delete(AFilename: string);
begin
  try FIdFtp.Delete(AFilename);  except end;
end;

destructor TFTP.Destroy;
begin
  FIdFtp.Free;
  inherited;
end;

procedure TFTP.Disconnect;
begin
  try FIdFtp.Disconnect;  except end;
end;

procedure TFTP.Get(Afilename: string; ADest: TStream);
begin
  try FIdFtp.Get(Afilename, ADest); except end;
end;

function TFTP.GetHost: string;
begin
  Result := FIdFtp.Host;
end;

function TFTP.GetListResult: TStrings;
begin
  Result := FIdFtp.ListResult;
end;

function TFTP.GetPassive: boolean;
begin
  Result := FIdFtp.Passive;
end;

function TFTP.GetPassword: string;
begin
  Result := FIdFtp.Password;
end;

function TFTP.GetPort: integer;
begin
  Result := FIdFtp.Port;
end;

function TFTP.GetUsername: string;
begin
  Result := FIdFtp.Username;
end;

procedure TFTP.List;
begin
  try FIdFtp.List; except end;
end;

procedure TFTP.Login;
begin
  try FIdFtp.Login; except end;
end;

procedure TFTP.MakeDir(ADirname: string);
begin
  try FIdFtp.MakeDir(ADirname); except end;
end;

procedure TFTP.Put(AsourceFile, ADestFile: string);
begin
  try FIdFtp.Put(AsourceFile, ADestFile); except end;
end;

procedure TFTP.PutStream(ASource: TStream; ADeestFile: string);
begin
    try FIdFtp.Put(ASource, ADeestFile, false, 0 ); except end;
end;

procedure TFTP.RemoveDir(ADirname: string);
begin
  try FIdFtp.RemoveDir(ADirname); except end;
end;

procedure TFTP.Rename(AsourceFile, AdestFile: string);
begin
  try FIdFtp.Rename(AsourceFile, AdestFile); except end;
end;

procedure TFTP.SetHost(Value: string);
begin
  FIdFtp.Host := Value;
end;

procedure TFTP.SetPassive(Value: boolean);
begin
  FIdFtp.Passive := true;
end;

procedure TFTP.SetPassword(Value: string);
begin
  FIdFtp.Password := Value;
end;

procedure TFTP.SetPort(Value: integer);
begin
  FIdFtp.Port := Value;
end;

procedure TFTP.SetUsername(Value: string);
begin
  FIdFtp.Username := Value;
end;

function TFTP.Size(AFilename: string): integer;
begin
  Result := FIdFtp.Size(AFilename);
end;

procedure TFTP.Status(AStatus: TStrings);
begin
  try FIdFtp.Status(AStatus); except end;
end;

{ TTCPServer }

constructor TTCPServer.Create;
begin
  FIdTCPServer := TIdTCPServer.Create(nil);
  FIdTCPServer.OnExecute := IdTCPServerOnExecute;
  FIdTCPServer.OnConnect := IdTCPServerOnConnect;
  FIdTCPServer.OnDisconnect := IdTCPServerOnDisconnect;
  FIdTCPServer.OnException := IdTCPServerOnException;
end;

destructor TTCPServer.Destroy;
begin
  FIdTCPServer.Active := false;
  FIdTCPServer.Free;
  inherited;
end;

procedure TTCPServer.IdTCPServerOnExecute(AContext: TIdContext);
var
  tmp: TSocketIdContext;
begin
  if Assigned(FOnExecute) then
  begin

    tmp:= TSocketIdContext.Create;
    tmp.SetIdContext(AContext);
    FOnExecute(tmp);
    tmp.Free;
  end;

end;

procedure TTCPServer.IdTCPServerOnConnect(AContext: TIdContext);
var
  tmp: TSocketIdContext;
begin
  if Assigned(FOnConnect) then
  begin
    AContext.Connection.IOHandler.DefStringEncoding := IndyUTF8Encoding;
    tmp:= TSocketIdContext.Create;
    tmp.SetIdContext(AContext);
    FOnConnect(tmp);
    tmp.Free;
  end;

end;

procedure TTCPServer.IdTCPServerOnDisconnect(AContext: TIdContext);
var
  tmp: TSocketIdContext;
begin
  if Assigned(FOnDisconnect) then
  begin
    tmp:= TSocketIdContext.Create;
    tmp.SetIdContext(AContext);
    FOnDisconnect(tmp);
    tmp.Free;
  end;

end;

procedure TTCPServer.IdTCPServerOnException(AContext: TIdContext; AException: Exception);
var
  tmp: TSocketIdContext;
begin
  if Assigned(FOnException) then
  begin
    tmp:= TSocketIdContext.Create;
    tmp.SetIdContext(AContext);
    FOnException(tmp, AException.Message);
    tmp.Free;
  end;

end;

function TTCPServer.GetActive: boolean;
begin
  Result := FIdTCPServer.Active;
end;

function TTCPServer.GetDefaultPort: integer;
begin
  Result := FIdTCPServer.DefaultPort;
end;

function TTCPServer.GetListenQueue: integer;
begin
  Result := FIdTCPServer.ListenQueue;
end;

function TTCPServer.GetMaxConnections: integer;
begin
  Result := FIdTCPServer.MaxConnections;
end;

procedure TTCPServer.SetActive(val: boolean);
begin
  FIdTCPServer.Active := val;
end;

procedure TTCPServer.SetDefaultPort(val: integer);
begin
  FIdTCPServer.DefaultPort := val;
end;

procedure TTCPServer.SetListenQueue(val: integer);
begin
  FIdTCPServer.ListenQueue := val;
end;

procedure TTCPServer.SetMaxConnections(val: integer);
begin
  FIdTCPServer.MaxConnections := val;
end;

{ TSocketIdContext }

destructor TSocketIdContext.Destroy;
begin
  if FSocketIdTCPConnection <> nil then
    FSocketIdTCPConnection.Free;
  inherited;
end;

procedure TSocketIdContext.RemoveFromList;
begin
  FIdContext.RemoveFromList;
end;

procedure TSocketIdContext.SetIdContext(IdContext: TIdContext);
var
  FIO: TSocketIOHandler;
begin
  FIdContext := IdContext;
  FIO := TSocketIOHandler.Create;
  FIO.SetIOHandler(IdContext.Connection.IOHandler);
  FSocketIdTCPConnection := TSocketIdTCPConnection.Create;
  FSocketIdTCPConnection.SetSocketIOHandler(FIO);
end;

{ TSocketIdTCPConnection }

function TSocketIdTCPConnection.Connected: Boolean;
begin
  Result := FIdTCPConnection.Connected;
end;

destructor TSocketIdTCPConnection.Destroy;
begin
  FSocketIOHandler.Free;
  inherited;
end;

function TSocketIdTCPConnection.GetIP: string;
begin
  Result := FIdTCPConnection.Socket.Binding.IP;
end;

function TSocketIdTCPConnection.GetPeerIP: string;
begin
  Result := FIdTCPConnection.Socket.Binding.PeerIP;
end;

function TSocketIdTCPConnection.GetPeerPort: integer;
begin
  Result := FIdTCPConnection.Socket.Binding.PeerPort;
end;

function TSocketIdTCPConnection.GetPort: integer;
begin
  Result := FIdTCPConnection.Socket.Binding.Port;
end;

procedure TSocketIdTCPConnection.SetSocketIOHandler(
  const Value: TSocketIOHandler);
begin
  FSocketIOHandler := Value;
end;

end.
