unit Socket;

interface

uses SysUtils ,Classes, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdIOHandler, IdGlobal, IdUDPBase,
  IdUDPClient, IdHTTP, IdMultipartFormData;

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

  //diðer propertyler entegre edilecek
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
  FIOHandler.Write(Value, IndyTextEncoding_UTF8);
end;

procedure TSocketIOHandler.WriteLn(Value: string);
begin
  Write(Value + chr(13));
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

end.
