unit SQLConnection;

interface

uses SysUtils ,Classes, DB,  DBAccess, Uni, MemDS,
  MongoDBUniProvider, SQLiteUniProvider, SQLServerUniProvider,
  PostgreSQLUniProvider, OracleUniProvider, NexusDBUniProvider,
  MySQLUniProvider, InterBaseUniProvider, DBFUniProvider, DB2UniProvider,
  ASEUniProvider, AdvantageUniProvider, UniProvider, ODBCUniProvider,
  AccessUniProvider;

type

  TSQLConnection = class(TObject)
    private
      SQLConnection: TUniConnection;
      function GetProviderName: string;
      procedure SetProviderName(val: string);
      function GetDatabase: string;
      function GetPassword: string;
      function GetServer: string;
      function GetUserName: string;
      procedure SetDatabase(const Value: string);
      procedure SetDriverName(val: string);
      procedure SetPassword(const Value: string);
      procedure SetServer(const Value: string);
      procedure SetUserName(const Value: string);
      function GetConnected: boolean;
      procedure SetConnected(const Value: boolean);
    public
      constructor Create;
      destructor Free;
      procedure Open();
      procedure Close();
      property ProviderName: string read GetProviderName write SetProviderName;
      property UserName: string read GetUserName write SetUserName;
      property Password: string read GetPassword write SetPassword;
      property Server: string read GetServer write SetServer;
      property Database: string read GetDatabase write SetDatabase;
      property Connected: boolean read GetConnected write SetConnected;
  end;

  TSQLQuery = class(TObject)
    private
      UniQuery: TUniQuery;
      FConnection: TSQLConnection;
      function GetActive: boolean;
      function GetSQL: TStrings;
      function GetSQLDelete: TStrings;
      function GetSQLInsert: TStrings;
      function GetSQLUpdate: TStrings;
      procedure SetActive(const Value: boolean);
      function GetConnection: TSQLConnection;
      procedure SetConnection(const Value: TSQLConnection);
    public
      constructor Create;
      destructor Free;
      procedure Open;
      procedure Close;
      procedure Next;
      procedure First;
      procedure Last;
      procedure Previous;
      procedure Append;
      procedure Edit;
      procedure Post;
      function Eof: boolean;
      function FieldByNameAsBoolean(FieldName: string): Boolean;
      function FieldByNameAsDateTime(FieldName: string): TDateTime;
      function FieldByNameAsFloat(FieldName: string): Double;
      function FieldByNameAsInteger(FieldName: string): Longint;
      function FieldByNameAsString(FieldName: string): String;
      property Active: boolean read GetActive write SetActive;
      property SQL: TStrings read GetSQL;
      property SQLDelete: TStrings read GetSQLDelete;
      property SQLInsert: TStrings read GetSQLInsert;
      property SQLUpdate: TStrings read GetSQLUpdate;
      property Connection: TSQLConnection read GetConnection write SetConnection;

  end;



implementation

{ TConnectionSQL }

procedure TSQLConnection.Close;
begin
  SQLConnection.Connected := false;
end;

constructor TSQLConnection.Create;
begin
  inherited;
  SQLConnection := TUniConnection.Create(nil);
  SQLConnection.LoginPrompt := false;
end;

destructor TSQLConnection.Free;
begin
  SQLConnection.Free;
end;

function TSQLConnection.GetConnected: boolean;
begin
  Result := SQLConnection.Connected;
end;

function TSQLConnection.GetDatabase: string;
begin
  Result := SQLConnection.Database;
end;

function TSQLConnection.GetPassword: string;
begin
  Result := SQLConnection.Password;
end;

function TSQLConnection.GetProviderName: string;
begin
  Result := SQLConnection.ProviderName;
end;

function TSQLConnection.GetServer: string;
begin
  Result := SQLConnection.Server;
end;

function TSQLConnection.GetUserName: string;
begin
  Result := SQLConnection.Username;
end;

procedure TSQLConnection.Open;
begin
  SQLConnection.Connected := true;
end;

procedure TSQLConnection.SetConnected(const Value: boolean);
begin
  SQLConnection.Connected := Value;
end;


procedure TSQLConnection.SetDatabase(const Value: string);
begin
  SQLConnection.Database := Value;
end;

procedure TSQLConnection.SetDriverName(val: string);
begin
  SQLConnection.ProviderName := val;
end;

procedure TSQLConnection.SetPassword(const Value: string);
begin
  SQLConnection.Password := Value;
end;

procedure TSQLConnection.SetProviderName(val: string);
begin
  SQLConnection.ProviderName := val;
end;

procedure TSQLConnection.SetServer(const Value: string);
begin
  SQLConnection.Server := Value;
end;

procedure TSQLConnection.SetUserName(const Value: string);
begin
  SQLConnection.Username := Value;
end;

{ TSQLQuery }

procedure TSQLQuery.Append;
begin
  UniQuery.Append;
end;

procedure TSQLQuery.Close;
begin
  UniQuery.Close;
end;

constructor TSQLQuery.Create;
begin
  UniQuery := TUniQuery.Create(nil);
end;

procedure TSQLQuery.Edit;
begin
  UniQuery.Edit;
end;

function TSQLQuery.Eof: boolean;
begin
  Result := UniQuery.Eof;
end;

function TSQLQuery.FieldByNameAsBoolean(FieldName: string): Boolean;
begin
  Result := UniQuery.FieldByName(FieldName).AsBoolean;
end;

function TSQLQuery.FieldByNameAsDateTime(FieldName: string): TDateTime;
begin
  Result := UniQuery.FieldByName(FieldName).AsDateTime;
end;

function TSQLQuery.FieldByNameAsFloat(FieldName: string): Double;
begin
  Result := UniQuery.FieldByName(FieldName).AsFloat;
end;

function TSQLQuery.FieldByNameAsInteger(FieldName: string): Longint;
begin
  Result := UniQuery.FieldByName(FieldName).AsInteger;
end;

function TSQLQuery.FieldByNameAsString(FieldName: string): String;
begin
  Result := UniQuery.FieldByName(FieldName).AsString;
end;

procedure TSQLQuery.First;
begin
  UniQuery.First;
end;

destructor TSQLQuery.Free;
begin
  UniQuery.Free;
end;

function TSQLQuery.GetActive: boolean;
begin
  Result := UniQuery.Active;
end;

function TSQLQuery.GetConnection: TSQLConnection;
begin
  Result := FConnection;
end;

function TSQLQuery.GetSQL: TStrings;
begin
  Result := UniQuery.SQL;
end;

function TSQLQuery.GetSQLDelete: TStrings;
begin
  Result := UniQuery.SQLDelete;
end;

function TSQLQuery.GetSQLInsert: TStrings;
begin
  Result := UniQuery.SQLInsert;
end;

function TSQLQuery.GetSQLUpdate: TStrings;
begin
  Result := UniQuery.SQLUpdate;
end;

procedure TSQLQuery.Last;
begin
  UniQuery.Last;
end;

procedure TSQLQuery.Next;
begin
  UniQuery.Next;
end;

procedure TSQLQuery.Open;
begin
  UniQuery.Open;
end;

procedure TSQLQuery.Post;
begin
  UniQuery.Post;
end;

procedure TSQLQuery.Previous;
begin
  UniQuery.Prior;
end;

procedure TSQLQuery.SetActive(const Value: boolean);
begin
  UniQuery.Active := Value;
end;

procedure TSQLQuery.SetConnection(const Value: TSQLConnection);
begin
  FConnection := Value;
  UniQuery.Connection := Value.SQLConnection;
end;



end.
