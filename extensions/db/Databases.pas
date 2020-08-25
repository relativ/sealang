unit Databases;

interface

uses SysUtils ,Classes,Data.DB, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Phys, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Stan.Param,
  FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.VCLUI.Error,
  FireDAC.VCLUI.Login, FireDAC.VCLUI.Async, FireDAC.VCLUI.Script,
  FireDAC.VCLUI.Wait, FireDAC.Phys.ODBCDef, FireDAC.Phys.MSAccDef,
  FireDAC.Phys.MySQLDef, FireDAC.Phys.ASADef, FireDAC.Phys.ADSDef,
  FireDAC.Phys.FBDef, FireDAC.Phys.PGDef, FireDAC.Phys.IBDef,
  FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteDef, FireDAC.Phys.OracleDef,
  FireDAC.Phys.DB2Def, FireDAC.Phys.InfxDef, FireDAC.Phys.MSSQLDef,
  FireDAC.Phys.TDataDef, FireDAC.Phys.MongoDBDef, IPPeerClient,
  FireDAC.Phys.DSDef, FireDAC.Phys.TDBXDef, FireDAC.Phys.TDBX,
  FireDAC.Phys.TDBXBase, FireDAC.Phys.DS, FireDAC.Phys.MongoDB,
  FireDAC.Phys.TData, FireDAC.Phys.MSSQL, FireDAC.Phys.Infx, FireDAC.Phys.DB2,
  FireDAC.Phys.Oracle, FireDAC.Phys.SQLite, FireDAC.Phys.IB, FireDAC.Phys.PG,
  FireDAC.Phys.IBBase, FireDAC.Phys.FB, FireDAC.Phys.ADS, FireDAC.Phys.ASA,
  FireDAC.Phys.MySQL, FireDAC.Phys.MSAcc, FireDAC.Phys.ODBCBase,
  FireDAC.Phys.ODBC, FireDAC.Stan.StorageJSON, FireDAC.Stan.StorageXML,
  FireDAC.Stan.StorageBin, FireDAC.Moni.FlatFile, FireDAC.Moni.Custom,
  FireDAC.Moni.Base, FireDAC.Moni.RemoteClient, FireDAC.Comp.UI,
  FireDAC.Comp.Client, FireDAC.Phys.SQLiteVDataSet, FireDAC.Comp.DataSet,
  Data.SqlTimSt, RTTI;

type

  TDBConnection = class(TObject)
    private
      SQLConnection: TFDConnection;
      function GetProviderName: string;
      procedure SetProviderName(const Value: string);
      function GetDatabase: string;
      function GetPassword: string;
      function GetServer: string;
      function GetUserName: string;
      procedure SetDatabase(const Value: string);
      procedure SetDriverName(const Value: string);
      procedure SetPassword(const Value: string);
      procedure SetServer(const Value: string);
      procedure SetUserName(const Value: string);
      function GetConnected: boolean;
      procedure SetConnected(const Value: boolean);
      function GetPort: integer;
      procedure SetPort(const Value: integer);
      function GetParams: TStringList;
    public
      constructor Create;
      destructor Destroy; override;
      procedure Open();
      procedure Close();
      property ProviderName: string read GetProviderName write SetProviderName;
      property UserName: string read GetUserName write SetUserName;
      property Password: string read GetPassword write SetPassword;
      property Server: string read GetServer write SetServer;
      property Port: integer read GetPort write SetPort;
      property Database: string read GetDatabase write SetDatabase;
      property Connected: boolean read GetConnected write SetConnected;
      property Params: TStringList read GetParams;
  end;

  TFieldNode = class(TObject)
  private
    FField: TField;
    function GetCalculated: Boolean;
    function GetDisplayLabel: string;
    function GetDisplayName: string;
    function GetDisplayText: string;
    function GetDisplayWidth: Integer;
    function GetEditText: string;
    function GetFullName: string;
    function GetIndex: Integer;
    function GetIsIndexField: Boolean;
    function GetCurValue: Variant;
    function GetNewValue: Variant;
    function GetOldValue: Variant;
    procedure SetCalculated(Value: Boolean);
    procedure SetDisplayLabel(Value: string);
    procedure SetDisplayWidth(Value: Integer);
    procedure SetEditText(const Value: string);
    procedure SetFieldName(const Value: string);
    procedure SetIndex(Value: Integer);
    procedure SetKeyFields(const Value: string);
    procedure SetNewValue(const Value: Variant);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetOrigin(Value: string);
    procedure SetRequired(Value: boolean);

    function GetKeyFields: string;
    function GetOrigin: string;
    function GetReadOnly: boolean;
    function GetRequired: boolean;

  protected
    function GetAsBoolean: Boolean;
    function GetAsCurrency: Currency;
    function GetAsDateTime: TDateTime;
    function GetAsSingle: Single;
    function GetAsFloat: Double;
    function GetAsExtended: Extended;
    function GetAsInteger: Longint;
    function GetAsLargeInt: LongInt;
    function GetAsLongWord: LongWord;
    function GetAsString: string;
    function GetAsWideString: string;
    function GetAsAnsiString: AnsiString;
    function GetAsVariant: Variant;
    function GetCanModify: Boolean;
    function GetDataSize: Integer;
    function GetFieldNo: Integer;
    function GetHasConstraints: Boolean;
    function GetIsNull: Boolean;
    function GetSize: Integer;
    procedure SetAsBoolean(Value: Boolean);
    procedure SetAsCurrency(Value: Currency);
    procedure SetAsDateTime(Value: TDateTime);
    procedure SetAsSingle(Value: Single);
    procedure SetAsFloat(Value: Double);
    procedure SetAsExtended(Value: Extended);
    procedure SetAsInteger(Value: Longint);
    procedure SetAsLargeInt(Value: LongInt);
    procedure SetAsLongWord(Value: LongWord);
    procedure SetAsString(const Value: string);
    procedure SetAsWideString(const Value: string);
    procedure SetAsAnsiString(const Value: AnsiString);
    procedure SetAsVariant(const Value: Variant);

    procedure SetSize(Value: Integer);
    procedure SetText(const Value: string);

    function GetAttributeSet: string;
    procedure SetAttributeSet(Value: string);

    function GetDataType: string;

    function GetFieldName: string;
  public
    constructor Create();
    destructor Destroy; override;
    procedure Clear;
    procedure FocusControl;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsSingle: Single read GetAsSingle write SetAsSingle;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsExtended: Extended read GetAsExtended write SetAsExtended;
    property AsInteger: Longint read GetAsInteger write SetAsInteger;
    property AsLargeInt: LongInt read GetAsLargeInt write SetAsLargeInt;
    property AsLongWord: LongWord read GetAsLongWord write SetAsLongWord;
    property AsString: string read GetAsString write SetAsString;
    property AsWideString: string read GetAsWideString write SetAsWideString;
    property AsAnsiString: AnsiString read GetAsAnsiString write SetAsAnsiString;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
    property AttributeSet: string read GetAttributeSet write SetAttributeSet;
    property CurValue: Variant read GetCurValue;
    property DataSize: Integer read GetDataSize;
    property DataType: string read GetDataType;
    property FieldNo: Integer read GetFieldNo;
    property FullName: string read GetFullName;
    property IsIndexField: Boolean read GetIsIndexField;
    property IsNull: Boolean read GetIsNull;
    property NewValue: Variant read GetNewValue write SetNewValue;
    property OldValue: Variant read GetOldValue;
    property Size: Integer read GetSize write SetSize;
    property Text: string read GetEditText write SetEditText;
    property Value: Variant read GetAsVariant write SetAsVariant;
  published
    property DisplayLabel: string read GetDisplayLabel write SetDisplayLabel;
    property DisplayWidth: Integer read GetDisplayWidth write SetDisplayWidth;
    property FieldName: string read GetFieldName write SetFieldName;
    property HasConstraints: Boolean read GetHasConstraints;
    property Index: Integer read GetIndex write SetIndex;
    property KeyFields: string read GetKeyFields write SetKeyFields;
    property Origin: string read GetOrigin write SetOrigin;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property Required: Boolean read GetRequired write SetRequired;
  end;

  TFieldList = class(TObject)
  private
    FDataSet: TFDQuery;
  protected
    function GetCount: Integer;
    function GetDataset: TObject;
    function GetField(Index: Integer): TFieldNode;
    procedure SetField(Index: Integer; Value: TFieldNode);
  public
    constructor Create(ADataSet: TObject);
    destructor Destroy; override;
    procedure Add(Field: TFieldNode);
    procedure CheckFieldName(const FieldName: string);
    procedure CheckFieldNames(const FieldNames: string);
    procedure Clear;
    procedure ClearAutomatic;
    function FindField(const FieldName: string): TFieldNode;
    function FieldByName(const FieldName: string): TFieldNode;
    function FieldByNumber(FieldNo: Integer): TFieldNode;
    procedure GetFieldNames(List: TStrings);
    function IndexOf(Field: TFieldNode): Integer;
    procedure Remove(Field: TFieldNode);
    property Dataset: TObject read GetDataset;
    property Count: Integer read GetCount;
    property Fields[Index: Integer]: TFieldNode read GetField write SetField;
  end;

  TDBQuery = class(TObject)
    private
      UniQuery: TFDQuery;
      FConnection: TDBConnection;
      FFieldList: TFieldList;
      function GetActive: boolean;
      function GetSQL: TStrings;
      procedure SetActive(const Value: boolean);
      function GetConnection: TDBConnection;
      procedure SetConnection(const Value: TDBConnection);
    public
      constructor Create;
      destructor Destroy; override;
      procedure Open;
      procedure Close;
      procedure Next;
      procedure First;
      procedure Last;
      procedure Previous;
      procedure Append;
      procedure Edit;
      procedure Post;
      procedure ExecSQL;
      function Eof: boolean;
      function FieldByNameAsBoolean(FieldName: string): Boolean;
      function FieldByNameAsDateTime(FieldName: string): TDateTime;
      function FieldByNameAsFloat(FieldName: string): Double;
      function FieldByNameAsInteger(FieldName: string): Longint;
      function FieldByNameAsString(FieldName: string): String;
      function FieldByName(const FieldName: string): TFieldNode;
      property Active: boolean read GetActive write SetActive;

      property Connection: TDBConnection read GetConnection write SetConnection;
      property SQL: TStrings read GetSQL;
      property Fields: TFieldList read FFieldList write FFieldList;

  end;



implementation

{ TConnectionSQL }

procedure TDBConnection.Close;
begin
  SQLConnection.Connected := false;
end;

constructor TDBConnection.Create;
begin
  inherited;
  SQLConnection := TFDConnection.Create(nil);
  SQLConnection.LoginPrompt := false;
end;

destructor TDBConnection.Destroy;
begin
  SQLConnection.Free;
  inherited;
end;

function TDBConnection.GetConnected: boolean;
begin
  Result := SQLConnection.Connected;
end;

function TDBConnection.GetDatabase: string;
begin
  Result := SQLConnection.Params.Database;
end;

function TDBConnection.GetParams: TStringList;
begin
  Result := SQLConnection.Params;
end;

function TDBConnection.GetPassword: string;
begin
  Result := SQLConnection.Params.Password;
end;

function TDBConnection.GetPort: integer;
begin
  Result := 0;
  TryStrToInt(SQLConnection.Params.Values['Port'], Result);
end;

function TDBConnection.GetProviderName: string;
begin
  Result := SQLConnection.Params.DriverID;
end;

function TDBConnection.GetServer: string;
begin
  Result := SQLConnection.Params.Values['Server'];
end;

function TDBConnection.GetUserName: string;
begin
  Result := SQLConnection.Params.UserName;
end;

procedure TDBConnection.Open;
begin
  SQLConnection.Connected := true;
end;

procedure TDBConnection.SetConnected(const Value: boolean);
begin
  SQLConnection.Connected := Value;
end;


procedure TDBConnection.SetDatabase(const Value: string);
begin
  SQLConnection.Params.Database := Value;
end;

procedure TDBConnection.SetDriverName(const Value: string);
begin
  SQLConnection.Params.DriverID := Value;
end;

procedure TDBConnection.SetPassword(const Value: string);
begin
  SQLConnection.Params.Password := Value;
end;

procedure TDBConnection.SetPort(const Value: integer);
begin
  SQLConnection.Params.Values['Port'] := IntToStr(Value);
end;

procedure TDBConnection.SetProviderName(const Value: string);
begin
  SQLConnection.DriverName := Value;
  SQLConnection.Params.DriverID := Value;
end;

procedure TDBConnection.SetServer(const Value: string);
begin
  SQLConnection.Params.Values['Server'] := Value;
end;

procedure TDBConnection.SetUserName(const Value: string);
begin
  SQLConnection.Params.UserName := Value;
end;

{ TDBQuery }

procedure TDBQuery.Append;
begin
  UniQuery.Append;
end;

procedure TDBQuery.Close;
begin
  UniQuery.Close;
end;

constructor TDBQuery.Create;
begin
  UniQuery := TFDQuery.Create(nil);
  FFieldList:= TFieldList.Create(UniQuery);
end;

procedure TDBQuery.Edit;
begin
  UniQuery.Edit;
end;

function TDBQuery.Eof: boolean;
begin
  Result := UniQuery.Eof;
end;

procedure TDBQuery.ExecSQL;
begin
  UniQuery.ExecSQL;
end;

function TDBQuery.FieldByName(const FieldName: string): TFieldNode;
begin
  Result := Fields.FieldByName(FieldName);
end;

function TDBQuery.FieldByNameAsBoolean(FieldName: string): Boolean;
begin
  Result := UniQuery.FieldByName(FieldName).AsBoolean;
end;

function TDBQuery.FieldByNameAsDateTime(FieldName: string): TDateTime;
begin
  Result := UniQuery.FieldByName(FieldName).AsDateTime;
end;

function TDBQuery.FieldByNameAsFloat(FieldName: string): Double;
begin
  Result := UniQuery.FieldByName(FieldName).AsFloat;
end;

function TDBQuery.FieldByNameAsInteger(FieldName: string): Longint;
begin
  Result := UniQuery.FieldByName(FieldName).AsInteger;
end;

function TDBQuery.FieldByNameAsString(FieldName: string): String;
begin
  Result := UniQuery.FieldByName(FieldName).AsString;
end;

procedure TDBQuery.First;
begin
  UniQuery.First;
end;

destructor TDBQuery.Destroy;
begin
  FFieldList.Free;
  UniQuery.Free;
  inherited;
end;

function TDBQuery.GetActive: boolean;
begin
  Result := UniQuery.Active;
end;

function TDBQuery.GetConnection: TDBConnection;
begin
  Result := FConnection;
end;

function TDBQuery.GetSQL: TStrings;
begin
  Result := UniQuery.SQL;
end;

procedure TDBQuery.Last;
begin
  UniQuery.Last;
end;

procedure TDBQuery.Next;
begin
  UniQuery.Next;
end;

procedure TDBQuery.Open;
begin
  UniQuery.Open;
end;

procedure TDBQuery.Post;
begin
  UniQuery.Post;
end;

procedure TDBQuery.Previous;
begin
  UniQuery.Prior;
end;

procedure TDBQuery.SetActive(const Value: boolean);
begin
  UniQuery.Active := Value;
end;

procedure TDBQuery.SetConnection(const Value: TDBConnection);
begin
  FConnection := Value;
  UniQuery.Connection := Value.SQLConnection;
end;



{ TFieldList }

procedure TFieldList.Add(Field: TFieldNode);
begin
  FDataSet.Fields.Add(Field.FField);
end;

procedure TFieldList.CheckFieldName(const FieldName: string);
begin
  FDataSet.Fields.CheckFieldName(FieldName);
end;

procedure TFieldList.CheckFieldNames(const FieldNames: string);
begin
  FDataSet.Fields.CheckFieldNames(FieldNames);
end;

procedure TFieldList.Clear;
begin
  FDataSet.Fields.Clear;
end;

procedure TFieldList.ClearAutomatic;
begin
  FDataSet.Fields.ClearAutomatic;
end;

constructor TFieldList.Create(ADataSet: TObject);
begin
  FDataSet := TFDQuery(ADataSet);
end;

destructor TFieldList.Destroy;
begin

  inherited;
end;

function TFieldList.FieldByName(const FieldName: string): TFieldNode;
var
  Field: TFieldNode;
begin
  Field:= TFieldNode.Create;
  Field.FField := FDataSet.Fields.FieldByName(FieldName);
  Result := Field;
end;

function TFieldList.FieldByNumber(FieldNo: Integer): TFieldNode;
var
  Field: TFieldNode;
begin
  Field:= TFieldNode.Create;
  Field.FField := FDataSet.Fields.FieldByNumber(FieldNo);
  Result := Field;
end;

function TFieldList.FindField(const FieldName: string): TFieldNode;
var
  Field: TFieldNode;
begin
  Field:= TFieldNode.Create;
  Field.FField := FDataSet.Fields.FindField(FieldName);
  Result := Field;
end;

function TFieldList.GetDataset: TObject;
begin
  Result := TObject(FDataSet);
end;

function TFieldList.GetCount: Integer;
begin
  Result := FDataSet.Fields.Count;
end;

function TFieldList.GetField(Index: Integer): TFieldNode;
var
  Field: TFieldNode;
begin
  Field:= TFieldNode.Create;
  Field.FField := FDataSet.Fields[Index];
  Result := Field;
end;

procedure TFieldList.GetFieldNames(List: TStrings);
begin
  FDataSet.Fields.GetFieldNames(List);
end;

function TFieldList.IndexOf(Field: TFieldNode): Integer;
begin
  Result := FDataSet.Fields.IndexOf(Field.FField);
end;

procedure TFieldList.Remove(Field: TFieldNode);
begin
  FDataSet.Fields.Remove(Field.FField);
end;

procedure TFieldList.SetField(Index: Integer; Value: TFieldNode);
begin
  FDataSet.Fields[Index] := Value.FField;
end;

{ TFieldNode }

procedure TFieldNode.Clear;
begin
  FField.Clear;
end;

constructor TFieldNode.Create;
begin
  //
end;

destructor TFieldNode.Destroy;
begin

  inherited;
end;

procedure TFieldNode.FocusControl;
begin
  FField.FocusControl;
end;

function TFieldNode.GetAsAnsiString: AnsiString;
begin
  Result := FField.AsAnsiString;
end;

function TFieldNode.GetAsBoolean: Boolean;
begin
  Result := FField.AsBoolean;
end;

function TFieldNode.GetAsCurrency: Currency;
begin
  Result := FField.AsCurrency;
end;

function TFieldNode.GetAsDateTime: TDateTime;
begin
  Result := FField.AsDateTime;
end;

function TFieldNode.GetAsExtended: Extended;
begin
  Result := FField.AsExtended;
end;

function TFieldNode.GetAsFloat: Double;
begin
  Result := FField.AsFloat;
end;

function TFieldNode.GetAsInteger: Longint;
begin
  Result := FField.AsInteger;
end;

function TFieldNode.GetAsLargeInt: LongInt;
begin
  Result := FField.AsLargeInt
end;

function TFieldNode.GetAsLongWord: LongWord;
begin
  Result := FField.AsLongWord;
end;

function TFieldNode.GetAsSingle: Single;
begin
  Result := FField.AsSingle;
end;

function TFieldNode.GetAsString: string;
begin
  Result :=  FField.AsString;
end;

function TFieldNode.GetAsVariant: Variant;
begin
  Result :=  FField.AsVariant;
end;

function TFieldNode.GetAsWideString: string;
begin
  Result :=  FField.AsWideString;
end;

function TFieldNode.GetAttributeSet: string;
begin
  Result :=  FField.AttributeSet;
end;

function TFieldNode.GetCalculated: Boolean;
begin
  Result := FField.Calculated;
end;

function TFieldNode.GetCanModify: Boolean;
begin
  Result := FField.CanModify;
end;

function TFieldNode.GetCurValue: Variant;
begin
  Result := FField.CurValue;
end;

function TFieldNode.GetDataSize: Integer;
begin
  Result := FField.DataSize;
end;

function TFieldNode.GetDataType: string;
begin
  Result := TRttiEnumerationType.GetName(FField.DataType);
end;

function TFieldNode.GetDisplayLabel: string;
begin
  Result := FField.DisplayLabel;
end;

function TFieldNode.GetDisplayName: string;
begin
    Result := FField.DisplayName;
end;

function TFieldNode.GetDisplayText: string;
begin
    Result := FField.DisplayText;
end;

function TFieldNode.GetDisplayWidth: Integer;
begin
    Result := FField.DisplayWidth;
end;

function TFieldNode.GetEditText: string;
begin
    Result := FField.Text;
end;

function TFieldNode.GetFieldName: string;
begin
    Result := FField.FieldName;
end;

function TFieldNode.GetFieldNo: Integer;
begin
    Result := FField.FieldNo;
end;

function TFieldNode.GetFullName: string;
begin
    Result := FField.FullName;
end;

function TFieldNode.GetHasConstraints: Boolean;
begin
    Result := FField.HasConstraints;
end;

function TFieldNode.GetIndex: Integer;
begin
    Result := FField.Index;
end;

function TFieldNode.GetIsIndexField: Boolean;
begin
  Result := FField.IsIndexField;
end;

function TFieldNode.GetIsNull: Boolean;
begin
  Result := FField.IsNull;
end;

function TFieldNode.GetKeyFields: string;
begin
  Result := FField.KeyFields;
end;

function TFieldNode.GetNewValue: Variant;
begin
  Result := FField.NewValue;
end;

function TFieldNode.GetOldValue: Variant;
begin
  Result := FField.OldValue;
end;

function TFieldNode.GetOrigin: string;
begin
  Result := FField.Origin;
end;

function TFieldNode.GetReadOnly: boolean;
begin
  Result := FField.ReadOnly;
end;

function TFieldNode.GetRequired: boolean;
begin
  Result := FField.Required;
end;

function TFieldNode.GetSize: Integer;
begin
  Result := FField.Size;
end;

procedure TFieldNode.SetAsAnsiString(const Value: AnsiString);
begin
  FField.AsAnsiString := Value;
end;

procedure TFieldNode.SetAsBoolean(Value: Boolean);
begin
  FField.AsBoolean := Value;
end;

procedure TFieldNode.SetAsCurrency(Value: Currency);
begin
  FField.AsCurrency := Value;
end;

procedure TFieldNode.SetAsDateTime(Value: TDateTime);
begin
  FField.AsDateTime := Value;
end;

procedure TFieldNode.SetAsExtended(Value: Extended);
begin
  FField.AsExtended := Value;
end;

procedure TFieldNode.SetAsFloat(Value: Double);
begin
  FField.AsFloat := Value;
end;

procedure TFieldNode.SetAsInteger(Value: Integer);
begin
  FField.AsInteger := Value;
end;

procedure TFieldNode.SetAsLargeInt(Value: LongInt);
begin
  FField.AsLargeInt := Value;
end;

procedure TFieldNode.SetAsLongWord(Value: LongWord);
begin
  FField.AsLongWord := Value;
end;

procedure TFieldNode.SetAsSingle(Value: Single);
begin
  FField.AsSingle := Value;
end;

procedure TFieldNode.SetAsString(const Value: string);
begin
  FField.AsString := Value;
end;

procedure TFieldNode.SetAsVariant(const Value: Variant);
begin
  FField.AsVariant := Value;
end;

procedure TFieldNode.SetAsWideString(const Value: string);
begin
  FField.AsWideString := Value;
end;

procedure TFieldNode.SetAttributeSet(Value: string);
begin
  FField.AttributeSet := Value;
end;

procedure TFieldNode.SetCalculated(Value: Boolean);
begin
  FField.Calculated := Value;
end;

procedure TFieldNode.SetDisplayLabel(Value: string);
begin
  FField.DisplayLabel := Value;
end;

procedure TFieldNode.SetDisplayWidth(Value: Integer);
begin
  FField.DisplayWidth := Value;
end;

procedure TFieldNode.SetEditText(const Value: string);
begin
  FField.Text := Value;
end;

procedure TFieldNode.SetFieldName(const Value: string);
begin
  FField.FieldName := Value;
end;

procedure TFieldNode.SetIndex(Value: Integer);
begin
  FField.Index := Value;
end;

procedure TFieldNode.SetKeyFields(const Value: string);
begin
  FField.KeyFields := Value;
end;

procedure TFieldNode.SetNewValue(const Value: Variant);
begin
  FField.NewValue := Value;
end;

procedure TFieldNode.SetOrigin(Value: string);
begin
  FField.Origin := Value;
end;

procedure TFieldNode.SetReadOnly(const Value: Boolean);
begin
  FField.ReadOnly := Value;
end;

procedure TFieldNode.SetRequired(Value: boolean);
begin
  FField.Required := Value;
end;

procedure TFieldNode.SetSize(Value: Integer);
begin
  FField.Size := Value;
end;

procedure TFieldNode.SetText(const Value: string);
begin
  FField.Text := Value;
end;

end.
