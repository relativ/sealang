{*******************************************************}
{                                                       }
{               Delphi DBX Framework                    }
{                                                       }
{ Copyright(c) 1995-2015 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit SqlExpr;

{$R-,T-,H+,X+}
{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
  SysUtils,
  Classes,
  DB,
  DBCommon,
  DBCommonTypes,
  DBXCommon;

const
// eSQLTableType
    eSQLTable       = $0001;
    eSQLView        = $0002;
    eSQLSystemTable = $0004;
    eSQLSynonym     = $0008;
    eSQLTempTable   = $0010;
    eSQLLocal       = $0020;

// eSQLProcType
    eSQLProcedure   = $0001;
    eSQLFunction    = $0002;
    eSQLPackage     = $0004;
    eSQLSysProcedure = $0008;

// eSQLColType
    eSQLRowId       = $0001;
    eSQLRowVersion  = $0002;
    eSQLAutoIncr    = $0004;
    eSQLDefault     = $0008;

// eSQLIndexType
    eSQLNonUnique   = $0001;
    eSQLUnique      = $0002;
    eSQLPrimaryKey  = $0004;

  SSelect         =   'select';               { Do not localize }
  SSelectStar     =   ' select * ';           { Do not localize }
  SSelectStarFrom =   ' select * from ';      { Do not localize }
  SSelectSpaces   =   ' select ';             { Do not localize }
  SWhere          =   ' where ';              { Do not localize }
  SAnd            =   ' and ';                { Do not localize }
  SOrderBy        =   ' order by ';           { Do not localize }
  SParam          =   '?';                    { Do not localize }
  DefaultCursor   =   0;
  HourGlassCursor =   -11;

{ Default Max BlobSize }

  DefaultMaxBlobSize = -1;   // values are in K; -1 means retrieve actual size

{ Default RowsetSize }

  DefaultRowsetSize = DBXDefaultRowSetSize;

  TErrorMessageSize = 2048;

{ FieldType Mappings }
  FldTypeMap: TFieldMap = (
    TDBXDataTypes.UnknownType, TDBXDataTypes.AnsiStringType, TDBXDataTypes.Int16Type, TDBXDataTypes.Int32Type, TDBXDataTypes.UInt16Type, TDBXDataTypes.BooleanType, // 0..5
    // represent TDateTime as TSQLTimeStamp.
    TDBXDataTypes.DoubleType, TDBXDataTypes.DoubleType, TDBXDataTypes.CurrencyType, TDBXDataTypes.DateType, TDBXDataTypes.TimeType, TDBXDataTypes.TimeStampType, TDBXDataTypes.BytesType, // 6..12
    TDBXDataTypes.VarBytesType, TDBXDataTypes.Int32Type, TDBXDataTypes.BlobType, TDBXDataTypes.BlobType, TDBXDataTypes.BlobType, TDBXDataTypes.BlobType, TDBXDataTypes.BlobType, // 13..19
    TDBXDataTypes.BlobType, TDBXDataTypes.BlobType, TDBXDataTypes.CursorType, TDBXDataTypes.AnsiStringType, TDBXDataTypes.WideStringType, TDBXDataTypes.Int64Type, TDBXDataTypes.AdtType, // 20..26
    TDBXDataTypes.ArrayType, TDBXDataTypes.RefType, TDBXDataTypes.TableType, TDBXDataTypes.BlobType, TDBXDataTypes.BlobType, TDBXDataTypes.VariantType, TDBXDataTypes.UnknownType, // 27..33
    TDBXDataTypes.UnknownType, TDBXDataTypes.AnsiStringType, TDBXDataTypes.TimeStampType, TDBXDataTypes.BcdType, // 34..37
    TDBXDataTypes.WideStringType, TDBXDataTypes.BlobType, TDBXDataTypes.TimeStampType, TDBXDataTypes.AnsiStringType, // 38..41
    TDBXDataTypes.UnknownType, TDBXDataTypes.Int8Type, TDBXDataTypes.UInt8Type, TDBXDataTypes.UnknownType,  //42..45
    TDBXDataTypes.DBXConnectionType, TDBXDataTypes.TableType, TDBXDataTypes.BinaryBlobType, //46..48
    TDBXDataTypes.TimeStampOffsetType, TDBXDataTypes.JsonValueType, TDBXDataTypes.SingleType ); //49..51

  FldSubTypeMap: array[TFieldType] of Word = (
    0, 0, 0, 0, 0, 0, 0, TDBXDataTypes.MoneySubType, 0, 0, 0, 0, 0, 0, TDBXDataTypes.AutoIncSubType, // 0..14
//    TDBXTypes.SUB_TYPE_BINARY, TDBXTypes.SUB_TYPE_MEMO, TDBXTypes.SUB_TYPE_GRAPHIC, TDBXTypes.SUB_TYPE_FMTMEMO, TDBXTypes.SUB_TYPE_OLEOBJ, // 15..19
    TDBXDataTypes.BinarySubType, TDBXDataTypes.MemoSubType, 0, 0, 0, // 15..19
//    TDBXTypes.SUB_TYPE_DBSOLEOBJ, TDBXTypes.SUB_TYPE_TYPEDBINARY, 0, TDBXTypes.SUB_TYPE_FIXED, 0, // 20..24
    0, 0, 0, TDBXDataTypes.FixedSubType, 0, // 20..24
    0, 0, 0, 0, 0, TDBXDataTypes.HBinarySubType, TDBXDataTypes.HMemoSubType, 0, 0, 0, 0, 0, 0, // 24..37
    TDBXDataTypes.FixedSubType, TDBXDataTypes.WideMemoSubType, TDBXDataTypes.OracleTimeStampSubType, TDBXDataTypes.OracleIntervalSubType, // 38 ..41
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0); // 42..51

  /// maps the TDBXDataTypes into FldTypeMap
  DataTypeMap: array[0..TDBXDataTypes.MaxBaseTypes - 1] of TFieldType = (
    ftUnknown, ftString, ftDate, ftBlob, ftBoolean, ftSmallint,
    ftInteger, ftFloat, ftFMTBCD, ftBytes, ftTime, ftDateTime,
    ftWord, ftInteger, ftUnknown, ftVarBytes, ftUnknown, ftCursor,
    ftLargeInt, ftLargeInt, ftADT, ftArray, ftReference, ftDataSet,
    ftTimeStamp, ftBCD, ftWideString, ftSingle, ftShortint, ftByte, ftUnknown,
    ftUnknown, ftUnknown, ftUnknown, ftUnknown, ftVariant, ftTimeStampOffset, ftObject, 
    ftObject);

const
  SUB_TYPE_MEMO = TDBXDataTypes.MemoSubType;

  BlobTypeMap: array[SUB_TYPE_MEMO..TDBXDataTypes.BFileSubType] of TFieldType = (
    ftMemo, ftBlob, ftFmtMemo, ftParadoxOle, ftGraphic, ftDBaseOle,
    ftTypedBinary, ftBlob, ftBlob, ftBlob, ftWideMemo, ftOraClob, ftOraBlob,
    ftBlob, ftBlob);

type

  TFieldList    = TList;
  TLocale = Pointer;

  // Deprecated, use TDBXErrorCode;
  SQLResult      = TDBXErrorCode;

{ Forward declarations }

  TSQLConnection = class;
  TCustomSQLDataSet = class;
  TSQLDataSet = class;
  TSQLQuery = class;
  TSQLStoredProc = class;
  TSQLTable = class;

  TLocaleCode = Integer;

  TSQLExceptionType = (exceptConnection, exceptCommand, exceptCursor, exceptMetaData, exceptUseLast);


  TTransIsolationLevel = (xilREADCOMMITTED, xilREPEATABLEREAD, xilDIRTYREAD, xilCUSTOM);

  TTransactionDesc = record
    TransactionID    : LongWord;             { Transaction id }
    GlobalID         : LongWord;             { Global transaction id }
    IsolationLevel   : TTransIsolationLevel; {Transaction Isolation level}
    CustomIsolation  : LongWord;             { DB specific custom isolation }
  end;


  SPParamDesc = class                   { Stored Proc Descriptor }
  public
    iParamNum       : Word;             { Field number (1..n) }
    szName          : string;           { Field name }
    iArgType        : TParamType;       { Field type }
    iDataType       : TFieldType;       { Field type }
    iUnits1         : SmallInt;         { Number of Chars, digits etc }
    iUnits2         : SmallInt;         { Decimal places etc. }
    iLen            : LongWord;         { Length in bytes  }
  end;

{ TSQLBlobStream }

  TSQLBlobStream = class(TMemoryStream)
  private
    FDataSet: TCustomSQLDataSet;
    FField: TBlobField;
    FFieldNo: Integer;
    FHasData: Boolean;
  protected
    procedure ReadBlobSize;
  public
    constructor Create(Field: TBlobField; Mode: TBlobStreamMode = bmRead);
    destructor Destroy; override;
    procedure ReadBlobData;
    function Read(Buffer: TBytes; Offset, Count: Longint): Longint; overload; override;
  end;

  TConnectionUserType = (eUserMonitor, eUserDataSet);


{ TSQLMonitor }


  TTraceEvent = procedure(Sender: TObject; TraceInfo: TDBXTraceInfo; var LogTrace: Boolean) of object;
  TTraceLogEvent = procedure(Sender: TObject; TraceInfo: TDBXTraceInfo) of object;

  TSQLMonitor = class(TComponent)
  private
    FActive: Boolean;
    FAutoSave: Boolean;
    FFileName: string;
    FKeepConnection: Boolean;
    FMaxTraceCount: Integer;
    FOnTrace: TTraceEvent;
    FOnLogTrace: TTraceLogEvent;
    FSQLConnection: TSQLConnection;
    FStreamedActive: Boolean;
    FTraceFlags: TDBXTraceFlag;
    FTraceList: TStrings;
    procedure CheckInactive;
    function GetTraceCount: Integer;
  protected
    function InvokeCallBack(TraceInfo: TDBXTraceInfo): CBRType;
    procedure SetActive(Value: Boolean);
    procedure SetSQLConnection(Value: TSQLConnection);
    procedure SetStreamedActive;
    procedure SetTraceList(Value: TStrings);
    procedure SetFileName(const Value: string);
    procedure SwitchConnection(const Value: TSQLConnection);
    procedure Trace(TraceInfo: TDBXTraceInfo; LogTrace: Boolean); virtual;
    procedure UpdateTraceCallBack;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(AFileName: string);
    procedure SaveToFile(AFileName: string);
    property MaxTraceCount: Integer read FMaxTraceCount write FMaxTraceCount;
    property TraceCount: Integer read GetTraceCount;
  published
    property Active: Boolean read FActive write SetActive default False;
    property AutoSave: Boolean read FAutoSave write FAutoSave default False;
    property FileName: string read FFileName write SetFileName;
    property OnLogTrace: TTraceLogEvent read FOnLogTrace write FOnLogTrace;
    property OnTrace: TTraceEvent read FOnTrace write FOnTrace;
{   property TraceFlags not supported in DBExpress 1.0 }
    property TraceList: TStrings read FTraceList write SetTraceList stored False;
    property SQLConnection: TSQLConnection read FSQLConnection write SetSQLConnection;
  end;

{ TSQLConnection }


  EConnectFlag = (eConnect, eReconnect, eDisconnect);

  TSchemaType = (stNoSchema, stTables, stSysTables, stProcedures, stColumns,
    stProcedureParams, stIndexes, stPackages, stUserNames);

  TConnectionState = (csStateClosed, csStateOpen, csStateConnecting,
    csStateExecuting, csStateFetching, csStateDisconnecting);

  TTableScope = (tsSynonym, tsSysTable, tsTable, tsView);

  TTableScopes = set of TTableScope;

  TSQLConnectionLoginEvent = procedure(Database: TSQLConnection;
    LoginParams: TStrings) of object;


  TConnectionData = class(TPersistent)
  private
    FConnection: TSqlConnection;
    FParentData: TConnectionData;
    FProperties: TDBXProperties;
    FOriginalProperties: TDBXProperties;
    FConnectionName: string;
    FDriverName: string;
    FDelegateConnection: TConnectionData;
    FChanging: Boolean;
    FHasIsModified: Boolean;
    FIsModified: Boolean;
    procedure DoChange(Sender: TObject);
    function IsMyProperty(const PropertyName: string): Boolean;
    function GetPrefix: string;
    function GetConectionName: string;
    function GetProperties: TDBXProperties;
    procedure SetConnectionName(const Value: string);
    procedure SetProperties(const Value: TDBXProperties);
    procedure SetDelegateConnection(const Value: TConnectionData);
    function GetIsModified: Boolean;
    function GetOriginalProperties: TDBXProperties;
    function GetDriverName: string;
    procedure SetDriverName(const Value: string);
  protected
    procedure GetFullParams(Params: TStrings);
  public
    constructor Create(AConnection: TSQLConnection); overload;
    constructor Create(AParentData: TConnectionData); overload;
    procedure UpdateProperties(NewProperties: TStrings);
    procedure AddProperties(NewProperties: TStrings);
    function BeginUpdate: Boolean;
    procedure EndUpdate;
    procedure ReloadProperties;
    procedure RefreshProperties;
    destructor Destroy; override;
  published
    property ConnectionName: string read GetConectionName write SetConnectionName;
    property DriverName: string read GetDriverName write SetDriverName;
    property Properties: TDBXProperties read GetProperties write SetProperties;
    property DelegateConnection: TConnectionData read FDelegateConnection write SetDelegateConnection;
    property IsModified: Boolean read GetIsModified;
  end;

  TSQLConnection = class(TCustomConnection)
  private
    function BeginTransaction(TransDesc: TTransactionDesc; Isolation: TDBXIsolation): TDBXTransaction; overload;
  private
    FSelectStatements: LongWord;
    FActiveStatements: LongWord;
    FAutoClone: Boolean;
    FCloneParent: TSQLConnection;
    FConnectionState: TConnectionState;
    FConnectionName: string;
    FConnectionRegistryFile: string;
    FDriverName: string;
    FGetDriverFunc: string;
    FTransactionCount: Integer;
    FIsCloned: Boolean;
    FDBXConnection: TDBXConnection;
    FKeepConnection: Boolean;
    FLibraryName: string;
    FLoadParamsOnConnect: Boolean;
    FMonitorUsers: TList;
    FOnLogin: TSQLConnectionLoginEvent;
    FParams: TStrings;
    FParamsLoaded: Boolean;
    FMaxStmtsPerConn: LongWord;
    FQuoteChar: string;
    FProcedureQuoteChar: string;
    FDefaultSchemaName: string;
    FRefCount: Integer;
    FSQLDllHandle: THandle;
    FSQLHourGlass: Boolean;
    FSupportsNestedTrans: LongBool;
    FTableScope: TTableScopes;
    FTraceCallbackEvent: TDBXTraceEvent;
    FTransactionsSupported: LongBool;
    FVendorLib: string;
    FLoginUsername: string;
//    FConnectionFactory: TConnectionFactory;
              
    FDefaultSchema: string;
    FConnectionData: TConnectionData;
    FUniqueID: string;
    FValidateCertificate: TValidateCertificate;
    procedure CheckActive;
    procedure CheckInactive;
    procedure CheckLoginParams;
    procedure ClearConnectionUsers;
    procedure ClearMonitors;
    procedure FreeSchemaTable(DataSet: TCustomSQLDataSet);
    function GetConnectionForStatement: TSQLConnection;
    function GetConnectionName: string;
    function GetLocaleCode: TLocaleCode;
    function GetInTransaction: Boolean;
    function GetLibraryName: string;
    procedure GetLoginParams(LoginParams: TStrings);
    function GetQuoteChar: string;
    function GetVendorLib: string;
    function GetMetaData: TDBXDatabaseMetaData;
    procedure Login(ConnectionProps: TStrings);
    function OpenSchemaTable(eKind: TSchemaType; SInfo: string; SQualifier: string = ''; SPackage: string = ''): TCustomSQLDataSet;overload;
    function OpenSchemaTable(eKind: TSchemaType; SInfo: string; SQualifier: string = ''; SPackage: string = ''; SSchemaName: string = ''): TCustomSQLDataSet;overload;
    procedure RegisterTraceMonitor(Client: TObject);
    procedure RegisterTraceCallback(Value: Boolean);
    procedure SetConnectionName(Value: string);
    procedure SetDriverName(Value: string);
    procedure SetKeepConnection(Value: Boolean);
    procedure SetParams(Value: TStrings);
    procedure SetCursor(CursorType: Integer);
    procedure SetLocaleCode(Value: TLocaleCode);
    procedure DoParamsChange(Sender: TObject);
//    function SQLTraceCallback(TraceInfo: Pointer): CBRType;
    procedure UnregisterTraceMonitor(Client: TObject);
{$IFNDEF NEXTGEN}
    procedure EndFreeAndNilTransaction(var Transaction: TDBXTransaction; Commit: Boolean); overload;
    procedure EndAndFreeTransaction(Commit: Boolean); overload;
    procedure EndAndFreeTransaction(TransDesc: TTransactionDesc; Commit: Boolean); overload;
{$ENDIF !NEXTGEN}
    procedure ReadUniqueId(Reader: TReader);
    procedure WriteUniqueId(Writer: TWriter);
    procedure ReadGetDriverFunc(Reader: TReader);
    procedure ReadLibraryName(Reader: TReader);
    procedure ReadVendorLib(Reader: TReader);
    property DefaultSchema: string read FDefaultSchema;
  protected
    procedure CheckConnection(eFlag: eConnectFlag);
    procedure CheckDisconnect; virtual;
    procedure ConnectionOptions; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoConnect; override;
    procedure DoDisconnect; override;
    function GetConnected: Boolean; override;
    function GetDataSet(Index: Integer): TCustomSQLDataSet; reintroduce;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure OpenSchema(eKind: TSchemaType; sInfo: string; List: TStrings); overload;
    procedure OpenSchema(eKind: TSchemaType; sInfo, SSchemaName: string; List: TStrings); overload;
    procedure RegisterClient(Client: TObject; Event: TConnectChangeEvent = nil); override;
    property ConnectionRegistryFile: string read FConnectionRegistryFile;
    property QuoteChar: string read FQuoteChar;
    property SQLDllHandle: THandle read FSQLDllHandle write FSQlDllHandle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddConnectNotification(Listener: TObject; Event: TConnectChangeEvent);
    procedure RemoveConnectNotification(Listener: TObject);
    function CloneConnection: TSQLConnection;
    procedure CloseDataSets;
    procedure Commit;

    procedure CommitFreeAndNil(var Transaction: TDBXTransaction);
    function Execute(const SQL: string; Params: TParams): Integer; overload;
    function Execute(const SQL: string; Params: TParams;
      var ResultSet: TDataSet): Integer; overload;
    function ExecuteDirect(const SQL: string): Integer;
    procedure GetFieldNames(const TableName: string; List: TStrings); overload;
    procedure GetFieldNames(const TableName: string; SchemaName: string; List: TStrings); overload;
    procedure GetIndexNames(const TableName: string; List: TStrings); overload;
    procedure GetIndexNames(const TableName, SchemaName: string; List: TStrings); overload;
    procedure GetProcedureNames(List: TStrings); overload;
    procedure GetProcedureNames(const PackageName: string; List: TStrings); overload;
    procedure GetProcedureNames(const PackageName, SchemaName: string; List: TStrings); overload;
    procedure GetPackageNames(List: TStrings); overload;
    procedure GetSchemaNames(List: TStrings); overload;
    procedure GetCommandTypes(List: TStrings);
    procedure GetServerMethodNames(List: TStrings);
    function GetDefaultSchemaName: string;
    procedure GetProcedureParams(ProcedureName : string; List: TProcParamList); overload;
    procedure GetProcedureParams(ProcedureName, PackageName: string; List: TProcParamList); overload;
    procedure GetProcedureParams(ProcedureName, PackageName, SchemaName: string; List: TProcParamList); overload;
    procedure GetTableNames(List: TStrings; SystemTables: Boolean = False); overload;
    procedure GetTableNames(List: TStrings; SchemaName: string; SystemTables: Boolean = False); overload;
    procedure LoadParamsFromIniFile( FFileName: string = '');
    procedure Rollback;
    procedure RollbackFreeAndNil(var Transaction: TDBXTransaction);
    procedure RollbackIncompleteFreeAndNil(var Transaction: TDBXTransaction);
    function  HasTransaction(Transaction:  TDBXTransaction): Boolean;
    procedure SetTraceEvent(Event: TDBXTraceEvent);
    function  BeginTransaction: TDBXTransaction; overload;
    function  BeginTransaction(Isolation: TDBXIsolation): TDBXTransaction; overload;
    function GetLoginUsername: string;
    property ActiveStatements: LongWord read FActiveStatements;
    property AutoClone: Boolean read FAutoClone write FAutoClone default True;
    property ConnectionState: TConnectionState read FConnectionState write FConnectionState;
    property DataSets[Index: Integer]: TCustomSQLDataSet read GetDataSet;
    property InTransaction: Boolean read GetInTransaction;
    property LocaleCode: TLocaleCode read GetLocaleCode write SetLocaleCode default 0;
    property MaxStmtsPerConn: LongWord read FMaxStmtsPerConn;
    property MetaData: TDBXDatabaseMetaData read GetMetaData;
    property MultipleTransactionsSupported: LongBool read FSupportsNestedTrans;
    property ParamsLoaded: Boolean read FParamsLoaded write FParamsLoaded;
    property DBXConnection: TDBXConnection read FDBXConnection;
    property SQLHourGlass: Boolean read FSQLHourGlass write FSQLHourGlass default True;
    property TraceCallbackEvent: TDBXTraceEvent read FTraceCallbackEvent;
    property TransactionsSupported: LongBool read FTransactionsSupported;
//    property Locale: TLocale read FLocale;
    property ConnectionData: TConnectionData read FConnectionData;
    property UniqueID: string read FUniqueID write FUniqueID;
    property GetDriverFunc: string read FGetDriverFunc write FGetDriverFunc;
    property LibraryName: string read GetLibraryName write FLibraryName;
    property VendorLib: string read GetVendorLib write FVendorLib;
  published
    property ConnectionName: string read GetConnectionName write SetConnectionName;
    property DriverName: string read FDriverName write SetDriverName;
    property KeepConnection: Boolean read FKeepConnection write SetKeepConnection default True;
    property LoadParamsOnConnect: Boolean read FLoadParamsOnConnect write FLoadParamsOnConnect default False;
    property LoginPrompt default True;
    property Params: TStrings read FParams write SetParams;
    property TableScope: TTableScopes read FTableScope write FTableScope default [tsTable, tsView];
    property ValidatePeerCertificate: TValidateCertificate read FValidateCertificate write FValidateCertificate;
    property AfterConnect;
    property AfterDisconnect;
    property BeforeConnect;
    property BeforeDisconnect;
    property OnLogin: TSQLConnectionLoginEvent read FOnLogin write FOnLogin;
    property Connected;
//    property ConnectionFactory: TConnectionFactory read FConnectionFactory write FConnectionFactory;
  end;

{ TSQLDataLink }

  TSQLDataLink = class(TDetailDataLink)
  private
    FSQLDataSet: TCustomSQLDataSet;
  protected
    procedure ActiveChanged; override;
    procedure CheckBrowseMode; override;
    function GetDetailDataSet: TDataSet; override;
    procedure RecordChanged(Field: TField); override;
  public
    constructor Create(ADataSet: TCustomSQLDataSet);
  end;

{ FLDDesc wrapper }

  TFLDDesc = class
  public
    FFldNum:          Word;             { Field number (1..n) }
    FName:            string;           { Field name }
    FFldType:         Word;             { Field type }
    FSubType:         Word;             { Field subtype (if applicable) }
    FUnits1:          SmallInt;         { Number of Chars, digits etc }
    FUnits2:          SmallInt;         { Decimal places etc. }
    FOffset:          Word;             { Offset in the record (computed) }
    FLen:             LongWord;         { Length in bytes (computed) }
    FNullOffset:      Word;             { For Null bits (computed) }
    FFLDVchk:         FLDVchk;          { Field Has vcheck (computed) }
    FFLDRights:       FLDRights;        { Field Rights (computed) }
    FCalcField:       WordBool;         { Is Calculated field (computed) }
  public
    property iFldNum: Word read FFldNum write FFldNum;
    property szName: string read FName write FName;
    property iFldType: Word read FFldType write FFldType;
    property iSubType: Word read FSubType write FSubType;
    property iUnits1: SmallInt read FUnits1 write FUnits1;
    property iUnits2: SmallInt read FUnits2 write FUnits2;
    property iOffset: Word read FOffset write FOffset;
    property iLen: LongWord read FLen write FLen;
    property iNullOffset: Word read FNullOffset write FNullOffset;
    property efldvVchk: FLDVchk read FFLDVchk write FFLDVchk;
    property efldrRights: FLDRights read FFLDRights write FFLDRights;
    property bCalcField: WordBool read FCalcField write FCalcField;
  end;

{ TCustomSQLDataSet }

  TSQLSchemaInfo = record
    FType: TSchemaType;
    ObjectName: string;
    Pattern: string;
    PackageName : string;
  end;

  TFieldDescList = array of TFLDDesc;

  TParseSqlEvent = procedure(var FieldNames: TStrings; SQL: string;
      var TableName: string) of object;
  TParseInsertSqlEvent = procedure(var FieldNames: TStrings; SQL: string;
      var BindAllFields: Boolean; var TableName: string) of object;

  TCustomSQLDataSet = class(TDataSet)
  private
    FFieldBuffer:  array of byte;
    FRefreshing: Boolean;
  private
    FBlobBuffer: TBlobByteData;
    FCalcFieldsBuffer: array of byte;
    FCheckRowsAffected: Boolean;
    FClonedConnection: TSqlConnection;
    FCommandText: string;
    FCommandType: TSQLCommandType;
    FDbxCommandType: string;
    FCurrentBlobSize: Int64;
    FDataLink: TDataLink;
    FDesignerData: string;
    FGetNextRecordSet: Boolean;
    FProvidedDBXReader: Boolean;
    FOwnsProvidedDBXReader: Boolean;
    FIndexDefs: TIndexDefs;
    FIndexDefsLoaded: Boolean;
    FMaxBlobSize: Integer;
    FMaxColSize: LongWord;
    FNativeCommand: string;
    FGetMetadata: Boolean;
    FNumericMapping: Boolean;
    FParamCheck: Boolean;
    FParamCount: Integer;
    FParams: TParams;
    FPrepared: Boolean;
    FProcParams: TProcParamList;
    FRecords: Integer;
    FRowsAffected: Integer;
    FSchemaInfo: TSQLSchemaInfo;
    FParseSelectSql: TParseSqlEvent;
    FParseUpdateSql: TParseSqlEvent;
    FParseDeleteSql: TParseSqlEvent;
    FParseInsertSql: TParseInsertSqlEvent;
    FSortFieldNames: string;

    FDBXCommand: TDBXCommand;
    FSQLConnection: TSQLConnection;
    FDBXReader: TDBXReader;

    FStatementOpen: Boolean;
    FSchemaName: string;
    function CheckFieldNames(const FieldNames: string): Boolean;
    procedure CheckConnection(eFlag: eConnectFlag);
    function CheckDetail(const SQL: string): string; virtual;
    procedure CheckStatement(ForSchema: Boolean = False);
    function GetCalculatedField(Field: TField; var Buffer: TValueBuffer): Boolean;
    function GetDataSetFromSQL(TableName: string): TCustomSQLDataSet;
    function GetProcParams: TProcParamList;
    function GetInternalConnection: TSQLConnection;
    function GetObjectProcParamCount: Integer; virtual;
    function GetParamCount: Integer; virtual;
    function GetQueryFromType: string; virtual;
    function GetRowsAffected: Integer;
    function AddMetadataQuotes(Identifier: string; StoredProc: Boolean): string;
    function QuoteIdentifier(Identifier: string; StoredProc: Boolean): string;
    procedure InitBuffers;
    procedure LoadFieldDef(FieldID: Word; var FldDesc: TFLDDesc); overload;
    procedure ReadDesignerData(Reader: TReader);
    procedure RefreshParams;
    procedure SetConnection(const Value: TSQLConnection); virtual;
    procedure SetCurrentBlobSize(Value: Int64);
    procedure SetDataSource(Value: TDataSource);
    procedure SetParameters(const Value: TParams);
    procedure SetParamsFromProcedure;
    procedure SetParamsFromServerMethod;
    procedure SetParamsFromSQL(DataSet: TDataSet; bFromFields: Boolean);
    procedure SetPrepared(Value: Boolean);
    procedure SetCommandType(const Value: TSQLCommandType); virtual;
    procedure SetDbxCommandType(const Value: string); virtual;
    procedure WriteDesignerData(Writer: TWriter);
    procedure SetSchemaName(const Value: string);
    procedure SetSchemaOption(var ACatalogName, ASchemaName: string);
    procedure ParseIdentifier(Identifier: string; var Catalog, Schema, Name: string);
    procedure GetParamsOutputParam(Param: TParam; Value: TDBXValue);
    procedure GetDataSetOutputParam(Param: TParam; Value: TDBXValue);
    function IsDbxCommandTypeStored: Boolean;
  protected
    { IProviderSupport }
    procedure PSEndTransaction(Commit: Boolean); override;
    procedure PSExecute; override;
    function PSExecuteStatement(const ASQL: string; AParams: TParams): Integer; overload; override;
    function PSExecuteStatement(const ASQL: string; AParams: TParams;
      var ResultSet: TDataSet): Integer; overload; override;
    procedure PSGetAttributes(List: TPacketAttributeList); override;
    function PSGetDefaultOrder: TIndexDef; override;
    function PSGetKeyFields: string; override;
    function PSGetQuoteChar: string; override;
    function PSGetTableName: string; override;
    function PSGetIndexDefs(IndexTypes: TIndexOptions): TIndexDefs; override;
    function PSGetParams: TParams; override;
    function PSGetUpdateException(E: Exception; Prev: EUpdateError): EUpdateError; override;
    function PSInTransaction: Boolean; override;
    function PSIsSQLBased: Boolean; override;
    function PSIsSQLSupported: Boolean; override;
    procedure PSReset; override;
    procedure PSSetCommandText(const ACommandText: string); override;
    procedure PSSetParams(AParams: TParams); override;
    procedure PSStartTransaction; override;
    function PSUpdateRecord(UpdateKind: TUpdateKind; Delta: TDataSet): Boolean; override;
    function PSGetCommandText: string; override;
    function PSGetCommandType: TPSCommandType; override;
  protected
    { implementation of abstract TDataSet methods }
    procedure InternalClose; override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    function IsCursorOpen: Boolean; override;
  protected
    procedure AddFieldDesc(FieldDescs: TFieldDescList; DescNo: Integer;
        var FieldID: Integer; RequiredFields: TBits; FieldDefs: TFieldDefs);
    procedure AddIndexDefs(SourceDS: TCustomSQLDataSet; IndexName: string = '') ;
    procedure CheckPrepareError;
    procedure ClearIndexDefs;
    procedure CloseCursor; override;
    procedure CloseStatement;
    procedure DefineProperties(Filer: TFiler); override;
    function ExecSQL(ExecDirect: Boolean = False): Integer; virtual;
    procedure ExecuteStatement;
    procedure FreeReader;
    procedure FreeBuffers;
    procedure InternalFreeCommand;
    procedure FreeCommand;
    function GetCanModify: Boolean; override;
    function GetCanRefresh: Boolean; override;
    function GetDataSource: TDataSource; override;
    procedure GetObjectTypeNames(Fields: TFields);
    procedure GetOutputParams(AProcParams: TProcParamList = nil);
    function GetRecord(Buffer: TRecBuf; GetMode: TGetMode; DoCheck: Boolean): TGetResult; overload; override;
    function GetSortFieldNames: string;
    procedure InitRecord(Buffer: TRecBuf); overload; override;

    procedure InternalRefresh; override;
    procedure Loaded; override;
    function LocateRecord(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions; SyncCursor: Boolean): Boolean;
    procedure OpenCursor(InfoQuery: Boolean); override;
    procedure OpenSchema; virtual;
    procedure PropertyChanged;
    procedure SetBufListSize(Value: Integer); override;
    procedure SetCommandText(const Value: string); virtual;

    procedure SetFieldData(Field: TField; Buffer: TValueBuffer); overload; override;
    procedure SetParamsFromCursor;
    procedure SetSortFieldNames(Value: string);
    procedure UpdateIndexDefs; override;
    { protected properties }
    property BlobBuffer: TBlobByteData read FBlobBuffer write FBlobBuffer;
    property CurrentBlobSize: Int64 read FCurrentBlobSize write SetCurrentBlobSize;
    property DataLink: TDataLink read FDataLink;
    property InternalConnection: TSqlConnection read GetInternalConnection;
    property NativeCommand: string read FNativeCommand write FNativeCommand;
    property ProcParams: TProcParamList read GetProcParams write FProcParams;
    property RowsAffected: Integer read GetRowsAffected;
    procedure SetMaxBlobSize(MaxSize: Integer);
    procedure SetFCommandText(const Value: string);
    property ParamCount: Integer read GetParamCount;
    property SchemaInfo: TSQLSchemaInfo read FSchemaInfo write FSchemaInfo;
  protected  { publish in TSQLDataSet }
    property CommandType: TSQLCommandType read FCommandType write SetCommandType default ctQuery;
    property DbxCommandType: string read FDbxCommandType write SetDbxCommandType;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property MaxBlobSize: Integer read FMaxBlobSize write SetMaxBlobSize default 0;
    function GetRecordCount: Integer; override;
    property Params: TParams read FParams write SetParameters;
    property ParamCheck: Boolean read FParamCheck write FParamCheck default True;
    property SortFieldNames: string read GetSortFieldNames write SetSortFieldNames;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; DBXReader: TDBXReader; AOwnsInstance: Boolean); reintroduce; overload;
    destructor Destroy; override;
    property CommandText: string read FCommandText write SetCommandText;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    function GetBlobFieldData(FieldNo: Integer; var Buffer: TBlobByteData): Integer; override;
    procedure GetCommandNames(List: TStrings);
    procedure GetDetailLinkFields(MasterFields, DetailFields: TList<TField>); overload; override;
    function GetFieldData(FieldNo: Integer; var Buffer: TValueBuffer): Boolean; overload; override;
    function GetFieldData(Field: TField; var Buffer: TValueBuffer): Boolean; overload; override;
    function GetKeyFieldNames(List: TStrings): Integer;
    function GetQuoteChar: string; virtual;
    function ParamByName(const Value: string): TParam;
    procedure PrepareStatement; virtual;
    property IndexDefs: TIndexDefs read FIndexDefs write FIndexDefs;
    function IsSequenced: Boolean; override;
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; override;
    function Lookup(const KeyFields: string; const KeyValues: Variant;
      const ResultFields: string): Variant; override;
    procedure SetSchemaInfo(SchemaType: TSchemaType; SchemaObjectName, SchemaPattern: string; PackageName: string = '' );
    property Prepared: Boolean read FPrepared write SetPrepared default False;
    property DesignerData: string read FDesignerData write FDesignerData;
    property RecordCount: Integer read GetRecordCount;
    property SQLConnection: TSQLConnection read FSQLConnection write SetConnection;
  published
    property ParseSelectSql: TParseSqlEvent read FParseSelectSql write FParseSelectSql;
    property ParseDeleteSql: TParseSqlEvent read FParseDeleteSql write FParseDeleteSql;
    property ParseUpdateSql: TParseSqlEvent read FParseUpdateSql write FParseUpdateSql;
    property ParseInsertSql: TParseInsertSqlEvent read FParseInsertSql write FParseInsertSql;
    property SchemaName: string read FSchemaName write SetSchemaName;
    property GetMetadata: Boolean read FGetMetadata write FGetMetadata default True;
    property NumericMapping: Boolean read FNumericMapping write FNumericMapping default False;
    property ObjectView default False;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeScroll;
    property AfterScroll;
    property BeforeRefresh;
    property AfterRefresh;
    property OnCalcFields;
    property Active default False;
  end;

{ TSQLDataSet }

  TSQLDataSet = class(TCustomSQLDataSet)
  public
    constructor Create(AOwner: TComponent); override;
    function ExecSQL(ExecDirect: Boolean = False): Integer; override;
  published
    property CommandText;
    property CommandType;
    property DbxCommandType;
    property DataSource;
    property MaxBlobSize;
    property ParamCheck;
    property Params;
    property SortFieldNames;
    property SQLConnection;
  end;

{ TSQLQuery }

  TSQLQuery = class(TCustomSQLDataSet)
  private
    FSQL: TStrings;
    FText: string;
    procedure QueryChanged(Sender: TObject);
    procedure SetSQL(Value: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecSQL(ExecDirect: Boolean = False): Integer; override;
    procedure PrepareStatement; override;
    property RowsAffected;
    property Text: string read FText;
  published
    property DataSource;
    property GetMetadata default False;
    property MaxBlobSize;
    property ParamCheck;
    property Params;
    property SQL: TStrings read FSQL write SetSQL;
    property SQLConnection;
  end;

{ TSQLStoredProc }

  TSQLStoredProc = class(TCustomSQLDataSet)
  private
    FStoredProcName: string;
    FPackageName: string;
    procedure SetStoredProcName(Value: string);
    procedure SetPackageName(Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    function ExecProc: Integer; virtual;
    function NextRecordSet: TCustomSQLDataSet;
    procedure PrepareStatement; override;
  published
    property MaxBlobSize;
    property ParamCheck;
    property Params;
    { SetPackageName set StoredProcName to empty string
      Need to set PackageName 1st, and StoredProcName 2nd.
      Don't change following 2 items order }
    property PackageName: string read FPackageName write SetPackageName;
    property SQLConnection;
    property StoredProcName: string read FStoredProcName write SetStoredProcName;
  end;

{ TSQLTable }

  TSQLTable = class(TCustomSQLDataSet)
  private
    FIsDetail: Boolean;
    FIndexFields: TList;
    FIndexFieldNames: string;
    FIndexName: string;
    FMasterLink: TMasterDataLink;
    FTableName: string;
    FIndexFieldCount: Integer;
    procedure AddParamsToQuery;
    function GetMasterFields: string;
    function GetIndexField(Index: Integer): TField;
    function GetIndexFieldCount: Integer;
    function RefreshIndexFields: Integer;
    procedure SetIndexFieldNames(Value: string);
    procedure SetIndexName(Value: string);
    procedure SetMasterFields(Value: string);
    procedure SetTableName(Value: string);
    function GetQueryFromType: string; override;
    procedure SetDataSource(Value: TDataSource);
  protected
    procedure OpenCursor(InfoQuery: Boolean); override;
    procedure SetIndexField(Index: Integer; Value: TField);
    property MasterLink: TMasterDataLink read FMasterLink;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DeleteRecords;
    procedure GetIndexNames(List: TStrings);
    procedure PrepareStatement; override;
    property IndexFields[Index: Integer]: TField read GetIndexField write SetIndexField;
    property IndexFieldCount: Integer read GetIndexFieldCount;
  published
    property Active default False;
    property IndexFieldNames: string read FIndexFieldNames write SetIndexFieldNames;
    property IndexName: string read FIndexName write SetIndexName;
    property MasterFields: string read GetMasterFields write SetMasterFields;
    property MasterSource: TDataSource read GetDataSource write SetDataSource;
    property MaxBlobSize;
    property SQLConnection;
    property TableName: string read FTableName write SetTableName;
  end;

  { TSqlServerMethod }

  ///<summary>
  ///  A DataSet used to call DataSnap server methods and retrieve results from them.
  ///</summary>
  TSqlServerMethod = class(TCustomSQLDataSet)
  private
    FServerMethodName: string;
    procedure SetServerMethodName(Value: string);
    function CheckDetail(const SQL: string): string; override;
  public
    ///<summary>
    ///  Creates a new instance of <c>TSqlServerMethod</c>
    ///</summary>
    constructor Create(AOwner: TComponent); override;
    ///<summary>
    ///  Executes a remote server method that does not return a resultset.
    ///</summary>
    procedure ExecuteMethod;
  published
    property GetMetadata default False;
    property Params;
    property SQLConnection;
    ///<summary>
    ///  Sets the method name of the remote DataSnap server method.
    ///</summary>
    property ServerMethodName: string read FServerMethodName write SetServerMethodName;
  end;

{ Utility Routines }

  procedure LoadParamListItems(Params: TParams; ProcParams: TProcParamList);
  procedure FreeProcParams(var ProcParams: TProcParamList);
  procedure GetConnectionNames(List: TStrings; Driver: string = ''; DesignMode: Boolean = True);
  function GetDisplayDriverName(const ADriverName: string): string;
  procedure GetDisplayDriverNames(List: TStrings; DesignMode: Boolean = True);
  procedure GetDriverNames(List: TStrings; DesignMode: Boolean = True);
  procedure GetDelegateDriverNames(List: TStrings; DesignMode: Boolean = True);
  function GetDriverRegistryFile(DesignMode: Boolean = False): string;
  function GetConnectionRegistryFile(DesignMode: Boolean = False): string;
  procedure RegisterDriver(const DriverName: string);
  procedure UnregisterDriver(const DriverName: string);

type
  TGetDriverFunc = function(SVendorLib, SResourceFile: PChar; out Obj): TDBXErrorCode; stdcall;

implementation

uses
{$IFNDEF POSIX}
  System.Win.Registry,
  Winapi.Windows,
{$ELSE}
  Posix.Unistd,
{$ENDIF}
  Data.SqlConst,
  Data.DBConsts,
  Data.FmtBcd,
  System.IniFiles,
  Data.DBConnAdmin,
  System.Variants,
  Data.DBByteBuffer,
  System.StrUtils,
  Data.DBXJSONReflect,
  Data.DBXDBReaders,
  Data.SqlTimSt,
  Data.DBXPlatform,
  Data.DBXCommonResStrs,
  Data.DBXMetaDataNames
{$IFDEF MACOS}
  , Macapi.CoreFoundation
{$ENDIF MACOS}
;

                                                
{$IFDEF LINUX}
uses SqlConst, DBConsts, IniFiles, Math, DBConnAdmin, FMTBcd, DBXMetaDataNames;
{$ENDIF}

type
{$IFNDEF NEXTGEN}
  // deprecated.  Will be replaced by newer DBX4 metadata in the next
  // release.
  //
  TDBXIndexType = class
    const
      NonUnique                = $0001;
      Unique                   = $0002;
      PrimaryKey               = $0004;
  end deprecated;
{$ENDIF !NEXTGEN}

  TDBXIndexColumn = class
  public
    FIndexName:   string;
    FColumnName:  string;
    FOrdinal:     Integer;
    FAscending:   Boolean;
  end;

  TDBXIndexColumns = class
    private
      FGetIndexesText:  string;
      [Weak]FSqlConnection:   TSQLConnection;
      FColumns:         TList<TDBXIndexColumn>;
      procedure Open;
      function HasAllFieldNames( IndexName: string;
                              DataSet: TCustomSQLDataSet;
                              var FieldNames: string;
                              var FirstColumnAscending: Boolean): Boolean;
      function GetFieldNames(IndexName: string): string;
    public
      constructor Create;
      destructor Destroy; override;
    end;


{ Utility routines }

//procedure CheckObject(const Value: IInterface; const eType: TSQLExceptionType);
//var
//  Message: string;
//begin
//  if not Assigned(Value) then
//  begin
//    case eType of
//      exceptConnection: Message := SDBXNOCONNECTION;
//      exceptCommand: Message := SDBXNOCOMMAND;
//      exceptCursor: Message := SDBXNOCURSOR;
//      exceptMetadata: Message := SDBXNOMETAOBJECT;
//    end;
//    DatabaseError(Message);
//  end;
//end;

{
function GetTableScope(Scope: TTableScopes): string;
begin
  Result := '';
  if tsTable in Scope then
    Result := Result + TDBXMetaDataTableTypes.Table;
  if tsView in Scope then
  begin
    if Result <> '' then
      Result := Result + ';';
    Result := Result + TDBXMetaDataTableTypes.View;
  end;
  if tsSysTable in Scope then
  begin
    if Result <> '' then
      Result := Result + ';';
    Result := Result + TDBXMetaDataTableTypes.SystemTable;
  end;
  if tsSynonym in Scope then
  begin
    if Result <> '' then
      Result := Result + ';';
    Result := Result + TDBXMetaDataTableTypes.Synonym;
  end;

end;
}

function GetTableScope(Scope: TTableScopes): string;
begin
  Result := '';
  if tsTable in Scope then
    Result := Result + TDBXMetaDataTableTypes.Table + ';';
  if tsView in Scope then
    Result := Result + TDBXMetaDataTableTypes.View + ';';
  if tsSysTable in Scope then
    Result := Result + TDBXMetaDataTableTypes.SystemTable + ';';
  if tsSynonym in Scope then
    Result := Result + TDBXMetaDataTableTypes.Synonym + ';';
end;

{$IFDEF POSIX}
function CopyConfFile(Source, Target: string): Boolean;
var
  List: TStrings;
  IniIn, IniOut: TMemIniFile;
begin
  List := TStringList.Create;
  try
    IniIn := TMemIniFile.Create(Source);
    try
      IniOut := TMemIniFile.Create(Target);
      try
        IniIn.GetStrings(List);
        IniOut.SetStrings(List);
        IniOut.UpdateFile;
        Result := True;
      finally
        IniOut.Free;
      end;
    finally
      IniIn.Free;
    end;
  finally
    List.Free;
  end;
end;
{$ENDIF}

function GetRegistryFile(Setting, Default: string; DesignMode: Boolean): string;
{$IFDEF POSIX}
begin
  Result := './' + Default;
  if not FileExists(Result) then
  begin
    Result := GetHomePath + LibraryPreferences + Default;
    if not FileExists(Result) then
    begin
      Result := LibraryPreferences + Default;
      if not FileExists(Result) then
        Result := '';
    end;
  end;
{$ELSE}
var
  Reg: TRegistry;
begin
  Result := '';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKeyReadOnly(TDBXRegistryKey) then
      Result := Reg.ReadString(Setting);
  finally
    Reg.Free;
  end;
  if Result = '' then
    Result := ExtractFileDir(ParamStr(0)) + '\' + Default;
{$ENDIF}
end;

function GetDriverRegistryFile(DesignMode: Boolean = False): string;
begin
  Result := GetRegistryFile(TDBXRegistryDriverValue, TDBXDriverFile, DesignMode);
end;

function GetConnectionRegistryFile(DesignMode: Boolean = False): string;
begin
  Result := GetRegistryFile(TDBXRegistryConnectionValue, TDBXConnectionFile, DesignMode);
end;

function GetBlobLength(DataSet: TCustomSQLDataSet; FieldNo: Integer): Int64;
var
  IsNull: LongBool;
begin
  Result := 0;
  if not DataSet.EOF then
  begin
    if DataSet.MaxBlobSize = 0 then
      exit;
    DataSet.FDBXReader.ByteReader.GetByteLength(FieldNo-1, Result, isNull);
    if isNull then
      Result := 0;
  end;
  DataSet.CurrentBlobSize := Result;
end;

function NextPiece(Start: string; InLiteral: Boolean; QuoteChar: WideChar; EndParam: Boolean = False): Integer;
var
  P, Len: Integer;
  C, LQuoteChar: Char;
begin
  P := 1;
  Len := Start.Length - 1;
  Result := -1;
  LQuoteChar := QuoteChar;
  while (Result = -1) and (P <= Len) and (Start.Chars[P] <> #0) do
  begin
    C := Start.Chars[P];
    if (C = '''') or (C = LQuoteChar) then
      InLiteral := not InLiteral
    else if not InLiteral and ((C = ' ') or (C = ')') or (C = ',') or (C = '=')
      or (C = ':') or (C = '>') or (C = '<') or (C = #13) or (C = #10)) then
    begin
      if EndParam then
      begin
        if not ((C = '=') or (C = ':') or (C = '<') or (C = '>')) then
          Result := P;
      end
      else
      begin
        if (C = ':') then
        begin
          if ((Start.Chars[P-1] = ' ') or (Start.Chars[P-1] = ')') or (Start.Chars[P-1] = ',')
            or (Start.Chars[P-1] = '=') or (Start.Chars[P-1] = '(')) then
            Result := P - 1;
        end
        else if (P < Len) and (Start.Chars[P + 1] = ':') then
          Result := P;
      end;
    end;
    Inc(P);
  end;
end;

// SqlObjects does not support named params: convert to ?
// if not yet converted
function FixParams(SQL: string; Count: Integer; QuoteChar: string): string;
var
  Param, Start: string;
  Pos, EndPos: Integer;
  InLiteral: Boolean;
  Q: Char;
begin
  Q := #0;
  if QuoteChar.Length > 0 then
    Q := QuoteChar.Chars[0];
  if (Q = #0) or (Q = ' ') then Q := '''';
  InLiteral := False;
  Start := SQL;
  Pos := NextPiece(Start, InLiteral, Q);
  while Pos >= 0 do
  begin
    Start := Start.SubString(Pos + 1, Start.Length - Pos + 1);
    EndPos := NextPiece(Start, InLiteral, Q, True);
    if EndPos = -1 then
      Param := Start.SubString(0, Start.Length)
    else
      Param := Start.SubString(0, EndPos);
    SQL := SQL.Replace(Param, ' ? ', []);
    Pos := NextPiece(Start, InLiteral, Q);
  end;
  Result := SQL;
end;

function GetProfileString(Section, Setting, IniFileName: string): string;
var
  IniFile: TMemIniFile;
  List: TStrings;
begin
  List := TStringList.Create;
  try
    IniFile := TMemIniFile.Create(IniFileName);
    try
      IniFile.ReadSectionValues(Section, List);
      Result := List.Values[ Setting ];
    finally
      IniFile.Free;
    end;
  finally
    List.Free;
  end;
end;

function GetDisplayDriverName(const ADriverName: string): string;
var
  ConnectionAdmin: IConnectionAdmin;
begin
  ConnectionAdmin := GetConnectionAdmin;
  Result := ConnectionAdmin.GetDisplayDriverName(ADriverName);
end;

procedure GetDisplayDriverNames(List: TStrings; DesignMode: Boolean = True);
var
  ConnectionAdmin: IConnectionAdmin;
begin
  ConnectionAdmin := GetConnectionAdmin;
  ConnectionAdmin.GetDisplayDriverNames(List);
end;

procedure GetDriverNames(List: TStrings; DesignMode: Boolean = True);
var
  ConnectionAdmin: IConnectionAdmin;
begin
  ConnectionAdmin := GetConnectionAdmin;
  ConnectionAdmin.GetDriverNames(List);
end;

procedure GetDelegateDriverNames(List: TStrings; DesignMode: Boolean = True);
var
  ConnectionAdmin: IConnectionAdmin;
begin
  ConnectionAdmin := GetConnectionAdmin;
  ConnectionAdmin.GetDelegateDriverNames(List);
end;

procedure GetConnectionNames(List: TStrings; Driver: string = ''; DesignMode: Boolean = True);
var
  ConnectionAdmin: IConnectionAdmin;
begin
  ConnectionAdmin := GetConnectionAdmin;
  ConnectionAdmin.GetConnectionNames(List, Driver);
end;

procedure RegisterDriver(const DriverName: string);
var
  ConnectionAdmin: IConnectionAdmin;
begin
  ConnectionAdmin := GetConnectionAdmin;
  ConnectionAdmin.RegisterDriver(DriverName);
end;

procedure UnregisterDriver(const DriverName: string);
var
  ConnectionAdmin: IConnectionAdmin;
begin
  ConnectionAdmin := GetConnectionAdmin;
  ConnectionAdmin.UnregisterDriver(DriverName);
end;

procedure CalcUnits( const Params: TParams; const ProcParams: TProcParamList;
  const Index: Integer; var pArgDesc: SPParamDesc; var ChildPos: array of Word );
var
  I:        Integer;
  ArgDesc:  SPParamDesc;
begin
  I := Index + 1;
  pArgDesc.iUnits1 := 0;
  pArgDesc.iUnits2 := 0;
  if ProcParams = nil then
    ArgDesc := SPParamDesc.Create
  else
    ArgDesc := nil;
  try
    while (I < Params.Count) do
    begin
      if ProcParams <> nil then
        ArgDesc := (SPParamDesc(ProcParams.Items[I]))
      else
      begin
        ArgDesc.iParamNum := Params[I].ID + 1;
        ArgDesc.szName := Params[I].Name;
        ArgDesc.iArgType := Params[I].ParamType;
        ArgDesc.iDataType := Params[I].DataType;
        ArgDesc.iUnits1 := Params[I].Precision;
        ArgDesc.iUnits2 := Params[I].NumericScale;
        ArgDesc.iLen := Params[I].GetDataSize;
      end;
      if ArgDesc.iParamNum <> pArgDesc.iParamNum then
        break;
      Inc(pArgDesc.iUnits1);
      Inc(pArgDesc.iUnits2);
      ChildPos[I] := I - Index;
      if ArgDesc.iDataType = ftADT then
      begin
        CalcUnits(Params, ProcParams, I, ArgDesc, ChildPos);
        Inc(pArgDesc.iUnits2, ArgDesc.iUnits2);
        Inc(I, ArgDesc.iUnits2);
      end else
        Inc(I);
    end;
  finally
    if ProcParams = nil then
      ArgDesc.Free;
  end;
end;

procedure CopyParamBytesToByteBuffer(const Param: TParam; const Buffer: TDBByteBuffer);
var
  Value: Variant;
  P: Pointer;
begin
  Value := Param.Value;
  if VarIsArray(Value) then
  begin
    P := VarArrayLock(Value);
    try
      Buffer.Append(PChar(P), VarArrayHighBound(Value, 1) + 1);
    finally
      VarArrayUnlock(Value);
    end;
  end;
end;

procedure SetQueryProcParams(const Sender: TSQLConnection;
  const Command: TDBXCommand; const Params: TParams; ProcParams: TProcParamList = nil);
var
  DataLen, DBXIndex, I, iInd: Integer;
  LFldNum: LongWord;
  LFldType, LSubType: Word;
  ArgDesc: SPParamDesc;
  ChildPosArray: array of Word;
  Param: TParam;
  Buffer: TDBByteBuffer;
  ExtractedBytes: TArray<Byte>;
  DbxParameter: TDBXParameter;
  Marshal: TJSONMarshal;
begin
  Buffer := nil;
  SetLength(ChildPosArray, Params.Count);
  ArgDesc := SPParamDesc.Create;
  try
    for I := 0 to Params.Count - 1 do
    begin
      iInd := 0;
      Param := Params[I];
      try
        if Param.ParamType = ptUnknown then  // Midas assumes its Input
          Param.ParamType := ptInput;
        LFldNum := I + 1;
        LFldType := FldTypeMap[Param.DataType];
        LSubType := 0;
        if LFldType in [TDBXDataTypes.BlobType, TDBXDataTypes.AnsiStringType, TDBXDataTypes.WideStringType] then
          LSubType := Word(FldSubTypeMap[Param.DataType])
        else if (not Param.IsNull) and (LFldType = TDBXDataTypes.UnknownType) then
          DatabaseErrorFmt(SNoParameterValue, [Param.Name]);
        if ProcParams <> nil then
          if I < ProcParams.Count then
          begin
            // This method will modify ArgDesc.iLen, so make a copy.
            //
            ArgDesc.iParamNum := SPParamDesc(ProcParams.Items[I]).iParamNum;
            ArgDesc.szName := SPParamDesc(ProcParams.Items[I]).szName;
            ArgDesc.iArgType := SPParamDesc(ProcParams.Items[I]).iArgType;
            ArgDesc.iDataType := SPParamDesc(ProcParams.Items[I]).iDataType;
            ArgDesc.iUnits1 := SPParamDesc(ProcParams.Items[I]).iUnits1;
            ArgDesc.iUnits2 := SPParamDesc(ProcParams.Items[I]).iUnits2;
            ArgDesc.iLen := SPParamDesc(ProcParams.Items[I]).iLen;
          end
          else
            DatabaseErrorFmt(STooManyActualParam, [Command.Text, I+1, ProcParams.Count])
        else
        begin
          ArgDesc.iParamNum := LFldNum;
          ArgDesc.szName := Param.Name;
          ArgDesc.iArgType := Param.ParamType;
          ArgDesc.iDataType := Param.DataType;
          ArgDesc.iUnits1 := Param.Precision;
          ArgDesc.iUnits2 := Param.NumericScale;
          if (Param.ParamType = ptResult) or  (Param.ParamType = ptOutput) then
            ArgDesc.iLen := Param.Size
          else if (Param.IsNull) or (Param.DataType = ftVariant) then
            ArgDesc.iLen := 0
          else
            ArgDesc.iLen := Param.GetDataSize;
        end;
        LFldType := FldTypeMap[ArgDesc.iDataType];
        if (Param.ParamType <> ptOutput) and (Param.ParamType <> ptResult) then
        begin
          if Param.IsNull or (Param.DataType = ftVariant) then
            DataLen := 0
          else
            DataLen := Param.GetDataSize;
        end
        else
          DataLen := ArgDesc.iLen;
        {Check if the IN param is NULL and set the NULL indicator}
        if ((Param.ParamType in [ptInput, ptInputOutput]) and Param.IsNull) then
          iInd := 1
        else
        if (DataLen > 0) then
        begin
          iInd := 0;
          if Param.ParamType = ptInput then
            Param.Size := 0;
          if (Param.ParamType = ptOutput) and not(LFldType in [TDBXDataTypes.DoubleType, TDBXDataTypes.SingleType]) then
          begin
            if LFldType in [TDBXDataTypes.BcdType, TDBXDataTypes.CurrencyType] then
              Param.Size := ArgDesc.iUnits1
            else
              ArgDesc.iLen := 0;
          end else
            case LFldType of
              TDBXDataTypes.BlobType:
                begin
                  ArgDesc.iLen := DataLen;
                  ArgDesc.iUnits2 := 0;
                  Param.Size := DataLen;
                end;
              TDBXDataTypes.AnsiStringType, TDBXDataTypes.BytesType, TDBXDataTypes.VarBytesType:
                begin
                  ArgDesc.iLen := DataLen;
                  ArgDesc.iUnits2 := 0;
                  if Param.ParamType = ptInput then
                  begin
                    if (LFldType <> TDBXDataTypes.AnsiStringType) then
                      Param.Size := DataLen;
                  end else if (Param.ParamType = ptInputOutput) and (DataLen > Param.Size) then
                  begin
                    Param.Size := DataLen;
                  end;
                end;
              TDBXDataTypes.SingleType: ArgDesc.iLen := SizeOf(Single);
              TDBXDataTypes.DoubleType:
                begin
                  if Param.Precision = 4 then
                    ArgDesc.iLen := 4
                  else
                    ArgDesc.iLen := Sizeof(Double);
                end;
              TDBXDataTypes.CurrencyType, TDBXDataTypes.BcdType:
                begin
                  LFldType := TDBXDataTypes.BcdType;
                  if Param.Size <> 0 then
                  begin
                    ArgDesc.iUnits2 := Param.NumericScale;
                  end;
                end;
              TDBXDataTypes.AdtType, TDBXDataTypes.ArrayType:
                begin
                  CalcUnits(Params, ProcParams, I, ArgDesc, ChildPosArray);
                  ArgDesc.iLen := DataLen;
                end;
            end;
        end else
        begin
          if LFldType in [TDBXDataTypes.AdtType, TDBXDataTypes.ArrayType] then
            DatabaseError(SObjectTypenameRequired);
          if not LFldType in [TDBXDataTypes.TableType, TDBXDataTypes.BinaryBlobType] then
            iInd := 1;
        end;
        DBXIndex := LFldNum - 1 - ChildPosArray[I];

        DbxParameter := Command.Parameters[DbxIndex];
        if DbxParameter = nil then
        begin
          DbxParameter := Command.CreateParameter;
          Command.Parameters.SetParameter(DbxIndex, DbxParameter);
        end;

        DbxParameter.ChildPosition      := ChildPosArray[I];
        DbxParameter.ParameterDirection := TDBXParameterDirection(ArgDesc.iArgType);
        if not ((LFldType = TDBXDataTypes.DoubleType) and ((DbxParameter.DataType = TDBXDataTypes.SingleType) or (DbxParameter.DataType = TDBXDataTypes.CurrencyType))) then
          if not ((LFldType = TDBXDataTypes.JsonValueType) and ((DbxParameter.DataType = TDBXDataTypes.JsonValueType) or (DbxParameter.DataType = TDBXDataTypes.CallbackType))) then
            DbxParameter.DataType           := LFldType;
        if DbxParameter.SubType = 0 then
          DbxParameter.SubType          := LSubType;
        DbxParameter.Size               := ArgDesc.iLen;
        DbxParameter.Precision          := Param.Size;
        DbxParameter.Scale              := Integer(ArgDesc.iUnits2);
        if    (DbxParameter.ParameterDirection = TDBXParameterDirections.InParameter)
          or  (DbxParameter.ParameterDirection = TDBXParameterDirections.InOutParameter) then
        begin
          if iInd <> 0 then
            DbxParameter.Value.SetNull
          else //if Param.ParamType <> ptOutput then
          // Should not be the Param type.  It must be the type from
          // ProcParams since this is the actual type.  Param has
          // the Variant type which may not be the same.
          //
            case ArgDesc.iDataType of
{$IFNDEF NEXTGEN}
              ftString, ftFixedChar, ftMemo, ftAdt:
                DbxParameter.Value.SetAnsiString(Param.AsAnsiString);
{$ELSE}
              ftString, ftFixedChar, ftMemo, ftAdt,
{$ENDIF !NEXTGEN}
              ftWideString, ftWideMemo, ftFixedWideChar:
                DbxParameter.Value.AsString := Param.AsWideString;
              ftByte:
                DbxParameter.Value.AsUInt8 := Param.AsInteger;
              ftShortint:
                DbxParameter.Value.AsInt8 := Param.AsInteger;
              ftSmallint:
                DbxParameter.Value.AsInt16 := Param.AsInteger;
              ftWord:
                DbxParameter.Value.AsUInt16 := Param.AsInteger;
              ftAutoInc, ftInteger:
                DbxParameter.Value.AsInt32 := Param.AsInteger;
              ftLargeint:
                DbxParameter.Value.AsInt64 := Param.AsLargeInt;
              ftTime:
                DbxParameter.Value.AsTime := DateTimeToTimeStamp(Param.AsDateTime).Time;
              ftDate:
                DbxParameter.Value.AsDate := DateTimeToTimeStamp(Param.AsDateTime).Date;
              ftBCD, ftFMTBCD:
                DbxParameter.Value.AsBCD := Param.AsFMTBcd;
              ftSingle:
                DbxParameter.Value.AsSingle := Param.AsSingle;
              ftCurrency, ftFloat:
                if DbxParameter.DataType = TDBXDataTypes.SingleType then
                  DbxParameter.Value.AsSingle := Param.AsFloat
                else if DbxParameter.DataType = TDBXDataTypes.CurrencyType then
                  DbxParameter.Value.AsBcd := Param.AsFMTBCD
                else
                  DbxParameter.Value.AsDouble := Param.AsFloat;
              ftTimeStamp, ftDateTime:
                DbxParameter.Value.AsTimeStamp := Param.AsSQLTimeStamp;
              ftTimeStampOffset:
                DbxParameter.Value.SetTimeStampOffset(Param.AsSQLTimeStampOffset);
              ftBoolean:
                DbxParameter.Value.AsBoolean := Param.AsBoolean;
              ftObject:
                if DbxParameter.SubType = TDBXDataTypes.UserSubType then
                begin
                  // convert TObject to JSON
                  Marshal := TJSONConverters.GetJSONMarshaler;
                  try
                    DbxParameter.Value.SetObjectValue(Marshal.Marshal(Param.AsObject), True);
                  finally
                    Marshal.Free;
                  end;
                end
                else
                  DbxParameter.Value.SetObjectValue( Param.AsObject, False );
              ftVariant:
                DbxParameter.Value.AsVariant := Param.Value;
              ftBytes, ftVarBytes:
              begin
                Buffer := TDBByteBuffer.Create(DataLen);
                CopyParamBytesToByteBuffer(Param, Buffer);
                ExtractedBytes := Buffer.GetBytes;
                DbxParameter.Value.SetDynamicBytes(0, ExtractedBytes, 0, Length(ExtractedBytes));
              end;
              ftBlob, ftGraphic..ftTypedBinary,ftOraBlob,ftOraClob:
              begin
                 Buffer := TDBByteBuffer.Create(DataLen);
                 if DataLen > 0 then
                   Buffer.Append(Param.AsBytes);
                 ExtractedBytes := Buffer.GetBytes;
                 DbxParameter.Value.SetDynamicBytes(0, ExtractedBytes, 0, Length(ExtractedBytes));
              end;
              ftArray, ftCursor,
              ftReference: {Nothing};
              ftStream:
                if (Param.ParamType = ptInput) or (Param.ParamType = ptInputOutput) then
                  DbxParameter.Value.SetStream(Param.AsStream, False);
              ftParams:
                if (Param.ParamType = ptInput) or (Param.ParamType = ptInputOutput) then
                  DbxParameter.Value.SetDBXReader(TDBXParamsReader.Create(Param.AsParams, False), True);
              ftDataSet:
                if (Param.ParamType = ptInput) or (Param.ParamType = ptInputOutput) then
                  DbxParameter.Value.SetDBXReader(TDBXDataSetReader.Create(Param.AsDataSet, False), True)
              else
                DatabaseError(Format(SUnknownDataType, [TDBXValueType.DataTypeName(DbxParameter.DataType), ArgDesc.szName]));
          end;
        end;
      finally
        if Buffer <> nil then
          FreeAndNil(Buffer);
      end;
    end;
  finally
    ArgDesc.Free;
  end;
end;

procedure FreeProcParams(var ProcParams: TProcParamList);
var
  ArgParam: SPParamDesc;
  I: Integer;
begin
  if not Assigned(ProcParams) then Exit;
  for I := 0 to ProcParams.Count - 1 do
  begin
    ArgParam := SPParamDesc(ProcParams[I]);
    ArgParam.Free;
  end;
  FreeAndNil(ProcParams);
end;

procedure LoadParamListItems(Params: TParams; ProcParams: TProcParamList);
var
  I: Integer;
  ArgParam: SPParamDesc;
  LParam: TParam;
begin
  for I := 0 to ProcParams.Count -1 do
  begin
    ArgParam := SPParamDesc(ProcParams.Items[I]);
    LParam := TParam(Params.Add);
    LParam.Name := ArgParam.szName;
    LParam.ParamType := ArgParam.iArgType;
    LParam.DataType := ArgParam.iDataType;
    LParam.NumericScale := ArgParam.iUnits2;
    LParam.Precision := ArgParam.iUnits1;
    if LParam.ParamType <> ptInput then
      LParam.Size := ArgParam.iLen;
  end;
end;

{ TSQLBlobStream }

constructor TSQLBlobStream.Create(Field: TBlobField; Mode: TBlobStreamMode = bmRead);
begin
  inherited Create;
  if not Field.DataSet.Active then
    DataBaseError(SDatasetClosed);
  FField := Field;
  FDataSet := FField.DataSet as TCustomSQLDataSet;
  FFieldNo := FField.FieldNo;
  FHasData := false;
  ReadBlobSize;
end;

destructor TSQLBlobStream.Destroy;
begin
  inherited Destroy;
end;

procedure TSQLBlobStream.ReadBlobSize;
var
  LOffset, LRead: Integer;
  LIsNull: LongBool;
  LBytes: TArray<Byte>;
  BlobLength: Int64;
const
  sMaxTmpBuffer = 32768;                                                                                            
begin
  Clear;
  BlobLength := GetBlobLength(FDataSet, FFieldNo);

  // For DataSnap connections, blob size can be -1 for large streams
  //  We need to determine the correct size now to be able to retrieve
  //  the data properly
  if BlobLength = -1 then
  begin
    //if MaxBlobSize is -1, then we are not limiting the buffer size
    //  so let's find out what the real blob size is
    if FDataSet <> nil then
    begin
      if FDataSet.MaxBlobSize = -1 then
      begin
        LOffset := 0;
        repeat
          SetLength(LBytes, LOffset + sMaxTmpBuffer);
          LRead :=  FDataSet.FDBXReader.ByteReader.GetBytes(FFieldNo-1, LOffset, LBytes, LOffset, sMaxTmpBuffer, LIsNull);
          if LRead < sMaxTmpBuffer then
          begin
            LOffset := LOffset + LRead;
            SetLength(LBytes, LOffset)
          end
          else
            LOffset := LOffset + sMaxTmpBuffer;
        until LRead < sMaxTmpBuffer;
        BlobLength := LOffset;
        Write(LBytes[0], BlobLength);
        FHasData := True;
        Position := 0;
      end
      else
        BlobLength := FDataSet.MaxBlobSize * 1024;
      FDataSet.CurrentBlobSize := BlobLength;
    end;
  end;

  if BlobLength > 0 then
    SetSize(BlobLength);
end;

procedure TSQLBlobStream.ReadBlobData;
begin
  if FDataSet.GetFieldData(FField, FDataSet.FBlobBuffer, True) then
    Write(FDataSet.FBlobBuffer[0], FDataSet.FCurrentBlobSize);

  Position := 0;
  FHasData := true;
end;

function TSQLBlobStream.Read(Buffer: TBytes; Offset, Count: LongInt): LongInt;
begin
  if not FHasData then
    ReadBlobData;
  Result := inherited Read(Buffer, Offset, Count);
end;

{$IFNDEF NEXTGEN}
function TSQLBlobStream.Read(var Buffer; Count: Longint): Longint;
begin
  if not FHasData then
    ReadBlobData;
  Result := inherited Read(Buffer, Count);
end;
{$ENDIF !NEXTGEN}

type

{ TSQLParams }

  TSQLParams = class(TParams)
  private
    FFieldName: TStrings;
    FBindAllFields: Boolean;
    FIdOption: IDENTIFIEROption;
    function ParseSelect(SQL: string; bDeleteQuery: Boolean): string;
    function ParseUpdate(SQL: string): string;
    function ParseInsert(SQL: string): string;
  public
    constructor Create(Owner: TPersistent; IdOption: IDENTIFIEROption);
    Destructor Destroy; override;
    function GetFieldName(Index: Integer): string;
    function Parse(Var SQL: string; DoCreate: Boolean): string;
    property BindAllFields: Boolean read FBindAllFields;
  end;

constructor TSQLParams.Create(Owner: TPersistent; IdOption: IDENTIFIEROption);
begin
  inherited Create;
  FBindAllFields := False;
  FFieldName := TStringList.Create;
  FIdOption := IdOption;
end;

destructor TSQLParams.Destroy;
begin
  inherited;
  FreeAndNil(FFieldName);
end;

function TSQLParams.GetFieldName(Index: Integer): string;
begin
   Result := FFieldName[ Index ];
end;

function TSQLParams.Parse(var SQL: string; DoCreate: Boolean): string;
const
  SDelete = 'delete';      { Do not localize }
  SUpdate = 'update';      { Do not localize }
  SInsert = 'insert';      { Do not localize }
var
  Start: string;
begin
  SQL := ParseSQL(SQL, DoCreate);
  Start := SQL.Substring(0, 6).ToLower;
{ attempt to determine fields and fieldtypes associated with params }
  if Start = SSelect then
    Result := ParseSelect(SQL, False)
  else if Start = SDelete then
    Result := ParseSelect(SQL, True)
  else if Start = SInsert then
    Result := ParseInsert(SQL)
  else if Start = SUpdate then
    Result := ParseUpdate(SQL)
  else
    Result := '';
end;

function Platform_NextSQLToken(  const SQL:  string;
                                 var p:      NativeInt;
                                 out Token:  string;
                                 CurSection: TSQLToken;
                                 IdOption: IDENTIFIEROption): TSQLToken;
var
  pSQL:       PChar;
  pSQLStart:  PChar;
begin
  pSQLStart := PChar(SQL);
  pSQL      := pSQLStart + p - 1;
  Result    := NextSQLTokenEx(pSQL, Token, CurSection, IdOption);
  p         := pSQL - pSQLStart + 1;
end;

                   
// Move this into ConnectionOptions in the next version.
function GetIdOption(Connection: TSQLConnection): IDENTIFIEROption;
var
  MetaData: TDBXDatabaseMetaData;
begin
  Result := idMixCase;
  if (Assigned(Connection) and Assigned(Connection.FDBXConnection)) then
  begin
    MetaData := Connection.MetaData;
    if Assigned(MetaData) and (MetaData.MetaDataVersion = DBXVersion40) then
    begin
      if not MetaData.SupportsLowerCaseIdentifiers then
        Result := idMakeUpperCase
      else if not MetaData.SupportsUpperCaseIdentifiers then
        Result := idMakeLowerCase;
    end;
  end;
end;

{ no attempt to match fields clause with values clause :
    types only added if all values are inserted }
function TSQLParams.ParseInsert(SQL: string): string;
var
  Start: NativeInt;
  Value: string;
  CurSection: TSQLToken;
begin
  Result := '';
  if ((Owner <> nil) and (Owner is TCustomSqlDataSet) and Assigned(TCustomSqlDataSet(Owner).ParseInsertSql)) then
    TCustomSqlDataSet(Owner).ParseInsertSql(FFieldName, SQL, FBindAllFields, Result)
  else
  begin
    if SQL.ToLower.IndexOf(SSelectSpaces) > 0 then Exit;  // can't parse sub queries
    Start := 1;
    CurSection := stUnknown;
    { move past 'insert ' }
    Platform_NextSQLToken(SQL, Start, Value, CurSection, FIdOption);
    { move past 'into ' }
    Platform_NextSQLToken(SQL, Start, Value, CurSection, FIdOption);
    { move past <TableName> }
    Platform_NextSQLToken(SQL, Start, Value, CurSection, FIdOption);

    { Check for owner qualified table name }
    if (Start <= SQL.Length) and (SQL.Chars[Start-1] = '.') then
      Platform_NextSQLToken(SQL, Start, Value, CurSection, FIdOption);
    Result := Value;

    { move past 'set' }
    Platform_NextSQLToken(SQL, Start, Value, CurSection, FIdOption);
    if (Value.ToLower = 'values') then
      FBindAllFields := True;
  end;
end;

function TSQLParams.ParseSelect(SQL: string; bDeleteQuery: Boolean): string;
var
  FWhereFound: Boolean;
  bParsed: Boolean;
  Start: NativeInt;
  FName, Value: string;
  SQLToken, CurSection, LastToken: TSQLToken;
  Params: Integer;
begin
  Result := '';
  bParsed := False;
  if ((Owner <> nil) and (Owner is TCustomSqlDataSet)) then
  begin
    if (not bDeleteQuery) and Assigned(TCustomSqlDataSet(Owner).ParseSelectSql) then
    begin
      TCustomSqlDataSet(Owner).ParseSelectSql(FFieldName, SQL, Result);
      bParsed := True;
    end
    else if bDeleteQuery and Assigned(TCustomSqlDataSet(Owner).ParseDeleteSql) then
    begin
      TCustomSqlDataSet(Owner).ParseDeleteSql(FFieldName, SQL, Result);
      bParsed := True;
    end;
  end;
  if not bParsed then
  begin
    if bDeleteQuery = False then
    begin
      if SQL.ToLower.IndexOf(SSelectSpaces, 8) > 0 then Exit;  // can't parse sub queries
    end
    else
    begin
      if SQL.ToLower.IndexOf(SSelectSpaces) > 0 then Exit;  // can't parse sub queries
      SQL := SSelectStar + SQL.Substring(7, SQL.Length - 7);
    end;
    Start := 1;
    CurSection := stUnknown;
    LastToken := stUnknown;
    FWhereFound := False;
    Params := 0;
    repeat
      repeat
        SQLToken := Platform_NextSQLToken(SQL, Start, FName, CurSection, FIdOption);
        if SQLToken = stWhere then
        begin
          FWhereFound := True;
          LastToken := stWhere;
        end
        else if SQLToken = stTableName then
        begin
          { Check for owner qualified table name }
          if (Start <= SQL.Length) and (SQL.Chars[Start-1] = '.') then
            Platform_NextSQLToken(SQL, Start, FName, CurSection, FIdOption);
          Result := FName;
        end
        else if (SQLToken = stValue) and (LastToken = stWhere) then
          SQLToken := stFieldName;
        if SQLToken in SQLSections then
          CurSection := SQLToken;
      until SQLToken in [stFieldName, stEnd];
      if FWhereFound and (SQLToken = stFieldName) then
        repeat
          SQLToken := Platform_NextSQLToken(SQL, Start, Value, CurSection, FIdOption);
          if SQLToken in SQLSections then
            CurSection := SQLToken;
        until SQLToken in [stEnd,stValue,stIsNull,stIsNotNull,stFieldName];
      if Value = '?' then
      begin
        FFieldName.Add(FName);
        Inc(Params);
      end;
    until (Params = Count) or (SQLToken = stEnd);
    if Result = '' then Result := GetTableNameFromSQLEx(SQL,FIdOption);
  end;
end;

function TSQLParams.ParseUpdate(SQL: string): string;
var
  Start: NativeInt;
  FName, Value: string;
  SQLToken, CurSection: TSQLToken;
  Params: Integer;
begin
  Result := '';
  if ((Owner <> nil) and (Owner is TCustomSqlDataSet) and Assigned(TCustomSqlDataSet(Owner).ParseUpdateSql)) then
    TCustomSqlDataSet(Owner).ParseUpdateSql(FFieldName, SQL, Result)
  else
  begin
    if SQL.ToLower.IndexOf(SSelectSpaces) > 0 then Exit;  // can't parse sub queries
    Start := 1;
    CurSection := stUnknown;
    { move past 'update ' }
    Platform_NextSQLToken(SQL, Start, FName, CurSection, FIdOption);
    { move past <TableName> }
    Platform_NextSQLToken(SQL, Start, FName, CurSection, FIdOption);

    { Check for owner qualified table name }
    if (Start <= SQL.Length) and (SQL.Chars[Start-1] = '.') then
      Platform_NextSQLToken(SQL, Start, FName, CurSection, FIdOption);

    Result := FName;
    { move past 'set ' }
    Platform_NextSQLToken(SQL, Start, FName, CurSection, FIdOption);
    Params := 0;
    CurSection := stSelect;
    repeat
      repeat
        SQLToken := Platform_NextSQLToken(SQL, Start, FName, CurSection, FIdOption);
        if SQLToken in SQLSections then CurSection := SQLToken;
      until SQLToken in [stFieldName, stEnd];
      if 'values('.IndexOf(FName.ToLower) > -1 then continue;   { do not localize }
      if 'values ('.IndexOf(FName.ToLower) > -1 then continue;  { do not localize }
      if SQLToken = stFieldName then
        repeat
          SQLToken := Platform_NextSQLToken(SQL, Start, Value, CurSection, FIdOption);
            if SQLToken in SQLSections then CurSection := SQLToken;
        until SQLToken in [stEnd,stValue,stIsNull,stIsNotNull,stFieldName];
      if Value = '?' then
      begin
        FFieldName.Add(FName);
        Inc(Params);
      end;
    until (Params = Count) or (SQLToken = stEnd);
  end;
end;

{ TSQLMonitor }

constructor TSQLMonitor.Create(AOwner: TComponent);
begin
  FTraceList := TStringList.Create;
  FMaxTraceCount := -1;
  inherited;
end;

destructor TSQLMonitor.Destroy;
begin
  if Active then SetActive(False);
  SetSQLConnection(nil);
  inherited;
  FreeAndNil(FTraceList);
end;

procedure TSQLMonitor.SetFileName(const Value: string);
begin
  FFileName := Value;
end;

procedure TSQLMonitor.CheckInactive;
begin
  if FActive then
  begin
    if (csDesigning in ComponentState) or (csLoading in ComponentState) then
      SetActive(False)
    else
      DatabaseError(SMonitorActive, Self);
  end;
end;

procedure TSQLMonitor.SetSQLConnection(Value: TSQLConnection);
var
  IsActive: Boolean;
begin
  if Value <> FSQLConnection then
  begin
    IsActive := Active;
    CheckInactive;
    if Assigned(FSQLConnection) and not FKeepConnection then
      SQLConnection.UnregisterTraceMonitor(Self);
    FSQLConnection := Value;
    if Assigned(FSQLConnection) then
    begin
      FSQLConnection.RegisterTraceMonitor(Self);
      Active := IsActive;
    end;
  end;
end;

procedure TSQLMonitor.SwitchConnection(const Value: TSQLConnection);
var
  MonitorActive: Boolean;
begin
  FKeepConnection := True;
  MonitorActive := Active;
  if MonitorActive then
    SetActive(False);
  SQLConnection := Value;
  if MonitorActive and Assigned(Value) then
    SetActive(True);
  FKeepConnection := False;
end;

procedure TSQLMonitor.Trace(TraceInfo: TDBXTraceInfo; LogTrace: Boolean);
begin
  if Assigned(FOnTrace) then
    FOnTrace(Self, TraceInfo, LogTrace);
end;

function TSQLMonitor.InvokeCallBack(TraceInfo: TDBXTraceInfo): CBRType;
var
  LogTrace: Boolean;
  Msg: string;
begin
  Result := cbrUSEDEF;
  if csDestroying in ComponentState then exit;
  LogTrace := (TraceInfo.TraceFlag and FTraceFlags) = 0;
  Trace(TraceInfo, LogTrace);
  if LogTrace then
  begin
    Msg := TraceInfo.Message;
    if (FMaxTraceCount = -1) or (TraceCount < FMaxTraceCount) then
      FTraceList.Add(Msg);
    if Assigned(FOnLogTrace) then
      FOnLogTrace(Self, TraceInfo);
    if FAutoSave and (FFileName <> '') then
      SaveToFile('');
  end;
end;


procedure TSQLMonitor.UpdateTraceCallBack;
begin
  if Assigned(FSQLConnection) then
  begin
    if Assigned(FSQLConnection.DBXConnection) then
    begin
      FSQLConnection.SetTraceEvent(InvokeCallback);
    end
    else
      FSQLConnection.SetTraceEvent(nil);
  end;
end;

procedure TSQLMonitor.SetActive(Value: Boolean);
var
  FileHandle: THandle;
begin
  if FActive <> Value then
  begin
    if (csReading in ComponentState) then
      FStreamedActive := Value
    else begin
      if not (csDestroying in ComponentState) and not Assigned(FSQLConnection) then
        DatabaseError(SConnectionNameMissing)
      else
      begin
        if Value and (FileName <> '') then
        begin
          if not FileExists(FileName) then
          begin
            FileHandle := FileCreate(FileName);
            if FileHandle = INVALID_HANDLE_VALUE then
              DatabaseErrorFmt(SCannotCreateFile, [FileName])
            else
              FileClose(FileHandle);
          end;
        end;
        if Assigned(FSQLConnection) then
        begin
          if Value then
            UpdateTraceCallBack
          else
            FSQLConnection.SetTraceEvent(nil);
        end;
        FActive := Value;
      end;
    end;
  end;
end;

procedure TSQLMonitor.SetStreamedActive;
begin
  if FStreamedActive then
    SetActive(True);
end;

function TSQLMonitor.GetTraceCount: Integer;
begin
  Result := FTraceList.Count;
end;

procedure TSQLMonitor.LoadFromFile(AFileName: string);
begin
  if AFileName <> '' then
    FTraceList.LoadFromFile(AFileName)
  else if FFileName <> '' then
    FTraceList.LoadFromFile(string(FFileName))
  else
    DatabaseError(SFileNameBlank);
end;

procedure TSQLMonitor.SaveToFile(AFileName: string);
begin
  if AFileName <> '' then
    FTraceList.SaveToFile(AFileName)
  else if FFileName <> '' then
    FTraceList.SaveToFile(FFileName)
  else
    DatabaseError(SFileNameBlank);
end;

procedure TSQLMonitor.SetTraceList(Value: TStrings);
begin
  if FTraceList <> Value then
  begin
    FTraceList.BeginUpdate;
    try
      FTraceList.Assign(Value);
    finally
      FTraceList.EndUpdate;
    end;
  end;
end;


{ TSQLConnection }

constructor TSQLConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnectionData := TConnectionData.Create(Self);
  FParams := TStringList.Create;
  TStringList(FParams).OnChange := DoParamsChange;
  FAutoClone := True;
  try
    FConnectionRegistryFile := GetConnectionRegistryFile(csDesigning in ComponentState);
  except
    FConnectionRegistryFile := '';
  end;
  FKeepConnection := True;
  FMonitorUsers := TList<TSQLMonitor>.Create;
  FSQLHourGlass := True;
  FQuoteChar := '';
  FProcedureQuoteChar := '';
  FTableScope := [tsTable, tsView];
  LoginPrompt := True;
  FLoginUserName := '';
  FDBXConnection := nil;                                  
  FValidateCertificate := nil;
end;

destructor TSQLConnection.Destroy;
begin
  Destroying;
  ClearConnectionUsers;
  Close;
  ClearMonitors;
  FreeAndNil(FMonitorUsers);
  if Assigned(FDBXConnection) then
    FreeAndNil(FDBXConnection);
  inherited Destroy;
  FreeAndNil(FParams);
  FreeAndNil(FConnectionData);
end;

{ user registration }

procedure TSQLConnection.ClearConnectionUsers;
begin
  while DataSetCount > 0 do
  begin
    if TCustomSQLDataSet(DataSets[0]).Active then
      TCustomSQLDataSet(DataSets[0]).Close;
       TCustomSQLDataSet(DataSets[0]).FreeCommand;
       TCustomSQLDataSet(DataSets[0]).SetConnection(nil);
    end;
  end;

procedure TSQLConnection.ClearMonitors;
var
  I: Integer;
begin
  for I := 0 to FMonitorUsers.Count -1 do
  begin
    if Self.FIsCloned then
      FMonitorUsers[I].SwitchConnection(Self.FCloneParent)
    else
    begin
      FMonitorUsers[I].SetActive(False);
      FMonitorUsers[I].FSQLConnection := nil;
    end;
  end;
end;

procedure TSQLConnection.RegisterTraceMonitor(Client: TObject);
begin
  FMonitorUsers.Add(TSQLMonitor(Client));
end;

procedure TSQLConnection.UnregisterTraceMonitor(Client: TObject);
begin
  FMonitorUsers.Remove(TSQLMonitor(Client));
end;

procedure TSQLConnection.CheckConnection(eFlag: eConnectFlag);
begin
  if (eFlag in [eDisconnect, eReconnect]) then
    Close;
  if (eFlag in [eConnect, eReconnect]) then
    Open
end;

procedure TSQLConnection.Login(ConnectionProps: TStrings);
var
  UserName, Password: string;

  function Login: Boolean;
  begin
    Result := Assigned(FOnLogin);
    if Result then FOnLogin(Self, ConnectionProps);
  end;

begin
  if not Login then
  begin
    UserName := ConnectionProps.Values[TDBXPropertyNames.UserName];
    if Assigned(LoginDialogExProc) then
    begin
      SetCursor(DefaultCursor);
      if not LoginDialogExProc(ConnectionName, UserName, Password, False) then
        DatabaseErrorFmt(SLoginError, [ConnectionName]);
      SetCursor(HourGlassCursor);
      ConnectionProps.Values[TDBXPropertyNames.UserName] := UserName;
      ConnectionProps.Values[szPASSWORD] := Password;
    end;
  end;
end;

procedure TSQLConnection.CheckLoginParams;
var
  I: Integer;
begin
  if FLoadParamsOnConnect then
  begin
    LoadParamsFromIniFile;
    FDriverName := GetProfileString(FConnectionName, DRIVERNAME_KEY, ConnectionRegistryFile);
  end;
  if FDriverName = '' then DataBaseError(SMissingDriverName);
  for I := 0 to FMonitorUsers.Count -1 do
    FMonitorUsers[I].SetStreamedActive;
end;

function TSQLConnection.GetQuoteChar: string;
begin
  if Assigned(MetaData) then
    Result := MetaData.QuoteChar
  else
    Result := '"';
  FQuoteChar := Result;
end;

procedure TSQLConnection.SetCursor(CursorType: Integer);
begin
  if SQLHourGlass or (CursorType = DefaultCursor) then
    if Assigned(ScreenCursorProc) then
      ScreenCursorProc(CursorType);
end;

procedure TSQLConnection.ConnectionOptions;
begin
  FQuoteChar := GetQuoteChar;
  if Assigned(MetaData) then
  begin
    FProcedureQuoteChar     := MetaData.ProcedureQuoteChar;
    FTransactionsSupported  := MetaData.SupportsTransactions;
    FSupportsNestedTrans    := MetaData.SupportsNestedTransactions;
    FMaxStmtsPerConn        := MetaData.MaxCommands;
  end
  else
  begin
    FProcedureQuoteChar     := '"';
    FTransactionsSupported  := True;
    FSupportsNestedTrans    := False;
    FMaxStmtsPerConn        := 0;
  end;
end;

procedure TSQLConnection.DefineProperties(Filer: TFiler);

  function DesignerDataStored: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := TSQLConnection(Filer.Ancestor).UniqueId <> UniqueId else
      Result := UniqueId <> '';
  end;

begin
  inherited;
  Filer.DefineProperty('UniqueId', ReadUniqueId, WriteUniqueId,
    DesignerDataStored);
  Filer.DefineProperty('GetDriverFunc', ReadGetDriverFunc, nil, False);
  Filer.DefineProperty('LibraryName', ReadLibraryName, nil, False);
  Filer.DefineProperty('VendorLib', ReadVendorLib, nil, False);
end;

procedure TSQLConnection.ReadUniqueId(Reader: TReader);
begin
  FUniqueID := Reader.ReadString;
end;

procedure TSQLConnection.WriteUniqueId(Writer: TWriter);
begin
  Writer.WriteString(FUniqueID);
end;

procedure TSQLConnection.ReadGetDriverFunc(Reader: TReader);
begin
  FGetDriverFunc := Reader.ReadString;
end;

procedure TSQLConnection.ReadLibraryName(Reader: TReader);
begin
  FLibraryName := Reader.ReadString;
end;

procedure TSQLConnection.ReadVendorLib(Reader: TReader);
begin
  FVendorLib := Reader.ReadString;
end;

procedure TSQLConnection.DoConnect;
var
  ConnectionProps: TDBXProperties;
  Ind: Integer;
  ConnectionFactory: TDBXConnectionFactory;
  LoginParams: TStrings;
  SchemaOverride: string;
  SchemaOverrideList: TStringList;
  Password: string;
  MemoryConnectionFactory: TDBXMemoryConnectionFactory;
begin
  ConnectionProps   := nil;
  LoginParams := TStringList.Create;
  MemoryConnectionFactory := nil;
  try
    if LoadParamsOnConnect then
    begin
      ConnectionFactory := TDBXConnectionFactory.GetConnectionFactory;
      TDBXConnectionFactory.Lock;
      try
        ConnectionProps   := ConnectionFactory.GetConnectionProperties(ConnectionName);
        ConnectionProps   := ConnectionProps.Clone;
      finally
        TDBXConnectionFactory.Unlock;
      end;
    end else
    begin
      ConnectionProps   := TDBXProperties.Create;
      try
        ConnectionFactory := TDBXConnectionFactory.GetConnectionFactory;
      except
        MemoryConnectionFactory := TDBXMemoryConnectionFactory.Create;
        ConnectionFactory := MemoryConnectionFactory;
        ConnectionFactory.Open;
      end;
      ConnectionProps.AddProperties(FParams);
      if ConnectionProps.Properties.IndexOfName(TDBXPropertyNames.DriverName) = -1 then
        ConnectionProps.Add(TDBXPropertyNames.DriverName, DriverName);
    end;
    CheckLoginParams;
    ConnectionState := csStateConnecting;

    GetLoginParams(LoginParams);
    if LoginParams.Values[TDBXPropertyNames.Database] <> ConnectionProps[TDBXPropertyNames.Database] then
    begin
      ConnectionProps[TDBXPropertyNames.Database] := LoginParams.Values[TDBXPropertyNames.Database];
    end;

    SetCursor(HourGlassCursor);

                                                                                                
{$IF NOT (DEFINED(IOS) or DEFINED(ANDROID))}
    ConnectionProps.Add('UNLICENSED_DRIVERS', IntToStr(GDAL)); // Do not translate.
{$ENDIF}

    FLoginUsername := LoginParams.Values[TDBXPropertyNames.UserName];
    if FLoginUserName <> '' then
      ConnectionProps[TDBXPropertyNames.UserName] := FLoginUsername;
    Password := LoginParams.Values[TDBXPropertyNames.Password];
    if Password <> '' then
      ConnectionProps[TDBXPropertyNames.Password] := Password;
    ConnectionProps.SetComponentOwner(self);
    ConnectionProps.Events.Events[sValidatePeerCertificate] := TEventPointer(ValidatePeerCertificate);

    ConnectionFactory.Lock;
    try
      FDBXConnection := ConnectionFactory.GetConnection(ConnectionProps);
    finally
      ConnectionFactory.Unlock;
    end;

    for Ind := 0 to FMonitorUsers.Count -1 do
      FMonitorUsers[Ind].UpdateTraceCallBack;

    SetCursor(HourGlassCursor);

    RegisterTraceCallback(True);

    FDefaultSchema := '';

    if (FDBXConnection.ProductName = 'BlackfishSQL') then {Do not localize}
    begin
      FDefaultSchema := 'DEFAULT_SCHEMA'; { Do not localize }
    end;

    SchemaOverride := ConnectionProps[TDBXPropertyNames.SchemaOverride];

    if (SchemaOverride = '') and LoadParamsOnConnect then
    begin
      TDBXConnectionFactory.Lock;
      try
        SchemaOverride := ConnectionFactory.GetDriverProperties(ConnectionProps[TDBXPropertyNames.DriverName])
                          [TDBXPropertyNames.SchemaOverride];
      finally
        TDBXConnectionFactory.Unlock;
      end;
    end;

    if SchemaOverride <> '' then
    begin
      SchemaOverrideList := TStringList.Create;
      try
        SchemaOverrideList.Delimiter := '.';
        SchemaOverrideList.DelimitedText := SchemaOverride;
        if SchemaOverrideList.Count = 2 then
        begin
          if (SchemaOverrideList[0] = '%') or (SchemaOverrideList[0] = FLoginUsername) then
            FDefaultSchema := SchemaOverrideList[1];
        end;
      finally
        SchemaOverrideList.Free;
      end;
    end;

    ConnectionOptions;

    ConnectionState := csStateOpen;
  finally
    FreeAndNil(MemoryConnectionFactory); // If allocated, free it.
    SetCursor(DefaultCursor);
    LoginParams.Free;
    ConnectionProps.Free;
    if ConnectionState = csStateConnecting then // an exception occurred
    begin
      ConnectionState := csStateClosed;
      if Assigned(FDBXConnection) then
        FreeAndNil(FDBXConnection)
    end;
  end;
end;

function TSQLConnection.GetLoginUsername : string;
begin
  Result := FLoginUserName;
end;

function TSQLConnection.GetMetaData: TDBXDatabaseMetaData;
begin
  Result := DBXConnection.DatabaseMetaData;
end;

procedure TSQLConnection.GetLoginParams(LoginParams: TStrings);
var
  DriverProps, MergedProps: TDBXProperties;
begin
  LoginParams.BeginUpdate;
  try
    LoginParams.Clear;
    DriverProps := TDBXConnectionFactory.GetConnectionFactory.GetDriverProperties(DriverName);
    if Assigned(DriverProps) then
      MergedProps := DriverProps.Clone
    else
      MergedProps := TDBXProperties.Create(nil);
    try
      MergedProps.MergeProperties(FParams);
      LoginParams.Values[TDBXPropertyNames.Password] := MergedProps.Values[TDBXPropertyNames.Password];
      LoginParams.Values[TDBXPropertyNames.UserName] := MergedProps.Values[TDBXPropertyNames.UserName];
      LoginParams.Values[TDBXPropertyNames.Database] := MergedProps.Values[TDBXPropertyNames.Database];
    finally
      MergedProps.Free;
    end;
  finally
    LoginParams.EndUpdate;
  end;
  if LoginPrompt then
    Login(LoginParams);
end;

procedure TSQLConnection.GetCommandTypes(List: TStrings);
begin
  FDBXConnection.GetCommandTypes(List);
end;

function TSQLConnection.GetConnected: Boolean;
begin
  Result := Assigned(FDBXConnection) and (not
      (ConnectionState in [csStateClosed, csStateConnecting,
      csStateDisconnecting]));
end;

procedure TSQLConnection.DoDisconnect;
begin
  if FDBXConnection <> nil then
  begin
    ConnectionState := csStateDisconnecting;
    CloseDataSets;
    RegisterTraceCallback(False);
    if (FDBXConnection <> nil) then
    begin
      FTransactionCount := 0;
      FreeAndNil(FDBXConnection)
    end;
    SQLDllHandle := THandle(0);
    ConnectionState := csStateClosed;
    FSelectStatements := 0;
//    FPrevSelectStatements := 0;
  end;
  FParamsLoaded := False;
end;

procedure TSQLConnection.DoParamsChange(Sender: TObject);
begin
  if Assigned(ConnectionData.Properties) then
  begin
    if ConnectionData.BeginUpdate then
    begin
      try
        ConnectionData.Properties.Clear;
        ConnectionData.AddProperties(FParams);
      finally
        ConnectionData.EndUpdate;
      end;
    end;
  end;
end;

procedure TSQLConnection.CloseDataSets;
var
  I: Integer;
begin
  for I := 0 to DataSetCount -1 do
  begin
    if TCustomSQLDataSet(DataSets[i]).Active then
      TCustomSQLDataSet(DataSets[i]).Close;
    TCustomSQLDataSet(DataSets[i]).FreeCommand;
  end;
  for I := 0 to FMonitorUsers.Count -1 do
  begin
    if Self.FIsCloned then
      FMonitorUsers[I].SwitchConnection( Self.FCloneParent );
  end;
end;

procedure TSQLConnection.CheckDisconnect;
var
  I: Integer;
begin
  if Connected and not (KeepConnection or InTransaction or (csLoading in ComponentState)) then
  begin
    for I := 0 to DataSetCount - 1 do
      if (DataSets[I].State <> dsInActive) then Exit;
    Close;
  end;
end;

procedure TSQLConnection.CheckInactive;
begin
  if FDBXConnection <> nil then
    if csDesigning in ComponentState then
      Close
    else
      DatabaseError(SdatabaseOpen, Self);
end;



procedure TSQLConnection.CheckActive;
begin
  if FDBXConnection = nil then DatabaseError(SDatabaseClosed, Self);
end;

{ Query execution }

function TSQLConnection.GetConnectionForStatement: TSQLConnection;
begin
  if (FMaxStmtsPerConn > 0) and (FSelectStatements >= FMaxStmtsPerConn)
       and not (FTransactionCount > 0) and AutoClone then
    Result := CloneConnection
  else
    Result := Self;
end;

function TSQLConnection.ExecuteDirect(const SQL: string): Integer;
var
  Command: TDBXCommand;
  Reader: TDBXReader;
  Connection: TSQLConnection;
  RowsetSize: Integer;
  CurSection : TSqlToken;
  Value: string;
  Start: NativeInt;
begin
  CheckConnection(eConnect);
//  Reader := nil;
//  Result := 0;
  RowsetSize := defaultRowsetSize;
  CurSection := stUnknown;
  Start := 1;
  CurSection := Platform_NextSQLToken(SQL, Start, Value, CurSection, GetIdOption(Self));
  if CurSection = stSelect then
    Inc(FSelectStatements);
  Connection := GetConnectionForStatement;
  Command := Connection.FDBXConnection.CreateCommand;
  Reader := nil;
  try

    if Params.Values[ROWSETSIZE_KEY] <> '' then
    try
      RowsetSize := StrToInt(trim(Params.Values[ROWSETSIZE_KEY]));
    except
      RowsetSize := defaultRowsetSize;
    end;
    if Assigned(MetaData) and MetaData.SupportsRowSetSize then
      Command.RowSetSize := RowsetSize;

    Command.Text := SQL;
                                                          
    Command.Prepare;
    Reader       := Command.ExecuteQuery;
    Result := Integer(Command.RowsAffected);

  finally
    Reader.Free;
    Command.DisposeOf;
    if (Connection <> nil) and Connection.FIsCloned then
      Connection.Free;
  end;
end;

function TSQLConnection.Execute(const SQL: string; Params: TParams): Integer;
var
  SQLText: string;
  DS: TCustomSQLDataSet;
  ParamCount: Integer;
begin
  DS := TCustomSQLDataSet.Create(nil);
  try
    CheckConnection(eConnect);
    SetCursor(HourGlassCursor);
    DS.SQLConnection := Self;
    ConnectionState := csStateExecuting;
    if (Params <> nil) and (Params.Count > 0) then
    begin
      SQLText := FixParams(SQL, Params.Count, Self.GetQuoteChar);
      ParamCount := Params.Count;
    end
    else
    begin
      SQLText := SQL.Substring(0, SQL.Length);
      ParamCount := 0;
    end;
    DS.FCommandText := SQLText;
    DS.CheckStatement;
    DS.FDBXCommand.Text := SQLText;
    if Params <> nil then
      DS.FDBXCommand.Parameters.SetCount(Params.Count);
    DS.FDBXCommand.Prepare;
    if ParamCount > 0 then
      SetQueryProcParams(Self, DS.FDBXCommand, Params);
    DS.FDBXReader :=  DS.FDBXCommand.ExecuteQuery;
    Result := DS.FDBXCommand.RowsAffected;
  finally
    SetCursor(DefaultCursor);
    DS.Free;
    ConnectionState := csStateOpen;
  end;
end;

function TSQLConnection.Execute(const SQL: string; Params: TParams;
  var ResultSet: TDataSet): Integer;
var
  SQLText: string;
  DS: TCustomSQLDataSet;
  I, ParamCount: Integer;
begin
  Result := 0;
  DS := TCustomSQLDataSet.Create(nil);
  try
    CheckConnection(eConnect);
    SetCursor(HourGlassCursor);
    DS.SQLConnection := Self;
    ConnectionState := csStateExecuting;
    if (Params <> nil) and (Params.Count > 0) then
    begin
      SQLText := FixParams(SQL, Params.Count, GetQuoteChar);
      ParamCount := Params.Count;
    end
    else
    begin
      SQLText := SQL.Substring(0, SQL.Length);
      ParamCount := 0;
    end;
    DS.FCommandText := SQLText;
    if ParamCount > 0 then
    begin
      for I := 0 to ParamCount -1 do
      begin
        DS.Params.CreateParam(Params.Items[I].DataType, string.Format('P%d',[I+1]), ptInput);
        DS.Params[I].Value := Params[I].Value;
      end;
    end;
    DS.MaxBlobSize := DefaultMaxBlobSize;
    DS.Active := True;
  finally
    SetCursor(DefaultCursor);
    ResultSet := DS;
    ConnectionState := csStateOpen;
  end;
end;

{$IFNDEF NEXTGEN}
function TSQLConnection.Execute(const SQL: string; Params: TParams;
      ResultSet: Pointer): Integer;
begin
  if ResultSet = nil then
    Result := Execute(SQL, Params)
  else
    Result := Execute(SQL, Params, TDataSet(ResultSet^));
end;
{$ENDIF !NEXTGEN}

{ Metadata retrieval }

function TSQLConnection.CloneConnection: TSQLConnection;
var
  SelfParent: TSQLConnection;
  I: Integer;
begin      // do not allow nested clones
  if Self.FIsCloned then
    SelfParent := Self.FCloneParent
  else
    SelfParent := Self;
  Result := TSQLConnection.Create(nil);
  Result.FIsCloned := True;
  Result.FLoadParamsOnConnect := FLoadParamsOnConnect;
  Result.LoginPrompt := False;
  Result.FDriverName := SelfParent.FDriverName;
  Result.FConnectionName := SelfParent.FConnectionName;
  Result.Name := SelfParent.Name + 'Clone1';
  Result.FParams.AddStrings(SelfParent.FParams);
  Result.FGetDriverFunc := SelfParent.FGetDriverFunc;
  Result.FLibraryName := SelfParent.FLibraryName;
  Result.FVendorLib := SelfParent.VendorLib;

  // This getter is not implemented in any of our dbx drivers.
//  Len := 0;
//  Status := FDBXConnection.getOption(eConnConnectionString, nil, 0, Len); // Len is number of byte.
//  if (Status <> 0) or (Len <= 0) then
//    Len := 1024;
//  SetLength(buf, Len div sizeof(WideChar));
//  FillChar(buf[1], Len div sizeof(WideChar), #0);
//  Status := FDBXConnection.getStringOption(eConnConnectionString, buf);
//  if Status = 0 then
//    Result.Params.Values[CONNECTION_STRING] := PWideChar(buf);

//  Result.FConnectionFactory := SelfParent.FConnectionFactory;
  Result.FTableScope := SelfParent.TableScope;
  for I := 0 to FMonitorUsers.Count -1 do
    FMonitorUsers[I].SwitchConnection( Result );
  Result.Connected := Self.Connected;
  Result.FCloneParent := SelfParent;
end;

function TSQLConnection.OpenSchemaTable(eKind: TSchemaType; SInfo: string; SQualifier: string = ''; SPackage: string = ''): TCustomSQLDataSet;
begin
  Result := OpenSchemaTable(eKind, SInfo, SQualifier, SPackage , '');
end;

function TSQLConnection.OpenSchemaTable(eKind: TSchemaType; SInfo: string; SQualifier: string = ''; SPackage: string = ''; SSchemaName: string = ''): TCustomSQLDataSet;
var
  DataSet: TCustomSQLDataSet;
begin
  CheckConnection(eConnect);
  DataSet := TCustomSQLDataSet.Create(nil);
  Result := nil;
  try
    Inc(FSelectStatements);
    DataSet.SetConnection(Self);
    DataSet.SetSchemaInfo(eKind, SInfo, SQualifier, SPackage);
    DataSet.SchemaName := SSchemaName;
    DataSet.Active := True;
    Result := DataSet;
  finally
    if Result = nil then
      FreeSchemaTable(DataSet);
  end;
end;

procedure TSQLConnection.FreeSchemaTable(DataSet: TCustomSQLDataSet);
var
  SaveKeepConnection: Boolean;
begin
//  if Assigned(Dataset) then
//    FreeAndNil(DataSet.FClonedConnection);
  Dec(FSelectStatements);
  SaveKeepConnection := FKeepConnection;
  FKeepConnection := True;
  if Assigned(Dataset) then
  begin
    DataSet.DisposeOf;
    DataSet := nil;
  end;
  FKeepConnection := SaveKeepConnection;
end;

procedure TSQLConnection.OpenSchema(eKind: TSchemaType; sInfo: string; List: TStrings);
begin
  OpenSchema(eKind, sInfo, '', List);
end;

function GetTableFieldName(SqlConnection: TSQLConnection): string;
var
  MetaData: TDBXDatabaseMetaData;
begin
  MetaData := SqlConnection.FDBXConnection.DatabaseMetaData;
  if Assigned(MetaData) and (MetaData.MetaDataVersion = DBXVersion40) then
    Result := 'TableName'
  else
    Result := 'TABLE_NAME';
end;

function GetProcedureFieldName(SqlConnection: TSQLConnection): string;
var
  MetaData: TDBXDatabaseMetaData;
begin
  MetaData := SqlConnection.FDBXConnection.DatabaseMetaData;
  if Assigned(MetaData) and (MetaData.MetaDataVersion = DBXVersion40) then
    Result := 'ProcedureName'
  else
    Result := 'PROC_NAME';
end;

function GetColumnFieldName(SqlConnection: TSQLConnection): string;
var
  MetaData: TDBXDatabaseMetaData;
begin
  MetaData := SqlConnection.FDBXConnection.DatabaseMetaData;
  if Assigned(MetaData) and (MetaData.MetaDataVersion = DBXVersion40) then
    Result := 'ColumnName'
  else
    Result := 'COLUMN_NAME';
end;

function GetIndexFieldName(SqlConnection: TSQLConnection): string;
var
  MetaData: TDBXDatabaseMetaData;
begin
  MetaData := SqlConnection.FDBXConnection.DatabaseMetaData;
  if Assigned(MetaData) and (MetaData.MetaDataVersion = DBXVersion40) then
    Result := TDBXIndexesColumns.IndexName
  else
    Result := 'INDEX_NAME';
end;

function GetSchemaFieldName(SqlConnection: TSQLConnection): string;
var
  MetaData: TDBXDatabaseMetaData;
begin
  MetaData := SqlConnection.FDBXConnection.DatabaseMetaData;
  if Assigned(MetaData) and (MetaData.MetaDataVersion = DBXVersion40) then
    Result := 'SchemaName'
  else
    Result := 'OBJECT_NAME';
end;

function GetPackageFieldName(SqlConnection: TSQLConnection): string;
var
  MetaData: TDBXDatabaseMetaData;
begin
  MetaData := SqlConnection.FDBXConnection.DatabaseMetaData;
  if Assigned(MetaData) and (MetaData.MetaDataVersion = DBXVersion40) then
    Result := 'PackageName'
  else
    Result := 'OBJECT_NAME';
end;

function IsResultSetParameterType(SqlConnection: TSQLConnection; DataSet: TCustomSQLDataSet): Boolean;
var
  MetaData: TDBXDatabaseMetaData;
  Mode: string;
begin
  Result := False;
  MetaData := SqlConnection.FDBXConnection.DatabaseMetaData;
  if Assigned(MetaData) and (MetaData.MetaDataVersion = DBXVersion40) then
  begin
    Mode := DataSet.FieldByName(TDBXProcedureParametersColumns.ParameterMode).AsString;
    Result := (Mode = 'RESULT');
  end;
end;

function GetParameterType(SqlConnection: TSQLConnection; DataSet: TCustomSQLDataSet; V:Variant): TParamType;
var
  MetaData: TDBXDatabaseMetaData;
  Mode: string;
begin
  MetaData := SqlConnection.FDBXConnection.DatabaseMetaData;
  if Assigned(MetaData) and (MetaData.MetaDataVersion = DBXVersion40) then
  begin
    Mode := DataSet.FieldByName(TDBXProcedureParametersColumns.ParameterMode).AsString;
    if (Mode = 'IN') or (Mode = '1') then
      Result := ptInput
    else if (Mode = 'OUT') or (Mode = '4') then
      Result := ptOutput
    else if (Mode = 'INOUT') or (Mode = '2') then
      Result := ptInputOutput
    else if (Mode = 'RESULT') or (Mode = 'RETURN') or (Mode = '3') or (Mode = '5') then
      Result := ptResult
    else
      Result := ptUnknown;
  end
  else
  begin
    V := DataSet.FieldByName('PARAM_TYPE').Value;
    if VarIsNull(V) then
      Result := ptUnknown
    else
      Result := TParamType(Integer(V));
  end;
end;

function GetParameterDataTypeFieldName(SqlConnection: TSQLConnection): string;
var
  MetaData: TDBXDatabaseMetaData;
begin
  MetaData := SqlConnection.FDBXConnection.DatabaseMetaData;
  if Assigned(MetaData) and (MetaData.MetaDataVersion = DBXVersion40) then
    Result := 'DbxDataType'         { do not localize }
  else
    Result := 'PARAM_DATATYPE';     { do not localize }
end;

function IsParameterFixedStringType(SqlConnection: TSQLConnection; DataSet: TCustomSQLDataSet; V:Variant): Boolean;
var
  MetaData: TDBXDatabaseMetaData;
begin
  MetaData := SqlConnection.FDBXConnection.DatabaseMetaData;
  if Assigned(MetaData) and (MetaData.MetaDataVersion = DBXVersion40) then
  begin
    V := DataSet.FieldByName('IsFixedLength').Value;
    if VarIsNull(V) then
      Result := False
    else
      Result := Boolean(V);
  end
  else
  begin
    V := DataSet.FieldByName('PARAM_SUBTYPE').Value;
    if VarIsNull(V) then
      Result := False
    else
      Result := (V = TDBXDataTypes.FixedSubType);
  end;
end;

function GetParameterPositionFieldName(SqlConnection: TSQLConnection): string;
var
  MetaData: TDBXDatabaseMetaData;
begin
  MetaData := SqlConnection.FDBXConnection.DatabaseMetaData;
  if Assigned(MetaData) and (MetaData.MetaDataVersion = DBXVersion40) then
    Result := 'Ordinal'                  { do not localize }
  else
    Result := 'PARAM_POSITION';          { do not localize }
end;

function GetParameterPrecisionFieldName(SqlConnection: TSQLConnection): string;
var
  MetaData: TDBXDatabaseMetaData;
begin
  MetaData := SqlConnection.FDBXConnection.DatabaseMetaData;
  if Assigned(MetaData) and (MetaData.MetaDataVersion = DBXVersion40) then
    Result := 'Precision'          { do not localize }
  else
    Result := 'PARAM_PRECISION';   { do not localize }
end;

function GetParameterScaleFieldName(SqlConnection: TSQLConnection): string;
var
  MetaData: TDBXDatabaseMetaData;
begin
  MetaData := SqlConnection.FDBXConnection.DatabaseMetaData;
  if Assigned(MetaData) and (MetaData.MetaDataVersion = DBXVersion40) then
    Result := 'Scale'                  { do not localize }
  else
    Result := 'PARAM_SCALE';           { do not localize }
end;

function GetParameterLengthFieldName(SqlConnection: TSQLConnection): string;
var
  MetaData: TDBXDatabaseMetaData;
begin
  MetaData := SqlConnection.FDBXConnection.DatabaseMetaData;
  if Assigned(MetaData) and (MetaData.MetaDataVersion = DBXVersion40) then
    Result := 'Precision'             { do not localize }
  else
    Result := 'PARAM_LENGTH';         { do not localize }
end;

function GetParameterNameFieldName(SqlConnection: TSQLConnection): string;
var
  MetaData: TDBXDatabaseMetaData;
begin
  MetaData := SqlConnection.FDBXConnection.DatabaseMetaData;
  if Assigned(MetaData) and (MetaData.MetaDataVersion = DBXVersion40) then
    Result := 'ParameterName'      { do not localize }
  else
    Result := 'PARAM_NAME';        { do not localize }
end;
//  SubTypeFieldName = 'PARAM_SUBTYPE';       { do not localize }

procedure TSQLConnection.OpenSchema(eKind: TSchemaType; sInfo, SSchemaName: string; List: TStrings);
var
  DataSet: TCustomSQLDataSet;
  NameField: TField;
  PackageName : string;
  ISList: TStringList;
begin
  CheckConnection(eConnect);
  if FDBXConnection = nil then
    DatabaseError(sConnectionNameMissing);
  DataSet := nil;
  NameField := nil;
  if eKind = stProcedures then
    PackageName := sInfo;
  CheckActive;
  SetCursor(HourGlassCursor);
  try
    DataSet := OpenSchemaTable(eKind, sInfo, '', PackageName, SSchemaName);
    if Assigned(DataSet) then
    begin
      case eKind of
        stColumns:
          NameField := DataSet.FieldByName(GetColumnFieldName(Self));
        stProcedures:
          NameField := DataSet.FieldByName(GetProcedureFieldName(Self));
        stPackages:
          NameField := DataSet.FieldByName(GetPackageFieldName(Self));
        stIndexes:
          NameField := DataSet.FieldByName(GetIndexFieldName(Self));
        stTables, stSysTables:
          NameField := DataSet.FieldByName(GetTableFieldName(Self));
        stUserNames:
          NameField := DataSet.FieldByName(GetSchemaFieldName(Self));
      end;
      if (not DataSet.Eof) then
      begin
        ISList:= TStringList.Create;
        try
          try
            ISList.BeginUpdate;
            ISList.Duplicates := dupIgnore;
            ISList.CaseSensitive := False;
            while not DataSet.Eof do
            begin
              ISList.Add(Trim(NameField.AsString));
              DataSet.Next;
            end;
            ISList.Sorted := True;
          finally
            ISList.EndUpdate;
          end;
          try
            List.BeginUpdate;
            List.Clear;
            List.AddStrings(ISList);
          finally
            List.EndUpdate;
          end;
        finally
          ISList.Free;
        end;
      end;
    end;
  finally
    SetCursor(DefaultCursor);
    if Assigned(DataSet) then FreeSchemaTable(DataSet);
  end;
end;

procedure TSQLConnection.GetFieldNames(const TableName: string; List: TStrings);
begin
  OpenSchema(stColumns, TableName, List);
end;

procedure TSQLConnection.GetFieldNames(const TableName: string; SchemaName: string; List: TStrings);
begin
  OpenSchema(stColumns, TableName, SchemaName, List);
end;

procedure TSQLConnection.GetProcedureNames(List: TStrings);
begin
  OpenSchema(stProcedures, '', List);
end;

procedure TSQLConnection.GetProcedureNames(const PackageName : string; List: TStrings);
begin
  OpenSchema(stProcedures, PackageName, '', List);
end;

procedure TSQLConnection.GetProcedureNames(const PackageName, SchemaName : string; List: TStrings);
begin
  OpenSchema(stProcedures, PackageName, SchemaName, List);
end;

procedure TSQLConnection.GetPackageNames(List: TStrings);
begin
  OpenSchema(stPackages, '', List);
end;

procedure TSQLConnection.GetSchemaNames(List: TStrings);
begin
  OpenSchema(stUserNames, '', List);
end;

procedure TSQLConnection.GetTableNames(List: TStrings; SystemTables: Boolean = False);
begin
  GetTableNames( List, '', SystemTables );
end;

procedure TSQLConnection.GetTableNames(List: TStrings; SchemaName: string; SystemTables: Boolean = False);
var
  eType: TSchemaType;
begin
  if SystemTables then
    eType := stSysTables
  else
    eType := stTables;
  OpenSchema(eType, '', SchemaName, List);
end;

procedure TSQLConnection.GetIndexNames(const TableName: string; List: TStrings);
begin
  OpenSchema(stIndexes, TableName, '', List);
end;

procedure TSQLConnection.GetIndexNames(const TableName, SchemaName: string; List: TStrings);
begin
  OpenSchema(stIndexes, TableName, SchemaName, List);
end;

// Jens Ole:
// Some databases (i.e. Oracle) does not specify data length and precision
// Set some reasonable values here for the buffer sizes....
// We should probably inspect the input value !!
procedure AdjustProcedureDataLength(ArgDesc: SPParamDesc);
begin
  if ArgDesc.iLen = 0 then
  begin
    case ArgDesc.iDataType of
      ftUnknown:     ArgDesc.iLen := 40;  //probably never used
      ftFixedChar,
      ftFixedWideChar,
      ftWideString,
      ftString:      ArgDesc.iLen := 2000;
      ftByte:        ArgDesc.iLen := sizeof(Byte);
      ftShortint:    ArgDesc.iLen := sizeof(ShortInt);
      ftSmallint:    ArgDesc.iLen := sizeof(SmallInt);
      ftLargeint:    ArgDesc.iLen := sizeof(LargeInt);
      ftInteger:     ArgDesc.iLen := sizeof(Integer);
      ftWord:        ArgDesc.iLen := sizeof(Word);
      ftBoolean:     ArgDesc.iLen := sizeof(Integer);
      ftAutoInc,
      ftCurrency,
      ftFMTBcd,
      ftBCD:         ArgDesc.iLen := Data.FmtBcd.SizeOfTBCD;
      ftSingle:      ArgDesc.iLen := sizeof(Single);
      ftFloat,
      ftDate,
      ftTime,
      ftTimeStamp,
      ftDateTime:    ArgDesc.iLen := sizeof(Double);
      ftTimeStampOffset: ArgDesc.iLen := sizeof(TSQLTimeStampOffset);
      ftOraTimeStamp,
      ftOraInterval: ArgDesc.iLen := 16;
    else
      ArgDesc.iLen := 8000;
    end;
    if ArgDesc.iUnits1 = 0 then
      ArgDesc.iUnits1 := ArgDesc.iLen;
  end;
end;

procedure TSQLConnection.GetProcedureParams(ProcedureName: string; List: TProcParamList);
begin
  GetProcedureParams(ProcedureName, '', List);
end;

procedure TSQLConnection.GetProcedureParams(ProcedureName, PackageName: string; List: TProcParamList);
begin
  GetProcedureParams(ProcedureName, PackageName, '',  List);
end;

procedure TSQLConnection.GetProcedureParams(ProcedureName, PackageName, SchemaName: string; List: TProcParamList);
const
  ResultParam = 'Result';                   { Do not localize }
var
  DataSet: TCustomSQLDataSet;
  ArgDesc: SPParamDesc;
  V: Variant;
begin
                                        
  DataSet := nil;
  try
    DataSet := OpenSchemaTable(stProcedureParams, ProcedureName,'', PackageName, SchemaName);
    while not DataSet.EOF do
    begin
      ArgDesc := SPParamDesc.Create;
      ArgDesc.iParamNum := DataSet.FieldByName(GetParameterPositionFieldName(self)).Value;
      ArgDesc.iArgType := GetParameterType(self,DataSet,V);

      V := DataSet.FieldByName(GetParameterDataTypeFieldName(self)).Value;
      if IsResultSetParameterType(self,DataSet) and (V = TDBXDataTypes.ObjectType) then
        FreeAndNil(ArgDesc)
      else
      begin
        if VarIsNull(V) then
          ArgDesc.iDataType := ftUnknown
        else if V < TDBXDataTypes.MaxBaseTypes then
          ArgDesc.iDataType := DataTypeMap[Integer(V)]
        else
          ArgDesc.iDataType := ftUnknown;
        if IsParameterFixedStringType(self,DataSet,V) then
        begin
          if ArgDesc.iDataType = ftString then
            ArgDesc.iDataType := ftFixedChar
          else if ArgDesc.iDataType = ftWideString then
            ArgDesc.iDataType := ftFixedWideChar;
        end;
        V := DataSet.FieldByName(GetParameterPrecisionFieldName(self)).Value;
        if VarIsNull(V) then
          ArgDesc.iUnits1 := 0
        else
          ArgDesc.iUnits1 := V;
        V := DataSet.FieldByName(GetParameterScaleFieldName(self)).Value;
        if VarIsNull(V) then
          ArgDesc.iUnits2 := 0
        else
          ArgDesc.iUnits2 := V;
        if ArgDesc.iDataType = ftBCD then
        begin
          // dbExpress only supports ftFMTBcd
          ArgDesc.iDataType := ftFMTBcd
        end;
        V := DataSet.FieldByName(GetParameterLengthFieldName(self)).Value;
        if VarIsNull(V) or (V < 0) then
          ArgDesc.iLen := 0
        else
          ArgDesc.iLen := V;
        AdjustProcedureDataLength(ArgDesc);
        V := DataSet.FieldByName(GetParameterNameFieldName(self)).Value;
        if VarIsNull(V) then
          ArgDesc.szName := ResultParam
        else
          ArgDesc.szName := V;
        List.Add(ArgDesc);
      end;
      DataSet.next;
    end;
  finally
    FreeSchemaTable(DataSet);
  end;
end;

{ trace }

procedure TSQLConnection.SetTraceEvent(Event: TDBXTraceEvent);
begin
  FTraceCallbackEvent := Event;
  if Connected and not (csLoading in ComponentState) then
  begin
    RegisterTraceCallBack(Assigned(Event));
  end;
end;

procedure TSQLConnection.AddConnectNotification(Listener: TObject; Event: TConnectChangeEvent);
begin
  RegisterClient(Listener, Event);
end;

procedure TSQLConnection.RemoveConnectNotification(Listener: TObject);
begin
  UnregisterClient(Listener);
end;

procedure TSQLConnection.RegisterClient(Client: TObject;
  Event: TConnectChangeEvent);
begin
  inherited;

end;

procedure TSQLConnection.RegisterTraceCallback(Value: Boolean);
begin
  if (Value) then
  begin
    if Assigned(FTraceCallbackEvent) then
      FDBXConnection.OnTrace := FTraceCallbackEvent;
  end else
  begin
    if Assigned(FDBXConnection) then
      FDBXConnection.OnTrace := FTraceCallbackEvent;
  end;
end;

{ transaction support }

function TSQLConnection.GetInTransaction: Boolean;
begin
  Result := FTransactionCount > 0;
end;


function TSQLConnection.BeginTransaction: TDBXTransaction;
begin
  Result := BeginTransaction(TDBXIsolations.ReadCommitted);
end;

{$IFNDEF NEXTGEN}
procedure TSQLConnection.StartTransaction( TransDesc: TTransactionDesc);
var
  Isolation: TDBXIsolation;
begin
  case TransDesc.IsolationLevel of
    xilREADCOMMITTED:
      Isolation := TDBXIsolations.ReadCommitted;
    xilREPEATABLEREAD:
      Isolation := TDBXIsolations.RepeatableRead;
    xilDIRTYREAD:
      Isolation := TDBXIsolations.DirtyRead;
//    xilCUSTOM:
    else
      Isolation := TDBXIsolations.ReadCommitted;
  end;
  BeginTransaction(TransDesc, Isolation);
end;
{$ENDIF !NEXTGEN}

function TSQLConnection.BeginTransaction(TransDesc: TTransactionDesc;
  Isolation: TDBXIsolation): TDBXTransaction;
{$IFNDEF NEXTGEN}
var
  Item: TTransactionItem;
{$ENDIF !NEXTGEN}
begin
  Result := nil;
  CheckConnection(eConnect);
  if Connected then
  begin
    if FTransactionsSupported then
    begin
      CheckActive;
      if (not InTransaction) or FSupportsNestedTrans then
      begin
{$IFNDEF NEXTGEN}
        Item := TTransactionItem.Create;
        Item.FTransactionDesc := TransDesc;
        try
          Item.FTransaction := FDBXConnection.BeginTransaction(Isolation);
          Result := Item.FTransaction;
          Item.FNext := FTransactionStack;
          FTransactionStack := Item;
        finally
          if Item.FTransaction = nil then
            Item.Free;
        end;
{$ELSE}
        Result := FDBXConnection.BeginTransaction(Isolation);
{$ENDIF !NEXTGEN}
        Inc(FTransactionCount);
      end
      else
        DatabaseError(sActiveTrans, self)
    end;
  end
  else
    DatabaseError(SDatabaseClosed, Self);
end;

function TSQLConnection.BeginTransaction(
  Isolation: TDBXIsolation): TDBXTransaction;
var
  TransactionDesc: TTransactionDesc;
begin
  TransactionDesc.TransactionID := FTransactionCount + 1;
  if FTransactionsSupported then
{$IFNDEF NEXTGEN}
  begin
    BeginTransaction(TransactionDesc, Isolation);
    Result := FTransactionStack.FTransaction
  end
{$ELSE}
    Result := BeginTransaction(TransactionDesc, Isolation)
{$ENDIF !NEXTGEN}
  else
    Result := nil;
end;

function TSQLConnection.HasTransaction(Transaction: TDBXTransaction): Boolean;
begin
  Result := FDBXConnection.HasTransaction(Transaction);
end;

{$IFNDEF NEXTGEN}
procedure TSQLConnection.EndFreeAndNilTransaction(var Transaction: TDBXTransaction; Commit: Boolean);
var
  Item: TTransactionItem;
  TargetItem: TTransactionItem;
begin
  Item := FTransactionStack;
  TargetItem := Item;
  while Item <> nil do
  begin
    TargetItem := Item;
    if Item.FTransaction = Transaction then
      break;
    Item := FTransactionStack.FNext
  end;
  if TargetItem <> nil then
  begin
    EndAndFreeTransaction(TargetItem.FTransactionDesc, Commit);
    Transaction := nil;
  end
  else
  begin
    DatabaseError(sInvalidTransaction);
  end;
end;

procedure TSQLConnection.EndAndFreeTransaction(Commit: Boolean);
var
  Temp: TDBXTransaction;
begin
  if FTransactionsSupported then
  begin
    // Must put in temp since EndFreeAndNilTransaction will set
    // out (internal FTransctionStack element) reference to nil.
    //
    if Assigned(FTransactionStack) then
    begin
      Temp := FTransactionStack.FTransaction;
      EndFreeAndNilTransaction(Temp, Commit);
    end;
  end;
end;

procedure TSQLConnection.EndAndFreeTransaction(TransDesc: TTransactionDesc; Commit: Boolean);
var
  TargetTransaction:  TDBXTransaction;
  NextTransaction: TTransactionItem;
  Match: Boolean;
  Item: TTransactionItem;
begin
  if FTransactionsSupported then
  begin
    if InTransaction then
    begin
      if Assigned(FDBXConnection) then
      begin
        Item := FTransactionStack;
        TargetTransaction := nil;
        NextTransaction := nil;
        Match := False;
        while ((not Match) and (Item <> nil)) do
        begin
          NextTransaction := Item.FNext;
          TargetTransaction := Item.FTransaction;
          if Item.FTransactionDesc.TransactionID = TransDesc.TransactionID then
            Match := True
          else
          begin
            Item.Free;
            Item := NextTransaction;
          end;
        end;
        if TargetTransaction <> nil then
        begin
          if Commit then
            FDBXConnection.CommitFreeAndNil(TargetTransaction)
          else
            FDBXConnection.RollbackFreeAndNil(TargetTransaction);
          FreeAndNil(Item);
          FTransactionStack := NextTransaction;
        end;
        Dec(FTransactionCount);
      end
      else
        DatabaseError(SDatabaseClosed, Self);
    end
    else
      DatabaseError(sNoActiveTrans, self);
    CheckDisconnect;
  end;
end;
{$ENDIF !NEXTGEN}

{$IFDEF NEXTGEN}
procedure TSQLConnection.Commit;
begin
  if FTransactionsSupported then
  begin
    if InTransaction then
    begin
      FDBXConnection.CommitTransaction;
      Dec(FTransactionCount);
    end
    else
      DatabaseError(sNoActiveTrans, Self);
  end;
end;
{$ENDIF NEXTGEN}

procedure TSQLConnection.CommitFreeAndNil(var Transaction: TDBXTransaction);
begin
{$IFNDEF NEXTGEN}
  EndFreeAndNilTransaction(Transaction, true);
{$ELSE}
  if FTransactionsSupported then
  begin
    if InTransaction then
    begin
      FDBXConnection.CommitFreeAndNil(Transaction);
      Dec(FTransactionCount);
    end
    else
      DatabaseError(sNoActiveTrans, Self);
  end;
{$ENDIF !NEXTGEN}
end;

{$IFDEF NEXTGEN}
procedure TSQLConnection.Rollback;
begin
  if FTransactionsSupported then
  begin
    if InTransaction then
    begin
      FDBXConnection.RollbackTransaction;
      Dec(FTransactionCount);
    end
    else
      DatabaseError(sNoActiveTrans, self);
  end;
end;
{$ENDIF NEXTGEN}

procedure TSQLConnection.RollbackFreeAndNil(var Transaction: TDBXTransaction);
begin
{$IFNDEF NEXTGEN}
  EndFreeAndNilTransaction(Transaction, false);
{$ELSE}
  if FTransactionsSupported then
  begin
    if InTransaction then
    begin
      FDBXConnection.RollbackFreeAndNil(Transaction);
      Dec(FTransactionCount);
    end
    else
      DatabaseError(sNoActiveTrans, self);
  end;
{$ENDIF !NEXTGEN}
end;

procedure TSQLConnection.RollbackIncompleteFreeAndNil(
  var Transaction: TDBXTransaction);
begin
  if FDBXConnection.HasTransaction(Transaction) then
    RollbackFreeAndNil(Transaction);
  Transaction := nil;
end;

{$IFNDEF NEXTGEN}
procedure TSQLConnection.Commit(TransDesc: TTransactionDesc);
begin
  EndAndFreeTransaction(TransDesc, True);
end;

procedure TSQLConnection.Rollback( TransDesc: TTransactionDesc);
begin
  EndAndFreeTransaction(TransDesc, false);
end;
{$ENDIF !NEXTGEN}

function TSQLConnection.GetDataSet(Index: Integer): TCustomSQLDataSet;
begin
  Result := TCustomSQLDataSet(inherited GetDataSet(Index));
end;

{ misc. property set/get }

procedure TSQLConnection.SetDriverName(Value: string);
var
  DriverProperties: TDBXProperties;
  ExistingParams: TStrings;
  I: Integer;
begin
  if FDriverName <> Value then
  begin
    CheckInactive;
    if FConnectionName = '' then
      FParams.Clear;
    ExistingParams := TStringList.Create;
    try
      if FDriverName = EmptyStr then
        ExistingParams.Assign(FParams);
      FDriverName := Value;
      if FDriverName <> EmptyStr then
      begin
        TDBXConnectionFactory.Lock;
        try
          DriverProperties := TDBXConnectionFactory.GetConnectionFactory.GetDriverProperties(FDriverName);
          if csDesigning in ComponentState then
          begin
            if DriverProperties.Properties.Values['VendorLib'] = 'ibtogo.dll' then
              DriverProperties.Properties.Values['VendorLib'] := 'gds32.dll';
          end;
          if Assigned(DriverProperties) then
          begin
            ConnectionData.Properties := DriverProperties;
            ConnectionData.Properties.SetComponentOwner(Self);
            for I := 0 to ExistingParams.Count - 1 do
              ConnectionData.Properties[ExistingParams.Names[I]] := ExistingParams.ValueFromIndex[i];
          end
          else
            ConnectionData.Properties := nil;
        finally
          TDBXConnectionFactory.Unlock;
        end;
        if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
        begin
          FParamsLoaded := False;
        end;
      end;
    finally
      ExistingParams.Free;
    end;
  end;
end;

function TSQLConnection.GetConnectionName: string;
begin
  Result := FConnectionName;
end;

procedure TSQLConnection.SetConnectionName(Value: string);
var
  NewDriver: string;
begin
  if FConnectionName <> Value then
  begin
    if not (csLoading in ComponentState) then
      if Connected then Connected := False;
    if (FDriverName = '') and (Value = '') then
    begin
      FVendorLib := '';
      FLibraryName := '';
      FParams.Clear;
    end;
    FParamsLoaded := False;
    FConnectionName := Value;
    if not (csLoading in ComponentState) then
      CloseDataSets;
    if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    begin
      if (Value = '') and (LoadParamsOnConnect) then
        FParams.Clear;
      if Value <> '' then
      begin
        NewDriver := GetProfileString(FConnectionName, DRIVERNAME_KEY, ConnectionRegistryFile);
        if NewDriver <> DriverName then
          DriverName := NewDriver;
        LoadParamsFromIniFile;
      end;
    end;
  end;
end;

function TSQLConnection.GetVendorLib: string;
begin
  Result := FVendorLib;
end;


function TSQLConnection.GetLibraryName: string;
begin
  Result := FLibraryName;
end;

procedure TSQLConnection.LoadParamsFromIniFile(FFileName: string = '');
var
  IniFile: TMemIniFile;
  List: TStrings;
  FIniFileName: string;
begin
  if not FParamsLoaded then
  begin
    if FConnectionName = '' then
      DatabaseError(SConnectionNameMissing);
    List := TStringList.Create;
    try
      if FFileName = '' then
        FIniFileName := ConnectionRegistryFile
      else
        FIniFileName := FFileName;
      IniFile := TMemIniFile.Create(FIniFileName);
      try
        if FileExists(FIniFileName) then
        begin
          IniFile.ReadSectionValues(FConnectionName, List);
          Params.BeginUpdate;
          try
            Params.Clear;
            Params.AddStrings(List);
          finally
            Params.EndUpdate;
          end;
        end else
          DatabaseErrorFmt(sMissingDriverRegFile, [FIniFileName]);
      finally
        IniFile.Free;
      end;
    finally
      List.Free;
    end;
    FParamsLoaded := True;
  end;
end;

procedure TSQLConnection.SetLocaleCode(Value: TLocaleCode);
begin
  FParams.Values[SQLLOCALE_CODE_KEY] := IntToHex(Value, 4);
end;

function TSQLConnection.GetLocaleCode: TLocaleCode;
begin
  if FParams.Values[SQLLOCALE_CODE_KEY] <> '' then
    Result := StrToInt(HexDisplayPrefix + FParams.Values[SQLLOCALE_CODE_KEY])
  else
    Result := 0;
end;

procedure TSQLConnection.SetKeepConnection(Value: Boolean);
begin
  if FKeepConnection <> Value then
  begin
    FKeepConnection := Value;
    if not Value and (FRefCount = 0) then Close;
  end;
end;

procedure TSQLConnection.SetParams(Value: TStrings);
begin
  CheckInactive;
  FParams.Assign(Value);
end;


procedure TSQLConnection.Loaded;
begin
  inherited Loaded;
end;

procedure TSQLConnection.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
end;

procedure TSQLConnection.GetServerMethodNames(List: TStrings);
begin
  FDBXConnection.GetCommands(TDBXCommandTypes.DSServerMethod, List);
end;

function TSQLConnection.GetDefaultSchemaName: string;
begin
  CheckConnection(eConnect);
  Result := FDefaultSchemaName;
end;

{ TSQLDataLink }

constructor TSQLDataLink.Create(ADataSet: TCustomSQLDataSet);
begin
  inherited Create;
  FSQLDataSet := ADataSet;
end;

procedure TSQLDataLink.ActiveChanged;
begin
  if FSQLDataSet.Active then FSQLDataSet.RefreshParams;
end;

function TSQLDataLink.GetDetailDataSet: TDataSet;
begin
  Result := FSQLDataSet;
end;

procedure TSQLDataLink.RecordChanged(Field: TField);
begin
  if (Field = nil) and FSQLDataSet.Active then FSQLDataSet.RefreshParams;
end;

procedure TSQLDataLink.CheckBrowseMode;
begin
  if FSQLDataSet.Active then FSQLDataSet.CheckBrowseMode;
end;

{ TCustomSQLDataSet }

constructor TCustomSQLDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParams := TParams.Create(Self);
  FDataLink := TSQLDataLink.Create(Self);
  FIndexDefs := TIndexDefs.Create(Self);
  FCommandType := ctQuery;
  FDbxCommandType := TDBXCommandTypes.DbxSQL;
  FCommandText := '';
  FParamCheck := True;
  FRecords := -1;
  FParamCount := -1;
  FSchemaInfo.FType := stNoSchema;
  SetUniDirectional(True);
  ObjectView := False;
end;

destructor TCustomSQLDataSet.Destroy;
begin
  Close;
  if Assigned(FDBXReader) then FreeReader;
  if Assigned(FDBXCommand) then FreeCommand;
  FreeAndNil(FParams);
  FreeAndNil(FIndexDefs);
  SetConnection(nil);
  FreeProcParams(FProcParams);
  inherited Destroy;
  FDataLink.Free;
  FreeBuffers;
end;

{ connection management }

procedure TCustomSQLDataSet.CheckConnection(eFlag: eConnectFlag);
begin
  if (FSQLConnection <> nil) then
    FSQLConnection.CheckConnection(eFlag)
  else if (eFlag in [eConnect, eReconnect ]) and not FProvidedDBXReader then
    DatabaseError(SMissingSQLConnection);
end;

procedure TCustomSQLDataSet.SetConnection(const Value: TSQLConnection);
begin
  CheckInactive;
  if Assigned(FSQLConnection) then
    FSQLConnection.UnRegisterClient(Self);
  FSQLConnection := Value;
  if (not (csLoading in ComponentState)) and (FSQLConnection <> Value) then
    SchemaName := '';
  if Assigned(FSQLConnection) then
  begin
    FSQLConnection.RegisterClient(Self,nil);
    if FMaxBlobSize = 0 then   // means it hasn't been changed
    begin
      if FSQLConnection.Params.Values[MAXBLOBSIZE_KEY] <> '' then
      try
        FMaxBlobSize := StrToInt(trim(FSQLConnection.Params.Values[MAXBLOBSIZE_KEY]));
      except
        FMaxBlobSize := DefaultMaxBlobSize;
      end else
        FMaxBlobSize := DefaultMaxBlobSize;
    end;
  end;
end;

function TCustomSQLDataSet.GetInternalConnection: TSQLConnection;
begin
  if Assigned(FClonedConnection) then
    Result := FClonedConnection
  else
    Result := FSQLConnection;
end;


{ open/close Cursors and Statements }

procedure TCustomSQLDataSet.GetObjectTypeNames(Fields: TFields);
var
  I: Integer;
  ObjectField: TObjectField;
begin
  for I := 0 to Fields.Count - 1 do
  begin
    if Fields[I] is TObjectField then
    begin
      ObjectField := TObjectField(Fields[I]);
      ObjectField.ObjectType := FDBXReader.GetObjectTypeName(ObjectField.FieldNo-1);
      if ObjectField.DataType in [ftADT, ftArray] then
      begin
        if (ObjectField.DataType = ftArray) and SparseArrays and
           (ObjectField.Fields[0].DataType = ftADT) then
          GetObjectTypeNames(TObjectField(ObjectField.Fields[0]).Fields) else
          GetObjectTypeNames(ObjectField.Fields);
      end;
    end;
  end;
end;

procedure TCustomSQLDataSet.InternalOpen;
begin
  ExecuteStatement;
  if not Assigned(FDBXReader) then
  begin
    FreeCommand;
    if not FGetNextRecordSet then
      DataBaseError(SNoCursor,Self)
    else
      Exit;
  end;
  FieldDefs.Update;
  if DefaultFields then CreateFields;
  BindFields(True);
  if ObjectView then GetObjectTypeNames(Fields);
  InitBuffers;
end;

function TCustomSQLDataSet.IsCursorOpen: Boolean;
begin
  Result := (FDBXReader <> nil);
end;

procedure TCustomSQLDataSet.OpenCursor(InfoQuery: Boolean);
begin
  if (SchemaInfo.FType = stNoSchema) and (FCommandText = '') and not FProvidedDBXReader then
    DatabaseError(SNoSQLStatement);
  CheckConnection(eConnect);
  SetPrepared(True);
  CheckPrepareError;
  if FDataLink.DataSource <> nil then
     SetParamsFromCursor;
  if (SchemaInfo.FType = stNoSchema) and (FSQLConnection <> nil) then
    Inc(FSqlConnection.FActiveStatements);
  inherited OpenCursor;
end;

procedure TCustomSQLDataSet.CloseCursor;
begin
  inherited CloseCursor;
  if (SchemaInfo.FType = stNoSchema) and (FSqlConnection <> nil) then
    Dec(FSqlConnection.FActiveStatements);
  if Assigned(FSQLConnection) and (FSQLConnection.FMaxStmtsPerConn > 0) then
    InternalFreeCommand;
end;

procedure TCustomSQLDataSet.FreeReader;
begin
  if Assigned(FDBXReader) then
  begin
    if FProvidedDBXReader and not FOwnsProvidedDBXReader then
      FDBXReader := nil
    else
      FreeAndNil(FDBXReader);
    FStatementOpen := False;   // Releasing Reader closes associated statement
  end;
end;

procedure TCustomSQLDataSet.FreeCommand;
begin
  InternalFreeCommand;
// Metadata requests now init FDBXCommand which is tested above.
//  else
//  if (FSchemaInfo.FType <> stNoSchema) then
//    if Assigned(FClonedConnection) then
//      FreeAndNil(FClonedConnection)
//    else
//      if Assigned(FSQLConnection) and (FSQLConnection.FSelectStatements > 0) then
//        Dec(FSQLConnection.FSelectStatements);

  if Assigned(FieldDefs) then
    FieldDefs.Updated := False;
  ClearIndexDefs;
end;

procedure TCustomSQLDataSet.CloseStatement;
begin
  FPrepared := False;
  FParamCount := -1;
  FStatementOpen := False;
  if Assigned(FDBXCommand) then
  begin
    FDBXCommand.DisposeOf;
    FDBXCommand := nil;
  end;
end;

procedure TCustomSQLDataSet.InternalClose;
var
  DetailList: TList<TDataSet>;
  I: Integer;
begin
  BindFields(False);
  if DefaultFields then DestroyFields;
  FreeBuffers;
  if not FRefreshing then
  begin
    DetailList := TList<TDataSet>.Create;
    try
      GetDetailDataSets(DetailList);
      for I := 0 to DetailList.Count -1 do
        if DetailList[I] is TCustomSQLDataSet then
        begin
          TCustomSQLDataSet(DetailList[I]).Close;
          TCustomSQLDataSet(DetailList[I]).SetPrepared(False);
        end;
    finally
      DetailList.Free;
    end;
  end;
  if Assigned(FSQLConnection) and ((FSQLConnection.KeepConnection) or
     (FSQLConnection.DataSetCount > 1)) then
    FreeReader
  else
    SetPrepared(False);
end;

procedure TCustomSQLDataSet.InternalFreeCommand;
begin
  if Assigned(FDBXCommand) then
  begin
    FreeReader;
    CloseStatement;
    if Assigned(FSQLConnection) then
      if Assigned(FClonedConnection) then
        FreeAndNil(FClonedConnection)
      else
       if FSQLConnection.FSelectStatements > 0 then
         Dec(FSQLConnection.FSelectStatements);
  end;
end;

procedure TCustomSQLDataSet.Loaded;
begin
  inherited Loaded;
end;

procedure TCustomSQLDataSet.InternalRefresh;
begin
  FRefreshing := True;
  try
    SetState(dsInactive);
    CloseCursor;
    OpenCursor(False);
    SetState(dsBrowse);
  finally
    FRefreshing := False;
  end;
end;

procedure TCustomSQLDataSet.InitBuffers;
begin
  if (MaxBlobSize > 0) then
    SetLength(FBlobBuffer, MaxBlobSize * 1024);
  if (CalcFieldsSize > 0) then
    SetLength(FCalcFieldsBuffer, CalcFieldsSize);
end;

procedure TCustomSQLDataSet.ClearIndexDefs;
begin
  FIndexDefs.Clear;
  FIndexDefsLoaded := False;
end;

procedure TCustomSQLDataSet.FreeBuffers;
begin
  if FBlobBuffer <> nil then
    SetLength(FBlobBuffer, 0);
  FBlobBuffer := nil;
  if FFieldBuffer <> nil then
    SetLength(FFieldBuffer, 0);
  FFieldBuffer := nil;
  FCurrentBlobSize := 0;
  if FCalcFieldsBuffer <> nil then
  begin
    SetLength(FCalcFieldsBuffer, 0);
    FCalcFieldsBuffer := nil;
  end;
end;

procedure TCustomSQLDataSet.InitRecord(Buffer: TRecBuf);
begin
  { NOP }
end;

{$IFNDEF NEXTGEN}
procedure TCustomSQLDataSet.InitRecord(Buffer: TRecordBuffer);
begin
  { NOP }
end;
{$ENDIF !NEXTGEN}

procedure TCustomSQLDataSet.SetBufListSize(Value: Integer);
begin
end;

{ Reader Level Metadata }

procedure TCustomSQLDataSet.AddFieldDesc(FieldDescs: TFieldDescList; DescNo: Integer;
  var FieldID: Integer; RequiredFields: TBits; FieldDefs: TFieldDefs);
const
  ArrayIndex = '[0]';
var
  LType: TFieldType;
  LSize: LongWord;
  LRequired: Boolean;
  LPrecision, I: Integer;
  FieldName: string;
  FieldDesc: TFLDDesc;
  FldDef: TFieldDef;
begin
  FieldDesc := FieldDescs[DescNo];
  FieldName := FieldDesc.szName;                                            
  FieldDesc.FName := FieldName;
  I := 0;
  while FieldDefs.IndexOf(FieldDesc.FName) >= 0 do
  begin
    Inc(I);
    FieldDesc.FName := Format('%s_%d', [FieldName, I]);
  end;
  if FieldDesc.iFldType < TDBXDataTypes.MaxBaseTypes then
    LType := DataTypeMap[FieldDesc.iFldType]
  else
    LType := ftUnknown;
  if FieldDesc.iFldType in [TDBXDataTypes.CurrencyType, TDBXDataTypes.BcdType] then
  begin
    FieldDesc.iUnits2 := Abs(FieldDesc.iUnits2);
    if FieldDesc.iUnits1 < FieldDesc.iUnits2 then   // iUnits1 indicates Oracle 'usable decimals'
      FieldDesc.iUnits1 := FieldDesc.iUnits2;
    // ftBCD supports only up to 18-4.  If Prec > 14 or Scale > 4, make FMTBcd
    if (FieldDesc.iUnits1 > (MaxBcdPrecision-4)) or (FieldDesc.iUnits2 > MaxBcdScale) or FNumericMapping then
    begin
      LType := ftFMTBcd;
      FieldDesc.iFldType := TDBXDataTypes.BcdType;
      if FieldDesc.iUnits1 > MaxFMTBcdDigits then
        FieldDesc.iUnits1 := MaxFMTBcdDigits;
    end;
  end;
  LSize := 0;
  LPrecision := 0;
  if RequiredFields.Size > FieldID then
    LRequired := RequiredFields[FieldID] else
    LRequired := False;
  case FieldDesc.iFldType of
    TDBXDataTypes.AnsiStringType:
      begin
        if FieldDesc.iUnits1 = 0 then { Ignore MLSLABEL field type on Oracle }
          LType := ftUnknown else
          LSize := FieldDesc.iUnits1;
      end;
    TDBXDataTypes.WideStringType:
      begin
        if FieldDesc.iUnits1 = 0 then { Ignore MLSLABEL field type on Oracle }
          LType := ftUnknown else
          LSize := FieldDesc.iUnits1;
      end;
    TDBXDataTypes.BytesType, TDBXDataTypes.VarBytesType, TDBXDataTypes.RefType:
      begin
        if FieldDesc.iUnits1 = 0 then { Ignore MLSLABEL field type on Oracle }
          LType := ftUnknown else
          LSize := FieldDesc.iUnits1;
      end;
    TDBXDataTypes.Int16Type, TDBXDataTypes.UInt16Type:
      if FieldDesc.iLen <> 2 then LType := ftUnknown;
    TDBXDataTypes.Int32Type:
      if FieldDesc.iSubType = TDBXDataTypes.AutoIncSubType then
      begin
        LType := ftAutoInc;
        LRequired := False;
      end;
    TDBXDataTypes.DoubleType:
      if FieldDesc.iSubType = TDBXDataTypes.MoneySubType then LType := ftCurrency;
    TDBXDataTypes.CurrencyType, TDBXDataTypes.BcdType:
      begin
        LSize := Abs(FieldDesc.iUnits2);
        LPrecision := FieldDesc.iUnits1;
      end;
    TDBXDataTypes.AdtType, TDBXDataTypes.ArrayType:
      begin
        LSize := FieldDesc.iUnits2;
        LPrecision := FieldDesc.iUnits1;
      end;
    TDBXDataTypes.BlobType:
      begin
        LSize := FieldDesc.iUnits1;
        if (FieldDesc.iSubType >= TDBXDataTypes.MemoSubType) and (FieldDesc.iSubType <= TDBXDataTypes.BFileSubType) then
          LType := BlobTypeMap[FieldDesc.iSubType];
      end;
  end;
  FldDef := FieldDefs.AddFieldDef;
  FldDef.FieldNo := FieldID;
  Inc(FieldID);
  FldDef.Name := FieldDesc.FName;
  FldDef.DataType := LType;
  FldDef.Size := LSize;
  FldDef.Precision := LPrecision;
  if LRequired then
    FldDef.Attributes := [faRequired];
  if FieldDesc.efldrRights = fldrREADONLY then
    FldDef.Attributes := FldDef.Attributes + [faReadonly];
  if FieldDesc.iSubType = TDBXDataTypes.FixedSubType then
    FldDef.Attributes := FldDef.Attributes + [faFixed];
  FldDef.InternalCalcField := FieldDesc.bCalcField;
  case LType of
    ftBlob, ftMemo, ftWideMemo, ftOraBlob, ftOraClob, ftGraphic, ftFmtMemo, ftParadoxOle, ftDBaseOle:
      if FldDef.Size > MaxBlobSize then
        MaxBlobSize := FldDef.Size;
    ftADT:
      begin
        if FieldDesc.iSubType = TDBXDataTypes.AdtNestedTableSubType then
          FldDef.Attributes := FldDef.Attributes + [faUnNamed];
        for I := 1 to FieldDesc.iUnits1 do
        begin
          LoadFieldDef(Word(FldDef.FieldNo + I), FieldDescs[1]);
          AddFieldDesc(FieldDescs, 1, FieldID, RequiredFields, FldDef.ChildDefs);
        end;
      end;
    ftArray:
      begin
        for I := 1 to FieldDesc.iUnits1 do
        begin
          LoadFieldDef(Word(FldDef.FieldNo + I), FieldDescs[1]);
          FieldDescs[1].szName := FieldDesc.szName + ArrayIndex;
          AddFieldDesc(FieldDescs, 1, FieldID, RequiredFields, FldDef.ChildDefs);
        end;
      end;
  end;
end;

procedure TCustomSQLDataSet.LoadFieldDef(FieldID: Word; var FldDesc: TFLDDesc);
var
  ValueType: TDBXValueType;
begin
  FldDesc.iFldNum   := FieldID;
  ValueType         := FDBXReader.ValueType[FieldID-1];
  FldDesc.szName    := ValueType.Name;
  FldDesc.iFldType  := ValueType.DataType;
  FldDesc.iSubtype  := ValueType.SubType;
  FldDesc.iLen      := ValueType.Size;
  FldDesc.iUnits1   := ValueType.Precision;
  FldDesc.iUnits2   := ValueType.Scale;
  if ValueType.ReadOnly then
    FldDesc.efldrRights := fldrREADONLY;
  if FldDesc.iUnits1 = 0 then
  begin
    case ValueType.DataType of
      TDBXDataTypes.WideStringType:
        // Must provide a length in order to create a dataset column.  Raid 272101.
        // This code is consistent with TDBXDBMetaData.AddClientDataSetFields and
        // TDBXDataSetTable.CopyValueTypeProperties when handling TDBXDataTypes.WideStringType
        // Don't provide a length when have a DBXCommand so that ORACLE special cases special
        // cases will continue to work.  Search this file for "{ Ignore MLSLABEL field type on Oracle }".
        if (FldDesc.iLen <= 0) and (FDBXCommand = nil) then
          FldDesc.iUnits1 := 128  // default size (make constant)
        else
          FldDesc.iUnits1 := FldDesc.iLen;
      TDBXDataTypes.AnsiStringType:
        FldDesc.iUnits1 := FldDesc.iLen;
      TDBXDataTypes.VarBytesType,
      TDBXDataTypes.BytesType:
      begin
        // data size is in scale
        FldDesc.iUnits1 := FldDesc.iUnits2;
        FldDesc.iLen := FldDesc.iUnits2;
      end;
    end;
  end;
end;

procedure TCustomSQLDataSet.InternalInitFieldDefs;
var
  FID: Integer;
  FieldDescs: TFieldDescList;
  RequiredFields: TBits;
  FldDescCount: Word;
begin
  if (FDBXReader <> nil) then
  begin
    RequiredFields := TBits.Create;
    try
      FldDescCount := FDBXReader.ColumnCount;
      SetLength(FieldDescs, FldDescCount);
      for FID := 1 to FldDescCount do
        FieldDescs[FID-1] := TFldDesc.Create;
      try
        RequiredFields.Size := FldDescCount + 1;
        FieldDefs.Clear;
        FID := 1;
        FMaxColSize := FldDescCount;
        while FID <= FldDescCount do
        begin
          RequiredFields[FID] := FDBXReader.ValueType[FID-1].Nullable = False;
          LoadFieldDef(Word(FID), FieldDescs[0]);
          if (FieldDescs[0].iLen > FMaxColSize) and
             (FieldDescs[0].iFldType <> TDBXDataTypes.BlobType) then
            FMaxColSize := (FMaxColSize + FieldDescs[0].iLen);
          AddFieldDesc(FieldDescs, Integer(0), FID, RequiredFields, FieldDefs);
        end;
      finally
        for FID := 1 to FldDescCount do
          FreeAndNil(FieldDescs[FID-1]);
        FieldDescs := nil;
      end;
    finally
      RequiredFields.Free;
    end;
  end
  else
     DatabaseError(SDataSetClosed, self);
end;

{ Field and Record Access }

procedure NormalizeBcdData(FieldBuffer: TArray<Byte>; BcdData: TValueBuffer; Precision, Scale: Word);
var
  InBcd: TBcd;
  LBcd: TBcd;
begin
  if Assigned(BcdData) then
  begin
    if Precision > MaxFMTBcdDigits then Precision := MaxFMTBcdDigits;
    InBcd := BcdFromBytes(FieldBuffer);
    if (InBcd.SignSpecialPlaces = 38) and ((Scale and 63)in [38,0]) then
    begin
      if (Scale and (1 shl 7)) <> 0 then
        NormalizeBcd(InBcd, LBcd, MaxFMTBcdDigits, Word((DefaultFMTBcdScale and 63) or (1 shl 7)))
      else
        NormalizeBcd(InBcd, LBcd, MaxFMTBcdDigits, DefaultFMTBcdScale);
    end else
      NormalizeBcd(InBcd, LBcd, Precision, Scale);
    TPlatformValueBuffer.Copy(BcdToBytes(LBcd), 0, BcdData, SizeOfTBcd);
  end;
end;

function TCustomSQLDataSet.GetFieldData(FieldNo: Integer; var Buffer: TValueBuffer): Boolean;
var
  FldType, Precision, Scale: Word;
  LBlank: LongBool;
  Field: TField;
  ByteReader: TDBXByteReader;
  ByteBufferLength, DataLength, FieldDataSize, Ordinal: Integer;
  ByteBuffer: TArray<Byte>;
  BytesRead:  Int64;
  ValueType: TDBXValueType;
  ValueStr: string;
  M: TMarshaller;
begin
  if (FDBXReader = nil) then
    DatabaseError(SDataSetClosed, self);

  // When EOF is True or we are dealing with a calculated field (FieldNo < 1)
  // we should not be calling into the driver to get Data
  //
  if (FieldNo < 1) then
  begin
    Result := False;
    Exit;
  end;
  if EOF and (not BOF) then
  begin
    Result := False;
    Exit;
  end;
  if (EOF and BOF and FDBXReader.Closed) then
  begin
    Result := False;
    Exit;
  end;

  LBlank := True;
  Ordinal := FieldNo - 1;
  ValueType := FDBXReader.ValueType[Ordinal];
  DataLength := ValueType.Size;
  FldType := ValueType.DataType;
  if (Length(FFieldBuffer) <> DataLength) and (FldType <> TDBXDataTypes.BlobType) then
    SetLength(FFieldBuffer, DataLength);
  ByteReader := FDBXReader.ByteReader;
  begin
    case FldType of
      TDBXDataTypes.AnsiStringType:
        begin
          FillChar(FFieldBuffer[0], Length(FFieldBuffer), 0);
{$IFNDEF NEXTGEN}
          if ByteReader <> nil then
            ByteReader.GetAnsiString(Ordinal, FFieldBuffer, 0, LBlank)
          else
{$ENDIF !NEXTGEN}
          begin
            ValueStr := FDBXReader.Value[Ordinal].AsString;
            LBlank := FDBXReader.Value[Ordinal].IsNull;
            if ValueStr.Length > 0 then
              TMarshal.Copy(M.AsAnsi(ValueStr), FFieldBuffer, 0, ValueStr.Length);
          end;
          if not LBlank then
            TPlatformValueBuffer.Copy(FFieldBuffer, 0, Buffer, DataLength);
        end;
      TDBXDataTypes.WideStringType:
        begin
          FieldDataSize := FieldByNumber(FieldNo).DataSize;
          if Length(FFieldBuffer) < FieldDataSize then
            SetLength(FFieldBuffer, FieldDataSize);
          FillChar(FFieldBuffer[0], Length(FFieldBuffer), 0);
          if Assigned(ByteReader) then
            ByteReader.GetWideString(Ordinal, FFieldBuffer, 0, LBlank)
          else
          begin
            ValueStr := FDBXReader.Value[Ordinal].AsString;
            LBlank := FDBXReader.Value[Ordinal].IsNull;
            if ValueStr.Length > 0 then
            begin
               ByteBuffer := TDBXPlatform.WideStrToBytes(ValueStr);
               ByteBufferLength := Length(ByteBuffer);
               TDBXPlatform.CopyByteArray(ByteBuffer, 0, FFieldBuffer, 0, ByteBufferLength);
            end;
          end;
          if not LBlank then
            TPlatformValueBuffer.Copy(FFieldBuffer, 0, Buffer, FieldDataSize);
        end;
      TDBXDataTypes.UInt8Type:
        begin
          if Assigned(ByteReader) then
            ByteReader.GetUInt8(Ordinal, FFieldBuffer, 0, LBlank)
          else
          begin
            TDBXPlatform.CopyUInt8(FDBXReader.Value[Ordinal].AsUInt8, FFieldBuffer, 0);
            LBlank := False;
          end;
          if not LBlank then
            TPlatformValueBuffer.Copy(FFieldBuffer, 0, Buffer, DataLength);
        end;
      TDBXDataTypes.Int8Type:
        begin
          if Assigned(ByteReader) then
            ByteReader.GetInt8(Ordinal, FFieldBuffer, 0, LBlank)
          else
          begin
            TDBXPlatform.CopyInt8(FDBXReader.Value[Ordinal].AsInt8, FFieldBuffer, 0);
            LBlank := False;
          end;
          if not LBlank then
            TPlatformValueBuffer.Copy(FFieldBuffer, 0, Buffer, DataLength);
        end;
      TDBXDataTypes.Int16Type:
        begin
          if Assigned(ByteReader) then
            ByteReader.GetInt16(Ordinal, FFieldBuffer, 0, LBlank)
          else
          begin
            TDBXPlatform.CopyInt16(FDBXReader.Value[Ordinal].AsInt16, FFieldBuffer, 0);
            LBlank := False;
          end;
          if not LBlank then
            TPlatformValueBuffer.Copy(FFieldBuffer, 0, Buffer, DataLength);
        end;
      TDBXDataTypes.UInt16Type:
        begin
          if Assigned(ByteReader) then
            ByteReader.GetInt16(Ordinal, FFieldBuffer, 0, LBlank)
          else
          begin
            TDBXPlatform.CopyUInt16(FDBXReader.Value[Ordinal].AsUInt16, FFieldBuffer, 0);
            LBlank := False;
          end;
          if not LBlank then
            TPlatformValueBuffer.Copy(FFieldBuffer, 0, Buffer, DataLength);
        end;
      TDBXDataTypes.Int32Type, TDBXDataTypes.UInt32Type:
        begin
          if Assigned(ByteReader) then
            ByteReader.GetInt32(Ordinal, FFieldBuffer, 0, LBlank)
          else
          begin
            TDBXPlatform.CopyInt32(FDBXReader.Value[Ordinal].AsInt32, FFieldBuffer, 0);
            LBlank := False;
          end;
          if not LBlank then
            TPlatformValueBuffer.Copy(FFieldBuffer, 0, Buffer, DataLength);
        end;
      TDBXDataTypes.Int64Type:
        begin
          if Assigned(ByteReader) then
            ByteReader.GetInt64(Ordinal, FFieldBuffer, 0, LBlank)
          else
          begin
            TDBXPlatform.CopyInt64(FDBXReader.Value[Ordinal].AsInt64, FFieldBuffer, 0);
            LBlank := False;
          end;
          if not LBlank then
            TPlatformValueBuffer.Copy(FFieldBuffer, 0, Buffer, DataLength);
        end;
      TDBXDataTypes.SingleType:
        begin
          if Assigned(ByteReader) then
            ByteReader.GetSingle(Ordinal, FFieldBuffer, 0, LBlank)
          else
          begin
            TDBXPlatform.CopyInt32(TDBXPlatform.SingleToInt32Bits(FDBXReader.Value[Ordinal].AsSingle), FFieldBuffer, 0);
            LBlank := False;
          end;
          if not LBlank then
            TPlatformValueBuffer.Copy(FFieldBuffer, 0, Buffer, DataLength);
        end;
      TDBXDataTypes.DoubleType:
        begin
          if Assigned(ByteReader) then
            ByteReader.GetDouble(Ordinal, FFieldBuffer, 0, LBlank)
          else
          begin
            TDBXPlatform.CopyInt64(TDBXPlatform.DoubleToInt64Bits(FDBXReader.Value[Ordinal].AsDouble), FFieldBuffer, 0);
            LBlank := False;
          end;
          if not LBlank then
            TPlatformValueBuffer.Copy(FFieldBuffer, 0, Buffer, DataLength);
        end;
      TDBXDataTypes.CurrencyType, TDBXDataTypes.BcdType:
        begin
          Field := FieldByNumber(FieldNo);
          if Length(FFieldBuffer) < Field.DataSize then
            SetLength(FFieldBuffer, Field.DataSize);
          if Assigned(ByteReader) then
            ByteReader.GetBcd(Ordinal, FFieldBuffer, 0, LBlank)
          else
          begin
            TDBXPlatform.CopyBcd(FDBXReader.Value[Ordinal].AsBcd, FFieldBuffer, 0);
            LBlank := False;
          end;
          if (not LBlank) and (Field <> nil) then
          begin
            if Field.DataType = ftBcd then
            begin
              Precision := TBcdField(Field).Precision;
              Scale := TBcdField(Field).Size;
            end else
            begin
              Precision := TFMTBcdField(Field).Precision;
              Scale := TFMTBcdField(Field).Size;
            end;
            NormalizeBcdData(FFieldBuffer, Buffer, Precision, Scale);
          end;
        end;
      TDBXDataTypes.DateType:
        begin
          if Assigned(ByteReader) then
            ByteReader.GetDate(Ordinal, FFieldBuffer, 0, LBlank)
          else
          begin
            TDBXPlatform.CopyInt32(FDBXReader.Value[Ordinal].AsDate, FFieldBuffer, 0);
            LBlank := False;
          end;
          if not LBlank then
            TPlatformValueBuffer.Copy(FFieldBuffer, 0, Buffer, DataLength);
        end;
      TDBXDataTypes.TimeType:
        begin
          if Assigned(ByteReader) then
            ByteReader.GetTime(Ordinal, FFieldBuffer, 0, LBlank)
          else
          begin
            TDBXPlatform.CopyInt32(FDBXReader.Value[Ordinal].AsTime, FFieldBuffer, 0);
            LBlank := False;
          end;
          if not LBlank then
            TPlatformValueBuffer.Copy(FFieldBuffer, 0, Buffer, DataLength);
        end;
      TDBXDataTypes.TimeStampType:
        begin
          if Assigned(ByteReader) then
            ByteReader.GetTimeStamp(Ordinal, FFieldBuffer, 0, LBlank)
          else
          begin
            TDBXPlatform.CopyInt64(TDBXPlatform.DoubleToInt64Bits(FDBXReader.Value[Ordinal].AsDateTime), FFieldBuffer, 0);
            LBlank := False;
          end;
          if not LBlank then
            TPlatformValueBuffer.Copy(FFieldBuffer, 0, Buffer, DataLength);
        end;
      TDBXDataTypes.TimeStampOffsetType:
        begin
          ByteReader.GetTimeStampOffset(Ordinal, FFieldBuffer, 0, LBlank);
          if not LBlank then
            TPlatformValueBuffer.Copy(FFieldBuffer, 0, Buffer, DataLength);
        end;
      TDBXDataTypes.DateTimeType:
        begin
          if not FDBXReader.Value[Ordinal].IsNull then
          begin
            Field := FieldByNumber(FieldNo);
            TDBXPlatform.CopyInt64(TDBXPlatform.DoubleToInt64Bits(FDBXReader.Value[Ordinal].AsDateTime), FFieldBuffer, 0);
            TPlatformValueBuffer.Copy(FFieldBuffer, 0, Buffer, DataLength);
            DataConvert(Field, Buffer, Buffer, True);
            LBlank := False;
          end
        end;
      TDBXDataTypes.BooleanType:
        begin
          if Assigned(ByteReader) then
            ByteReader.GetInt16(Ordinal, FFieldBuffer, 0, LBlank)
          else
          begin
            TDBXPlatform.CopyInt16(FDBXReader.Value[Ordinal].AsInt16, FFieldBuffer, 0);
            LBlank := False;
          end;
          if not LBlank then
            // DbxClient returns DataSize of 1, but we are reading 2 bytes.
            TPlatformValueBuffer.Copy(FFieldBuffer, 0, Buffer, 2);//DataLength);
        end;
      TDBXDataTypes.VarBytesType:
        begin
          DataLength := FDBXReader.ValueType[Ordinal].Size;
          SetLength(ByteBuffer, DataLength+2);
          if Assigned(ByteReader) then
            BytesRead := ByteReader.GetBytes(Ordinal, 0, ByteBuffer, 2, DataLength, LBlank)
          else
          begin
            BytesRead := FDBXReader.Value[Ordinal].GetBytes(0, ByteBuffer, 2, DataLength);
            LBlank := False;
          end;
          ByteBuffer[0] := BytesRead;
          ByteBuffer[1] := BytesRead shr 8;
          if not LBlank then
            TPlatformValueBuffer.Copy(ByteBuffer, 0, Buffer, DataLength+2);
        end;
      TDBXDataTypes.BytesType:
        begin
          DataLength := FDBXReader.ValueType[Ordinal].Size;
          SetLength(ByteBuffer, DataLength);
          if Assigned(ByteReader) then
            ByteReader.GetBytes(Ordinal, 0, ByteBuffer, 0, DataLength, LBlank)
          else
          begin
            FDBXReader.Value[Ordinal].GetBytes(0, ByteBuffer, 0, DataLength);
            LBlank := False;
          end;
          if not LBlank then
            TPlatformValueBuffer.Copy(ByteBuffer, 0, Buffer, DataLength);
        end;
      TDBXDataTypes.BlobType:
        begin
//          DataLength := GetBlobLength(Self, FieldNo);
          if CurrentBlobSize = 0 then
            LBlank := True
          else
            begin
              if Assigned(ByteReader) then
              begin
                // Temporary for bug 249185.  Needs to be fixed properly for both managed
                // and native in a better way than this.  This change will keep things
                // working the same way they did in bds2006.
                // Need to modify all drivers to return 0 bytes read if they cannot read
                // a blob twice.  The temporary change below is also an optimization for
                // blobs since it avoids a copy of the blob.  This is not the right way
                // to fix this.  Solution should solve the problem for both native and
                // managed. One option is to duplicate blob read code from the TSQLBlobStream
                // class.  Virtually all apps will go through a blob stream to access blob
                // data.  However there is a path to this method though TField.GetData.
                // Sure would be nice if TDataSet could manage record buffers as TArray<Byte>.
                // sshaughnessy 2007.04.19.
                //
                if Buffer = FBlobBuffer then
                begin
                  ByteBuffer := TArray<Byte>(Buffer);
                  ByteReader.GetBytes(Ordinal, 0, ByteBuffer, 0, CurrentBlobSize, LBlank);
                end else
                begin
                  SetLength(ByteBuffer, CurrentBlobSize);
                  ByteReader.GetBytes(Ordinal, 0, ByteBuffer, 0, CurrentBlobSize, LBlank);
                  if not LBlank then
                    TPlatformValueBuffer.Copy(ByteBuffer, 0, Buffer, CurrentBlobSize);
                end;
              end
              else
              begin
                SetLength(ByteBuffer, CurrentBlobSize);
                FDBXReader.Value[Ordinal].GetBytes(0, ByteBuffer, 0, CurrentBlobSize);
                TPlatformValueBuffer.Copy(ByteBuffer, 0, Buffer, CurrentBlobSize);
              end;
            end;
        end;
    end;
  end;
//    SetLength(FFieldBuffer, 1);
  Result := not LBlank;
end;

function TCustomSQLDataSet.GetFieldData(Field: TField; var Buffer: TValueBuffer): Boolean;
var
   FieldNo: Word;
   TempBuffer: TValueBuffer;
   ThisBuffer: TValueBuffer;
   BlobSize: Int64;
   BlobNull: LongBool;
begin
  if not Self.Active then
    DataBaseError(SDatasetClosed);
  FieldNo := Field.FieldNo;
  if not Assigned(Buffer) then
  begin
    if Field.IsBlob then
    begin
      if EOF then
        BlobNull := True
      else
        FDBXReader.ByteReader.GetByteLength(Word(FieldNo)-1, BlobSize, BlobNull);
      Result := not Boolean(BlobNull);
      Exit;
    end
    else if Field.Size > Field.DataSize then
      TempBuffer := TPlatformValueBuffer.CreateValueBuffer(Field.Size)
    else
      TempBuffer := TPlatformValueBuffer.CreateValueBuffer(Field.DataSize);
    ThisBuffer := TempBuffer;
  end else
  begin
    ThisBuffer := Buffer;
    TempBuffer := nil;
  end;
  try
    if Field.FieldNo < 1 then
      Result := GetCalculatedField(Field, ThisBuffer)
    else
      Result := GetFieldData(FieldNo, ThisBuffer);
  finally
    if Assigned(TempBuffer) then
      TPlatformValueBuffer.Free(TempBuffer);
  end;
end;

{$IFNDEF NEXTGEN}
function TCustomSQLDataSet.GetFieldData(FieldNo: Integer; Buffer: Pointer): Boolean;
var
  FldType: Word;
  FBlank: LongBool;
  Field: TField;
  Precision, Scale: Word;
  ByteReader: TDBXByteReader;
  Ordinal:  Integer;
  ByteBuffer: TArray<Byte>;
  DataLength, ByteBufferLength: Integer;
  BytesRead:  Int64;
  ValueType: TDBXValueType;
  FieldDataSize: Integer;
  ValueStr: string;
begin
  if (FDBXReader = nil) then
    DatabaseError(SDataSetClosed, self);

  // When EOF is True or we are dealing with a calculated field (FieldNo < 1)
  // we should not be calling into the driver to get Data
  //
  if (FieldNo < 1) then
  begin
    Result := False;
    Exit;
  end;
  if EOF and (not BOF) then
  begin
    Result := False;
    Exit;
  end;
  if (EOF and BOF and FDBXReader.Closed) then
  begin
    Result := False;
    Exit;
  end;

  FBlank := True;
  Ordinal := FieldNo - 1;
  ValueType := FDBXReader.ValueType[Ordinal];
  DataLength := ValueType.Size;
  FldType := ValueType.DataType;
  if (Length(FFieldBuffer) < DataLength) and (FldType <> TDBXDataTypes.BlobType) then
    SetLength(FFieldBuffer, DataLength);
  ByteReader := FDBXReader.ByteReader;
  begin
    case FldType of
      TDBXDataTypes.AnsiStringType:
        begin
          FillChar(FFieldBuffer[0], Length(FFieldBuffer), 0);
          if ByteReader <> nil then
            ByteReader.GetAnsiString(Ordinal, FFieldBuffer, 0, FBlank)
          else
          begin
            ValueStr := FDBXReader.Value[Ordinal].AsString;
            if ValueStr.Length > 0 then
            begin
              ByteBuffer := TDBXPlatform.AnsiStrToBytes(AnsiString(ValueStr));
              ByteBufferLength := Length(ByteBuffer);
              TDBXPlatform.CopyByteArray(ByteBuffer, 0, FFieldBuffer, 0, ByteBufferLength);
              FBlank := ByteBufferLength = 0;
            end;
          end;
          if not FBlank then
            TPlatformValueBuffer.Copy(FFieldBuffer, 0, Buffer, DataLength);
        end;
      TDBXDataTypes.WideStringType:
        begin
          FieldDataSize := FieldByNumber(FieldNo).DataSize;
          if Length(FFieldBuffer) < FieldDataSize then
            SetLength(FFieldBuffer, FieldDataSize);
          FillChar(FFieldBuffer[0], Length(FFieldBuffer), 0);
          if Assigned(ByteReader) then
            ByteReader.GetWideString(Ordinal, FFieldBuffer, 0, FBlank)
          else
          begin
            ValueStr := FDBXReader.Value[Ordinal].AsString;
            if ValueStr.Length > 0 then
            begin
               ByteBuffer := TDBXPlatform.WideStrToBytes(ValueStr);
               ByteBufferLength := Length(ByteBuffer);
               TDBXPlatform.CopyByteArray(ByteBuffer, 0, FFieldBuffer, 0, ByteBufferLength);
               FBlank := ByteBufferLength = 0;
            end;
          end;
          if not FBlank then
            TPlatformValueBuffer.Copy(FFieldBuffer, 0, Buffer, FieldDataSize);
        end;
      TDBXDataTypes.UInt8Type:
        begin
          if Assigned(ByteReader) then
            ByteReader.GetUInt8(Ordinal, FFieldBuffer, 0, FBlank)
          else
          begin
            TDBXPlatform.CopyUInt8(FDBXReader.Value[Ordinal].AsUInt8, FFieldBuffer, 0);
            FBlank := False;
          end;
          if not FBlank then
            TPlatformValueBuffer.Copy(FFieldBuffer, 0, Buffer, DataLength);
        end;
      TDBXDataTypes.Int8Type:
        begin
          if Assigned(ByteReader) then
            ByteReader.GetInt8(Ordinal, FFieldBuffer, 0, FBlank)
          else
          begin
            TDBXPlatform.CopyInt8(FDBXReader.Value[Ordinal].AsInt8, FFieldBuffer, 0);
            FBlank := False;
          end;
          if not FBlank then
            TPlatformValueBuffer.Copy(FFieldBuffer, 0, Buffer, DataLength);
        end;
      TDBXDataTypes.Int16Type:
        begin
          if Assigned(ByteReader) then
            ByteReader.GetInt16(Ordinal, FFieldBuffer, 0, FBlank)
          else
          begin
            TDBXPlatform.CopyInt16(FDBXReader.Value[Ordinal].AsInt16, FFieldBuffer, 0);
            FBlank := False;
          end;
          if not FBlank then
            TPlatformValueBuffer.Copy(FFieldBuffer, 0, Buffer, DataLength);
        end;
      TDBXDataTypes.UInt16Type:
        begin
          if Assigned(ByteReader) then
            ByteReader.GetInt16(Ordinal, FFieldBuffer, 0, FBlank)
          else
          begin
            TDBXPlatform.CopyUInt16(FDBXReader.Value[Ordinal].AsUInt16, FFieldBuffer, 0);
            FBlank := False;
          end;
          if not FBlank then
            TPlatformValueBuffer.Copy(FFieldBuffer, 0, Buffer, DataLength);
        end;
      TDBXDataTypes.Int32Type, TDBXDataTypes.UInt32Type:
        begin
          if Assigned(ByteReader) then
            ByteReader.GetInt32(Ordinal, FFieldBuffer, 0, FBlank)
          else
          begin
            TDBXPlatform.CopyInt32(FDBXReader.Value[Ordinal].AsInt32, FFieldBuffer, 0);
            FBlank := False;
          end;
          if not FBlank then
            TPlatformValueBuffer.Copy(FFieldBuffer, 0, Buffer, DataLength);
        end;
      TDBXDataTypes.Int64Type:
        begin
          if Assigned(ByteReader) then
            ByteReader.GetInt64(Ordinal, FFieldBuffer, 0, FBlank)
          else
          begin
            TDBXPlatform.CopyInt64(FDBXReader.Value[Ordinal].AsInt64, FFieldBuffer, 0);
            FBlank := False;
          end;
          if not FBlank then
            TPlatformValueBuffer.Copy(FFieldBuffer, 0, Buffer, DataLength);
        end;
      TDBXDataTypes.SingleType:
        begin
          if Assigned(ByteReader) then
            ByteReader.GetSingle(Ordinal, FFieldBuffer, 0, FBlank)
          else
          begin
            TDBXPlatform.CopyInt32(TDBXPlatform.SingleToInt32Bits(FDBXReader.Value[Ordinal].AsSingle), FFieldBuffer, 0);
            FBlank := False;
          end;
          if not FBlank then
            TPlatformValueBuffer.Copy(FFieldBuffer, 0, Buffer, DataLength);
        end;
      TDBXDataTypes.DoubleType:
        begin
          if Assigned(ByteReader) then
            ByteReader.GetDouble(Ordinal, FFieldBuffer, 0, FBlank)
          else
          begin
            TDBXPlatform.CopyInt64(TDBXPlatform.DoubleToInt64Bits(FDBXReader.Value[Ordinal].AsDouble), FFieldBuffer, 0);
            FBlank := False;
          end;
          if not FBlank then
            TPlatformValueBuffer.Copy(FFieldBuffer, 0, Buffer, DataLength);
        end;
      TDBXDataTypes.CurrencyType, TDBXDataTypes.BcdType:
        begin
          Field := FieldByNumber(FieldNo);
          if Length(FFieldBuffer) < Field.DataSize then
            SetLength(FFieldBuffer, Field.DataSize);
          if Assigned(ByteReader) then
            ByteReader.GetBcd(Ordinal, FFieldBuffer, 0, FBlank)
          else
          begin
            TDBXPlatform.CopyBcd(FDBXReader.Value[Ordinal].AsBcd, FFieldBuffer, 0);
            FBlank := False;
          end;
          if (not FBlank) and (Field <> nil) then
          begin
            if Field.DataType = ftBcd then
            begin
              Precision := TBcdField(Field).Precision;
              Scale := TBcdField(Field).Size;
            end else
            begin
              Precision := TFMTBcdField(Field).Precision;
              Scale := TFMTBcdField(Field).Size;
            end;
            NormalizeBcdData(FFieldBuffer, Buffer, Precision, Scale);
          end;
        end;
      TDBXDataTypes.DateType:
        begin
          if Assigned(ByteReader) then
            ByteReader.GetDate(Ordinal, FFieldBuffer, 0, FBlank)
          else
          begin
            TDBXPlatform.CopyInt32(FDBXReader.Value[Ordinal].AsDate, FFieldBuffer, 0);
            FBlank := False;
          end;
          if not FBlank then
            TPlatformValueBuffer.Copy(FFieldBuffer, 0, Buffer, DataLength);
        end;
      TDBXDataTypes.TimeType:
        begin
          if Assigned(ByteReader) then
            ByteReader.GetTime(Ordinal, FFieldBuffer, 0, FBlank)
          else
          begin
            TDBXPlatform.CopyInt32(FDBXReader.Value[Ordinal].AsTime, FFieldBuffer, 0);
            FBlank := False;
          end;
          if not FBlank then
            TPlatformValueBuffer.Copy(FFieldBuffer, 0, Buffer, DataLength);
        end;
      TDBXDataTypes.TimeStampType:
        begin
          if Assigned(ByteReader) then
            ByteReader.GetTimeStamp(Ordinal, FFieldBuffer, 0, FBlank)
          else
          begin
            TDBXPlatform.CopyInt64(TDBXPlatform.DoubleToInt64Bits(FDBXReader.Value[Ordinal].AsDateTime), FFieldBuffer, 0);
            FBlank := False;
          end;
          if not FBlank then
            TPlatformValueBuffer.Copy(FFieldBuffer, 0, Buffer, DataLength);
        end;
      TDBXDataTypes.TimeStampOffsetType:
        begin
          ByteReader.GetTimeStampOffset(Ordinal, FFieldBuffer, 0, FBlank);
          if not FBlank then
            TPlatformValueBuffer.Copy(FFieldBuffer, 0, Buffer, DataLength);
        end;
      TDBXDataTypes.DateTimeType:
        begin
          if not FDBXReader.Value[Ordinal].IsNull then
          begin
            Field := FieldByNumber(FieldNo);
            TDBXPlatform.CopyInt64(TDBXPlatform.DoubleToInt64Bits(FDBXReader.Value[Ordinal].AsDateTime), FFieldBuffer, 0);
            TPlatformValueBuffer.Copy(FFieldBuffer, 0, Buffer, DataLength);
            DataConvert(Field, Buffer, Buffer, True);
            FBlank := False;
          end
        end;
      TDBXDataTypes.BooleanType:
        begin
          if Assigned(ByteReader) then
            ByteReader.GetInt16(Ordinal, FFieldBuffer, 0, FBlank)
          else
          begin
            TDBXPlatform.CopyInt16(FDBXReader.Value[Ordinal].AsInt16, FFieldBuffer, 0);
            FBlank := False;
          end;
          if not FBlank then
            // DbxClient returns DataSize of 1, but we are reading 2 bytes.
            TPlatformValueBuffer.Copy(FFieldBuffer, 0, Buffer, 2);//DataLength);
        end;
      TDBXDataTypes.VarBytesType:
        begin
          DataLength := FDBXReader.ValueType[Ordinal].Size;
          SetLength(ByteBuffer, DataLength+2);
          if Assigned(ByteReader) then
            BytesRead := ByteReader.GetBytes(Ordinal, 0, ByteBuffer, 2, DataLength, FBlank)
          else
          begin
            BytesRead := FDBXReader.Value[Ordinal].GetBytes(0, ByteBuffer, 2, DataLength);
            FBlank := False;
          end;
          ByteBuffer[0] := BytesRead;
          ByteBuffer[1] := BytesRead shr 8;
          if not FBlank then
            TPlatformValueBuffer.Copy(ByteBuffer, 0, Buffer, DataLength+2);
        end;
      TDBXDataTypes.BytesType:
        begin
          DataLength := FDBXReader.ValueType[Ordinal].Size;
          SetLength(ByteBuffer, DataLength);
          if Assigned(ByteReader) then
            ByteReader.GetBytes(Ordinal, 0, ByteBuffer, 0, DataLength, FBlank)
          else
          begin
            FDBXReader.Value[Ordinal].GetBytes(0, ByteBuffer, 0, DataLength);
            FBlank := False;
          end;
          if not FBlank then
            TPlatformValueBuffer.Copy(ByteBuffer, 0, Buffer, DataLength);
        end;
      TDBXDataTypes.BlobType:
        begin
//          DataLength := GetBlobLength(Self, FieldNo);
          if CurrentBlobSize = 0 then
            FBlank := True
          else
            begin
              if Assigned(ByteReader) then
              begin
                // Temporary for bug 249185.  Needs to be fixed properly for both managed
                // and native in a better way than this.  This change will keep things
                // working the same way they did in bds2006.
                // Need to modify all drivers to return 0 bytes read if they cannot read
                // a blob twice.  The temporary change below is also an optimization for
                // blobs since it avoids a copy of the blob.  This is not the right way
                // to fix this.  Solution should solve the problem for both native and
                // managed. One option is to duplicate blob read code from the TSQLBlobStream
                // class.  Virtually all apps will go through a blob stream to access blob
                // data.  However there is a path to this method though TField.GetData.
                // Sure would be nice if TDataSet could manage record buffers as TArray<Byte>.
                // sshaughnessy 2007.04.19.
                //
                if Buffer = FBlobBuffer then
                begin
                  ByteBuffer := TArray<Byte>(Buffer);
                  ByteReader.GetBytes(Ordinal, 0, ByteBuffer, 0, CurrentBlobSize, FBlank);
                end else
                begin
                  SetLength(ByteBuffer, CurrentBlobSize);
                  ByteReader.GetBytes(Ordinal, 0, ByteBuffer, 0, CurrentBlobSize, FBlank);
                  if not FBlank then
                    TPlatformValueBuffer.Copy(ByteBuffer, 0, Buffer, CurrentBlobSize);
                end;
              end
              else
              begin
                SetLength(ByteBuffer, CurrentBlobSize);
                FDBXReader.Value[Ordinal].GetBytes(0, ByteBuffer, 0, CurrentBlobSize);
                TPlatformValueBuffer.Copy(ByteBuffer, 0, Buffer, CurrentBlobSize);
              end;
            end;
        end;
    end;
  end;
//    SetLength(FFieldBuffer, 1);
  Result := not FBlank;
end;

function TCustomSQLDataSet.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
   FieldNo: Word;
   TempBuffer: TValueBuffer;
   ThisBuffer: TValueBuffer;
   BlobSize: Int64;
   BlobNull: LongBool;
begin
  if not Self.Active then
    DataBaseError(SDatasetClosed);
  FieldNo := Field.FieldNo;
  if not Assigned(Buffer) then
  begin
    if Field.IsBlob then
    begin
      if EOF then
        BlobNull := True
      else
        FDBXReader.ByteReader.GetByteLength(Word(FieldNo)-1, BlobSize, BlobNull);
      Result := not Boolean(BlobNull);
      Exit;
    end
    else if Field.Size > Field.DataSize then
      TempBuffer := TPlatformValueBuffer.CreateValueBuffer(Field.Size)
    else
      TempBuffer := TPlatformValueBuffer.CreateValueBuffer(Field.DataSize);
    ThisBuffer := TempBuffer;
  end else
  begin
    ThisBuffer := Buffer;
    TempBuffer := nil;
  end;
  try
    if Field.FieldNo < 1 then
      Result := GetCalculatedField(Field, ThisBuffer)
    else
      Result := GetFieldData(FieldNo, ThisBuffer);
  finally
    if Assigned(TempBuffer) then
      TPlatformValueBuffer.Free(TempBuffer);
  end;
end;
{$ENDIF !NEXTGEN}

procedure TCustomSQLDataSet.SetCurrentBlobSize(Value: Int64);
begin
  if Value > FCurrentBlobSize then
    SetLength(FBlobBuffer, Value);
  FCurrentBlobSize := Value;
end;

function TCustomSQLDataSet.GetBlobFieldData(FieldNo: Integer; var Buffer: TBlobByteData): Integer;
var
  IsNull: LongBool;
  Ordinal: Integer;
begin
  Result := 0;
  Ordinal := FieldNo - 1;
  GetBlobLength(Self, FieldNo);
  if (FDBXReader = nil) then
    DatabaseError(SDataSetClosed, self);
  if FCurrentBlobSize > 0 then
  begin
    if LongWord(Length(Buffer)) < CurrentBlobSize then
      SetLength(Buffer, CurrentBlobSize);
    FDBXReader.ByteReader.GetBytes(Ordinal, 0, TArray<Byte>(Buffer), 0, FCurrentBlobSize, IsNull);
    if not IsNull then
      Result := CurrentBlobSize;
  end;
end;


constructor TCustomSQLDataSet.Create(AOwner: TComponent; DBXReader: TDBXReader; AOwnsInstance: Boolean);
begin
  Create(AOwner);
  FProvidedDBXReader := true;
  FOwnsProvidedDBXReader := AOwnsInstance;
  FDBXReader := DBXReader;

                                                                      
  //  DBXReader's connection properties as well (if possible), but for now, we can set this to
  //  the default value otherwise assuming it is unspecified there
  if FMaxBlobSize = 0 then   // means it hasn't been changed
    FMaxBlobSize := DefaultMaxBlobSize;
end;

function TCustomSQLDataSet.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  Result := TSQLBlobStream.Create(Field as TBlobField, Mode);
end;

procedure TCustomSQLDataSet.SetFieldData(Field: TField; Buffer: TValueBuffer);
var
  RecBuf: TArray<Byte>;
begin
  RecBuf := FCalcFieldsBuffer;
  if Field.FieldNo < 1 then   //{fkCalculated}
  begin
    if Assigned(Buffer) then
    begin
      RecBuf[Field.Offset] := 1;
      TPlatformValueBuffer.Copy(Buffer, RecBuf, Field.Offset+1, Field.DataSize);
    end
    else
      RecBuf[Field.Offset] := 0;
  end;
end;

{$IFNDEF NEXTGEN}
procedure TCustomSQLDataSet.SetFieldData(Field: TField; Buffer: Pointer);
var
  RecBuf: TArray<Byte>;
begin
  RecBuf := FCalcFieldsBuffer;
  if Field.FieldNo < 1 then   //{fkCalculated}
  begin
    if Assigned(Buffer) then
    begin
      RecBuf[Field.Offset] := 1;
      Move(Buffer^, RecBuf[Field.Offset+1], Field.DataSize);
    end
    else
      RecBuf[Field.Offset] := 0;
  end;
end;
{$ENDIF !NEXTGEN}

function TCustomSQLDataSet.GetCalculatedField(Field: TField; var Buffer: TValueBuffer): Boolean;
var
  RecBuf: TArray<Byte>;
begin
  Result := False;
  RecBuf := FCalcFieldsBuffer;
  if Field.FieldNo < 1 then   //{fkCalculated}
  begin
    if Boolean(RecBuf[Field.Offset]) then
    begin
      TPlatformValueBuffer.Copy(RecBuf, Field.Offset+1, Buffer, Field.DataSize);
      Result := True;
    end;
  end;
end;

function TCustomSQLDataSet.GetCanModify: Boolean;
begin
  Result := False;
end;

function TCustomSQLDataSet.GetCanRefresh: Boolean;
begin
  Result := True;
end;

procedure TCustomSQLDataSet.GetCommandNames(List: TStrings);
begin
  FSQLConnection.FDBXConnection.GetCommands(FDbxCommandType, List);
end;

function TCustomSQLDataSet.GetRecord(Buffer: TRecBuf; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
begin
  if FDBXReader.Next then
  begin
    GetCalcFields(Buffer);
    if Buffer <> 0 then
      Move(PByte(Buffer)[0], FCalcFieldsBuffer[0], Length(FCalcFieldsBuffer));
    Result := grOK
  end
  else
    Result := grEOF;
end;

{$IFNDEF NEXTGEN}
function TCustomSQLDataSet.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
begin
  if FDBXReader.Next then
  begin
    GetCalcFields(Buffer);
    if Buffer <> nil then
      Move(Buffer^, FCalcFieldsBuffer[0], Length(FCalcFieldsBuffer));
    Result := grOK
  end
  else
    Result := grEOF;
end;
{$ENDIF !NEXTGEN}

{ CommandText Management }

procedure TCustomSQLDataSet.SetFCommandText(const Value: string);
begin
  CheckInactive;
  FCommandText := Value;
  FNativeCommand := '';
end;

procedure TCustomSQLDataSet.SetCommandText(const Value: string);
var
  HasDataLink: Boolean;
  DataSet: TDataSet;
begin
  if FCommandText <> Value then
  begin
    CheckInactive;
    PropertyChanged;
    FCommandText := Trim(Value);
    if (SQLConnection <> nil) and (Value <> '') then
    begin
      if FParamCheck and (FCommandType <> ctTable) then
      begin
        HasDataLink := (FDataLink.DataSource <> nil) and (FDataLink.DataSource.DataSet is TCustomSQLDataSet);
        if HasDataLink then
          DataSet := FDataLink.DataSource.DataSet
        else
          DataSet := nil;
        SetParamsFromSQL(DataSet, not HasDataLink);
      end;
    end;
    DataEvent(dePropertyChange, 0);
  end;
end;

function TCustomSQLDataSet.GetDataSetFromSQL(TableName: string): TCustomSQLDataSet;
var
  QuoteChar: string;
begin
  if TableName = '' then
    TableName := GetTableNameFromSQLEx(SSelectStarFrom + CommandText.Substring(7, CommandText.Length - 7),
                                       GetIdOption(FSQLConnection));
  if TableName = '' then
    Result := nil
  else
  begin
    Result := TCustomSQLDataSet.Create(nil);
    try
      Result.SetConnection(Self.SQLConnection);
      QuoteChar := FSqlConnection.GetQuoteChar;
      Result.CommandText := SSelectStarFrom +
                  QuoteChar + TableName + QuoteChar +
                  SWhere + ' 0 = 1';    // only metadata is needed
      Result.Active := True;
    except
      FreeAndNil(Result);
    end;
  end;
end;

{ Parameters }

function TCustomSQLDataSet.GetProcParams: TProcParamList;
begin
  if (Self.FSQLConnection.Connected) and not Assigned(FProcParams) and ParamCheck then
  begin
    FProcParams := TProcParamList.Create;
    FSQLConnection.GetProcedureParams(CommandText, FSchemaInfo.PackageName, FSchemaName, FProcParams);
  end;
  Result := FProcParams;
end;

procedure TCustomSQLDataSet.SetParamsFromServerMethod;
var
  List: TParams;
  Command: TDBXCommand;
  Parameters: TDBXParameterList;
  Parameter: TDBXParameter;
  DataType: Integer;
  Ordinal: Integer;
  Count: Integer;
  ArgDesc: SPParamDesc;
begin
  List := TParams.Create;
  Command := nil;
  try
    try
      { Preserve existing values }
      List.AssignValues(Params);
      if Assigned(FProcParams) then
        FreeProcParams(FProcParams);
      ProcParams := TProcParamList.Create;
      FSQLConnection.Open;
      Command := FSQLConnection.FDBXConnection.CreateCommand;
      Command.CommandType := TDBXCommandTypes.DSServerMethod;
      Command.Text := FCommandText;
      Command.Prepare;
      Parameters := Command.Parameters;
      Count := Parameters.Count;
      for Ordinal := 0 to Count - 1 do
      begin
        Parameter := Parameters[Ordinal];
        ArgDesc := SPParamDesc.Create;
        ArgDesc.iParamNum := Ordinal;
        ArgDesc.iArgType := TParamType(Parameter.ParameterDirection);
        ArgDesc.szName := Parameter.Name;

        DataType := Parameter.DataType;
        if (DataType = TDBXDataTypes.TableType) then
        begin
          ArgDesc.iDataType := ftDataSet;
          ProcParams.Add(ArgDesc);
        end
        else if DataType = TDBXDataTypes.DBXConnectionType then
        begin
          ArgDesc.iDataType := ftConnection;
          ProcParams.Add(ArgDesc);
        end
        else if DataType = TDBXDataTypes.BinaryBlobType then
        begin
          ArgDesc.iDataType := ftBlob;
          ProcParams.Add(ArgDesc);
        end
        else
        begin
          if (DataType < TDBXDataTypes.MaxBaseTypes) and (DataType >= 0) then
            ArgDesc.iDataType := DataTypeMap[DataType]
          else
            ArgDesc.iDataType := ftUnknown;
          ArgDesc.iUnits1 := Parameter.Precision;
          ArgDesc.iUnits2 := Parameter.Scale;
          if ArgDesc.iDataType = ftBCD then
          begin
            // dbExpress only supports ftFMTBcd
            ArgDesc.iDataType := ftFMTBcd
          end;
          ArgDesc.iLen := Parameter.Size;
          AdjustProcedureDataLength(ArgDesc);
          ProcParams.Add(ArgDesc);
        end;
      end;
      LoadParamListItems(List, FProcParams);
      for Ordinal := 0 to Count - 1 do
      begin
        Parameter := Parameters[Ordinal];
        if Parameter.Literal then
          List[Ordinal].Value := Parameter.Value.AsString;
      end;
    except
        FreeProcParams(FProcParams);
    end;
    if List.Count > 0 then
      Params.Assign(List)
  finally
    List.Free;
    Command.DisposeOf;  // Force command to be removed from connection's command list
  end;
end;

procedure TCustomSQLDataSet.SetParamsFromProcedure;
var
  List: TParams;
begin
  List := TParams.Create;
  try
    try
      { Preserve existing values }
      List.AssignValues(Params);
      if Assigned(FProcParams) then
        FreeProcParams(FProcParams);
      ProcParams := TProcParamList.Create;
      FSQLConnection.GetProcedureParams(CommandText, FSchemaInfo.PackageName, FSchemaName, ProcParams);
      LoadParamListItems(List, FProcParams);
    except
        FreeProcParams(FProcParams);
    end;
    if List.Count > 0 then
      Params.Assign(List)
  finally
    List.Free;
  end;
end;

procedure TCustomSQLDataSet.SetParamsFromSQL(DataSet: TDataSet; bFromFields: Boolean);
var
  Field: TField;
  I: Integer;
  List: TSQLParams;
  DSCreated, WasDatasetActive: Boolean;
  FTblName: string;
begin
  DSCreated := False;
  FNativeCommand := CommandText.Substring(0, CommandText.Length);
  if (CommandType = ctStoredProc) then
  begin
    SetParamsFromProcedure;
    Exit;
  end
  else if CommandType = ctServerMethod then
  begin
    SetParamsFromServerMethod;
    Exit;
  end;
  List := TSQLParams.Create(Self, GetIdOption(SQLConnection));
  try                                              // DBExpress only supports '?', so
    FTblName := List.Parse(FNativeCommand, True);  // save query to avoid
    { Preserve existing values }                   // parsing again with prepare
    List.AssignValues(Params);
    if (Assigned(SQLConnection)) and (List.Count > 0) then
    begin
      WasDataSetActive := True;
      if DataSet = nil then
      begin
        if FTblName <> '' then
        begin
          if csDesigning in ComponentState then
          begin
            DataSet := GetDataSetFromSQL(FTblName);
            if Assigned(DataSet) then
              DSCreated := True;
          end;
        end;
      end
      else
      begin
        WasDataSetActive := DataSet.Active;
        if not DataSet.Active then DataSet.Active := True;
      end;
      for I := 0 to List.Count - 1 do
        List[I].ParamType := ptInput;
      if (DataSet <> nil) and
            ((not List.BindAllFields) or
            (List.Count = DataSet.FieldCount)) then
        try
          for I := 0 to List.Count - 1 do
          begin
            if List.BindAllFields then
              Field := DataSet.Fields[I]
            else if List.FFieldName.Count > I then
            begin
              if (bFromFields) then
                Field := DataSet.FieldByName(List.GetFieldName(I))
              else
                Field := DataSet.FieldByName(List[I].Name);
            end
            else
              Field := nil;
            if Assigned(Field) then
            begin
              if Field.DataType <> ftString then
                List[I].DataType := Field.DataType
              else if TStringField(Field).FixedChar then
                List[I].DataType := ftFixedChar
              else
                List[I].DataType := ftString;
            end;
          end;
        except
          // ignore exception: Column type won't be provided
        end;
      if List.Count > 0 then
        Params.Assign(List);
      if Assigned(DataSet) and (not WasDataSetActive) then DataSet.Active := False;
    end
    else
      Params.Clear;
  finally
    List.Free;
    if DSCreated then DataSet.Free;
  end;
end;

procedure TCustomSQLDataSet.RefreshParams;
var
  DataSet: TDataSet;
begin
  DisableControls;
  try
    if FDataLink.DataSource <> nil then
    begin
      DataSet := FDataLink.DataSource.DataSet;
      if DataSet <> nil then
        if DataSet.Active and (DataSet.State <> dsSetKey) then
          Refresh;
    end;
  finally
    EnableControls;
  end;
end;

procedure TCustomSQLDataSet.SetParamsFromCursor;
var
  I: Integer;
  DataSet: TDataSet;
begin
  if (FDataLink.DataSource <> nil) and (FParams.Count > 0) then
  begin
    DataSet := FDataLink.DataSource.DataSet;
    if (DataSet <> nil) then
    begin
      for I := 0 to FParams.Count - 1 do
        if not FParams[I].Bound then
        begin
          if not DataSet.EOF then
            FParams[I].AssignField(DataSet.FieldByName(FParams[I].Name))
          else
            FParams[I].Value := Null;
          FParams[I].Bound := False;
        end;
    end;
  end;
end;

function TCustomSQLDataSet.ParamByName(const Value: string): TParam;
begin
  Result := FParams.ParamByName(Value);
end;


procedure TCustomSQLDataSet.GetDataSetOutputParam(Param: TParam;
  Value: TDBXValue);
var
  DataSet: TCustomSQLDataset;
  ParamDataset: TDataSet;
begin
  if (csDesigning in ComponentState) then
    Param.Value := TDataSet.ClassName
  else if Value.ValueType.ParameterDirection = TDBXParameterDirections.ReturnParameter then
    Param.SetDataSet(Self, False)
  else
  begin
    ParamDataSet := Param.AsDataSet;
    if  (ParamDataSet <> nil)
      and (ParamDataSet is TCustomSQLDataSet)
      and (Value.ValueType.ParameterDirection = TDBXParameterDirections.InOutParameter) then
    begin
      DataSet := ParamDataSet as TCustomSQLDataSet;
      DataSet.Active := False;
      DataSet.FDBXReader := Value.GetDBXReader(False);
      DataSet.FProvidedDBXReader := True;
      DataSet.FOwnsProvidedDBXReader := True;
      DataSet.Active := True;
    end
    else
    begin
      DataSet := TCustomSQLDataSet.Create(nil, value.GetDBXReader(False), True);
      DataSet.Open;
      Param.SetDataSet(DataSet, True);
    end;
  end;
end;

function TCustomSQLDataSet.IsDbxCommandTypeStored: Boolean;
begin
  Result := not SameText(FDbxCommandType, TDBXCommandTypes.DbxSQL);
end;

procedure TCustomSQLDataSet.GetParamsOutputParam(Param: TParam; Value: TDBXValue);
var
  Params: TParams;
begin

  if (csDesigning in ComponentState) then
    Param.Value := TParams.ClassName
  else
  begin
    Params := Param.AsParams;
    if (Params <> nil) then
      TDBXParamsReader.CopyReaderToParams(Value.GetDBXReader(Param.ParamType <> ptResult), Params)
    else
    begin
      Params := TDBXParamsReader.ToParams(nil, Value.GetDBXReader(Param.ParamType <> ptResult), False);
      Param.SetParams(Params, True);
    end;
  end;
end;

procedure TCustomSQLDataSet.GetOutputParams(AProcParams: TProcParamList);
var
  I: Integer;
  ArgDesc: SPParamDesc;
  Param: TParam;
  TimeStamp: TTimeStamp;
  CurrencyValue: Currency;
  Bytes: TArray<Byte>;
  Unmarshal: TJSONUnMarshal;
begin
  if AProcParams = nil then
    ArgDesc := SPParamDesc.Create
  else
    ArgDesc := nil;
  try
    for I := 0 to Params.Count - 1 do
    begin
      if AProcParams <> nil then
        ArgDesc := (SPParamDesc(AProcParams.Items[I]))
      else
      begin
        ArgDesc.iParamNum := I + 1;
        ArgDesc.szName := Params[I].Name;
        ArgDesc.iArgType := Params[I].ParamType;
        ArgDesc.iDataType := Params[I].DataType;
        ArgDesc.iUnits1 := Params[I].Precision;
        ArgDesc.iUnits2 := Params[I].NumericScale;
        if Params[I].IsNull then
          ArgDesc.iLen := 0
        else
          ArgDesc.iLen := Params[I].GetDataSize
      end;
      if (Params[I].ParamType in [ptOutput, ptResult, ptInputOutput]) and
         (ArgDesc.iDataType <> ftCursor) then
      begin
        Param := Params[I];
        FDBXCommand.Parameters[I].ParameterDirection := TDBXParameterDirection(Param.ParamType);
        if not (((Param.DataType = ftFloat) and (FDBXCommand.Parameters[I].DataType = TDBXDataTypes.SingleType)) or
                ((Param.DataType = ftFMTBcd) and (FDBXCommand.Parameters[I].DataType = TDBXDataTypes.BcdType)) or
                ((Param.DataType = ftCurrency) and (FDBXCommand.Parameters[I].DataType = TDBXDataTypes.CurrencyType))) then
          FDBXCommand.Parameters[I].DataType := FldTypeMap[Param.DataType];
        FDBXCommand.Parameters[I].Precision := Param.Size;

        case Param.DataType of
          ftBlob, ftGraphic..ftTypedBinary,ftOraBlob,ftOraClob, ftBytes, ftVarBytes:
          begin
            // dynalink dbx drivers cannot handle requests for blob length
            // that is made by the IsNull check.  See TDBXBugsSuite.Native s:TestRaid_238830
            // for an example where thiw will not work for Interbase.
            //
            if FCommandType = ctServerMethod then
            begin
              if FDBXCommand.Parameters[I].Value.IsNull then
                Param.Value := Null
              else
              if (csDesigning in ComponentState) then
                Param.Value := TStream.ClassName
              else
                Param.SetStream(FDBXCommand.Parameters[I].Value.GetStream(False), True, FDBXCommand.Parameters[I].Value.GetValueSize);
            end
            else
            begin
              FDBXCommand.Parameters[I].Size := Params[I].Size;
              //Two byte length prefix for varbinary
              if (Param.DataType = ftBytes) or (Param.DataType = ftVarBytes) then
                SetLength(Bytes, FDBXCommand.Parameters[I].Size+2)
              else
                SetLength(Bytes, FDBXCommand.Parameters[I].Size);
              FDBXCommand.Parameters[I].Value.GetBytes(0, Bytes, 0, FDBXCommand.Parameters[I].Size);
              if FDBXCommand.Parameters[I].Value.IsNull then
                Param.Value := Null
              else
              begin
                Param.SetBlobData(Bytes, Length(Bytes));
                SetLength(Bytes, 0);
              end;
            end;
          end
          else
          begin
            if Param.DataType <> ftObject then
              FDBXCommand.Parameters[I].Size := ArgDesc.iLen;
            if FDBXCommand.Parameters[I].Value.IsNull then
              if Param.DataType = ftObject then
                Param.AsObject := nil
              else
                Param.Value := Null
            else
              case Param.DataType of
                ftObject:
                  if FDBXCommand.Parameters[I].SubType = TDBXDataTypes.UserSubType then
                  begin
                    // convert TObject to JSON
                    Unmarshal := TJSONConverters.GetJSONUnMarshaler;
                    try
                      Param.AsObject := Unmarshal.Unmarshal(FDBXCommand.Parameters[I].Value.GetJSONValue(True))
                    finally
                      Unmarshal.Free;
                    end;
                  end
                  else
                    Param.AsObject := FDBXCommand.Parameters[I].Value.GetJSONValue(False); // False means Value object will not free JSONValue
                ftWord:
                  Param.AsWord := FDBXCommand.Parameters[I].Value.AsUInt16;
                ftByte:
                  Param.AsByte := FDBXCommand.Parameters[I].Value.AsUInt8;
                ftShortInt:
                  Param.AsShortInt := FDBXCommand.Parameters[I].Value.AsInt8;
                ftSmallInt:
                  Param.AsSmallInt := FDBXCommand.Parameters[I].Value.AsInt16;
                ftAutoInc, ftInteger:
                  Param.Value := FDBXCommand.Parameters[I].Value.AsInt32;
                ftLargeint:
                  Param.Value := FDBXCommand.Parameters[I].Value.AsInt64;
                ftTime:
                  begin
                    TimeStamp.Time := FDBXCommand.Parameters[I].Value.AsTime;
                    TimeStamp.Date := DateDelta;
                    Param.AsTime := TimeStampToDateTime(TimeStamp);
                  end;
                ftDate:
                  begin
                    TimeStamp.Time := 0;
                    TimeStamp.Date := FDBXCommand.Parameters[I].Value.AsDate;
                    Param.AsDate := TimeStampToDateTime(TimeStamp);
                  end;
                ftDateTime:
                  Param.AsDateTime := FDBXCommand.Parameters[I].Value.AsDateTime;
                ftTimeStamp:
                  Param.AsSQLTimeStamp := FDBXCommand.Parameters[I].Value.AsTimeStamp;
                ftTimeStampOffset:
                  Param.AsSQLTimeStampOffset := FDBXCommand.Parameters[I].Value.AsTimeStampOffset;
                ftCurrency, ftBCD:
                  if BCDToCurr(FDBXCommand.Parameters[I].Value.GetBcd, CurrencyValue) then
                    Param.AsBCD := CurrencyValue
                  else
                    Param.AsBCD := 0;
                ftFMTBCD:
                  Param.AsFMTBCD := FDBXCommand.Parameters[I].Value.AsBcd;
                ftVariant:
                  Param.Value := FDBXCommand.Parameters[I].Value.AsVariant;
                ftSingle:
                  Param.AsSingle := FDBXCommand.Parameters[I].Value.AsSingle;
                ftFloat:
                begin
                  if (FDBXCommand.Parameters[I].DataType = TDBXDataTypes.SingleType) then
                    Param.AsFloat := FDBXCommand.Parameters[I].Value.AsSingle
                  else
                    Param.AsFloat := FDBXCommand.Parameters[I].Value.AsDouble;
                end;
                ftBoolean:
                  Param.AsBoolean := FDBXCommand.Parameters[I].Value.AsBoolean;
{$IFNDEF NEXTGEN}
                ftMemo:
                  Param.AsMemo := string(FDBXCommand.Parameters[I].Value.GetAnsiString);
                ftString, ftFixedChar:
                  Param.AsAnsiString :=  FDBXCommand.Parameters[I].Value.GetAnsiString;
{$ELSE}
                ftMemo, ftString, ftFixedChar,
{$ENDIF !NEXTGEN}
                ftWideString, ftWideMemo, ftFixedWideChar:
                  Param.AsWideString := FDBXCommand.Parameters[I].Value.AsString;
                ftStream:
                begin
                  if (csDesigning in ComponentState) then
                    Param.Value := TStream.ClassName
                  else
                    Param.SetStream(FDBXCommand.Parameters[I].Value.GetStream(False), True, FDBXCommand.Parameters[I].Value.GetValueSize);
                end;
                ftParams:
                  GetParamsOutputParam(Param, FDBXCommand.Parameters[I].Value);
                ftDataSet:
                  GetDataSetOutputParam(Param, FDBXCommand.Parameters[I].Value)
              else
                DatabaseErrorFmt(SBadFieldType, [FDBXCommand.Parameters[I].Name], Self);
              end;
            end;
          end;
        end;
      end;
  finally
    if AProcParams = nil then
      ArgDesc.Free;
  end;
end;

procedure TCustomSQLDataSet.SetParameters(const Value: TParams);
begin
  FParams.AssignValues(Value);
end;

{ Query Management }

procedure TCustomSQLDataSet.SetPrepared(Value: Boolean);
var
  Complete: Boolean;
begin
  if Value then CheckConnection(eConnect);
  if FGetNextRecordSet or FProvidedDBXReader then
    FPrepared := Value
  else
    FreeReader;
  if SchemaInfo.FType <> stNoSchema then
  begin
    if Value then
      CheckStatement(True)
    else
      FreeCommand;
  end
  else
  if Value <> Prepared then
  begin
//    try
//      if Value then
//        begin
//          if FDBXCommand <> nil then DatabaseError(SSQLDataSetOpen, Self);
//          FRowsAffected := -1;
//          FCheckRowsAffected := True;
//          PrepareStatement;
//        end
//      else
//        begin
//          if FCheckRowsAffected then
//            FRowsAffected := RowsAffected;
//          FreeCommand;
//          if Assigned(FSQLConnection) then
//            FSQLConnection.CheckDisconnect;
//        end;
//      FPrepared := Value;
//    except
//      if Assigned(FDBXCommand) then
//        FreeCommand;
//      FPrepared := False;
//    end;

    Complete := false;
    if Value then
      try
        if FDBXCommand <> nil then DatabaseError(SSQLDataSetOpen, Self);
        FRowsAffected := -1;
        FCheckRowsAffected := True;
        PrepareStatement;
        Complete := true;
      finally
        if not Complete then
        begin
          if Assigned(FDBXCommand) then
            FreeCommand;
          FPrepared := False;
        end
      end
    else
      try
        if FCheckRowsAffected then
          FRowsAffected := RowsAffected;
        FreeCommand;
        if Assigned(FSQLConnection) then
          FSQLConnection.CheckDisconnect;
      except
        if Assigned(FDBXCommand) then
          FreeCommand;
        FPrepared := False;
      end;

    FPrepared := Value;
  end;
end;

procedure TCustomSQLDataSet.CheckStatement(ForSchema: Boolean = False);
var
  Connection: TSqlConnection;
  RowsetSize: Integer;
begin
  RowsetSize := defaultRowsetSize;
  if not Assigned(FSQLConnection) then
    DatabaseError(SMissingSQLConnection);
  if (FSQLConnection.FMaxStmtsPerConn > 0) and (FSQLConnection.FSelectStatements >= FSQLConnection.FMaxStmtsPerConn)
       and (FSQLConnection.FSelectStatements > 0) and not (FSQLConnection.FTransactionCount > 0) and FSQLConnection.AutoClone then
  begin
    Connection := FSQLConnection.CloneConnection;
    FClonedConnection := Connection;
  end
  else
    Connection := FSQLConnection;
  if Connection.LoadParamsOnConnect then
    Connection.LoadParamsFromIniFile;
  if Assigned(FDBXCommand) then
    FreeCommand;
  if not Assigned(Connection.DBXConnection) then
    DatabaseError(SdatabaseOpen, Self);
  if not ForSchema then
  begin
    if FCommandText.Length = 0 then
      DatabaseError(SEmptySQLStatement, Self);
    FDBXCommand := Connection.DBXConnection.CreateCommand;
    FDBXCommand.CommandType := FDbxCommandType;

    if FSQLConnection.Params.Values[ROWSETSIZE_KEY] <> '' then
    try
      RowsetSize := StrToInt(Trim(FSQLConnection.Params.Values[ROWSETSIZE_KEY]));
    except
      RowsetSize := defaultRowsetSize;
    end;

    if Assigned(Connection.MetaData) and Connection.MetaData.SupportsRowSetSize then
      FDBXCommand.RowSetSize := RowsetSize;

    FStatementOpen := True;

    if FNativeCommand = '' then
    begin
      if FParams.Count > 0 then
        FNativeCommand := FixParams(CommandText, FParams.Count, Connection.GetQuoteChar)
      else
        FNativeCommand := CommandText;
    end;
  end;
end;

function TCustomSQLDataSet.GetQueryFromType: string;
var
  STableName : string;
begin
  case CommandType of
     ctTable:
       begin
         if FSortFieldNames > '' then
         begin
           if Self.FSchemaName <> '' then
             STableName := QuoteIdentifier(FSchemaName + '.' + FCommandText, false)
           else
             STableName := QuoteIdentifier(FCommandText, false);
           Result := SSelectStarFrom + STableName + SOrderBy + FSortFieldNames
         end
         else
         begin
           if FNativeCommand = '' then
           begin
             if Self.FSchemaName <> '' then
               STableName := QuoteIdentifier(FSchemaName + '.' + FCommandText, false)
             else
               STableName := QuoteIdentifier(FCommandText, false);
           end
           else
           begin
             if Trim(FSchemaName) <> '' then
               STableName := QuoteIdentifier(FSchemaName + '.' + FNativeCommand, false)
             else
               STableName := QuoteIdentifier(FNativeCommand, false);
           end;
           Result := SSelectStarFrom + STableName
         end;
       end;
     ctStoredProc:
       begin
         if FSchemaName <> '' then
           Result := QuoteIdentifier(FSchemaName + '.' + FCommandText, true)
         else
           Result := QuoteIdentifier(FCommandText, true)
       end;
     ctServerMethod:
       begin
           Result := FCommandText;
       end;
     else
       if (FSortFieldNames > '') and (FCommandText.ToLower.IndexOf(SOrderBy) = -1) then
         Result := FNativeCommand + SOrderBy + FSortFieldNames
       else
         Result := FNativeCommand;
  end;
end;

function TCustomSQLDataSet.CheckDetail(const SQL: string): string;
begin
  Result := SQL;
  if SQL.IndexOf(SParam) = -1 then
    if SQL.ToLower.IndexOf(SSelect) > -1 then // Select Query with no ?, but Parameters are set
      Result := AddParamSQLForDetail(Params, SQL, True);
end;

procedure TCustomSQLDataSet.PrepareStatement;
var
  SQLText, Value: string;
  CurSection : TSqlToken;
  Start: NativeInt;
  IdOption: IDENTIFIEROption;
begin
  if CommandText.Length = 0 then
    DatabaseError(SEmptySQLStatement, Self);
  IdOption := GetIdOption(FSQLConnection);
  CurSection := stUnknown;
  CheckStatement;
  SQLText := GetQueryFromType;
  if CommandType <> ctStoredProc then
  begin
    Start := 1;
    CurSection := Platform_NextSQLToken(SQLText, Start, Value, CurSection, IdOption);
    if (CurSection = stSelect) or (CommandType = ctTable) then
      Inc(FSQLConnection.FSelectStatements);
    if Params.Count > 0 then
      SQLText := CheckDetail(SQLText);
    FDBXCommand.CommandType := FDbxCommandType;
  end
  else
    FDBXCommand.CommandType := FDbxCommandType;
  FDBXCommand.Text := SQLText;
  if (Params.Count > 0) and (CommandType <> ctServerMethod) then
    FDBXCommand.Parameters.SetCount(Params.Count);
  FDBXCommand.Prepare;
end;

procedure TCustomSQLDataSet.CheckPrepareError;
begin
  if (FDBXCommand = nil) and (SchemaInfo.FType = stNoSchema) and not FProvidedDBXReader then
  begin     // prepare has failed
    if (CommandType = ctQuery) or (SortFieldNames <> '') then
      DatabaseError(sPrepareError)
    else
      DatabaseError(sObjectNameError);
  end;
end;

function TCustomSQLDataSet.ExecSQL(ExecDirect: Boolean = False): Integer;
var
  WasPrepared: Boolean;
begin
  CheckInActive;
  CheckConnection(eConnect);
  FRowsAffected := 0;

  if ExecDirect then
  begin
    try
      CheckStatement;
      FDBXCommand.CommandType := FDbxCommandType;
      FDBXCommand.Text := CommandText;
      FDBXCommand.Prepare;
      FDBXReader := FDBXCommand.ExecuteQuery;
      if FDBXReader <> nil then
         SetParamsFromCursor;
      Result := RowsAffected;
    finally
      if Assigned(FDBXReader) then
        FreeReader;
      FreeCommand;
      CloseStatement;
    end;
  end else
  begin
    WasPrepared := Prepared;
    try
      SetPrepared(True);
      CheckPrepareError;
      ExecuteStatement;
      if FDBXReader <> nil then
         SetParamsFromCursor;
      Result := RowsAffected;
    finally
      if Assigned(FDBXReader) then
        FreeReader;
      if not WasPrepared then
      begin
        FreeCommand;
        CloseStatement;
      end;
    end;
  end;
end;

procedure TCustomSQLDataSet.ExecuteStatement;

 function UseParams: Boolean;
//  var
//    SQL: string;
  begin
    Result := (FParams.Count <> 0);
    if Result and (FCommandType = ctTable) then
       Result := SqlRequiresParams(FDBXCommand.Text);
  end;

begin
  if SchemaInfo.FType = stNoSchema then
    begin
      if Assigned(FParams) and not FGetNextRecordSet then
      begin
        if (CommandType = ctStoredProc) {or (CommandType = ctServerMethod) }then
          SetQueryProcParams(Self.FSQLConnection, FDBXCommand, Params, ProcParams)
        else
        if UseParams() then
          SetQueryProcParams(Self.FSQLConnection, FDBXCommand, Params);
      end;
      if FProvidedDBXReader then
      begin
        if Active then
          Active := False
      end
      else
      if FGetNextRecordSet then
      begin
        FreeAndNil(FDBXReader);
        FDBXReader := FDBXCommand.GetNextReader;
        if FDBXReader <> nil then
        begin
          if Active then
            Active := False
          else  // Active might be false when calling getNextCursor.
            CloseCursor;
        end else
          begin
            if (CommandType = ctStoredProc) or (CommandType = ctServerMethod) then
              begin
                if Params.Count > 0 then
                  GetOutputParams(FProcParams);
              end
            else
              begin
                if Params.Count > 0 then
                  GetOutputParams;
              end;
          end;
      end
      else
      begin
        FDBXReader := FDBXCommand.ExecuteQuery;
        if (CommandType = ctStoredProc) or (CommandType = ctServerMethod) then
          begin
            if Params.Count > 0 then
              GetOutputParams(FProcParams);
          end
        else
          begin
            if Params.Count > 0 then
              GetOutputParams;
          end;
      end;
    end
  else
    OpenSchema;
  FStatementOpen := True;
  FRecords := -1;
end;

function TCustomSQLDataSet.GetObjectProcParamCount: Integer;
var
  I, LastParamNum: Integer;
  ArgDesc: SPParamDesc;
begin
  GetProcParams;    // make sure FProcParams is loaded.
  Result := 0;
  LastParamNum := 0;
  for I := 0 to Params.Count -1 do
  begin
    ArgDesc := (SPParamDesc(ProcParams.Items[I]));
    if ArgDesc.iParamNum <> LastParamNum then Inc(Result);
    LastParamNum := ArgDesc.iParamNum;
  end;
end;

function TCustomSQLDataSet.GetParamCount: Integer;
var
  I : Integer;
begin
  Result := FParamCount;
  if Result = -1 then
  begin
    Result := 0;
    if Assigned(FParams) then
    begin
      if FCommandType = ctStoredProc then
      begin
        for I := 0 to Params.Count -1 do
        begin
          if Params.Items[I].DataType in [ftADT, ftARRAY] then
          begin
            Result := GetObjectProcParamCount;
            break;
          end;
        end;
      end;
      if Result = 0 then
        Result := FParams.Count
    end;
  end;
end;


function GetRows(Query: string; Connection: TSQLConnection): Integer;
var
  DS: TSQLDataSet;
begin
  Result := -1;
  DS := TSQLDataSet.Create(nil);
  try
    DS.SQLConnection := Connection;
    DS.CommandText := Query;
    DS.Active := True;
    if not DS.EOF then
      Result := DS.Fields[0].AsInteger;
  finally
    DS.DisposeOf;
    if Result = -1 then
      DatabaseError(SNotSupported);
  end;
end;

function TCustomSQLDataSet.GetRecordCount: Integer;
const
  SDistinct = ' distinct ';                 { do not localize }
  SSelectCount = 'select count(*) from ';   { do not localize }
var
  TableName, Query: string;
  HoldPos: Integer;
begin
  if FRecords <> -1 then
    Result := FRecords
  else
  begin
    CheckConnection(eConnect);
    if Self.CommandText = '' then
      DatabaseError(SNoSQLStatement);
    case CommandType of
      ctServerMethod,
      ctStoredProc:
        DatabaseError(SNotSupported);
      ctTable:
        Query := 'select count(*) from ' + QuoteIdentifier(CommandText, false);
      ctQuery:
        begin
          TableName := GetTableNameFromSQLEx(FCommandText, GetIdOption(FSQLConnection));
          if (TableName = '') or (Params.Count > 0) then
            DatabaseError(SNotSupported);
          if FCommandText.ToLower.IndexOf(SDistinct) = -1 then
            Query := SSelectCount
          else
            DatabaseError(SNotSupported);
          HoldPos := FCommandText.ToLower.IndexOf(SWhere) + 1;
          if HoldPos = 0 then
            Query := Query + GetQuoteChar + TableName + GetQuoteChar
          else
          begin
            Query := Query + GetQuoteChar + TableName + GetQuoteChar + FCommandText.Substring(HoldPos - 1, FCommandText.Length - (HoldPos-1));
            HoldPos := Query.ToLower.IndexOf(sOrderBy) + 1;
            if HoldPos > 0 then
              Query := Query.Substring(0, HoldPos - 1);
          end;
        end;
    end;
    FRecords := GetRows(Query, FSQLConnection);
    Result := FRecords;
  end;
end;

function TCustomSQLDataSet.GetRowsAffected: Integer;
var
  UpdateCount: LongWord;
begin
  if FRowsAffected > 0 then
    Result := Integer(FRowsAffected)
  else
    begin
      if FDBXCommand <> nil then
        UpdateCount := FDBXCommand.RowsAffected
      else
        UpdateCount := 0;
      FRowsAffected := Integer(UpdateCount);
      Result := Integer(UpdateCount);
    end;
end;

{ Misc. Set/Get Property }

procedure TCustomSQLDataSet.SetDataSource(Value: TDataSource);
begin
  if IsLinkedTo(Value) then DatabaseError(SCircularDataLink, Self);
  if FDataLink.DataSource <> Value then
    FDataLink.DataSource := Value;
end;

procedure TCustomSQLDataSet.SetDbxCommandType(const Value: string);
begin
  if Value = TDBXCommandTypes.DbxStoredProcedure then
    FCommandType := ctStoredProc
  else if Value = TDBXCommandTypes.DSServerMethod then
    FCommandType := ctServerMethod
  else if Value = TDBXCommandTypes.DbxTable then
    FCommandType := ctTable
  else
    FCommandType := ctQuery;

  FDbxCommandType := Value;
end;

function TCustomSQLDataSet.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TCustomSQLDataSet.GetDetailLinkFields(MasterFields, DetailFields: TList<TField>);

  function AddFieldToList(const FieldName: string; DataSet: TDataSet;
    List: TList<TField>): Boolean;
  var
    Field: TField;
  begin
    Field := DataSet.FindField(FieldName);
    if Field <> nil then
      List.Add(Field);
    Result := Field <> nil;
  end;

var
  I: Integer;
begin
  MasterFields.Clear;
  DetailFields.Clear;
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    for I := 0 to Params.Count - 1 do
      if AddFieldToList(Params[I].Name, DataSource.DataSet, MasterFields) then
        AddFieldToList(Params[I].Name, Self, DetailFields);
end;

{$IFNDEF NEXTGEN}
procedure TCustomSQLDataSet.GetDetailLinkFields(MasterFields, DetailFields: TList);

  function AddFieldToList(const FieldName: string; DataSet: TDataSet;
    List: TList): Boolean;
  var
    Field: TField;
  begin
    Field := DataSet.FindField(FieldName);
    if Field <> nil then
      List.Add(Field);
    Result := Field <> nil;
  end;

var
  I: Integer;
begin
  MasterFields.Clear;
  DetailFields.Clear;
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    for I := 0 to Params.Count - 1 do
      if AddFieldToList(Params[I].Name, DataSource.DataSet, MasterFields) then
        AddFieldToList(Params[I].Name, Self, DetailFields);
end;
{$ENDIF !NEXTGEN}

function TCustomSQLDataSet.GetSortFieldNames: string;
begin
  Result := FSortFieldNames;
end;

procedure TCustomSQLDataSet.SetSortFieldNames(Value: string);
begin
  if Value <> FSortFieldNames then
  begin
    CheckInactive;
    FSortFieldNames := Value;
    SetPrepared(False);
  end;
end;

procedure TCustomSQLDataSet.SetMaxBlobSize(MaxSize: Integer);
begin
  FMaxBlobSize := MaxSize;
end;

procedure TCustomSQLDataSet.SetCommandType(const Value: TSQLCommandType);
begin
  if FCommandType <> Value then
  begin
    CheckInactive;
    FCommandType := Value;
    case Value of
      ctQuery:        FDbxCommandType := TDBXCommandTypes.DbxSQL;
      ctTable:        FDbxCommandType := TDBXCommandTypes.DbxTable;
      ctStoredProc:   FDbxCommandType := TDBXCommandTypes.DbxStoredProcedure;
      ctServerMethod: FDbxCommandType := TDBXCommandTypes.DSServerMethod;
    end;
    PropertyChanged;
    DataEvent(dePropertyChange, 0);
  end;
end;

procedure TCustomSQLDataSet.PropertyChanged;
begin
  if not (csLoading in ComponentState) then
  begin
    SetPrepared(False);
    FNativeCommand := '';
    FRecords := -1;
    FreeCommand;
    if SortFieldNames <> '' then
      FSortFieldNames := '';
    if FCommandText <> '' then
      FCommandText := '';
    FParams.Clear;
  end;
end;

{ Miscellaneous }

function TCustomSQLDataSet.IsSequenced: Boolean;
begin
  Result := False;
end;

procedure TCustomSQLDataSet.DefineProperties(Filer: TFiler);

  function DesignerDataStored: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := TCustomSQLDataSet(Filer.Ancestor).DesignerData <> DesignerData else
      Result := DesignerData <> '';
  end;

begin
  inherited;
  Filer.DefineProperty('DesignerData', ReadDesignerData, WriteDesignerData,
    DesignerDataStored);
end;

procedure TCustomSQLDataSet.ReadDesignerData(Reader: TReader);
begin
  FDesignerData := Reader.ReadString;
end;

procedure TCustomSQLDataSet.WriteDesignerData(Writer: TWriter);
begin
  Writer.WriteString(FDesignerData);
end;


procedure TCustomSQLDataSet.InternalHandleException;
begin
end;

{ Index Support }

procedure TCustomSQLDataSet.UpdateIndexDefs;
begin
  AddIndexDefs(Self);
end;

function TCustomSQLDataSet.CheckFieldNames(const FieldNames: string): Boolean;
var
  S: string;
  Pos: Integer;
begin
  Result := True;
  S := FieldNames;
  Pos := 1;
  while Result and (Pos <= S.Length) do
    Result := FindField(ExtractFieldName(S, Pos)) <> nil;
end;

const
  IDX_TYPE_FIELD = 'INDEX_TYPE';           { Do not localize }
  IDX_SORT_FIELD = 'SORT_ORDER';           { Do not localize }
  DescendingOrder = 'D';                   { Do not localize }

procedure TCustomSQLDataSet.AddIndexDefs(SourceDS: TCustomSQLDataSet; IndexName: string = '');

  function DontUseIndex: Boolean;
  begin
    Result := CommandType in [ctQuery, ctStoredProc, ctServerMethod];
    if Result and (CommandType = ctQuery) then
      Result := IsMultiTableQuery(CommandText);
    if Result then FIndexDefsLoaded := True;
  end;

var
  DataSet: TCustomSQLDataSet;
  TableName, IdxName, SortOrder, FieldNames: string;
  IdxType: Integer;
  Options: TIndexOptions;
  IdxDef: TIndexDef;
  MetaData: TDBXDatabaseMetaData;
  Dbx4Metadata: Boolean;
  IndexColumns: TDBXIndexColumns;
  FirstColumnAscending: Boolean;
begin
  if not FGetMetadata then FIndexDefsLoaded := True;
  if FIndexDefsLoaded then Exit;
  if SchemaInfo.FType <> stNoSchema then Exit;
  if DontUseIndex then Exit;
  if FCommandType = ctTable then
    TableName := FCommandText
  else
    TableName := GetTableNameFromSQLEx(CommandText, GetIdOption(FSQLConnection));
  DataSet := FSQLConnection.OpenSchemaTable(stIndexes, TableName, '', '', '');
  MetaData := FSQLConnection.FDBXConnection.DatabaseMetaData;
  IndexColumns := nil;
  if Assigned(MetaData) and (MetaData.MetaDataVersion = DBXVersion40) then
    Dbx4Metadata := true
  else
    Dbx4Metadata := false;
  try
    FIndexDefs.Clear;
    IndexDefs.Clear;
    if Dbx4Metadata then
    begin
      while not DataSet.EOF do
      begin
        begin
          Options := [];
          IdxName := DataSet.FieldByName(GetIndexFieldName(FSQLConnection)).AsString;
          if ((IndexName = '') or (IdxName = IndexName)) and (IdxName <> '') then
          begin
            if not MetaData.SupportsCatalogFunctions then
            begin
              IndexColumns := TDBXIndexColumns.Create;
              try
                IndexColumns.FGetIndexesText := DataSet.FDBXCommand.Text;
                IndexColumns.FSqlConnection := DataSet.FSQLConnection;
                IndexColumns.Open;
                if IndexColumns.HasAllFieldNames(IdxName, SourceDS, FieldNames, FirstColumnAscending) then
                begin
                  if DataSet.FieldByName(TDBXIndexesColumns.IsPrimary).Value then
                    Options := Options + [ixPrimary];
                  if DataSet.FieldByName(TDBXIndexesColumns.IsUnique).Value then
                    Options := Options + [ixUnique];
                  // vcl assumes all index columns are all asc or all desc, so
                  // just look at the first.
                  //
                  if not FirstColumnAscending then
                    Options := Options + [ixDescending];
                  FIndexDefs.Add(IdxName, FieldNames, Options);
                end;
              finally
                FreeAndNil(IndexColumns);
              end;
            end
            else
            begin
              if DataSet.FieldByName(TDBXIndexesColumns.IsUnique).AsBoolean = False then
                Options := Options + [ixUnique];
              if DataSet.FieldByName(TDBXIndexesColumns.AscDesc).AsString = 'D' then
                Options := Options + [ixDescending];
                                                  
              IdxDef := TIndexDef(TDefCollection(FIndexDefs).Find(IdxName));
              if Assigned(IdxDef) then
                IdxDef.Fields := IdxDef.Fields + ';' + DataSet.FieldByName(TDBXIndexesColumns.ColumnName).AsString
              else
                FIndexDefs.Add(IdxName, DataSet.FieldByName(TDBXIndexesColumns.ColumnName).AsString, Options);
            end;
          end;
        end;
        DataSet.Next;
      end;
    end else
    begin
      while not DataSet.EOF do
      begin
        begin
          Options := [];
          IdxName := DataSet.FieldByName(GetIndexFieldName(FSQLConnection)).Value;
          if (IndexName = '') or (IdxName = IndexName) then
          begin
            if IndexDefs.IndexOf(IdxName) = -1 then
            begin
              FieldNames := DataSet.FieldByName(GetColumnFieldName(FSQLConnection)).Value;
              // don't add indexes on fields not in result set
              if SourceDS.CheckFieldNames(FieldNames) then
              begin
                IdxType := DataSet.FieldByName(IDX_TYPE_FIELD).Value;
                if (IdxType and eSQLPrimaryKey) = eSQLPrimaryKey then
                  Options := Options + [ixPrimary];
                if (IdxType and eSQLUnique) = eSQLUnique then
                  Options := Options + [ixUnique];
                SortOrder := DataSet.FieldByName(IDX_SORT_FIELD).Value;
                if SortOrder = DescendingOrder then
                  Options := Options + [ixDescending];
                FIndexDefs.Add(IdxName, FieldNames, Options);
              end;
            end else
            begin
              IdxDef := IndexDefs.Find(IdxName);
              IdxDef.Fields := IdxDef.Fields + ';' + DataSet.FieldByName(GetColumnFieldName(SQLConnection)).Value;
            end;
          end;
        end;
        DataSet.Next;
      end;
    end;
  finally
    FreeAndNil(IndexColumns);
    FSQLConnection.FreeSchemaTable(DataSet);
  end;
  FIndexDefsLoaded := True;
end;


function TCustomSQLDataSet.AddMetadataQuotes(Identifier: string; StoredProc: Boolean): string;
var
  QuoteChar:  string;
begin
//  if StoredProc then
//    QuoteChar := FSQLConnection.FQuoteChar
//  else
    QuoteChar := FSQLConnection.FQuoteChar;
  // We have to have a quote character for metadata even if the driver
  // indicates that it does not have one.  Informix driver currently
  // reports that it does not have a quote character, but allows spaces
  // in its identifiers.
  //
  if QuoteChar = '' then
    QuoteChar := '"';
  if Identifier <> '' then
    Result := QuoteChar + StringReplace(Identifier, QuoteChar, QuoteChar + QuoteChar, [rfReplaceAll]) + QuoteChar
  else
    Result := Identifier;
end;

function TCustomSQLDataSet.GetKeyFieldNames(List: TStrings): Integer;
var
  I: Integer;
begin
  if not FIndexDefsLoaded then
    AddIndexDefs(Self);
  Result := IndexDefs.Count;
  List.BeginUpdate;
  try
    List.Clear;
    for I := 0 to Result - 1 do
      List.Add(IndexDefs[I].Fields);
  finally
    List.EndUpdate;
  end;
end;

{ Schema Tables }

procedure TCustomSQLDataSet.SetSchemaInfo(SchemaType: TSchemaType; SchemaObjectName, SchemaPattern: string; PackageName: string = '' );
begin
  FreeCommand;
  FSchemaInfo.FType := SchemaType;
  FSchemaInfo.ObjectName := SchemaObjectName;
  FSchemaInfo.Pattern := SchemaPattern;
  FSchemaInfo.PackageName := PackageName;
end;

procedure TCustomSQLDataSet.OpenSchema;


  function ExtractObjectName(Value: string): string;
  var
    NamePos: Integer;
    Q: string;
  begin
    Result := Value;
    Q := GetQuoteChar;
    if (Q = '') or (Q = ' ') then exit;
    NamePos := Value.IndexOf('.' + Q) + 1;
    if NamePos = 0 then
      NamePos := Value.IndexOf(Q + '.') + 1;
    if NamePos = 0 then exit;
    Result := Value.Substring(NamePos + 1, Value.Length - NamePos);
    if Result.IndexOf(Q) > -1 then
      Result := Result.Substring(0, Result.Length - 1);
  end;


  function MakeDbxMetadataCommand(CommandName: string; Args: array of string): string;
  var
    I: Integer;
  begin
    Result := CommandName;
    for I := Low(Args) to High(Args) do
      Result := Result + ' ' + Args[I];
  end;

  procedure AppendToIdentifier(var Identifier: string; NewElement: string);
  begin
    if NewElement = '' then
    begin
      if Identifier <> '' then
        Identifier := Identifier + '.%'
      else
        Identifier := AddMetadataQuotes('%', False);
    end
    else
    begin
      if Identifier = '' then
        Identifier := NewElement
      else
        Identifier := Identifier + '.' + NewElement;
    end;
  end;

var
  TableType:        string;
  Pattern:          string;
  ACatalogName:     string;
  ASchemaName:      string;
  Identifier:       string;
  DbxMetadataCommand:      string;
  Connection:       TSQLConnection;
  Dbx4Metadata:     Boolean;
var
  MetaData: TDBXDatabaseMetaData;
begin
  if FSQLConnection = nil then
    DatabaseError(sConnectionNameMissing);
  Pattern := FSchemaInfo.Pattern;
  Assert(FDBXReader = nil);
  Assert(FDBXCommand = nil);
  Connection := GetInternalConnection;
  MetaData := Connection.FDBXConnection.DatabaseMetaData;
  if Assigned(MetaData) and (MetaData.MetaDataVersion = DBXVersion40) then
    Dbx4Metadata := true
  else
    Dbx4Metadata := false;

  try
    FDBXCommand := Connection.DBXConnection.CreateCommand;
    FDBXCommand.CommandType := TDBXCommandTypes.DbxMetaData;

    Identifier := '';
    DbxMetadataCommand := '';

    case FSchemaInfo.FType of
      stTables:
        begin
          SetSchemaOption(ACatalogName, ASchemaName);

          AppendToIdentifier(Identifier, AddMetadataQuotes(ACatalogName, false));
          AppendToIdentifier(Identifier, AddMetadataQuotes(ASchemaName, false));
          AppendToIdentifier(Identifier, AddMetadataQuotes(Pattern, false));
          TableType           := GetTableScope(GetInternalConnection.FTableScope);
          DbxMetadataCommand         := MakeDbxMetadataCommand( TDBXMetaDataCommands.GetTables,
                                                  [Identifier,
                                                  TableType]);
        end;
      stSysTables:
        begin
          SetSchemaOption(ACatalogName, ASchemaName);
          AppendToIdentifier(Identifier, AddMetadataQuotes(ACatalogName, false));
          AppendToIdentifier(Identifier, AddMetadataQuotes(ASchemaName, false));
          AppendToIdentifier(Identifier, AddMetadataQuotes(Pattern, false));
          DbxMetadataCommand         := MakeDbxMetadataCommand( TDBXMetaDataCommands.GetTables,
                                                  [Identifier,
                                                  TDBXMetadataTableTypes.SystemTable]);
        end;
      stColumns:
        begin
          SetSchemaOption(ACatalogName, ASchemaName);
          AppendToIdentifier(Identifier, AddMetadataQuotes(ACatalogName, false));
          AppendToIdentifier(Identifier, AddMetadataQuotes(ASchemaName, false));
          AppendToIdentifier(Identifier, AddMetadataQuotes(FSchemaInfo.ObjectName, false));
          if not Dbx4Metadata then
            AppendToIdentifier(Identifier, AddMetadataQuotes(Pattern, false));
          DbxMetadataCommand         := MakeDbxMetadataCommand( TDBXMetaDataCommands.GetColumns,
                                                  [Identifier
                                                  ]);
        end;
      stProcedures:
        begin
          SetSchemaOption(ACatalogName, ASchemaName);
          AppendToIdentifier(Identifier, AddMetadataQuotes(ACatalogName, false));
          AppendToIdentifier(Identifier, AddMetadataQuotes(ASchemaName, false));
          if Dbx4Metadata and (FSchemaInfo.PackageName <> '') then
            AppendToIdentifier(Identifier, AddMetadataQuotes(FSchemaInfo.PackageName, false));
          AppendToIdentifier(Identifier, AddMetadataQuotes(Pattern, true));
          if Dbx4Metadata and (FSchemaInfo.PackageName <> '') then
            DbxMetadataCommand       := MakeDbxMetadataCommand( TDBXMetaDataCommands.GetPackageProcedures,
                                                  [Identifier])
          else
          begin
            DbxMetadataCommand       := MakeDbxMetadataCommand( TDBXMetaDataCommands.GetProcedures,
                                                  [Identifier,
                                                  FSchemaInfo.PackageName]);
          end
        end;
      stPackages:
        begin
          DbxMetadataCommand         := MakeDbxMetadataCommand(TDBXMetaDataCommands.GetPackages, []);
        end;

      stUserNames:
        begin
          if Dbx4Metadata then
            DbxMetadataCommand       := MakeDbxMetadataCommand(TDBXMetaDataCommands.GetSchemas, ['%'])
          else
            DbxMetadataCommand       := MakeDbxMetadataCommand(TDBXMetaDataCommands.GetUsers, []);
        end;

      stProcedureParams:
        begin
          SetSchemaOption(ACatalogName, ASchemaName);
          AppendToIdentifier(Identifier, AddMetadataQuotes(ACatalogName, false));
          AppendToIdentifier(Identifier, AddMetadataQuotes(ASchemaName, false));
          if Dbx4Metadata and (FSchemaInfo.PackageName <> '') then
            AppendToIdentifier(Identifier, AddMetadataQuotes(FSchemaInfo.PackageName, false));
          AppendToIdentifier(Identifier, AddMetadataQuotes(FSchemaInfo.ObjectName, true));
          if Dbx4Metadata then
            if FSchemaInfo.PackageName <> '' then
              DbxMetadataCommand := MakeDbxMetadataCommand( TDBXMetaDataCommands.GetPackageProcedureParameters,
                                                  [Identifier])
            else
              DbxMetadataCommand := MakeDbxMetadataCommand( TDBXMetaDataCommands.GetProcedureParameters,
                                                  [Identifier])
          else
          begin
            AppendToIdentifier(Identifier, AddMetadataQuotes(Pattern, false));
            DbxMetadataCommand := MakeDbxMetadataCommand( TDBXMetaDataCommands.GetProcedureParameters,
                                                  [Identifier,
                                                   FSchemaInfo.PackageName
                                                  ]);
          end
        end;
      stIndexes:
        begin
          SetSchemaOption(ACatalogName, ASchemaName);
          AppendToIdentifier(Identifier, AddMetadataQuotes(ACatalogName, false));
          AppendToIdentifier(Identifier, AddMetadataQuotes(ASchemaName, false));
          AppendToIdentifier(Identifier, AddMetadataQuotes(FSchemaInfo.ObjectName, false));
//          if Dbx4Metadata then
//            AppendToIdentifier(Identifier, '%');

          DbxMetadataCommand         := MakeDbxMetadataCommand( TDBXMetaDataCommands.GetIndexes,
                                                  [Identifier,
                                                   FSchemaInfo.PackageName
                                                  ]);
        end;
    end;
    FDBXCommand.Text  := DbxMetadataCommand;
    FDBXReader    := FDBXCommand.ExecuteQuery;
  finally

  end;
end;

{ ProviderSupport }

procedure TCustomSQLDataSet.PSEndTransaction(Commit: Boolean);
begin
{$IFNDEF NEXTGEN}
  FSQLConnection.EndAndFreeTransaction(Commit);
{$ELSE}
  if FSQLConnection.InTransaction then
  begin
    if Commit then
      FSQLConnection.Commit
    else
      FSQLConnection.Rollback;
  end;
{$ENDIF !NEXTGEN}
end;

procedure TCustomSQLDataSet.PSExecute;
begin
   ExecSQL;
end;

function TCustomSQLDataSet.PSExecuteStatement(const ASQL: string; AParams: TParams): Integer;
begin
  Result := FSQLConnection.Execute(ASQL, AParams);
end;

function TCustomSQLDataSet.PSExecuteStatement(const ASQL: string; AParams: TParams;
  var ResultSet: TDataSet): Integer;
begin
  Result := FSQLConnection.Execute(ASQL, AParams, ResultSet);
end;

{$IFNDEF NEXTGEN}
function TCustomSQLDataSet.PSExecuteStatement(const ASQL: string; AParams: TParams;
      ResultSet: Pointer = nil): Integer;
begin
  if ResultSet = nil then
    Result := FSQLConnection.Execute(ASQL, AParams)
  else
    Result := FSQLConnection.Execute(ASQL, AParams, ResultSet);
end;
{$ENDIF !NEXTGEN}

procedure TCustomSQLDataSet.PSGetAttributes(List: TPacketAttributeList);
var
{$IFDEF NEXTGEN}
  Attr: TPacketAttribute;
{$ELSE}
  Attr: PPacketAttribute;
{$ENDIF NEXTGEN}
begin
  inherited PSGetAttributes(List);
{$IFNDEF NEXTGEN}
  New(Attr);
{$ENDIF !NEXTGEN}
  Attr.Name := SLocaleCode;
  if FSQLConnection <> nil then
    Attr.Value := Integer(FSQLConnection.LocaleCode)
  else
    Attr.Value := Integer(0);
  Attr.IncludeInDelta := False;
  List.Add(Attr);
end;

function TCustomSQLDataSet.PSGetIndexDefs(IndexTypes: TIndexOptions): TIndexDefs;
begin
  if (not FIndexDefsLoaded)
      and (CommandType <> ctStoredProc)
      and (CommandType <> ctServerMethod)
      and (SchemaInfo.FType = stNoSchema) then
  begin
    AddIndexDefs(Self);
  end;
  Result := GetIndexDefs(IndexDefs, IndexTypes);
end;

function TCustomSQLDataSet.PSGetDefaultOrder: TIndexDef;

  function FieldsInQuery(IdxFields: string): Boolean;
  var
    I:  Integer;
    IdxFlds, Flds: TStrings;
    FldNames: string;
  begin
    Result := True;
    IdxFlds := TStringList.Create;
    try
      IdxFlds.CommaText := IdxFields;
      Flds := TStringList.Create;
      try
        Fields.GetFieldNames(Flds);
        FldNames := Flds.CommaText;
        for I := 0 to IdxFlds.Count -1 do
        begin
          if FldNames.IndexOf(IdxFlds[I]) = -1 then
          begin
            Result := False;
            Exit;
          end;
        end;
      finally
        Flds.Free;
      end;
    finally
      IdxFlds.Free;
    end;
  end;

var
  I: Integer;
begin
  Result := inherited PSGetDefaultOrder;
  if not Assigned(Result) then
    Result := GetIndexForOrderBy(GetQueryFromType, Self);
  if (not Assigned(Result))
      and (CommandType <> ctStoredProc)
      and (CommandType <> ctServerMethod)
      and (SchemaInfo.FType = stNoSchema) then
  begin
    if not FIndexDefsLoaded then
      AddIndexDefs(Self);
    for I := 0 to IndexDefs.Count - 1 do
    begin
      if (ixPrimary in TIndexDef(IndexDefs[I]).Options) and
         FieldsInQuery(TIndexDef(IndexDefs[I]).Fields) then
      begin
        Result := TIndexDef.Create(nil);
        Result.Assign(IndexDefs[I]);
        Break;
      end;
    end;
  end;
end;

function TCustomSQLDataSet.PSGetKeyFields: string;
var
  HoldPos, I: Integer;
  IndexFound: Boolean;
begin
  if (CommandType = ctStoredProc) or (CommandType = ctServerMethod) or
     (SchemaInfo.FType <> stNoSchema) then
    Exit;
  Result := inherited PSGetKeyFields;
  IndexFound := False;
  if (Result = '') and (SchemaInfo.FType = stNoSchema) then
  begin
    if not FIndexDefsLoaded then
      AddIndexDefs(Self);
    for I := 0 to IndexDefs.Count - 1 do
      if (ixUnique in IndexDefs[I].Options) or
         (ixPrimary in IndexDefs[I].Options) then
      begin
        Result := IndexDefs[I].Fields;
        IndexFound := (FieldCount = 0);
        if not IndexFound then
        begin
          HoldPos := 1;
          while HoldPos <= Result.Length do
          begin
            IndexFound := FindField(ExtractFieldName(Result, HoldPos)) <> nil;
            if not IndexFound then Break;
          end;
        end;
        if IndexFound then Break;
      end;
    if not IndexFound then
      Result := '';
  end;
end;

function TCustomSQLDataSet.PSGetParams: TParams;
begin
  Result := Params;
end;

function TCustomSQLDataSet.GetQuoteChar: string;
begin
  Result := PSGetQuoteChar;
end;

function TCustomSQLDataSet.PSGetQuoteChar: string;
begin
  Result := '';
  if (Assigned(FSqlConnection) and (FSQLConnection.QuoteChar <> '')) then
    Result := FSQLConnection.QuoteChar;
end;

procedure TCustomSQLDataSet.PSReset;
begin
  inherited PSReset;
  if Active and (not BOF) then
    First
  else if Active then
  begin
    Close;
    Open;
  end;
end;

function TCustomSQLDataSet.PSGetTableName: string;
begin
   if CommandType = ctTable then
     Result := CommandText
   else
     Result := GetTableNameFromSQLEx(CommandText, GetIdOption(FSQLConnection));
end;

function TCustomSQLDataSet.PSGetUpdateException(E: Exception; Prev: EUpdateError): EUpdateError;
begin
  if not Assigned(E) then
    E := EDatabaseError.Create(SErrorMappingError);
  Result := inherited PSGetUpdateException(E, Prev);
end;

function TCustomSQLDataSet.PSInTransaction: Boolean;
begin
  Result := (FSQLConnection <> nil) and (FSQLConnection.InTransaction);
end;

function TCustomSQLDataSet.PSIsSQLBased: Boolean;
var
  IsSQLBased: string;
begin
  Result := True;
  if Assigned(FSQLConnection) then
  begin
    if Assigned(FSQLConnection.FDBXConnection) then
    begin
      IsSQLBased := FSQLConnection.FDBXConnection.GetVendorProperty('IsSQLBased'); { Do not localize. }
      if (IsSqlBased <> '') and (string.Compare(IsSQLBased, 'false', True) = 0) then
        Result := False;
    end
    else
      Result := True;
  end;
end;

function TCustomSQLDataSet.PSIsSQLSupported: Boolean;
begin
  Result := True;
end;

procedure TCustomSQLDataSet.PSSetParams(AParams: TParams);
begin
  if (AParams.Count <> 0) and (AParams <> Params) then
  begin
    Params.Assign(AParams);
    if Prepared and (FNativeCommand.IndexOf(SParam) = -1) then
      SetPrepared(False);
  end;
  Close;
end;

procedure TCustomSQLDataSet.PSSetCommandText(const ACommandText: string);
begin
  if ACommandText <> '' then
    CommandText := ACommandText;
end;

procedure TCustomSQLDataSet.PSStartTransaction;
begin
  FSQLConnection.BeginTransaction;
end;

function TCustomSQLDataSet.PSUpdateRecord(UpdateKind: TUpdateKind; Delta: TDataSet): Boolean;
begin
  { OnUpdateRecord is not supported }
  Result := False;
end;

function TCustomSQLDataSet.QuoteIdentifier(Identifier: string;
  StoredProc: Boolean): string;
var
  Catalog, Name, QuotePrefix, QuoteSuffix, Schema: string;
  MetaData: TDBXDatabaseMetaData;
begin
  if (Assigned(FSQLConnection) and Assigned(FSQLConnection.FDBXConnection)) then
  begin
    MetaData := FSQLConnection.MetaData;
    if Assigned(MetaData) and (MetaData.MetaDataVersion = DBXVersion40) then
    begin
      QuotePrefix := MetaData.QuotePrefix;
      QuoteSuffix := MetaData.QuoteSuffix;
    end
    else
    begin
      // backward compatibility
      QuotePrefix := FSQLConnection.FQuoteChar;
      QuoteSuffix := FSQLConnection.FQuoteChar;
    end
  end
  else
  begin
    // backward compatibility
    QuotePrefix := FSQLConnection.FQuoteChar;
    QuoteSuffix := FSQLConnection.FQuoteChar;
  end;

  if QuotePrefix.Length = 0 then
    Result := Identifier
  else
  begin
    ParseIdentifier(Identifier, Catalog, Schema, Name);
    if Catalog.Length > 0 then
    begin
      Result :=     QuotePrefix + Catalog   + QuoteSuffix + '.'
                  + QuotePrefix + Schema    + QuoteSuffix + '.'
                  + QuotePrefix + Name      + QuoteSuffix;
    end
    else if Schema.Length > 0 then
    begin
      Result :=     QuotePrefix + Schema    + QuoteSuffix + '.'
                  + QuotePrefix + Name      + QuoteSuffix;

    end
    else
    begin
      Result :=     QuotePrefix + Name      + QuoteSuffix;
    end;
  end;
end;

function TCustomSQLDataSet.PSGetCommandText: string;
begin
  Result := CommandText;
end;

function TCustomSQLDataSet.PSGetCommandType: TPSCommandType;
begin
  Result := CommandType;
end;

function TCustomSQLDataSet.LocateRecord(const KeyFields: string; const KeyValues: Variant;
  Options: TLocateOptions; SyncCursor: Boolean): Boolean;

  function SameValue(V1, V2: Variant; IsString, CaseInsensitive,
           PartialLength: Boolean): Boolean;
  var
    V: Variant;
  begin
    if not IsString then
      Result := VarCompareValue(V1, V2) = vrEqual
    else
    begin
      if PartialLength then
        V := string(V1).Substring(0, string(V2).Length)
      else
        V := V1;
      if CaseInsensitive then
        Result := string(V).ToLower = string(V2).ToLower
      else
        Result := V = V2;
    end;
  end;

  function CheckValues(AFields: TStrings; Values: Variant;
           CaseInsensitive, PartialLength: Boolean): Boolean;
  var
    J: Integer;
    Field: TField;
  begin
    Result := True;
    for J := 0 to AFields.Count -1 do
    begin
      Field := FieldByName(AFields[J]);
      if not SameValue(Field.Value, Values[J],
        Field.DataType in [ftString, ftFixedChar, ftWideString, ftFixedWideChar], CaseInsensitive, PartialLength) then
      begin
        Result := False;
        break;
      end;
    end;
  end;

var
  I: Integer;
  SaveFields, AFields: TStrings;
  PartialLength, CaseInsensitive: Boolean;
  Values, StartValues: Variant;
  bFound: Boolean;
begin
  CheckBrowseMode;
  CursorPosChanged;
  AFields := TStringList.Create;
  SaveFields := TStringList.Create;
  try
    AFields.CommaText := StringReplace(KeyFields, ';', ',', [rfReplaceAll]);
    PartialLength := loPartialKey in Options;
    CaseInsensitive := loCaseInsensitive in Options;
    if VarIsArray(KeyValues) then
      Values := KeyValues
    else
      Values := VarArrayOf([KeyValues]);
    { save current record in case we cannot locate KeyValues }
    StartValues := VarArrayCreate([0, FieldCount], varVariant);
    for I := 0 to FieldCount -1 do
    begin
      StartValues[I] := Fields[I].Value;
      SaveFields.Add(Fields[I].FieldName);
    end;
    First;
    while not EOF do
    begin
      if CheckValues(AFields, Values, CaseInsensitive, PartialLength) then
        break;
      Next;
    end;
    { if not found, reset cursor to starting position }
    bFound := not EOF;
    if not bFound then
    begin
      First;
      while not EOF do
      begin
        if CheckValues(SaveFields, StartValues, False, False) then
          break;
        Next;
      end;
    end;
    Result := bFound;
  finally
    AFields.Free;
    SaveFields.Free;
  end;
end;

function TCustomSQLDataSet.Locate(const KeyFields: string; const KeyValues: Variant;
  Options: TLocateOptions): Boolean;
begin
  DoBeforeScroll;
  Result := LocateRecord(KeyFields, KeyValues, Options, True);
  if Result then
  begin
    Resync([rmExact, rmCenter]);
    DoAfterScroll;
  end;
end;

function TCustomSQLDataSet.Lookup(const KeyFields: string; const KeyValues: Variant;
  const ResultFields: string): Variant;
begin
  Result := Null;
  if LocateRecord(KeyFields, KeyValues, [], False) then
  begin
    SetTempState(dsCalcFields);
    try
      CalculateFields(0);
      Result := FieldValues[ResultFields];
    finally
      RestoreState(dsBrowse);
    end;
  end;
end;

procedure TCustomSQLDataSet.SetSchemaName(const Value: string);
begin
  if FSchemaName <> Value then
  begin
    PropertyChanged;
    FSchemaName := Value;
  end;
end;

procedure TCustomSQLDataSet.ParseIdentifier(Identifier: string; var Catalog, Schema,
  Name: string);
var
  Start: NativeInt;
  IdOption: IDENTIFIEROption;
begin
  Catalog := '';
  Schema := '';
  Start := 1;
  IdOption := idMixCase;  // This is used for TSQLStoredProc and TSQLTable
  Platform_NextSQLToken(Identifier, Start, Name, stTableName, IdOption);
  if (Start < Identifier.Length) and (Identifier.Chars[Start - 1] = '.') then
  begin
    Schema := Name;
    Platform_NextSQLToken(Identifier, Start, Name, stTableName, IdOption);
  end;
  if (Start < Identifier.Length) and (Identifier.Chars[Start - 1] = '.') then
  begin
    Catalog := Schema;
    Schema := Name;
    Platform_NextSQLToken(Identifier, Start, Name, stTableName, IdOption);
  end;
end;

procedure TCustomSQLDataSet.SetSchemaOption(var ACatalogName, ASchemaName: string);
var
  ObjectName: string;
begin
  ACatalogName  := '';
  ASchemaName   := '';
  ObjectName := FSchemaInfo.ObjectName;
  if ObjectName <> '' then
    ParseIdentifier(ObjectName, ACatalogName, ASchemaName, FSchemaInfo.ObjectName);

  if ACatalogName.Length = 0 then
    ACatalogName := GetInternalConnection.FParams.Values[DATABASENAME_KEY];
  (* by default, ASchemaName has been retrieved from getOption(eMetaSchemaName).
     if this is NOT set, then try TCustomDataSet.SchemaName;
     if this is NOT set, then try DefaultSchemaName;
     if this is NOT set, then try the UserName used to login;
     only if this is NOT set, get UserName from Parameter StringList *)
  if ASchemaName.Length = 0 then
    ASchemaName := SchemaName;
  if ASchemaName.Length = 0 then
  begin
    ASchemaName := GetInternalConnection.DefaultSchema;
    if (ASchemaName.Length <= 0) then
    begin
      // This is mostly for Interbase's benefit.
      //
      ASchemaName := GetInternalConnection.GetLoginUsername;
      if (ASchemaName.Length <= 0) then
        ASchemaName := GetInternalConnection.FParams.Values[TDBXPropertyNames.UserName];
    end;
  end;
end;
{ TSQLDataSet }

constructor TSQLDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCommandType := ctQuery;
  FGetMetadata := True;
end;

function TSQLDataSet.ExecSQL(ExecDirect: Boolean = False): Integer;
begin
  Result := inherited ExecSQL(ExecDirect);
end;

{ TSQLQuery }

constructor TSQLQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCommandType := ctQuery;
  FSQL := TStringList.Create;
  FGetMetaData := False;
  TStringList(SQL).OnChange := QueryChanged;
end;

destructor TSQLQuery.Destroy;
begin
  FSQL.Free;
  inherited Destroy;
end;

function TSQLQuery.ExecSQL(ExecDirect: Boolean = False): Integer;
begin
  Result := inherited ExecSQL(ExecDirect);
end;

procedure TSQLQuery.PrepareStatement;
var
  Start: NativeInt;
  SQLText, Value: string;
  CurSection: TSqlToken;
begin
  if FCommandText = '' then
    SetSQL(SQL);
  if CommandText.Length = 0 then
    DatabaseError(SEmptySQLStatement, Self);
  CurSection := stUnknown;
  Start := 1;
  CurSection := Platform_NextSQLToken(CommandText, Start, Value, CurSection, GetIdOption(FSQLConnection));
  if CurSection = stSelect then
    Inc(FSQLConnection.FSelectStatements);
  CheckStatement;
  SQLText := FNativeCommand;
  FDBXCommand.CommandType := FDbxCommandType;
  FDBXCommand.Text := SQLText;
  FDBXCommand.Parameters.SetCount(Params.Count);
  FDBXCommand.Prepare;
end;

procedure TSQLQuery.QueryChanged(Sender: TObject);
begin
  if not (csReading in ComponentState) then
  begin
    Close;
    SetPrepared(False);
    if ParamCheck or (csDesigning in ComponentState) then
    begin
      FCommandText := SQL.Text;
      FText := FCommandText;
      SetParamsFromSQL(nil, False);
    end
    else
      FText := SQL.Text;
    DataEvent(dePropertyChange, 0);
  end
  else
    FText := FParams.ParseSQL(SQL.Text, False);
  SetFCommandText(FText);
end;

procedure TSQLQuery.SetSQL(Value: TStrings);
begin
  if SQL.Text <> Value.Text then
  begin
    Close;
    SQL.BeginUpdate;
    try
      SQL.Assign(Value);
    finally
      SQL.EndUpdate;
    end;
  end;
end;

{ TSQLStoredProc }

constructor TSQLStoredProc.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCommandType := ctStoredProc;
  FGetMetadata := True;
end;

function TSQLStoredProc.ExecProc: Integer;
begin
  Result := ExecSQL;
end;

procedure TSQLStoredProc.PrepareStatement;
var
  SQLText: string;
begin
  if FCommandText = '' then
    SetStoredProcName(FStoredProcName);
  if CommandText.Length = 0 then
    DatabaseError(SEmptySQLStatement, Self);
  CheckStatement;
  FDBXCommand.CommandType := TDBXCommandTypes.DbxStoredProcedure;

  if FPackageName <> '' then
    SQLText := FPackageName + '.' + FNativeCommand
  else
    SQLText := FNativeCommand;
  if FSchemaName <> '' then
    SQLText := QuoteIdentifier(FSchemaName + '.' + SQLText, true)
  else
    SQLText := QuoteIdentifier(SQLText, true);

  FDBXCommand.Text := SQLText;
  FDBXCommand.Parameters.SetCount(Params.Count);
  FDBXCommand.Prepare;
end;

procedure TSQLStoredProc.SetStoredProcName(Value: string);
begin
  //if FStoredProcName <> Value then
  //begin
    FStoredProcName := Value;
    SetCommandText(Value);
    if Assigned(FProcParams) then  // free output params if any
      FreeProcParams(FProcParams);
  //end;
end;

procedure TSQLStoredProc.SetPackageName(Value: string);
begin
  if FPackageName <> Value then
  begin
    FPackageName := Value;
    FSchemaInfo.PackageName := Value;
    if Assigned(FProcParams) then
      FreeProcParams(FProcParams);
    FStoredProcName := '';
    SetCommandText('');
  end;
end;

function TSQLStoredProc.NextRecordSet: TCustomSQLDataSet;
begin
  FGetNextRecordSet := True;
  SetState(dsInactive);
  CloseCursor;
  if Assigned(FieldDefs) then
    FieldDefs.Updated := False;
  try
    Active := True;
  finally
    FGetNextRecordSet := False;
  end;
  if Assigned(FDBXReader) then
    Result := TCustomSQLDataSet(Self)
  else
    Result := Nil;
end;

{ TSQLTable }

constructor TSQLTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCommandType := ctTable;
  FGetMetadata := True;
  FIndexFieldCount := -1;
  FMasterLink := TMasterDataLink.Create(Self);
  FIndexFields := TList<TField>.Create;
end;

destructor TSQLTable.Destroy;
begin
  FreeAndNil(FMasterLink);
  FreeAndNil(FIndexFields);
  inherited Destroy;
end;

procedure TSQLTable.DeleteRecords;
begin
  SQLConnection.ExecuteDirect('delete from ' + TableName);   { do not localize }
end;

function TSQLTable.GetIndexField(Index: Integer): TField;
begin
  if IndexName = '' then Result := nil
  else
  begin
    if FIndexFieldCount = -1 then
      RefreshIndexFields;
    Result := FIndexFields[Index];
  end;
end;

function TSQLTable.GetIndexFieldCount: Integer;
begin
  if IndexName = '' then Result := 0
  else if FIndexFieldCount >= 0 then Result := FIndexFieldCount
  else Result := RefreshIndexFields;
end;

procedure TSQLTable.GetIndexNames(List: TStrings);
begin
  FSQLConnection.GetIndexNames(FTableName,List);
end;

procedure TSQLTable.OpenCursor(InfoQuery: Boolean);
begin
  inherited OpenCursor(InfoQuery);
  if not FIsDetail and not FIndexDefsLoaded then
    AddIndexDefs(Self);
end;

procedure TSQLTable.AddParamsToQuery;
var
  I: Integer;
  Value: string;
begin
  if NativeCommand.IndexOf('?') = -1 then
  begin
    for I := 0 to Params.Count -1 do
    begin
      if Params[I].IsNull then
        Value := 'is NULL'
      else
        Value := '= ?';
      if I = 0 then
        NativeCommand := string.Format('%s%s(%s %s)', [NativeCommand, SWhere, Params[I].Name, Value])
      else
        NativeCommand := string.Format('%s%s(%s %s)', [NativeCommand, SAnd, Params[I].Name, Value]);
    end;
  end;
end;

procedure TSQLTable.SetDataSource(Value: TDataSource);
begin
  inherited SetDataSource(Value);
end;

function TSQLTable.GetQueryFromType: string;
begin
  if FNativeCommand <> '' then
    Result := FNativeCommand
  else
    Result := inherited GetQueryFromType;
end;

procedure TSQLTable.PrepareStatement;

  function GetFieldsForIndexName(IndexName: string): string;
  var
    DataSet:      TCustomSQLDataSet;
    IdxName:      string;
    IndexColumns: TDBXIndexColumns;
    MetaData:     TDBXDatabaseMetaData;
  begin
    DataSet := FSQLConnection.OpenSchemaTable(stIndexes, TableName,'','','');
    try
      MetaData := FSQLConnection.FDBXConnection.DatabaseMetaData;
      if Assigned(MetaData) and (MetaData.MetaDataVersion = DBXVersion40) then
      begin
        while not DataSet.EOF do
        begin
          IdxName := DataSet.FieldByName(GetIndexFieldName(FSQLConnection)).Value;
          if IdxName = IndexName then
          begin
            IndexColumns := TDBXIndexColumns.Create;
            try
              IndexColumns.FGetIndexesText := DataSet.FDBXCommand.Text;
              IndexColumns.FSqlConnection := DataSet.FSQLConnection;
              IndexColumns.Open;
              Result := IndexColumns.GetFieldNames(IdxName);
              exit;
            finally
              IndexColumns.Free;
            end;
          end;
          DataSet.Next;
        end;
      end else
      begin
        while not DataSet.EOF do
        begin
          IdxName := DataSet.FieldByName(GetIndexFieldName(FSQLConnection)).Value;
          if IdxName = IndexName then
          begin
            if Result = '' then
              Result := DataSet.FieldByName(GetColumnFieldName(FSQLConnection)).Value
            else
              Result := Result + ';' + DataSet.FieldByName(GetColumnFieldName(FSQLConnection)).Value;
          end;
          DataSet.Next;
        end;
      end;
    finally
      FSQLConnection.FreeSchemaTable(DataSet);
    end;
  end;

  function GetIndexFieldNames(FieldNames, IndexName: string): string;
  begin
    if (FieldNames = '') and (IndexName = '') then
      Result := ''
    else if FieldNames <> '' then
      Result := FieldNames
    else
      Result := GetFieldsForIndexName(IndexName);
  end;

var
  IdxFieldNames, LDetailWhere, LName1, LName2, SQLText, STableName, TempString1, TempString2: string;
  LIndex, Pos1, Pos2: Integer;
begin  // first, convert TableName into valid Query.
  if FTableName.Length = 0 then
    DatabaseError(SEmptySQLStatement, Self);
  if FNativeCommand = '' then  // otherwise, already prepared
  begin
    if (FDataLink.DataSource <> nil) and (MasterFields <> '') then
    begin
      FIsDetail := True;
      Pos1 := 1;
      Pos2 := 1;
      LIndex := 1;
      TempString1 := MasterFields;
      TempString2 := IndexFieldNames;
      while Pos1 <= TempString1.Length do
        begin
          LName1 := ExtractFieldName(TempString1, Pos1);
          LName2 := ExtractFieldName(TempString2, Pos2);
          if LName1 = '' then Break;
          if LIndex = 1 then
            LDetailWhere := SWhere
          else
            LDetailWhere := LDetailWhere + SAnd;
          if LName2 = '' then
            LDetailWhere := LDetailWhere + LName1 + ' = :' + LName1
          else
            LDetailWhere := LDetailWhere + LName2 + ' = :' + LName1;
          Inc(LIndex);
        end;
      FCommandType := ctQuery;
      SetCommandText(SSelectStarFrom + QuoteIdentifier(FTableName, false)
                      + LDetailWhere);
    end
    else
    begin
      FIsDetail := False;
      IdxFieldNames := GetIndexFieldNames(IndexFieldNames, IndexName);
      if Self.FSchemaName <> '' then
        STableName := QuoteIdentifier(FSchemaName + '.' + FTableName, false)
      else
        STableName := QuoteIdentifier(FTableName, false);
      if IdxFieldNames = '' then
        FCommandText := SSelectStarFrom + STableName
      else
        FCommandText := SSelectStarFrom + STableName + SOrderBy + IdxFieldNames.Replace(';', ',');
    end;
  end
  else if Params.Count > 0 then
    AddParamsToQuery;

  Inc(FSQLConnection.FSelectStatements);
  CheckStatement;
  SQLText := FNativeCommand;
  FDBXCommand.CommandType := FDbxCommandType;
  FDBXCommand.Text := SQLText;
  FDBXCommand.Parameters.SetCount(Params.Count);
  FDBXCommand.Prepare;
  FCommandType := ctTable;
  FCommandText := FTableName;
end;

function TSQLTable.RefreshIndexFields: Integer;
var
  I, Pos: Integer;
  Temp: string;
  FField: TField;
begin
  Result := 0;
  if not FIndexDefsLoaded then
    AddIndexDefs(Self);
  FIndexFields.Clear;
  for I := 0 to IndexDefs.Count - 1 do
  begin
    if string.Compare(IndexDefs[I].Name, IndexName, True) = 0 then
    begin
      Temp := IndexDefs[I].Fields;
      Pos := 1;
      while Pos <= Temp.Length do
      begin
        FField := FindField(ExtractFieldName(Temp, Pos));
        if FField = nil then
          Break
        else
          FIndexFields.Add(FField);
        Inc(Result);
      end;
      Break;
    end;
  end;
end;

function TSQLTable.GetMasterFields: string;
begin
  Result := FMasterLink.FieldNames;
end;

procedure TSQLTable.SetMasterFields(Value: string);
begin
  FMasterLink.FieldNames := Value;
  if not (csLoading in ComponentState) then
  begin
    Close;
    FreeCommand;
    FNativeCommand := '';
    FParams.clear;
  end;
end;

procedure TSQLTable.SetTableName(Value: string);
begin
  if FTableName <> Value then
  begin
    FNativeCommand := '';
    FTableName := Value;
    SetCommandText(Value);
  end;
end;

procedure TSQLTable.SetIndexFieldNames(Value: string);
begin
  if FIndexFieldNames <> Value then
  begin
    if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
      if (TableName = '') and (Value <> '') then DatabaseError(SNoTableName,Self);
    FIndexFieldNames := Value;
    if FIndexFieldNames <> '' then
      SetIndexName('');
    FNativeCommand := '';
    SetPrepared(False);
  end;
end;

procedure TSQLTable.SetIndexField(Index: Integer; Value: TField);
begin
  GetIndexField(Index).Assign(Value);
end;

procedure TSQLTable.SetIndexName(Value: string);
begin
  if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    if (TableName = '') and (Value <> '') then DatabaseError(SNoTableName,Self);
  if FIndexName <> Value then
  begin
    FIndexName := Value;
    FNativeCommand := '';
    if Assigned(FSQLConnection) and (Value <> '') then
    begin
      SetIndexFieldNames('');  // clear out IndexFieldNames
      if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
        AddIndexDefs(Self, Value);
    end;
    SetPrepared(False);
  end;
  FIndexFieldCount := -1;
end;

{ TDBXIndexColumns }

constructor TDBXIndexColumns.Create;
begin
  inherited Create;
  FColumns := TObjectList<TDBXIndexColumn>.Create(True);
end;

destructor TDBXIndexColumns.Destroy;
begin
  FreeAndNil(FColumns);
  inherited;
end;

function TDBXIndexColumns.GetFieldNames(IndexName: string): string;
var
  IndexColumn:  TDBXIndexColumn;
  Index:        Integer;
begin
  Result := '';
  for Index := 0 to FColumns.Count - 1 do
  begin
    IndexColumn := FColumns[Index];
    if IndexColumn.FIndexName = IndexName then
    begin
      if Result = '' then
        Result := IndexColumn.FColumnName
      else
        Result := Result + ';' + IndexColumn.FColumnName;
    end;
  end;
end;

function TDBXIndexColumns.HasAllFieldNames(IndexName: string;
  DataSet: TCustomSQLDataSet; var FieldNames: string;
  var FirstColumnAscending: Boolean): Boolean;
var
  IndexColumn:  TDBXIndexColumn;
  StartIndex:   Integer;
  LastIndex:    Integer;
  Index:        Integer;
begin
  StartIndex    := -1;
  LastIndex     := -1;
  for Index := 0 to FColumns.Count - 1 do
  begin
    IndexColumn := FColumns[Index];
    if IndexColumn.FIndexName = IndexName then
    begin
      if DataSet.FindField(IndexColumn.FColumnName) = nil then
      begin
        Result := false;
        exit;
      end;
      if StartIndex = -1 then
        StartIndex := Index;
      LastIndex := Index;
    end;
  end;
  FieldNames := '';
  FirstColumnAscending := true;

  if StartIndex < 0 then
    Result := false
  else
  begin
    for Index := StartIndex to LastIndex do
    begin
      IndexColumn := FColumns[Index];
      if Index = StartIndex then
      begin
        FirstColumnAscending := IndexColumn.FAscending;
        FieldNames := IndexColumn.FColumnName;
      end else
        FieldNames := FieldNames + ';' + IndexColumn.FColumnName;
    end;
    Result := True;
  end;
end;

procedure TDBXIndexColumns.Open;
var
  Reader: TDBXReader;
  Column: TDBXIndexColumn;
  Count, StartIndex: Integer;
  Command: TDBXCommand;
  Connection: TSQLConnection;
begin
  Connection := FSqlConnection.GetConnectionForStatement;
  Command := Connection.FDBXConnection.CreateCommand;
  try
    StartIndex  := TDBXMetaDataCommands.GetIndexes.Length;
    Count       := FGetIndexesText.Length - StartIndex;
    Command.Text := TDBXMetaDataCommands.GetIndexColumns
                        + FGetIndexesText.Substring(StartIndex, Count);
    Command.CommandType := TDBXCommandTypes.DbxMetaData;
    Reader := Command.ExecuteQuery;
    try
      while Reader.Next do
      begin
        Column := TDBXIndexColumn.Create;
        Column.FIndexName   := Reader.Value['IndexName'].GetWideString; {Do not Localize}
        Column.FColumnName  := Reader.Value['ColumnName'].GetWideString; {Do not Localize}
        Column.FOrdinal     := Reader.Value['Ordinal'].GetInt32; {Do not Localize}
        Column.FAscending   := Reader.Value['IsAscending'].GetBoolean; {Do not Localize}
        FColumns.Add(Column);
      end;
    finally
      Reader.Free;
    end;
  finally
    Command.DisposeOf;
    if (Connection <> nil) and Connection.FIsCloned then
      Connection.Free;
  end;
end;

{ TSqlServerMethod }

function TSqlServerMethod.CheckDetail(const SQL: string): string;
begin
  Result := SQL;
end;

constructor TSqlServerMethod.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CommandType := ctServerMethod;
  FGetMetadata := False;
end;

procedure TSqlServerMethod.SetServerMethodName(Value: string);
begin
  if Value <> FServerMethodName then
  begin
    FServerMethodName := Value;
    SetCommandText(Value);
  end;
end;

procedure TSqlServerMethod.ExecuteMethod;
begin
  ExecSQL;
end;

{ TConnectionData }

constructor TConnectionData.Create(AConnection: TSQLConnection);
begin
  inherited Create;
  FConnection := AConnection;
end;

procedure TConnectionData.AddProperties(NewProperties: TStrings);
var
  Name, Value: string;
  I: Integer;
  DelegateDriverName: string;
  DelegateDriverProps: TDBXProperties;
begin
  for I := 0 to NewProperties.Count - 1 do
  begin
    Name := NewProperties.Names[I];
    Value := NewProperties.ValueFromIndex[I];
    if IsMyProperty(Name) then
    begin
      if SameText(Name, TDBXPropertyNames.DelegateConnection) then
      begin
        DelegateDriverName := NewProperties.Values[Value+'.'+TDBXPropertyNames.DriverName];
        if DelegateDriverName <> EmptyStr then
        begin
          TDBXConnectionFactory.Lock;
          try
            DelegateDriverProps := TDBXConnectionFactory.GetConnectionFactory.GetDriverProperties(DelegateDriverName);
          finally
            TDBXConnectionFactory.Unlock;
          end;
          if Assigned(DelegateDriverProps) then
          begin
            if not Assigned(FDelegateConnection) then
              FDelegateConnection := TConnectionData.Create(Self);
            DelegateConnection.Properties := DelegateDriverProps;
            DelegateConnection.DriverName := DelegateDriverName;
            DelegateConnection.ConnectionName := Value;
            TDBXConnectionFactory.Lock;
            try
              DelegateConnection.AddProperties(NewProperties);
            finally
              TDBXConnectionFactory.Unlock;
            end;
          end;
        end;
      end
      else
        Properties.Add(Name, Value);
    end;
  end;
end;

function TConnectionData.BeginUpdate: Boolean;
begin
  Result := not FChanging;
  FChanging := True;
end;

constructor TConnectionData.Create(AParentData: TConnectionData);
begin
  inherited Create;
  FParentData := AParentData;
end;

destructor TConnectionData.Destroy;
begin
  FreeAndNil(FProperties);
  FreeAndNil(FDelegateConnection);
  inherited;
end;

procedure TConnectionData.DoChange(Sender: TObject);
begin
  if not FChanging then
  begin
    FChanging := True;
    try
      if Assigned(FConnection) then
      begin
        if Assigned(FConnection.Params) then
        begin
          FConnection.Params.Clear;
          FConnection.Params.BeginUpdate;
          try
            GetFullParams(FConnection.Params);
          finally
            FConnection.Params.EndUpdate;
          end;
        end;
      end
      else
        FParentData.DoChange(Sender);
      FHasIsModified := False;
    finally
      FChanging := False;
    end;
  end;
end;

procedure TConnectionData.EndUpdate;
begin
  FChanging := False;
  FHasIsModified := False;
end;

function TConnectionData.GetConectionName: string;
begin
  if Assigned(FConnection) then
    Result := FConnection.ConnectionName
  else
    Result := FConnectionName;
end;

function TConnectionData.GetDriverName: string;
begin
  if Assigned(FConnection) then
    Result := FConnection.DriverName
  else
    Result := FDriverName;
end;

procedure TConnectionData.GetFullParams(Params: TStrings);
var
  I: Integer;
  Props: TDBXProperties;
begin
  if DriverName = EmptyStr then
    Exit;
  TDBXConnectionFactory.Lock;
  try
    Props := TDBXConnectionFactory.GetConnectionFactory.GetDriverProperties(DriverName);
    if Assigned(Props) then
    begin
      // driver properties that are not exposed at design time
      for I := 0 to Props.Count - 1 do
        if Properties.Properties.IndexOfName(Props.Properties.Names[I]) < 0 then
          Params.Add(GetPrefix + Props.Properties[I]);
    end;
  finally
    TDBXConnectionFactory.Unlock;
  end;

  if Assigned(Properties) then
  begin
    // add design time properties
    for I := 0 to Properties.Count - 1 do
      Params.Add(GetPrefix+Properties.Properties[I]);
  end;

  if Assigned(FParentData) then
    Params.Values[GetPrefix+TDBXPropertyNames.DriverName] := DriverName;

  if Assigned(FDelegateConnection) then
  begin
    Params.Values[GetPrefix+TDBXPropertyNames.DelegateConnection] := FDelegateConnection.ConnectionName;
    FDelegateConnection.GetFullParams(Params);
  end;
end;

function TConnectionData.GetIsModified: Boolean;
var
  I: Integer;
  OriginalProperties: TDBXProperties;
begin
  if not FHasIsModified then
  begin
    FHasIsModified := True;
    if not Assigned(FProperties) then
      Exit(False);
    OriginalProperties := GetOriginalProperties;
    FIsModified := Assigned(OriginalProperties) and (FProperties.Count <> OriginalProperties.Count - 1);
    if not FIsModified and Assigned(OriginalProperties) then
    begin
      for I := 0 to FProperties.Count - 1 do
      begin
        FIsModified := FProperties.Properties.ValueFromIndex[I] <> OriginalProperties.Values[FProperties.Properties.Names[I]];
        if FIsModified then
          Break;
      end;
    end;
  end;
  Result := FIsModified;
end;

function TConnectionData.GetOriginalProperties: TDBXProperties;
var
  OriginalName: string;
  ConnectionProperties: TDBXProperties;
begin
  if Assigned(FOriginalProperties) then
    OriginalName := FOriginalProperties[TDBXPropertyNames.ConnectionName]
  else
    OriginalName := EmptyStr;
  if ConnectionName <> OriginalName then
  begin
    FreeAndNil(FOriginalProperties);
    if (ConnectionName <> EmptyStr) then
    begin
      TDBXConnectionFactory.Lock;
      try
        ConnectionProperties := TDBXConnectionFactory.GetConnectionFactory.HasConnectionProperties(ConnectionName);
        if Assigned(ConnectionProperties) then
          FOriginalProperties := ConnectionProperties.Clone;
      finally
        TDBXConnectionFactory.Unlock;
      end;
    end;
  end;
  Result := FOriginalProperties;
end;

function TConnectionData.GetPrefix: string;
begin
  if Assigned(FConnection) then
    Result := EmptyStr
  else
    Result := FConnectionName + '.';
end;

function TConnectionData.GetProperties: TDBXProperties;
begin
  Result := FProperties;
end;

function TConnectionData.IsMyProperty(const PropertyName: string): Boolean;
begin
  if Assigned(FConnection) then
    Result := not ContainsText(PropertyName, '.')
  else
    Result := StartsText(FConnectionName, PropertyName);
end;

procedure TConnectionData.RefreshProperties;
begin
  if Assigned(FOriginalProperties) then
    FOriginalProperties.Clear;
  FHasIsModified := False;
end;

procedure TConnectionData.ReloadProperties;
begin
  FProperties.Clear;
  FProperties.AddProperties(GetOriginalProperties.Properties);
  FProperties.Properties.Delete(FProperties.Properties.IndexOfName(TDBXPropertyNames.ConnectionName));
end;

procedure TConnectionData.SetConnectionName(const Value: string);
begin
  if Assigned(FConnection) then
    FConnection.ConnectionName := Value
  else
    FConnectionName := Value;
  DoChange(nil);
end;

procedure TConnectionData.SetDelegateConnection(const Value: TConnectionData);
begin
  FreeAndNil(FDelegateConnection);
  FDelegateConnection := Value;
  DoChange(nil);
end;

procedure TConnectionData.SetDriverName(const Value: string);
begin
  if Assigned(FConnection) then
    FConnection.DriverName := Value
  else
    FDriverName := Value;
  DoChange(nil);
end;

procedure TConnectionData.SetProperties(const Value: TDBXProperties);
begin
  if not Assigned(FProperties) or not Assigned(Value) or (FProperties.ClassType <> Value.ClassType)
    and not (csDesigning in FConnection.ComponentState) then
  begin
    FreeAndNil(FProperties);
    if Assigned(Value) then
    begin
      FProperties := Value.Clone;
      FProperties.OnChange := DoChange;
    end;
  end
  else
  begin
    FProperties.Clear;
    FProperties.AddProperties(Value.Properties);
  end;
  FHasIsModified := False;
  DoChange(nil);
end;

procedure TConnectionData.UpdateProperties(NewProperties: TStrings);
var
  I: Integer;
begin
  FProperties.Clear;
  for I := 0 to NewProperties.Count - 1 do
  begin
    if IsMyProperty(NewProperties.Names[I]) then
      FProperties.Add(NewProperties.Names[I], NewProperties.ValueFromIndex[I]);
  end;
  if Assigned(FDelegateConnection) then
    FDelegateConnection.UpdateProperties(NewProperties);
  FHasIsModified := False;
end;

end.
