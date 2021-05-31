unit Lists;

interface



type
  TDataList = class(TObject)
  private
    FList: TList;
  protected
    function Get(Index: Integer): TObject;
    procedure Put(Index: Integer; Item: TObject);
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(Item: TObject): Integer;
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    function Expand: TDataList;
    function First: TObject; inline;
    function IndexOf(Item: TObject): Integer;
    procedure Insert(Index: Integer; Item: TObject);
    function Last: TObject;
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(Item: TObject): Integer; inline;
    procedure Pack;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TObject read Get write Put; default;

  end;

implementation

{ TDataList }

function TDataList.Add(Item: TObject): Integer;
begin
  FList.Add(@Item);
end;

procedure TDataList.Clear;
begin
  FList.Clear;
end;

constructor TDataList.Create;
begin
  FList := TList.Create;
end;

procedure TDataList.Delete(Index: Integer);
begin
  FList.Delete(Index);
end;

destructor TDataList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TDataList.Expand: TDataList;
var
  FDataList: TDataList;
begin
  FDataList:= TDataList.Create;
  FDataList.FList := FList.Expand;
  Result := FDataList;
end;

function TDataList.First: TObject;
begin
  Result := TObject(FList.First^);
end;

function TDataList.Get(Index: Integer): TObject;
begin
    Result := TObject(FList[Index]^);
end;

function TDataList.GetCount: integer;
begin
  Result := FList.Count;
end;

function TDataList.IndexOf(Item: TObject): Integer;
begin
  Result := FList.IndexOf(@Item);
end;

procedure TDataList.Insert(Index: Integer; Item: TObject);
begin
  FList.Insert(Index, @Item);
end;

function TDataList.Last: TObject;
begin
  Result := TObject(FList.Last^);
end;

procedure TDataList.Move(CurIndex, NewIndex: Integer);
begin
  FList.Move(CurIndex, NewIndex);
end;

procedure TDataList.Pack;
begin
  FList.Pack;
end;

procedure TDataList.Put(Index: Integer; Item: TObject);
begin
  FList[Index] := @Item;
end;

function TDataList.Remove(Item: TObject): Integer;
begin
  FList.Remove(@Item);
end;


end.
