unit djson;

interface

uses System.SysUtils, System.Classes, System.Variants, generics.collections;

type
  TJSON = class;

  TJSONItems = class(TObject)
    public
      Items: TDictionary<string,TJSON>;
      function Get(const name: string): TJSON;
      constructor Create;
      destructor Destroy; override;
  end;

  TJSONListItems = class(TObject)
    public
      ListItems: TList<TJSON>;
      constructor Create;
      destructor Destroy; override;
  end;

  TJSON = class(TObject)
    private
      FParent: TJSON;
      FIsList: boolean;
      FIsKeyValue: boolean;
      FIsDict: boolean;
      FValue: Variant;
      FItems: TJSONItems;
      FListItems: TJSONListItems;
	  FName: string;
      function GetJSONByNameOrIndex(const AData: variant): TJSON;
      function GetString: string;
      function GetInteger: integer;
      function GetBoolean: boolean;
      function GetInt64: int64;
      function GetDouble: double;
      function GetDateTime: TDateTime;
      function GetIsNull: boolean;
    public
      constructor Create(AParent: TJSON = nil);
      destructor Destroy; override;
     // function GetEnumerator: TList<TJSON>.TEnumerator;
      function Parse(const AJSON: string): TJSON;

      property Parent: TJSON read FParent;
      property IsList: boolean read FIsList;
      property IsDict: boolean read FIsDict;
      property IsNull: boolean read GetIsNull;
      property Items: TJSONItems read FItems;
      property ListItems: TJSONListItems read FListItems;
      property Value: Variant read FValue;
	  property Name: string read FName;
      property AsString: string read GetString;
      property AsInteger: integer read GetInteger;
      property AsBoolean: boolean read GetBoolean;
      property AsInt64: int64 read GetInt64;
      property AsDouble: double read GetDouble;
      property AsDateTime: TDateTime read GetDateTime;
      property JSONByNameOrIndex[const AData: variant]: TJSON read GetJSONByNameOrIndex; default;
      property _[const AData: variant]: TJSON read GetJSONByNameOrIndex;
  end;

  EJSONUnknownFieldOrIndex = class(Exception);
  EJSONParseError = class(Exception);
  {$IFDEF MSWINDOWS}
  procedure DebugStr(const msg: variant);
  {$ENDIF}

var
  DJSONFormatSettings: TFormatSettings;

implementation

uses
  XSBuiltIns
  {$IFDEF MSWINDOWS}, Windows{$ENDIF}
  ;

{$M+}
{$TYPEINFO ON}

{$IFDEF MSWINDOWS}
procedure DebugStr(const msg: variant);
begin
  OutputDebugString(PWideChar(format('%s: %s', [FormatDateTime('hh:nn:ss.zzz', now), msg])));
end;
{$ENDIF}

{ TJSON }

constructor TJSON.Create(AParent: TJSON);
begin
  FParent := AParent;
  FValue := Unassigned;
end;

destructor TJSON.Destroy;
begin
  if assigned(FListItems) then
    FListItems.free;
  if assigned(FItems) then
    FItems.Free;
  inherited;
end;

function TJSON.GetBoolean: boolean;
begin
  result := VarAsType(FValue, varBoolean);
end;

function TJSON.GetDateTime: TDateTime;
// TODO: Make a better date/time parser
var
  d: string;
begin
  d := VarToStr(FValue);
  if length(d) = 10 then // date
    result := StrToDate(d, DJSONFormatSettings)
  else if length(d) = 8 then // time
    result := StrToTime(d, DJSONFormatSettings)
  else
    with TXSDateTime.Create() do
    try
      XSToNative(d);
      Result := AsDateTime;
    finally
      Free();
    end;
end;

function TJSON.GetDouble: double;
begin
  result := VarAsType(FValue, varDouble);
end;

{ function TJSON.GetEnumerator: TList<TJSON>.TEnumerator;
begin
  result := FListItems.GetEnumerator;
end; }

function TJSON.GetInt64: int64;
begin
  result := VarAsType(FValue, varInt64);
end;

function TJSON.GetInteger: integer;
begin
  result := VarAsType(FValue, varInteger);
end;

function TJSON.GetIsNull: boolean;
begin
  result := Value = null;
end;

function TJSON.GetJSONByNameOrIndex(const AData: variant): TJSON;
begin
  case VarType(AData) and VarTypeMask of
    varString, varUString, varWord, varLongWord:
      if not FItems.Items.TryGetValue(AData, result) then
        raise EJSONUnknownFieldOrIndex.Create(format('Unknown field: %s', [AData]))
      else
        exit;
    varInteger, varInt64, varSmallint, varShortInt, varByte:
    begin
      if (FListItems.ListItems.Count - 1) >= AData then
      begin
        result := FListItems.ListItems.items[AData];
        exit;
      end
      else
        raise EJSONUnknownFieldOrIndex.Create(format('Unknown index: %d', [AData]));
    end;
  end;
  raise EJSONUnknownFieldOrIndex.Create(format('Unknown field variant type: %s.', [VarTypeAsText(AData)]));
end;

function TJSON.GetString: string;
begin
  if VarIsType(FValue, varNull) then
    result := ''
  else
    result := VarToStr(FValue);
end;

function TJSON.Parse(const AJSON: string): TJSON;
var
  a, prevA: char;
  index, tag: integer;
  inString, escaped: boolean;
  temp: variant;
  obj: TJSON;
	
  function getName():  string;
  var
	  resultS,s: string;
    i: integer;
  begin
    s := trim(AJSON.Substring(tag-1, index-tag));
    for i := 0 to s.Length - 1 do
    begin
      case s.chars[i] of
        '\', '"': continue;
		',', '}': break;
      else
        resultS := resultS + s.chars[i];
      end;
    end;
    result := resultS;
  end;
  
  function getValue: variant;
  var
    prev, prevPrev: char;
    ubuf, i, skip: integer;
    s: string;
    resultS: string;
  begin
    s := trim(AJSON.Substring(tag-1, index-tag));
    result := unassigned;
    if s = '' then
      exit;

    if s.Chars[0] <> '"' then
    begin
      if s = 'null' then
        exit(null)
      else if s = 'false' then
        exit(false)
      else if s = 'true' then
        exit(true);
      exit(s);
    end;

    if s = '""' then
      exit('');
    resultS := '';
    prev := #0;
    prevPrev := #0;
    skip := 0;
    for i := 0 to s.Length - 1 do
    begin
      if skip > 0 then
      begin
        dec(skip);
        Continue;
      end;
      try
        if (prev = '\') and (prevPrev <> '\') then
        begin
          case s.chars[i] of
            '\', '/', '"': resultS := resultS + s.chars[i];
            'u':
            begin
              if not TryStrToInt('$' + s.Substring(i+1, 4), ubuf) then
                raise EJSONParseError.Create(format('Invalid unicode \u%s', [s.Substring(i+1, 4)]));
              resultS := resultS + WideChar(ubuf);
              skip := 4;
              Continue;
            end;
            'b': resultS := resultS + #8;
            'n': resultS := resultS + #10;
            'r': resultS := resultS + #13;
            't': resultS := resultS + #9;
            'f': resultS := resultS + #12;
          end;
        end
        else
          case s.chars[i] of
            '\', '"': continue;
          else
            resultS := resultS + s.chars[i];
          end;
      finally
        if (prev = '\') and (prevPrev = '\') then
          prevPrev := #0
        else
          prevPrev := prev;
        prev := s.chars[i];
      end;
    end;
    if resultS <> '' then
      result := resultS;
  end;

  procedure SetValue;
  begin
    obj.FValue := getValue;
  end;

  procedure AddSingleValue;
  begin
    temp := getValue();
    if not VarIsEmpty(temp) then
    begin
      obj := TJSON.Create(obj);
      obj.FValue := temp;
      obj.parent.ListItems.ListItems.Add(obj);
      obj := obj.Parent;
    end;
  end;

begin
  result := nil;
  index := 0;
  tag := 0;
  prevA := ' ';
  inString := false;
  escaped := false;
  obj := nil;
  for a in AJSON do
  begin
    inc(index);
    if tag = 0 then
      tag := index;
    escaped := (prevA = '\') and (not escaped);
    if (a = '"') and (not escaped) then
      inString := not inString;
    prevA := a;
    if inString or (CharInSet(a, [#9, #10, #13, ' '])) then
      continue;
    case a of
      '{':
      begin
        if not assigned(obj) or not obj.FIsKeyValue then
          obj := TJSON.Create(obj);
        obj.FIsKeyValue := false;
        obj.FIsDict := true;
		obj.FName   := getName();
        obj.FItems := TJSONItems.Create;
        if not assigned(result) then
        begin
          result := obj;
        end;
        if assigned(obj.parent) and obj.parent.IsList then
        begin
          obj.Parent.ListItems.ListItems.Add(obj);
        end;
        tag := 0;
      end;
      '}':
      begin
        if not obj.IsDict then
        begin
          SetValue();
          obj := obj.Parent;
        end;
        obj := obj.Parent;
        tag := 0;
      end;
      '[':
      begin
        if not assigned(obj) or not obj.FIsKeyValue then
          obj := TJSON.Create(obj);
        obj.FIsKeyValue := false;
        obj.FIsList := true;
		obj.FName   := getName();
        obj.FListItems := TJSONListItems.Create;
        if not assigned(result) then
        begin
          result := obj;
        end;
        if assigned(obj.parent) and obj.parent.IsList then
        begin
          obj.Parent.ListItems.ListItems.Add(obj);
        end;
        tag := 0;
      end;
      ']':
      begin
        if not obj.IsList and not obj.IsDict then
        begin
          SetValue();
          obj.Parent.ListItems.ListItems.Add(obj);
        end
        else if obj.IsList then
        begin
          AddSingleValue();
        end;
        obj := obj.Parent;
        tag := 0;
      end;
      ':':
      begin
        obj := TJSON.Create(obj);
        obj.FIsKeyValue := true;
        obj.Parent.Items.Items.Add(getValue(), obj);
        tag := 0;
      end;
      ',':
      begin
        if not obj.IsList and not obj.IsDict then
        begin
          SetValue();
          obj := obj.Parent;
        end
        else if obj.IsList then
        begin
          AddSingleValue();
        end;
        tag := 0;
      end;
    end;
  end;
end;

{ TJSONItems }

constructor TJSONItems.Create;
begin
  Items:= TDictionary<string,TJSON>.Create();
end;

destructor TJSONItems.Destroy;
var
  item: TJSON;
begin
  for item in self.Items.Values do
    item.Free;
  Items.Free;
  inherited;
end;

function TJSONItems.Get(const name: string): TJSON;
begin
  Result := Items[name];
end;

{ TJSONListItem }

constructor TJSONListItems.Create;
begin
  ListItems:= TList<TJSON>.Create();
end;

destructor TJSONListItems.Destroy;
var
  item: TJSON;
begin
  for item in self.ListItems do
    item.Free;
  ListItems.Free;
  inherited;
end;

initialization

  DJSONFormatSettings := TFormatsettings.Create;
  with DJSONFormatSettings do
  begin
    DateSeparator := '-';
    TimeSeparator := ':';
    ShortDateFormat := 'yyyy-mm-dd';
    LongDateFormat := 'yyyy-mm-dd';
    ShortTimeFormat := 'hh:nn:ss';
    LongTimeFormat := 'hh:nn:ss';
  end;

end.
