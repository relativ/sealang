{*******************************************************}
{                                                       }
{                Delphi Runtime Library                 }
{                                                       }
{ Copyright(c) 1995-2015 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

{*******************************************************}
{               XML Document Support                    }
{*******************************************************}

unit XMLDocument;

interface



type

  TNodeList = class;
  TXMLDoc = class;

  TNode = class(TObject)
  private
    FXMLNode: TXMLNode;
    FList: TList;
    FXMLDoc: TXMLDoc;

    function GetAttribute(const AttrName: string): Variant;
    procedure SetAttribute(const AttrName: string; const Value: Variant);
    function GetChildValue(const IndexOrName: Variant): Variant;
    procedure SetChildValue(const IndexOrName: Variant; const Value: Variant);
    function GetHasChildNodes: Boolean;
    function GetIsTextElement: Boolean;
    function GetLocalName: string;
    function GetNamespaceURI: string;
    function GetNodeName: string;
    function GetNodeType: TNodeType;
    function GetNodeValue: Variant;
    procedure SetNodeValue(const Value: Variant);
    function GetParentNode: TNode;
    function GetPrefix: string;
    function GetReadOnly: Boolean;
    procedure SetReadOnly(const Value: Boolean);
    function GetText: string;
    function GetXML: string;
    procedure SetText(const Value: string);
  public

    function GetAttributeNodes: TNodeList;
    function GetAttributeNS(const AttrName, NamespaceURI: string): Variant;
    function GetChildNodes: TNodeList;
    { TNode - Methods }
    function AddChild(const TagName: string; Index: Integer = -1): TNode;
    function CloneNode(Deep: Boolean): TNode;
    procedure DeclareNamespace(const Prefix, URI: string);
    function FindNamespaceDecl(const NamespaceURI: string): TNode;
    function FindNamespaceURI(const TagOrPrefix: string): string;
    function HasAttribute(const Name: string): Boolean;
    function NextSibling: TNode;
    procedure Normalize;
    function PreviousSibling: TNode;
    procedure SetAttributeNS(const AttrName, NamespaceURI: string; const Value: Variant);
  public
    constructor Create(const AParentNode: TXMLNode; const OwnerDoc: TXMLDoc);
    destructor Destroy; override;

    property Attributes[const AttrName: string]: Variant read GetAttribute write SetAttribute;
    property ChildValues[const IndexOrName: Variant]: Variant read GetChildValue write SetChildValue; default;
    property HasChildNodes: Boolean read GetHasChildNodes;
    property IsTextElement: Boolean read GetIsTextElement;
    property LocalName: string read GetLocalName;
    property NamespaceURI: string read GetNameSpaceURI;
    property NodeName: string read GetNodeName;
    property NodeType: TNodeType read GetNodeType;
    property NodeValue: Variant read GetNodeValue write SetNodeValue;
    property ParentNode: TNode read GetParentNode;
    property Prefix: string read GetPrefix;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property Text: string read GetText write SetText;
    property XML: string read GetXML;

    property AttributeNodes: TNodeList read GetAttributeNodes;
    property ChildNodes: TNodeList read GetChildNodes ;
  end;

  TNodeList = class(TObject)
  private
    ListNodes: TList;
    FXMLDoc: TXMLDoc;
  public
    { IXMLNodeList }
    function Add(const Node: TNode): Integer;
    procedure Clear;
    function Delete(const Index: Integer): Integer;
    function DeleteByName(const Name: string): Integer;
    function First: TNode;
    function FindNode(NodeName: string): TNode;
    function FindSibling(const Node: TNode; Delta: Integer): TNode;
    function Get(Index: Integer): TNode;
    function GetNode(const IndexOrName: Variant): TNode;
    function GetCount: Integer;
    function IndexOf(const Node: TNode): Integer;
    function IndexOfByName(const Name: string): Integer;
    procedure Insert(Index: Integer; const Node: TNode);
    function Last: TNode;
    function Remove(const Node: TNode): Integer;
    function ReplaceNode(const OldNode, NewNode: TNode): TNode;
    property Count: Integer read GetCount;
    property Nodes[const IndexOrName: Variant]: TNode read GetNode; default;
  protected
    FXMLNodeList: TXMLNodeList;

  public
    destructor Destroy; override;
    constructor Create(XMLDoc: TXMLDoc);
  end;

  TXMLDoc = class(TObject)
  private
    FXMLDoc : TXMLDocument;
    NodeList: TList;
    procedure SetFileName(const Value: string);
  protected
    function GetActive: Boolean; virtual;
    function GetChildNodes: TNodeList;
    function GetDocumentElement: TNode;
    function GetEncoding: string;
    function GetDocumentNode: TNode;
    function GetFileName: string;
    function GetNodeIndentStr: string;
    function GetOptions: TXMLDocOptions;
    function GetParseOptions: TParseOptions;
    function GetVersion: string;
    function GetXML: TStrings;
    procedure SetActive(const Value: Boolean); virtual;
    procedure SetDocumentElement(const Value: TNode);
    procedure SetEncoding(const Value: string);
    procedure SetNodeIndentStr(const Value: string);
    procedure SetOptions(const Value: TXMLDocOptions);
    procedure SetParseOptions(const Value: TParseOptions);
    procedure SetVersion(const Value: string);
    procedure SetXML(const Value: TStrings);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    { IXMLDocument Methods }
    function AddChild(const TagName: string): TNode;
    function CreateElement(const TagOrData, NamespaceURI: string): TNode;
    function CreateNode(const NameOrData: string;
      NodeType: TNodeType = ntElement; const AddlData: string = ''): TNode;
    function GetDocBinding(const TagName: string;
      DocNodeClass: TClass; NamespaceURI: string = ''): TNode;
    function IsEmptyDoc: Boolean;
    procedure LoadFromFile(const AFileName: string = '');
    procedure LoadFromStream(const Stream: TStream; EncodingType:
      TXMLEncodingType = xetUnknown);
    procedure LoadFromXML(const XML: string);
    procedure Refresh;
    procedure RegisterDocBinding(const TagName: string;
      DocNodeClass: TClass; NamespaceURI: string = '');
    procedure Resync;
    procedure SaveToFile(const AFileName: string = '');
    procedure SaveToStream(const Stream: TStream);
    procedure SaveToXML(var XML: string);
    { Properties }
    property ChildNodes: TNodeList read GetChildNodes;
    property DocumentElement: TNode read GetDocumentElement write SetDocumentElement;
    property Encoding: string read GetEncoding write SetEncoding;
    property Node: TNode read GetDocumentNode;
    property Version: string read GetVersion write SetVersion;
  published
    property Active: Boolean read GetActive write SetActive default False;
    property FileName: string read GetFileName write SetFileName;
    property Options: TXMLDocOptions read GetOptions write SetOptions
      default [doNodeAutoCreate, doAttrNull, doAutoPrefix, doNamespaceDecl];
    property ParseOptions: TParseOptions read GetParseOptions write SetParseOptions default [];
    property XML: TStrings read GetXML write SetXML;
  end;
implementation


{ TXMLDoc }

function TXMLDoc.AddChild(const TagName: string): TNode;
var
  FNode : TNode;
begin
  FNode := TNode.Create(nil, Self);
  FNode.FXMLNode := TXMLNode(FXMLDoc.AddChild(TagName));
  NodeList.Add(FNode);
  Result := FNode;
end;

constructor TXMLDoc.Create;
begin
  inherited;
  FXMLDoc := TXMLDocument.Create(nil);
  NodeList:= TList.Create;
end;

function TXMLDoc.CreateElement(const TagOrData,
  NamespaceURI: string): TNode;
var
  FNode : TNode;
begin
  FNode := TNode.Create(nil, Self);
  FNode.FXMLNode := TXMLNode(FXMLDoc.CreateElement(TagOrData, NamespaceURI));
  NodeList.Add(FNode);
  Result := FNode;

end;

function TXMLDoc.CreateNode(const NameOrData: string; NodeType: TNodeType;
  const AddlData: string): TNode;
var
  FNode : TNode;
begin
  FNode := TNode.Create(nil, Self);
  FNode.FXMLNode := TXMLNode(FXMLDoc.CreateNode(NameOrData, NodeType, AddlData));
  NodeList.Add(FNode);
  Result := FNode;

end;

destructor TXMLDoc.Destroy;
var
  I: Integer;
begin
  for I := NodeList.Count -1 downto 0 do
  begin
    TObject(NodeList[I]).Free;
    NodeList.Delete(I);
  end;
  NodeList.Free;
  inherited;
end;

function TXMLDoc.GetActive: Boolean;
begin
  Result := FXMLDoc.Active;
end;

function TXMLDoc.GetChildNodes: TNodeList;
var
  FNode : TNodeList;
begin
  FNode := TNodeList.Create(Self);
  FNode.FXMLNodeList := TXMLNodeList(FXMLDoc.ChildNodes);
  NodeList.Add(FNode);
  Result := FNode;
end;

function TXMLDoc.GetDocBinding(const TagName: string; DocNodeClass: TClass;
  NamespaceURI: string): TNode;
var
  FNode : TNode;
begin
  FNode := TNode.Create(nil, Self);
  FNode.FXMLNode := TXMLNode(FXMLDoc.GetDocBinding(TagName, DocNodeClass, NamespaceURI));
  NodeList.Add(FNode);
  Result := FNode;

end;

function TXMLDoc.GetDocumentElement: TNode;
var
  FNode : TNode;
begin
  FNode := TNode.Create(nil, Self);
  FNode.FXMLNode := TXMLNode(FXMLDoc.DocumentElement);
  NodeList.Add(FNode);
  Result := FNode;
end;

function TXMLDoc.GetDocumentNode: TNode;
var
  FNode : TNode;
begin
  FNode := TNode.Create(nil, Self);
  FNode.FXMLNode := TXMLNode(FXMLDoc.Node);
  NodeList.Add(FNode);
  Result := FNode;

end;

function TXMLDoc.GetEncoding: string;
begin
  Result := FXMLDoc.Encoding;
end;

function TXMLDoc.GetFileName: string;
begin
  Result := FXMLDoc.FileName;
end;

function TXMLDoc.GetNodeIndentStr: string;
begin
  Result := FXMLDoc.NodeIndentStr;
end;

function TXMLDoc.GetOptions: TXMLDocOptions;
begin
  Result := FXMLDoc.Options;
end;

function TXMLDoc.GetParseOptions: TParseOptions;
begin
  Result := FXMLDoc.ParseOptions;
end;

function TXMLDoc.GetVersion: string;
begin
  Result := FXMLDoc.Version;
end;

function TXMLDoc.GetXML: TStrings;
begin
  Result := FXMLDoc.XML;
end;

function TXMLDoc.IsEmptyDoc: Boolean;
begin
  Result := FXMLDoc.IsEmptyDoc;
end;

procedure TXMLDoc.LoadFromFile(const AFileName: string);
begin
  FXMLDoc.LoadFromFile(AFileName);
end;

procedure TXMLDoc.LoadFromStream(const Stream: TStream;
  EncodingType: TXMLEncodingType);
begin
  FXMLDoc.LoadFromStream(Stream, EncodingType);
end;

procedure TXMLDoc.LoadFromXML(const XML: string);
begin
  FXMLDoc.LoadFromXML(XML);
end;

procedure TXMLDoc.Refresh;
begin
  FXMLDoc.Refresh;
end;

procedure TXMLDoc.RegisterDocBinding(const TagName: string;
  DocNodeClass: TClass; NamespaceURI: string);
begin
  FXMLDoc.RegisterDocBinding(TagName, DocNodeClass, NamespaceURI);
end;

procedure TXMLDoc.Resync;
begin
  FXMLDoc.Resync;
end;

procedure TXMLDoc.SaveToFile(const AFileName: string);
begin
  FXMLDoc.SaveToFile(AFileName);
end;

procedure TXMLDoc.SaveToStream(const Stream: TStream);
begin
  FXMLDoc.SaveToStream(Stream);
end;

procedure TXMLDoc.SaveToXML(var XML: string);
begin
  FXMLDoc.SaveToXML(XML);
end;

procedure TXMLDoc.SetActive(const Value: Boolean);
begin
  FXMLDoc.Active := Value;
end;

procedure TXMLDoc.SetDocumentElement(const Value: TNode);
begin
  FXMLDoc.DocumentElement := Value.FXMLNode;
end;

procedure TXMLDoc.SetEncoding(const Value: string);
begin
  FXMLDoc.Encoding := Value;
end;

procedure TXMLDoc.SetFileName(const Value: string);
begin
  FXMLDoc.FileName := Value;
end;

procedure TXMLDoc.SetNodeIndentStr(const Value: string);
begin
  FXMLDoc.NodeIndentStr := Value;
end;

procedure TXMLDoc.SetOptions(const Value: TXMLDocOptions);
begin
  FXMLDoc.Options := Value;
end;

procedure TXMLDoc.SetParseOptions(const Value: TParseOptions);
begin
  FXMLDoc.ParseOptions := Value;
end;

procedure TXMLDoc.SetVersion(const Value: string);
begin
  FXMLDoc.Version := Value;
end;

procedure TXMLDoc.SetXML(const Value: TStrings);
begin
  FXMLDoc.XML := Value;
end;

{ TNodeList }

function TNodeList.Add(const Node: TNode): Integer;
begin
  Result := (FXMLNodeList as IXMLNodeList).Add(IXMLNode(Node.FXMLNode));
end;

procedure TNodeList.Clear;
begin
  (FXMLNodeList as IXMLNodeList).Clear;
end;

constructor TNodeList.Create(XMLDoc: TXMLDoc);
begin
  ListNodes:= TList.Create;
  FXMLDoc := XMLDoc;
end;

function TNodeList.Delete(const Index: Integer): Integer;
begin
  Result := (FXMLNodeList as IXMLNodeList).Delete(Index);
end;

function TNodeList.DeleteByName(const Name: string): Integer;
begin
  Result := (FXMLNodeList as IXMLNodeList).Delete(Name);
end;

destructor TNodeList.Destroy;
var
  I: Integer;
begin
  for I := ListNodes.Count -1 downto 0 do
  begin
    TObject(ListNodes[I]).Free;
    ListNodes.Delete(I);
  end;
  ListNodes.Free;
  inherited;
end;

function TNodeList.FindNode(NodeName: string): TNode;
var
  FNode : TNode;
begin
  FNode := TNode.Create(nil, FXMLDoc);
  FNode.FXMLNode := TXMLNode((FXMLNodeList as IXMLNodeList).FindNode(NodeName));
  ListNodes.Add(FNode);
  Result := FNode;

end;

function TNodeList.FindSibling(const Node: TNode; Delta: Integer): TNode;
var
  FNode : TNode;
begin
  FNode := TNode.Create(nil, FXMLDoc);
  FNode.FXMLNode := TXMLNode((FXMLNodeList as IXMLNodeList).FindSibling(Node.FXMLNode, Delta));
  ListNodes.Add(FNode);
  Result := FNode;

end;

function TNodeList.First: TNode;
var
  FNode : TNode;
begin
  FNode := TNode.Create(nil, FXMLDoc);
  FNode.FXMLNode := TXMLNode((FXMLNodeList as IXMLNodeList).First());
  ListNodes.Add(FNode);
  Result := FNode;

end;

function TNodeList.Get(Index: Integer): TNode;
var
  FNode : TNode;
begin
  FNode := TNode.Create(nil, FXMLDoc);
  FNode.FXMLNode := TXMLNode((FXMLNodeList as IXMLNodeList).Get(Index));
  ListNodes.Add(FNode);
  Result := FNode;

end;

function TNodeList.GetCount: Integer;
begin
  Result := (FXMLNodeList as IXMLNodeList).Count;
end;

function TNodeList.GetNode(const IndexOrName: Variant): TNode;
var
  FNode : TNode;
begin
  FNode := TNode.Create(nil, FXMLDoc);
  FNode.FXMLNode := TXMLNode((FXMLNodeList as IXMLNodeList).Nodes[IndexOrName]);
  ListNodes.Add(FNode);
  Result := FNode;

end;

function TNodeList.IndexOf(const Node: TNode): Integer;
begin
  Result := (FXMLNodeList as IXMLNodeList).IndexOf(Node.FXMLNode);
end;

function TNodeList.IndexOfByName(const Name: string): Integer;
begin
  Result := (FXMLNodeList as IXMLNodeList).IndexOf(Name);
end;

procedure TNodeList.Insert(Index: Integer; const Node: TNode);
begin
  (FXMLNodeList as IXMLNodeList).Insert(Index, Node.FXMLNode);
end;

function TNodeList.Last: TNode;
var
  FNode : TNode;
begin
  FNode := TNode.Create(nil, FXMLDoc);
  FNode.FXMLNode := TXMLNode((FXMLNodeList as IXMLNodeList).Last);
  ListNodes.Add(FNode);
  Result := FNode;

end;

function TNodeList.Remove(const Node: TNode): Integer;
begin
  Result := (FXMLNodeList as IXMLNodeList).Remove(Node.FXMLNode);
end;

function TNodeList.ReplaceNode(const OldNode, NewNode: TNode): TNode;
var
  FNode : TNode;
begin
  FNode := TNode.Create(nil, FXMLDoc);
  FNode.FXMLNode := TXMLNode((FXMLNodeList as IXMLNodeList).ReplaceNode(OldNode.FXMLNode, NewNode.FXMLNode));
  ListNodes.Add(FNode);
  Result := FNode;

end;

{ TNode }

function TNode.AddChild(const TagName: string; Index: Integer): TNode;
var
  FNode : TNode;
begin
  FNode := TNode.Create(nil, FXMLDoc);
  FNode.FXMLNode := TXMLNode((FXMLNode as IXMLNode).AddChild(TagName, Index));
  FList.Add(FNode);
  Result := FNode;

end;

function TNode.CloneNode(Deep: Boolean): TNode;
var
  FNode : TNode;
begin
  FNode := TNode.Create(nil, FXMLDoc);
  FNode.FXMLNode := TXMLNode((FXMLNode as IXMLNode).CloneNode(Deep));
  FList.Add(FNode);
  Result := FNode;

end;

constructor TNode.Create(const AParentNode: TXMLNode; const OwnerDoc: TXMLDoc);
begin
  FList := TList.Create;
  FXMLDoc := OwnerDoc;
  FXMLNode := TXMLNode.Create(CreateDOMNode(OwnerDoc.FXMLDoc.DOMDocument,
      '', ntText), nil, OwnerDoc.FXMLDoc);
end;

procedure TNode.DeclareNamespace(const Prefix, URI: string);
begin
  (FXMLNode as IXMLNode).DeclareNamespace(Prefix, URI);
end;

destructor TNode.Destroy;
var
  I: Integer;
begin
  for I := FList.Count -1 downto 0 do
  begin
    TObject(FList[I]).Free;
    FList.Delete(I);
  end;
  FList.Free;
  inherited;
end;

function TNode.FindNamespaceDecl(const NamespaceURI: string): TNode;
var
  FNode : TNode;
begin
  FNode := TNode.Create(nil, FXMLDoc);
  FNode.FXMLNode := TXMLNode((FXMLNode as IXMLNode).FindNamespaceDecl(NamespaceURI));
  FList.Add(FNode);
  Result := FNode;

end;

function TNode.FindNamespaceURI(const TagOrPrefix: string): string;
begin
  Result :=(FXMLNode as IXMLNode).FindNamespaceURI(TagOrPrefix);

end;

function TNode.GetAttribute(const AttrName: string): Variant;
begin
  Result := (FXMLNode as IXMLNode).Attributes[AttrName];
end;

function TNode.GetAttributeNodes: TNodeList;
var
  FNode : TNodeList;
begin
  FNode := TNodeList.Create(FXMLDoc);
  FNode.FXMLNodeList := (FXMLNode as IXMLNode).AttributeNodes as TXMLNodeList;
  FList.Add(FNode);
  Result := FNode;

end;

function TNode.GetAttributeNS(const AttrName, NamespaceURI: string): Variant;
begin
  Result := (FXMLNode as IXMLNode).GetAttributeNS(AttrName, NamespaceURI);
end;

function TNode.GetChildNodes: TNodeList;
var
  FNode : TNodeList;
begin
  FNode := TNodeList.Create(FXMLDoc);
  FNode.FXMLNodeList := TXMLNodeList((FXMLNode as IXMLNode).ChildNodes);
  FList.Add(FNode);
  Result := FNode;
end;

function TNode.GetChildValue(const IndexOrName: Variant): Variant;
begin
  Result := (FXMLNode as IXMLNode).ChildValues[IndexOrName];
end;

function TNode.GetHasChildNodes: Boolean;
begin
  Result := (FXMLNode as IXMLNode).HasChildNodes;
end;

function TNode.GetIsTextElement: Boolean;
begin
  Result := (FXMLNode as IXMLNode).IsTextElement;

end;

function TNode.GetLocalName: string;
begin
  Result := (FXMLNode as IXMLNode).LocalName;
end;

function TNode.GetNamespaceURI: string;
begin
  Result := (FXMLNode as IXMLNode).NamespaceURI;
end;

function TNode.GetNodeName: string;
begin
  Result := (FXMLNode as IXMLNode).NodeName;
end;


function TNode.GetNodeType: TNodeType;
begin
  Result := (FXMLNode as IXMLNode).NodeType;
end;

function TNode.GetNodeValue: Variant;
begin
  Result := (FXMLNode as IXMLNode).NodeValue;
end;

function TNode.GetParentNode: TNode;
var
  FNode : TNode;
begin
  FNode := TNode.Create(nil, FXMLDoc);
  FNode.FXMLNode := TXMLNode((FXMLNode as IXMLNode).ParentNode);
  FList.Add(FNode);
  Result := FNode;
end;

function TNode.GetPrefix: string;
begin
  Result := (FXMLNode as IXMLNode).Prefix;
end;


function TNode.GetReadOnly: Boolean;
begin
  Result := (FXMLNode as IXMLNode).ReadOnly;
end;

function TNode.GetText: string;
begin
  Result := (FXMLNode as IXMLNode).Text;
end;

function TNode.GetXML: string;
begin
  Result := (FXMLNode as IXMLNode).XML;
end;

function TNode.HasAttribute(const Name: string): Boolean;
begin
  Result := (FXMLNode as IXMLNode).HasAttribute(Name);
end;

function TNode.NextSibling: TNode;
var
  FNode : TNode;
begin
  FNode := TNode.Create(nil, FXMLDoc);
  FNode.FXMLNode := TXMLNode((FXMLNode as IXMLNode).NextSibling());
  FList.Add(FNode);
  Result := FNode;

end;

procedure TNode.Normalize;
begin
  (FXMLNode as IXMLNode).Normalize;
end;

function TNode.PreviousSibling: TNode;
var
  FNode : TNode;
begin
  FNode := TNode.Create(nil, FXMLDoc);
  FNode.FXMLNode := TXMLNode((FXMLNode as IXMLNode).PreviousSibling());
  FList.Add(FNode);
  Result := FNode;

end;

procedure TNode.SetAttribute(const AttrName: string; const Value: Variant);
begin
  (FXMLNode as IXMLNode).Attributes[AttrName] := Value;
end;

procedure TNode.SetAttributeNS(const AttrName, NamespaceURI: string;
  const Value: Variant);
begin
  (FXMLNode as IXMLNode).SetAttributeNS(AttrName, NamespaceURI, Value);
end;

procedure TNode.SetChildValue(const IndexOrName, Value: Variant);
begin
    (FXMLNode as IXMLNode).ChildValues[IndexOrName] := Value;
end;

procedure TNode.SetNodeValue(const Value: Variant);
begin
    (FXMLNode as IXMLNode).NodeValue := Value;
end;

procedure TNode.SetReadOnly(const Value: Boolean);
begin
  (FXMLNode as IXMLNode).ReadOnly := Value;
end;

procedure TNode.SetText(const Value: string);
begin
  (FXMLNode as IXMLNode).Text := Value;
end;

end.
