unit XMLObject;

interface

uses
  Classes, SysUtils;

type
  TXmlObjectVerySimple = class;
  TXmlObjectNode = class;
  TXmlObjectNodeList = class;

  TXmlObjectStreamReader = class(TObject)
  protected
    FXmlStreamReader: TXmlStreamReader;
  public
    ///	<summary> Extend the TStreamReader with RTTI pointers </summary>
    constructor Create(Stream: TStream; Encoding: TEncoding; DetectBOM: Boolean = False; BufferSize: Integer = 4096);
    ///	<summary> Assures the read buffer holds at least Value characters </summary>
    function PrepareBuffer(Value: Integer): Boolean;
    ///	<summary> Extract text until chars found in StopChars </summary>
    function ReadText(const StopChars: String; Options: TExtractTextOptions): String; virtual;
    ///	<summary> Returns fist char but does not removes it from the buffer </summary>
    function FirstChar: String;
    ///	<summary> Proceed with the next character(s) (value optional, default 1) </summary>
    procedure IncCharPos(Value: Integer = 1); virtual;
    ///	<summary> Returns True if the first uppercased characters at the current position match Value </summary>
    function IsUppercaseText(const Value: String): Boolean; virtual;
  end;


  TXmlObjectAttribute = class(TObject)
  private
    procedure SetValue(const Value: String);
    function GetValue: string;
    function GetAttributeType: TXmlAttributeType;
    procedure SetAttributeType(Value: TXmlAttributeType);
    function GetName: String;
    procedure SetName(Value: String);
  protected
    FXmlAttribute: TXmlAttribute;
  public

    ///	<summary> Attribute name </summary>
    property Name: String read GetName write SetName;
    ///	<summary> Attributes without values are set to atSingle, else to atValue </summary>
    property AttributeType: TXmlAttributeType read GetAttributeType write SetAttributeType;
    ///	<summary> Create a new attribute </summary>
    ///	<summary> Return the attribute as a String </summary>
    function AsString: String;
    /// <summary> Escapes XML control characters </summar>
    class function Escape(const Value: String): String; virtual;
    ///	<summary> Attribute value (always a String) </summary>
    property Value: String read GetValue write SetValue;
  end;

  TXmlObjectAttributeList = class(TObject)
  private
    FList: TList;
    function GetDocument: TXmlObjectVerySimple;
    procedure SetDocument(Value: TXmlObjectVerySimple);
  protected
    FXmlAttributeList: TXmlAttributeList;
    destructor Destroy; override;
  public
    constructor Create;
    ///	<summary> The xml document of the attribute list of the node</summary>

    property Document: TXmlObjectVerySimple read GetDocument write SetDocument;
    ///	<summary> Add a name only attribute </summary>
    function Add(const Name: String): TXmlObjectAttribute; overload; virtual;
    ///	<summary> Returns the attribute given by name (case insensitive), NIL if no attribute found </summary>
    function Find(const Name: String): TXmlObjectAttribute; virtual;
    ///	<summary> Deletes an attribute given by name (case insensitive) </summary>
    procedure Delete(const Name: String); overload; virtual;
    ///	<summary> Returns True if an attribute with the given name is found (case insensitive) </summary>
    function HasAttribute(const AttrName: String): Boolean; virtual;
    ///	<summary> Returns the attributes in string representation </summary>
    function AsString: String; virtual;
  end;

  TXmlObjectNode = class(TObject)
  private
    FList: TList;
  protected
    FXMLNode: TXmlNode;
    FDocument: TXmlObjectVerySimple;
    function GetAttr(const AttrName: String): String; virtual;
    procedure SetAttr(const AttrName: String; const AttrValue: String); virtual;
    function GetParent: TXmlObjectNode;
    procedure SetParent(Value: TXmlObjectNode);
    function GetNodeType: TXmlNodeType;
    procedure SetNodeTypeI(Value: TXmlNodeType);
    function GetName: String;
    procedure SetName(Value: String);
    function GetChildNodes: TXmlObjectNodeList;
    function GetAttributeList: TXmlObjectAttributeList;
    procedure SetAttributeList(Value: TXmlObjectAttributeList);
    function GetText: String;
    procedure SetTextI(Value: String);
    function GetDocument: TXmlObjectVerySimple;
    procedure SetDocument(Value: TXmlObjectVerySimple);

  public
    ///	<summary> All attributes of the node </summary>
    property AttributeList: TXmlObjectAttributeList read GetAttributeList write SetAttributeList;
    ///	<summary> List of child nodes, never NIL </summary>
    property ChildNodes: TXmlObjectNodeList read GetChildNodes;
    ///	<summary> Name of the node </summary>
    property Name: String read GetName write SetName; // Node name
    ///	<summary> The node type, see TXmlObjectNodeType </summary>
    property NodeType: TXmlNodeType read GetNodeType write SetNodeTypeI;
    ///	<summary> Parent node, may be NIL </summary>
    property Parent: TXmlObjectNode read GetParent write SetParent;
    ///	<summary> Text value of the node </summary>
    property Text: String read GetText write SetTextI;
    /// <summary> Creates a new XML node </summary>
    constructor Create(ANodeType: TXmlNodeType = ntElement); virtual;
    ///	<summary> Removes the node from its parent and frees all of its childs </summary>
    destructor Destroy; override;
    ///	<summary> Clears the attributes, the text and all of its child nodes (but not the name) </summary>
    procedure Clear;
    ///	<summary> Find a child node by its name </summary>
    function Find(const Name: String; NodeTypes: TXmlNodeTypes = [ntElement]): TXmlObjectNode; overload; virtual;
    ///	<summary> Find a child node by name and attribute name </summary>
    function Find(const Name, AttrName: String; NodeTypes: TXmlNodeTypes = [ntElement]): TXmlObjectNode; overload; virtual;
    ///	<summary> Find a child node by name, attribute name and attribute value </summary>
    function Find(const Name, AttrName, AttrValue: String; NodeTypes: TXmlNodeTypes = [ntElement]): TXmlObjectNode; overload; virtual;
    ///	<summary> Return a list of child nodes with the given name and (optional) node types </summary>
    function FindNodes(const Name: String; NodeTypes: TXmlNodeTypes = [ntElement]): TXmlObjectNodeList; virtual;
    ///	<summary> Returns True if the attribute exists </summary>
    function HasAttribute(const AttrName: String): Boolean; virtual;
    ///	<summary> Returns True if a child node with that name exits </summary>
    function HasChild(const Name: String; NodeTypes: TXmlNodeTypes = [ntElement]): Boolean; virtual;
    ///	<summary> Add a child node with an optional NodeType (default: ntElement)</summary>
    function AddChild(const AName: String; ANodeType: TXmlNodeType = ntElement): TXmlObjectNode; virtual;
    ///	<summary> Insert a child node at a specific position with a (optional) NodeType (default: ntElement)</summary>
    function InsertChild(const Name: String; Position: Integer; NodeType: TXmlNodeType = ntElement): TXmlObjectNode; virtual;
    ///	<summary> Fluent interface for setting the text of the node </summary>
    function SetText(const Value: String): TXmlObjectNode; virtual;
    ///	<summary> Fluent interface for setting the node attribute given by attribute name and attribute value </summary>
    function SetAttribute(const AttrName, AttrValue: String): TXmlObjectNode; virtual;
    ///	<summary> Returns first child or NIL if there aren't any child nodes </summary>
    function FirstChild: TXmlObjectNode; virtual;
    ///	<summary> Returns last child node or NIL if there aren't any child nodes </summary>
    function LastChild: TXmlObjectNode; virtual;
    ///	<summary> Returns next sibling </summary>
    function NextSibling: TXmlObjectNode; overload; virtual;
    ///	<summary> Returns previous sibling </summary>
    function PreviousSibling: TXmlObjectNode; overload; virtual;
    ///	<summary> Returns True if the node has at least one child node </summary>
    function HasChildNodes: Boolean; virtual;
    ///	<summary> Returns True if the node has a text content and no child nodes </summary>
    function IsTextElement: Boolean; virtual;
    ///	<summary> Fluent interface for setting the node type </summary>
    function SetNodeType(Value: TXmlNodeType): TXmlObjectNode; virtual;
    ///	<summary> Attributes of a node, accessible by attribute name (case insensitive) </summary>
    property Attributes[const AttrName: String]: String read GetAttr write SetAttr;
    ///	<summary> The xml document of the node </summary>
    property Document: TXmlObjectVerySimple read GetDocument write SetDocument;
    ///	<summary> The node name, same as property Name </summary>
    property NodeName: String read GetName write SetName;
    ///	<summary> The node text, same as property Text </summary>
    property NodeValue: String read GetText write SetTextI;
  end;

  TXmlObjectNodeList = class(TObject)
  private
    FList: TList;
    FDocument: TXmlObjectVerySimple;
    function GetParent: TXmlObjectNode;
    procedure SetParent(Value: TXmlObjectNode);
    function GetDocument: TXmlObjectVerySimple;
    procedure SetDocument(Value: TXmlObjectVerySimple);
  protected
    FXmlNodeList: TXmlNodeList;

    destructor Destroy; override;
  public
    constructor Create;
    ///	<summary> The xml document of the node list </summary>
    property Document: TXmlObjectVerySimple read GetDocument write SetDocument;
    ///	<summary> The parent node of the node list </summary>
    property Parent: TXmlObjectNode read GetParent write SetParent;
    ///	<summary> Adds a node and sets the parent of the node to the parent of the list </summary>
    function Add(Value: TXmlObjectNode): Integer; overload; virtual;
    ///	<summary> Creates a new node of type NodeType (default ntElement) and adds it to the list </summary>
    function Add(NodeType: TXmlNodeType = ntElement): TXmlObjectNode; overload; virtual;
    ///	<summary> Add a child node with an optional NodeType (default: ntElement)</summary>
    function Add(const Name: String; NodeType: TXmlNodeType = ntElement): TXmlObjectNode; overload; virtual;
    ///	<summary> Find a node by its name (case sensitive), returns NIL if no node is found </summary>
    function Find(const Name: String; NodeTypes: TXmlNodeTypes = [ntElement]): TXmlObjectNode; overload; virtual;
    ///	<summary> Same as Find(), returnsa a node by its name (case sensitive) </summary>
    function FindNode(const Name: String; NodeTypes: TXmlNodeTypes = [ntElement]): TXmlObjectNode; virtual;
    ///	<summary> Find a node that has the the given attribute, returns NIL if no node is found </summary>
    function Find(const Name, AttrName: String; NodeTypes: TXmlNodeTypes = [ntElement]): TXmlObjectNode; overload; virtual;
    ///	<summary> Find a node that as the given attribute name and value, returns NIL otherwise </summary>
    function Find(const Name, AttrName, AttrValue: String; NodeTypes: TXmlNodeTypes = [ntElement]): TXmlObjectNode; overload; virtual;
    ///	<summary> Return a list of child nodes with the given name and (optional) node types </summary>
    function FindNodes(const Name: String; NodeTypes: TXmlNodeTypes = [ntElement]): TXmlObjectNodeList; virtual;
    ///	<summary> Returns True if the list contains a node with the given name </summary>
    function HasNode(const Name: String; NodeTypes: TXmlNodeTypes = [ntElement]): Boolean; virtual;
    ///	<summary> Inserts a node at the given position </summary>
    function Insert(const Name: String; Position: Integer; NodeType: TXmlNodeType = ntElement): TXmlObjectNode; overload; virtual;
    ///	<summary> Returns the first child node, same as .First </summary>
    function FirstChild: TXmlObjectNode; virtual;
    ///	<summary> Returns next sibling node </summary>
    function NextSibling(Node: TXmlObjectNode): TXmlObjectNode; virtual;
    ///	<summary> Returns previous sibling node </summary>
    function PreviousSibling(Node: TXmlObjectNode): TXmlObjectNode; virtual;
    ///	<summary> Returns the node at the given position </summary>
    function Get(Index: Integer): TXmlObjectNode; virtual;
  end;

  TXmlObjectVerySimple = class(TObject)
  private
    FChildList: TList;
    function GetHeader: TXmlObjectNode;
    procedure SetHeader(Value: TXmlObjectNode);
    function GetDocumentElement: TXmlObjectNode;
  protected
    FXMLVerySimple: TXmlVerySimple;
    procedure Parse(Reader: TXmlObjectStreamReader); virtual;
    procedure ParseComment(Reader: TXmlObjectStreamReader; var Parent: TXmlObjectNode); virtual;
    procedure ParseDocType(Reader: TXmlObjectStreamReader; var Parent: TXmlObjectNode); virtual;
    procedure ParseProcessingInstr(Reader: TXmlObjectStreamReader; var Parent: TXmlObjectNode); virtual;
    procedure ParseCData(Reader: TXmlObjectStreamReader; var Parent: TXmlObjectNode); virtual;
    procedure ParseText(const Line: String; Parent: TXmlObjectNode); virtual;
    function ParseTag(Reader: TXmlObjectStreamReader; ParseText: Boolean; var Parent: TXmlObjectNode): TXmlObjectNode; overload; virtual;
    function ParseTag(const TagStr: String; var Parent: TXmlObjectNode): TXmlObjectNode; overload; virtual;
    procedure Walk(Writer: TStreamWriter; const PrefixNode: String; Node: TXmlObjectNode); virtual;
    procedure SetText(const Value: String); virtual;
    function GetText: String; virtual;
    procedure SetEncoding(const Value: String); virtual;
    function GetEncoding: String; virtual;
    procedure SetVersion(const Value: String); virtual;
    function GetVersion: String; virtual;
    procedure SetStandAlone(const Value: String); virtual;
    function GetStandAlone: String; virtual;
    function GetChildNodes: TXmlObjectNodeList; virtual;
    procedure SetDocumentElement(Value: TXmlObjectNode); virtual;
    procedure SetPreserveWhitespace(Value: Boolean);
    function GetPreserveWhitespace: Boolean;
    function IsSame(const Value1, Value2: String): Boolean;
    function GetOptions: TXmlOptions;
    procedure SetOptions(Value: TXmlOptions);
    function GetNodeIndentStr: string;
    procedure SetNodeIndentStr(Value: string);
    function GetLineBreak: string;
    procedure SetLineBreak(Value: string);
  public
    ///	<summary> Indent used for the xml output </summary>
    property NodeIndentStr: String read GetNodeIndentStr write SetNodeIndentStr;
    ///	<summary> LineBreak used for the xml output, default set to sLineBreak which is OS dependent </summary>
    property LineBreak: String read GetLineBreak write SetLineBreak;
    ///	<summary> Options for xml output like indentation type </summary>
    property Options: TXmlOptions read GetOptions write SetOptions;
    ///	<summary> Creates a new XML document parser </summary>
    constructor Create; virtual;
    ///	<summary> Destroys the XML document parser </summary>
    destructor Destroy; override;
    ///	<summary> Deletes all nodes </summary>
    procedure Clear; virtual;
    ///	<summary> Adds a new node to the document, if it's the first ntElement then sets it as .DocumentElement </summary>
    function AddChild(const Name: String; NodeType: TXmlNodeType = ntElement): TXmlObjectNode; virtual;
    ///	<summary> Creates a new node but doesn't adds it to the document nodes </summary>
    function CreateNode(const Name: String; NodeType: TXmlNodeType = ntElement): TXmlObjectNode; virtual;
    /// <summary> Escapes XML control characters </summar>
    class function Escape(const Value: String): String; virtual;
    /// <summary> Translates escaped characters back into XML control characters </summar>
    class function Unescape(const Value: String): String; virtual;
    ///	<summary> Loads the XML from a file </summary>
    function LoadFromFile(const FileName: String; BufferSize: Integer = 4096): TXmlObjectVerySimple; virtual;
    ///	<summary> Loads the XML from a stream </summary>
    function LoadFromStream(const Stream: TStream; BufferSize: Integer = 4096): TXmlObjectVerySimple; virtual;
    ///	<summary> Parse attributes into the attribute list for a given string </summary>
    procedure ParseAttributes(const AttribStr: String; AttributeList: TXmlObjectAttributeList); virtual;
    ///	<summary> Saves the XML to a file </summary>
    function SaveToFile(const FileName: String): TXmlObjectVerySimple; virtual;
    ///	<summary> Saves the XML to a stream, the encoding is specified in the .Encoding property </summary>
    function SaveToStream(const Stream: TStream): TXmlObjectVerySimple; virtual;
    ///	<summary> A list of all root nodes of the document </summary>
    property ChildNodes: TXmlObjectNodeList read GetChildNodes;
    ///	<summary> Returns the first element node </summary>
    property DocumentElement: TXmlObjectNode read GetDocumentElement;
    ///	<summary> Specifies the encoding of the XML file, anything else then 'utf-8' is considered as ANSI </summary>
    property Encoding: String read GetEncoding write SetEncoding;
    ///	<summary> XML declarations are stored in here as Attributes </summary>
    property Header: TXmlObjectNode read GetHeader;
    ///	<summary> Set to True if all spaces and linebreaks should be included as a text node, same as doPreserve option </summary>
    property PreserveWhitespace: Boolean read GetPreserveWhitespace write SetPreserveWhitespace;
    ///	<summary> Defines the xml declaration property "StandAlone", set it to "yes" or "no" </summary>
    property StandAlone: String read GetStandAlone write SetStandAlone;
    ///	<summary> The XML as a string representation </summary>
    property Text: String read GetText write SetText;
    ///	<summary> Defines the xml declaration property "Version", default set to "1.0" </summary>
    property Version: String read GetVersion write SetVersion;
    ///	<summary> The XML as a string representation, same as .Text </summary>
    property Xml: String read GetText write SetText;
  end;

implementation

{ TXmlObjectVerySimple }

function TXmlObjectVerySimple.AddChild(const Name: String;
  NodeType: TXmlNodeType): TXmlObjectNode;
var
  Node: TXmlObjectNode;
begin
  Node:= TXmlObjectNode.Create(NodeType);
  Node.FXMLNode := FXMLVerySimple.AddChild(Name, NodeType);
  FChildList.Add(Node);
  Result := Node;
end;

procedure TXmlObjectVerySimple.Clear;
begin
  FXMLVerySimple.Clear;
end;

constructor TXmlObjectVerySimple.Create;
begin
  FXMLVerySimple:= TXmlVerySimple.Create;
  FChildList:= TList.Create;
end;

function TXmlObjectVerySimple.CreateNode(const Name: String;
  NodeType: TXmlNodeType): TXmlObjectNode;
var
  Node: TXmlObjectNode;
begin
  Node:= TXmlObjectNode.Create(NodeType);
  Node.FXMLNode := FXMLVerySimple.CreateNode(Name, NodeType);
  FChildList.Add(Node);
  Result := Node;
end;

destructor TXmlObjectVerySimple.Destroy;
var
  I: Integer;
begin
  for I := FChildList.Count -1 downto 0 do
  begin
    TObject(FChildList[I]).Free;
    FChildList.Delete(I);
  end;
  FChildList.Free;
  FXMLVerySimple.Free;
  inherited;
end;

class function TXmlObjectVerySimple.Escape(const Value: String): String;
begin
  Result := TXMLVerySimple.Escape(Value);
end;

function TXmlObjectVerySimple.GetChildNodes: TXmlObjectNodeList;
var
  ANode: TXmlObjectNodeList;
begin
  ANode:= TXmlObjectNodeList.Create;
  ANode.FXmlNodeList := FXMLVerySimple.ChildNodes;
  FChildList.Add(ANode);
  Result := ANode;
end;

function TXmlObjectVerySimple.GetDocumentElement: TXmlObjectNode;
var
  ONode: TXmlObjectNode;
  Node: TXMLNode;
begin
  Node := FXMLVerySimple.DocumentElement;
  ONode:= TXmlObjectNode.Create(Node.NodeType);
  ONode.FXMLNode := Node;
  FChildList.Add(ONode);
  Result := ONode;
end;

function TXmlObjectVerySimple.GetEncoding: String;
begin
  Result := FXMLVerySimple.Encoding;
end;

function TXmlObjectVerySimple.GetHeader: TXmlObjectNode;
var
  ONode: TXmlObjectNode;
  Node: TXMLNode;
begin
  Node := FXMLVerySimple.Header;
  ONode:= TXmlObjectNode.Create(Node.NodeType);
  ONode.FXMLNode := Node;
  FChildList.Add(ONode);
  Result := ONode;

end;

function TXmlObjectVerySimple.GetLineBreak: string;
begin
  Result := FXMLVerySimple.LineBreak;
end;

function TXmlObjectVerySimple.GetNodeIndentStr: string;
begin
  Result := FXMLVerySimple.NodeIndentStr;
end;

function TXmlObjectVerySimple.GetOptions: TXmlOptions;
begin
  Result := FXMLVerySimple.Options;
end;

function TXmlObjectVerySimple.GetPreserveWhitespace: Boolean;
begin
  Result := FXMLVerySimple.PreserveWhitespace;
end;

function TXmlObjectVerySimple.GetStandAlone: String;
begin
  Result := FXMLVerySimple.StandAlone;
end;

function TXmlObjectVerySimple.GetText: String;
begin
  Result := FXMLVerySimple.Text;
end;

function TXmlObjectVerySimple.GetVersion: String;
begin
  Result := FXMLVerySimple.Version;
end;

function TXmlObjectVerySimple.IsSame(const Value1, Value2: String): Boolean;
begin

end;

function TXmlObjectVerySimple.LoadFromFile(const FileName: String;
  BufferSize: Integer): TXmlObjectVerySimple;
var
  ONode: TXmlObjectVerySimple;
  Node: TXmlVerySimple;
begin
  Node := FXMLVerySimple.LoadFromFile(FileName, BufferSize);
  ONode:= TXmlObjectVerySimple.Create;
  ONode.FXMLVerySimple := Node;
  FChildList.Add(ONode);
  Result := ONode;

end;

function TXmlObjectVerySimple.LoadFromStream(const Stream: TStream;
  BufferSize: Integer): TXmlObjectVerySimple;
var
  ONode: TXmlObjectVerySimple;
  Node: TXmlVerySimple;
begin
  Node := FXMLVerySimple.LoadFromStream(Stream, BufferSize);
  ONode:= TXmlObjectVerySimple.Create;
  ONode.FXMLVerySimple := Node;
  FChildList.Add(ONode);
  Result := ONode;

end;

procedure TXmlObjectVerySimple.Parse(Reader: TXmlObjectStreamReader);
begin

end;

procedure TXmlObjectVerySimple.ParseAttributes(const AttribStr: String;
  AttributeList: TXmlObjectAttributeList);
begin
  FXMLVerySimple.ParseAttributes(AttribStr, AttributeList.FXmlAttributeList);
end;

procedure TXmlObjectVerySimple.ParseCData(Reader: TXmlObjectStreamReader;
  var Parent: TXmlObjectNode);
begin

end;

procedure TXmlObjectVerySimple.ParseComment(Reader: TXmlObjectStreamReader;
  var Parent: TXmlObjectNode);
begin

end;

procedure TXmlObjectVerySimple.ParseDocType(Reader: TXmlObjectStreamReader;
  var Parent: TXmlObjectNode);
begin

end;

procedure TXmlObjectVerySimple.ParseProcessingInstr(
  Reader: TXmlObjectStreamReader; var Parent: TXmlObjectNode);
begin

end;

function TXmlObjectVerySimple.ParseTag(const TagStr: String;
  var Parent: TXmlObjectNode): TXmlObjectNode;
begin

end;

function TXmlObjectVerySimple.ParseTag(Reader: TXmlObjectStreamReader;
  ParseText: Boolean; var Parent: TXmlObjectNode): TXmlObjectNode;
begin

end;

procedure TXmlObjectVerySimple.ParseText(const Line: String;
  Parent: TXmlObjectNode);
begin

end;

function TXmlObjectVerySimple.SaveToFile(
  const FileName: String): TXmlObjectVerySimple;
var
  ONode: TXmlObjectVerySimple;
  Node: TXmlVerySimple;
begin
  Node := FXMLVerySimple.SaveToFile(FileName);
  ONode:= TXmlObjectVerySimple.Create;
  ONode.FXMLVerySimple := Node;
  FChildList.Add(ONode);
  Result := ONode;

end;

function TXmlObjectVerySimple.SaveToStream(
  const Stream: TStream): TXmlObjectVerySimple;
var
  ONode: TXmlObjectVerySimple;
  Node: TXmlVerySimple;
begin
  Node := FXMLVerySimple.SaveToStream(Stream);
  ONode:= TXmlObjectVerySimple.Create;
  ONode.FXMLVerySimple := Node;
  FChildList.Add(ONode);
  Result := ONode;

end;

procedure TXmlObjectVerySimple.SetDocumentElement(Value: TXmlObjectNode);
begin
  FXMLVerySimple.DocumentElement := Value.FXMLNode;
end;

procedure TXmlObjectVerySimple.SetEncoding(const Value: String);
begin
  FXMLVerySimple.Encoding := Value;
end;

procedure TXmlObjectVerySimple.SetHeader(Value: TXmlObjectNode);
begin

end;

procedure TXmlObjectVerySimple.SetLineBreak(Value: string);
begin
  FXMLVerySimple.LineBreak := Value;
end;

procedure TXmlObjectVerySimple.SetNodeIndentStr(Value: string);
begin
  FXMLVerySimple.NodeIndentStr := Value;
end;

procedure TXmlObjectVerySimple.SetOptions(Value: TXmlOptions);
begin
  FXMLVerySimple.Options := Value;
end;

procedure TXmlObjectVerySimple.SetPreserveWhitespace(Value: Boolean);
begin
  FXMLVerySimple.PreserveWhitespace := Value;
end;

procedure TXmlObjectVerySimple.SetStandAlone(const Value: String);
begin
  FXMLVerySimple.StandAlone := Value;
end;

procedure TXmlObjectVerySimple.SetText(const Value: String);
begin
  FXMLVerySimple.Text := Value;
end;

procedure TXmlObjectVerySimple.SetVersion(const Value: String);
begin
  FXMLVerySimple.Version := Value;
end;

class function TXmlObjectVerySimple.Unescape(const Value: String): String;
begin
  TXmlVerySimple.Unescape(Value);
end;

procedure TXmlObjectVerySimple.Walk(Writer: TStreamWriter;
  const PrefixNode: String; Node: TXmlObjectNode);
begin

end;

{ TXmlObjectNodeList }

function TXmlObjectNodeList.Add(NodeType: TXmlNodeType): TXmlObjectNode;
var
  ONode: TXmlObjectNode;
  Node: TXmlNode;
begin
  Node := FXmlNodeList.Add(NodeType);
  ONode:= TXmlObjectNode.Create(NodeType);
  ONode.FXMLNode := Node;
  FList.Add(ONode);
  Result := ONode;
end;

function TXmlObjectNodeList.Add(Value: TXmlObjectNode): Integer;
begin
  if Value.FXMLNode = nil then
    Value.FXMLNode := TXmlNode.Create();
  Result := FXmlNodeList.Add(Value.FXMLNode);
end;

function TXmlObjectNodeList.Add(const Name: String;
  NodeType: TXmlNodeType): TXmlObjectNode;
var
  ONode: TXmlObjectNode;
  Node: TXmlNode;
begin
  Node := FXmlNodeList.Add(Name, NodeType);
  ONode:= TXmlObjectNode.Create(NodeType);
  ONode.FXMLNode := Node;
  FList.Add(ONode);
  Result := ONode;

end;

constructor TXmlObjectNodeList.Create;
begin
  FList := TList.Create;
end;

destructor TXmlObjectNodeList.Destroy;
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

function TXmlObjectNodeList.Find(const Name, AttrName: String;
  NodeTypes: TXmlNodeTypes): TXmlObjectNode;
var
  ONode: TXmlObjectNode;
  Node: TXmlNode;
begin
  Node := FXmlNodeList.Find(Name, AttrName, NodeTypes);
  ONode:= TXmlObjectNode.Create(Node.NodeType);
  ONode.FXMLNode := Node;
  FList.Add(ONode);
  Result := ONode;

end;

function TXmlObjectNodeList.Find(const Name, AttrName, AttrValue: String;
  NodeTypes: TXmlNodeTypes): TXmlObjectNode;
var
  ONode: TXmlObjectNode;
  Node: TXmlNode;
begin
  Node := FXmlNodeList.Find(Name, AttrName, AttrValue, NodeTypes);
  ONode:= TXmlObjectNode.Create(Node.NodeType);
  ONode.FXMLNode := Node;
  FList.Add(ONode);
  Result := ONode;

end;

function TXmlObjectNodeList.Find(const Name: String;
  NodeTypes: TXmlNodeTypes): TXmlObjectNode;
var
  ONode: TXmlObjectNode;
  Node: TXmlNode;
begin
  Node := FXmlNodeList.Find(Name, NodeTypes);
  ONode:= TXmlObjectNode.Create(Node.NodeType);
  ONode.FXMLNode := Node;
  FList.Add(ONode);
  Result := ONode;

end;

function TXmlObjectNodeList.FindNode(const Name: String;
  NodeTypes: TXmlNodeTypes): TXmlObjectNode;
var
  ONode: TXmlObjectNode;
  Node: TXmlNode;
begin
  Node := FXmlNodeList.FindNode(Name, NodeTypes);
  ONode:= TXmlObjectNode.Create(Node.NodeType);
  ONode.FXMLNode := Node;
  FList.Add(ONode);
  Result := ONode;

end;

function TXmlObjectNodeList.FindNodes(const Name: String;
  NodeTypes: TXmlNodeTypes): TXmlObjectNodeList;
var
  ONode: TXmlObjectNodeList;
  Node: TXmlNodeList;
begin
  Node := FXmlNodeList.FindNodes(Name, NodeTypes);
  ONode:= TXmlObjectNodeList.Create();
  ONode.FXmlNodeList := Node;
  FList.Add(ONode);
  Result := ONode;

end;

function TXmlObjectNodeList.FirstChild: TXmlObjectNode;
var
  ONode: TXmlObjectNode;
  Node: TXmlNode;
begin
  Node := FXmlNodeList.FirstChild();
  ONode:= TXmlObjectNode.Create(Node.NodeType);
  ONode.FXMLNode := Node;
  FList.Add(ONode);
  Result := ONode;

end;

function TXmlObjectNodeList.Get(Index: Integer): TXmlObjectNode;
var
  ONode: TXmlObjectNode;
  Node: TXmlNode;
begin
  Node := FXmlNodeList.Get(Index);
  ONode:= TXmlObjectNode.Create(Node.NodeType);
  ONode.FXMLNode := Node;
  FList.Add(ONode);
  Result := ONode;

end;

function TXmlObjectNodeList.GetDocument: TXmlObjectVerySimple;
var
  ONode: TXmlObjectVerySimple;
  Node: TXmlVerySimple;
begin
  Node := FXmlNodeList.Document;
  ONode:= TXmlObjectVerySimple.Create();
  ONode.FXMLVerySimple := Node;
  FList.Add(ONode);
  Result := ONode;
end;

function TXmlObjectNodeList.GetParent: TXmlObjectNode;
var
  ONode: TXmlObjectNode;
  Node: TXmlNode;
begin
  Node := FXmlNodeList.Parent;
  ONode:= TXmlObjectNode.Create(Node.NodeType);
  ONode.FXMLNode := Node;
  FList.Add(ONode);
  Result := ONode;
end;

function TXmlObjectNodeList.HasNode(const Name: String;
  NodeTypes: TXmlNodeTypes): Boolean;
begin
  Result := FXmlNodeList.HasNode(Name, NodeTypes);
end;

function TXmlObjectNodeList.Insert(const Name: String; Position: Integer;
  NodeType: TXmlNodeType): TXmlObjectNode;
var
  ONode: TXmlObjectNode;
  Node: TXmlNode;
begin
  Node := FXmlNodeList.Insert(Name, Position, NodeType);
  ONode:= TXmlObjectNode.Create(Node.NodeType);
  ONode.FXMLNode := Node;
  FList.Add(ONode);
  Result := ONode;

end;

function TXmlObjectNodeList.NextSibling(Node: TXmlObjectNode): TXmlObjectNode;
var
  ONode: TXmlObjectNode;
  XNode: TXmlNode;
begin
  XNode := FXmlNodeList.NextSibling(Node.FXMLNode);
  ONode:= TXmlObjectNode.Create(Node.NodeType);
  ONode.FXMLNode := XNode;
  FList.Add(ONode);
  Result := ONode;

end;

function TXmlObjectNodeList.PreviousSibling(
  Node: TXmlObjectNode): TXmlObjectNode;
var
  ONode: TXmlObjectNode;
  XNode: TXmlNode;
begin
  XNode := FXmlNodeList.PreviousSibling(Node.FXMLNode);
  ONode:= TXmlObjectNode.Create(Node.NodeType);
  ONode.FXMLNode := XNode;
  FList.Add(ONode);
  Result := ONode;

end;

procedure TXmlObjectNodeList.SetDocument(Value: TXmlObjectVerySimple);
begin
  FXmlNodeList.Document := Value.FXMLVerySimple;
end;

procedure TXmlObjectNodeList.SetParent(Value: TXmlObjectNode);
begin
  FXmlNodeList.Parent := Value.FXMLNode;
end;

{ TXmlObjectStreamReader }

constructor TXmlObjectStreamReader.Create(Stream: TStream; Encoding: TEncoding;
  DetectBOM: Boolean; BufferSize: Integer);
begin
  FXmlStreamReader:= TXmlStreamReader.Create(Stream, Encoding, DetectBOM, BufferSize);
end;

function TXmlObjectStreamReader.FirstChar: String;
begin
  Result := FXmlStreamReader.FirstChar;
end;

procedure TXmlObjectStreamReader.IncCharPos(Value: Integer);
begin
  FXmlStreamReader.IncCharPos(Value);
end;

function TXmlObjectStreamReader.IsUppercaseText(const Value: String): Boolean;
begin
  Result := FXmlStreamReader.IsUppercaseText(Value);
end;

function TXmlObjectStreamReader.PrepareBuffer(Value: Integer): Boolean;
begin
  Result := FXmlStreamReader.PrepareBuffer(Value);
end;

function TXmlObjectStreamReader.ReadText(const StopChars: String;
  Options: TExtractTextOptions): String;
begin
  Result := FXmlStreamReader.ReadText(StopChars, Options);
end;

{ TXmlObjectAttribute }

function TXmlObjectAttribute.AsString: String;
begin
  Result := FXmlAttribute.AsString();
end;

class function TXmlObjectAttribute.Escape(const Value: String): String;
begin
  Result := TXmlAttribute.Escape(Value);
end;

function TXmlObjectAttribute.GetAttributeType: TXmlAttributeType;
begin
  Result := FXmlAttribute.AttributeType;
end;

function TXmlObjectAttribute.GetName: String;
begin
  Result := FXmlAttribute.Name;
end;

function TXmlObjectAttribute.GetValue: string;
begin
  Result := FXmlAttribute.Value;
end;

procedure TXmlObjectAttribute.SetAttributeType(Value: TXmlAttributeType);
begin
  FXmlAttribute.AttributeType := Value;
end;

procedure TXmlObjectAttribute.SetName(Value: String);
begin
  FXmlAttribute.Name := Value;
end;

procedure TXmlObjectAttribute.SetValue(const Value: String);
begin
  FXmlAttribute.Value := Value;
end;

{ TXmlObjectAttributeList }

function TXmlObjectAttributeList.Add(const Name: String): TXmlObjectAttribute;
var
  XmlObjectAttribute: TXmlObjectAttribute;
begin
  XmlObjectAttribute:= TXmlObjectAttribute.Create();
  XmlObjectAttribute.FXmlAttribute := FXmlAttributeList.Add(Name);
  FList.Add(XmlObjectAttribute);
  Result := XmlObjectAttribute;
end;

function TXmlObjectAttributeList.AsString: String;
begin
  Result := FXmlAttributeList.AsString();
end;

constructor TXmlObjectAttributeList.Create;
begin
  FList:= TList.Create;
end;

procedure TXmlObjectAttributeList.Delete(const Name: String);
begin
  FXmlAttributeList.Delete(Name);
end;

destructor TXmlObjectAttributeList.Destroy;
var
  I: Integer;
begin
  for I := FList.Count -1 downto 0 do
  begin
    TObject(Flist[I]).Free;
    Flist.Delete(I);
  end;
  Flist.Free;
  inherited;
end;

function TXmlObjectAttributeList.Find(const Name: String): TXmlObjectAttribute;
var
  XmlObjectAttribute: TXmlObjectAttribute;
begin
  XmlObjectAttribute:= TXmlObjectAttribute.Create();
  XmlObjectAttribute.FXmlAttribute := FXmlAttributeList.Find(Name);
  FList.Add(XmlObjectAttribute);
  Result := XmlObjectAttribute;
end;

function TXmlObjectAttributeList.GetDocument: TXmlObjectVerySimple;
var
  XmlObjectVerySimple: TXmlObjectVerySimple;
begin
  XmlObjectVerySimple:= TXmlObjectVerySimple.Create;
  XmlObjectVerySimple.FXMLVerySimple := FXmlAttributeList.Document;
  FList.Add(XmlObjectVerySimple);
  Result := XmlObjectVerySimple;
end;

function TXmlObjectAttributeList.HasAttribute(const AttrName: String): Boolean;
begin
  Result := FXmlAttributeList.HasAttribute(AttrName);
end;

procedure TXmlObjectAttributeList.SetDocument(Value: TXmlObjectVerySimple);
begin
  FXmlAttributeList.Document := Value.FXMLVerySimple;
end;

{ TXmlObjectNode }

function TXmlObjectNode.AddChild(const AName: String;
  ANodeType: TXmlNodeType): TXmlObjectNode;
var
  ONode: TXmlObjectNode;
begin
  ONode:= TXmlObjectNode.Create;
  ONode.FXMLNode := FXMLNode.AddChild(Name, ANodeType);
  FList.Add(ONode);
  Result := ONode;
end;

procedure TXmlObjectNode.Clear;
begin
  FXMLNode.Clear;
end;

constructor TXmlObjectNode.Create(ANodeType: TXmlNodeType);
begin
  FList:= TList.Create;
//  FXMLNode := TXmlNode.Create(ANodeType);
//  FXMLNode.Document := Document.FXMLVerySimple;
end;

destructor TXmlObjectNode.Destroy;
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

function TXmlObjectNode.Find(const Name: String;
  NodeTypes: TXmlNodeTypes): TXmlObjectNode;
var
  ONode: TXmlObjectNode;
begin
  ONode:= TXmlObjectNode.Create;
  ONode.FXMLNode := FXMLNode.Find(Name, NodeTypes);
  if ONode.FXMLNode <> nil then
  begin
    FList.Add(ONode);
    Result := ONode;
  end else
    ONode.Free;

end;

function TXmlObjectNode.Find(const Name, AttrName, AttrValue: String;
  NodeTypes: TXmlNodeTypes): TXmlObjectNode;
var
  ONode: TXmlObjectNode;
begin
  ONode:= TXmlObjectNode.Create;
  ONode.FXMLNode := FXMLNode.Find(Name, AttrName, AttrValue, NodeTypes);
  if ONode.FXMLNode <> nil then
  begin
    FList.Add(ONode);
    Result := ONode;
  end else
    ONode.Free;

end;

function TXmlObjectNode.Find(const Name, AttrName: String;
  NodeTypes: TXmlNodeTypes): TXmlObjectNode;
var
  ONode: TXmlObjectNode;
begin
  ONode:= TXmlObjectNode.Create;
  ONode.FXMLNode := FXMLNode.Find(Name, AttrName, NodeTypes);
  if ONode.FXMLNode <> nil then
  begin
    FList.Add(ONode);
    Result := ONode;
  end else
    ONode.Free;

end;

function TXmlObjectNode.FindNodes(const Name: String;
  NodeTypes: TXmlNodeTypes): TXmlObjectNodeList;
var
  ONode: TXmlObjectNodeList;
begin
  ONode:= TXmlObjectNodeList.Create;
  ONode.FXmlNodeList := FXMLNode.FindNodes(Name, NodeTypes);
  if ONode.FXmlNodeList <> nil then
  begin
    FList.Add(ONode);
    Result := ONode;
  end else
    ONode.Free;

end;

function TXmlObjectNode.FirstChild: TXmlObjectNode;
var
  ONode: TXmlObjectNode;
begin
  ONode:= TXmlObjectNode.Create;
  ONode.FXMLNode := FXMLNode.FirstChild;
  if ONode.FXMLNode <> nil then
  begin
    FList.Add(ONode);
    Result := ONode;
  end else
    ONode.Free;

end;

function TXmlObjectNode.GetAttr(const AttrName: String): String;
begin
  Result := FXMLNode.Attributes[AttrName];
end;

function TXmlObjectNode.GetAttributeList: TXmlObjectAttributeList;
var
  ONode: TXmlObjectAttributeList;
begin
  ONode:= TXmlObjectAttributeList.Create;
  ONode.FXmlAttributeList := FXMLNode.AttributeList;
  if ONode.FXmlAttributeList <> nil then
  begin
    FList.Add(ONode);
    Result := ONode;
  end else
    ONode.Free;

end;

function TXmlObjectNode.GetChildNodes: TXmlObjectNodeList;
var
  ONode: TXmlObjectNodeList;
begin
  ONode:= TXmlObjectNodeList.Create;
  ONode.FXmlNodeList := FXMLNode.ChildNodes;
  if ONode.FXmlNodeList <> nil then
  begin
    FList.Add(ONode);
    Result := ONode;
  end else
    ONode.Free;

end;

function TXmlObjectNode.GetDocument: TXmlObjectVerySimple;
var
  ONode: TXmlObjectVerySimple;
begin
  ONode:= TXmlObjectVerySimple.Create;
  ONode.FXMLVerySimple := FXMLNode.Document;
  if ONode.FXMLVerySimple <> nil then
  begin
    FList.Add(ONode);
    Result := ONode;
  end else
    ONode.Free;

end;

function TXmlObjectNode.GetName: String;
begin
  Result := FXMLNode.Name;
end;

function TXmlObjectNode.GetNodeType: TXmlNodeType;
begin
  Result := FXMLNode.NodeType;
end;

function TXmlObjectNode.GetParent: TXmlObjectNode;
var
  ONode: TXmlObjectNode;
begin
  ONode:= TXmlObjectNode.Create;
  ONode.FXMLNode := FXMLNode.Parent;
  if ONode.FXMLNode <> nil then
  begin
    FList.Add(ONode);
    Result := ONode;
  end else
    ONode.Free;

end;

function TXmlObjectNode.GetText: String;
begin
  Result := FXMLNode.Text;
end;

function TXmlObjectNode.HasAttribute(const AttrName: String): Boolean;
begin
  Result := FXMLNode.HasAttribute(AttrName);
end;

function TXmlObjectNode.HasChild(const Name: String;
  NodeTypes: TXmlNodeTypes): Boolean;
begin
  Result := FXMLNode.HasChild(Name, NodeTypes);
end;

function TXmlObjectNode.HasChildNodes: Boolean;
begin
  Result := FXMLNode.HasChildNodes();
end;

function TXmlObjectNode.InsertChild(const Name: String; Position: Integer;
  NodeType: TXmlNodeType): TXmlObjectNode;
var
  ONode: TXmlObjectNode;
begin
  ONode:= TXmlObjectNode.Create;
  ONode.FXMLNode := FXMLNode.InsertChild(Name, Position, NodeType);
  if ONode.FXMLNode <> nil then
  begin
    FList.Add(ONode);
    Result := ONode;
  end else
    ONode.Free;

end;

function TXmlObjectNode.IsTextElement: Boolean;
begin
  Result := FXMLNode.IsTextElement();
end;

function TXmlObjectNode.LastChild: TXmlObjectNode;
var
  ONode: TXmlObjectNode;
begin
  ONode:= TXmlObjectNode.Create;
  ONode.FXMLNode := FXMLNode.LastChild();
  if ONode.FXMLNode <> nil then
  begin
    FList.Add(ONode);
    Result := ONode;
  end else
    ONode.Free;

end;

function TXmlObjectNode.NextSibling: TXmlObjectNode;
var
  ONode: TXmlObjectNode;
begin
  ONode:= TXmlObjectNode.Create;
  ONode.FXMLNode := FXMLNode.NextSibling();
  if ONode.FXMLNode <> nil then
  begin
    FList.Add(ONode);
    Result := ONode;
  end else
    ONode.Free;

end;

function TXmlObjectNode.PreviousSibling: TXmlObjectNode;
var
  ONode: TXmlObjectNode;
begin
  ONode:= TXmlObjectNode.Create;
  ONode.FXMLNode := FXMLNode.PreviousSibling();
  if ONode.FXMLNode <> nil then
  begin
    FList.Add(ONode);
    Result := ONode;
  end else
    ONode.Free;

end;

procedure TXmlObjectNode.SetAttr(const AttrName, AttrValue: String);
begin
  FXMLNode.Attributes[AttrName] := AttrValue;
end;

function TXmlObjectNode.SetAttribute(const AttrName,
  AttrValue: String): TXmlObjectNode;
var
  ONode: TXmlObjectNode;
begin
  ONode:= TXmlObjectNode.Create;
  ONode.FXMLNode := FXMLNode.SetAttribute(AttrName, AttrValue);
  if ONode.FXMLNode <> nil then
  begin
    FList.Add(ONode);
    Result := ONode;
  end else
    ONode.Free;

end;

procedure TXmlObjectNode.SetAttributeList(Value: TXmlObjectAttributeList);
begin

end;

procedure TXmlObjectNode.SetDocument(Value: TXmlObjectVerySimple);
begin
  FXMLNode.Document := Value.FXMLVerySimple;
end;

procedure TXmlObjectNode.SetName(Value: String);
begin
  FXMLNode.Name := Value;
end;

function TXmlObjectNode.SetNodeType(Value: TXmlNodeType): TXmlObjectNode;
var
  ONode: TXmlObjectNode;
begin
  ONode:= TXmlObjectNode.Create;
  ONode.FXMLNode := FXMLNode.SetNodeType(Value);
  if ONode.FXMLNode <> nil then
  begin
    FList.Add(ONode);
    Result := ONode;
  end else
    ONode.Free;
end;

procedure TXmlObjectNode.SetNodeTypeI(Value: TXmlNodeType);
begin
  FXMLNode.NodeType := Value;
end;

procedure TXmlObjectNode.SetParent(Value: TXmlObjectNode);
begin
  FXMLNode.Parent := Value.FXMLNode;
end;

function TXmlObjectNode.SetText(const Value: String): TXmlObjectNode;
var
  ONode: TXmlObjectNode;
begin
  ONode:= TXmlObjectNode.Create;
  ONode.FXMLNode := FXMLNode.SetText(Value);
  if ONode.FXMLNode <> nil then
  begin
    FList.Add(ONode);
    Result := ONode;
  end else
    ONode.Free;

end;

procedure TXmlObjectNode.SetTextI(Value: String);
begin
  FXMLNode.Text := Value;
end;

end.
