unit MultipartParser;

// #################################################
// #### Multipart parser based on Matlus multipart
// #### parser. But made compatible with the delphi 2007
// #### framework. The original framework can be found on
// #### http://matlus.com/scripts/website.dll
// #################################################

interface

uses SysUtils, Classes, HTTPAPP, contnrs;

type
  EClientConnectionDropped = class(Exception);

  TMultiPartHTTPFile = class(TAbstractWebRequestFile)
  private
    fFieldName: string;
    fContentType: string;
    fFileName: string;
    fFileData: TMemoryStream;
  protected
    function GetFieldName: string; override;
    function GetFileName: string; override;
    function GetStream: TStream; override;
    function GetContentType: string; override;
  public
    constructor Create(const Data; dataSize : LongWord; const ContentType, FieldName, FileName : string);
    destructor Destroy;override;

    procedure SaveToFile(SaveAsFile: string);
    procedure SaveToStream(Stream: TStream);
  end;

  TMultiPartHTTPFiles = class(TAbstractWebRequestFiles)
  private
    fFileList: TObjectList;
  protected
    function GetCount: Integer; override;
    function GetItem(I: Integer): TAbstractWebRequestFile; override;
  public
    constructor Create;
    destructor Destroy; override;

    // new introduced methods
    procedure Clear;
    function Add(AObject: TMultiPartHTTPFile): Integer;
  end;

  TMultiPartContentParser = class(TAbstractContentParser)
  private
    fHTTPFiles: TMultiPartHTTPFiles;
    fContentFields: TStrings;

  protected
    function GetContentFields: TStrings; override;
    function GetFiles: TAbstractWebRequestFiles; override;
  public
    procedure Parse;
    constructor Create(AWebRequest: TWebRequest); override;
    destructor Destroy; override;
    class function CanParse(AWebRequest: TWebRequest): Boolean; override;
  end;


implementation

uses StrUtils, Math;


// #################################################
// #### Utf8 helper functions
// no real utf8 code checking but the ansi version should work just fine
function Utf8StrPos(subStr, pS : PAnsiChar; len : integer) : PAnsiChar;
var pSub : PAnsiChar;
    ppS : PAnsiChar;
    res : boolean;
    idx : integer;
begin
     Result := nil;

     idx := 0;
     while idx < len do
     begin
          pSub := substr;
          ppS := pS;
          res := True;
          while (pSub^ <> #0) and (ppS^ <> #0) and res do
          begin
               res := res and (pSub^ = ppS^);
               inc(pSub);
               inc(ppS);
          end;

          res := res and (ppS^ <> #0);

          if res then
             exit(pS);

          inc(pS);
          inc(idx);
     end;
end;

function AnsiPosEx(const subStr, s: UTF8String;
  startIdx: integer): integer;
var pS : PAnsiChar;
    pRes : PAnsiChar;
begin
     Result := 0;
     if (s = '') or (Length(s) < startIdx) or (subStr = '') then
        exit;

     pS := PAnsiChar(s);
     inc(pS, startIdx - 1);

     pRes := Utf8StrPos(PAnsiChar(subStr), pS, Length(s) - Max(0, (startIdx - 1)));
     if Assigned(pRes) then
        Result := Integer(pRes) - Integer(PAnsiChar(S)) + 1;
end;

function AnsiPos(const subStr, s : UTF8String) : integer;
begin
     Result := AnsiPosEx(substr, s, 1);
end;


{ TMultiPartHTTPFile }

constructor TMultiPartHTTPFile.Create(const Data; dataSize : LongWord; const ContentType, FieldName, FileName : string);
begin
     inherited Create;

     fFileData := TMemoryStream.Create;
     if dataSize > 0 then
        fFileData.WriteBuffer(Data, dataSize);

     fFieldName := FieldName;
     fContentType := ContentType;
     fFileName := FileName;

     fFileData.Position := 0;
end;

destructor TMultiPartHTTPFile.Destroy;
begin
     fFileData.Free;

     inherited;
end;

function TMultiPartHTTPFile.GetContentType: string;
begin
     Result := Utf8String(fContentType);
end;

function TMultiPartHTTPFile.GetFieldName: string;
begin
     Result := Utf8String(fFieldName);
end;

function TMultiPartHTTPFile.GetFileName: string;
begin
     Result := Utf8String(fFileName);
end;

function TMultiPartHTTPFile.GetStream: TStream;
begin
     Result := fFileData;
end;

procedure TMultiPartHTTPFile.SaveToFile(SaveAsFile: string);
begin
     fFileData.SaveToFile(SaveAsFile);
end;

procedure TMultiPartHTTPFile.SaveToStream(Stream: TStream);
begin
     fFileData.Position := 0;
     TMemoryStream(fFileData).SaveToStream(Stream);
     Stream.Position := 0;
end;

{ TMultiPartHTTPFiles }

function TMultiPartHTTPFiles.Add(AObject: TMultiPartHTTPFile): Integer;
begin
     Result := fFileList.Add(AObject);
end;

procedure TMultiPartHTTPFiles.Clear;
begin
     fFileList.Clear;
end;

constructor TMultiPartHTTPFiles.Create;
begin
     inherited Create;

     fFileList := TObjectList.Create(True);
end;

destructor TMultiPartHTTPFiles.Destroy;
begin
     fFileList.Free;

     inherited;
end;

function TMultiPartHTTPFiles.GetCount: Integer;
begin
     Result := fFileList.Count;
end;

function TMultiPartHTTPFiles.GetItem(I: Integer): TAbstractWebRequestFile;
begin
     assert((i >= 0) and (i < fFileList.Count), 'Multipart file index out of bounds');

     Result := TAbstractWebRequestFile(fFileList[i]);
end;

{ TMultiPartContentParser }

class function TMultiPartContentParser.CanParse(
  AWebRequest: TWebRequest): Boolean;
const cMultiPartContentType = 'multipart/form-data';
      cMultiPartLen = Length(cMultiPartContentType);
begin
     // #################################################
     // #### Check for the field: multipart/form-data
     Result := CompareText( Copy(UTF8ToString(AWebRequest.ContentType), 1, cMultiPartLen), cMultiPartContentType) = 0;
end;

constructor TMultiPartContentParser.Create(AWebRequest: TWebRequest);
begin
     inherited Create(AWebRequest);

     fHTTPFiles := TMultiPartHTTPFiles.Create;
     fContentFields := TStringList.Create;
end;

destructor TMultiPartContentParser.Destroy;
begin
     fContentFields.Free;
     fHTTPFiles.Free;

     inherited;
end;

function TMultiPartContentParser.GetContentFields: TStrings;
begin
     if (fHTTPFiles.Count = 0) and (fContentFields.Count = 0) then
        Parse;

     Result := fContentFields;
end;

function TMultiPartContentParser.GetFiles: TAbstractWebRequestFiles;
begin
     if (fHTTPFiles.Count = 0) and (fContentFields.Count = 0) then
        Parse;

     Result := fHTTPFiles;
end;

procedure TMultiPartContentParser.Parse;
const
  HeaderTerminator : AnsiString = #13#10#13#10;
  LnHeaderTerminator : integer = 4;
  cMaxChunkSize : integer = 65536;
var HTTPFile: TMultiPartHTTPFile;
    TotalBytes: LongInt;
    BytesRead: Longint;
    HeaderInfoLn: Longint;
    ChunkSize: Longint;
    Buffer: array of Byte;
    HeaderInfo: UTF8String;
    FieldNameInHeader: UTF8String;
    ContentType: String;
    FileNameInHeader: UTF8String;
    HeaderDataTerminator: UTF8String;
    sBuffer: RawByteString;
    sValue: UTF8String;
    ContentStream : TStringStream;
    origHeaInfo : UTF8String;
begin
     TotalBytes := WebRequest.ContentLength;
     SetLength(sBuffer, TotalBytes);

     BytesRead := Length(WebRequest.RawContent);
     Move(WebRequest.RawContent[1], sBuffer[1], BytesRead);

     // #################################################
     // #### Read Multipart content
     if BytesRead < TotalBytes then
     begin
          SetLength(Buffer, Min(cMaxChunkSize, TotalBytes - BytesRead));
          repeat
                ChunkSize := WebRequest.ReadClient(Buffer[0], Min(cMaxChunkSize, TotalBytes - BytesRead));
                if (ChunkSize <= 0) or (bytesRead + chunkSize > Length(sBuffer)) then
                   Break;

                Move(Buffer[0], sBuffer[BytesRead + 1], ChunkSize);
                Inc(BytesRead, ChunkSize);
          until (TotalBytes = BytesRead);
     end;

     // #################################################
     // #### Check if upload complete
     if TotalBytes - BytesRead > 0 then
       raise EClientConnectionDropped.Create('Client Dropped Connection.'#13#10 +
         'Total Bytes indicated by Header: ' + IntToStr(TotalBytes) + #13#10 +
         'Total Bytes Read: ' + IntToStr(BytesRead));

     // #################################################
     // #### Parse buffer
     while Length(sBuffer) <> 0 do
     begin
          { Extract the Header from the ContentStream. There can be multiple "Headers"
            if multiple files are being uploaded or there are additonal form fields }
          BytesRead := Pos(HeaderTerminator, sBuffer) - 1;
          if BytesRead = -1 then
             Break;

          origHeaInfo := Copy(sBuffer, 1, BytesRead);
          HeaderInfo := UTF8String(LowerCase(String(origHeaInfo)));
          HeaderInfoLn := Length(HeaderInfo);
          Delete(sBuffer, 1, BytesRead + LnHeaderTerminator);

          FieldNameInHeader := '';
          ContentType := '';
          FileNameInHeader := '';

          { FieldNameInHeader }
          if (AnsiPos('name="', HeaderInfo) > 0) then
          begin
               FieldNameInHeader := Copy(HeaderInfo, AnsiPos('name="', HeaderInfo) + 6,
                                         HeaderInfoLn);
               Delete(FieldNameInHeader, AnsiPos('"', FieldNameInHeader), Length(FieldNameInHeader));
          end;

          { ContentType }
          if (AnsiPos('content-type: ', HeaderInfo) > 0) then
          begin
               ContentType := String(Copy(HeaderInfo, AnsiPos('content-type: ', HeaderInfo) + 14,
                                          HeaderInfoLn));
          end;

          { FileNameInHeader }
          if (AnsiPos('filename="', HeaderInfo) > 0) then
          begin
               FileNameInHeader := Copy(HeaderInfo, AnsiPos('filename="', HeaderInfo) + 10,
                                        HeaderInfoLn);
               Delete(FileNameInHeader, AnsiPos('"', FileNameInHeader), Length(FileNameInHeader));
               FileNameInHeader := UTF8String(ExtractFileName(String(FileNameInHeader)));
          end;

          { Set the HeaderDataTermininator if required }
          if (HeaderDataTerminator = '') then
             HeaderDataTerminator := #13#10 + Copy(origHeaInfo, 1, AnsiPos(#13#10, origHeaInfo) -1);

          { Extract the data and put it in sBuffer }
          BytesRead := AnsiPos(HeaderDataTerminator, sBuffer) -1;
          sValue := Copy(sBuffer, 1, BytesRead);
          Delete(sBuffer, 1, BytesRead + Length(HeaderDataTerminator));
          { sBuffer now contains the actual data }

          if (ContentType <> '') and (sValue <> '') then
          begin
               HTTPFile := TMultiPartHTTPFile.Create(Pointer(sValue)^, Length(sValue),
                                                     ContentType, String(FieldNameInHeader),
                                                     String(FileNameInHeader));
               fHTTPFiles.Add(HTTPfile);
          end
          else { Then this must be additional fields of the form }
              fContentFields.Add(String(FieldNameInHeader + '=' + sValue));
     end; { while Length(sBuffer) <> 0 do }
end;

procedure RegisterParser;
begin
     RegisterContentParser(TMultiPartContentParser);
end;

initialization
   RegisterParser;

end.
