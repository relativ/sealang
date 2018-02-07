unit ExternalFunctions;

interface

uses
   SysUtils
  ,Classes
  , uPSCompiler, uPSRuntime, uPSDisassembly, uPSPreprocessor, uPSUtils,
  Menus, uPSC_comobj, uPSR_comobj, uPSComponent, uPSC_dateutils, uPSI_HTTPApp;


  function Replace(const S, OldPattern, NewPattern: string): string;

  procedure ImplementFucntions(PS: TPSScript);
implementation


function Replace(const S, OldPattern, NewPattern: string): string;
begin
  result := StringReplace(S, OldPattern, NewPattern, [rfReplaceAll]);
end;

procedure ImplementFucntions(PS: TPSScript);
begin
  PS.AddFunction(@Replace, 'function Replace(const S, OldPattern, NewPattern: string): string;');
  PS.AddFunction(@ExtractFilePath, 'function ExtractFilePath(const FileName: string): string;');
  PS.AddFunction(@ExtractFileDir, 'function ExtractFileDir(const FileName: string): string;');
  PS.AddFunction(@ExtractFileName, 'function ExtractFileName(const FileName: string): string;');
  PS.AddFunction(@ExtractFileExt, 'function ExtractFileExt(const FileName: string): string;');
end;


end.
