unit uPSI_djson;
{
This file has been generated by UnitParser v0.7, written by M. Knight
and updated by NP. v/d Spek and George Birbilis.
Source Code from Carlo Kok has been used to implement various sections of
UnitParser. Components of ROPS are used in the construction of UnitParser,
code implementing the class wrapper is taken from Carlo Kok's conv utility

}
interface



uses
   SysUtils
  ,Classes
  ,uPSComponent
  ,uPSRuntime
  ,uPSCompiler
  ;

type
(*----------------------------------------------------------------------------*)
  TPSImport_djson = class(TPSPlugin)
  public
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter); override;
  end;


{ compile-time registration functions }
procedure SIRegister_TJSON(CL: TPSPascalCompiler);
procedure SIRegister_TJSONListItems(CL: TPSPascalCompiler);
procedure SIRegister_TJSONItems(CL: TPSPascalCompiler);
procedure SIRegister_djson(CL: TPSPascalCompiler);

{ run-time registration functions }
procedure RIRegister_djson_Routines(S: TPSExec);
procedure RIRegister_TJSON(CL: TPSRuntimeClassImporter);
procedure RIRegister_TJSONListItems(CL: TPSRuntimeClassImporter);
procedure RIRegister_TJSONItems(CL: TPSRuntimeClassImporter);
procedure RIRegister_djson(CL: TPSRuntimeClassImporter);


implementation


uses
   djson
  ;


(* === compile-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure SIRegister_TJSON(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'TObject', 'TJSON') do
  with CL.AddClassN(CL.FindClass('TObject'),'TJSON') do
  begin
    RegisterMethod('Constructor Create( AParent : TJSON)');
    RegisterMethod('Function Parse( const AJSON : string) : TJSON');
    RegisterProperty('Parent', 'TJSON', iptr);
    RegisterProperty('IsList', 'boolean', iptr);
    RegisterProperty('IsDict', 'boolean', iptr);
    RegisterProperty('IsNull', 'boolean', iptr);
    RegisterProperty('Items', 'TJSONItems', iptr);
    RegisterProperty('ListItems', 'TJSONListItems', iptr);
    RegisterProperty('Value', 'Variant', iptr);
	RegisterProperty('Name', 'string', iptr);
    RegisterProperty('AsString', 'string', iptr);
    RegisterProperty('AsInteger', 'integer', iptr);
    RegisterProperty('AsBoolean', 'boolean', iptr);
    RegisterProperty('AsInt64', 'int64', iptr);
    RegisterProperty('AsDouble', 'double', iptr);
    RegisterProperty('AsDateTime', 'TDateTime', iptr);
    RegisterProperty('JSONByNameOrIndex', 'TJSON variant', iptr);
    SetDefaultPropery('JSONByNameOrIndex');
    RegisterProperty('_', 'TJSON variant', iptr);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TJSONListItems(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'TObject', 'TJSONListItems') do
  with CL.AddClassN(CL.FindClass('TObject'),'TJSONListItems') do
  begin
    RegisterMethod('Constructor Create');
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TJSONItems(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'TObject', 'TJSONItems') do
  with CL.AddClassN(CL.FindClass('TObject'),'TJSONItems') do
  begin
    RegisterMethod('Constructor Create');
    RegisterMethod('function Get(const name: string): TJSON');
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_djson(CL: TPSPascalCompiler);
begin
  CL.AddClassN(CL.FindClass('TOBJECT'),'TJSON');
  SIRegister_TJSONItems(CL);
  SIRegister_TJSONListItems(CL);
  SIRegister_TJSON(CL);
  CL.AddClassN(CL.FindClass('TOBJECT'),'EJSONUnknownFieldOrIndex');
  CL.AddClassN(CL.FindClass('TOBJECT'),'EJSONParseError');
 CL.AddDelphiFunction('Procedure DebugStr( const msg : variant)');
end;

(* === run-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure TJSON__R(Self: TJSON; var T: TJSON; const t1: variant);
begin T := Self._[t1]; end;

(*----------------------------------------------------------------------------*)
procedure TJSONJSONByNameOrIndex_R(Self: TJSON; var T: TJSON; const t1: variant);
begin T := Self.JSONByNameOrIndex[t1]; end;

(*----------------------------------------------------------------------------*)
procedure TJSONAsDateTime_R(Self: TJSON; var T: TDateTime);
begin T := Self.AsDateTime; end;

(*----------------------------------------------------------------------------*)
procedure TJSONAsDouble_R(Self: TJSON; var T: double);
begin T := Self.AsDouble; end;

(*----------------------------------------------------------------------------*)
procedure TJSONAsInt64_R(Self: TJSON; var T: int64);
begin T := Self.AsInt64; end;

(*----------------------------------------------------------------------------*)
procedure TJSONAsBoolean_R(Self: TJSON; var T: boolean);
begin T := Self.AsBoolean; end;

(*----------------------------------------------------------------------------*)
procedure TJSONAsInteger_R(Self: TJSON; var T: integer);
begin T := Self.AsInteger; end;

(*----------------------------------------------------------------------------*)
procedure TJSONAsString_R(Self: TJSON; var T: string);
begin T := Self.AsString; end;

(*----------------------------------------------------------------------------*)
procedure TJSONValue_R(Self: TJSON; var T: Variant);
begin T := Self.Value; end;

procedure TJSONName_R(Self: TJSON; var T: Variant);
begin T := Self.Name; end;

(*----------------------------------------------------------------------------*)
procedure TJSONListItems_R(Self: TJSON; var T: TJSONListItems);
begin T := Self.ListItems; end;

(*----------------------------------------------------------------------------*)
procedure TJSONItems_R(Self: TJSON; var T: TJSONItems);
begin T := Self.Items; end;

(*----------------------------------------------------------------------------*)
procedure TJSONIsNull_R(Self: TJSON; var T: boolean);
begin T := Self.IsNull; end;

(*----------------------------------------------------------------------------*)
procedure TJSONIsDict_R(Self: TJSON; var T: boolean);
begin T := Self.IsDict; end;

(*----------------------------------------------------------------------------*)
procedure TJSONIsList_R(Self: TJSON; var T: boolean);
begin T := Self.IsList; end;

(*----------------------------------------------------------------------------*)
procedure TJSONParent_R(Self: TJSON; var T: TJSON);
begin T := Self.Parent; end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_djson_Routines(S: TPSExec);
begin
 S.RegisterDelphiFunction(@DebugStr, 'DebugStr', cdRegister);
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TJSON(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TJSON) do
  begin
    RegisterConstructor(@TJSON.Create, 'Create');
    RegisterMethod(@TJSON.Parse, 'Parse');
    RegisterPropertyHelper(@TJSONParent_R,nil,'Parent');
    RegisterPropertyHelper(@TJSONIsList_R,nil,'IsList');
    RegisterPropertyHelper(@TJSONIsDict_R,nil,'IsDict');
    RegisterPropertyHelper(@TJSONIsNull_R,nil,'IsNull');
    RegisterPropertyHelper(@TJSONItems_R,nil,'Items');
    RegisterPropertyHelper(@TJSONListItems_R,nil,'ListItems');
    RegisterPropertyHelper(@TJSONValue_R,nil,'Value');
	RegisterPropertyHelper(@TJSONName_R,nil,'Name');
    RegisterPropertyHelper(@TJSONAsString_R,nil,'AsString');
    RegisterPropertyHelper(@TJSONAsInteger_R,nil,'AsInteger');
    RegisterPropertyHelper(@TJSONAsBoolean_R,nil,'AsBoolean');
    RegisterPropertyHelper(@TJSONAsInt64_R,nil,'AsInt64');
    RegisterPropertyHelper(@TJSONAsDouble_R,nil,'AsDouble');
    RegisterPropertyHelper(@TJSONAsDateTime_R,nil,'AsDateTime');
    RegisterPropertyHelper(@TJSONJSONByNameOrIndex_R,nil,'JSONByNameOrIndex');
    RegisterPropertyHelper(@TJSON__R,nil,'_');
  end;
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TJSONListItems(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TJSONListItems) do
  begin
    RegisterConstructor(@TJSONListItems.Create, 'Create');
  end;
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TJSONItems(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TJSONItems) do
  begin
    RegisterConstructor(@TJSONItems.Create, 'Create');
    RegisterMethod(@TJSONItems.Get, 'Get');
  end;
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_djson(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TJSON) do
  RIRegister_TJSONItems(CL);
  RIRegister_TJSONListItems(CL);
  RIRegister_TJSON(CL);
  with CL.Add(EJSONUnknownFieldOrIndex) do
  with CL.Add(EJSONParseError) do
end;



{ TPSImport_djson }
(*----------------------------------------------------------------------------*)
procedure TPSImport_djson.CompileImport1(CompExec: TPSScript);
begin
  SIRegister_djson(CompExec.Comp);
end;
(*----------------------------------------------------------------------------*)
procedure TPSImport_djson.ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter);
begin
  RIRegister_djson(ri);
  RIRegister_djson_Routines(CompExec.Exec); // comment it if no routines
end;
(*----------------------------------------------------------------------------*)


end.
