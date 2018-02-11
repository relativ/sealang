library mod_pascal;

uses
  Winapi.ActiveX,
  System.Win.ComObj,
  Web.WebBroker,
  Web.ApacheApp,
  Web.HTTPD24Impl,
  Main in 'Main.pas' {PascalModule: TWebModule},
  PascalScript_Core_Ext_Reg in 'compiler\Source\PascalScript_Core_Ext_Reg.pas',
  PascalScript_Core_Reg in 'compiler\Source\PascalScript_Core_Reg.pas',
  PascalScript_Core_Reg_noDB in 'compiler\Source\PascalScript_Core_Reg_noDB.pas',
  uPSC_buttons in 'compiler\Source\uPSC_buttons.pas',
  uPSC_classes in 'compiler\Source\uPSC_classes.pas',
  uPSC_comobj in 'compiler\Source\uPSC_comobj.pas',
  uPSC_controls in 'compiler\Source\uPSC_controls.pas',
  uPSC_dateutils in 'compiler\Source\uPSC_dateutils.pas',
  uPSC_DB in 'compiler\Source\uPSC_DB.pas',
  uPSC_dll in 'compiler\Source\uPSC_dll.pas',
  uPSC_extctrls in 'compiler\Source\uPSC_extctrls.pas',
  uPSC_forms in 'compiler\Source\uPSC_forms.pas',
  uPSC_graphics in 'compiler\Source\uPSC_graphics.pas',
  uPSC_menus in 'compiler\Source\uPSC_menus.pas',
  uPSC_std in 'compiler\Source\uPSC_std.pas',
  uPSC_stdctrls in 'compiler\Source\uPSC_stdctrls.pas',
  uPSCompiler in 'compiler\Source\uPSCompiler.pas',
  uPSComponent in 'compiler\Source\uPSComponent.pas',
  uPSComponent_COM in 'compiler\Source\uPSComponent_COM.pas',
  uPSComponent_Controls in 'compiler\Source\uPSComponent_Controls.pas',
  uPSComponent_DB in 'compiler\Source\uPSComponent_DB.pas',
  uPSComponent_Default in 'compiler\Source\uPSComponent_Default.pas',
  uPSComponent_Forms in 'compiler\Source\uPSComponent_Forms.pas',
  uPSComponent_StdCtrls in 'compiler\Source\uPSComponent_StdCtrls.pas',
  uPSComponentExt in 'compiler\Source\uPSComponentExt.pas',
  uPSDebugger in 'compiler\Source\uPSDebugger.pas',
  uPSDisassembly in 'compiler\Source\uPSDisassembly.pas',
  uPSPreProcessor in 'compiler\Source\uPSPreProcessor.pas',
  uPSR_buttons in 'compiler\Source\uPSR_buttons.pas',
  uPSR_classes in 'compiler\Source\uPSR_classes.pas',
  uPSR_comobj in 'compiler\Source\uPSR_comobj.pas',
  uPSR_controls in 'compiler\Source\uPSR_controls.pas',
  uPSR_dateutils in 'compiler\Source\uPSR_dateutils.pas',
  uPSR_DB in 'compiler\Source\uPSR_DB.pas',
  uPSR_dll in 'compiler\Source\uPSR_dll.pas',
  uPSR_extctrls in 'compiler\Source\uPSR_extctrls.pas',
  uPSR_forms in 'compiler\Source\uPSR_forms.pas',
  uPSR_graphics in 'compiler\Source\uPSR_graphics.pas',
  uPSR_menus in 'compiler\Source\uPSR_menus.pas',
  uPSR_std in 'compiler\Source\uPSR_std.pas',
  uPSR_stdctrls in 'compiler\Source\uPSR_stdctrls.pas',
  uPSRuntime in 'compiler\Source\uPSRuntime.pas',
  uPSUtils in 'compiler\Source\uPSUtils.pas',
  uPSI_HTTPApp in 'utils\uPSI_HTTPApp.pas',
  ExternalFunctions in 'utils\ExternalFunctions.pas',
  MultipartParser in 'utils\MultipartParser.pas',
  MVCFramework.Session in 'utils\MVCFramework.Session.pas',
  SessionUnit in 'library\SessionUnit.pas',
  uPSI_SqlExpr in 'library\uPSI_SqlExpr.pas';

{$R *.res}

// httpd.conf entries:
//
(*
 LoadModule pascal_module modules/mod_pascal.dll

 <Location /xyz>
    SetHandler mod_pascal-handler
 </Location>
*)
//
// These entries assume that the output directory for this project is the apache/modules directory.
//
// httpd.conf entries should be different if the project is changed in these ways:
//   1. The TApacheModuleData variable name is changed
//   2. The project is renamed.
//   3. The output directory is not the apache/modules directory
//

// Declare exported variable so that Apache can access this module.
var
  GModuleData: TApacheModuleData;
exports
  GModuleData name 'pascal_module';

begin
  CoInitFlags := COINIT_MULTITHREADED;
  Web.ApacheApp.InitApplication(@GModuleData);

  Application.Initialize;
  Application.WebModuleClass := WebModuleClass;
  Application.Run;
end.
