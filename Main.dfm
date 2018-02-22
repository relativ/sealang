object PascalModule: TPascalModule
  OldCreateOrder = True
  OnCreate = WebModuleCreate
  OnDestroy = WebModuleDestroy
  Actions = <
    item
      Default = True
      Name = 'DefaultHandler'
      PathInfo = '/*'
      OnAction = PascalModuleDefaultHandlerAction
    end>
  OnException = WebModuleException
  Height = 411
  Width = 613
  object PSImport_Classes: TPSImport_Classes
    EnableStreams = True
    EnableClasses = True
    Left = 104
    Top = 8
  end
  object PSImport_DateUtils: TPSImport_DateUtils
    Left = 200
    Top = 8
  end
  object PSImport_ComObj: TPSImport_ComObj
    Left = 304
    Top = 8
  end
  object PSImport_DB: TPSImport_DB
    Left = 104
    Top = 56
  end
  object PSImport_Controls: TPSImport_Controls
    EnableStreams = True
    EnableGraphics = True
    EnableControls = True
    Left = 200
    Top = 56
  end
  object PSImport_StdCtrls: TPSImport_StdCtrls
    EnableExtCtrls = True
    EnableButtons = True
    Left = 304
    Top = 57
  end
end
