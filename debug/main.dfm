object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 475
  ClientWidth = 777
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 704
    Top = 208
    Width = 105
    Height = 105
  end
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 635
    Height = 459
    Lines.Strings = (
      'Program IFSTest;'
      'var'
      #9'SQLConnection: TSQLConnection;'
      #9'SQLQuery: TSQLQuery;'
      'Begin'
      #9'SQLConnection:= TSQLConnection.Create();'
      
        #9'SQLConnection.ProviderName := '#39'MySQL'#39'; // Access, Advantage, AS' +
        'E, DB2, DBF, InterBase, MySQL, NexusDB, ODBC, Oracle, PostgreSQL' +
        ', SQL Server, SQLite, MongoDB'
      #9'SQLConnection.UserName := '#39'root'#39';'
      #9'SQLConnection.Password := '#39'toor'#39';'
      #9'SQLConnection.Server := '#39'localhost'#39';'
      #9'SQLConnection.Database := '#39'mysql'#39'; '
      #9'SQLConnection.Open();'
      #9
      #9'SQLQuery:= TSQLQuery.Create();'
      #9'SQLQuery.Connection := SQLConnection;'
      #9'SQLQuery.SQL.Text := '#39'select * from user'#39';'
      #9'SQLQuery.Open;'
      #9'while not SQLQuery.Eof do'
      #9'begin'
      
        '                                Memo1.Lines.Add(SQLQuery.FieldBy' +
        'NameAsString('#39'User'#39')) ;'
      #9#9'SQLQuery.Next;'
      #9'end;'
      #9
      #9'SQLQuery.Close();'
      #9'SQLQuery.Free();'
      #9
      #9'SQLConnection.Close();'
      #9'SQLConnection.free();'
      #9
      'End.')
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Button2: TButton
    Left = 672
    Top = 23
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button1: TButton
    Left = 672
    Top = 95
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 2
    OnClick = Button1Click
  end
  object PSScript1: TPSScript
    CompilerOptions = []
    OnCompile = PSScript1Compile
    OnExecute = PSScript1Execute
    OnExecImport = PSScript1ExecImport
    Plugins = <
      item
        Plugin = PSDllPlugin1
      end
      item
        Plugin = PSImport_Classes1
      end
      item
        Plugin = PSImport_ComObj1
      end
      item
        Plugin = PSImport_Controls1
      end
      item
        Plugin = PSImport_DateUtils1
      end
      item
        Plugin = PSImport_StdCtrls1
      end
      item
        Plugin = PSImport_DB1
      end>
    UsePreProcessor = False
    Left = 688
    Top = 144
  end
  object PSDllPlugin1: TPSDllPlugin
    Left = 384
    Top = 240
  end
  object PSImport_Classes1: TPSImport_Classes
    EnableStreams = True
    EnableClasses = True
    Left = 472
    Top = 296
  end
  object PSImport_DateUtils1: TPSImport_DateUtils
    Left = 552
    Top = 280
  end
  object PSImport_ComObj1: TPSImport_ComObj
    Left = 640
    Top = 280
  end
  object PSImport_DB1: TPSImport_DB
    Left = 464
    Top = 240
  end
  object PSImport_Controls1: TPSImport_Controls
    EnableStreams = True
    EnableGraphics = True
    EnableControls = True
    Left = 544
    Top = 232
  end
  object PSImport_StdCtrls1: TPSImport_StdCtrls
    EnableExtCtrls = True
    EnableButtons = True
    Left = 640
    Top = 224
  end
  object IdTCPClient1: TIdTCPClient
    ConnectTimeout = 0
    IPVersion = Id_IPv4
    Port = 0
    ReadTimeout = -1
    Left = 696
    Top = 368
  end
  object IdUDPClient1: TIdUDPClient
    Port = 0
    Left = 608
    Top = 376
  end
end
