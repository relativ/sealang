object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 435
  ClientWidth = 777
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 513
    Height = 409
    Lines.Strings = (
      'Program IFSTest;'
      'var'
      '  e: variant;'
      'Begin'
      '  e := null;'
      '  case VarType(e) of'
      #9'varempty :writeln('#39'unassigned'#39');'
      #9'varNull: Writeln('#39'null'#39');'
      #9'varstring: Writeln('#39'String'#39');'
      #9'varInteger : writeln('#39'VarInteger'#39');'
      #9'varSingle: Writeln('#39'Single'#39');'
      #9'varDouble: Writeln('#39'Double'#39');'
      '  end;'
      'End.')
    TabOrder = 0
  end
  object Button1: TButton
    Left = 568
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 568
    Top = 47
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 2
    OnClick = Button2Click
  end
  object SQLQuery1: TSQLQuery
    MaxBlobSize = -1
    Params = <>
    SQL.Strings = (
      'select * from user')
    SQLConnection = SQLConnection1
    Left = 688
    Top = 264
  end
  object SQLDataSet1: TSQLDataSet
    Params = <>
    Left = 616
    Top = 232
  end
  object SQLTable1: TSQLTable
    Left = 600
    Top = 296
  end
  object PSScript1: TPSScript
    CompilerOptions = []
    Plugins = <
      item
        Plugin = PSImport_Classes1
      end
      item
        Plugin = PSImport_DB1
      end>
    UsePreProcessor = False
    Left = 688
    Top = 40
  end
  object PSImport_DB1: TPSImport_DB
    Left = 688
    Top = 96
  end
  object PSImport_Classes1: TPSImport_Classes
    EnableStreams = True
    EnableClasses = True
    Left = 600
    Top = 104
  end
  object SQLConnection1: TSQLConnection
    DriverName = 'MySQL'
    LoginPrompt = False
    Params.Strings = (
      'DriverUnit=Data.DBXMySQL'
      
        'DriverPackageLoader=TDBXDynalinkDriverLoader,DbxCommonDriver230.' +
        'bpl'
      
        'DriverAssemblyLoader=Borland.Data.TDBXDynalinkDriverLoader,Borla' +
        'nd.Data.DbxCommonDriver,Version=23.0.0.0,Culture=neutral,PublicK' +
        'eyToken=91d62ebb5b0d1b1b'
      
        'MetaDataPackageLoader=TDBXMySqlMetaDataCommandFactory,DbxMySQLDr' +
        'iver230.bpl'
      
        'MetaDataAssemblyLoader=Borland.Data.TDBXMySqlMetaDataCommandFact' +
        'ory,Borland.Data.DbxMySQLDriver,Version=23.0.0.0,Culture=neutral' +
        ',PublicKeyToken=91d62ebb5b0d1b1b'
      'GetDriverFunc=getSQLDriverMYSQL'
      'LibraryName=dbxmys.dll'
      'LibraryNameOsx=libsqlmys.dylib'
      'VendorLib=LIBMYSQL.dll'
      'VendorLibWin64=libmysql.dll'
      'VendorLibOsx=libmysqlclient.dylib'
      'HostName=localhost'
      'Database=mysql'
      'User_Name=root'
      'Password='
      'MaxBlobSize=-1'
      'LocaleCode=0000'
      'Compressed=False'
      'Encrypted=False'
      'BlobSize=-1'
      'ErrorResourceFile=')
    Left = 672
    Top = 184
  end
end
