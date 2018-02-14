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
  PixelsPerInch = 96
  TextHeight = 13
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
      #9'obj: TObject;'
      'Begin'
      #9'SQLConnection:= TSQLConnection.Create();'
      
        #9'SQLConnection.ProviderName := '#39'MySQL'#39'; // Access, Advantage, AS' +
        'E, DB2, DBF, InterBase, MySQL, NexusDB, ODBC, Oracle, PostgreSQL' +
        ', SQL Server, SQLite, MongoDB'
      #9'SQLConnection.UserName := '#39'root'#39';'
      #9'SQLConnection.Password := '#39'toor'#39';'
      #9'SQLConnection.Server := '#39'localhost'#39';'
      #9'SQLConnection.Database := '#39'mysql'#39';'
      #9'SQLConnection.Open();'
      #9
      #9'SQLQuery:= TSQLQuery.Create();'
      #9'SQLQuery.Connection := SQLConnection;'
      #9'//SQLQuery.SQLConnection :=SQLConnection; '
      #9'SQLQuery.SQL.Text := '#39'select * from user'#39';'
      #9'SQLQuery.Open;'
      #9'while not SQLQuery.Eof do'
      #9'begin'
      
        '                               Memo1.lines.text := SQLQuery.Fiel' +
        'dByName('#39'User'#39').AsString'
      ''
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
  object PSScript1: TPSScript
    CompilerOptions = []
    OnCompile = PSScript1Compile
    OnExecute = PSScript1Execute
    OnExecImport = PSScript1ExecImport
    Plugins = <>
    UsePreProcessor = False
    Left = 688
    Top = 144
  end
  object UniConnection1: TUniConnection
    ProviderName = 'MongoDB'
    Database = 'mysql'
    Username = 'root'
    Server = 'localhost'
    LoginPrompt = False
    Left = 704
    Top = 248
    EncryptedPassword = '8BFF90FF90FF8DFF'
  end
  object UniQuery1: TUniQuery
    Connection = UniConnection1
    Left = 696
    Top = 320
  end
end
