<?pas

Program IFSTest;
var
	SQLConnection: TSQLConnection;
	SQLQuery: TSQLQuery;
Begin
	try
	SQLConnection:= TSQLConnection.Create();
	SQLConnection.ProviderName := 'MySQL'; // Access, Advantage, ASE, DB2, DBF, InterBase, MySQL, NexusDB, ODBC, Oracle, PostgreSQL, SQL Server, SQLite, MongoDB
	SQLConnection.UserName := 'root';
	SQLConnection.Password := 'toor';
	SQLConnection.Server := 'localhost';
	SQLConnection.Database := 'mysql';
	SQLConnection.Open();
	except
		//echo(GetExceptionMessage);
	end;
	
	{SQLQuery:= TSQLQuery.Create();
	SQLQuery.Connection := SQLConnection;
	//SQLQuery.SQLConnection :=SQLConnection; 
	SQLQuery.SQL.Text := 'select * from user';
	SQLQuery.Open;
	while not SQLQuery.Eof do
	begin
?>
	<div style="background-color: yellow"><?pas echo(SQLQuery.FieldByName('User').AsString); ?></div>
<?pas

		SQLQuery.Next;
	end;
	
	SQLQuery.Close();
	SQLQuery.Free();}
	
	SQLConnection.Close();
	SQLConnection.free();
	
End.

?>