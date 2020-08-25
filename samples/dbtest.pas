<?pas

Program IFSTest;
var
	SQLConnection: TDBConnection;
	SQLQuery: TDBQuery;
Begin
	SQLConnection:= TDBConnection.Create();
	SQLConnection.ProviderName := 'MySQL'; // Access, Advantage, ASE, DB2, DBF, InterBase, MySQL, NexusDB, ODBC, Oracle, PostgreSQL, SQL Server, SQLite, MongoDB
	SQLConnection.UserName := 'root';
	SQLConnection.Password := '';
	SQLConnection.Server := 'localhost';
	SQLConnection.Database := 'mysql';
	SQLConnection.Open();
	
	SQLQuery:= TDBQuery.Create();
	SQLQuery.Connection := SQLConnection;
	SQLQuery.SQL.Text := 'select * from user';
	SQLQuery.Open;
	while not SQLQuery.Eof do
	begin
?>
<div style="background-color:yellow;width:100px"><?pas echo(SQLQuery.FieldByNameAsString('User')) ?></div>
<?pas
		SQLQuery.Next;
	end;
	
	SQLQuery.Close();
	SQLQuery.Free();
	
	SQLConnection.Close();
	SQLConnection.free();
	
End.

?>