<?pas

Program IFSTest;
var
	SQLConnection: TSQLConnection;
	SQLQuery: TSQLQuery;
Begin
	SQLConnection:= TSQLConnection.Create();
	SQLConnection.ProviderName := 'MySQL'; // Access, Advantage, ASE, DB2, DBF, InterBase, MySQL, NexusDB, ODBC, Oracle, PostgreSQL, SQL Server, SQLite, MongoDB
	SQLConnection.UserName := 'root';
	SQLConnection.Password := 'toor';
	SQLConnection.Server := 'localhost';
	SQLConnection.Database := 'mysql';
	SQLConnection.Open();
	
	SQLQuery:= TSQLQuery.Create();
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