<?pas

Program IFSTest;

	
	procedure OnStatus(ASender: TObject;  AStatusText: AnsiString);
	begin
		echo (AStatusText);
	end;

var
	ftp: TFTP;
	sStatus: TStringList;

Begin
	ftp:= TFTP.Create();
	ftp.OnStatus := @OnStatus;
	ftp.Host := '94.73.148.47';
	ftp.Port:=21;
	ftp.Username := 'root';
	ftp.Password := 'test';
	ftp.Connect;
	ftp.Login;
	ftp.List;
	var i: integer;
	for i:= 0 to ftp.ListResult.Count -1 do
	begin
?>
<div style="background-color:yellow;width:100px"><?pas echo(ftp.ListResult[i]); ?></div>
<?pas
	end;

	ftp.Disconnect;
	ftp.free;
	
End.

?>