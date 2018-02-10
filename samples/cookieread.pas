<?pas

Program IFSTest;
var
	I: integer;
Begin
	echo('cookie list <br>');
	for I := 0 to Cookies.Count -1 do
	  begin
		
		echo(Cookies[I]+ '<br/>');
	  end;
End.

?>

