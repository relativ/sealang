<?pas

Program IFSTest;
var
	I: integer;
	Cookie: TCookie;
Begin
	echo ('--------Cookie yazýldý ------');
	Cookie := Response.Cookies.Add;
	Cookie.Name := 'testCookie';
	Cookie.Value := 'cookie deðeri burada';
End.

?>
