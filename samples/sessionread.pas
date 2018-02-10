<?pas

Program IFSTest;
var
	text: string;
	I: integer;
Begin
	text := Session.GetValue('deger');
	echo (text + '<br/>');
	echo ('<br/>Session okundu..');
End.

?>