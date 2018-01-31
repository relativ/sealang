<?pas

Program IFSTest;
var
  i, i2: Longint;
Begin
  for i := 0 to 100000 do
  begin
    i2 := i -1;
	echo(inttostr(i2) + '<br/>');
  end;
  
End.

?>