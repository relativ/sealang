<?pas

Program IFSTest;
var
  i, i2: Longint;
  sList: TStringList;
Begin
	sList := TStringList.Create();
  for i := 0 to 1000000 do
  begin
    i2 := i -1;
	sList.add(inttostr(i2));
	//echo(inttostr(i2) + '<br/>');
  end;
  echo(sList.text);
  sList.free;
  echo ('bitti');
  
End.

?>