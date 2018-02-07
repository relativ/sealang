<?pas

Program IFSTest;
var
  i: integer;
Begin
	
	for i := 0 to 5 do
	begin
?>
	<div style="background-color:red;width:100px"><?pas echo(inttostr(i) + '. satýr'); ?></div>
	
<?pas

	end;
	
	{$I 'include-part-second.pas'}
  
End.

?>