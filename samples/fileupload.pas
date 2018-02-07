<?pas

Program IFSTest;
var
  strLine, tmpStr: string;
  i: integer;
  MS: TMemoryStream;
Begin
	if Request.QueryFields.Values['upload'] <> 'true' then
	begin
		strLine := 'merhaba<pas writeln("dünya"); ? > alooo';
		tmpStr := copy(strLine, Pos('<pas',strLine) + 4, Length(strLine));
		Delete(tmpStr, 1, Pos('? >',tmpStr) + 2 );
		echo(tmpStr);
		Response.Content := Request.PathTranslated;
	end else begin
		
		for i := 0 to Request.Files.Count -1 do
		begin
			MS:= TMemoryStream.Create();
			MS.LoadFromStream(Request.Files.Items[i].Stream);
			MS.SaveToFile('C:\wamp64\www\' + Request.Files.Items[i].FileName);
			MS.Free;
			//Response.ContentStream := Request.Files.Items[i].Stream;
		end;
		echo ('file uploaded.');
	end;
	
?>
	<div style="background-color:red">
		<form action="test.pas?upload=true" method="post" enctype="multipart/form-data">
			<input type="file" name="dosya" />
			<input type="submit" value="submit" />
		</form>
	</div>
<?pas
  
End.

?>