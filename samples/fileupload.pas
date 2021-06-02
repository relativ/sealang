<?pas

Program IFSTest;
var
  strLine, tmpStr: string;
  i: integer;
  MS: TMemoryStream;

Begin

	if Request.QueryFields.Values['upload'] = 'true' then
	begin


		echo (IntToStr(Request.Files.Count));
		for i := 0 to Request.Files.Count -1 do
		begin
			echo (Request.Files.Items[i].FileName + '<br/>');
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
		<form action="fileupload.pas?upload=true" method="post" enctype="multipart/form-data">
			<input type="file" name="dosya" />
			<input type="submit" value="submit" />
		</form>
	</div>
<?pas
  
End.

?>