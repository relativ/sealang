<?pas

Program IFSTest;
Begin
		var path: string;
		path := Request.PathTranslated;
		path := Replace(path, '/','\');
		path := ExtractFilePath(path);
		var Image: TImage;
		Image:= TImage.Create(nil);
		
		Image.Picture.LoadFromFile(path + 'resim.bmp');
		Image.Canvas.Font.Size := 45;
		Image.Canvas.TextOut(190, 360, 'Merhaba Dünyaaa');
		Image.Picture.SaveToFile(path + 'resim.bmp');
		Image.Free;
?>
	<img src="resim.bmp" / >
<?pas

End.

?>