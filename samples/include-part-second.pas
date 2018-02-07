for i := 0 to 5 do
begin
?>
	<div style="background-color:green;width:100px"><?pas echo(inttostr(i) + '. satýr'); ?></div>
<?pas
end;

{$I    'include-part-third.pas'}