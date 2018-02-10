<?pas

var b: integer;

for b := 0 to 5 do
begin
?>
	<div style="background-color:green;width:100px"><?pas echo(inttostr(b) + '. satýr'); ?></div>
<?pas
end;

{$I    'include-part-third.pas'}

?>