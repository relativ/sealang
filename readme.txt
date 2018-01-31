PascalWeb is an opensource application for creating dynamic web pages like as php, nodejs, python.

Configuring


Put that lines to apache httpd.conf :

LoadModule pascal_module "${INSTALL_DIR}/bin/pascal.net/mod_pascal.dll" 

<IfModule mime_module>
    AddType application/x-httpd-pas .pas
	AddHandler mod_pascal-handler .pas

</IfModule>

Thats it !