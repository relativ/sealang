Sealang is an opensource event based software language for creating dynamic web pages like as php, nodejs, python.

Configuring


Put that lines to apache httpd.conf :

LoadModule sealang_module "${INSTALL_DIR}/bin/sealang/sealang.dll" 

<IfModule mime_module>
    AddType application/x-httpd-pas .pas
	AddHandler sealang-handler .pas

</IfModule>

Thats it !

Contact : srkgns@gmail.com
