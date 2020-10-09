@ECHO OFF
echo Setting TCP/IP interface by DHCP
netsh interface ipv4 set address "Ethernet" dhcp
netsh interface ipv4 set dns "Ethernet" dhcp



@ECHO OFF
echo Setting TCP/IP interface by static
netsh interface ipv4 set address name="Ethernet" source=static addr=192.168.1.10 mask=255.255.255.0 gateway=192.168.1.1
netsh interface ipv4 set dnsserver name="Ethernet" source=static addr=192.168.1.254
