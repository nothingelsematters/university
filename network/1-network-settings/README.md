# Console network components settings utilities in Linux and Windows OS

> Purpose of work: to get practical skills on work with ORACLE Virtual Box
> virtualization environment, on network configuration in Microsoft Windows
> and Linux operating systems, to get acquainted with command line utilities
> intended for diagnostics and configuration of a network, to develop
> executable files configuring a network interface by the set parameters,
> to get acquainted with a format of record of a way to a network resource UNC.

**Necessary**:

- ORACLE Virtual Box virtualization environment installed on your computer.
- Windows and Linux virtual hard disk images

## Procedure of work performance

### [Part 1. MS Windows](windows.bat)

1. Start virtual machine and log on to the system under administrator
    account using user name and password specified by teacher.
    Check whether the following items are active in the
    properties of the network connection being used:

      - Client for Microsoft networks
      - File and printer access service Microsoft
      - TCP/IP protocol.

    Determine the purpose of these components.

2.  Set the following parameters in the properties of the TCP/IP protocol:

    - `IP 192.168.1.10`
    - `mask 255.255.255.0`
    - `gateway 192.168.1.1`
    - `DNS 192.168.1.254`

3. Using the knowledge gained in *point 1*, configure the network interface so
   that external users could not access computer resources.
4. To understand the assignment of parameters and keys of the following utilities:
   - `ping`
   - `ipconfig`
   - `net` with directives `use`, `view`, `stop`, `start`, `share`, `config`,
     `session`, `user`, `statistics`, `localgroup`

5. Use the `netsh` utility to create command files for the
   `CMD.exe` interpreter, which could be used to configure the selected network
   interface in two ways:

      - obtain all settings through a **DHCP server** (automatically)
        (**IP**, **mask**, **gateway**, **DNS**)
      - **manual** input of all settings (statically)

    > Note: use the settings from *point 2* as network settings.

7. This task can be done in PowerShell.


### [Part 2. Linux](linux.sh)

1. Start the virtual machine and log on to the system under an
   administrator account.
2. Know how to assign parameters and keys to the `ifconfig` utility.
3. Create an executable file that configures the selected network interface
   in two ways:

      - obtain all settings through a **DHCP server** (automatically)
        (**IP**, **mask**, **gateway**, **DNS**)
      - **manual** input of all settings (statically)

        Use the following data as static settings:
        ```
        IP 172.16.10.50
        Mask 255.255.0.0
        Gateway 172.16.0.1
        DNS 172.16.255.254
        ```
