## File Systems

***

## Features

+ **std::begin**, **std::end**

+ **cbegin** and **cend** (const begin and const end) wasn't included in namespace std

+ move semantic in lambda functions:

```cpp
[a = std::move(a)] {...}
```

+ No std::move in initializer_list -> copies twice

***

## Dynamic libraries loading

W/dynamic libraries an extra parameter is given: where it has been loaded

2 options are available in order to fix that:

1. All global variables are stored in **relocation table**

 dll hase **base address** - preferred address to be loaded // windows

 relocation: `loc += x - base_address`

 dll loaded w/base address is shared between all the processes in os, otherwise
 it is unique for each process

 **GOT** (Global Offset Table) // linux

2. it is able to "jump" in reference

 **POS** (Position Independent Code)
