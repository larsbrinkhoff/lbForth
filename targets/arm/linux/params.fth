include lib/elf.fth

hex
08048000 constant load-address
: exe-header   load-address arm elf32-header, ;
: entry-point   elf-entry-point ;
: exe-code   s" targets/arm/linux/io.fth" included ;
: extra-bytes   elf-extra-bytes ;
: exe-end   elf-end ;
decimal
