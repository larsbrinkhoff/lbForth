include lib/elf.fth

hex
08048000 constant load-address
: exe-header   load-address x86 elf32-header, ;
: entry-point   elf-entry-point ;
: exe-code   s" targets/x86/linux/io.fth" included ;
: extra-bytes   elf-extra-bytes ;
: exe-end   elf-end ;
decimal
