include lib/gemdos.fth

hex
0 constant load-address
: exe-header   gemdos-header, ;
: exe-code   s" targets/m68k/tos/io.fth" included ;
: extra-bytes   gemdos-extra-bytes ;
: exe-end   gemdos-end ;
decimal
