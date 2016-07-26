include lib/aout.fth

17000 constant load-address
hex
: exe-header   aout-header, ;
: entry-point   aout-entry-point ;
: exe-code   s" targets/pdp11/unix/io.fth" included ;
: extra-bytes   aout-extra-bytes ;
: exe-end   aout-end ;
decimal
