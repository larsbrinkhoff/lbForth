include lib/pe.fth

hex
400000 constant load-address
: exe-header   pe-header, ;
: entry-point   pe-entry ;
: exe-code   pe-code 401000 org  s" targets/x86/windows/io.fth" included ;
: extra-bytes   pe-extra-bytes ;
: exe-end   pe-end ;
decimal
