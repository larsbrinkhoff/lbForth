\ 17 constant buckets
65536 constant buckets
create array  buckets cells allot
: 0array   array buckets cells erase ;
: .array   buckets 0 do i . i cells array + @ . cr loop ;
: array+   buckets u/mod drop cells array + 1 swap +! ;

variable hash

: 0fnv1a   2166136261 hash ! ;
: fnv1a+    hash @ xor  16777619 *  hash ! ;
: fnv1a-hash   0fnv1a bounds ?do i c@ fnv1a+ loop ;

: rot3   dup 3 lshift swap 29 rshift + ;

: 0tcl   0 hash ! ;
: tcl+   hash @  over xor rot3 over xor rot3 rot3 xor  hash ! ;
: tcl-hash   0tcl bounds ?do i c@ tcl+ loop ;

\ : 0crc16   -1 hash ! ;
hex : 0crc16   1D0F hash ! ; decimal
: bit   dup 1 rshift swap 1 and if 33800 xor then ;
: crc16+   hash @ xor  8 0 do bit loop  hash ! ;
: crc16-hash   0crc16 bounds ?do i c@ crc16+ loop ;

defer function
: fold   dup 16 rshift xor 65535 and ;
: sum   >name function hash @ fold array+ -1 ;
: >fn   ." TESTING: " ' dup >name type cr is function ;
: test   >fn 0array ['] forth ['] sum traverse-wordlist ;

\ test fnv1a-hash .array
\ test tcl-hash .array
test crc16-hash .array

hex
s" " crc16-hash hex hash @ ffff and u. \ 1D0F
s" A" crc16-hash hex hash @ ffff and u. \ 9479
s" 123456789" crc16-hash hex hash @ ffff and u. \ E5CC
.