host base @ hex target
: offset   swap here - 8 - 2 rshift 00FFFFFF and ;
: jump! ( a1 a2 -- ) offset EA000000 + swap ! ;
: call! ( a1 a2 -- ) offset EB000000 + swap ! ;
: call, ( a -- ) here call!  4 allot ;
host base ! target
