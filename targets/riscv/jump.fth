host base @ hex target
: field   >r >r over r> lshift r> and + ;
: offset   swap here - 0 014 7FE00000 field
                         009 00100000 field
                         000 000FF000 field
                         00B 80000000 field nip ;
: jump! ( a1 a2 -- ) offset 0000006F + swap ! ;
: call! ( a1 a2 -- ) offset 000000EF + swap ! ;
: call, ( a -- ) here call!  4 allot ;
host base ! target
