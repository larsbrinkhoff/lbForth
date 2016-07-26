host base @ hex target
: jump! ( a1 a2 -- ) 005F over !  2 + ! ;
: call! ( a1 a2 -- ) 081F over !  2 + ! ;
: call, ( a -- ) here call!  4 allot ;
host base ! target
