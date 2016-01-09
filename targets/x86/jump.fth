host base @ hex target
: rel! ( a1 a2 -- ) tuck 4 + -  swap ! ;
: jump! ( a1 a2 -- ) e9 over c!  1+ rel! ;
: call! ( a1 a2 -- ) e8 over c!  1+ rel! ;
: call, ( a -- ) here call!  5 allot ;
host base ! target
