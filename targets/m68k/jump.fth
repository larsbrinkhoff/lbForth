host base @ hex target
0 [if]
: jump! ( a1 a2 -- ) 4EF9 over h!  2 + ! ;
: call! ( a1 a2 -- ) 4EB9 over h!  2 + ! ;
: call, ( a -- ) here call!  6 allot ;
[else]
: rel! ( a1 a2 -- ) tuck - swap h! ;
: jump! ( a1 a2 -- ) 6000 over h!  2 + rel! ;
: call! ( a1 a2 -- ) 6100 over h!  2 + rel! ;
: call, ( a -- ) here call!  4 allot ;
[then]
host base ! target
