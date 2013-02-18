
create facit
     3 ,  6 ,  8 , 11 , 12 , 17 , 18 , 22 , 27 , 28 ,
    31 , 33 , 36 , 41 , 45 , 47 , 52 , 56 , 57 , 58 ,

: check   facit 20 cells bounds
          do dup i @ = if dup . then /cell +loop drop ;

: keno   create 0 do , loop
         does> 5 cells bounds ." Rätt: " do i @ check /cell +loop cr ;

21 30 31 33 51 5 keno rad1
 4  6 21 31 34 5 keno rad2
16 19 62 66 70 5 keno rad3
26 34 60 62 65 5 keno rad4
10 18 32 35 36 5 keno rad5

rad1 rad2 rad3 rad4 rad5
