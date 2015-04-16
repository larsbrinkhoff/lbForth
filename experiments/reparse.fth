: (reparse)   >in @ >r execute r> >in ! ;
: reparse ( "name" -- ) ' postpone literal  postpone (reparse) ; immediate
