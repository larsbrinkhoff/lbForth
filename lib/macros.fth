: ppn,   number ['] literal compile, ;
finders  pp-name  postpone, ppn, compile,
: ]]     begin next-word 2dup s" [[" compare
         while pp-name repeat 2drop ; immediate
