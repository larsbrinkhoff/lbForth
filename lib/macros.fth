: ppn,   number ['] literal compile, ;
finders  pp-xt  postpone, ppn, compile,
: ]]     begin next-word 2dup s" [[" compare
         while find-name pp-xt repeat 2drop ; immediate
