\ From "Thinking Forth".

variable marker
: doer   create ['] noop >body ,  does> @ >r ;
: (make)   r> dup cell+  dup cell+ swap @ >body !  @ ?dup if >r then ;
: make   ' >body ! postpone [ !csp ;
: make   postpone (make)  here marker ! 0 , ; compile-only
: ;and   postpone exit  here marker @ ! ; immediate
: undo   ['] noop >body  ' >body ! ;

doer why?
: recital
   cr ." Your daddy is standing on the table.  Ask him 'WHY?' "
    make why? ." To change the light bulb."
  begin
  make why? ." Because it's burned out."
  make why? ." Because it was old."
  make why? ." Because we put it in there a long time ago."
  make why? ." Because it was dark!"
  make why? ." Because it was night time!!"
  make why? ." Stop saying WHY?"
  make why? ." Because it's driving me crazy."
  make why? ." Just let me change this light bulb!"
  again ;
: why?   cr why? quit ;
