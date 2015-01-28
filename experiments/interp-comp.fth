\ Make compile-only words if, do, ?do, and begin work in interpreted
\ mode by temporarily switching to compile mode.

variable a
variable n
: here!   here - allot ;
: start   here a !  depth n !  ] ;
: stop   n @ depth = if postpone exit postpone  [ a @ dup here! >r then ;
: comp'   state @  ] ' postpone [  swap state! ;
: start:   >in @ comp' swap >in !  create ,  does> start perform ;
: stop:   >in @ comp' swap >in !  create compile-only ,  does> perform stop ;
start: if  start: do  start: ?do  start: begin
stop: then  stop: loop  stop: +loop  stop: until  stop: repeat
