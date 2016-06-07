\ This implements simple cooperative multitasking.
\
\ TASK ( "name" -- ) Create a new task.
\ ACTIVATE ( a -- ) Make task a active; continue running in this definition.
\ PAUSE ( -- ) Switch to run the next active task.
\ .TASKS ( -- ) Print a list of all active tasks.

64 cells constant /data-stack
64 cells constant /return-stack

2 cells /data-stack + /return-stack + constant /task


variable current-task
create root-task  here current-task !  here ,  here ,

: >next-task ;
: next-task   current-task @ >next-task ;

: >rp   cell+ ;
: task-rp   current-task @ >rp ;

: >data-stack   2 cells + /data-stack + ;

: task ( "name" -- ) create here >rp /task allot here 2 cells - swap ! ;
: switch   next-task @ current-task ! ;
: pause   sp@ >r  rp@ task-rp !  switch  task-rp @ rp!  r> sp! ;

: insert-task ( a  -- ) next-task @ over !  next-task ! ;
: 0sp ( a -- a ) dup >data-stack  over /task + 2 cells - ! ;
: 0rp ( a -- ) over /task + cell - ! ;
: activate ( a -- ) r> 0rp  0sp  insert-task ;

: .task   current-task @ dup . dup cell - id. ." @ " >rp @ cell+ @ .' cr ;
: end?   dup current-task @ = ;
: .tasks   current-task @ begin .task switch end? until drop ;

quit

\ Test.
task foo
: bar foo activate begin ." BAR" pause again ;
: baz   100 0 do i . pause loop ;
bar 
baz
