\ Empty; the C target doesn't mess with machine instructions.

\ JUMP! ( a1 a2 -- ) Write a jump (or call) instruction to the code
\ field at a2.  Used for defining words and ;CODE.

\ CALL! ( a1 a2 -- ) Write a call (or jump) instruction to the code
\ field at a2.  Used for DOES> child.

\ CALL, ( a -- ) Lay down a call instruction in the dictionary.  Used
\ for DOES> parent.
