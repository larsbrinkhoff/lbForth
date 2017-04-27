\ I/O for CP/M.  Copyright 2017 Lars Brinkhoff.

code emit
  X de ld,
  2 # c ld, \ C_WRITE
  5 call,
  next,
end-code

code bye
  0 # c ld, \ P_TERMCPM
  5 jp,
end-code

code open-file
  15 # c ld, \ F_OPEN
  5 call,
  \ Also 22 F_MAKE?
  next,
end-code

code read-file
  20 # c ld, \ F_READ
  5 call,
  next,
end-code

code write-file
  21 # c ld, \ F_WRITE
  5 call,
  next,
end-code

code close-file
  16 # c ld, \ F_CLOSE
  5 call,
  next,
end-code
