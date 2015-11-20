# GDB script for the Linux x86 target.

b *0x08048054
r

define s
  si
  x/1i $pc
end
