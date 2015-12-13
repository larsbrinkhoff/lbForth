# GDB script for the Linux x86 target.

b *0x08048054
r

define s
  si
  x/1i $pc
  set $n = {char}$edx
  if $edx > 0x08048054 && $n > 0 && $n < 16
    p {char}($edx + 1) @ $n
  end
end
