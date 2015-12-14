# GDB script for the Linux x86 target.

b *0x08048054
r

define forth-word
  if $arg0 > 0x08048054 && $arg0 < 0x08100000
    set $n = {char}$arg0
    if $n > 0 && $n < 16
      p {char}($arg0 + 1) @ $n
    end
  end
end

define forth-step
  while {char}$pc != 0xFF && {char}($pc+1) != 0x62 && {char}($pc+2) != 0x18
    si
  end
  si
  forth-word $edx
end

define s
  si
  x/1i $pc
  forth-word $edx
end
