# GDB script for the Linux x86 target.

b *0x804AA18
r

define forth-word
  if $arg0 > 0x08048054 && $arg0 < 0x08100000
    set $offset = {char}($arg0 - 5)
    if $offset >= 8 && $offset < 64
      set $nfa = $arg0 - $offset
      set $n = {char}$nfa
      if $n > 0 && $n < 16
        p {char}($nfa + 1) @ $n
      end
    end
  end
end

define forth-step
  while {char}$pc != 0xFF && {char}($pc+1) != 0x22
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
