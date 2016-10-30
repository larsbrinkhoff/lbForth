\ -*- forth -*- Copyright 2013, 2015-2017 Lars Brinkhoff

code cold \ int main (void)
  extern struct word dp0_word, sp0_word, rp0_word, SP_word, RP_word,
    limit_word, latest0_word, turnkey_word;
  static cell data_stack[110];
  static cell return_stack[256];
  static cell dictionary[19000];
  xt_t *IP = 0, xt = &turnkey_word;

  sp0_word.param[0] = (cell)(&data_stack[100]);
  rp0_word.param[0] = (cell)(&return_stack[256]);
  dp0_word.param[0] = (cell)dictionary;
  limit_word.param[0] = (cell)dictionary + sizeof dictionary;
  latest0_word.param[0] = (cell)(&turnkey_word);

  SP_word.param[0] = sp0_word.param[0];
  RP_word.param[0] = rp0_word.param[0];

  for (;;)
    {
      EXECUTE (xt);
      xt = NEXT_XT;
    }

  return 0;
end-code

code exit ( R: ret -- )
    RPOP (xt_t *, x);
    IP = x;
end-code

code dodoes ( -- addr ) ( R: -- ret )
    PUSH (word->param);
    RPUSH (IP);
    IP = (xt_t *)(word->does);
end-code

\ Possible, but slow, implementation of 0branch.
\ : select   0= dup invert swap rot nand invert rot rot nand invert + ;
\ : 0branch   r> dup cell+ swap @ rot select >r ;

code 0branch ( x -- )
    xt_t *addr = *(xt_t **)IP;
    POP (cell, x);
    if (!x)
      IP = addr;
    else
      IP++;
end-code

\ This works, but is too slow.
\ create 'cell   cell ,
\ variable temp
\ : (literal)   r> temp ! temp @ temp @ 'cell @ + >r @ ;

code (literal) ( -- n )
    PUSH (*(cell *)IP);
    IP++;
end-code

code ! ( x addr -- )
    POP (cell *, addr);
    POP (cell, x);
    *addr = x;
end-code

code @ ( addr -- x )
    POP (cell *, addr);
    PUSH (*addr);
end-code

\ : +   begin ?dup while 2dup xor -rot and 2* repeat ;

code + ( x y -- x+y )
    POP (cell, x);
    POP (cell, y);
    PUSH (x + y);
end-code

\ This works, but is too slow.
\ : >r   r@ rp@ -4 + rp! rp@ ! rp@ 4 + ! ;

code >r  ( x -- ) ( R: -- x )
    POP (cell, x);
    RPUSH (x);
end-code

\ This works, but is too slow.
\ : r>   rp@ 4 + @ r@ rp@ 4 + rp! rp@ ! ;

code r> ( -- x ) ( R: x -- )
    RPOP (cell, x);
    PUSH (x);
end-code

code nand ( x y -- ~(x&y) )
    POP (cell, x);
    POP (cell, y);
    PUSH (~(x & y));
end-code

code c! ( c addr -- )
    POP (char_t *, addr);
    POP (cell, c);
    *addr = c;
end-code

code c@ ( addr -- c )
    POP (uchar_t *, addr);
    PUSH (*addr);
end-code

code emit ( c -- )
    POP (cell, c);
    putchar (c);
end-code

code bye ( ... -- <no return> )
    exit (0);
end-code

code close-file ( fileid -- ior )
    POP (FILE *, fileid);
    PUSH (fclose (fileid) == 0 ? 0 : errno);
end-code

code open-file ( addr u mode -- fileid ior )
    char *name;
    int i;
    FILE *fileid;

    POP (char *, mode);
    POP (cell, u);
    POP (char_t *, addr);
    name = malloc (u + 1);

    for (i = 0; i < u; i++)
      name[i] = *addr++;
    name[i] = 0;
    fileid = fopen (name, mode);
    free (name);
    PUSH (fileid);
    PUSH (fileid == 0 ? errno : 0 );
end-code

code read-file ( addr u1 fileid -- u2 ior )
    static char buffer[1024];
    size_t u2;
    int i;

    POP (FILE *, fileid);
    POP (cell, u1);
    POP (char_t *, addr);

    if (fileid == 0)
      fileid = stdin;
    if (u1 > sizeof buffer)
      u1 = sizeof buffer;
    u2 = fread (buffer, 1, u1, fileid);
    for (i = 0; i < u2; i++)
      addr[i] = buffer[i];
    PUSH (u2);
    PUSH (ferror (fileid) ? errno : 0);
end-code
