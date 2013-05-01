/* Copyright 2004, 2013 Lars Brinkhoff */

#include "forth.h"

cell dictionary[10000];
cell data_stack[100];
cell return_stack[100];
char tib[256];
char fib[256];

cell *SP = data_stack + sizeof data_stack / sizeof (cell);
cell *RP = return_stack + sizeof return_stack / sizeof (cell);

jmp_buf env;
