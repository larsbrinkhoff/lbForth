/* Copyright 2004, 2013 Lars Brinkhoff */

#include <stdio.h>
#include <signal.h>
#include <setjmp.h>
#include "forth.h"

cell word_area[10000];
cell data_stack[100];
cell return_stack[100];
char tib[256];
char fib[256];

cell *SP = data_stack + sizeof data_stack / sizeof (cell);
cell *RP = return_stack + sizeof return_stack / sizeof (cell);

static jmp_buf env;

static void signal_handler (int i)
{
  sigset_t set;
  sigemptyset (&set);
  sigaddset (&set, i);
  sigprocmask (SIG_UNBLOCK, &set, 0);
  signal (i, signal_handler);
  longjmp (env, i);
}

int
main (int argc, char **argv)
{
  extern struct word sigint_word;
  extern struct word boot_word;
  xt_t *IP = (xt_t *)boot_word.param;

  siginterrupt (SIGINT, 1);
  signal (SIGINT, signal_handler);
  siginterrupt (SIGSEGV, 1);
  signal (SIGSEGV, signal_handler);

  for (;;)
    {
      if (setjmp (env))
        EXECUTE (&sigint_word);

      for (;;)
        {
          xt_t xt = NEXT_XT;
          EXECUTE (xt);
        }
    }
}
