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

#if 1
#define TRACE(XT)
#else
#define TRACE(XT)					\
  do {							\
    extern struct word tracing_word;			\
    struct word *word = XT_WORD (XT);			\
    cell *p;						\
    if (tracing_word.param[0])				\
      {							\
        for (p =  return_stack + 100 - 1; p >= RP; p--)	\
          putchar (' ');				\
        printf ("Executing %s\n", word->name);		\
      }							\
  } while (0)
#endif

static jmp_buf env;

static void signal_handler (int i)
{
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
          TRACE (xt);
          EXECUTE (xt);
        }
    }
}
