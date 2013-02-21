/* Copyright 2004, 2013 Lars Brinkhoff */

#include <stdio.h>
#include <signal.h>
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

static volatile sig_atomic_t signal_raised = 0;

static void signal_handler (int i)
{
  signal_raised = i;
}

int
main (int argc, char **argv)
{
  extern struct word boot_word;
  xt_t *IP = (xt_t *)boot_word.param;

  siginterrupt (SIGINT, 1);
  signal (SIGINT, signal_handler);

  for (;;)
    {
      xt_t xt = NEXT_XT;
      TRACE (xt);
      EXECUTE (xt);

      if (signal_raised)
	{
	  extern xt_t *enter_code (xt_t *, struct word *);
	  extern struct word sigint_word;
	  signal_raised = 0;
	  EXECUTE (&sigint_word);
	}
    }
}
