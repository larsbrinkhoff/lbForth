#include <stdio.h>
#include "forth.h"

cell word_area[10000];
cell data_stack[100];
cell return_stack[100];
char tib[100];
char fib[100];

xt_t *IP;
cell *SP = data_stack + 100;
cell *RP = return_stack + 100;

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

int
main (int argc, char **argv)
{
  extern struct word boot_word;
  IP = (xt_t *)boot_word.param;

  for (;;)
    {
      xt_t xt = NEXT_XT;
      TRACE (xt);
      EXECUTE (xt);
    }

  exit (0);
}
