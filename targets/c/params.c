#include "forth.h"

static void check (size_t n)
{
  if (sizeof (cell) < n)
    {
      fprintf (stderr, "Cell size too small.\n");
      exit (1);
    }
}

static void defvar (const char *name, int value)
{
  printf ("(defvar %s %d)\n", name, value);
}

int main (void)
{
  check (sizeof (cell *));
  check (sizeof (char *));
  check (sizeof (code_t *));
  check (sizeof (xt_t));

  defvar ("*cell-size*", sizeof (cell));
  defvar ("*name-size*", NAME_LENGTH);
  defvar ("*jmp_buf-size*", sizeof (jmp_buf));
  defvar ("*next-offset*", offsetof (struct word, next));
  defvar ("*code-offset*", offsetof (struct word, code));
  defvar ("*does-offset*", offsetof (struct word, does));
  defvar ("*body-offset*", offsetof (struct word, param));

  return 0;
}
