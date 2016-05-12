#include <string.h>
#include "forth.h"

static void error (const char *message)
{
  fprintf (stderr, "%s\n", message);
  exit (1);
}

static void check (size_t n)
{
  if (sizeof (cell) < n)
    error ("Cell size too small.");
}

static void forth_constant (const char *name, int value)
{
  printf ("%d constant %s\n", value, name);
}

static void lisp_defvar (const char *name, int value)
{
  printf ("(defvar *%s* %d)\n", name, value);
}

static void usage (const char *name, int value)
{
  error ("Specify -forth or -lisp");
}

int main (int argc, const char **argv)
{
  void (*output) (const char *, int) = usage;

  if (argc == 2)
    {
      if (strcmp (argv[1], "-forth") == 0)
	output = forth_constant;
      else if (strcmp (argv[1], "-lisp") == 0)
	output = lisp_defvar;
    }

  check (sizeof (cell *));
  check (sizeof (char *));
  check (sizeof (code_t *));
  check (sizeof (xt_t));

  output ("cell-size", sizeof (cell));
  output ("name-size", NAME_LENGTH);
  output ("next-offset", offsetof (struct word, next));
  output ("code-offset", offsetof (struct word, code));
  output ("does-offset", offsetof (struct word, does));
  output ("body-offset", offsetof (struct word, param));

  return 0;
}
