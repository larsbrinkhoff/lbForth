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

static int arg (int argc, const char **argv, const char *x)
{
  return argc == 2 && strcmp (argv[1], x) == 0;
}

int main (int argc, const char **argv)
{
  void (*output) (const char *, int);

  if (arg (argc, argv, "-forth"))
    output = forth_constant;
  else if (arg (argc, argv, "-lisp"))
    output = lisp_defvar;
  else
    error ("Specify -forth or -lisp.");

  check (sizeof (cell *));
  check (sizeof (char *));
  check (sizeof (code_t *));
  check (sizeof (xt_t));

  output ("cell-size", sizeof (cell));
  output ("name-size", NAME_LENGTH);
  output ("jmp_buf-size", sizeof (jmp_buf));
  output ("next-offset", offsetof (struct word, next));
  output ("code-offset", offsetof (struct word, code));
  output ("does-offset", offsetof (struct word, does));
  output ("body-offset", offsetof (struct word, param));

  return 0;
}
