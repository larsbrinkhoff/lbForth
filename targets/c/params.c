#include "forth.h"

int main (void)
{
  printf ("(defvar *sizeof-cell* %d)\n", sizeof (cell));
  printf ("(defvar *sizeof-jmp_buf* %d)\n", sizeof (jmp_buf));
  return 0;
}
