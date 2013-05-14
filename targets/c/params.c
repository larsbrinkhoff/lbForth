#include "forth.h"

int main (void)
{
  printf ("(defvar *sizeof-cell* %d)\n", sizeof (cell));
  printf ("(defvar *sizeof-jmp_buf* %d)\n", sizeof (jmp_buf));
  printf ("(defvar *NAME_LENGTH* %d)\n", NAME_LENGTH);
  printf ("(defvar *TO_NEXT* %d)\n", TO_NEXT);
  printf ("(defvar *TO_CODE* %d)\n", TO_CODE);
  printf ("(defvar *TO_DOES* %d)\n", TO_DOES);
  printf ("(defvar *TO_BODY* %d)\n", TO_BODY);

  return 0;
}
