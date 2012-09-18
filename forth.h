#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <stddef.h>

#undef TOKEN_THREADED

typedef long int cell;
typedef long unsigned ucell;
typedef long long dcell;
typedef unsigned long long udcell;

#ifdef TOKEN_THREADED
typedef unsigned short xt_t;
#else
typedef struct word *xt_t;
#endif

#define NEXT_XT (*IP++)

#define NAME_LENGTH 16
#define TO_NEXT offsetof (struct word, next)
#define TO_CODE offsetof (struct word, code)
#define TO_BODY offsetof (struct word, param)

struct word
{
  char nlen;
  char name[NAME_LENGTH - 1];
  struct word *next;
  void (*code) (struct word *);
  cell param[];
};

extern xt_t *IP;
extern struct word *lastxt;
extern cell *SP;
extern cell *RP;
extern char *HERE;
extern cell state;
extern cell base;
extern char tib[];
extern char fib[];
extern char squote[];
extern cell ntib;
extern cell to_in;
extern cell sink;
extern cell data_stack[];
extern cell return_stack[];

#ifdef TOKEN_THREADED
extern struct word *words[];
#define XT_WORD(XT) (words[XT])
#else
#define XT_WORD(XT) (XT)
#endif

#define EXECUTE(XT)				\
  do {						\
    struct word *word = XT_WORD (XT);		\
    word->code (word);				\
  } while (0)

#define POP(TYPE)	((TYPE)(*SP++))
#define PUSH(X)		(*--SP = (cell)(X))
#define RPOP(TYPE)	((TYPE)(*RP++))
#define RPUSH(X)	(*--RP = (cell)(X))
