/* Copyright 2004, 2013 Lars Brinkhoff */

#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>

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
#define TO_DOES offsetof (struct word, does)
#define TO_BODY offsetof (struct word, param)

#if defined(__GNUC__) && defined (__i386__)
#define REGPARM __attribute__((regparm(2)))
#else
#define REGPARM
#endif

struct word
{
  char nlen;
  char name[NAME_LENGTH - 1];
  struct word *next;
  xt_t * (*code) (xt_t *, struct word *) REGPARM;
  cell *does;
  cell param[];
};

extern cell *SP;
extern cell *RP;
extern char tib[];
extern char fib[];
extern cell data_stack[];
extern cell return_stack[];
extern cell word_area[];

#ifdef TOKEN_THREADED
extern struct word *words[];
#define XT_WORD(XT) (words[XT])
#else
#define XT_WORD(XT) (XT)
#endif

#define EXECUTE(XT)				\
  do {						\
    struct word *word = XT_WORD (XT);		\
    IP = word->code (IP, word);			\
  } while (0)

#define POP(TYPE)	((TYPE)(*SP++))
#define PUSH(X)		(*--SP = (cell)(X))
#define RPOP(TYPE)	((TYPE)(*RP++))
#define RPUSH(X)	(*--RP = (cell)(X))
