/* Copyright 2004, 2013, 2015-2016 Lars Brinkhoff */

#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <stddef.h>
#include <unistd.h>

#if defined(__GNUC__) && defined (__i386__)
#define REGPARM __attribute__((regparm(2)))
#else
#define REGPARM
#endif

typedef long int cell;
typedef char char_t;
typedef unsigned char uchar_t;
typedef struct word *nt_t;
typedef struct word *xt_t;
typedef xt_t * REGPARM code_t (xt_t *, nt_t);

#define NAME_LENGTH 16

struct word
{
  uchar_t nlen;
  char_t name[NAME_LENGTH - 1];
  nt_t next;
  cell *does;
  code_t *code;
  cell param[];
};

#define NEXT_XT  (*IP++)
#define EXECUTE(XT)  IP = (XT)->code (IP, XT)

extern struct word SP_word, RP_word;

#define POP(TYPE)	((TYPE)(*(*((cell **)SP_word.param))++))
#define PUSH(X)		(*--(*((cell **)SP_word.param)) = (cell)(X))
#define RPOP(TYPE)	((TYPE)(*(*((cell **)RP_word.param))++))
#define RPUSH(X)	(*--(*((cell **)RP_word.param)) = (cell)(X))
