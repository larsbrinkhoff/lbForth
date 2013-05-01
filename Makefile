CC = gcc
CFLAGS = -g -m32 -fomit-frame-pointer -O2
LISP = sbcl
LDFLAGS = -g -m32 

OBJS = forth.o kernel.o

all: forth

forth: $(OBJS)

forth.o: forth.c forth.h

kernel.o: kernel.c kernel.h forth.h

%.c %.h: %.fth meta.lisp
	$(LISP) --load meta.lisp \
	        --eval '(progn (compile-forth "$<") (quit))'

clean:
	rm -f forth *.o kernel.c kernel.h
