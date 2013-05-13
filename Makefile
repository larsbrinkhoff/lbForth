CC = gcc
LISP = sbcl
CFLAGS = -g -m32 -fomit-frame-pointer -O2 -I.
LDFLAGS = -g -m32 

meta = targets/c/meta.lisp
nucleus = targets/c/nucleus.fth

all: forth

forth: kernel.o
	$(CC) $(LDFLAGS) $^ -o $@

kernel.o: kernel.c kernel.h forth.h

%.c %.h: %.fth $(nucleus) $(meta) params.lisp
	$(LISP) --noinform --load $(meta) \
	        --eval '(compile-forth "$(nucleus)" "$<")'

params.lisp: params
	./$< > $@

params: targets/c/params.c
	$(CC) $(CFLAGS) -I. $^ -o $@

clean:
	rm -f forth *.o kernel.c kernel.h params params.lisp
