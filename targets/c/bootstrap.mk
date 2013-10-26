TARGET = targets/c
LISP = ./lisp/lisp.sh
meta = lisp/meta.lisp
nucleus = $(TARGET)/nucleus.fth

# Bootstrap metacompiler, written in Lisp.
%.c: %.fth
	$(LISP) '(progn (load "$(meta)") (compile-forth "$(nucleus)" "$<"))'

all: forth

forth: kernel.o
	$(CC) $(LDFLAGS) $^ -o $@
	rm kernel.c
	touch .bootstrap

kernel.o: kernel.c $(TARGET)/forth.h

kernel.c: kernel.fth dictionary.fth $(nucleus) $(meta) params.lisp

params.lisp: params
	./$< -lisp > $@

params:
	$(MAKE) params
