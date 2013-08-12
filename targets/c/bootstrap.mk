TARGET = targets/c
meta = $(TARGET)/meta.lisp
nucleus = $(TARGET)/nucleus.fth

# Bootstrap metacompiler, written in Lisp.
%.c: %.fth
	./lisp.sh '(progn (load "$(meta)") (compile-forth "$(nucleus)" "$<"))'

all: forth

forth: kernel.o
	$(CC) $(LDFLAGS) $^ -o $@
	rm kernel.c
	touch .bootstrap

kernel.o: kernel.c $(TARGET)/forth.h

kernel.c: kernel.fth c.fth $(nucleus) $(meta) params.lisp

params.lisp: params
	./$< -lisp > $@

params:
	make params
