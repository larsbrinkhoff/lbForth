TARGET = c
THREADING = ctc

TDIR = targets/$(TARGET)
LISP = ./lisp/lisp.sh
meta = lisp/meta.lisp
nucleus = $(TDIR)/nucleus.fth
DEPS = dictionary.fth jump.fth threading.fth $(nucleus) $(meta)

# Bootstrap metacompiler, written in Lisp.
%.c: %.fth
	$(LISP) '(load "$(meta)") (compile-forth "$(nucleus)" "$<")'

all: forth

forth: kernel.o
	$(CC) $(LDFLAGS) $^ -o $@
	rm kernel.c jump.fth threading.fth
	touch .bootstrap

kernel.o: kernel.c $(TDIR)/forth.h

kernel.c: kernel.fth $(DEPS) params.lisp

params.lisp: params
	./$< -lisp > $@

params:
	$(MAKE) params

jump.fth: $(TDIR)/jump.fth
	cp $^ $@

threading.fth: targets/$(THREADING).fth
	cp $^ $@
