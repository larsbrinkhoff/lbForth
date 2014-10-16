TARGET = c
THREADING = ctc

CC = gcc
M32 = -m32
CFLAGS = $(M32) -O2 -fomit-frame-pointer -fno-unit-at-a-time
CPPFLAGS = -I$(TDIR)
LDFLAGS = $(M32)

GREP = grep -a
ERROR_PATTERNS = -e 'INCORRECT RESULT' -e 'WRONG NUMBER'
EXPECTED_ERRORS = 77

TDIR = targets/$(TARGET)
meta = $(TDIR)/meta.fth
nucleus = $(TDIR)/nucleus.fth

DEPS = dictionary.fth params.fth jump.fth threading.fth $(nucleus) $(meta)

%.c: %.fth
	echo 'include $(meta)  bye' | ./forth | tail -n+3 > $@
	$(GREP) Meta-OK $@

all: .bootstrap forth

.bootstrap: lisp/meta.lisp lisp/words.lisp
	$(MAKE) -ftargets/c/bootstrap.mk CC="$(CC)" CFLAGS="$(CFLAGS)" \
	CPPFLAGS="$(CPPFLAGS)" LDFLAGS="$(LDFLAGS)"

lisp/meta.lisp:
	git submodule update --init

forth: kernel.o
	$(CC) $(LDFLAGS) $^ -o $@

kernel.o: kernel.c $(TDIR)/forth.h

kernel.c: kernel.fth $(DEPS)

params.fth: params
	./$< -forth > $@

params: $(TDIR)/params.c $(TDIR)/forth.h Makefile
	$(CC) $(CFLAGS) $(CPPFLAGS) $< -o $@

jump.fth: $(TDIR)/jump.fth
	cp $^ $@

threading.fth: targets/$(THREADING).fth
	cp $^ $@

check: test-errors
	test `cat $<` -eq $(EXPECTED_ERRORS)

test-errors: test-output
	$(GREP) $(ERROR_PATTERNS) $< | wc -l > $@

test-output: test/test-input smoke-test
	./forth < $< > $@
	$(GREP) Test-OK $@

smoke-test: all
	echo 'words cr .( Smoke-OK )' | ./forth > $@
	grep 'Smoke-OK' $@

clean:
	rm -f forth .bootstrap *.o kernel.c params* test-output test-errors
