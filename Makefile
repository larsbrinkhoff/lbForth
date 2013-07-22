CC = gcc
M32 = -m32
CFLAGS = $(M32) -O2 -fomit-frame-pointer -fno-unit-at-a-time
CPPFLAGS = -I$(TARGET)
LDFLAGS = $(M32)

TARGET = targets/c
boot = $(TARGET)/meta.lisp
meta = $(TARGET)/meta.fth
nucleus = $(TARGET)/nucleus.fth

all: forth

forth: kernel.o
	$(CC) $(LDFLAGS) $^ -o $@

kernel.o: kernel.c $(TARGET)/forth.h

%.c: %.fth c.fth $(nucleus) $(boot) params.lisp
	./lisp.sh '(progn (load "$(boot)") (compile-forth "$(nucleus)" "$<"))'

params.lisp: params
	./$< -lisp > $@

params.fth: params
	./$< -forth > $@

params: $(TARGET)/params.c $(TARGET)/forth.h Makefile
	$(CC) $(CFLAGS) $(CPPFLAGS) $< -o $@

clean:
	rm -f forth *.o kernel.c kernel2.c params*

kernel2.c: forth kernel.fth c.fth params.fth $(nucleus) $(meta)
	echo 'include $(meta)  bye' | ./forth | tail -n+3 > $@
