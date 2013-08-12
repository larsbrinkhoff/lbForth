CC = gcc
M32 = -m32
CFLAGS = $(M32) -O2 -fomit-frame-pointer -fno-unit-at-a-time
CPPFLAGS = -I$(TARGET)
LDFLAGS = $(M32)

TARGET = targets/c
meta = $(TARGET)/meta.fth
nucleus = $(TARGET)/nucleus.fth

%.c: %.fth
	echo 'include $(meta)  bye' | ./forth | tail -n+3 > $@

all: .bootstrap forth

.bootstrap: targets/c/meta.lisp
	make -f$(TARGET)/bootstrap.mk CC="$(CC)" CFLAGS="$(CFLAGS)" \
	CPPFLAGS="$(CPPFLAGS)" LDFLAGS="$(LDFLAGS)"

forth: kernel.o
	$(CC) $(LDFLAGS) $^ -o $@

kernel.o: kernel.c $(TARGET)/forth.h

kernel.c: kernel.fth c.fth params.fth $(nucleus) $(meta)

params.fth: params
	./$< -forth > $@

params: $(TARGET)/params.c $(TARGET)/forth.h Makefile
	$(CC) $(CFLAGS) $(CPPFLAGS) $< -o $@

clean:
	rm -f forth .bootstrap *.o kernel.c params*
