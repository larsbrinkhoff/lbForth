CC = gcc
M32 = -m32
CFLAGS = $(M32) -O2 -fomit-frame-pointer -fno-unit-at-a-time
CPPFLAGS = -I$(TDIR)
LDFLAGS = $(M32)
GREP = grep -a

TDIR = targets/c
META = $(TDIR)/meta.fth
RUN = $(TDIR)/run.sh
PARAMS = params.fth jump.fth threading.fth

METACOMPILE = echo 'include $(META)  bye' | $(RUN) ./forth | tail -n+3 > $@ ; \
	$(GREP) Meta-OK $@

all: forth

forth: kernel.o
	$(CC) $(LDFLAGS) $^ -o $@

kernel.o: kernel.c $(TDIR)/forth.h

kernel.c: $(DEPS) $(PARAMS) $(META)
	$(METACOMPILE)

params.fth: params
	$(RUN) ./$< -forth > $@

params: $(TDIR)/params.c $(TDIR)/forth.h $(TDIR)/forth.mk
	$(CC) $(CFLAGS) $(CPPFLAGS) $< -o $@

jump.fth: $(TDIR)/jump.fth
	cp $^ $@

threading.fth: targets/ctc.fth
	cp $^ $@

clean:
	rm -f forth *.o kernel.c params* $(PARAMS)
