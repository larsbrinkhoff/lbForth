CC = gcc
M32 = -m32
CFLAGS = $(M32) -O2 -fomit-frame-pointer -fno-unit-at-a-time
CPPFLAGS = -I$(TDIR)
LDFLAGS = $(M32)

META-OUTPUT = kernel.c
RUN = $(TDIR)/run.sh

METACOMPILE ?= echo 'include $(META)  bye' | $(RUN) ./forth | tail -n+3 > $@ ; \
	$(GREP) Meta-OK $@

$(TFORTH): kernel.o
	$(CC) $(LDFLAGS) $^ -o $@
	cp $@ forth

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

t-clean:
	rm -f *.o kernel.c params* $(PARAMS)
