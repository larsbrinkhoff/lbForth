TDIR = targets/x86
# META = $(TDIR)/meta.fth
NUCLEUS = $(TDIR)/nucleus.fth
PARAMS = params.fth jump.fth threading.fth
DEPS = kernel.fth dictionary.fth $(PARAMS) $(NUCLEUS)

METACOMPILE = touch forth

all: forth

forth: kernel.fth $(DEPS) # $(META)
	$(METACOMPILE)

params.fth: $(TDIR)/params.fth
	cp $^ $@

jump.fth: $(TDIR)/jump.fth
	cp $^ $@

threading.fth: targets/ctc.fth
	cp $^ $@

clean:
	rm -f forth $(PARAMS)
