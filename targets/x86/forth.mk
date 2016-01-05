META-OUTPUT = $(TFORTH)

$(TFORTH): b-forth $(DEPS) $(PARAMS) $(META)
	echo include $(META) | targets/c/run.sh ./forth | tail -n+3 > $@
	chmod a+x $@

jump.fth: targets/x86/jump.fth $(TSTAMP)
	cp $< $@

threading.fth: targets/itc.fth $(TSTAMP)
	cp $< $@

params.fth:$ $(TDIR)/params.fth $(TDIR)/$(OS)/params.fth $(TSTAMP)
	cat $(TDIR)/params.fth $(TDIR)/$(OS)/params.fth > $@

t-kern.fth: $(TDIR)/t-kern.fth $(TDIR)/$(OS)/t-kern.fth $(TSTAMP)
	cat $(TDIR)/t-kern.fth $(TDIR)/$(OS)/t-kern.fth > $@

t-clean:
	rm -f $(PARAMS)
