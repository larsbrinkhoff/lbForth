DEPS += $(TDIR)/cold.fth
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

target.fth: $(TDIR)/target.fth $(TDIR)/$(OS)/target.fth $(TSTAMP)
	cat $(TDIR)/target.fth $(TDIR)/$(OS)/target.fth > $@

t-clean:
	rm -f $(PARAMS)
