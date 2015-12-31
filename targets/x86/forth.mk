META-OUTPUT = $(TFORTH)

$(TFORTH): b-forth $(DEPS) $(PARAMS) $(META)
	echo include $(META) | targets/c/run.sh ./forth | tail -n+3 > $@
	chmod a+x $@

# For now, use the same jumps and threading as the C target.
jump.fth: targets/c/jump.fth
	cp $^ $@

threading.fth: targets/ctc.fth
	cp $^ $@

params.fth:$ $(TDIR)/params.fth $(TDIR)/$(OS)/params.fth $(TSTAMP)
	cp $(TDIR)/params.fth $@
	cat $(TDIR)/$(OS)/params.fth >> $@

t-kern.fth: $(TDIR)/$(OS)/t-kern.fth $(TSTAMP)
	cp $(TDIR)/$(OS)/t-kern.fth t-kern.fth

t-clean:
	rm -f $(PARAMS)
