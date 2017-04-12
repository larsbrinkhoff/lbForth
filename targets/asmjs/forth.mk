DEPS += $(TDIR)/cold.fth
META-OUTPUT = $(TFORTH)

$(TFORTH): b-forth $(DEPS) $(PARAMS) $(META)
	echo include $(META) | targets/c/run.sh ./forth | tail -n+3 > $@
	-mv image $@
	chmod a+x $@

jump.fth: $(TDIR)/jump.fth $(TSTAMP)
	cp $< $@

threading.fth: targets/ctc.fth $(TSTAMP)
	cp $< $@

params.fth:$ $(TDIR)/params.fth $(TSTAMP)
	cp $(TDIR)/params.fth $@

target.fth: $(TDIR)/target.fth $(TSTAMP) Makefile
	cp $(TDIR)/target.fth  $@
	echo ": sysdir   s\" $(sysdir)/\" ;" >> $@

t-clean:
	rm -f $(PARAMS)
