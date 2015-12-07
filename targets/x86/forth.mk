$(TFORTH): b-forth $(DEPS) $(PARAMS) $(META)
	echo include $(META) | ./forth | tail -n+3 > $@
	chmod a+x $@
	cp $@ forth

params.fth: $(TDIR)/params.fth
	cp $^ $@

# For now, use the same jumps and threading as the C target.
jump.fth: targets/c/jump.fth
	cp $^ $@

threading.fth: targets/ctc.fth
	cp $^ $@

t-clean:
	rm -f $(PARAMS)
