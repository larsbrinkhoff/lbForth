CHECKS = test-standard test-6502-asm test-pdp11-asm test-avr-asm \
         test-image test-meta test-lib test-copyright
MORE_CHECKS = test-x86-asm test-m68k-asm test-msp430-asm test-arm-asm test-exe

RUNTFORTH = $(TDIR)/run.sh ./$(TFORTH)

check: check-16-bit $(MORE_CHECKS)

check-16-bit: $(CHECKS)

test-standard: test-errors $(TDIR)/expected-errors
	test `cat $<` -eq `cat $(TDIR)/expected-errors` && touch $@

test-errors: test-output
	$(GREP) $(ERROR_PATTERNS) $< | wc -l > $@

test-output: test/test-input src/core.fth test-smoke
	$(RUNTFORTH) < $< > $@
	$(GREP) Test-OK $@

test-%-asm: test/test-%-asm.fth targets/%/asm.fth test-smoke
	echo 'include $< .( Asm-OK )' | $(RUNTFORTH) > $@
	$(GREP) Asm-OK $@

test-smoke: $(TFORTH)
	echo 'words cr .( Smoke-OK )' | $(RUNTFORTH) > $@
	$(GREP) Smoke-OK $@

test-image: test/test-image.fth lib/image.fth test-smoke
	echo 'include $< .( Image-OK )' | $(RUNTFORTH) > $@
	$(GREP) Image-OK $@

test-exe: test/test-exe.sh test/test-elf.fth test/test-pe.fth targets/x86/asm.fth lib/elf.fth lib/pe.fth test-smoke
	FORTH="$(RUNTFORTH)" ./test/test-exe.sh > $@
	$(GREP) 42 $@

test-lib: lib/README test/test-lib.sh
	sh test/test-lib.sh && touch $@

test-meta: $(META) $(DEPS) test-smoke
	echo 'include $< bye' | $(RUNTFORTH) | tail -n+3 > $@
	-mv image $@
	-cmp $@ $(META-OUTPUT)

test-copyright:
	sh test/test-copyright.sh && touch $@
