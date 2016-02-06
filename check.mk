CHECKS = test-standard test-assembler test-m68k-asm test-msp430-asm \
         test-image test-exe test-lib test-copyright test-meta

RUNTFORTH = $(TDIR)/run.sh ./$(TFORTH)

check: $(CHECKS)

test-standard: test-errors $(TDIR)/expected-errors
	test `cat $<` -eq `cat $(TDIR)/expected-errors` && touch $@

test-errors: test-output
	$(GREP) $(ERROR_PATTERNS) $< | wc -l > $@

test-output: test/test-input src/core.fth test-smoke
	$(RUNTFORTH) < $< > $@
	$(GREP) Test-OK $@

test-assembler: test/test-asm.fth targets/x86/asm.fth test-smoke
	echo 'include $< .( Asm-OK )' | $(RUNTFORTH) > $@
	$(GREP) Asm-OK $@

test-m68k-asm: test/test-m68k-asm.fth targets/m68k/asm.fth test-smoke
	echo 'include $< .( Asm-OK )' | $(RUNTFORTH) > $@
	$(GREP) Asm-OK $@

test-msp430-asm: test/test-msp430-asm.fth targets/msp430/asm.fth test-smoke
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
