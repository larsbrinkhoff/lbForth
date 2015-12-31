CHECKS = test-errors test-assembler test-image test-exe test-lib \
         test-copyright test-meta

RUNTFORTH = $(TDIR)/run.sh ./$(TFORTH)

check: $(CHECKS)
	test `cat $<` -eq $(EXPECTED_ERRORS)

test-errors: test-output
	$(GREP) $(ERROR_PATTERNS) $< | wc -l > $@

test-output: test/test-input smoke-test
	$(RUNTFORTH) < $< > $@
	$(GREP) Test-OK $@

test-assembler: test/test-asm.fth targets/x86/asm.fth
	echo 'include $< .( Asm-OK )' | $(RUNTFORTH) > $@
	$(GREP) Asm-OK $@

smoke-test: $(TFORTH)
	echo 'words cr .( Smoke-OK )' | $(RUNTFORTH) > $@
	$(GREP) Smoke-OK $@

test-image: test/test-image.fth lib/image.fth
	echo 'include $< .( Image-OK )' | $(RUNTFORTH) > $@
	$(GREP) Image-OK $@

test-exe: test/test-exe.sh test/test-elf.fth test/test-pe.fth targets/x86/asm.fth lib/elf.fth lib/pe.fth
	FORTH="$(RUNTFORTH)" ./test/test-exe.sh > $@
	$(GREP) 42 $@

test-lib: test/test-lib.sh
	sh test/test-lib.sh

test-meta: $(META) $(DEPS)
	echo 'include $< bye' | $(RUNTFORTH) | tail -n+3 > $@
	-cmp $@ $(META-OUTPUT)

test-copyright:
	sh test/test-copyright.sh
