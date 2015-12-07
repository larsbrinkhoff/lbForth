check: test-errors test-assembler test-image test-exe test-lib
	test `cat $<` -eq $(EXPECTED_ERRORS)

test-errors: test-output
	$(GREP) $(ERROR_PATTERNS) $< | wc -l > $@

test-output: test/test-input smoke-test
	$(FORTH) < $< > $@
	$(GREP) Test-OK $@

test-assembler: test/test-asm.fth targets/x86/asm.fth
	echo 'include $< .( Asm-OK )' | $(FORTH) > $@
	$(GREP) Asm-OK $@

smoke-test: all
	echo 'words cr .( Smoke-OK )' | $(FORTH) > $@
	$(GREP) Smoke-OK $@

test-image: test/test-image.fth lib/image.fth
	echo 'include $< .( Image-OK )' | $(FORTH) > $@
	$(GREP) Image-OK $@

test-exe: test/test-exe.sh test/test-elf.fth test/test-pe.fth targets/x86/asm.fth
	TDIR=$(TDIR) ./test/test-exe.sh > $@
	$(GREP) 42 $@

test-lib: test/test-lib.sh
	sh test/test-lib.sh

test-meta: targets/x86/meta.fth targets/x86/nucleus.fth kernel.fth
	echo 'include $<' | $(FORTH) | tail -n+3 > $@
	chmod a+x $@
