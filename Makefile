TARGET = c
TFORTH = $(TARGET)-forth
TDIR = targets/$(TARGET)
OS = .
TSTAMP = $(TARGET)-$(OS)-stamp
META = $(TDIR)/meta.fth
FORTH = $(TDIR)/run.sh ./forth
DEPS = kernel.fth dictionary.fth $(TDIR)/nucleus.fth
PARAMS = params.fth jump.fth threading.fth t-kern.fth

GREP = grep -a
ERROR_PATTERNS = -e 'INCORRECT RESULT' -e 'WRONG NUMBER'
EXPECTED_ERRORS = 77


all: b-forth forth

b-forth:
	$(MAKE) -ftargets/c/bootstrap.mk
	cp $@ forth

forth: $(TFORTH)
	rm -f forth.exe
	cp $< $@

$(TSTAMP):
	rm -f *-stamp
	touch $@

include $(TDIR)/forth.mk

include check.mk

clean: t-clean
	rm -f forth *-forth smoke-test test-* *-stamp *.exe
