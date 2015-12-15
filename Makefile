TARGET = c
TFORTH = $(TARGET)-forth
TDIR = targets/$(TARGET)
META = $(TDIR)/meta.fth
FORTH = $(TDIR)/run.sh ./forth
DEPS = kernel.fth dictionary.fth $(TDIR)/nucleus.fth
PARAMS = params.fth jump.fth threading.fth

GREP = grep -a
ERROR_PATTERNS = -e 'INCORRECT RESULT' -e 'WRONG NUMBER'
EXPECTED_ERRORS = 77


all: b-forth $(TFORTH)

b-forth:
	$(MAKE) -ftargets/c/bootstrap.mk

include $(TDIR)/forth.mk

include check.mk

clean: t-clean
	rm -f forth *-forth smoke-test test-* *.exe
