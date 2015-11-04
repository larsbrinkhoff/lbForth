TDIR = targets/c
RUN = $(TDIR)/run.sh
TMAKE = $(MAKE) -f$(TDIR)/forth.mk
LISP = ./lisp/lisp.sh
META = lisp/meta.lisp
NUCLEUS = $(TDIR)/nucleus.fth

# Bootstrap metacompiler, written in Lisp.
METACOMPILE = $(LISP) '(load \"$(META)\") (compile-forth \"$(NUCLEUS)\" \"$<\")'

all: forth

forth: kernel.fth params.lisp $(META)
	$(TMAKE) METACOMPILE="$(METACOMPILE)" $@
	rm kernel.c

$(META):
	git submodule update --init

params.lisp: params
	$(RUN) ./$< -lisp > $@

params:
	$(TMAKE) $@
