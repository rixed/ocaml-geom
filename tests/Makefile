top_srcdir = ..

PROGRAMS = test.opt test_word.opt test_prec.opt test_rounded.opt test_inside.opt

all: opt
opt: $(PROGRAMS)

REQUIRES = pfds oaah

OCAMLFLAGS =
OCAMLOPTFLAGS =

ML_SOURCES = test.ml test_word.ml

include $(top_srcdir)/make.common

%.cmx: %.ml $(top_srcdir)/geom.cmxa
	$(OCAMLOPT) -package "$(REQUIRES)" -I $(top_srcdir) $(OCAMLOPTFLAGS) -c $<

%.opt: %.cmx $(top_srcdir)/geom.cmxa
	$(OCAMLOPT) -package "$(REQUIRES)" -I $(top_srcdir) geom.cmxa -linkpkg $(OCAMLOPTFLAGS) $< -o $@

clean-spec:

distclean-spec:
	$(RM) $(PROGRAMS)

-include .depend
