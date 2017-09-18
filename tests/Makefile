top_srcdir = ..

PROGRAMS = test.opt test_word.opt test_prec.opt test_rounded.opt test_inside.opt

all: opt
byte: $(PROGRAMS:.opt=.byte)
opt: $(PROGRAMS)

REQUIRES = pfds oaah

OCAMLFLAGS =
OCAMLOPTFLAGS =

ML_SOURCES = test.ml test_word.ml
WITH_OWN_LIB = geom

include $(top_srcdir)/make.common

$(PROGRAMS): $(top_srcdir)/geom.cmxa
$(PROGRAMS:.opt=.byte): $(top_srcdir)/geom.cma

.cmo.byte:
	$(OCAMLC) -package "$(REQUIRES)" -I $(top_srcdir) geom.cma -linkpkg $(OCAMLFLAGS) $< -o $@

.cmx.opt: $(top_srcdir)/geom.cmxa
	$(OCAMLOPT) -package "$(REQUIRES)" -I $(top_srcdir) geom.cmxa -linkpkg $(OCAMLOPTFLAGS) $< -o $@

clean-spec:
	@rm -f $(PROGRAMS) $(PROGRAMS:.opt=.byte)

-include .depend
