OCAMLPATH = ../..

PROGRAMS = test.byte test_word.byte

all: $(PROGRAMS)
opt: $(PROGRAMS:.byte=.opt)

REQUIRES = geom glop

OCAMLFLAGS = -rectypes
OCAMLOPTFLAGS = -rectypes

ML_SOURCES = test.ml test_word.ml

include ../make.common

.cmo.byte:
	$(OCAMLC)   -o $@ -package "$(REQUIRES)" -linkpkg $(OCAMLFLAGS) $^

.cmx.opt:
	$(OCAMLOPT) -o $@ -package "$(REQUIRES)" -linkpkg $(OCAMLOPTFLAGS) $^

clean-spec:
	@rm -f $(PROGRAMS) $(PROGRAMS:.byte=.opt)

-include .depend
