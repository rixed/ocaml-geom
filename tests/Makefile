OCAMLPATH = ../..

PROGRAMS = test.byte test_word.byte test_prec.byte test_rounded.byte

all: libft2.a byte opt
byte: $(PROGRAMS)
opt: $(PROGRAMS:.byte=.opt)

REQUIRES = geom glop

OCAMLFLAGS =
OCAMLOPTFLAGS =

ML_SOURCES = test.ml test_word.ml

include ../make.common

libft2.a:
	ln -s ../libft2.a

.cmo.byte:
	$(OCAMLC)   -package "$(REQUIRES)" -linkpkg $(OCAMLFLAGS) $^ -o $@

.cmx.opt:
	$(OCAMLOPT) -package "$(REQUIRES)" -linkpkg $(OCAMLOPTFLAGS) $^ -o $@

clean-spec:
	@rm -f $(PROGRAMS) $(PROGRAMS:.byte=.opt)

-include .depend
