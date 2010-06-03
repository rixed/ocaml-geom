OCAMLC   = OCAMLPATH=../.. ocamlfind ocamlc
OCAMLOPT = OCAMLPATH=../.. ocamlfind ocamlopt
OCAMLDEP = OCAMLPATH=../.. ocamlfind ocamldep
OCAMLOPTFLAGS = -S -w Ael -g
OCAMLFLAGS    = -w Ael -g

REQUIRES = geom

.PHONY: all clean

PROGRAMS = test.byte testpic.byte testplot.byte test_word.byte

all: $(PROGRAMS)
opt: $(PROGRAMS:.byte=.opt)

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx .opt .byte

.cmo.byte:
	$(OCAMLC)   -o $@ -package "$(REQUIRES)" -linkpkg $(OCAMLFLAGS) $^

.cmx.opt:
	$(OCAMLOPT) -o $@ -package "$(REQUIRES)" -linkpkg $(OCAMLOPTFLAGS) $^

.ml.cmo:
	$(OCAMLC) -package "$(REQUIRES)" $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) -package "$(REQUIRES)" $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) -package "$(REQUIRES)" $(OCAMLOPTFLAGS) -c $<

# Clean up
clean:
	rm -f *.cm[ioxa] *.cmxa *.a *.s *.o *.byte *.opt .depend

# Dependencies
.depend: *.ml
	$(OCAMLDEP) -package "$(REQUIRES)" -I .. $^ > $@

include .depend
