OCAMLC   = OCAMLPATH=../.. ocamlfind ocamlc
OCAMLOPT = OCAMLPATH=../.. ocamlfind ocamlopt
OCAMLDEP = OCAMLPATH=../.. ocamlfind ocamldep
OCAMLOPTFLAGS = -w Ael -g -S
OCAMLFLAGS    = -w Ael -g

REQUIRES = geom

.PHONY: all clean

PROGRAMS = mlrocket.byte
SOURCES = mlrocket.ml rocket.ml world.ml game.ml main.ml

all: $(PROGRAMS)
opt: $(PROGRAMS:.byte=.opt)

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx .opt .byte

mlrocket.byte: $(SOURCES:.ml=.cmo)
	$(OCAMLC) -o $@ -package "$(REQUIRES)" -linkpkg $(OCAMLFLAGS) $^

mlrocket.opt: $(SOURCES:.ml=.cmx)
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
.depend: *.ml *.mli
	$(OCAMLDEP) -package "$(REQUIRES)" -I .. $^ > $@

-include .depend
