NAME = geom

OCAMLC     = ocamlfind ocamlc -thread
OCAMLOPT   = ocamlfind ocamlopt -thread
OCAMLDEP   = ocamlfind ocamldep
INCS       =
OCAMLOPTFLAGS = $(INCS) -S -w Ae -g
OCAMLFLAGS    = $(INCS) -w Ae -g

SOURCES  = cnt.ml cnt_impl.ml \
	geom.ml geom_algebr.ml geom_shapes.ml geom_path.ml geom_algo.ml geom_text.ml \
	view.ml view_simple.ml turtle.ml \
	pic_intf.ml pic_impl.ml plot_intf.ml plot_impl.ml
OBJECTS  = $(SOURCES:.ml=.cmo)
XOBJECTS = $(OBJECTS:.cmo=.cmx)

ARCHIVE  = $(NAME).cma
XARCHIVE = $(NAME).cmxa

REQUIRES = lablGL.glut camlimages.freetype

.PHONY: all clean install uninstall reinstall

all: $(ARCHIVE)
opt: $(XARCHIVE)

$(ARCHIVE): $(OBJECTS)
	$(OCAMLC)   -a -o $@ -package "$(REQUIRES)" -linkpkg $(OCAMLFLAGS) $^

$(XARCHIVE): $(XOBJECTS)
	$(OCAMLOPT) -a -o $@ -package "$(REQUIRES)" $(OCAMLOPTFLAGS) $^

install: all
	if test -f $(XARCHIVE) ; then extra="$(XARCHIVE) "`basename $(XARCHIVE) .cmxa`.a ; fi ; \
	ocamlfind install $(NAME) *.cmi $(ARCHIVE) META geom.ml cnt.ml $$extra

uninstall:
	ocamlfind remove $(NAME)

reinstall: uninstall install

check: geom.cma geom.cmxa
	make -C tests all opt && tests/testpic.byte

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) -package "$(REQUIRES)" $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) -package "$(REQUIRES)" $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) -package "$(REQUIRES)" $(OCAMLOPTFLAGS) -c $<

# Clean up
clean:
	rm -f *.cm[ioxa] *.cmxa *.a *.o *.s .depend

# Dependencies
.depend: $(SOURCES)
	$(OCAMLDEP) -package "$(REQUIRES)" $^ > $@

include .depend
