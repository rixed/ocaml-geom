OCAMLPATH = ..

all: geom.cma
opt: geom.cmxa

NAME = geom

ML_SOURCES  = cnt.ml cnt_impl.ml \
	geom.ml geom_shapes.ml geom_path.ml geom_algo.ml \
	text_intf.ml text_impl.ml

REQUIRES = bricabrac pfds algen camlimages.freetype

OCAMLOPTFLAGS = -rectypes
OCAMLFLAGS = -rectypes
include make.common

ARCHIVE = $(NAME).cma
XARCHIVE = $(NAME).cmxa

$(ARCHIVE): $(ML_OBJS)
	$(OCAMLC)   -a -o $@ -package "$(REQUIRES)" -linkpkg $(OCAMLFLAGS) $^

$(XARCHIVE): $(ML_XOBJS)
	$(OCAMLOPT) -a -o $@ -package "$(REQUIRES)" $(OCAMLOPTFLAGS) $^

install: all
	if test -f $(XARCHIVE) ; then extra="$(XARCHIVE) "`basename $(XARCHIVE) .cmxa`.a ; fi ; \
	ocamlfind install $(NAME) *.cmi $(ARCHIVE) META geom.ml cnt.ml $$extra

uninstall:
	ocamlfind remove $(NAME)

reinstall: uninstall install

check: $(ARCHIVE) $(XARCHIVE)
	@make -C tests all opt
	@for t in tests/*.byte tests/*.opt ; do $$t ; done
	@echo Ok

clean-spec:
	@make -C tests clean

distclean: clean

include .depend
