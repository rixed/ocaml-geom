.PHONY: all top
all: byte opt

top: geom.top
	OCAMLPATH=../ rlwrap ./geom.top -init geominit

byte: geom.cma
opt: geom.cmxa

LIBS = -cclib -lfreetype -cclib -lft2

NAME = geom

ML_SOURCES  = cnt.ml cnt_impl.ml \
	geom.ml geom_shapes.ml geom_path.ml geom_algo.ml \
	ftlow.ml freetype.ml \
	text_intf.ml text_impl.ml

C_SOURCES = ftintf.c
CPPFLAGS += $(shell freetype-config --cflags)
LDLIBS += $(shell freetype-config --libs)

REQUIRES = bricabrac pfds algen

OCAMLOPTFLAGS =
OCAMLFLAGS =
include make.common

ARCHIVE = $(NAME).cma
XARCHIVE = $(NAME).cmxa

libft2.a: $(C_SOURCES:.c=.o)
	$(AR) rcs $@ $^

$(ARCHIVE): $(ML_OBJS) libft2.a
	$(OCAMLC)   -package "$(REQUIRES)" -custom -linkpkg $(OCAMLFLAGS) $(ML_OBJS) $(LIBS) -a -o $@

$(XARCHIVE): $(ML_XOBJS) libft2.a
	$(OCAMLOPT) -package "$(REQUIRES)" $(OCAMLOPTFLAGS) $(ML_XOBJS) $(LIBS) -a -o $@

geom.top: $(ARCHIVE)
	$(OCAMLMKTOP) -o $@ -package "findlib,$(REQUIRES)" -linkpkg $(ARCHIVE)

install: all
	if test -f $(XARCHIVE) ; then extra="$(XARCHIVE) "`basename $(XARCHIVE) .cmxa`.a ; fi ; \
	ocamlfind install $(NAME) *.cmi $(ARCHIVE) META freetype.mli libft2.a geom.ml cnt.ml $$extra

uninstall:
	ocamlfind remove $(NAME)

reinstall: uninstall install

check: $(ARCHIVE) $(XARCHIVE)
	$(MAKE) -C tests all opt
	@for t in tests/*.byte tests/*.opt ; do $$t ; done
	@echo Ok

clean-spec:
	$(MAKE) -C tests clean

distclean: clean

-include .depend
