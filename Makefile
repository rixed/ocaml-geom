OCAMLPATH = ..

.PHONY: all
all: byte opt

byte: geom.cma
opt: geom.cmxa

LIBS = -cclib -lfreetype

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
	$(OCAMLC)   -a -o $@ -package "$(REQUIRES)" -custom -linkpkg $(OCAMLFLAGS) $(ML_OBJS) -cclib -lft2 $(LIBS)

$(XARCHIVE): $(ML_XOBJS) libft2.a
	$(OCAMLOPT) -a -o $@ -package "$(REQUIRES)" $(OCAMLOPTFLAGS) $(ML_XOBJS) -cclib -lft2 $(LIBS)

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
