all: byte-code-library native-code-library

SOURCES = inuit_base.mli inuit_base.ml inuit.mli inuit.ml
RESULT = inuit
PACKS = grenier

OCAMLMAKEFILE=OCamlMakefile
-include $(OCAMLMAKEFILE)

install: libinstall

uninstall: libuninstall

reinstall:
	-$(MAKE) uninstall
	$(MAKE) install

LIBINSTALL_FILES = \
	inuit.mli  \
	inuit.cmi  \
	inuit.a    \
	inuit.cma  \
	inuit.cmxa

OCAMLFLAGS += -g
OCAMLLDFLAGS += -g
