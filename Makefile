all: byte-code-library native-code-library

SOURCES = inuit.mli inuit.ml
RESULT = inuit

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
