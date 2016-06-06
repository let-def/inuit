all: byte-code-library native-code-library

MODULES = inuit_base inuit_region inuit_cursor inuit_remote inuit_widget inuit
SOURCES = $(foreach MOD,$(MODULES),$(MOD).mli $(MOD).ml)
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
  $(foreach MOD,$(MODULES),$(MOD).mli $(MOD).cmi $(MOD).cmx) \
	inuit.cma  inuit.a  inuit.cmxa

OCAMLFLAGS += -g
OCAMLLDFLAGS += -g
