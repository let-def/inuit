TARGETS = default clean lib-native doc tests
OCAMLBUILD ?= ocamlbuild -use-ocamlfind -classic-display -tag debug

all: default

.PHONY: all $(TARGETS)

default: lib-native

lib-native:
	$(OCAMLBUILD) inuit.cmxa
	
lib-byte:
	$(OCAMLBUILD) inuit.cma

test tests:
	$(OCAMLBUILD) test/tests.otarget

doc:    
	topkg doc $(ARGS)

dev-doc:
	topkg doc --dev $(ARGS)

clean:
	$(OCAMLBUILD) -clean
