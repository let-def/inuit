TARGETS = default clean lib-native doc tests
all: default

.PHONY: all $(TARGETS)

$(TARGETS):
	sh build $@

%:
	sh build $@
