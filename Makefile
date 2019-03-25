SOURCES = \
  bimsb/packages/staging.scm  \
  bimsb/packages/variants.scm

GOBJECTS = $(SOURCES:%.scm=%.go)
%.go: %.scm
	export GUILE_LOAD_PATH=.:$(GUILE_LOAD_PATH); \
	export GUILE_LOAD_COMPILED_PATH=.:$(GUILE_LOAD_COMPILED_PATH); \
	export GUILE_AUTO_COMPILE=0; \
	guild compile -W unbound-variable -o "$@" "$<"

all: $(GOBJECTS)
	echo "done"
