.PHONY: test compile clean package

EL := $(wildcard *.el)
ELC := $(patsubst %.el,%.elc,$(wildcard *.el))
TAR := gtar
JSON := json
METADATA := meta.json
PKG_NAME := $(shell $(JSON) -a -d- name version < $(METADATA))

%.elc: %.el
	emacs -Q -batch -L `pwd` -f batch-byte-compile $<

test:
	$(MAKE) -C test fixtures
	@for idx in test/test_*; do \
		printf '* %s\n' $$idx ; \
		./$$idx ; \
		[ $$? -ne 0 ] && exit 1 ; \
	done; :

compile: $(ELC)

ph-pkg.el: meta.json
	bin/ph-make-pkg $< $@

package: ph-pkg.el
	$(TAR) --transform='s,^,$(PKG_NAME)/,S' -cf $(PKG_NAME).tar \
		`$(JSON) files < $(METADATA) | $(JSON) -a`

clean:
	rm -rf $(ELC) ph-pkg.el $(PKG_NAME).tar
	$(MAKE) -C test $@

# Debug. Use 'gmake p-obj' to print $(obj) variable.
p-%:
	@echo $* = $($*)
	@echo $*\'s origin is $(origin $*)
