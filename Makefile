.PHONY: test compile clean package packageName

EL := $(wildcard *.el)
ELC := $(patsubst %.el,%.elc,$(wildcard *.el))
TAR := gtar
JSON := json

%.elc: %.el
	emacs -Q -batch -L `pwd` -f batch-byte-compile $<

test: ph-pkg.el
	$(MAKE) -C test fixtures
	@for idx in test/test_*; do \
		printf '* %s\n' $$idx ; \
		./$$idx ; \
		[ $$? -ne 0 ] && exit 1 ; \
	done; :

compile: $(ELC)

ph-pkg.el: ph-meta.el
	bin/ph-make-pkg

packageName: ph-pkg.el
	$(eval PKG_NAME := $(shell bin/ph-pkg2json $< | $(JSON) -a -d- name version))
	@if [ "$(PKG_NAME)" = "" ] ; then \
		echo "cannot extract pkg name"; \
		exit 1; \
	fi; :

package: packageName
	$(TAR) --transform='s,^,$(PKG_NAME)/,S' -cf $(PKG_NAME).tar \
		$(EL) README

clean: packageName
	rm -rf $(ELC) $(PKG_NAME).tar
	$(MAKE) -C test $@

# Debug. Use 'gmake p-obj' to print $(obj) variable.
p-%:
	@echo $* = $($*)
	@echo $*\'s origin is $(origin $*)
