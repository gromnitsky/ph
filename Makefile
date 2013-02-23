.PHONY: test compile clean package

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

package: ph-pkg.el
	pkg_name=`bin/ph-pkg2json $< | $(JSON) -a -d- name version` && \
		$(TAR) --transform="s,^,$$pkg_name/,S" -cf $$pkg_name.tar \
			$(EL) README

clean:
	rm -rf $(ELC) $(PKG_NAME).tar
	$(MAKE) -C test $@

# Debug. Use 'gmake p-obj' to print $(obj) variable.
p-%:
	@echo $* = $($*)
	@echo $*\'s origin is $(origin $*)
