.PHONY: test compile clean

ELC := $(patsubst %.el,%.elc,$(wildcard *.el))

%.elc: %.el
	emacs -Q -batch -f batch-byte-compile $<

test:
	$(MAKE) -C test fixtures
	@cd test; for idx in test_*; do \
		printf '* %s\n' $$idx ; \
		./$$idx ; \
		[ $$? -ne 0 ] && exit 1 ; \
	done; :

compile: $(ELC)

clean:
	rm -rf $(ELC)
	$(MAKE) -C test $@

# Debug. Use 'gmake p-obj' to print $(obj) variable.
p-%:
	@echo $* = $($*)
	@echo $*\'s origin is $(origin $*)
