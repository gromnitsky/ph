.PHONY: clean fixtures

clean:
	rm -rf tmp

fixtures:
	@for i in `cat recreate.txt`; do \
		mkdir -p `dirname $$i`; \
		touch $$i; \
	done

# Debug. Use 'gmake p-obj' to print $(obj) variable.
p-%:
	@echo $* = $($*)
	@echo $*\'s origin is $(origin $*)
