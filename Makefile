all: ebin/
	for dep in $(wildcard deps/*) ; do \
	(cd $$dep;$(MAKE) all) ; \
	done
	(cd src;$(MAKE) all)

edoc:
	(cd src;$(MAKE) edoc)

test:
	(cd src;$(MAKE) test)

clean:
	for dep in $(wildcard deps/*) ; do \
	(cd $$dep;$(MAKE) clean) ; \
	done
	(cd src;$(MAKE) clean)

clean_plt:
	(cd src;$(MAKE) clean_plt)

dialyzer:
	(cd src;$(MAKE) dialyzer)

ebin/:
	@mkdir -p ebin
