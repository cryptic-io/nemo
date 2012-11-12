all: ebin/
	(cd deps/periodically;$(MAKE) all)
	(cd src;$(MAKE) all)

edoc:
	(cd src;$(MAKE) edoc)

test:
	(cd src;$(MAKE) test)

clean:
	(cd deps/periodically;$(MAKE) clean)
	(cd src;$(MAKE) clean)

clean_plt:
	(cd src;$(MAKE) clean_plt)

dialyzer:
	(cd src;$(MAKE) dialyzer)

ebin/:
	@mkdir -p ebin
