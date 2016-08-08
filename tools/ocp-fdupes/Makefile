all:
	ocp-build ocp-fdupes

test:
	rm -rf tests
	mkdir tests
	cp Makefile tests/Makefile.1
	chmod 777  tests/Makefile.1
	cp Makefile tests/Makefile.2
	chmod 777  tests/Makefile.2
	cp Makefile tests/Makefile.3
	chmod 666  tests/Makefile.3
	ls -li tests
	../../_obuild/ocp-fdupes/ocp-fdupes.asm tests
	ls -li tests
