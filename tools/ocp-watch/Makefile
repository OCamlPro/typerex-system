PREFIX=/usr/local
OBUILD=../../_obuild

all:
	ocp-build ocp-watch
	$(OBUILD)/ocp-watch/ocp-watch.asm install
	cp $(OBUILD)/ocp-watch/ocp-watch.asm $(PREFIX)/bin/ocp-watch
