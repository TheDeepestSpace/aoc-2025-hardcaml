INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

default:
	dune build

watch:
	dune build --watch --terminal-persistence=clear-on-rebuild-and-flush-history bin/generate.exe @runtest

test:
	dune test

install:
	dune install $(INSTALL_ARGS)

uninstall:
	dune uninstall $(INSTALL_ARGS)

reinstall: uninstall install

clean:
	dune clean

.PHONY: default watch test install uninstall reinstall clean
