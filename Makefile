# Invoke `make` to build, `make clean` to clean up, etc.

.PHONY: default all utop test clean build_server

default: all

# Build one library and one standalone executable that implements
# multiple subcommands and uses the library.
# The library can be loaded in utop for interactive testing.
all:
	jbuilder build @install
	@test -L bin || ln -s _build/install/default/bin .

# Launch utop such that it finds our library.
utop: all
	OCAMLPATH=_build/install/default/lib:$(OCAMLPATH) utop

# Build and run tests
test: all
	./bin/setml test

# Clean up
clean:
# Remove files produced by jbuilder.
	jbuilder clean