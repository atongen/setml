.PHONY: default all utop setup_tests test test_processes test_sequential test_async test_client clean

default: all

# Build one library and one standalone executable that implements
# multiple subcommands and uses the library.
# The library can be loaded in utop for interactive testing.
all:
	dune build @install
	@test -L bin || ln -s _build/install/default/bin .

# Launch utop such that it finds our library.
utop: all
	OCAMLPATH=_build/install/default/lib:$(OCAMLPATH) utop

setup_tests: all
	export SETML_ENV=test

test_processes: setup_tests
	./bin/test_processes -runner processes

test_sequential: setup_tests
	./bin/test_sequential -runner sequential

test_async: setup_tests
	./bin/test_async -runner processes

build_client:
	npm run-script build

test_client: build_client
	npm run-script test

test: test_processes test_sequential test_async test_client

clean:
	dune clean
	npm run-script clean
