NAME=setml
VERSION=$(shell cat ${NAME}.opam | egrep '^version: ' | cut -d '"' -f2)
BUG_REPORTS=$(shell cat ${NAME}.opam | egrep '^bug-reports: ' | cut -d '"' -f2)
BUILD_TIME=$(shell date -u +"%Y-%m-%d %T")
BUILD_HASH=$(shell git rev-parse HEAD | cut -c 1-7 2>/dev/null || echo "")
OCAML_VERSION=$(shell ocaml -vnum)
INFO_FILE=src/server/info.ml

.PHONY: default all utop setup_tests test test_processes test_sequential test_async test_client clean

default: all

# Build one library and one standalone executable that implements
# multiple subcommands and uses the library.
# The library can be loaded in utop for interactive testing.
all:
	@cp ${INFO_FILE} /tmp/info.ml
	@sed --in-place="" 's/version = "[^"]*"/version = "${VERSION}"/' ${INFO_FILE}
	@sed --in-place="" 's/build_time = "[^"]*"/build_time = "${BUILD_TIME}"/' ${INFO_FILE}
	@sed --in-place="" 's/build_hash = "[^"]*"/build_hash = "${BUILD_HASH}"/' ${INFO_FILE}
	@sed --in-place="" 's/ocaml_version = "[^"]*"/ocaml_version = "${OCAML_VERSION}"/' ${INFO_FILE}
	@sed --in-place="" 's|bug_reports = "[^"]*"|bug_reports = "${BUG_REPORTS}"|' ${INFO_FILE}
	dune build @install
	@test -f /tmp/info.ml && mv /tmp/info.ml ${INFO_FILE}
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
