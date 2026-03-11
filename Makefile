# copyright (c) 2014, guillaume bury
# copyright (c) 2017, simon cruanes

J?=3
OPTS= -j $(J)

build:
	@dune build $(OPTS) @install --profile=release

dev: build-dev test

build-dev:
	@dune build $(OPTS) @install

test: build-dev
	@echo "run tests…"
	@OCAMLRUNPARAM=b dune runtest --force --no-buffer

clean:
	@dune clean

install: build-install
	@dune install

uninstall:
	@dune uninstall

doc:
	@dune build $(OPTS) @doc


reinstall: | uninstall install

format:
	@dune build $(DUNE_OPTS) @fmt --auto-promote

format-check:
	@dune build $(DUNE_OPTS) @fmt --display=quiet

WATCH=all
watch:
	@dune build @all -w

.PHONY: clean doc all bench install uninstall remove reinstall bin test
