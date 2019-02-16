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

ocp-indent:
	@which ocp-indent > /dev/null || { \
	  	echo 'ocp-indent not found; please run `opam install ocp-indent`'; \
		exit 1 ; \
	  }

reindent: ocp-indent
	@find src '(' -name '*.ml' -or -name '*.mli' ')' -print0 | xargs -0 echo "reindenting: "
	@find src '(' -name '*.ml' -or -name '*.mli' ')' -print0 | xargs -0 ocp-indent -i

WATCH=all
watch:
	@dune build @all -w

.PHONY: clean doc all bench install uninstall remove reinstall bin test
