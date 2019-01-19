# copyright (c) 2014, guillaume bury
# copyright (c) 2017, simon cruanes

BIN=main.native
TEST_BIN=tests/test_api.native

NAME=msat
J?=3
TIMEOUT?=30
TARGETS=src/bin/main.exe
OPTS= -j $(J)

LIB=$(addprefix $(NAME), .cma .cmxa .cmxs)

dev: build-dev test

build:
	@dune build $(OPTS) @install --profile=release

build-dev:
	@dune build $(OPTS) @install

test:
	@echo "run tests…"
	@dune runtest

test-full: test
	@echo "run benchmarks…"
	@/usr/bin/time -f "%e" ./tests/run sat

enable_log:
	cd src/core; ln -sf log_real.ml log.ml

disable_log:
	cd src/core; ln -sf log_dummy.ml log.ml

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

.PHONY: clean doc all bench install uninstall remove reinstall enable_log disable_log bin test
