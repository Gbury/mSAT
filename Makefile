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

all: build-dev test

build:
	jbuilder build $(OPTS) @install

build-dev:
	jbuilder build $(OPTS) @install --dev

test: build
	@echo "run API tests…"
	jbuilder runtest
	@echo "run benchmarks…"
	# @/usr/bin/time -f "%e" ./tests/run smt
	@/usr/bin/time -f "%e" ./tests/run mcsat

enable_log:
	cd src/core; ln -sf log_real.ml log.ml

disable_log:
	cd src/core; ln -sf log_dummy.ml log.ml

clean:
	jbuilder clean

install: build-install
	jbuilder install

uninstall:
	jbuilder uninstall

doc:
	jbuilder build $(OPTS) @doc


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
	while find src/ tests/ -print0 | xargs -0 inotifywait -e delete_self -e modify ; do \
		echo "============ at `date` ==========" ; \
		make $(WATCH); \
	done

.PHONY: clean doc all bench install uninstall remove reinstall enable_log disable_log bin test
