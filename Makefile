# copyright (c) 2014, guillaume bury

LOG=build.log
COMP=ocamlbuild -log $(LOG) -use-ocamlfind
FLAGS=
DOC=msat.docdir/index.html msat_sat.docdir/index.html msat_smt.docdir/index.html
BIN=main.native
TEST_BIN=tests/test_api.native

NAME_OCAMLFIND=msat
NAME_BIN=msat
NAME_CORE=msat
NAME_SAT=msat_sat
NAME_SMT=msat_smt
NAME_MCSAT=msat_mcsat

LIB_CORE=$(addprefix $(NAME_CORE), .cma .cmxa .cmxs)
LIB_SAT=$(addprefix $(NAME_SAT), .cma .cmxa .cmxs)
LIB_SMT=$(addprefix $(NAME_SMT), .cma .cmxa .cmxs)
LIB_MCSAT=$(addprefix $(NAME_MCSAT), .cma .cmxa .cmxs)
LIB=$(LIB_CORE) $(LIB_SAT) # $(LIB_SMT) $(LIB_MCSAT)

all: lib test

lib:
	$(COMP) $(FLAGS) $(LIB)

doc:
	$(COMP) $(FLAGS) $(DOC)

bin: lib
	$(COMP) $(FLAGS) $(BIN)
	cp $(BIN) $(NAME_BIN) && rm $(BIN)

test_bin:
	$(COMP) $(FLAGS) $(TEST_BIN)

test: bin test_bin
	@echo "run API tests…"
	@./test_api.native
	@echo "run benchmarks…"
	@/usr/bin/time -f "%e" ./tests/run smt
	@/usr/bin/time -f "%e" ./tests/run mcsat

enable_log:
	cd src/util; ln -sf log_real.ml log.ml

disable_log:
	cd src/util; ln -sf log_dummy.ml log.ml

log:
	cat _build/$(LOG) || true

clean:
	$(COMP) -clean

ALL_NAMES = $(NAME_CORE) $(NAME_SAT) $(NAME_SMT)
TO_INSTALL_LIB=$(addsuffix .a, $(ALL_NAMES)) \
	       $(addsuffix .cmi, $(ALL_NAMES))
TO_INSTALL=META $(addprefix _build/src/,$(LIB) $(TO_INSTALL_LIB))

install: lib
	ocamlfind install $(NAME_OCAMLFIND) $(TO_INSTALL)

uninstall:
	ocamlfind remove $(NAME_OCAMLFIND)

reinstall: all
	ocamlfind remove $(NAME_OCAMLFIND) || true
	ocamlfind install $(NAME_OCAMLFIND) $(TO_INSTALL)

watch:
	while find src/ -print0 | xargs -0 inotifywait -e delete_self -e modify ; do \
		echo "============ at `date` ==========" ; \
		sleep 0.1; \
		make all; \
	done

.PHONY: clean doc all bench install uninstall reinstall enable_log disable_log bin test
