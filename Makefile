# copyright (c) 2014, guillaume bury

LOG=build.log
COMP=ocamlbuild -log $(LOG) -use-ocamlfind
FLAGS=
DOC=msat.docdir/index.html
BIN=main.native
NAME=msat

LIB=$(addprefix $(NAME), .cma .cmxa .cmxs)

all: lib test

lib:
	$(COMP) $(FLAGS) $(LIB)

doc:
	$(COMP) $(FLAGS) $(DOC)

bin:
	$(COMP) $(FLAGS) $(BIN)
	cp $(BIN) $(NAME) && rm $(BIN)

test: bin
	@/usr/bin/time -f "%e" ./tests/run smt
	@/usr/bin/time -f "%e" ./tests/run mcsat

enable_log:
	cd util; ln -sf log_real.ml log.ml

disable_log:
	cd util; ln -sf log_dummy.ml log.ml

log:
	cat _build/$(LOG) || true

clean:
	$(COMP) -clean

TO_INSTALL=META $(addprefix _build/src/,$(LIB) $(NAME).a $(NAME).cmi)

install: lib
	ocamlfind install msat $(TO_INSTALL)

uninstall:
	ocamlfind remove msat

reinstall: all
	ocamlfind remove msat || true
	ocamlfind install msat $(TO_INSTALL)

.PHONY: clean doc all bench install uninstall reinstall enable_log disable_log bin test
