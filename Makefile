# copyright (c) 2014, guillaume bury

LOG=build.log
COMP=ocamlbuild -log $(LOG) -use-ocamlfind
FLAGS=
DOC=src/msat.docdir/index.html
BIN=main.native
TEST_BIN=tests/test_api.native

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

bench: bin
	OCAML_LANDMARKS=on,output=temporary,format=json ./$(NAME) tests/pigeon/hole8.cnf

test_bin:
	$(COMP) $(FLAGS) $(TEST_BIN)

test: bin test_bin
	@echo "run API tests…"
	@./test_api.native
	@echo "run benchmarks…"
	# @/usr/bin/time -f "%e" ./tests/run smt
	@/usr/bin/time -f "%e" ./tests/run mcsat

enable_log:
	cd src/util; ln -sf log_real.ml log.ml

disable_log:
	cd src/util; ln -sf log_dummy.ml log.ml

clean:
	$(COMP) -clean
	rm -rf $(NAME)

TO_INSTALL_LIB=$(addsuffix .a, $(NAME)) $(addsuffix .cmi, $(NAME))
TO_INSTALL=META $(addprefix _build/src/,$(LIB) $(TO_INSTALL_LIB))

install: lib
	ocamlfind install $(NAME) $(TO_INSTALL)
	if [ -d "$(NAME).docdir" ]; then \
		mkdir -p $(DOCDIR) ; \
		cp -v $(NAME).docdir/*.html $(NAME).docdir/*.css $(DOCDIR) ; \
	fi

uninstall:
	ocamlfind remove $(NAME)
	rm -rf $(DOCDIR)

reinstall: | uninstall install

.PHONY: clean doc all bench install uninstall remove reinstall enable_log disable_log bin test
