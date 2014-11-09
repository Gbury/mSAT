# copyright (c) 2014, guillaume bury

LOG=build.log
COMP=ocamlbuild -log $(LOG) -use-ocamlfind -classic-display
FLAGS=
DIRS=-Is sat,smt,util,util/smtlib
DOC=msat.docdir/index.html
TEST=sat_solve.native

NAME=msat

LIB=$(addprefix $(NAME), .cma .cmxa .cmxs)

all:$(LIB) $(TEST)

$(LIB):
	$(COMP) $(FLAGS) $(DIRS) $(LIB)

doc:
	$(COMP) $(FLAGS) $(DIRS) $(DOC)

test: $(TEST)
	./tests/run

$(TEST): $(LIB)
	$(COMP) $(FLAGS) $(DIRS) $(TEST)

bench: $(TEST)
	cd bench && $(MAKE)

log:
	cat _build/$(LOG) || true

clean:
	$(COMP) -clean

TO_INSTALL=META $(addprefix _build/,$(LIB) $(NAME).a $(NAME).cmi)

install: all
	ocamlfind install msat $(TO_INSTALL)

uninstall:
	ocamlfind remove msat

reinstall: all
	ocamlfind remove msat || true
	ocamlfind install msat $(TO_INSTALL)

.PHONY: clean doc all bench install uninstall reinstall
