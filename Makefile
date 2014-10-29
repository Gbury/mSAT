# copyright (c) 2014, guillaume bury

LOG=build.log
COMP=ocamlbuild -log $(LOG) -use-ocamlfind -package num,zarith,unix -classic-display
FLAGS=
DIRS=-Is smt,common
DOC=msat.docdir/index.html

NAME=msat

LIB=$(addprefix $(NAME), .cma .cmxa .cmxs)

all:$(LIB)

$(LIB):
	$(COMP) $(FLAGS) $(DIRS) $(LIB)

doc:
	$(COMP) $(FLAGS) $(DIRS) $(DOC)

clean:
	$(COMP) -clean

TO_INSTALL=META $(addprefix _build/,$(LIB) $(NAME).a $(NAME).cmi)

install: all
	ocamlfind install msat $(TO_INSTALL)

uninstall:
	ocamlfind remove msat

.PHONY: clean doc all install uninstall
