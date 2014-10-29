# copyright (c) 2014, guillaume bury

LOG=build.log
COMP=ocamlbuild -log $(LOG) -use-ocamlfind -package zarith,unix -classic-display
FLAGS=
DIRS=-Is smt,common
DOC=lib.docdir/index.html
LIB=msat.cma msat.cmxa msat.cmxs
GENERATED=$(MAIN) $(BIN) gmon.out

all:$(LIB)

$(LIB):
	$(COMP) $(FLAGS) $(DIRS) $(LIB)

doc:
	$(COMP) $(FLAGS) $(DIRS) $(DOC)

clean:
	$(COMP) -clean
	rm -f $(GENERATED)
