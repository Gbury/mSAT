# copyright (c) 2014, guillaume bury

LOG=build.log
COMP=ocamlbuild -log $(LOG)
OCAMLFIND= -use-ocamlfind -tag package\(zarith\)
FLAGS=
DIRS=-Is smt,common
DOC=lib.docdir/index.html
LIB=sat.cma
GENERATED=$(MAIN) $(BIN) gmon.out

all:$(LIB)

$(LIB):
	$(COMP) $(FLAGS) $(DIRS) $(LIB)

doc:
	$(COMP) $(FLAGS) $(DIRS) $(DOC)

clean:
	$(COMP) -clean
	rm -f $(GENERATED)
