# Edit this for your own project dependencies
OPAM_DEPENDS="ocamlfind ocamlbuild"

opam install ${OPAM_DEPENDS}

make lib
make bin
make test
