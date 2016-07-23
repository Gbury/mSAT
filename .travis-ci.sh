# Edit this for your own project dependencies
OPAM_DEPENDS="ocamlfind ocamlbuild"

export OPAMYES=1
export OPAMVERBOSE=1

opam init
opam switch ${OCAML_VERSION}
eval `opam config env`
opam install ${OPAM_DEPENDS}
eval `opam config env`

make lib
make bin
make test
