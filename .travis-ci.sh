# Edit this for your own project dependencies
OPAM_DEPENDS="ocamlfind ocamlbuild"

echo "yes" | sudo add-apt-repository ppa:avsm/ocaml42+opam12
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam
export OPAMYES=1
export OPAMVERBOSE=1
opam init
opam switch OCAML_VERSION
eval `opam config env`
opam install ${OPAM_DEPENDS}
eval `opam config env`
make lib
make bin
make test
