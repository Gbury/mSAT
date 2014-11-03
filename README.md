# MSAT

MSAT is an OCaml library that features a modular SAT-solver and some
extensions (including SMT). This is *work in progress*.


It derives from [Alt-Ergo Zero](http://cubicle.lri.fr/alt-ergo-zero).

The following theories should be supported:

- Equality and uninterpreted functions
- Arithmetic (linear, non-linear, integer, real)
- Enumerated data-types

## COPYRIGHT

This program is distributed under the Apache Software License version
2.0. See the enclosed file `LICENSE`.


## INSTALLATION

### Via opam

Once the package is on [opam](http://opam.ocaml.org), just `opam install msat`.
For the development version, use:

    opam pin add msat https://github.com/Gbury/mSAT.git

### Manual installation You will need ocamlfind. The command is:

    make install



