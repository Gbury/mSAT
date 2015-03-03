# MSAT

MSAT is an OCaml library that features a modular SAT-solver and some
extensions (including SMT). This is *work in progress*.


It derives from [Alt-Ergo Zero](http://cubicle.lri.fr/alt-ergo-zero).


## COPYRIGHT

This program is distributed under the Apache Software License version
2.0. See the enclosed file `LICENSE`.


## USAGE

### Sat Solver

A ready-to-use SAT solver is available in the Sat module. It can be used
as shown in the following code :

```ocaml
    (* Module initialization *)
    module F = Msat.Sat.Tseitin
    module Sat = Msat.Sat.Make(Msat.Log)

    (* We create here two distinct atoms *)
    let a = Sat.new_atom () (* A 'new_atom' is always distinct from any other atom *)
    let b = Sat.make 1      (* Atoms can be created from integers *)

    (* Let's create some formulas *)
    let p = F.make_and [a; b]
    let q = F.make_or [F.make_not a; F.make_not b]

    (* We can try and check the satisfiability of the given formulas *)
    Sat.assume (F.make_cnf p)
    let _ = Sat.solve ()        (* Should return Sat.Sat *)

    (* The Sat solver has an incremental mutable state, so we still have
     * the formula 'p' in our assumptions *)
    Sat.assume (F.make_cnf q)
    let _ = Sat.solve ()        (* Should return Sat.Unsat *)
```

### Generic SAT/SMT Solver

A modular implementation of the SMT algorithm can be found in the `Msat.Solver` module,
as a functor which takes three modules :

  - A log module for debug output

  - A representation of formulas (which implements the `Formula_intf.S` signature)

  - A theory (which implements the `Theory_intf.S` signature) to check consistence of assertions.

## INSTALLATION

### Via opam

Once the package is on [opam](http://opam.ocaml.org), just `opam install msat`.
For the development version, use:

    opam pin add msat https://github.com/Gbury/mSAT.git

### Manual installation You will need ocamlfind. The command is:

    make install



