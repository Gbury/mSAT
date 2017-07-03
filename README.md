# MSAT  [![Build Status](https://travis-ci.org/Gbury/mSAT.svg?branch=master)](https://travis-ci.org/Gbury/mSAT)

MSAT is an OCaml library that features a modular SAT-solver and some
extensions (including SMT).


It derives from [Alt-Ergo Zero](http://cubicle.lri.fr/alt-ergo-zero).


## COPYRIGHT

This program is distributed under the Apache Software License version
2.0. See the enclosed file `LICENSE`.

## Documentation

See https://gbury.github.io/mSAT/

## INSTALLATION

### Via opam

Once the package is on [opam](http://opam.ocaml.org), just `opam install msat`.
For the development version, use:

    opam pin add msat https://github.com/Gbury/mSAT.git

### Manual installation

You will need ocamlfind and ocamlbuild. The command is:

    make install

## USAGE

### Generic SAT/SMT Solver

A modular implementation of the SMT algorithm can be found in the `Msat.Solver` module,
as a functor which takes two modules :

  - A representation of formulas (which implements the `Formula_intf.S` signature)

  - A theory (which implements the `Theory_intf.S` signature) to check consistence of assertions.

  - A dummy empty module to ensure generativity of the solver (solver modules heavily relies on
  side effects to their internal state)

### Sat Solver

A ready-to-use SAT solver is available in the Sat module. It can be used
as shown in the following code :

```ocaml
(* Module initialization *)
module Sat = Msat.Sat.Make()
module E = Msat.Sat.Expr (* expressions *)

(* We create here two distinct atoms *)
let a = E.fresh ()    (* A 'new_atom' is always distinct from any other atom *)
let b = E.make 1      (* Atoms can be created from integers *)

(* We can try and check the satisfiability of some clauses --
   here, the clause [a or b].
   Sat.assume adds a list of clauses to the solver. *)
let() = Sat.assume [[a; b]]
let res = Sat.solve ()        (* Should return (Sat.Sat _) *)

(* The Sat solver has an incremental mutable state, so we still have
   the clause [a or b] in our assumptions.
   We add [not a] and [not b] to the state. *)
let () = Sat.assume [[E.neg a]; [E.neg b]]
let res = Sat.solve ()        (* Should return (Sat.Unsat _) *)
```


#### Formulas API

Writing clauses by hand can be tedious and error-prone.
The functor `Msat.Tseitin.Make` proposes a formula AST (parametrized by
atoms) and a function to convert these formulas into clauses:

```ocaml
(* Module initialization *)
module Sat = Msat.Sat.Make()
module E = Msat.Sat.Expr (* expressions *)
module F = Msat.Tseitin.Make(E)

(* We create here two distinct atoms *)
let a = E.fresh ()    (* A fresh is always distinct from any other atom *)
let b = E.make 1      (* Atoms can be created from integers *)

(* Let's create some formulas *)
let p = F.make_atom a
let q = F.make_atom b
let r = F.make_and [p; q]
let s = F.make_or [F.make_not p; F.make_not q]

(* We can try and check the satisfiability of the given formulas *)
let () = Sat.assume (F.make_cnf r)
let _ = Sat.solve ()        (* Should return (Sat.Sat _) *)

(* The Sat solver has an incremental mutable state, so we still have
 * the formula 'r' in our assumptions *)
let () = Sat.assume (F.make_cnf s)
let _ = Sat.solve ()        (* Should return (Sat.Unsat _) *)
```

