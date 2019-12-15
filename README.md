# MSAT  [![Build Status](https://travis-ci.org/Gbury/mSAT.svg?branch=master)](https://travis-ci.org/Gbury/mSAT)

MSAT is an OCaml library that features a modular SAT-solver and some
extensions (including SMT), derived from [Alt-Ergo Zero](http://cubicle.lri.fr/alt-ergo-zero).

It was presented at [ICFP 2017](https://icfp17.sigplan.org/event/ocaml-2017-papers-msat-an-ocaml-sat-solver),
using a [poster](https://github.com/Gbury/mSAT/blob/master/articles/icfp_2017.pdf)


## COPYRIGHT

This program is distributed under the Apache Software License version
2.0. See the enclosed file `LICENSE`.

## Documentation

See https://gbury.github.io/mSAT/

## INSTALLATION

### Via opam

Once the package is on [opam](http://opam.ocaml.org), just `opam install msat`.
For the development version, use:

```
opam pin add msat https://github.com/Gbury/mSAT.git
```

### Manual installation

You will need `dune` and `iter`. The command is:

```
$ make install
```

## USAGE

### Generic SAT/SMT Solver

A modular implementation of the SMT algorithm can be found in the `Msat.Solver` module,
as a functor which takes two modules :

  - A representation of formulas (which implements the `Formula_intf.S` signature)

  - A theory (which implements the `Theory_intf.S` signature) to check consistence of assertions.

  - A dummy empty module to ensure generativity of the solver (solver modules heavily relies on
  side effects to their internal state)

### Sat Solver

A ready-to-use SAT solver is available in the `Msat_sat` module
using the `msat.sat` library. It can be loaded
as shown in the following code :

```ocaml
# #require "msat";;
# #require "msat.sat";;
# #print_depth 0;; (* do not print details *)
```

Then we can create a solver and create some boolean variables:

```ocaml
module Sat = Msat_sat
module E = Sat.Int_lit (* expressions *)

let solver = Sat.create()

(* We create here two distinct atoms *)
let a = E.fresh ()    (* A 'new_atom' is always distinct from any other atom *)
let b = E.make 1      (* Atoms can be created from integers *)
```

We can try and check the satisfiability of some clauses — here, the clause `a or b`.
`Sat.assume` adds a list of clauses to the solver. Calling `Sat.solve`
will check the satisfiability of the current set of clauses, here "Sat".

```ocaml
# a <> b;;
- : bool = true
# Sat.assume solver [[a; b]] ();;
- : unit = ()
# let res = Sat.solve solver;;
val res : Sat.res = Sat.Sat ...
```

The Sat solver has an incremental mutable state, so we still have
the clause `a or b` in our assumptions.
We add `not a` and `not b` to the state, and get "Unsat".

```ocaml
# Sat.assume solver [[E.neg a]; [E.neg b]] () ;;
- : unit = ()
# let res = Sat.solve solver ;;
val res : Sat.res = Sat.Unsat ...
```

#### Formulas API

Writing clauses by hand can be tedious and error-prone.
The functor `Msat_tseitin.Make` in the library `msat.tseitin`
proposes a formula AST (parametrized by
atoms) and a function to convert these formulas into clauses:

```ocaml
# #require "msat.tseitin";;
```

```ocaml
(* Module initialization *)
module F = Msat_tseitin.Make(E)

let solver = Sat.create ()

(* We create here two distinct atoms *)
let a = E.fresh ()    (* A fresh atom is always distinct from any other atom *)
let b = E.make 1      (* Atoms can be created from integers *)

(* Let's create some formulas *)
let p = F.make_atom a
let q = F.make_atom b
let r = F.make_and [p; q]
let s = F.make_or [F.make_not p; F.make_not q]
```

We can try and check the satisfiability of the given formulas, by turning
it into clauses using `make_cnf`:

```ocaml
# Sat.assume solver (F.make_cnf r) ();;
- : unit = ()
# Sat.solve solver;;
- : Sat.res = Sat.Sat ...
```

```ocaml
# Sat.assume solver (F.make_cnf s) ();;
- : unit = ()
# Sat.solve solver ;;
- : Sat.res = Sat.Unsat ...
```

### CDCL(T): a Sudoku solver as an example

The directory `src/sudoku/` contains a simple Sudoku solver that
uses the interface `Msat.Make_cdcl_t`.
In essence, it implements the logical theory `CDCL(Sudoku)`.
The script `sudoku_solve.sh` compiles and runs the solver,
as does `dune exec src/sudoku/sudoku_solve.exe`.

It's able to parse sudoku grids denoted as 81 integers
(see `tests/sudoku/sudoku.txt` for example).

Here is a sample grid and the output from the solver (in roughly .5s):

```sh non-deterministic=command
$ echo '..............3.85..1.2.......5.7.....4...1...9.......5......73..2.1........4...9' > sudoku.txt
$ dune exec src/sudoku/sudoku_solve.exe -- sudoku.txt
...
#########################
solve grid:
  .........
  .....3.85
  ..1.2....
  ...5.7...
  ..4...1..
  .9.......
  5......73
  ..2.1....
  ....4...9
  
...
  987654321
  246173985
  351928746
  128537694
  634892157
  795461832
  519286473
  472319568
  863745219
  
###################
...
```
