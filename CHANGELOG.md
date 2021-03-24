# CHANGES

## 0.9.1

- add `on_conflit` callback
- fix termination issue when using `push_decision_lit` from plugin

## 0.9

- feat: allow the theory to ask for some literals to be decided on
- feat: allow to set the default polarity of variables at creation time

## 0.8.3

- support containers 3.0

## 0.8.2

- fix opam file
- fix: allow conflicts below decision level in `Make_cdcl_t`

## 0.8.1

- fixes in `Heap`
- package for `msat-bin`
- use `iter` instead of `sequence` in dune and opam files
- more docs

## 0.8

big refactoring, change of API with fewer functions, etc.
see `git log` for more details.

## 0.6.1

- add simple functor for DOT backend
- various bugfixes

## 0.6

### Feature

- An already instantiated sat solver in the Sat module
- A `full_slice` function for running possibly expensive satisfiability
  tests (in SMT) when a propositional model has been found
- Forgetful propagations: propagations whose reason (i.e clause) is not watched

## 0.5.1

### Bug

- Removed some needless allocations

### Breaking

- Better interface for mcsat propagations

### Feature

- Allow level 0 semantic propagations

## 0.5

### Bug

- Grow heap when adding local hyps
- Avoid forgetting some one atom clauses
- Fixed a bug for propagations at level 0
- Late propagations need to be re-propagated
- Fixed conflict at level 0
- Avoid forgetting some theory conflict clauses

### Breaking

- Changed `if_sat` interface

## 0.4.1

### Bug

- fix bug in `add_clause`

## 0.4

- performance improvements
- many bugfixes
- more tests

### Breaking

- remove push/pop (source of many bugs)
- replaced by solve : assumptions:lit list -> unit -> result

### Features

- Accept late conflict clauses
- cleaner API, moving some types outside the client-required interface

## 0.3

### Features

- Proofs for atoms at level 0
- Compatibility with ocaml >= 4.00
- Released some restrictions on dummy sat theories

## 0.2

### Breaking

- Log argument has been removed from functors
- All the functors now take a dummy last argument to ensure the solver modules are unique

### Features

- push/pop operations
- access to decision level when evaluating literals

