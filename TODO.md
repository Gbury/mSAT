# Goals

## Main goals

- Modify theories to allow passing bulk of assumed literals
    * Create shared "vector" (formulas/atoms ?)
    * Allow theory propagation
- Cleanup code
    * Simplify Solver.Make functor
- Add proof output for smt/theories
    * Each theory brings its own proof output (tautologies), somehow
- Allow to plug one's code into boolean propagation
    * react upon propagation (possibly by propagating more, or side-effect)
    * more advanced/specific propagation (2-clauses)?
    * implement 'constraints' (see https://www.lri.fr/~conchon/TER/2013/3/minisat.pdf )
- Adapt old code for theories, inorder to plug it into new Solver Functor

## Long term goals

- max-sat/max-smt
