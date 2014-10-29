# Goals

- Functorise `solver.ml` with term module and theory module
    * SAT solver depends on actual boolean literals
    * Move terms and theories (SMT part) to another directory
- Add proof output as resolution
    * Each theory brings its own proof output (tautologies), somehow
    * pure resolution proofs between boolean clauses and theory tautologies
- Allow to plug one's code into boolean propagation
    * react upon propagation (possibly by propagating more, or side-effect)
    * more advanced/specific propagation (2-clauses)?

