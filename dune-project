(lang dune 3.0)

(generate_opam_files true)

(package
 (name msat)
 (synopsis "Library containing a SAT solver that can be parametrized by a theory")
 (version "0.9.1")
 (license "Apache-2.0")
 (authors "Simon Cruanes" "Guillaume Bury")
 (maintainers "guillaume.bury@gmail.com" "simon.cruanes.2007@m4x.org")
 (tags ("sat" "smt" "cdcl" "functor"))
 (homepage "https://github.com/Gbury/mSAT")
 (source (github Gbury/mSAT))
 (bug_reports "https://github.com/Gbury/mSAT/issues/")
 (depends
  (ocaml (>= 4.03))
  (dune (>= 3.0))
  (iter (>= 1.2))
  (containers (and :with-test (>= 2.8.1) (< 4.0)))
  (mdx :with-test)))

(package
 (name msat-bin)
 (synopsis "SAT solver binary based on the msat library")
 (version "0.9.1")
 (license "Apache-2.0")
 (authors "Simon Cruanes" "Guillaume Bury")
 (maintainers "guillaume.bury@gmail.com" "simon.cruanes.2007@m4x.org")
 (tags ("sat"))
 (homepage "https://github.com/Gbury/mSAT")
 (source (github Gbury/mSAT))
 (bug_reports "https://github.com/Gbury/mSAT/issues/")
 (depends
  (ocaml (>= 4.03))
  (dune (>= 3.0))
  (msat (= :version))
  (containers (and (>= 2.8.1) (< 4.0)))
  camlzip))
