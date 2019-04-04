/*  Copyright 2005 INRIA  */

%{
  let lnum pos = pos.Lexing.pos_lnum
  let cnum pos = pos.Lexing.pos_cnum - pos.Lexing.pos_bol
  let pp_pos out (start,stop) =
    Format.fprintf out "(at %d:%d - %d:%d)"
      (lnum start) (cnum start) (lnum stop) (cnum stop)
%}

%token <int> LIT
%token ZERO
%token P CNF EOF

%start file
%type <int list list> file

%%

/* DIMACS syntax */

prelude:
  | P CNF LIT LIT { () }

clauses:
  | { [] }
  | clause clauses { $1 :: $2 }

file:
  | prelude clauses EOF { $2 }

clause:
  | ZERO { [] }
  | LIT clause { $1 :: $2 }
