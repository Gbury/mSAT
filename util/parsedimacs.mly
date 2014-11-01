/*  Copyright 2005 INRIA  */

%{
    exception Clause_ending of int
%}

%token <int> LIT
%token P CNF EOL EOF

%start file
%type <int list list> file

%%

/* DIMACS syntax */

file:
  | EOF                         { [] }
  | P CNF LIT LIT EOL clause_list   { $6 }
;
clause_list:
  | EOF                 { [] }
  | clause clause_list  { $1 :: $2 }
;
clause:
  /* Clause always ends with a '0' */
  | LIT EOL         { if $1 = 0 then [] else raise (Clause_ending $1) }
  | LIT clause      { $1 :: $2 }
;
