(*  Copyright 2005 INRIA  *)
{
open Lexing;;
open Parsedimacs;;
}

let number = ['0' - '9']+

rule token = parse
  | ' '                     { token lexbuf }
  | 'p'                     { P }
  | "cnf"                   { CNF }
  | '\n'                    { EOL }
  | "c " [^ '\n']* '\n'      { token lexbuf }
  | ['-']? number           { LIT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof                     { EOF }
