
{
  type token = A | I of int | EOF
}

let space = [' ' '\t']
let nat = (['0'-'9'])+
let int = ('-' nat) | nat

rule token = parse
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | space { token lexbuf }
  | 'c' { skip_line lexbuf }
  | 'p' { skip_line lexbuf }
  | int { let i = int_of_string (Lexing.lexeme lexbuf) in I i }
  | 'a' { A }
  | eof { EOF }
  | _ as c
    {
      let msg = Printf.sprintf "lexer fails on char %c\n" c in
      failwith msg
    }

and skip_line = parse
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | eof { EOF }
  | _ { skip_line lexbuf }

