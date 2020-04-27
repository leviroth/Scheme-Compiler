{
  open Lexing
  open Tok
  open Base

exception TokenError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}


(* general *)
let letter = ['a'-'z' 'A'-'Z']

(* Variables - lifted out of the language definition *)
let special_init = ['!' '$' '%' '&' '*' '/' ':' '<' '=' '>' '?' '^' '_' '~']
let special_sub = ['+' '-' '.' '@']
let initial = letter | special_init
let subsequent = inital | digit | special_sub
let ident = initial subsequent*

(* First-class values *)
let fixnum = ['1'-'9']+
let string = '\"' _* '\"'

(* Whitespace *)
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read =
  parse
  | white { read lexbuf }

  | ident { Ident (Char.of_string (Lexing.lexeme lexbuf)) }

  | "#t" { True }
  | "#f" { False }
  | fixnum { Fixnum (Int.of_string (Lexing.lexme lexbuf)) }
  | string { String (Lexing.lexeme lexbuf) }
  | lambda { Lambda }

  | "define" { Def }
  | "let" { Let } | "let*" { Lets } | "letr" { Letr }
  | "begin" { Begin }
  | "do" { Do }
  | 'or' { OR } | 'not' { NOT } | 'and' { AND }
  | '(' { LPAR } | ')' { RPAR }
  | '\'' { Quote } | "quote" { Quote } | "=>" { Arrow } | "#" { Hash }
  | eof { EOF }
  | _ { raise (TokenError ("Unerwartetes Zeichen:" ^ Lexing.lexeme lexbuf)) }
