{
  open Lexing
  open Parser
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
let digit = ['0' - '9']

(* Variables - lifted out of the language definition *)
let special_init = ['!' '$' '%' '&' '*' '+' '-' '/' ':' '<' '=' '>' '?' '^' '_' '~']
let special_sub = ['+' '-' '.' '@']
let initial = letter | special_init
let subsequent = initial | digit | special_sub
let ident = initial subsequent*

(* First-class values *)
let fixnum = digit+
let string = '\"' _* '\"'

(* Whitespace *)
let white = [' ' '\t' ]+
let newline = '\r' | '\n' | "\r\n" | "\n\r"

(* TODO check Case-Sensivity*)

rule read =
  parse
  | white { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | ident { Ident (Lexing.lexeme lexbuf) }

  | "#t" { True }
  | "#f" { False }
  | fixnum { Fixnum (Int.of_string (Lexing.lexeme lexbuf)) }
  | string { String (Lexing.lexeme lexbuf) }
  | "lambda" { Lambda }

  | "define" { Def }
  | "let" { Let } | "let*" { Lets } | "letr" { Letr }
  | "begin" { Begin }
  | "do" { Do }
  |  "if" { If } | "case" { Case } | "cond" { Cond } | "else" { Else }
  | "or" { Or } | "not" { Not } | "and" { And }
  | '(' { Lpar } | ')' { Rpar }
  | '\'' { Quote } | "quote" { Quote } | "=>" { Arrow } | "#" { Hash }
  | eof { Eof }
  | _ { raise (TokenError ("Unerwartetes Zeichen:" ^ Lexing.lexeme lexbuf)) }
