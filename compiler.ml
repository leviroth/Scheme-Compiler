open Base
open Stdio
open Ast

let parse_with_error lexbuf =
  try Parser.form (Lexer.read) lexbuf with
  | Lexer.TokenError msg ->
    print_endline ("Syntax Fehler:\n" ^ msg);
    None
  | Parser.Error ->
    print_endline "Parser Fehler";
    Caml.exit (-1)

let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some value ->
    print_endline "hey";
    parse_and_print lexbuf
  | None -> print_endline "Reached EOF"

let () =
  let lexbuf = Lexing.from_channel stdin in
  Stdlib.flush_all ();
  parse_and_print lexbuf;
