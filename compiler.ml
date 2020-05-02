open Base
open Stdio
open Ast

let parse_with_error lexbuf =
  try Parser.form (Lexer.read) lexbuf with
  | Lexer.TokenError msg ->
    print_endline ("Token Fehler:\n" ^ msg);
    None
  | Parser.Error ->
    print_endline "Parser Fehler";
    Caml.exit (-1)

let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some value ->
    print_s ([%sexp_of: Ast.form] value);
    value |> Desugar.desugar |> Ast.pp_form |> print_endline;
    parse_and_print lexbuf
  | None -> print_endline "Reached EOF"

let () =
  let lexbuf = Lexing.from_string "(or 1 2 3 4)" in
  parse_and_print lexbuf;
