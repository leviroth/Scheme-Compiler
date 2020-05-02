open Ast
open Base
open Traverse

let or2if expr =
  let rec builder exprs =
    match exprs with
    | [x] -> x
    | hd::tl -> Ife (hd, builder tl, Bool false)
    (* should never be reached *)
    | [] -> raise (Failure "Error: failed to desugar or") in
  match expr with
  | Or (Some exprs) -> builder exprs
  | Or (None) -> Bool false
  | x -> x

let and2if expr =
  let rec builder exprs =
    match exprs with
    | hd::tl -> Ife (hd, hd, builder tl)
    | [] -> Bool false in
  match expr with
  | Or (Some exprs) -> builder exprs
  | Or (None) -> Bool true
  | x -> x

(* TODO use fold with a list of funs *)
let desugar form =
  traverse form ~tfe:or2if ()
  (* ((form |> traverse) ~tfe:or2if () |> traverse) ~tfe:and2if () *)
