open Base
open Sexp_pretty

type expr =
  | Ident of string
  | String of string
  | Symbol of string
  | Fixnum of int
  | Bool of bool
  | List of expr list
  | Listd of expr list * expr
  | Vector of expr list
  | Application of expr * expr list
  (* Special Forms *)
  | Quote of expr
  | Lambda of expr list * body
  | If of expr * expr
  | Ife of expr * expr * expr
  | Set of expr * expr
  (* TODO Include the other 2 posibble Cond Forms *)
  | Cond of (expr * expr list) list * expr option
  | Case of expr * (expr list option * expr list) * expr list option
  (* TODO Implement Do *)
  | Do
  | Or of expr list
  | And of expr list
  | Not of expr
  | Begin of expr list
  (* TODO Named let-Loop  *)
  | Let of (expr * expr) list * body
  | Lets of (expr * expr) list * body
  | Letr of (expr * expr) list * body
and definition =
  | Defv of string * expr
  | Deff of string * string list * body
  | Deffl of string * string list * string * body
and form =
  | Def of definition
  | Expr of expr
and body = { defs : definition list; exprs : expr list }

let indent = 2

let pp_sexp body = "(" ^ body ^ ")"

let pp_arglist arglist =
  List.fold arglist ~init:"" ~f:(fun acc x -> acc ^ " " ^ x)

let rec pp_body {defs; exprs} depth =
  let newd = depth + indent in
  let defsstr = List.fold defs ~init:"" ~f:(fun acc x -> acc ^ (pp_def x newd)) in
  let exprsstr = List.fold exprs ~init:"" ~f:(fun acc x -> acc ^ (pp_expr x newd)) in
  defsstr ^ exprsstr
and pp_def def depth=
  let newd = depth + indent in
  match def with
  | Defv (name, expr) -> pp_sexp ( name ^ " " ^ ( pp_expr expr newd) )
  | Deff (name, arglist, body) -> pp_sexp
                                    (pp_sexp name ^ " " ^ (pp_arglist arglist)) ^ "\n"
                                  ^ (pp_body body newd) ^ "\n"
  | Deffl (name, arglist, listarg, body) -> pp_sexp
                                    (pp_sexp name ^ " " ^ (pp_arglist arglist) ^ " " ^ listarg) ^ "\n"
                                  ^ (pp_body body newd) ^ "\n"
and pp_expr_list lst depth ?(expr=None) ?(pars=true) () =
  let newd = depth + indent in
  let expr_lst =
    match expr with
    | None -> (List.fold lst ~init:"" ~f:(fun acc x -> acc ^ " " ^ (pp_expr x newd)))
    | Some expr -> (List.fold lst ~init:"" ~f:(fun acc x -> acc ^ " " ^ (pp_expr x newd)) ^ " . " ^ (pp_expr expr newd))
  in if pars then "(" ^ expr_lst ^ ")" else expr_lst
and pp_var_expr_list lst depth =
  let newd = depth + indent in
  pp_sexp
    (List.fold lst
       ~init:""
       ~f:(fun acc (var, expr) -> acc ^ (pp_sexp ((pp_expr var newd) ^ " " ^ (pp_expr expr newd))) ^ "\n" ) ) ^ "\n"
and pp_expr expr depth =
  let newd = depth + indent in
  match expr with
  | Ident str -> str
  | String str -> " " ^ str ^ " "
  | Symbol str -> str
  | Fixnum num -> Int.to_string num
  | Bool bool -> if bool then "#t" else "#f"
  | List exprs -> pp_expr_list exprs newd ()
  | Listd (exprs, expr) -> pp_expr_list exprs newd ~expr:(Some expr) ()
  | Vector exprs -> "#" ^ pp_sexp (List.fold exprs ~init:"" ~f:(fun acc x -> acc ^ (pp_expr x newd)))
  | Application (expr, exprs) -> pp_sexp ((pp_expr expr newd) ^ (pp_expr_list exprs newd ~pars:false ())) ^ "\n"
  | Quote expr -> pp_sexp ("quote " ^ (pp_expr expr newd))
  | Lambda (exprs, body) -> pp_sexp ((pp_expr_list exprs newd ()) ^ (pp_body body newd))
  | If (expr1, expr2) -> pp_sexp ("if " ^ (pp_expr expr1 newd) ^ "\n" ^ (pp_expr expr2 newd) )
  | Ife (expr1, expr2, expr3) -> pp_sexp ("if " ^ (pp_expr expr1 newd) ^ "\n" ^ (pp_expr expr2 newd) ^ "\n" ^ (pp_expr expr3 newd) )
  | Set (expr1, expr2) -> pp_sexp ("set " ^ (pp_expr expr1 newd) ^ " " ^ (pp_expr expr2 newd))
  | Cond _ -> "Cond not implemented"
  | Case _ -> "Case not implemented"
  | Do -> "Not implemented"
  | Or (exprs) -> pp_sexp ("or " ^ (pp_expr_list exprs newd ~pars:false ()))
  | And (exprs) -> pp_sexp ("or " ^ (pp_expr_list exprs newd ~pars:false ()))
  | Not (expr) -> pp_sexp ("not " ^ (pp_expr expr newd))
  | Begin (exprs) -> pp_sexp ("Begin" ^ (pp_expr_list exprs newd ~pars:false ()))
  | Let (var_expr_list, body) -> pp_sexp ("let " ^ (pp_var_expr_list var_expr_list newd) ^ (pp_body body newd))
  | Lets (var_expr_list, body) -> pp_sexp ("let* " ^ (pp_var_expr_list var_expr_list newd) ^ (pp_body body newd))
  | Letr (var_expr_list, body) -> pp_sexp ("letrec " ^ (pp_var_expr_list var_expr_list newd) ^ (pp_body body newd))

(* Quick hack for simple formatting *)
let pp_form form =
  sexp_of_string
    (match form with
     | Def def -> pp_def def 0
     | Expr expr -> pp_expr expr 0) |> pretty_string (Config.create ~interpret_atom_as_sexp:true ())
