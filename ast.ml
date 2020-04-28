open Base

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

let pp_sexp body = "(" ^ body ^ ")"

let pp_arglist arglist =
  List.fold arglist ~init:"" ~f:(fun acc x -> acc ^ " " ^ x)

let rec pp_body {defs; exprs} =
  let defsstr = List.fold defs ~init:"" ~f:(fun acc x -> acc ^ (pp_def x)) in
  let exprsstr = List.fold exprs ~init:"" ~f:(fun acc x -> acc ^ (pp_expr x)) in
  defsstr ^ exprsstr
and pp_def def =
  match def with
  | Defv (name, expr) -> pp_sexp ( name ^ " " ^ ( pp_expr expr ) )
  | Deff (name, arglist, body) -> pp_sexp
                                    (pp_sexp name ^ " " ^ (pp_arglist arglist)) ^ "\n"
                                  ^ (pp_body body) ^ "\n"
  | Deffl (name, arglist, listarg, body) -> pp_sexp
                                    (pp_sexp name ^ " " ^ (pp_arglist arglist) ^ " " ^ listarg) ^ "\n"
                                  ^ (pp_body body) ^ "\n"
and pp_expr_list lst ?(expr=None) ?(pars=true) () =
  let expr_lst =
    match expr with
    | None -> (List.fold lst ~init:"" ~f:(fun acc x -> acc ^ " " ^ (pp_expr x)))
    | Some expr -> (List.fold lst ~init:"" ~f:(fun acc x -> acc ^ " " ^ (pp_expr x)) ^ " . " ^ pp_expr expr)
  in if pars then "(" ^ expr_lst ^ ")" else expr_lst
and pp_var_expr_list lst =
  pp_sexp
    (List.fold lst
       ~init:""
       ~f:(fun acc (var, expr) -> acc ^ (pp_sexp ((pp_expr var) ^ " " ^ (pp_expr expr))) ^ "\n" ) ) ^ "\n"
and pp_expr expr =
  match expr with
  | Ident str -> str
  | String str -> " " ^ str ^ " "
  | Symbol str -> str
  | Fixnum num -> Int.to_string num
  | Bool bool -> if bool then "#t" else "#f"
  | List exprs -> pp_expr_list exprs ()
  | Listd (exprs, expr) -> pp_expr_list exprs ~expr:(Some expr) ()
  | Vector exprs -> "#" ^ pp_sexp (List.fold exprs ~init:"" ~f:(fun acc x -> acc ^ (pp_expr x)))
  | Application (expr, exprs) -> pp_sexp ((pp_expr expr) ^ (pp_expr_list exprs ~pars:false ())) ^ "\n"
  | Quote expr -> pp_sexp ("quote " ^ (pp_expr expr))
  | Lambda (exprs, body) -> pp_sexp ((pp_expr_list exprs ()) ^ (pp_body body))
  | If (expr1, expr2) -> pp_sexp ("if " ^ (pp_expr expr1) ^ "\n" ^ (pp_expr expr2) )
  | Ife (expr1, expr2, expr3) -> pp_sexp ("if " ^ (pp_expr expr1) ^ "\n" ^ (pp_expr expr2) ^ "\n" ^ (pp_expr expr3) )
  | Cond _ -> "Cond not implemented"
  | Case _ -> "Case not implemented"
  | Do -> "Not implemented"
  | Or (exprs) -> pp_sexp ("or " ^ (pp_expr_list exprs ~pars:false ()))
  | And (exprs) -> pp_sexp ("or " ^ (pp_expr_list exprs ~pars:false ()))
  | Not (expr) -> pp_sexp ("not " ^ (pp_expr expr))
  | Begin (exprs) -> pp_sexp ("Begin" ^ (pp_expr_list exprs ~pars:false ()))
  | Let (var_expr_list, body) -> pp_sexp ("let " ^ (pp_var_expr_list var_expr_list) ^ (pp_body body))
  | Lets (var_expr_list, body) -> pp_sexp ("let* " ^ (pp_var_expr_list var_expr_list) ^ (pp_body body))
  | Letr (var_expr_list, body) -> pp_sexp ("letrec " ^ (pp_var_expr_list var_expr_list) ^ (pp_body body))

let pp_form form =
  match form with
  | Def def -> pp_def def
  | Expr expr -> pp_expr expr
