open Ast
open Base

let id_expr expr =
  match expr with
  | x -> x

let id_def def =
  match def with
  | x -> x

let traverse form ?(tfe=id_expr) ?(tfd=id_def) () =
  let rec recur_body {defs; exprs} =
    let defs = List.map defs ~f:(fun def -> recur_def def ) in
    let exprs = List.map exprs ~f:(fun expr -> recur_expr expr) in
    {defs; exprs}
  and recur_var_expr_list var_expr_list =
    match var_expr_list with
    | ((var, expr) as x)::xs -> (var, recur_expr expr) :: recur_var_expr_list xs
    | [] -> []
  and recur_def def =
    (match def with
    | Defv (name, expr) -> Defv(name, recur_expr expr)
    | Deff (name, arglist, body) -> Deff(name , arglist, recur_body body)
    | Deffl (name, arglist, listarg, body) -> Deffl(name, arglist, listarg, recur_body body)) |> tfd
  and recur_expr expr =
    (* let recur_expr = recur_expr transform in *)
    (match expr with
     | Application (expr, exprs) -> Application(expr, List.map exprs ~f:(fun expr -> recur_expr expr))
     | Quote expr -> Quote (recur_expr expr)
     | Lambda (args, body) -> Lambda(args, recur_body body)
     | If (expr1, expr2) -> If(recur_expr expr1, recur_expr expr2)
     | Ife (expr1, expr2, expr3) -> Ife(recur_expr expr1, recur_expr expr2, recur_expr expr3)
     | Set (var, expr2) -> Set(var, recur_expr expr2)
     | Cond _ as x -> x
     | Case _ as x -> x
     | Do -> Do
     | Or (Some exprs) -> Or(Some (List.map exprs ~f:(fun expr -> recur_expr expr)))
     | And (Some exprs) -> And(Some (List.map exprs ~f:(fun expr -> recur_expr expr)))
     | Not (expr) -> Not(expr)
     | Begin (exprs) -> Begin(List.map exprs ~f:(fun expr -> recur_expr expr))
     | Let (var_expr_list, body) -> Let(recur_var_expr_list var_expr_list, recur_body body)
     | Lets (var_expr_list, body) -> Lets(recur_var_expr_list var_expr_list, recur_body body)
     | Letr (var_expr_list, body) -> Letr(recur_var_expr_list var_expr_list, recur_body body)
     | x -> x) |> tfe in
  match form with
  | Def def -> Def(recur_def def)
  | Expr expr -> Expr(recur_expr expr)
