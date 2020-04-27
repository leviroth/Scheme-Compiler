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
  | If of expr * expr * expr option
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
  | Deffl of string * string list * string option * body
and form =
  | Def of definition
  | Expr of expr
and body = { defs : definition list; exprs : expr list }
