%token <string> Ident
%token <string> String
%token <int> Fixnum
%token Lambda True False
%token Def
%token Not And Or
%token Let Lets Letr
%token Begin Do
%token If When Case Cond
%token Lpar Rpar
%token Quote Dot Arrow Hash
%token Eof


%start <Ast.form option> form
%{open Ast}%

let sexp(x) ==
  | LPAR; ~ = x; RPAR; < >

let form :=
  | ~ = expr EOF? < Some >
  | ~ = def EOF? < Some >
  | EOF { None }

let def ==
  | sexp(Def; ~ = Ident; ~ = expr <Defv>)
  | sexp(Def; Lpar; ~ = Ident; ~ = list(Ident); Rpar; ~ = body; < Deff >)
  | sexp(Def; Lpar; ~ = Ident; ~ = list(Ident); Dot; ~ = Ident ; Rpar; ~ = body < Deffl >)

let expr ==
  | ~ = constant <>
  | ~ = Ident < Ident >
  | sexp(Quote; ~ = expr; < Quote >)
  | sexp(Lambda; ~ = sexp(list(ident)); ~ = body; < Lambda >)
  | sexp(If; ~ = expr; ~ = expr; ~ = option(expr) < If >)
  | sexp(And; ~ = list(expr) < And >)
  | sexp(Or; ~ = list(expr) < Or >)
  | sexp(Let; Lpar; ~ = list(binding_spec); Rpar; ~ = body < Let >)
  | sexp(Lets; Lpar; ~ = list(binding_spec); Rpar; ~ = body < Lets >)
  | sexp(Letr; Lpar; ~ = list(binding_spec); Rpar; ~ = body < Letr >)
  | sexp(Begin; ~ = nonempty_list(expr) < Begin >)
  | sexp(Cond, ~ = nonempty_list(cond_clause); ~ = option(nonempty_list(expr)) < Cond >)
  (* TODO Implement Do *)

let body ==
  | defs = list(def); exprs = nonempty_list(exprs) { {defs: defs; exprs: exprs} }

let binding_spec ==
  | Lpar; ~ = ident; ~ = expr; Rpar { ident * expr }

let cond_clause ==
  | Lpar; ~ = expr; ~ = nonempty_list(expr); Rpar < >

let case_clause ==
  | Lpar; Lpar; ~ = list()

let ident ==
  | ~ = Ident < String >

let datum ==
  | ~ = constant <>
  | ~ = Ident < Symbol > (* Symbol *)
  | ~ = list < >
  | ~ = Hash; Lpar; ~ = list(datum); Rpar < Vector >

let list ==
  | Lpar; ~ = list(datum); Rpar < List >
  | Lpar; ~ = nonempty_list(datum); Dot; ~ = datum; Rpar < Listd >
  | Quote; ~ = datum < Quote >
  | Lpar; Quote; ~ = datum; Rpar < Quote >

let constant ==
  | True { Bool(true) }
  | False { Bool(false) }
  | num = Fixnum { Fixnum(num) }
  | str = String { String(str) }
