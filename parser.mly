%token <string> Ident
%token <string> String
%token <int> Fixnum
%token Lambda True False
%token Def
%token Not And Or
%token Let Lets Letr
%token Begin Do
%token If /* When */ Case Cond Else
%token Lpar Rpar
%token Quote Dot Arrow Hash
%token Eof


%start <Ast.form option> form
%{ open Ast %}

%%

let sexp(X) ==
  | Lpar; ~ = X; Rpar; < >

let form :=
  | ~ = expr; { Some( Expr(expr) )}
  | ~ = def; { Some( Def(def) )}
  | Eof; { None }

let def :=
  | sexp(Def; ~ = Ident; ~ = expr; <Defv>)
  | sexp(Def; Lpar; name = Ident; args = list(Ident); Rpar; ~ = body; { Deff(name, args, body) })
  | sexp(Def; Lpar; ~ = Ident; ~ = list(Ident); Dot; ~ = Ident ; Rpar; ~ = body; < Deffl >)

let expr :=
  | ~ = constant; <>
  | ~ = Ident; < Ident >
  | sexp(~ = expr; ~ = list(expr); < Application >)
  | sexp(Quote; ~ = datum; < Quote >)
  | Quote; ~ = datum; < Quote >
  | sexp(Lambda; Lpar; ~ = list(ident); Rpar; ~ = body; < Lambda >)
  | sexp(If; e1 = expr; e2 = expr; oe = option(expr);
    { match oe with | Some e -> Ife(e1, e2, e) | None -> If(e1, e2) })
  | sexp(And; ~ = option(nonempty_list(expr)); < And >)
  | sexp(Or; ~ = option(nonempty_list(expr)); < Or >)
  | sexp(Let; Lpar; ~ = list(binding_spec); Rpar; ~ = body; < Let >)
  | sexp(Lets; Lpar; ~ = list(binding_spec); Rpar; ~ = body; < Lets >)
  | sexp(Letr; Lpar; ~ = list(binding_spec); Rpar; ~ = body; < Letr >)
  | sexp(Begin; ~ = nonempty_list(expr); < Begin >)
  | sexp(Cond; ~ = nonempty_list(cond_clause); ~ = optional; < Cond >)
  | sexp(Case; ~ = expr; ~ = nonempty_list(case_clause); ~ = optional; < Case >)
  (* TODO Implement Do *)

let body :=
  | defs = list(def); exprs = nonempty_list(expr); { {defs=defs; exprs=exprs} }

let binding_spec :=
  | Lpar; ~ = ident; ~ = expr; Rpar; < >

let cond_clause :=
  | Lpar; ~ = expr; ~ = nonempty_list(expr); Rpar; < >

let case_clause :=
  | Lpar; Lpar; ~ = option(nonempty_list(datum)); Rpar; ~ = nonempty_list(expr); Rpar; <  >

/* For Cond / Case */
let optional :=
  | option(Lpar; Else; ~ = nonempty_list(expr); Rpar; < >)

let ident :=
  | ~ = Ident; < String >

let datum :=
  | ~ = constant; <>
  | ~ = Ident; < Symbol >
  | ~ = lst; < >
  | Hash; Lpar; ~ = list(datum); Rpar; < Vector >

let lst :=
  | Lpar; ~ = list(datum); Rpar; < List >
  | Lpar; ~ = nonempty_list(datum); Dot; ~ = datum; Rpar; < Listd >
  | Quote; ~ = datum; < Quote >
  | Lpar; Quote; ~ = datum; Rpar; < Quote >

let constant :=
  | True; { Bool(true) }
  | False; { Bool(false) }
  | num = Fixnum; { Fixnum(num) }
  | str = String; { String(str) }
