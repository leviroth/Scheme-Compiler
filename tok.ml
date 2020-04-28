type token =
  (* First-Class values *)
  | Ident of string (* = Symbol *)
  | String of string
  | Fixnum of int
  | Lambda
  | True | False
  (* Special Forms  *)
  | Def
  | Or | And | Not
  | Let | Lets | Letr
  | Begin | Do
  | If | When | Case | Cond | Else
  (* Syntactic glue *)
  | Lpar | Rpar
  | Quote | Dot | Arrow | Hash
  | Eof
