[@deriving sexp]
type t =
  | Underscore
  | IntLit(string)
  | FloatLit(string)
  | BoolLit(bool)
  | ExpandingKeyword(ExpandingKeyword.t)
  | Var(Var.t);

/* Eventually replace Ocaml's ___of_string_opt with our own rules */
/* Ocaml accepts _1 as a float */
let hazel_float_of_string_opt: string => option(float);

let of_text: string => option(t);
