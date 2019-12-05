/* Variable: kw */
[@deriving sexp]
type t =
  | Let
  | Case;

let to_var = (kw: t): Var.t =>
  switch (kw) {
  | Let => "let"
  | Case => "case"
  };
