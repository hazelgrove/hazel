/* Variable: kw */
[@deriving sexp]
type t =
  | Let
  | Case;

let to_string = (kw: t): string =>
  switch (kw) {
  | Let => "let"
  | Case => "case"
  };
