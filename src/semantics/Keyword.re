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

let of_string = (str: string): option(t) =>
  switch (str) {
  | "let" => Some(Let)
  | "case" => Some(Case)
  | _ => None
  };
