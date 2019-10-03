/* Variable: kw */
[@deriving sexp]
type t =
  | Let
  | Case
  | Forall
  | Type
  | Num
  | Bool
  | List
  | Fn;

let to_string = (k: t): string =>
  switch (k) {
  | Let => "let"
  | Case => "case"
  | Forall => "forall"
  | Type => "type"
  | Num => "Num"
  | Bool => "Bool"
  | List => "List"
  | Fn => "fn"
  };
