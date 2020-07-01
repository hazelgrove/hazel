[@deriving sexp]
type t =
  | Arrow
  | Prod
  | Sum;

let to_string: t => string;

let to_parse_string: t => string;
