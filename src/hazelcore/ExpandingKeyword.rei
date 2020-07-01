[@deriving sexp]
type t =
  | Let
  | Case;

let is_Let: String.t => bool;

let is_Case: String.t => bool;

let mk: string => option(t);

let to_string: t => string;
