[@deriving sexp]
type t =
  | L
  | R;

let to_string: t => string;

let pick: (t, 'a, 'a) => 'a;
