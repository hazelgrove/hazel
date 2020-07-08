[@deriving sexp]
type t =
  | DivideByZero;

let err_msg: t => string;
