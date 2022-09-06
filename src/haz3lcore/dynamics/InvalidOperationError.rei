[@deriving sexp]
type t =
  | DivideByZero
  | OutOfFuel;

let err_msg: t => string;
