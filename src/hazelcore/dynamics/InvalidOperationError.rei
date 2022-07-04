[@deriving sexp]
type t =
  | OutOfFuel
  | DivideByZero;

let err_msg: t => string;
