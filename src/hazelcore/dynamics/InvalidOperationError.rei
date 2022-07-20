[@deriving sexp]
type t =
  | OutOfGas
  | DivideByZero;

let err_msg: t => string;
