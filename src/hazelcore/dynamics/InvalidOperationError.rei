[@deriving (sexp, show)]
type t =
  | DivideByZero;

let err_msg: t => string;
