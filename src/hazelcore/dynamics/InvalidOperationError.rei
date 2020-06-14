[@deriving sexp]
type t =
  | DivideByZero
  | IndexOutBound
  | IllegalEscape;

let err_msg: t => string;
