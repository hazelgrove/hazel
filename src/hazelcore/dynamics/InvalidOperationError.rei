[@deriving sexp]
type t =
  | DivideByZero
  | StartOutBound
  | EndOutBound
  | StartEndOutBound
  | IllegalEscape;

let err_msg: t => string;
