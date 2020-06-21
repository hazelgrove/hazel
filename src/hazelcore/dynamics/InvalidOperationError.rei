[@deriving sexp]
type t =
  | DivideByZero
  | StartOutBound
  | EndOutBound
  | StartEndOutBound
  | GreaterStart
  | IllegalEscape;

let err_msg: t => string;
