[@deriving sexp]
type t =
  | DivideByZero
  | SubscriptOutOfBounds(UnescapedString.subscript_error)
  | InvalidIntOfString;

let err_msg: t => string;
