[@deriving sexp]
type t =
  | DivideByZero
  | SubscriptOutOfBounds(UnescapedString.subscript_error)
  | InvalidIntOfString
  | InvalidFloatOfString
  | InvalidBoolOfString;

let err_msg: t => string;
