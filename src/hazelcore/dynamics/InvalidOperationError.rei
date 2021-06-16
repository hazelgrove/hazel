[@deriving sexp]
type t =
  | DivideByZero
  | IndexOutBound
  | StrNotConvToInt
  | StrNotConvToFloat
  | StrNotConvToBool
  | IntOutBound
  | StrNotTerminate
  | StartOutBound
  | EndOutBound
  | StartEndOutBound
  | GreaterStart
  | IllegalEscape;

let err_msg: t => string;
