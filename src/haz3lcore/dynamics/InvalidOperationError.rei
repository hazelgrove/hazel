[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | ToStringFailed
  | IndexOutOfBounds
  | DivideByZero
  | NegativeExponent
  | OutOfFuel
  | InvalidProjection;

let err_msg: t => string;
