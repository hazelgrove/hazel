[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | DivideByZero
  | OutOfFuel;

let err_msg: t => string;
