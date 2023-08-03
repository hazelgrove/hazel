[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | DivideByZero
  | NegativeExponent
  | OutOfFuel
  | InvalidProjection
  | LetOperatorsNotDefined(string)
  | InvalidIntOfString
  | InvalidFloatOfString;

let err_msg: t => string;
