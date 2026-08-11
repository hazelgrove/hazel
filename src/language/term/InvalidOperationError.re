[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t =
  | InvalidOfString
  | IndexOutOfBounds
  | DivideByZero
  | NegativeExponent
  | NegativeNat
  | IntegerTooBig
  | Incomparable;
