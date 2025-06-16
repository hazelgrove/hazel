[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t =
  | InvalidOfString
  | IndexOutOfBounds
  | DivideByZero
  | NegativeExponent
  | NegativeNat
  | IntegerTooBig
  | CompareArrow
  | Inconsistent;

let err_msg = (err: t): string =>
  switch (err) {
  | InvalidOfString => "Error: Invalid String Conversion"
  | IndexOutOfBounds => "Error: Index Out of Bounds"
  | DivideByZero => "Error: Divide by Zero"
  | NegativeExponent => "Error: Negative Exponent in Integer Exponentiation (Consider using **.)"
  | NegativeNat => "Error: Cannot convert negative number to Nat"
  | IntegerTooBig => "Error: Integer too big"
  | CompareArrow => "Error: Comparison of Arrow Types"
  | Inconsistent => "Error: Inconsistent Type"
  };
