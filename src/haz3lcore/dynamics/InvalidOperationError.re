[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | InvalidOfString
  | IndexOutOfBounds
  | DivideByZero
  | NegativeExponent
  | OutOfFuel
  | InvalidProjection;

let err_msg = (err: t): string =>
  switch (err) {
  | InvalidOfString => "Error: Invalid String Conversion"
  | IndexOutOfBounds => "Error: Index Out of Bounds"
  | DivideByZero => "Error: Divide by Zero"
  | NegativeExponent => "Error: Negative Exponent in Integer Exponentiation (Consider using **.)"
  | OutOfFuel => "Error: Out of Fuel"
  | InvalidProjection => "Error: Invalid Projection"
  };
