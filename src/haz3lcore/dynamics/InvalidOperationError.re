[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | InvalidOfString
  | IndexOutOfBounds
  | DivideByZero
  | NegativeExponent
  | OutOfFuel
  | InvalidIntOfString
  | InvalidFloatOfString
  | InvalidProjection;

let err_msg = (err: t): string =>
  switch (err) {
  | InvalidOfString => "Error: Invalid String Conversion"
  | IndexOutOfBounds => "Error: Index Out of Bounds"
  | DivideByZero => "Error: Divide by Zero"
  | NegativeExponent => "Error: Negative Exponent in Integer Exponentiation (Consider using **.)"
  | OutOfFuel => "Error: Out of Fuel"
  | InvalidIntOfString => "Error: Invalid String to Int Conversion"
  | InvalidFloatOfString => "Error: Invalid String to Float Conversion"
  | InvalidProjection => "Error: Invalid Projection"
  };
