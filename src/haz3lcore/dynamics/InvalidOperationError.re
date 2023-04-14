[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | ToStringFailed
  | IndexOutOfBounds
  | DivideByZero
  | NegativeExponent
  | OutOfFuel
  | InvalidProjection;

let err_msg = (err: t): string =>
  switch (err) {
  | ToStringFailed => "Error: ToString Failed"
  | IndexOutOfBounds => "Error: Index Out of Bounds"
  | DivideByZero => "Error: Divide by Zero"
  | NegativeExponent => "Error: Negative Exponent in Integer Exponentiation (Consider using **.)"
  | OutOfFuel => "Error: Out of Fuel"
  | InvalidProjection => "Error: Invalid Projection"
  };
